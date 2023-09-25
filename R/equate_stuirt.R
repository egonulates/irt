

#' Equate an Item using Mean/Mean, Mean/Sigma, Haebara or Stocking-Lord
#'
#' @param ip An Itempool object. The item parameters of this item pool will be
#'   put on a new scale.
#' @param slope A numeric value representing the slope parameter (A) of the
#'   equating method.
#' @param intercept A numeric value representing the intercept parameter (B) of
#'   the equating method.
#'
#' @return Function returns an Itempool object with equated item parametersA
#'
#' @noRd
#'
#' @examples
#' itm <- generate_item(model = "2PL")
#' equate_item_slope_intercept(itm, slope = 2, intercept = -2)
#'
#' itm <- generate_item(model = "Rasch")
#' equate_item_slope_intercept(itm, slope = 2, intercept = -2)
#'
#' itm <- generate_item(model = "3PL")
#' equate_item_slope_intercept(itm, slope = 2, intercept = -2)
#'
#' itm <- generate_item(model = "GPCM2")
#' equate_item_slope_intercept(itm, slope = 2, intercept = -2)
#'
#' itm <- generate_item(model = "GPCM")
#' equate_item_slope_intercept(itm, slope = 2, intercept = -2)
#'
#' itm <- generate_item(model = "GRM")
#' equate_item_slope_intercept(itm, slope = 2, intercept = -2)
#'

equate_item_slope_intercept <- function(itm, slope, intercept) {
  itm_model <- itm$model
  if (itm_model %in% c("Rasch", "1PL", "2PL", "3PL", "GPCM", "GPC")) {
    itm$b <- slope * itm$b + intercept
    if (!is.null(itm$se_b)) {
      itm$se_b <- slope * itm$se_b
    }
    if (itm_model %in% c("2PL", "3PL", "GPCM", "GPC")) {
      itm$a <- itm$a / slope
      if (!is.null(itm$se_a)) {
        itm$se_a <- itm$se_a / slope
      }
    }
  } else if (itm_model %in% c("GPCM2")) {
    itm$a <- itm$a / slope
    itm$b <- slope * itm$b + intercept
    itm$d <- slope * itm$d

    # Equate Standard Errors
    if (!is.null(itm$se_a)) {
      itm$se_a <- itm$se_a / slope
    }
    if (!is.null(itm$se_b)) {
      itm$se_b <- slope * itm$se_b
    }
    if (!is.null(itm$se_d)) {
      itm$se_d <- slope * itm$se_d
    }
  } else stop("Invalid item. This model has not been implmented: ",
              itm_model, ".")
  return(itm)
}


#' Equate an Item Pool using Mean/Mean, Mean/Sigma, Haebara or Stocking-Lord
#'
#' @description
#' This function equates a given item pool using the slope and intercept of
#' one of the following methods: Mean/Mean, Mean/Sigma, Haebara or
#' Stocking-Lord.
#'
#' @param ip An Itempool object. The item parameters of this item pool will be
#'   put on a new scale.
#' @param slope A numeric value representing the slope parameter (A) of the
#'   equating method.
#' @param intercept A numeric value representing the intercept parameter (B) of
#'   the equating method.
#'
#' @return Function returns an Itempool object with equated item parametersA
#'
#' @noRd
#'
#' @examples
#'
#' tt <- generate_testlet()
equate_slope_intercept <- function(ip, slope, intercept) {
  n_item <- length(ip)
  for (i in 1:n_item) {
    if (inherits(ip[[i]], "Testlet")) {
      tt <- ip[[i]]
      for (j in 1:length(tt)) {
        tt[[j]] <- equate_item_slope_intercept(tt[[j]], slope, intercept)
      }
      ip[[i]] <- tt
    } else if (inherits(ip[[i]], "Item")) {
      ip[[i]] <- equate_item_slope_intercept(ip[[i]], slope, intercept)
    } else {
      stop("Invalid item. Check item ", i, ".")
    }
  }
  return(ip)
}


#' Write syntax for a given list of item parameters
#'
#' @param ip An Itempool object
#'
#' @noRd
#'
#' @examples
#'
#' models <- rep(c("3PL", "GPCM2", "2PL", "GPCM"), each = 3)
#' new_ip <- generate_ip(model = models, D = 1.702, n_categories = 3)
#'
#' cat(stuirt_write_items(new_ip), sep = "\n")
#' stuirt_write_items(new_ip)
#'
stuirt_write_items <- function(ip) {
  text <- c()
  for (i in 1:length(ip)) {
    itm <- ip[[i]]
    model <- itm$model
    if (model == "3PL") {
      text <- c(
        text,
        paste0(i, " L3 2 DW ", itm$D, " ", itm$a, " ", itm$b, " ", itm$c))

    } else if (model == "2PL") {
      text <- c(
        text,
        paste0(i, " L3 2 DW ", itm$D, " ", itm$a, " ", itm$b, " ", 0))
    } else if (model == "1PL") {
      text <- c(
        text,
        paste0(i, " L3 2 DW ", itm$D, " ", 1, " ", itm$b, " ", 0))
    } else if (model == "Rasch") {
      text <- c(
        text,
        paste0(i, " L3 2 DW ", 1, " ", 1, " ", itm$b, " ", 0))
    } else if (model == "GPCM") {
      # "IS stands for “item-step”"
      text <- c(
        text,
        paste0(i, " PC ", length(itm$b) + 1, " DW ", itm$D, " ", itm$a, " IS ",
               paste0(itm$b, collapse = " ")))
    } else if (model == "GPCM2") {
      # LC stands for “location and category”
      text <- c(
        text,
        paste0(i, " PC ", length(itm$d) + 1, " DW ", itm$D, " ", itm$a, " LC ",
               itm$b, " 0 ", paste0(itm$d, collapse = " ")))
    } else if (model == "GRM") {
      text <- c(
        text,
        paste0(i, " GR ", length(itm$b) + 1, " DW ", itm$D, " ", itm$a, " 0 ",
               paste0(itm$b, collapse = " ")))
    } else {
      stop(paste0("Invalid item model. Item model of '", itm$item_id, "' is ",
                  itm$model, ". This model has not been implemented in ",
                  "this function."))
    }
  }
  return(text)
}

#' Write STUIRT distribution syntax
#' @description
#' Writes the distribution syntax and checks whether the new_dist or ref_dist
#' arguments of equate_stuirt() function is valid.
#'
#' @param dist A list that should have elements "type", "n" and "pars". See the
#' description of new_dist in the equate_stuirt() function.
#' @param keyword A string. "ND" for specifying the new distribution and "OD"
#'   for specifying the reference  distribution.
#'
#' @noRd
#'
#' @examples
#' # example dist values:
#' dist = list(type = "GH", n = 41, pars = NULL)
#' dist = list(type = "PN", n = 41, pars = c(0, 1, 4))
#' dist = list(type = "PB", n = 51, pars = c(2, 15, 0, 40))
#' dist = list(type = "RN", n = 31, pars = c(0, 1))
#' dist = list(type = "RU", n = 41, pars = c(0, 3))
#' dist = list(type = "ED", n = 25, pars = c(-3, 3))
#' stuirt_write_dist(dist)
#'
stuirt_write_dist <- function(dist, keyword = c("OD", "ND")) {


  if (is.null(dist)) {
    return(NULL)
  }

  keyword <- match.arg(keyword)

  if (!is.list(dist)) {
    stop("Invalid 'new_dist' or 'old_dist' argument. Both of these arguments ",
         "should be a list object.")
  }
  if (length(dist) != 3) {
    stop("Invalid 'new_dist' or 'old_dist' argument. Both of these arguments ",
         "should have three elements.")
  }
  if (!all(c("type", "n", "pars") %in% names(dist))) {
    stop("Invalid 'new_dist' or 'old_dist' argument. Both of these arguments ",
         "should have three elements with the following names: 'type', 'n' ",
         "and 'pars'. See description of these variables for examples. ")
  }

  acceptable_types <- c("GH", "PN", "PB", "RN", "RU", "ED")
  if (!is_single_value(dist$type, class = "character") ||
      !dist$type %in% acceptable_types) {
    stop("Invalid 'type' element of 'new_dist' or 'old_dist' argument. ",
         "The 'type' element should be one of the following:  '",
         paste0(acceptable_types, collapse = "', '"), "'")

  }

  if (!is_atomic_vector(dist$pars, class = "numeric")) {
    stop("Invalid 'pars' element of 'new_dist' or 'old_dist' argument. ",
         "The 'pars' element should be a numeric vector.")
  }

  if (!is_single_value(dist$n, class = "numeric", accept_na = FALSE)) {
    stop("Invalid 'n' element of 'new_dist' or 'old_dist' argument. ",
         "The 'n' argument should be an integer.")
  }

  return(paste0(keyword, " ", dist$n, " ", dist$type, " ",
                paste0(dist$pars, collapse = " ")))
}


#' Read the output file for equating parameters
#'
#' @param output_path The path of the STUIRT output
#' @noRd
#'
#' @examples
#' \dontrun{
#' output_path <- file.path(target_dir, "stuirt_analysis.out")
#' stuirt_read_output(output_path)
#' }
stuirt_read_output <- function(output_path) {
  output_text <- readLines(output_path)

  output <- list(
    output_path = output_path,
    first_solution = NULL,
    termcode = NULL,
    mean_mean = NULL,
    mean_sigma = NULL,
    haebara = NULL,
    stocking_lord = NULL)

  # Find the lines for first solutions for characteristic curve methods start
  temp <- which(grepl("First Solutions for Characteristic Curve Methods",
                      output_text))[1]
  temp_text <- output_text[seq(temp + 2, temp + 4)]
  temp_text <- strsplit(temp_text, split = "[ ]+")
  temp_text <- setNames(do.call(rbind.data.frame, temp_text[2:3]),
                        temp_text[[1]])
  output$first_solution <- temp_text

  output$termcode <- setNames(sapply(
    temp_text$TermC,
    function(x) switch(
      x,
      `1` = "Successful Execution! Terminated with gradient close to zero. The solution point is probably optimal.",
      `2` = "Probably Successful Execution! Terminated with stepsize small. The solution point is probably optimal.",
      `3` = "Probably Successful Execution! Lower point cannot be found. The solution point is probably optimal.",
      `4` = "Solutions are obtained since iteration limit has been exceeded.",
      `5` = "Too many large steps, function may be unbounded.")),
    temp_text$`Meth.`)

  # Find the lines where final equating results start
  temp <- which(grepl("Final Summary of Solutions by Method", output_text))[1]
  temp_text <- output_text[seq(temp + 2, temp + 7)]
  extract_nums <- function(x, title) {
    x <- x[grepl(title, x)]
    x <- gsub(title, "", x)
    x <- unlist(regmatches(x, gregexpr('\\(?[0-9,.,-]+', x)))
    stats::setNames(as.numeric(x), c("slope", "intercept"))
  }

  output$mean_mean <- extract_nums(temp_text, "Mean/Mean")
  output$mean_sigma <- extract_nums(temp_text, "Mean/Sigma")
  output$haebara <- extract_nums(temp_text, "Haebara")
  output$stocking_lord <- extract_nums(temp_text, "Stocking-Lord")

  return(output)
}



#' IRT Scale Transformation using STUIRT Program
#'
#' @description
#' This function serves as an interface for the STUIRT program (Kim & Kolen,
#' 2004) which offers a range of equating methods including mean-mean,
#' mean-sigma, Haebara, and Stocking-Lord. It is essential to have the
#' STUIRT program installed on your computer for this function to work.
#' You can download the program from the University of Iowa's Center for
#' Advanced Studies in Measurement and Assessment (CASMA) webpage:
#' \url{https://education.uiowa.edu/casma/computer-programs}
#'
#'
#' @param new_ip An \code{\link{Itempool-class}} object holding the item
#'   parameters of the new form.
#' @param ref_ip An \code{\link{Itempool-class}} object holding the item
#'   parameters of the old form which is the reference form.
#' @param method A string specifying the method to use for equating the new item
#'   parameters \code{new_ip} to the reference scale (\code{ref_ip}). Choose
#'   from methods like \code{"stocking-lord", "haebara", "mean-mean",
#'   "mean-sigma"}. The default method is \code{"stocking-lord"}.
#' @param common_item_ids The item IDs of the common items. The default is
#'   \code{NULL}, assuming that all items are common. Ensure that the same
#'   'item_id's are used in both 'new_ip' and 'ref_ip'.
#' @param target_dir The directory/folder where the STUIRT analysis will be
#'   saved. The default value is the current working directory, i.e.
#'   \code{get_wd()}.
#' @param stuirt_exe_path The path for the STUIRT executable "STUIRT.exe".
#'   Example: \code{"C:/STUIRT/STUIRT.exe"}.
#' @param analysis_name A short file name for the data files created for the
#'   analysis.
#' @param add_options A logical value. If \code{TRUE}, the keyword "OP" will be
#'   added to the syntax. This option is useful for detailed program output.
#'   Without \code{OP}, the section "OPTIONS AND DEFAULTS" will not appear in
#'   the main output file.
#' @param starting_values A numeric vector of length two providing starting
#'   values for the slope and intercept of the linear transformation in the
#'   Haebara and Stocking-Lord methods. The default values are \code{c(1, 0)}
#'   (i.e., slope = 1 and intercept = 0).
#' @param number_of_iterations An integer indicating the maximum number of
#'   iterations to obtain transformation constants that minimize criterion
#'   functions for the Haebara and Stocking-Lord methods. The default value is
#'   \code{NULL}, which lets STUIRT use a maximum of 20 iterations.
#' @param new_dist A list specifying proficiency distribution
#'   of new group's distribution. The list should have three named elements:
#'   \describe{
#'     \item{"type"}{A string indication the type of the distribution. The
#'       following values can be used:
#'       \describe{
#'         \item{"GH"}{From STUIRT manual "Gauss-Hermite quadrature points and
#'         weights. This subkeyword can be used properly, if a continuous
#'         distribution of proficiency is known or estimated and the summation
#'         of the criterion function could be replaced by integration over the
#'         proficiency continuum. In the program, the proficiency distribution
#'         is assumed a standard normal one. The possible maximum number of
#'         quadrature points is 180. Although more than 180 quadrature points
#'         are theoretically possible, the authors’ experiences suggest that
#'         quadrature weights tend to be unstable when trying to obtain more
#'         than about 200 quadrature points. According to Zeng and Kolen (1994),
#'         even 80 quadrature points seem to be enough to estimate the slope and
#'         intercept of a linear transformation to a satisfactory degree."
#'         (p.12)}
#'         \item{"PN"}{From STUIRT manual: "PN stands for a polygonal
#'         approximation to a normal distribution. A polygonal approximation is
#'         often encountered in finding areas and evaluating integrals. PN can
#'         be used to evaluate n proficiency points and their weights to
#'         approximate a normal distribution having a value of mean and a value
#'         of std, with a left end point and a right end point being mean -
#'         multiple-of-std × std and mean + multiple-of-std × std, respectively.
#'         More specifically, n proficiency points are equally spaced over the
#'         range of a left end point to a right end point. At each proficiency
#'         point, the density from N(mean, std) is computed. All the densities
#'         are summed and then each density is divided by the sum so that the
#'         densities are standardized. The resulting values of densities are
#'         used as the weights. These steps are similar to those used in
#'         PC-BILOG (Mislevy & Bock, 1986)." (p.12)}
#'         \item{"PB"}{From STUIRT manual: "Third, PB stands for a polygonal
#'         approximation to a four-parameter beta distribution , where alpha and
#'         beta are two scale parameters and l and u are lower and upper limits.
#'         To evaluate n proficiency points and their weights, the same logic
#'         used in PN is applied except that the interval [l, u] is divided into
#'         n subintervals and then the midpoint of each subinterval is used for
#'         a proficiency point." (p.12)}
#'         \item{"RN"}{From STUIRT manual: "Fourth, RN stands for random numbers
#'         from a normal distribution. With two real numbers for a mean and a
#'         standard deviation, RN generates n pseudo-random proficiency values
#'         sampled from a normal distribution, N(mean, std)." (p.12)}
#'         \item{"RU"}{From STUIRT manual: "RU stands for random numbers from a
#'         uniform distribution. With two real numbers for a lower limit and an
#'         upper limit, RU generates n pseudo-random proficiency values ranging
#'         from lower-limit to upper-limit." (p.12)}
#'         \item{"ED"}{From STUIRT manual: "ED stands for equal distance in the
#'         intervals between two theta points on the proficiency scale. Users
#'         should supply two real numbers for a starting point and an ending
#'         point. The theta continuum ranging from the starting point to the
#'         ending point is divided into 1 - n intervals with an equal length."
#'         (p.12). In this funciton only "EQ" is available which means same
#'         constant weight of 1 is used. }
#'       }
#'     }
#'     \item{"n"}{An integer specifying the "the number of proficiency points
#'     and should be a number between 1 and 1000, inclusive" (p.11)}
#'     \item{"pars"}{A vector of numbers specifying the parameters used
#'     for each distribution. Based on the type of distribution following
#'     parameters should be specified.
#'     \describe{
#'       \item{GH}{No parameters is needed, it can be NULL}
#'       \item{PN}{mean, std, multiple-of-std}
#'       \item{PB}{alpha, beta, lower-limit, upper-limit}
#'       \item{RN}{mean, std}
#'       \item{RU}{lower-limit, upper-limit}
#'       \item{ED}{starting, ending}
#'     }
#'     }
#'   }
#'
#'   The default is \code{NULL}. If so the STUIRT default will be used which is
#'   according to manual: "The default setting for proficiency values and their
#'   weights is that 25 proficiency values, which are equally spaced between
#'   -3.0 and 3.0, are used with the same weight 1.0 for all proficiency
#'   values." (p.13)
#'
#'   Here are some examples for different distributions:
#'   \describe{
#'     \item{GH}{\code{list(type = "GH", n = 41, pars = NULL)}}
#'     \item{PN}{\code{list(type = "PN", n = 41, pars = c(0, 1, 4))}}
#'     \item{PB}{\code{list(type = "PB", n = 51, pars = c(2, 15, 0, 40))}}
#'     \item{RN}{\code{list(type = "RN", n = 31, pars = c(0, 1))}}
#'     \item{RU}{\code{list(type = "RU", n = 41, pars = c(0, 3))}}
#'     \item{ED}{\code{list(type = "ED", n = 25, pars = c(-3, 3))}}
#'   }
#' @param ref_dist A list representing the proficiency distribution
#'   of the reference group. Refer to the description of the 'new_dist'
#'   argument for more details.
#' @param fs A two-element string vector. Each element should be
#'   one of the following values: \code{"DO"} or \code{"NO"}.
#'
#'   From the STUIRT manual: "The option keyword FS is used for standardizing
#'   the criterion functions for the Haebara and Stocking-Lord methods. The two
#'   subkeywords, DO and NO, are used to specify options. The first DO or NO is
#'   for the Haebara method and the second DO or NO is for the Stocking- Lord
#'   method. DO means that standardization is done and NO means that no
#'   standardization is done. What is meant by standardization of the criterion
#'   function is that one divides a sum of squared differences between
#'   characteristic curves in the criterion function by the number of the
#'   squared differences or the sum of weights assigned to the differences. For
#'   example, usually, to standardize the criterion function for the
#'   Stocking-Lord method, one divides the sum of squared differences between
#'   test characteristic curves by the number of proficiency values (or
#'   examinees). For more detailed information, refer to Kim and Lee (2004).
#'   Theoretically, the standardization of the criterion functions should not
#'   affect the solutions of the Haebara and Stocking-Lord methods. However, in
#'   practice, it could affect the solutions since the minimization algorithm
#'   used for nonlinear problems is affected by the magnitude of the criterion
#'   function due to its stopping rules. By default, standardization is
#'   conducted for both the Haebara and Stocking-Lord criterion functions."
#'   (p.13)
#'
#'   If the value is \code{NULL} where this keyword is not added to the
#'   syntax and STUIRT will use it's default values.
#'
#' @param sy A two-element string vector. Both of the elements should be
#'   one of the following values: \code{"BI"}, \code{"ON"} or \code{"NO"}.
#'
#'   From the STUIRT manual: "The option keyword SY is used to define criterion
#'   functions as non-symmetric or symmetric. Three subkeywords, BI, NO, and ON,
#'   are used to specify options. The first BI, NO, or ON is for the Haebara
#'   method and the second BI, NO, or ON is for the Stocking-Lord method.
#'   Theoretically, the criterion function for either of the Haebara and
#'   Stocking-Lord methods could be defined in three symmetry-related ways. The
#'   first is one in which the criterion function is defined only on the old
#'   scale as in the typical use of the Stocking-Lord method (new-to-old
#'   direction: NO). The second is one in which the criterion function is
#'   defined only on the new scale (old-to-new direction: ON). The third is one
#'   in which the criterion function is defined on the both old and new scales
#'   as in the use of the Haebara method (new-to-old and old-to-new, i.e.,
#'   bi-directional: BI) Theoretically, the three ways, BI, NO, and ON, to
#'   define the criterion function in question should give the same solutions as
#'   far as sampling error and model misfit do not happen. However, with sample
#'   data, the three ways will give different solutions for scale
#'   transformation. The default setting is in such that SY BI BI." (p.14)
#'
#'   If the value is \code{NULL} where this keyword is not added to the
#'   syntax and STUIRT will use it's default values.
#'
#' @param lm A six element list with following elements:
#'   (1) slope, (2) intercept, (3) number-of-searches, (4) radius,
#'   (5) tolerance, and, (6) either "NO" or "IN".
#'
#'   From the STUIRT manual: "The option keyword LM is used to search for
#'   possible local minimum solutions for the scale transformation constants
#'   after the first solutions for the Haebara and Stocking-Lord methods are
#'   obtained. Three subkeywords, NO, IN, and FI are prepared to instruct the
#'   program how and where to show the resulting history of local minimum
#'   search. If NO is used, no history is shown in an output file. If IN is
#'   used, the resulting history is shown within the main output file specified
#'   by users or opened by the program. If FI followed by a file name is used,
#'   the resulting history is saved separately in the file, which does not need
#'   to be located in the folder having the executable file of STUIRT." (p.14)
#'
#'   If the value is \code{NULL} where this keyword is not added to the
#'   syntax and STUIRT will use it's default values.
#'
#' @param ko A string that specify "input files for the program POLYEQUATE".
#'   Available values are \code{"MM"}, \code{"MS"}, \code{"HA"} and \code{"SL"}.
#'
#'   From the STUIRT manual: "The four subkeywords, MM, MS, HA, and SL stand for
#'   the mean/mean, mean/sigma, Haebara, and Stocking-Lord methods,
#'   respectively. " (p.15)
#'
#'   If the value is \code{NULL} where this keyword is not added to the
#'   syntax and STUIRT will use it's default values.
#'
#' @param show_output_on_console logical (not NA), indicates whether to capture
#'   the output of the command and show it on the R console. The default value
#'   is \code{TRUE}.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @references
#' Kim, S., & Kolen, M. J. (2004). STUIRT [Computer software]. Iowa City,
#' IA: Iowa Testing Programs, The University of Iowa
#'
#' @examples
#' \dontrun{
#'
#' # ---------- Mixed Models ------------ #
#' n_item <- 30
#' models <- sample(c("3PL", "GPCM2"), n_item, TRUE)
#' new_ip <- generate_ip(model = models, D = 1.702)
#' old_ip_df <- data.frame(new_ip)
#' old_ip_df$a <- old_ip_df$a + round(runif(n_item, min = -.2, max = .2), 2)
#' old_ip_df$b <- old_ip_df$b + round(runif(n_item, min = -.2, max = .2), 2)
#' old_ip_df$d1 <- old_ip_df$d1 + round(runif(n_item, min = -.2, max = .2), 2)
#' old_ip_df$d2 <- old_ip_df$d2 + round(runif(n_item, min = -.2, max = .2), 2)
#' old_ip_df$d3 <- old_ip_df$d3 + round(runif(n_item, min = -.2, max = .2), 2)
#' ref_ip <- itempool(old_ip_df)
#'
#' result <- equate_stuirt(new_ip = new_ip,
#'                         ref_ip = ref_ip,
#'                         target_dir = "C:/Temp/testthat-stuirt",
#'                         stuirt_exe_path = "C:/STUIRT/STUIRT.exe",
#'                         )
#' result
#'
#' }
#'
equate_stuirt <- function(
    new_ip,
    ref_ip,
    method = c("stocking-lord", "haebara", "mean-mean", "mean-sigma"),
    common_item_ids = NULL,
    stuirt_exe_path = "C:/STUIRT/STUIRT.exe",
    target_dir = getwd(),
    analysis_name = "stuirt_analysis",
    add_options = TRUE,
    starting_values = c(1, 0),
    number_of_iterations = NULL,
    new_dist = NULL,
    ref_dist = NULL,
    fs = c("DO", "DO"),
    sy = c("BI", "BI"),
    lm = NULL,
    ko = "SL",
    show_output_on_console = TRUE
    ) {

  if (!file.exists(stuirt_exe_path) || tools::file_ext(stuirt_exe_path) != "exe") {
    stop("Invalid 'stuirt_exe_path' argument. Please provide a valid path ",
         "for STUIRT exacutable 'STUIRT.exe'. The following file does not ",
         "exists: \n'", stuirt_exe_path, "'")
  }

  if (!inherits(new_ip, "Itempool") | !inherits(ref_ip, "Itempool")) {
    stop("Invalid 'new_ip' and/or  'ref_ip'. Both of these arguments ",
         "should be an Itempool object.")
  }

  if (is.null(common_item_ids) && length(new_ip) != length(ref_ip)) {
    stop("Invalid 'common_item_ids'. When 'common_item_ids' is NULL, ",
         "all items are assumed to be common. The program requires the ",
         "number of item in new and reference to be equal. The number of ",
         "items in 'new_ip' and 'ref_ip' are different.")
  }

  result <- list(syntax = NULL,
                 syntax_path = NULL,
                 output_path = NULL,
                 input = c(as.list(environment()), call = match.call()),
                 output = NULL
                 )

  method <- match.arg(method)
  result$input$method <- method

  # Make sure the directory exists
  if (!dir.exists(target_dir))
    dir.create(path = target_dir, recursive = TRUE)
  if (!dir.exists(target_dir))
    stop(paste0("The directory for STUIRT syntax file cannot be created at: \n",
                target_dir, "\nPlease create directory manually."),
                call. = FALSE)

  # Create syntax:
  syntax_text <- paste0("/* STUIRT Command File: ", analysis_name, "*/")
  # Add new items
  syntax_text <- c(syntax_text, paste0("NE ", length(new_ip)))
  syntax_text <- c(syntax_text, stuirt_write_items(new_ip))
  # Add reference items
  syntax_text <- c(syntax_text, paste0("OL ", length(ref_ip)))
  syntax_text <- c(syntax_text, stuirt_write_items(ref_ip))

  # Add common items
  if (is.null(common_item_ids)) {
    # if null, it is assumed that all items are anchor items, and the order
    # of new items are the same as the reference items.
    # "AO": already ordered
    syntax_text <- c(syntax_text, paste0("CI ", length(ref_ip), " AO"))
    if (!all(new_ip$item_id %in% ref_ip$item_id) ||
        !all(new_ip$item_id == ref_ip$item_id)) {
      warning("All items are used as common items, but the item_ids of ",
              "'ref_ip' and 'new_ip' possess dissimilarities in their orders. ",
              "If this was not intentional, it could lead to inaccurate ",
              "outcomes. It's crucial to ensure that the order of item_id's ",
              "in both item pools remains consistent.")
    }

    if (!all(new_ip$model == ref_ip$model)) {
      stop("The psychometric models of 'ref_ip' and 'new_ip' are different. ",
           "If this was not intentional, it could lead to inaccurate ",
           "outcomes. Please make sure the models of the items match.")
    }
  } else {
    # "MA": match
    if (!all(common_item_ids %in% new_ip$item_id)) {
      stop("Invalid 'common_item_ids'. The following 'common_item_ids' are ",
           "not in 'new_ip': ", paste0(
             common_item_ids[!common_item_ids %in% new_ip$item_id],
             collapse = ", "))
    }
    if (!all(common_item_ids %in% ref_ip$item_id)) {
      stop("Invalid 'common_item_ids'. The following 'common_item_ids' are ",
           "not in 'ref_ip': ", paste0(
             common_item_ids[!common_item_ids %in% ref_ip$item_id],
             collapse = ", "))
    }
    temp_no_new <- match(common_item_ids, new_ip$item_id)
    temp_no_ref <- match(common_item_ids, ref_ip$item_id)

    if (!all(new_ip$model[temp_no_new] == ref_ip$model[temp_no_ref])) {
      stop("The psychometric models of the common items in 'ref_ip' and ",
           "'new_ip' are different. If this was not intentional, it could ",
           "lead to inaccurate outcomes. Please make sure the models of ",
           "the common items match.")
    }

    syntax_text <- c(syntax_text, paste0("CI ", length(common_item_ids), " MA"))
    # "Make sure that, in a pair of two numbers, the first one is for the
    # new form and the second one corresponds to the old form."
    for (i in seq_along(common_item_ids)) {
      syntax_text <- c(syntax_text, paste0(temp_no_new[i], " ", temp_no_ref[i]))
    }
  }

  # Add options keyword:
  if (add_options) {
    syntax_text <- c(syntax_text, "OP")
  }

  # Add starting values:
  if (!is_atomic_vector(starting_values, class = "numeric", accept_na = FALSE) ||
      length(starting_values) != 2) {
    stop("Invalid 'starting_values'. 'starting_values' should be a numeric ",
         "vector of length two where the first number is for starting slope ",
         "value and the second value is for starting intercept value. ")
  }
  syntax_text <- c(syntax_text, paste0("ST ", paste0(starting_values, collapse = " ")))

  # Add number_of_iterations
  if (!is.null(number_of_iterations) &&
      is_single_value(number_of_iterations, class = "numeric",
                      accept_na = FALSE)) {
    syntax_text <- c(syntax_text, paste0("IT ", number_of_iterations))

  }

  # Add Proficiency distributions for new and reference item pools
  syntax_text <- c(syntax_text,
                   stuirt_write_dist(dist = new_dist, keyword = "ND"))
  syntax_text <- c(syntax_text,
                   stuirt_write_dist(dist = ref_dist, keyword = "OD"))

  # Add fs:
  if (!is.null(fs)) {
    if (!is_atomic_vector(fs, class = "character", accept_na = FALSE) ||
        length(fs) != 2 ||
        !all(fs %in% c("NO", "DO"))
        ) {
      stop("Invalid 'fs' argument. 'fs' argument should be a two element ",
           "character vector where elements should be one of the following: ",
           "'DO' or 'NO'.")
    }
    syntax_text <- c(syntax_text, paste0("FS ", paste0(fs, collapse = " ")))
  }

  # Add sy:
  if (!is.null(sy)) {
    if (!is_atomic_vector(sy, class = "character", accept_na = FALSE) ||
        length(sy) != 2 ||
        !all(sy %in% c("NO", "ON", "BI"))
        ) {
      stop("Invalid 'sy' argument. 'sy' argument should be a two element ",
           "character vector where elements should be one of the following: ",
           "'BI', 'NO' or 'ON'.")
    }
    syntax_text <- c(syntax_text, paste0("SY ", paste0(sy, collapse = " ")))
  }

  # Add sy:
  if (!is.null(lm)) {
    if (!is_atomic_vector(lm, accept_na = FALSE) || length(lm) != 6) {
      stop("Invalid 'lm' argument. 'lm' argument should be a six element ",
           "vector.")
    }
    syntax_text <- c(syntax_text, paste0(
      "LM ", paste0(lm[1:2], collapse = " "), " / ",
      paste0(lm[3:6], collapse = " ")))
  }

  # Add ko:
  if (!is.null(ko)) {
    available_values <- c("MM", "MS", "HA", "SL")
    if (!is_single_value(ko, class = "character", accept_na = FALSE) ||
        !ko %in% available_values) {
      stop("Invalid 'ko' argument. 'ko' argument should be one of the  ",
           "following: '", paste0(available_values, collapse = "', '"), "'.")
    }
    syntax_text <- c(syntax_text, paste0("KO ", ko))

  }
  # End script
  syntax_text <- c(syntax_text, "BY")


  ### Write Syntax ###
  syntax_path <- file.path(target_dir, paste0(analysis_name, ".stu"))
  output_path <- file.path(target_dir, paste0(analysis_name, ".out"))

  result$syntax <- syntax_text
  result$syntax_path <- syntax_path
  result$output_path <- output_path

  writeLines(syntax_text, con = syntax_path)

  ### Run Syntax ###
  command <- paste0(stuirt_exe_path, " \"", syntax_path, "\" \"", output_path,
                    "\"")
  system("cmd.exe", input = command, wait = TRUE,
         show.output.on.console = show_output_on_console)

  ### Read Syntax ###
  if (file.exists(output_path)) {
    result$output <- stuirt_read_output(output_path)
    method <- gsub("-", "_", method)
    pars <- result$output[[method]]
    if (is.null(pars)) {
      warning("The equating parameters cannot be extracted. The equated ",
              "item pool (..$output$equated_ip) cannot be created. ")
      return(result)
    }
    result$output$equated_ip <- equate_slope_intercept(new_ip, slope = pars[1],
                                                       intercept = pars[2])

  } else {
    warning("STUIRT cannot produce an output. Please check the syntax file ",
            "generated.")
  }
  return(result)
}


