
############################################################################@###
############################################################################@###
################### summarize_consecutive_ids ##############################@###
############################################################################@###
############################################################################@###
#' This function will shortens consecutive IDs.
#'
#' @description This function will shorten a list of id's like
#'   \code{c("itm_12", "itm_18", "itm_19", "itm_20", "itm_21", "itm_13", "abc")}
#'   to \code{c("abc", "itm_12-itm_13", "itm_18-itm_21")}.
#'
#' @param x A vector of IDs
#' @param sep A character that separates the beginning and end of a sequence of
#'   ids.
#'
#'
#' @noRd
#'
#' @examples
#' x <- c("itm_12", "itm_16", "abc", "itm_19", "itm_20", "itm_21", "itm_13",
#'        "b_14", "b_15")
#' summarize_consecutive_ids(x)
#'
#' x <- paste0("Item_", c(1:22, 33, 35:44))
#' summarize_consecutive_ids(x)
summarize_consecutive_ids <- function(x, sep = "-") {
  # Since '-' will be used in summarizing items, an item with id contianing
  # this character will potentially cause unintended errors. So, don't
  # summarize item id's if they contain "-" character.
  if (any(grepl(sep, x))) return(x)
  # If there is a column name that is integer
  if (any(grepl("^[[:digit:]]+$", x))) return(x)

  res <- data.frame(
    x = x,
    alpha_part = gsub("[[:digit:]]*$", "\\1", x),
    numeric_part = NA,
    # Whether x == alpha_part + numeric_part
    recovered = FALSE,
    group = 1
    )
  for (i in seq_along(x)) {
    res$numeric_part[i] <- as.numeric(gsub(res$alpha_part[i], "", res$x[i],
                                            fixed = TRUE))
  }
  res$recovered <- res$x == paste0(res$alpha_part, res$numeric_part)

  # sort based on numeric part
  res <- res[order(res$alpha_part, res$numeric_part, na.last = FALSE), ]
  if (nrow(res) > 1)
    for (i in 2:nrow(res)) {
      if (res$recovered[i] &&
          res$alpha_part[i] == res$alpha_part[i - 1] &&
          res$numeric_part[i] == (res$numeric_part[i - 1] + 1)
          ) {
        res$group[i] <- res$group[i - 1]
      } else res$group[i] <- res$group[i - 1] + 1
    }

  output <- c()
  for (i in unique(res$group)) {
    if (sum(res$group == i) == 1) {
      output <- c(output, res$x[res$group == i])
    } else {
      output <- c(output, paste0(head(res$x[res$group == i], 1), sep,
                                 utils::tail(res$x[res$group == i], 1)))
    }
  }
  return(output)
}


############################################################################@###
################### flexmirt_sanitize_var_names ############################@###
############################################################################@###
#' This function sanitizes the variable names and makes them acceptable for
#' flexMIRT.
#'
#' @noRd
#'
#' @examples
#' x <- c("itm 3", "stu-12", "Item_002")
#' flexmirt_sanitize_var_names(x)
flexmirt_sanitize_var_names <- function(x) {
  result <- gsub("-|_| ", "", x)
  return(result)
}

############################################################################@###
################### flexmirt_read_main_output_file #########################@###
############################################################################@###
flexmirt_read_main_output_file <- function(
  target_dir = getwd(), analysis_name = "flexMIRT_calibration") {
  output_fn <- file.path(target_dir, paste0(analysis_name, "-irt.txt"))
  if (!file.exists(output_fn)) {
    stop("The file to check convergence cannot be found at:\n", output_fn)
  }
  return(readLines(output_fn, warn = FALSE))
}


############################################################################@###
############################################################################@###
################### flexmirt_check_convergence #############################@###
############################################################################@###
############################################################################@###
flexmirt_check_convergence <- function(target_dir = getwd(),
                                       analysis_name = "flexMIRT_calibration") {
  text <- flexmirt_read_main_output_file(target_dir = target_dir,
                                         analysis_name = analysis_name)

  # First- and Second-order tests:
  # "... the reported first-order test examines if the gradient has
  # vanished sufficiently for the solution to be a stationary point. The
  # second-order test tests if the information matrix is positive definite, a
  # prerequisite for the solution to be a possible maximum. For the second-order
  # test, reporting that the solution is a possible maximum simply means that
  # the program reached a statistically desirable solution. The other possible
  # message that may be printed for the outcome of the second-order test is
  # “Solution is not a maximum; caution is advised.” If a warning message is
  # received for either the first- or second-order test, all parameter estimates
  # should be taken a provisional and should not be used as final estimates, for
  # future scoring, etc. but, rather, should be used to diagnose possible issues
  # with the model/items." (FlexMIRT User Manual v.3.5, p. 11)

  ### Example convergence text:
  # First-order test: Convergence criteria satisfied
  # Second-order test: Solution is a possible local maximum
  ### Example non-convergence text:
  # Second-order test: Solution is not a maximum; caution is advised

  first_order_test <- text[grepl("^First-order test: ", text)]
  first_order_test <- gsub("^First-order test: ", "", first_order_test)
  second_order_test <- text[grepl("^Second-order test: ", text)]
  second_order_test <- gsub("^Second-order test: ", "", second_order_test)

  # Text signalling non-convergence:
  non_convergence_text <- c("not satisfied", "Solution is not a maximum")

  converged_first_order <- !any(sapply(non_convergence_text, grepl,
                                       x = first_order_test))
  converged_second_order <- !any(sapply(non_convergence_text, grepl,
                                        x = second_order_test))

  # Return FALSE if any of the non_convergence_text is found in first- or
  # second-order tests
  return(list(
    converged = converged_first_order && converged_second_order,
    converged_first_order = converged_first_order,
    converged_second_order = converged_second_order,
    # converged = !any(sapply(
    #   non_convergence_text, grepl, x = c(first_order_test, second_order_test))),
    details = c(`First-order test` = first_order_test,
                `Second-order test` = second_order_test)))
}

############################################################################@###
############################################################################@###
################### flexmirt_check_model_argument ##########################@###
############################################################################@###
############################################################################@###
#' This function will check the 'model' argument flexMIRT function.
#'
#' @param resp_data The response data. It is assumed to be consist of only
#'   response data.
#' @param item_ids A vector of column names or numbers of the \code{x} that
#'   represents the responses.
#' @param model The psychometric model(s) of items.
#'
#' @noRd
#'
#' @examples
#' n_item <- 30
#' n_theta <- 500
#' true_ip <- generate_ip(model = c("1PL","1PL",sample(c("2PL", "GRM"), 28, T)))
#' resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
#'                               prop_missing = .1)
#' resp_data <- as.data.frame(as.matrix(resp_set, output = "score"))
#' item_ids <- colnames(resp_data)
#' model_data <- flexmirt_check_model_argument(
#'   resp_data = resp_data[, item_ids], item_ids = item_ids,
#'   model = true_ip$model, additional_constraints = NULL)
#'
#' # Output:
#'
#' ## $item_info
#' ##         item_id model num_of_categories model_syntax
#' ## Item_1   Item_1   1PL                 2    Graded(2)
#' ## Item_2   Item_2   1PL                 2    Graded(2)
#' ## Item_3   Item_3   GRM                 4    Graded(4)
#' ## Item_4   Item_4   GRM                 4    Graded(4)
#' ## Item_5   Item_5   GRM                 4    Graded(4)
#' ## Item_6   Item_6   2PL                 2    Graded(2)
#' ## Item_7   Item_7   2PL                 2    Graded(2)
#' ## ...
#' ##
#' ## $constraints
#' ## [1] "Equal (Item_1-Item_2), Slope;"
flexmirt_check_model_argument <- function(resp_data, item_ids, model,
                                          additional_constraints) {
  # This is necessary because in the code there are checks (like lapply, sapply)
  # that can be applied to a data.frame but not to a matrix.
  if (!inherits(resp_data, "data.frame")) {
    stop("Invalid 'resp_data'. 'resp_data' should be a data.frame.")
  }
  num_of_items <- ncol(resp_data)
  if (num_of_items != length(item_ids))
    stop("Invalid 'item_ids'. The length of 'item_ids' should be equal to ",
         "the number of columns of the response data 'resp_data'.")

  if (!all(sapply(resp_data, is.numeric)))
    stop("Invalid 'resp_data'. All of the columns of the response data ",
         "'resp_data' should be numeric.")

  # Find the number of categories of each item.
  item_categories <- lapply(resp_data, function(x) as.numeric(names(table(x))))
  num_of_categories <- sapply(item_categories, length)
  # Make sure that the scores are from 0 to (num_of_categories - 1)
  for (i in 1:length(num_of_categories)) {
    if (!all(item_categories[[i]] %in% seq(0, num_of_categories[i] - 1)))
      stop("Invalid response data. All of the responses should be between ",
           "0 and the one minus the number of categories. Please check ",
           "item ", item_ids[i], ".")
  }
  if (is.null(model)) {
    model <- ifelse(num_of_categories <= 2, "3PL", "GRM")
  } else if (length(model) == 1) {
    model <- rep(model, num_of_items)
  }

  if (length(model) != num_of_items)
    stop("Invalid 'model'. The length model argument should be the same ",
         "as the number of items or should be equal to one if items will ",
         "follow the same model.")

  acceptable_models <- c("Rasch", "1PL", "2PL", "3PL", "GRM", "GPCM")
  if (!all(model %in% acceptable_models)) {
    stop("Invalid 'model'. Models should be one of the following values: ",
         paste0(acceptable_models, collapse = ", "))
  }

  item_info <- data.frame(item_id = item_ids,
                          model = model,
                          num_of_categories = num_of_categories,
                          # The model name that will be used in the syntax
                          model_syntax = NA)
  item_info$model_syntax[item_info$model == "3PL"] <- "ThreePL"
  item_info$model_syntax[item_info$model == "2PL"] <- paste0("Graded(2)")
  item_info$model_syntax[item_info$model == "GRM"] <- paste0(
    "Graded(", item_info$num_of_categories[item_info$model == "GRM"],")")
  item_info$model_syntax[item_info$model == "GPCM"] <- paste0(
    "GPC(", item_info$num_of_categories[item_info$model == "GPCM"],")")
  item_info$model_syntax[item_info$model == "1PL"] <- paste0("Graded(2)")
  item_info$model_syntax[item_info$model == "Rasch"] <- paste0("Graded(2)")
  if (any(item_info$model == "1PL")) {
    additional_constraints <- c(
      additional_constraints,
      # Equal (Item_1-Item_5), Slope;
      paste0("Equal (", paste0(summarize_consecutive_ids(
        item_info$item_id[item_info$model == "1PL"]), collapse = ","),
        "), Slope;"))
  }
  if (any(item_info$model == "Rasch")) {
    # "For example, for a unidimensional model in a single group, the following
    # syntax results in a set of constraints that fixes the item slopes to 1.0
    # and then frees the estimation of the variance of the latent variable,
    # effectively creating a Rasch-type model.
    # Fix (v1-v5), Slope;
    # Value (v1-v5), Slope, 1.0;
    # Free Cov(1,1);
    # " (FlexMIRT User Manual v.3.5, p. 192)
    additional_constraints <- c(
      additional_constraints,
      # Fix (Item_1-Item_5), Slope;
      paste0("Fix (", paste0(summarize_consecutive_ids(
        item_info$item_id[item_info$model == "Rasch"]), collapse = ","),
        "), Slope;"),
      # Value (Item_1-Item_5), Slope, 1.0;
      paste0("Value (", paste0(summarize_consecutive_ids(
        item_info$item_id[item_info$model == "Rasch"]), collapse = ","),
        "), Slope, 1.0;"),
      # Free Cov(1, 1);
      "Free Cov(1, 1);"
      )
  }
  return(list(item_info = item_info, constraints = additional_constraints))
}


############################################################################@###
############################################################################@###
################### flexmirt_find_exe ######################################@###
############################################################################@###
############################################################################@###
#' Find or check the path for flexMIRT executable file
#'
#' @param flexmirt_exe The path of the flexMIRT executable file
#'   "WinFlexMIRT.exe".
#'
#' @noRd
flexmirt_find_exe <- function(flexmirt_exe = NULL) {
  if (is.null(flexmirt_exe)) {
    for (prog_files_dir in c("C:/Program Files/",
                             "C:/Program Files (x86)")) {
      flex_mirt_dir <- list.dirs(prog_files_dir, recursive = FALSE,
                                 full.names = FALSE)
      # In case there are multiple flexMIRT versions, get the latest version
      flex_mirt_dir <- utils::tail(sort(flex_mirt_dir[grepl("^flexMIRT",
                                                     flex_mirt_dir)]), 1)

      if (length(flex_mirt_dir) == 1) {
        flexmirt_exe <- file.path(prog_files_dir, flex_mirt_dir,
                                  "WinFlexMIRT.exe")
        break
      }
    }
  }

  if (!is.null(flexmirt_exe) && file.exists(flexmirt_exe)) {
    return(flexmirt_exe)
  } else
    stop("Invalid 'flexmirt_exe'. 'WinFlexMIRT.exe' cannot be located. If ",
         "flexMIRT installed on your computer, please ",
         "provide a valid path for the file 'WinFlexMIRT.exe'.", call. = FALSE)
}

############################################################################@###
############################################################################@###
################### flexmirt_check_prior_argument ##########################@###
############################################################################@###
############################################################################@###
#' This function will check the 'prior' argument flexMIRT function
#'
#' @param prior A data.frame with following one of the two forms.
#'   Form 1: If all items will follow the same prior distributions, provide a
#'   data.frame with following column names: "par", "dist", "v1", "v2".
#'
#'   "par" can take the following values: "intercept" usually for
#'   item difficulty ('b') parameters, "slope" usually for item discrimination
#'   parameters and "guessing" for guessing parameter.
#'
#'   "dist" can take the following values: "normal", "lognormal", "beta".
#'
#'   "v1" represents mean for "normal" and "lognormal" and "alpha - 1" for
#'   "beta" distribution.
#'
#'   "v2" represents standard deviation for "normal" and "lognormal" and
#'   "beta - 1" for "beta" distribution.
#'
#'   Second form will be used if different items will follow different prior
#'   distributions. Provide a  data.frame with following column names:
#'   "item_id", "par", "dist", "v1", "v2". Here "item_id" is a string
#'   representing the ID of the item. The rest of the values are explained
#'   above.
#'
#' @param fm_data the output of the function \code{flexmirt_create_data}.
#'
#' @return Return a vector of string with the syntax that will be added in
#'   the constraints section. See examples below for example output.
#'
#' @noRd
#'
#' @examples
#' n_item <- 30
#' n_theta <- 500
#' true_ip <- generate_ip(model = c("1PL","1PL",sample(c("2PL", "GRM"), 28, T)))
#' resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
#'                               prop_missing = .1)
#' resp_data <- as.data.frame(as.matrix(resp_set, output = "score"))
#' item_ids <- colnames(resp_data)
#'
#' fm_data <- flexmirt_create_data(x = resp_data,
#'                                 target_dir = "C:/Temp/testthat-flexmirt",
#'                                 analysis_name = "flexMIRT_calibration",
#'                                 model = true_ip$model,
#'                                 overwrite = TRUE)
#' # Same prior for all items
#' prior <- data.frame(par = c("intercept", "slope", "guessing"),
#'                     dist = c("normal", "lognormal", "beta"),
#'                     v1 = c(0, 0, 1),
#'                     v2 = c(1, 0.5, 3))
#' flexmirt_check_prior_argument(prior, fm_data)
#' # Output:
#' ## [1] "Prior Group1, (), Intercept : Normal(0,1);"
#' ## [2] "Prior Group1, (), Slope : logNormal(0,1);"
#'
#' # Different prior for different items
#' prior <- data.frame(
#'   item_id = c("Item_1", "Item_1", "Item_3", "Item_3", "Item_10"),
#'   par = c("intercept", "slope", "intercept", "slope", "slope"),
#'   dist = c("normal", "lognormal", "normal", "lognormal", "lognormal"),
#'   v1 = c(0, 0, 0, 0, 0),
#'   v2 = c(1, 0.5, 2, 0.6, 0.7))
#'
#' flexmirt_check_prior_argument(prior, fm_data)
#' # Output:
#' ## [1] "Prior Group1, (Item_1-Item_30), Intercept : Normal(0,1);"
#' ## [2] "Prior Group1, (Item_1-Item_30), Slope : logNormal(0,1);"
#' ## [3] "Prior Group1, (Item_1-Item_30), Intercept : Normal(0,1);"
#' ## [4] "Prior Group1, (Item_1-Item_30), Slope : logNormal(0,1);"
#' ## [5] "Prior Group1, (Item_1-Item_30), Slope : logNormal(0,1);"
#'
flexmirt_check_prior_argument <- function(
  prior = NULL, fm_data = NULL) {

  ### Check if 'prior' argument is valid ###
  if (is.null(prior)) {
    return(prior)
  } else if (!inherits(prior, "data.frame") ||
             !all(colnames(prior) %in% c("item_id", "par", "dist", "v1",
                                        "v2"))) {
    stop("Invalid 'prior' argument. 'prior' argument should be a data.frame ",
         "object with either following four columns: 'par', 'dist', ",
         "'v1', 'v2' or following five columns: 'item_id', 'par', ",
         "'dist', 'v1', 'v2'.")
  }
  # Check distribution values
  prior$dist <- tolower(prior$dist)
  if (!all(prior$dist %in% c("normal", "lognormal", "beta"))) {
    stop("Invalid 'prior' argument. All of the values in 'dist' column should ",
         "be either 'normal', 'lognormal' or 'beta'.")
  }
  prior$dist[prior$dist == "normal"] <- "Normal"
  prior$dist[prior$dist == "beta"] <- "Beta"
  prior$dist[prior$dist == "lognormal"] <- "logNormal"

  # Check parameter values
  prior$par <- tolower(prior$par)
  if (!all(prior$par %in% c("intercept", "slope", "guessing"))) {
    stop("Invalid 'prior' argument. All of the values in 'par' column should ",
         "be either 'intercept', 'slope' or 'guessing'.")
  }
  prior$par <- tools::toTitleCase(tolower(prior$par))

  if (!is.numeric(prior$v1) || !is.numeric(prior$v2))
    stop("Invalid 'prior' argument. All of the values in 'v1' and 'v2' columns",
         " should be numeric.")

  # Check item_id's
  if ("item_id" %in% colnames(prior)) {
    for (g in seq_along(fm_data)) {
      if (!all(prior$item_id %in% fm_data[[g]]$item_info$item_id))
        stop("Invalid 'prior' argument. All of the item_id values should ",
             "be in the response data.")
    }
  }


  ### Create syntax for the 'prior' argument ###
  result <- c()

  # Option 1: There is no "item_id" in 'prior', so same priors will be applied
  # to all of the items
  if (!"item_id" %in% colnames(prior)) {
    for (g in seq_along(fm_data)) {
      for (i in seq_len(nrow(prior))) {
        temp_item_ids <- summarize_consecutive_ids(fm_data[[g]]$item_info$item_id)
        for (j in seq_along(temp_item_ids)) {
          temp_text <- paste0("Prior ", names(fm_data)[g], ", (",
                              temp_item_ids[j], "), ", prior$par[i], " : ",
                              prior$dist[i], "(", prior$v1[i], ",", prior$v2[i],
                              ");")
          result <- c(result, temp_text)
        }
      }
    }
  # Option 2: There is "item_id" in 'prior', each item have different prior
  # Currently function assumes that same prior will be applied to all groups
  } else {
    for (g in seq_along(fm_data)) {
      for (i in seq_len(nrow(prior))) {
        temp_text <- paste0("Prior ", names(fm_data)[g], ", (",
                            prior$item_id[i], "), ", prior$par[i], " : ",
                            prior$dist[i], "(", prior$v1[i], ",", prior$v2[i],
                            ");")
        result <- c(result, temp_text)
      }
    }
  }
  return(result)
}

############################################################################@###
############################################################################@###
################### flexmirt_create_data ###################################@###
############################################################################@###
############################################################################@###
#' Create flexmirt data and return a list containing relevant info for syntax
#' @param x The data.
#' @param target_dir The directory/folder where the flexMIRT syntax data
#'   files and output will be saved. The default value is the current working
#'   directory, i.e. \code{get_wd()}.
#' @param item_ids A vector of column names or numbers of the \code{x} that
#'   represents the responses. The default value is \code{NULL} where all of
#'   the columns in the data are assumed to be a response matrix (unless
#'   specified by \code{group_var} or \code{examinee_id_var} arguments).
#' @param model The psychometric model(s) of items. The user can provide an
#'   input in the following three ways: (a) A vector of length one which
#'   represents the model of each item. (b) A vector which has the same length
#'   as the number of items that will be calibrated that specifies the model
#'   of each item. (c) \code{NULL}, where the program will check the number of
#'   categories of each item. Item with two or fewer categories will be
#'   calculated using \code{"3PL"} (three-parameter logistic IRT model), items
#'   with more than two categories will be calibrated using \code{"GRM"} (Graded
#'   Response Model).
#'
#'   \describe{
#'     \item{\code{"1PL"}}{One-parameter logistic model.}
#'     \item{\code{"2PL"}}{Two-parameter logistic model.}
#'     \item{\code{"3PL"}}{Three-parameter logistic model.}
#'     \item{\code{"GRM"}}{Graded Response Model}
#'     \item{\code{"GPCM"}}{Generalized Partial Credit Model}
#'   }
#'
#' @param group_var The column name or number that contains group membership
#'   information if multi-group calibration is desired. Ideally, it grouping
#'   variable is represented by single digit integers. If other type of data
#'   provided, an integer value will automatically assigned to the variables.
#'   The default value is \code{NULL}, where no multi-group analysis will be
#'   performed.
#' @param overwrite If TRUE and there is already a BILOG-MG data file in the
#'   target path with the same name, the file will be overwritten.
#'
#' @noRd
#'
#' @examples
#' n_item <- 30
#' n_theta <- 500
#' true_ip <- generate_ip(model = c("1PL","1PL",sample(c("2PL", "GRM"), 28, T)))
#' resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
#'                               prop_missing = .1)
#' resp_data <- as.data.frame(as.matrix(resp_set, output = "score"))
#' item_ids <- colnames(resp_data)
#'
#' fm_data <- flexmirt_create_data(x = resp_data,
#'                                 target_dir = "C:/Temp/testthat-flexmirt",
#'                                 analysis_name = "flexMIRT_calibration",
#'                                 model = true_ip$model,
#'                                 overwrite = TRUE)
#' # Output:
#' ## $Group1
#' ## $Group1$item_info
#' ##         item_id model num_of_categories model_syntax
#' ## Item_1   Item_1   1PL                 2    Graded(2)
#' ## Item_2   Item_2   1PL                 2    Graded(2)
#' ## Item_3   Item_3   2PL                 2    Graded(2)
#' ## Item_4   Item_4   GRM                 4    Graded(4)
#' ## Item_5   Item_5   2PL                 2    Graded(2)
#' ## Item_6   Item_6   GRM                 4    Graded(4)
#' ## Item_7   Item_7   2PL                 2    Graded(2)
#' ## Item_8   Item_8   2PL                 2    Graded(2)
#' ## Item_9   Item_9   2PL                 2    Graded(2)
#' ## Item_10 Item_10   2PL                 2    Graded(2)
#' ## Item_11 Item_11   2PL                 2    Graded(2)
#' ## Item_12 Item_12   2PL                 2    Graded(2)
#' ## Item_13 Item_13   GRM                 4    Graded(4)
#' ## Item_14 Item_14   2PL                 2    Graded(2)
#' ## Item_15 Item_15   2PL                 2    Graded(2)
#' ## Item_16 Item_16   GRM                 4    Graded(4)
#' ## Item_17 Item_17   2PL                 2    Graded(2)
#' ## Item_18 Item_18   GRM                 4    Graded(4)
#' ## Item_19 Item_19   2PL                 2    Graded(2)
#' ## Item_20 Item_20   2PL                 2    Graded(2)
#' ## Item_21 Item_21   GRM                 4    Graded(4)
#' ## Item_22 Item_22   2PL                 2    Graded(2)
#' ## Item_23 Item_23   2PL                 2    Graded(2)
#' ## Item_24 Item_24   2PL                 2    Graded(2)
#' ## Item_25 Item_25   GRM                 4    Graded(4)
#' ## Item_26 Item_26   GRM                 4    Graded(4)
#' ## Item_27 Item_27   GRM                 4    Graded(4)
#' ## Item_28 Item_28   GRM                 4    Graded(4)
#' ## Item_29 Item_29   GRM                 4    Graded(4)
#' ## Item_30 Item_30   2PL                 2    Graded(2)
#' ##
#' ## $Group1$constraints
#' ## [1] "Equal (Item_1-Item_2), Slope;"
#' ##
#' ## $Group1$data_file_name
#' ## [1] "C:/Temp/testthat-flexmirt/flexMIRT_calibration.dat"
#' ##
#' ## $Group1$num_of_examinees
#' ## [1] 500
#' ##
#' ## $Group1$examinee_id_var
#' ## [1] "examinee_id"
#'
flexmirt_create_data <- function(
  x,
  target_dir = getwd(),
  analysis_name = "flexMIRT_calibration",
  item_ids = NULL,
  examinee_id_var = NULL,
  group_var = NULL,
  model = NULL,
  overwrite = FALSE,
  additional_constraints = NULL
  ) {
  if (is(x, "Response_set")) {
    resp_data <- as.matrix(x, output = "score")
    resp_data <- data.frame(resp_data, check.names = FALSE)
    # This assumes all resp_set objects have examinee id. Test this.
    resp_data <- cbind(examinee_id = rownames(resp_data), resp_data)
    examinee_id_var <- "examinee_id"
  } else if (inherits(x, c("matrix", "data.frame"))) {
    resp_data <- x
    if (inherits(resp_data, "matrix")) {
      resp_data <- as.data.frame(resp_data)
    }
    if (is.null(examinee_id_var) && !is.null(rownames(resp_data))) {
      resp_data <- cbind(examinee_id = rownames(resp_data), resp_data)
      examinee_id_var <- "examinee_id"
    }
  } else {
    stop("Invalid 'x'. The data set should be Response_set, data.frame or ",
         "matrix.")
  }

  if (is.null(item_ids)) {
    item_ids <- setdiff(colnames(resp_data), c(examinee_id_var, group_var))
  }

  # Remove unused columns
  resp_data <- resp_data[, c(examinee_id_var, group_var, item_ids)]

  data_fn <- file.path(target_dir, paste0(analysis_name, ".dat"))

  if (!dir.exists(target_dir))
    dir.create(path = target_dir, recursive = TRUE)
  utils::write.table(x = resp_data, file = data_fn, sep = " ",
                     row.names = FALSE, col.names = FALSE, quote = FALSE,
                     na = "-9")

  # Check the model argument
  model_data <- flexmirt_check_model_argument(
    resp_data = resp_data[, item_ids], item_ids = item_ids, model = model,
    additional_constraints = additional_constraints)
  output <- list(
    Group1 = list(
      item_info = model_data$item_info,
      constraints = model_data$constraints,
      data_file_name = data_fn,
      num_of_examinees = nrow(resp_data),
      examinee_id_var = examinee_id_var)
    )
}


############################################################################@###
############################################################################@###
################### flexmirt_create_syntax #################################@###
############################################################################@###
############################################################################@###
#' Create flexMIRT syntax.
#'
#' @description This function creates syntax that can be used in flexMIRT.
#'   Please note that the syntax was mainly tested for flexMIRT version 3.5.2
#'   and used the document "Houts, C. R., & Cai, L. (2020). flexMIRT user’s
#'   manual version 3.52: Flexible multilevel multidimensional item analysis
#'   and test scoring. Chapel Hill, NC: Vector Psychometric Group."
#'
#' @noRd
#'
flexmirt_create_syntax <- function(
  x = NULL,
  model = NULL,
  D = 1,
  target_dir = getwd(),
  analysis_name = "flexMIRT_calibration",
  prior = NULL,
  max_em_cycles = c(500, 100),
  quadrature = c(49, 6),
  em_tol = c(1e-4, 1e-9),
  gof = "Basic",
  item_ids = NULL,
  examinee_id_var = NULL,
  additional_options = NULL,
  additional_constraints = NULL,
  group_var = NULL,
  scoring_method = "EAP", # EAP/MAP/ML/SSC/MI
  overwrite = FALSE
  ) {

  #######################################@###
  ################# Project #############@###
  #######################################@###
  syntax_text <- "<Project>"
  # Add Title
  temp_text <- paste0("Title = \"", analysis_name, "\";")
  syntax_text <- c(syntax_text, temp_text)
  # Add Description
  temp_text <- paste0("Description = \"", analysis_name, "\";")
  syntax_text <- c(syntax_text, temp_text)

  #######################################@###
  ################# Options #############@###
  #######################################@###
  syntax_text <- c(syntax_text, "\n<Options>")
  # Add Mode. Possible options: Calibration/Scoring/Simulation
  temp_text <- paste0("Mode = Calibration;")
  syntax_text <- c(syntax_text, temp_text)

  # # 'TechOut': "TechOut determines if controls values such as tolerance
  # # values, processing times, names of outputted files, etc. will be
  # # printed in the preamble of the output." (FlexMIRT User Manual v.3.5,
  # # p. 167)
  temp_text <- paste0("TechOut = Yes;")
  syntax_text <- c(syntax_text, temp_text)

  # "NumDec determines the number of decimal places reported for item parameters
  # in the output file - the default is 2." (FlexMIRT User Manual v.3.5, p. 167)
  temp_text <- paste0("NumDec = 5;")
  syntax_text <- c(syntax_text, temp_text)

  # "SaveCOV refers to the covariance matrix of the parameter estimates;"
  # (FlexMIRT User Manual v.3.5, p. 167)
  temp_text <- paste0("SaveCOV = Yes;")
  syntax_text <- c(syntax_text, temp_text)

  # # "SaveINF, the Fisher information function values of the items and the test;
  # # " (FlexMIRT User Manual v.3.5, p. 167)
  # temp_text <- paste0("SaveINF = Yes;")
  # syntax_text <- c(syntax_text, temp_text)

  # "SavePRM, the item and group parameter estimates;" (FlexMIRT User Manual
  # v.3.5, p. 167)
  temp_text <- paste0("SavePRM = Yes;")
  syntax_text <- c(syntax_text, temp_text)

  # "SaveSCO, the individual IRT scale scores" (FlexMIRT User Manual v.3.5, p.
  # 167)
  temp_text <- paste0("SaveSCO = ",
                      ifelse(is.null(scoring_method), "No", "Yes"), ";")
  syntax_text <- c(syntax_text, temp_text)

  # "SaveDBG, additional technical information" (FlexMIRT User Manual v.3.5, p.
  # 167)
  temp_text <- paste0("SaveCOV = Yes;")
  syntax_text <- c(syntax_text, temp_text)

  # "SavePCC, the unique elements of the polychoric correlation matrix followed
  # by a table of item thresholds" (FlexMIRT User Manual v.3.5, p. 167)
  temp_text <- paste0("SavePCC = Yes;")
  syntax_text <- c(syntax_text, temp_text)

  # max_em_cycles
  syntax_text <- c(syntax_text,
                   paste0("MaxE = ", max_em_cycles[1], ";"),
                   paste0("MaxM = ", max_em_cycles[2], ";"))

  # quadrature
  syntax_text <- c(syntax_text,
                   paste0("Quadrature = ", quadrature[1], ", ",
                          quadrature[2],";"))
  # em_tol
  syntax_text <- c(syntax_text,
                   paste0("Etol = ", em_tol[1], ";"),
                   paste0("Mtol = ", em_tol[2], ";"))

  ### Scoring Options ###
  if (!is.null(scoring_method)) {
    temp_text <- paste0("Score = ", scoring_method, ";")
    syntax_text <- c(syntax_text, temp_text)
  }

  ### GOF and LD Indices Commands ###
  gof <- tools::toTitleCase(gof)
  if (!gof %in% c("Basic", "Extended", "Complete")) gof <- "Basic"
  syntax_text <- c(syntax_text, paste0("GOF = ", gof, ";"))

  ### MH-RM Specific Options ###


  ### Additional options ###
  if (!is.null(additional_options)) {
    syntax_text <- c(syntax_text, additional_options)
  }


  # "" (FlexMIRT User Manual v.3.5, p. 167)
  # "" (FlexMIRT User Manual v.3.5, p. 167)
  # "" (FlexMIRT User Manual v.3.5, p. 167)
  # "" (FlexMIRT User Manual v.3.5, p. 167)

  #######################################@###
  ################# Groups ##############@###
  #######################################@###
  syntax_text <- c(syntax_text, "\n<Groups>")
  fm_data <- flexmirt_create_data(
    x = x,
    target_dir = target_dir,
    analysis_name =  analysis_name,
    item_ids = item_ids,
    examinee_id_var = examinee_id_var,
    group_var = group_var,
    model = model,
    overwrite = overwrite)
  for (i in seq_len(length(fm_data))) {
    item_info <- fm_data[[i]]$item_info
    syntax_text <- c(syntax_text, paste0("%Group", i, "%"))
    # File
    syntax_text <- c(syntax_text,
                     paste0("File = \"", fm_data[[i]]$data_file_name, "\";"))
    # N
    syntax_text <- c(syntax_text,
                     paste0("N = ", fm_data[[i]]$num_of_examinees, ";"))
    # Varnames
    temp_text <- paste0("Varnames = ", paste0(
      c(fm_data[[i]]$examinee_id_var,
        summarize_consecutive_ids(item_info$item_id)), collapse = ","), ";")
    syntax_text <- c(syntax_text, temp_text)

    # Select
    if (!is.null(fm_data[[i]]$examinee_id_var)) {
      temp_text <- paste0("Select = ", paste0(
        summarize_consecutive_ids(item_info$item_id), collapse = ","), ";")
      syntax_text <- c(syntax_text, temp_text)
    }
    # Ncats
    for (temp_cat in unique(unique(item_info$num_of_categories))) {
      temp_text <- paste0(summarize_consecutive_ids(item_info$item_id[
        item_info$num_of_categories == temp_cat]), collapse = ",")
      temp_text <- paste0("Ncats(", temp_text, ") = ", temp_cat, ";")
      syntax_text <- c(syntax_text, temp_text)
    }
    # Model
    for (temp_model in unique(unique(item_info$model_syntax))) {
      temp_text <- paste0(
        summarize_consecutive_ids(
          item_info$item_id[item_info$model_syntax == temp_model]),
        collapse = ",")
      temp_text <- paste0("Model(", temp_text, ") = ", temp_model, ";")
      syntax_text <- c(syntax_text, temp_text)
    }
    # Missing
    syntax_text <- c(syntax_text, paste0("Missing = -9;"))

    # CaseID
    if (!is.null(fm_data[[i]]$examinee_id_var)) {
      syntax_text <- c(syntax_text, paste0("CaseID = ",
                                           fm_data[[i]]$examinee_id_var, ";"))
    }
  }

  #######################################@###
  ################# Constraints #########@###
  #######################################@###
  syntax_text <- c(syntax_text, "\n<Constraints>")

  if (D != 1) {
    temp_text <- paste0("Coeff Group1, (", paste0(
      summarize_consecutive_ids(item_info$item_id), collapse = ","),
      "), Slope, ", D, ";")
    additional_constraints <- c(additional_constraints, temp_text)
  }

  for (g in fm_data) {
    syntax_text <- c(syntax_text, g$constraints)
  }

  # Add priors
  if (!is.null(prior)) {
    temp_text <- flexmirt_check_prior_argument(prior = prior, fm_data = fm_data)
    additional_constraints <- c(additional_constraints, temp_text)
  }

  syntax_text <- c(syntax_text, additional_constraints)

  #######################################@###
  #######################################@###
  syntax_file_name <- file.path(target_dir, paste0(analysis_name, ".flexmirt"))
  # fileConn<-file(syntax_file_name)
  # writeLines(c("Hello","World"), fileConn)
  # close(fileConn)
  writeLines(text = syntax_text, con = syntax_file_name)

  return(list(groups = fm_data,
              syntax = syntax_text,
              syntax_file_name = syntax_file_name))
}

############################################################################@###
############################################################################@###
################### flexmirt_read_item_summary #############################@###
############################################################################@###
############################################################################@###
#' Read flexMIRT item models and number of categories
#'
#'
#' @noRd
#'
flexmirt_read_item_summary <- function(
  target_dir = getwd(),
  analysis_name = "flexMIRT_calibration",
  models = NULL) {
  text <- flexmirt_read_main_output_file(target_dir = target_dir,
                                         analysis_name = analysis_name)
  # Get the section of item parameters
  text <- text[seq(which(grepl("Summary of the Data and Dimensions", text,
                               fixed = TRUE))[1] + 7,
                   which(grepl("Bock-Aitkin EM Algorithm Control Values", text,
                               fixed = TRUE))[1] - 2
                   )]
  fn <- function(x) {
    y <- strsplit(x, split = " ")[[1]]
    return(matrix(y[y != ""], nrow = 1)[1:3])
  }
  output <- lapply(text, fn)
  output <- do.call("rbind.data.frame", output)
  colnames(output) <- c("item_no", "num_of_categories", "model")
  output$model[output$model == "Graded" &
                 output$num_of_categories == 2] <- "2PL"
  # Model name in 'irt' package
  output$irt_model <- rep(models, length.out = nrow(output))
  output$model[output$model == "Nominal" &
                 output$irt_model %in% c("GPCM", "GPCM2")] <- "GPC"
  return(output)
}



############################################################################@###
############################################################################@###
################### flexmirt_read_item_par #################################@###
############################################################################@###
############################################################################@###
#' Read flexMIRT item parameters
#'
#'
#' @noRd
#'
flexmirt_read_item_par <- function(
  target_dir = getwd(),
  D = 1,
  models = NULL,
  analysis_name = "flexMIRT_calibration") {
  whole_text <- flexmirt_read_main_output_file(target_dir = target_dir,
                                               analysis_name = analysis_name)
  # Get the model of each item (flexMIRT model names)
  item_models <- flexmirt_read_item_summary(target_dir = target_dir,
                                            models = models,
                                            analysis_name = analysis_name)
  item_models$item_id <- NA
  ip_list <- list()
  for (model in unique(item_models$model)) {
    if (model == "3PL") {
      if (any(grepl("3PL Items in Classic Normal Ogive Metric", whole_text))) {
        text <- whole_text[seq(which(grepl(
          "3PL Items in Classic Normal Ogive Metric", whole_text,
          fixed = TRUE))[1], length(whole_text))]
        text <- text[seq(3, which(grepl(analysis_name, text,
                                        fixed = TRUE))[1] - 2)]
        temp_pars <- data.frame()

        for (i in seq_len(length(text))) {
          y <- gsub("-", " -", text[i])
          y <- strsplit(y, split = " ")[[1]];
          y <- as.data.frame(lapply(as.list(y[y != ""]), utils::type.convert,
                                    as.is = TRUE))
          # Update item id in item_models
          item_models$item_id[item_models$item_no == y[1, 1]] <- y[1, 2]

          # headers for 3PL:
          #   1 --     2 -- 3  -- 4 --   5  --  6 -- 7 --   8 --  9 -- 10 -- 11
          #Item -- Label -- P# -- a -- s.e. -- P# -- b -- s.e.-- P# --  g -- s.e
          y <- y[, c(2, 4, 5, 7, 8, 10, 11)]
          colnames(y) <- c("item_id", "a", "se_a", "b", "se_b", "c", "se_c")
          temp_pars <- rbind(temp_pars, y)
        }
        ip_list <- c(ip_list, itempool(temp_pars, D = D))
      } else {
        text <- whole_text[seq(which(grepl(paste0(model, " Items for Group 1"),
                                     whole_text, fixed = TRUE))[1],
                               length(whole_text))]

        text <- text[seq(3, which(grepl(analysis_name, text,
                                        fixed = TRUE))[1] - 2)]

        temp_pars <- data.frame()

        for (i in seq_len(length(text))) {
          y <- gsub("-", " -", text[i])
          y <- strsplit(y, split = " ")[[1]];
          y <- as.data.frame(lapply(as.list(y[y != ""]), utils::type.convert,
                                    as.is = TRUE))
          # Update item id in item_models
          item_models$item_id[item_models$item_no == y[1, 1]] <- y[1, 2]
          # headers for 3PL:
          #    1 --     2 --  3 -- 4 --    5 --  6 -- 7 --    8 -- 9 --   10 --
          # Item -- Label -- P# -- a -- s.e. -- P# -- c -- s.e. -- b -- s.e. --
          # -- 11 --      12 --   13 -- 14 --   15
          # -- P# -- logit-g -- s.e. --  g -- s.e.
          y <- y[, c(2, 4, 5, 9, 10, 14, 15)]
          colnames(y) <- c("item_id", "a", "se_a", "b", "se_b", "c", "se_c")
          temp_pars <- rbind(temp_pars, y)
        }
        ip_list <- c(ip_list, itempool(temp_pars, D = D))
      }
    } else if (model == "2PL") {
      text <- whole_text[seq(which(grepl(paste0(model, " Items for Group 1"),
                                   whole_text, fixed = TRUE))[1],
                             length(whole_text))]
      text <- text[seq(3, which(grepl(analysis_name, text,
                                      fixed = TRUE))[1] - 2)]

      #   1  --   2   --  3 -- 4 --  5   --  6 -- 7 --  8   -- 9 --  10
      # Item -- Label -- P# -- a -- s.e. -- P# -- c -- s.e. -- b -- s.e.
      temp_ip_list <- list()
      for (i in seq_len(length(text))) {
        y <- strsplit(text[i], split = " ")[[1]]
        y <- as.data.frame(lapply(as.list(y[y != ""]), utils::type.convert,
                                  as.is = TRUE))
        # Update item id in item_models
        item_models$item_id[item_models$item_no == y[1, 1]] <- y[1, 2]

        if (any(sapply(y, function(x) x == "----"))) { # Rasch model item
          # This is a temporary fix. For Rasch model items, since Slope is
          # fixed, there is no column for "P#".
          # Example output:
          #   1  --  2 --  3  --   3 --  4 --  5  --   6  -- 7   --    8 --  9
          # Item   Label  P#       a   s.e.   P#       c    s.e.       b    s.e.
          #    1  Item_1      1.0000   ----    1 -1.0311  0.0921  1.0311  0.0921
          #    2  Item_2      1.0000   ----    2 -0.1745  0.0864  0.1745  0.0864
          # ...
          y <- y[, -c(1, 3, 4, 5, 6, 7)]
          colnames(y) <- c("item_id", "b", "se_b")
          temp_item <- item(b = y$b, se_b = y$se_b, model = "Rasch",
                            # In case item_id's are integer values
                            item_id = as.character(y$item_id))
        } else { # 2PL or 1PL item
          # Remove "Item" and "P#" columns
          y <- y[, -c(1, 3, 6, 7, 8)]
          colnames(y) <- c("item_id", "a", "se_a", "b", "se_b")
          temp_item <- item(a = y$a, se_a = y$se_a, b = y$b, se_b = y$se_b,
                            model = "2PL", item_id = y$item_id, D = D)
        }
        temp_ip_list <- c(temp_ip_list, temp_item)
      }
      ip_list <- c(ip_list, itempool(temp_ip_list))
    } else if (model == "Graded") {
      text <- whole_text[seq(utils::tail(
        which(grepl(paste0(model, " Items for Group 1"), whole_text,
                    fixed = TRUE)), 1), length(whole_text))]
      text <- text[seq(3, which(grepl(analysis_name, text,
                                      fixed = TRUE))[1] - 2)]
      # Headers for GPC:
      # Item -- Label -- P# -- a -- s.e. -- b -- s.e. -- d 1 -- s.e. -- d 2
      # -- s.e. -- d 4 -- s.e. -- d 5 -- s.e.
      temp_ip_list <- list()
      for (i in seq_len(length(text))) {
        y <- strsplit(text[i], split = " ")[[1]]
        y <- as.data.frame(lapply(as.list(y[y != ""]), utils::type.convert,
                                  as.is = TRUE))
        # Update item id in item_models
        item_models$item_id[item_models$item_no == y[1, 1]] <- y[1, 2]
        # Remove "Item" and "P#" columns
        y <- y[, -c(1, 3)]
        n_cat <- (ncol(y) - 3) / 2
        colnames(y) <- c("item_id", "a", "se_a",
                         paste0(rep(c("b", "se_b"), n_cat),
                                rep(1:n_cat, each = 2)))
        temp_item <- itempool(y, model = "GRM", D = D)[[1]]
        temp_ip_list <- c(temp_ip_list, temp_item)
      }
      ip_list <- c(ip_list, itempool(temp_ip_list))
    } else if (model == "GPC") {
      text <- whole_text[seq(which(grepl(paste0(model, " Items for Group 1"),
                                   whole_text, fixed = TRUE))[1],
                             length(whole_text))]
      text <- text[seq(3, which(grepl(analysis_name, text,
                                      fixed = TRUE))[1] - 2)]
      temp_ip_list <- list()
      for (i in seq_len(length(text))) {
        y <- strsplit(text[i], split = " ")[[1]]
        y <- as.data.frame(lapply(as.list(y[y != ""]), utils::type.convert,
                                  as.is = TRUE))
        # Remove "P#" columns
        y <- y[, -c(3, 8)]
        n_cat <- (ncol(y) - 6) / 2
        colnames(y) <- c("item_no", "item_id", "a", "se_a", "b", "se_b",
                              paste0(rep(c("d", "se_d"), n_cat),
                                     rep(1:n_cat, each = 2)))
        # Update item id in item_models
        item_models$item_id[item_models$item_no == y$item_no[1]] <- y$item_id[1]
        # By default import items as GPCM2 model
        temp_item <- item(
          a = y$a, se_a = y$se_a, b = y$b, b = y$se_b,
          d = unlist(y[, paste0("d", 1:n_cat)]),
          se_d = unlist(y[, paste0("se_d", 1:n_cat)]),
          item_id = y$item_id, model = "GPCM2", D = D)
        # Check whether model specified is "GPCM" or "GPCM2"
        if (item_models$irt_model[item_models$item_no == y$item_no] == "GPCM") {
          # Since converting from GPCM2 to GPCM might make SE rather meaningless
          # remove SE:
          temp_item@se_a <- NULL
          temp_item@se_b <- NULL
          temp_item@se_d <- NULL
          temp_item <- convert_model(temp_item, "GPCM")
        }
        temp_ip_list <- c(temp_ip_list, temp_item)
      }
      ip_list <- c(ip_list, itempool(temp_ip_list))
    } else stop("This model has not been implemented yet.")
  }
  ip <- do.call("c", ip_list)
  return(ip[item_models$item_id])
}


############################################################################@###
############################################################################@###
################### flexmirt_read_scores ###################################@###
############################################################################@###
############################################################################@###
#' Read flexMIRT scores
#'
#' @noRd
#'
flexmirt_read_scores <- function(
  scoring_method = "EAP", # EAP/MAP/ML/SSC/MI
  target_dir = getwd(),
  analysis_name = "flexMIRT_calibration") {
  if (is.null(scoring_method)) return(NULL)
  score_fn <- file.path(target_dir, paste0(analysis_name, "-sco.txt"))
  text <- readLines(score_fn, warn = FALSE)
  fn <- function(x) {
    y <- strsplit(x, split = " ")[[1]];
    as.data.frame(lapply(as.list(y[y != ""]), utils::type.convert,
                         as.is = TRUE),
                  fix.empty.names = FALSE, check.names = TRUE)
  }
  output <- lapply(text, fn)
  output <- do.call("rbind.data.frame", output)
  if (scoring_method %in% c("EAP", "MAP")) {
    colnames(output) <- c("group", "no", "examinee_id", "theta", "se")
  } else if (scoring_method %in% c("ML")) {
    # "the first column indicates group membership and the second column
    # provides the flexMIRT assigned observation number. The third column
    # reports the number of iterations to reach the reported score. The fourth
    # and fifth columns are the ML theta estimate and the associated SE,
    # respectively."(FlexMIRT User Manual v.3.5, p.23)
    colnames(output) <- c("group", "no", "examinee_id", "num_iterations",
                          "theta", "se")
  } else {
    stop("Invalid 'scoring_method'. This method has not been implemented yet.")
  }

  if (requireNamespace("tibble")) output <- tibble::as_tibble(output)
  return(output)
}


############################################################################@###
############################################################################@###
################### flexmirt_read_gof ######################################@###
############################################################################@###
############################################################################@###
#' Read flexMIRT Goodness-of-fit statistics
#'
#'
#' @noRd
#'
flexmirt_read_gof <- function(target_dir = getwd(),
                              analysis_name = "flexMIRT_calibration") {
  whole_text <- flexmirt_read_main_output_file(target_dir = target_dir,
                                               analysis_name = analysis_name)
  output <- list()
  # -2loglikelihood:
  temp_text <- "^( )*-2loglikelihood:( )*"
  output$neg_2_log_likelihood <- as.numeric(
    gsub(temp_text, "", whole_text[grepl(temp_text, whole_text)]))
  # Akaike Information Criterion (AIC):
  temp_text <- "^( )*Akaike Information Criterion \\(AIC\\):( )*"
  output$AIC <- as.numeric(
    gsub(temp_text, "", whole_text[grepl(temp_text, whole_text)]))
  # Bayesian Information Criterion (BIC):
  temp_text <- "^( )*Bayesian Information Criterion \\(BIC\\):( )*"
  output$BIC <- as.numeric(
    gsub(temp_text, "", whole_text[grepl(temp_text, whole_text)]))

  # Marginal reliability for response pattern scores:
  temp_text <- "^( )*Marginal reliability for response pattern scores:( )*"
  output$marginal_reliability <- as.numeric(
    gsub(temp_text, "", whole_text[grepl(temp_text, whole_text)]))

  return(output)
}

############################################################################@###
############################################################################@###
################### est_flexmirt ###########################################@###
############################################################################@###
############################################################################@###
#' Unidimensional Item Calibration via flexMIRT
#'
#' @description \code{est_flexmirt} runs flexMIRT in batch mode. This function
#'   requires flexMIRT program already installed on the Windows machine.
#'   Visit \url{https://vpgcentral.com/software/flexmirt/} for more details
#'   about the software. Even though flexMIRT can run various models, only a
#'   selected set of unidimentional models can be fitted using
#'   \code{est_flexmirt} function.
#'
#' @param x A matrix/data.frame/Response_set object including examinee
#'   item responses. In it's bare form, it can be a matrix of item responses,
#'   where ideally the column names are the item IDs and row names are the
#'   examinee IDs (though neither are necessary).
#' @param model The psychometric model(s) of items. The user can provide an
#'   input in the following three ways: (a) A vector of length one which
#'   represents the model of each item. (b) A vector which has the same length
#'   as the number of items that will be calibrated that specifies the model
#'   of each item. (c) \code{NULL}, the default value, where the program will
#'   check the number of categories of each item. Items with two or fewer
#'   categories will be calibrated using \code{"3PL"} (three-parameter logistic
#'   IRT model), items with more than two categories will be calibrated using
#'   \code{"GRM"} (Graded Response Model).
#'
#'   \describe{
#'     \item{\code{"1PL"}}{One-parameter logistic model.}
#'     \item{\code{"2PL"}}{Two-parameter logistic model.}
#'     \item{\code{"3PL"}}{Three-parameter logistic model.}
#'     \item{\code{"GRM"}}{Graded Response Model}
#'     \item{\code{"GPCM"}}{Generalized Partial Credit Model}
#'   }
#' @param target_dir The directory/folder where the flexMIRT syntax data
#'   files and output will be saved. The default value is the current working
#'   directory, i.e. \code{get_wd()}.
#' @param analysis_name This will be the file names of the data, flexMIRT
#'   syntax file and output files. The default value is
#'   \code{"flexMIRT_calibration"}.
#' @param item_ids A vector of column names or numbers of the \code{x} that
#'   represents the responses. The default value is \code{NULL} where all of
#'   the columns in the data are assumed to be a response matrix (unless
#'   specified by \code{group_var} or \code{examinee_id_var} arguments).
#' @param D Scaling constant. Default value is 1. If it is not equal to 1,
#'   a new line added to constraints to multiply the slope parameter with the
#'   D value specified.
#' @param max_em_cycles A numeric vector of length two specifying the maximum
#'   number of iterations allowed in E- and M-steps. The default value is
#'   \code{c(500, 100)} where there will be maximum 500 iterations allowed in
#'   E-steps and 100 iterations M-steps,
#' @param quadrature A numeric vector of length two specifying the number of
#'   quadrature points and the maximum theta value. The default value is
#'   \code{c(49, 6)} where there will be 49 rectangular quadrature points over
#'   -6 and +6,
#' @param em_tol A numeric vector of length two specifying the convergence
#'   criteria for E- and M-steps. The default value is \code{c(1e-4, 1e-9)}
#'   where convergence criteria for E-steps is 0.0001 and the convergence
#'   criteria for M-step is 1e-9.
#' @param examinee_id_var If examinee IDs are saved in one of the columns of
#'   argument \code{x}, this will be the name of that column. The default value
#'   is \code{NULL}. When the value is \code{NULL}, the program will check the
#'   row names of the data.frame or matrix. If the row names are not
#'   \code{NULL}, the program will use these values as examinee IDs.
#' @param group_var The column name or number that contains group membership
#'   information if multi-group calibration is desired. Ideally, it grouping
#'   variable is represented by single digit integers. If other type of data
#'   provided, an integer value will automatically assigned to the variables.
#'   The default value is \code{NULL}, where no multi-group analysis will be
#'   performed.
#' @param gof A string specifying the extent of Goodness-of-fit indices that
#'   will be calculated and reported. The available options are
#'   \code{"Basic"} (the default value), \code{"Extended"} and
#'   \code{"Complete"}.
#' @param prior A data frame that specifies the priors for the estimated item
#'   parameters. There are two possible options.
#'
#'   Option 1: The same priors will be imposed on all items. The data.frame
#'   should have four columns:
#'   \describe{
#'     \item{\code{"par"}}{The parameter on which prior will be imposed. It
#'       can take the following values: \code{"intercept"} for location
#'       parameters (usually for item difficulty parameters, \code{"slope"} for
#'       item discrimination parameters, \code{"guessing"} for lower asymptote
#'       parameter of 3PL.}
#'     \item{\code{"dist"}}{The distribution of the prior. It can take the
#'       following values: \code{"normal"} for normal distribution,
#'       \code{"lognormal"} for log-normal distribution, and \code{"beta"} for
#'       Beta distribution.}
#'     \item{\code{"v1"}}{A number for the first parameter of the selected
#'       distribution. For normal and log-normal distributions this is the mean
#'       of the distribution. For Beta distribution, this is the alpha-1 value.
#'       So, if the a Beta distribution with alpha = 10 desired, the value of
#'       'v1' should be 11.}
#'     \item{\code{"v2"}}{A number for the second parameter of the selected
#'       distribution. For normal and log-normal distributions this is the
#'       standard deviation of the distribution. For Beta distribution, this is
#'       the beta-1 value. So, if the a Beta distribution with beta = 3 desired,
#'       the value of 'v2' should be 4.}
#'   }
#'
#'   Here is an example:
#'   \code{prior <- data.frame(par = c("intercept", "slope", "guessing"),
#'                             dist = c("normal", "lognormal", "beta"),
#'                             v1 = c(0, 0, 1),
#'                             v2 = c(1, 0.5, 3))}
#'
#'   Option 2: Different priors will be assingned to individual items.
#'     The data.frame should have five columns. In addition to four columns
#'     described above, a column specifiying item's ID should be added.
#'     The column anme shoud be \code{"item_id"} and it's values should
#'     correspond to the item ID's specified in the response data.
#'
#'   Here is an example:
#'   \code{prior <- data.frame(
#'     item_id = c("Item_1", "Item_1", "Item_3", "Item_3", "Item_10"),
#'     par = c("intercept", "slope", "intercept", "slope", "slope"),
#'     dist = c("normal", "lognormal", "normal", "lognormal", "lognormal"),
#'     v1 = c(0, 0, 0, 0, 0),
#'     v2 = c(1, 0.5, 2, 0.6, 0.7))}
#'
#' @param scoring_method A string value representing the method of scoring. The
#'   currently available options are: "EAP" and "MAP".
#' @param additional_options A vector of strings that will be added to the
#'   syntax. For example, when \code{scoring_method = "ML"}, the minimun and
#'   maximum values of theta estimates needs to be specified:
#'   \code{c("MinMLscore = -5;", "MaxMLscore = 5;")}. Or, when estimating "3PL"
#'   parameters \code{"NormalMetric3PL = Yes;"} can be added to get a normal
#'   metric scale. See flexMIRT manual for other options.
#'   The default value is \code{NULL}, where no  additional options will be
#'   added to the syntax.
#'   Note that following additional options will be added by other arguments
#'   of this function, so you don't need to add them in this argument
#'   separately:
#'   \code{"Quadrature =", "MaxE =", "MaxM =", "Etol =", "Mtol =", "Score ="}
#' @param additional_constraints The The default value is \code{NULL}, where no
#'   additional constraints will be added to the syntax except the constraints
#'   that will be added for models such as \code{"Rasch"} or \code{"1PL"}.
#'
#'   Examples of additional constraints can be:
#'   \itemize{
#'     \item Guessing parameter. For example, if the items are multiple choice
#'       and there are four possible choices, the prior distribution of the
#'       pseudo-guessing parameter can be set to Beta(1, 3), where parameters
#'       alpha = 2 (2 - 1 = 1) and beta = 4 (4-1 = 3). This will
#'       correspond to prior sample size of 4 and prior mode of
#'       (1 / (3+1) = 0.25). You can add the following line to the
#'       \code{additional_constraints} (see Example 2 below.):
#'
#'       \code{Prior (Item_1-Item_35), Guessing : Beta(1.0,3.0);}
#'
#'       The distribution can be different, for example, for Normal distribution
#'       provide mean and standard deviation, for Log-normal distribution
#'       provide mean and standard deviation in logarithmic scale.
#'
#'       For example, in the code below, a normal prior with mean -1.09 and
#'       standard deviation 0.5 is imposed on the \emph{logit} of the guessing
#'       parameters:
#'       \code{Prior (Item_1-Item_35), Guessing : Normal(-1.09,0.5);}
#'     \item Slope parameters. The following argument will set
#'       the item slopes for Item_1 to Item_10 equal:
#'       \code{Fix (Item_1-Item_10), Slope;}
#'
#'       Similarly a log-normal prior can be imposed on slope parameters:
#'       \code{Prior (Item_1-Item_35), Slope : logNormal(1, 1.6487);}
#'
#'
#'   }
#' @param flexmirt_exe This is the executable file to run flexMIRT syntax. On
#'   most Windows computers this is the path where "WinFlexMIRT.exe" can be
#'   found. For example:\code{"C:\Program Files\flexMIRT3.5.2\WinFlexMIRT.exe"}.
#'   By default the value is \code{NULL}, the function will search the
#'   relevant locations for "WinFlexMIRT.exe".
#' @param overwrite If TRUE and there is already a BILOG-MG data file in the
#'   target path with the same name, the file will be overwritten.
#' @param show_output_on_console logical (not NA), indicates whether to capture
#'   the output of the command and show it on the R console. The default value
#'   is \code{TRUE}.
#'
#' @return A list containg calibration results
#'   \describe{
#'     \item{"ip"}{An \code{\link{Itempool-class}} object holding the item
#'       parameters. Please check whether model converged (using
#'       \code{...$converged}) before interpreting/using \code{ip}.}
#'     \item{"score"}{A data frame object that holds examinee IDs, ability
#'       estimates and standard error of ability estimates.}
#'     \item{"syntax"}{The syntax file.}
#'     \item{"converged"}{A logical value indicating whether a model has been
#'       converged or not. This value is \code{TRUE} only when both
#'       \code{converged_first_order} and \code{converged_second_order} are
#'       \code{TRUE}.}
#'     \item{"converged_first_order"}{A logical value indicating whether
#'       first-order test indicates convergence. }
#'     \item{"converged_second_order"}{A logical value indicating whether
#'       second-order test indicates convergence.}
#'     \item{"convergence_details"}{A more detailed information about
#'       convergence. This element has two values, "First-order test" and
#'       "Second-order test". Use this information to further judge the
#'       convergence. From flexMIRT user manual (p.11): "the reported
#'       first-order test examines if the gradient has vanished sufficiently for
#'       the solution to be a stationary point. The second-order test tests if
#'       the information matrix is positive definite, a prerequisite for the
#'       solution to be a possible maximum. For the second-order test, reporting
#'       that the solution is a possible maximum simply means that the program
#'       reached a statistically desirable solution. The other possible message
#'       that may be printed for the outcome of the second-order test is
#'       “Solution is not a maximum; caution is advised.” If a warning message
#'       is received for either the first- or second-order test, all parameter
#'       estimates should be taken a provisional and should not be used as final
#'       estimates, for future scoring, etc. but, rather, should be used to
#'       diagnose possible issues with the model/items."
#'       }
#'     \item{"gof"}{The goodness-of-fit statistics.}
#'     \item{"input"}{A list object that stores the arguments that are passed
#'       to the function.}
#'   }
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' \dontrun{
#'
#' #############################################
#' ############## Example 1 - 2PL ##############
#' #############################################
#' # IRT Two-parameter Logistic Model Calibration
#'
#' # Create responses to be used in flexMIRT estimation
#' true_theta <- rnorm(1000)
#' true_ip <- generate_ip(n = 30, model = "2PL")
#' resp <- sim_resp(true_ip, true_theta)
#' # The following line will run flexMIRT, estimate 2PL model and put the
#' # analysis results in the target directory:
#' fm_calib <- est_flexmirt(x = resp, model = "2PL",
#'                          target_dir = "C:/Temp/Analysis", overwrite = TRUE)
#' # Check whether the calibration converged
#' fm_calib$converged
#' fm_calib$convergence_details
#'
#' # Get the estimated item pool
#' fm_calib$ip
#'
#' # See the BILOG-MG syntax
#' cat(fm_calib$syntax, sep = "\n")
#'
#' # Get goodness-of-fit statistics and marginal reliability:
#' fm_calib$gof
#'
#' # Get estimated scores
#' head(fm_calib$score)
#'
#' # Compare true and estimated abilities
#' plot(true_theta, fm_calib$score$theta, xlab = "True Theta",
#'      ylab = "Estimated theta")
#' abline(a = 0, b = 1, col = "red", lty = 2)
#'
#' # Compare true item parameters
#' plot(true_ip$a, fm_calib$ip$a, xlab = "True 'a'", ylab = "Estimated 'a'")
#' abline(a = 0, b = 1, col = "red", lty = 2)
#'
#' plot(true_ip$b, fm_calib$ip$b, xlab = "True 'b'", ylab = "Estimated 'b'")
#' abline(a = 0, b = 1, col = "red", lty = 2)
#'
#' mean(fm_calib$score$theta)
#'
#' #############################################
#' ############## Example 2 - 3PL ##############
#' #############################################
#' # IRT Three-parameter Logistic Model Calibration with D = 1.7
#'
#' # Create responses to be used in flexMIRT estimation
#' true_theta <- rnorm(5000)
#' true_ip <- generate_ip(n = 35, model = "3PL", D = 1)
#' resp <- sim_resp(true_ip, true_theta)
#'
#' # The following line will run 3PL calibration via flexMIRT:
#' fm_calib <- est_flexmirt(
#'   x = resp,
#'   model = "3PL",
#'   max_em_cycles = c(1000, 200),
#'   prior = data.frame(par = c("intercept", "slope", "guessing"),
#'                      dist = c("normal", "lognormal", "beta"),
#'                      v1 = c(0, 0, 1),
#'                      v2 = c(1, 0.5, 3)),
#'   target_dir = "C:/Temp/Analysis",
#'   overwrite = TRUE)
#'
#' # Check whether the calibration converged
#' fm_calib$converged
#'
#' fm_calib$convergence_details
#'
#' # Get the estimated item pool
#' fm_calib$ip
#'
#' # Get goodness-of-fit statistics and marginal reliability:
#' fm_calib$gof
#'
#' # Get estimated scores
#' head(fm_calib$score)
#'
#' # Compare true and estimated abilities
#' plot(true_theta, fm_calib$score$theta, xlab = "True Theta",
#'      ylab = "Estimated theta")
#' abline(a = 0, b = 1, col = "red", lty = 2)
#'
#' # Compare true item parameters
#' plot(true_ip$a, fm_calib$ip$a, xlab = "True 'a'", ylab = "Estimated 'a'")
#' abline(a = 0, b = 1, col = "red", lty = 2)
#'
#' plot(true_ip$b, fm_calib$ip$b, xlab = "True 'b'", ylab = "Estimated 'b'")
#' abline(a = 0, b = 1, col = "red", lty = 2)
#'
#' mean(fm_calib$score$theta)
#'
#' } # end dontrun

est_flexmirt <- function(
  x = NULL,
  model = NULL,
  target_dir = getwd(),
  analysis_name = "flexMIRT_calibration",
  item_ids = NULL,
  D = 1,
  max_em_cycles = c(500, 100),
  quadrature = c(49, 6),
  em_tol = c(1e-4, 1e-9),
  prior = NULL,
  gof = "Basic",
  examinee_id_var = NULL,
  group_var = NULL,
  scoring_method = NULL, # "EAP", # EAP/MAP/ML/SSC/MI
  additional_options = NULL,
  additional_constraints = NULL,
  flexmirt_exe  = NULL,
  overwrite = FALSE,
  show_output_on_console = TRUE
  ) {

  # x = NULL
  # model = NULL
  # target_dir = getwd()
  # analysis_name = "flexMIRT_calibration"
  # item_ids = NULL
  # examinee_id_var = NULL
  # group_var = NULL
  # scoring_method = "EAP"
  # additional_options = NULL
  # additional_constraints = NULL
  # flexmirt_exe  = NULL
  # overwrite = FALSE
  # show_output_on_console = TRUE
  result <- list(ip = NULL,
                 score = NULL,
                 syntax = NULL,
                 input = c(as.list(environment()), call = match.call())
                 )
  result$input$x <- NULL


  # RDS file where the output of the calibration will be saved.
  result_fn <- file.path(target_dir, paste0(analysis_name, ".rds"))
  if (!overwrite && file.exists(result_fn)) {
    temp_result <- readRDS(result_fn)
    # The names in input that can differ but not affect calibration results
    temp <- c("overwrite", "call", "show_output_on_console")
    if (identical(temp_result$input[!names(temp_result$input) %in% temp],
                  result$input[!names(temp_result$input) %in% temp]))
      return(temp_result)
  }


  # Check the path of WinFlexMIRT.exe
  flexmirt_exe <- flexmirt_find_exe(flexmirt_exe)

  # If overwrite is TRUE, delete important output files:
  if (overwrite) {
    extensions <- c(".flexmirt", ".dat", "-cov.txt", "-inf.txt", "-irt.txt",
                    "-pcc.txt", "-prm.txt", "-sco.txt", "-ssc.txt")
    suppressWarnings(file.remove(
      file.path(target_dir, paste0(analysis_name, extensions))))
  }

  syntax_output <- flexmirt_create_syntax(
    x = x,
    model = model,
    D = D,
    target_dir = target_dir,
    max_em_cycles = max_em_cycles,
    quadrature = quadrature,
    em_tol = em_tol,
    gof = gof,
    prior = prior,
    analysis_name = analysis_name,
    item_ids = item_ids,
    examinee_id_var = examinee_id_var,
    group_var = group_var,
    scoring_method = scoring_method,
    additional_options = additional_options,
    additional_constraints = additional_constraints,
    overwrite = overwrite)

  result$syntax <- syntax_output$syntax

  #################################@###
  ##### Run calibration ###########@###
  #################################@###
  command <- paste0("\"", normalizePath(flexmirt_exe), "\" -r \"",
                    syntax_output$syntax_file_name, "\"")

  system("cmd.exe", input = command, wait = TRUE,
         show.output.on.console = show_output_on_console)

  #################################@###
  ##### Check Results #############@###
  #################################@###

  # Check convergence
  convergence_output <- flexmirt_check_convergence(
    target_dir = target_dir, analysis_name = analysis_name)
  result$converged <- convergence_output$converged
  result$converged_first_order <- convergence_output$converged_first_order
  result$converged_second_order <- convergence_output$converged_second_order
  result$convergence_details <- convergence_output$details
  if (result$converged_first_order) {
    result$ip <- flexmirt_read_item_par(target_dir = target_dir, D = D,
                                        models = model,
                                        analysis_name = analysis_name)
    result$score <- flexmirt_read_scores(scoring_method = scoring_method,
                                         target_dir = target_dir,
                                         analysis_name = analysis_name)
    result$gof <- flexmirt_read_gof(target_dir = target_dir,
                                    analysis_name = analysis_name)
  }
  attr(result, "class") <- "flexmirt_output"
  saveRDS(object = result, file = result_fn)
  return(result)
}

# @param n_categories A vector representing the number of response categories
#   observed in the response data.
