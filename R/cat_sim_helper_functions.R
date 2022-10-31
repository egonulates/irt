
###############################################################################@
############################# print.cat_design #################################
###############################################################################@
#' Prints cat_design objects.
#'
#' @param x A \code{cat_design} object.
#' @param ... further arguments passed to or from other methods.
#' @param verbose If \code{TRUE}, a list object will be returned for each
#'          step.
#'
#' @keywords internal
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @method print cat_design
#'
#' @examples
#' ip <- generate_ip(n = 5)
#' cd <- create_cat_design(ip = ip, next_item_rule = 'random',
#'                         termination_rule = 'min_item',
#'                         termination_par = list('min_item' = 5))
#' cd
print.cat_design <- function(x, ..., verbose = FALSE) {
  if (verbose) {
    NextMethod()
    return(NULL)
  }
  # This function extracts the sub parameters from 'ability_est_par' or
  # 'termination_par'.
  # 'g_par_name': is the general parameter name. It should be either
  #    'ability_est_par'  or 'termination_par'
  # 'title': The title that will be printed for common section.
  # 'preamble_for_df': this is the preamble that will be put before the
  #   parameter name in steps_df data frame.
  process_parameters <- function(g_par_name, steps_df, steps_list,
                                 title, preamble_for_df) {
    # Extract the prior parameter names for each step
    par_names <- lapply(x$step, function(y) names(y[[g_par_name]]))
    par_names_union <- unique(unlist(par_names))
    par_names_intersection <- Reduce(intersect, par_names)
    common_parameters_exists <- FALSE
    if (length(par_names_intersection) > 0)
      for (pp_name in par_names_intersection)
        # Extract all parameters with the name pp_name
        # Check whether all of parameters are the same
        if (length(unique(lapply(x$step,
                                 function(y) y[[g_par_name]][[pp_name]]))) == 1)
          common_parameters_exists <- TRUE
    if (common_parameters_exists)
      cat(paste0(title, ": \n"))
    if (!is.null(par_names_intersection))
      max_char <- max(nchar(par_names_intersection))
    # Deal with the common prior parameter elements, either put them in the
    # common parameter section or put them in steps_df
    for (pp_name in par_names_intersection) {
      # Extract all parameters with the name pp_name
      temp <- lapply(x$step, function(y) y[[g_par_name]][[pp_name]])
      # Check whether all of parameters are the same
      if (length(unique(temp)) == 1) {
        temp <- unique(temp)[[1]]
        if (class(temp) %in% printable_classes) {
          cat(paste0("    ", sprintf(paste0("%", max_char, "s"), pp_name), ": ",
                     print_with_quotes(temp), "\n"))
          # cat(paste0("    ", pp_name, ": ", print_with_quotes(temp), "\n"))
        } else if (is(temp, "list") &&
                   all(sapply(temp, class) %in% printable_classes)) {
          cat(sprintf(paste0("    %", max_char, "s: \n"), pp_name))
          # cat(sprintf("    %s: \n", pp_name))
          for (i in seq_len(length(temp)))
            cat(paste0("        ", sprintf(paste0("%", max_char, "s:"),
                                           names(temp)[i]), ": ",
                       print_with_quotes(temp[[i]]), "\n"))
            # cat(paste0("        ", names(temp)[i], ": ",
            #            print_with_quotes(temp[[i]]), "\n"))
        } else {
          cat(sprintf(paste0("    %", max_char, "s: '\n"), pp_name))
          # cat(sprintf("    %s: '\n", pp_name))
          # cat(sprintf("    %s: '\n", pp_name))
          print(temp)
        }
      } else {
        # If all of them are printable classes add them to the steps_df
        # otherwise add them to steps_list
        if (all(sapply(temp, class) %in% printable_classes)) {
          steps_df[, paste0(preamble_for_df, "_", pp_name)] <- unlist(temp)
        } else {
          for (i in seq_len(number_of_steps))
            steps_list[[i]][pp_name] <- temp[[i]]
        }
      }
    }
    # Deal with the elements that are not common accross the steps
    for (pp_name in setdiff(par_names_union, par_names_intersection)) {
      # Add them directly to steps_list
      for (i in seq_len(number_of_steps))
        steps_list[[i]][pp_name] <- x$step[[i]][[g_par_name]][pp_name]
    }
    return(list(steps_df = steps_df, steps_list = steps_list))
  }

  print_with_quotes <- function(x)
    ifelse(test = is(x, "character"), yes = paste0("'", x, "'"), no = paste0(x))

  number_of_steps <- length(x$step)
  steps_df <- data.frame(step = seq_len(number_of_steps))
  steps_list <- vector("list", number_of_steps)
  printable_classes <-  c('logical', 'character', 'integer', 'numeric')

  cat("CAT Design\n")
  cat("--------------------------------------------------\n")
  if (!is.null(x$title))
    cat(sprintf("Title: '%s'\n", x$title))
  cat(sprintf("Item Pool Size: %d\n", length(x$ip)))
  cat(sprintf("Maximum Test Length: %d\n", x$max_test_length))

  ## First Item Parameters ##
  cat(sprintf("First Item Rule: '%s'\n", x$first_item_rule))
  cat("First Item Parameters: \n")
  for (i in seq_len(length(x$first_item_par)))
    if (class(x$first_item_par[[i]]) %in% printable_classes) {
      cat(paste0("   ", names(x$first_item_par)[i], ": ",
                 print_with_quotes(x$first_item_par[[i]]), "\n"))
    } else {
      cat(sprintf("    %s: \n", names(x$first_item_par)[i]))
      print(x$first_item_par[[i]])
    }

  # Next item rule
  temp <- vapply(x$step, function(y) y$next_item_rule, character(1))
  if (all(temp == temp[1])) {
    cat(sprintf("Next Item Rule: %s\n", print_with_quotes(temp[1])))
  } else steps_df$next_item_rule <- temp

  temp <- process_parameters(g_par_name = 'next_item_par',
                             steps_df = steps_df,
                             steps_list = steps_list,
                             title = "Next Item Parameters",
                             preamble_for_df = "NIP")
  steps_df <- temp$steps_df
  steps_list <- temp$steps_list

  # Ability Estimation Rule
  temp <- vapply(x$step, function(y) y$ability_est_rule, character(1))
  if (all(temp == temp[1])) {
    cat(sprintf("Ability Estimation Rule: %s\n", print_with_quotes(temp[1])))
  } else steps_df$ability_est_rule <- temp

  ## Ability Estimation Parameters ##
  temp <- process_parameters(g_par_name = 'ability_est_par',
                             steps_df = steps_df, steps_list = steps_list,
                             title = "Ability Estimation Parameters",
                             preamble_for_df = "AEP")
  steps_df <- temp$steps_df
  steps_list <- temp$steps_list

  ## Final Ability Estimation Parameters ##
  if (!is.null(x$final_ability_est_rule)) {
    cat(sprintf("Final Ability Estimation Rule: '%s'\n",
                x$final_ability_est_rule))
    cat("Final Ability Estimation Parameters: \n")
    for (i in seq_len(length(x$final_ability_est_par)))
      if (class(x$final_ability_est_par[[i]]) %in% printable_classes) {
        cat(paste0("    ", names(x$final_ability_est_par)[i], ": ",
                   print_with_quotes(x$final_ability_est_par[[i]]), "\n"))
      } else {
        cat(sprintf("    %s: \n", names(x$final_ability_est_par)[i]))
        print(x$final_ability_est_par[[i]])
      }
  }

  ## Test Termination Rule ##
  cat("Test Termination Rules and Parameters (in order): \n")
  max_char <- max(nchar(unlist(sapply(x$termination_par, names))))
  for (i in seq_len(length(x$termination_par)))
    if (length(x$termination_par[[i]]) > 1) {
      cat(paste0("    (Rule ", i, ") ", names(x$termination_par)[i], ": \n"))
      for (j in 1:length(x$termination_par[[i]])) {
        cat(paste0("        ", sprintf(paste0("%", max_char, "s: "),
                                       names(x$termination_par[[i]])[j]),
                   print_with_quotes(x$termination_par[[i]][[j]]), "\n"))
      }
    } else if (length(x$termination_par[[i]]) == 1) {
      cat(paste0("    (Rule ", i, ") ", names(x$termination_par)[i], ": ",
                 print_with_quotes(x$termination_par[[i]]),
                 "\n"))

    }

    # if (class(x$termination_par[[i]]) %in% printable_classes) {
    #   cat(paste0("   (Rule ", i, ") ", names(x$termination_par)[i], ": ",
    #              print_with_quotes(x$termination_par[[i]]), "\n"))
    # } else {
    #   cat(sprintf("    (Rule %d) %s: \n", i, names(x$termination_par)[i]))
    #   print(x$termination_par[[i]])
    # }

  # Print irregular parameters
  if (ncol(steps_df) > 1) {
    cat("\nStep Arguments that are not Common:\n")
    print(steps_df)
  }
  for (i in number_of_steps:1)
    if (is.null(steps_list[[i]])) steps_list[[i]] <- NULL
  if (length(steps_list) > 0) {
    cat("\nUncommon and Irregular Step Arguments:\n")
    print(steps_list)
  }
}


###############################################################################@
############################# c.cat_design #####################################
###############################################################################@
#' Concatenate 'cat_design' objects
#'
#' @param x A \code{cat_design} class object.
#' @param ... Remaining \code{cat_design} class objects.
#'
#' @return A list of \code{cat_design} objects.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#'
#' ip <- generate_ip(n = 20)
#' cd1 <- create_cat_design(ip = ip,
#'                          termination_rule = c('max_item'),
#'                          termination_par = list(max_item = 5))
#' cd2 <- create_cat_design(ip = ip,
#'                          termination_rule = c('max_item'),
#'                          termination_par = list(max_item = 9))
#' cd <- c(cd1, cd2)
c.cat_design <- function(x, ...) {
  args = list(x, ...)
  if (!all(sapply(args, inherits, c("cat_design"))))
    stop("All of the elements should be 'cat_design' class.", .call = FALSE)
  return(args)
}


###############################################################################@
############################# summary.cat_output ###############################
###############################################################################@
#' Summarizes the raw output of cat_sim
#'
#' @description This function summarizes a list consist of cat_output objects.
#' It returns a summary data frame of the CAT simulation.
#'
#' @param object This is a cat_output object or a list object containing
#'   elements that are "cat_output" class.
#' @param ... Additional arguments.
#' @param cols The variables that will be included in the summary. There should
#'          be at least one column. Available columns are:
#'          \describe{
#'            \item{examinee_id}{Examinee ID's if named true theta vector
#'              has been provided to \code{cat_sim()} function.}
#'            \item{true_ability}{True ability of the simulee}
#'            \item{est_ability}{Ability Estimate}
#'            \item{se}{Standard Error of the ability estimate}
#'            \item{test_length}{Test length.}
#'            \item{bias}{The difference between true ability and ability
#'              estimate}
#'            \item{mse}{Mean squared error}
#'            \item{mean_qip}{Mean of Quality of Item Pool Index.
#'                    See \code{qip_index()} function for details.}
#'            \item{median_qip}{Median of Quality of Item Pool Index.
#'                    See \code{qip_index()} function for details.}
#'            \item{min_qip}{Minimum value of Quality of Item Pool Index.
#'                    See \code{qip_index()} function for details.}
#'            \item{max_qip}{Maximum value of Quality of Item Pool Index.
#'                    See \code{qip_index()} function for details.}
#'          }
#' @return This function returns a summary data frame of adaptive tests. Each
#' row will represent a different adaptive test.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @seealso \code{\link{cat_sim}}
#'
#' @examples
#' n <- 100 # number of items
#' ip <- generate_ip(n = n,
#'                   content = sample(c("Algebra", "Arithmetic", "Geometry"),
#'                                    n, replace = TRUE))
#' cd <- create_cat_design(ip = ip, next_item_rule = 'mfi',
#'                         termination_rule = 'max_item',
#'                         termination_par = list(max_item = 10))
#' cat_data <- cat_sim(true_ability = rnorm(5), cd = cd)
#' summary(cat_data)
#'
#' # Get only selected columns
#' summary(cat_data, cols = c("examinee_id", "true_ability", "est_ability",
#'                            "bias"))
#' summary(cat_data, cols = c("examinee_id", "true_ability", "est_ability",
#'                            "mean_qip", "median_qip", "min_qip"))
summary.cat_output <- function(
  object, ...,
  cols = c("examinee_id", "true_ability", "est_ability", "se", "test_length")) {
  # Check whether it is one or multiple CAT outputs
  if (is(object, "cat_output")) { # There is a single cat_output element
     nrows <- 1
  } else if (all(sapply(object, is, "cat_output"))) { # a list of cat_output
    nrows <- length(object)
  } else
    stop("All of the elements of 'cat_output' should be 'cat_output' class.")
  ncols <- length(cols)

  # if (!all(sapply(x, FUN = function(y) is(y, "cat_output"))))
  #   stop("All of the elements of 'cat_output' should be 'cat_output' class.")
  if (length(cols) < 1) stop("There should be at least one column.")
  if (!all(cols %in% c("examinee_id", "true_ability", "est_ability", "se",
                       "test_length", "bias", "mse",
                       "mean_qip", "median_qip", "min_qip", "max_qip")))
    stop("Inadmissable column. 'cols' should be composed of one of the
         following: 'true_ability', 'est_ability', 'se', 'test_length', 'bias',
         'mse', 'mean_qip', 'median_qip', 'min_qip', 'max_qip'.")
  cat_summary <- data.frame(matrix(vector(), nrows, ncols,
                                   dimnames = list(c(), cols)),
                            stringsAsFactors = FALSE)
  # Function returns the true ability
  get_examinee_id <- function() {
    if (nrows == 1) {
      return(object$examinee_id)
      } else
      return(unlist(sapply(object, `[[`, "examinee_id")))
  }
  # Function returns the true ability
  get_true_ability <- function() {
    if (nrows == 1) {
      return(unlist(object$true_ability))
      } else
      return(unlist(sapply(object, `[[`, "true_ability")))
  }
  get_est_ability <- function() {
    if (nrows == 1) {
      eh <- object$est_history
      return(eh[[length(eh)]]$est_after)
    } else {
      return(sapply(object, function(x) {
        eh <- x$est_history; eh[[length(eh)]]$est_after}))
    }
  }
  for (col in cols) {
    switch(
      col,
      "examinee_id" = {
        cat_summary$examinee_id <- get_examinee_id()
      },
      "true_ability" = {
        cat_summary$true_ability <- get_true_ability()
      },
      "est_ability" = {
        cat_summary$est_ability <- get_est_ability()
      },
      "se" = {
        if (nrows == 1) {
          eh <- object$est_history
          cat_summary$se <- eh[[length(eh)]]$se_after
        } else {
          cat_summary$se <- sapply(
            object, function(x) {
              eh = x$est_history; eh[[length(eh)]]$se_after
              })
        }
      },
      "test_length" = {
        if (nrows == 1) {
          cat_summary$test_length <- length(object$est_history)
        } else {
          cat_summary$test_length <- sapply(
            object, function(x) {length(x$est_history)})
        }
      },
      "bias" = {
        cat_summary$bias <- get_est_ability() - get_true_ability()
      },
      "mse" = {
        cat_summary$mse <- (get_est_ability() - get_true_ability())^2
        },
      "mean_qip" = {
        cat_summary$mean_qip <- qip_index(cat_sim_output = object,
                                          summary_func = "mean")
        },
      "median_qip" = {
        cat_summary$median_qip <- qip_index(cat_sim_output = object,
                                          summary_func = "median")
        },
      "min_qip" = {
        cat_summary$min_qip <- qip_index(cat_sim_output = object,
                                          summary_func = "min")
        },
      "max_qip" = {
        cat_summary$max_qip <- qip_index(cat_sim_output = object,
                                          summary_func = "max")
        }
    )
  }
  if (requireNamespace("tibble")) {
    return(tibble::as_tibble(cat_summary))
  } else return(cat_summary)
}


###############################################################################@
############################# $.cat_output #####################################
###############################################################################@
#' Prints the raw output of cat_sim
#' @description This function prints a data frame that shows all of the steps of
#'   a CAT for a single examinee.
#'
#' @param x This is a cat_output object which has \code{"cat_output"} class.
#' @param name Name of the field.
#'   Available options:
#'   \describe{
#'     \item{\strong{\code{"ip"}}}{Extract items administered to examinee}
#'     \item{\strong{\code{"resp"}}}{Extract responses}
#'     \item{\strong{\code{"testlet"}}}{Extract testlets administered}
#'     \item{\strong{\code{"est_before"}}}{Extract ability estimate
#'       before administration of an item.}
#'     \item{\strong{\code{"item_id"}}}{Extract administered item IDs.}
#'     \item{\strong{\code{"est_after"}}}{Extract ability estimate
#'       after administration of an item.}
#'     \item{\strong{\code{"se_before"}}}{Extract standard error
#'       before administration of an item.}
#'     \item{\strong{\code{"se_after"}}}{Extract standard error
#'       after administration of an item.}
#'     \item{\strong{\code{"true_theta"}}}{Extract true theta as a vector}
#'     \item{\strong{\code{"test_length"}}}{Extract test length of the adaptive
#'       test}
#'     \item{\strong{\code{"final_est"}}}{Extract final ability estimate.}
#'     \item{\strong{\code{"final_se"}}}{Extract final standard error.}
#'     }
#' @return See the 'name' argument above for possible return values.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @seealso \code{\link{cat_sim}}
#'
#' @examples
#' n <- 20 # number of items
#' ip <- generate_ip(n = n)
#' cd <- create_cat_design(ip = ip, next_item_rule = 'mfi',
#'                         termination_rule = 'max_item',
#'                         termination_par = list(max_item = 10))
#' cat_data <- cat_sim(true_ability = rnorm(1), cd = cd)
#' cat_data
#' cat_data$resp # Extract responses to administered items
#' cat_data$ip # Administered items
#' cat_data$item_id # Extract administered item IDs
#' cat_data$est_before # Ability estimates before the administration of an item
#' cat_data$est_after # Ability estimates after the administration of an item
#' cat_data$true_theta # True ability that generates examinee responses
#'
#' # Simulation with more than one simulees
#' n <- 20 # number of items
#' ip <- generate_ip(n = n)
#' cd <- create_cat_design(ip = ip, next_item_rule = 'mfi',
#'                         termination_rule = 'max_item',
#'                         termination_par = list(max_item = 10))
#' n_examinee <- 3
#' cat_data_list <- cat_sim(true_ability = rnorm(n_examinee), cd = cd)
#' cat_data_list[[3]]$item_id
#' cat_data_list[[2]]$item_id
#' cat_data_list[[3]]$resp
#' cat_data_list[[2]]$resp
#' cat_data_list[[2]]$test_length
#' cat_data_list[[2]]$final_est
#' cat_data_list[[2]]$final_se
`$.cat_output` <- function(x, name) {
  switch(
    name,
    "ip" = itempool(lapply(x$est_history, function(x) x$item)),
    "resp" = sapply(x$est_history, function(x) x$resp),
    "testlet" = sapply(x$est_history, function(x) x$testlet),
    "item_id" = sapply(x$est_history, function(x) x$item@item_id),
    "est_before" = sapply(x$est_history, function(x) x$est_before),
    "est_after" = sapply(x$est_history, function(x) x$est_after),
    "se_before" = sapply(x$est_history, function(x) x$se_before),
    "se_after" = sapply(x$est_history, function(x) x$se_after),
    "test_length" = length(x$est_history),
    "final_est" = utils::tail(x$est_history, 1)[[1]]$est_after,
    "final_se" = utils::tail(x$est_history, 1)[[1]]$se_after,
    # "true_ability" = ,
    "true_theta" = x$true_ability[[1]],
    {
      x[[name]]
    }
  )
}


###############################################################################@
############################# as.data.frame.cat_output #########################
###############################################################################@
#' Convert a \code{cat_output} object into a \code{data.frame}.
#'
#' @description This function converts \code{cat_output} objects to a
#'   \code{data.frame} object.
#'
#' @param x An \code{cat_output} object
#' @param row.names \code{NULL} or a character vector giving the row names for
#'   the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting
#'   column names
#' @param ... additional arguments
#'
#' @return A data frame with the following columns:
#'   \describe{
#'     \item{true_ability}{True ability of the simulee}
#'     \item{est_before}{Ability estimate before administration of an item.}
#'     \item{se_before}{Standard error before administration of an item.}
#'     \item{testlet_id}{Administered testlet's ID.}
#'     \item{item_id}{Administered item's ID.}
#'     \item{resp}{Response to the item}
#'     \item{est_after}{Ability estimate after the administration of an item.}
#'     \item{se_after}{Standard error after administration of an item.}
#'   }
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' ip <- generate_ip(n = 40)
#' cd <- create_cat_design(ip = ip, next_item_rule = 'mfi',
#'                         termination_rule = 'max_item',
#'                         termination_par = list(max_item = 10))
#' cat_data <- cat_sim(true_ability = rnorm(1), cd = cd)
#' as.data.frame(cat_data)
#'
as.data.frame.cat_output <- function(x, row.names = NULL, optional = FALSE,
                                     ...)  {
  args <- list(...)

  est_history <- x$est_history

  data.frame(
    # the following line was added for compatibility with older cat_output
    # objects that does not have examinee_id field.
    examinee_id = ifelse(is.null(x$examinee_id), NA, x$examinee_id),
    true_ability = x$true_ability[[1]],
    est_before = sapply(est_history, `[[`, "est_before"),
    se_before = sapply(est_history, `[[`, "se_before"),
    testlet_id = sapply(est_history, function(i)
      ifelse(is.null(i$testlet), NA, i$testlet$testlet_id)),
    item_id = sapply(est_history, function(i)
      ifelse(is.null(i$item), NA, i$item$item_id)),
    resp = sapply(est_history, `[[`, "resp"),
    est_after = sapply(est_history, `[[`, "est_after"),
    se_after = sapply(est_history, `[[`, "se_after"),
    stringsAsFactors = FALSE
  )
}

###############################################################################@
############################# .print.cat_output ################################
###############################################################################@
#' Prints the raw output of cat_sim
#' @description This function prints a data frame that shows all of the steps of
#'   a CAT for a single examinee.
#'
#' @param x This is a cat_output object which has "cat_output" class.
#' @param ... Additional arguments.
#' @param silent If TRUE, no output will be printed on the console, only
#'   a data frame will be returned.
#'
#' @keywords internal
#'
#' @noRd
#'
#' @author Emre Gonulates
#'
#' @seealso \code{\link{cat_sim}}
#'
.print.cat_output <- function(x, ..., n = NULL, print_header = TRUE,
                              base_print = FALSE, silent = FALSE) {
  # Check whether it is one or multiple CAT outputs
  if (!is(x, "cat_output"))
    stop("x should be 'cat_output' class.")
  est_history <- x$est_history
  n_items <- length(est_history)

  output <- as.data.frame(x)
  output$examinee_id <- NULL
  output$true_ability <- NULL

  if (!silent) {
    if (is.null(n)) n <- ifelse(nrow(output) <= 20, nrow(output), 10)
    n <- max(1, n)
    n <- min(n, nrow(output))
    print_tibble <- !base_print &&
      requireNamespace("pillar", quietly = TRUE) &&
      requireNamespace("tibble", quietly = TRUE)

    ### Print Header ###
    header_text <- c()
    if (print_header)
      header_text <- c(header_text,
                       "An object of class 'cat_output'.\n",
                       paste0("Examinee ID: \"", x$examinee_id, "\"\n"),
                       paste0("True Ability: ", round(x$true_ability[[1]], 4),
                              "\n\n")
                       )

    if (all(is.na(output$testlet_id))) output$testlet_id <- NULL

    if (n < 1) {
      result <- ""
    } else if (n < nrow(output)) {
      result <- output[1:n, ]
    } else result <- output

    if (print_tibble) {
      cat(pillar::style_subtle(header_text), sep = "")
    } else {
      cat(header_text, sep = "")
    }

    # First try tibble/pillar, if not use base R to print administered items
    if (print_tibble) {
      setup_tbl <- pillar::tbl_format_setup(tibble::tibble(result),
                                            width = NULL,
                                            n = n,
                                            max_extra_cols = NULL,
                                            max_footer_lines = NULL)
      print(setup_tbl$body)
      if (setup_tbl$extra_cols_total > 0) {
        footer_extra_col_text <- paste0(
          setup_tbl$extra_cols_total, " more variable",
          ifelse(setup_tbl$extra_cols_total > 1, "s", ""), ": '",
          paste0(names(setup_tbl$extra_cols), collapse = "', '"), "'")
      } else footer_extra_col_text <- ""

      # Base R print:
    } else {
      print(result)
    }

    ### Print Footer ###

    if (n < 1) {
      # This should never happen, n is at least 1.
    } else if (n < nrow(output)) {
      if (print_tibble && setup_tbl$extra_cols_total > 0) {
        text_after_df <- paste0(
          "# ... with ", nrow(output) - n, " more administered items, and ",
          footer_extra_col_text, "\n")
      } else
        text_after_df <- paste0(
          paste0("# ... with ", nrow(output) - n,
                 " more administered items.\n"))
    } else {
      if (print_tibble && setup_tbl$extra_cols_total > 0) {
        text_after_df <- paste0("# ... with ", footer_extra_col_text, "\n")
      } else {
        text_after_df <- paste0()
      }
    }

    if (print_tibble) {
      cat(pillar::style_subtle(text_after_df))
    } else {
      cat(text_after_df)
    }

    # cat("An object of class 'cat_output'.\n")
    # cat(paste0("Examinee ID: \"", x$examinee_id, "\"\n"))
    # cat(paste0("True Ability: ", x$true_ability, "\n\n"))
    # print(output)
  }
  invisible(output)
}

###############################################################################@
############################# show.cat_output ##################################
###############################################################################@
#' This method shows an "cat_output" class object
#'
#' @param object An 'cat_output' class object that will be showed.
#'
#' @export
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
show.cat_output <- function(object) .print.cat_output(object)


###############################################################################@
############################# print.cat_output #################################
###############################################################################@
#' This method prints an "cat_output" class object
#'
#' @param object An 'cat_output' class object that will be printed.
#' @param ... Additional arguments
#' @param n maximum number of administered items to print. Default is
#'   \code{NULL}, where all items are printed if the number of items are
#'   smaller than 20, otherwise only first 10 items are printed.
#' @param print_header Whether to print the object class in the first line.
#' @param base_print Whether to print the \code{cat_output} object
#'   using the base printing capabilities. If FALSE, the function will look at
#'   'tibble' package and tries to print the \code{cat_output} using
#'   that function.
#' @param silent If TRUE, no output will be printed on the console, only
#'   a data frame will be returned.
#'
#' @export
#'
#' @method print cat_output
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
print.cat_output <- function(x, ..., n = NULL, print_header = TRUE,
                             base_print = FALSE, silent = FALSE)
  .print.cat_output(x, ..., n = n, print_header = print_header,
                    base_print = base_print, silent = silent)


###############################################################################@
############################# summary.list #####################################
###############################################################################@
#' If a list object consists of all "cat_output" objects, then it will run
#' summary.cat_output.
#'
#' @param object A list object consists of all "cat_output" objects.
#' @param ... Arguments passed to the \code{summary.cat_output()} function.
#' @return A data frame that summarizes the CAT outputs.
#'
#' @export
#' @keywords internal
#'
#' @author Emre Gonulates
#'
summary.list <- function(object, ...) {
  if (all(sapply(object, class) == "cat_output")) {
    summary.cat_output(object, ...) } else NextMethod()
}


###############################################################################@
############################# get_cat_response_data ############################
###############################################################################@
#' Extract the response from one cat_output object
#'
#' @description This function extracts responses from one \code{cat_output}
#'   object and returns a \code{Response} object.
#'
#' @param cat_sim_output This is a list object containing elements that are
#'   \code{cat_output} class.
#'
#' @return A \code{Response} class object.
#'
#' @noRd
#'
#' @examples
#' ip <- generate_ip(n = 40)
#' cd <- create_cat_design(ip = ip, next_item_rule = 'mfi',
#'                         termination_rule = 'max_item',
#'                         termination_par = list(max_item = 10))
#' cat_data <- cat_sim(true_ability = rnorm(1), cd = cd)
#' get_cat_response_data_single(cat_sim_output = cat_data)
#'
get_cat_response_data_single <- function(cat_sim_output) {

  # it is assumed that `cat_sim_output` is cat_outpu object
  stopifnot(is(cat_sim_output, "cat_output"))

  eh <- cat_sim_output$est_history
  resp <- sapply(eh, "[[", "resp")
  item_ids <- sapply(eh, function(x) x$item$id)
  testlet_ids <- sapply(eh, function(x) ifelse(is.null(x$testlet), NA,
                                               x$testlet$testlet_id))
  if (all(is.na(testlet_ids))) {
    testlet_ids <- NULL
  }
  cs <- summary(cat_sim_output)
  misc <- list(true_ability = cs$true_ability,
               est_ability = cs$est_ability,
               se = cs$se,
               test_length = cs$test_length
  )

  response(score = resp, item_id = item_ids, testlet_id = testlet_ids,
           examinee_id = cat_sim_output$examinee_id, misc = misc)
}


#' Extracts the response data of CAT output.
#'
#' @description This function extracts the response data from a single
#' \code{cat_output} object or a list of \code{cat_output} objects and returns
#' a \code{Response_set} object that contains the administered items of each
#' simulee or a matrix or responses.
#'
#' If \code{cd}, cat design, object is given, then the item pool in the
#' \code{cd} will be used.
#'
#'
#' @param cat_sim_output This is a list object containing elements that are
#'   \code{cat_output} class.
#' @param cd A \code{cat_design} object that is created by function
#'          \code{create_cat_design}.
#' @param output_type A string that specifies the output type. Available
#'   options are \code{"Response_set"} which returns a \code{Response_set}
#'   object and \code{"matrix"} which returns a matrix.
#'   If \code{attach_summary = TRUE} and \code{output_type = "matrix"}, a
#'   data frame will be returned instead of a matrix.
#'   The default value is \code{"Response_set"}.
#' @param remove_na If \code{TRUE}, the columns that are all \code{NA} will be
#'          removed.
#' @param attach_summary If \code{TRUE} and \code{output_type = "matrix"}, the
#'   summary of each CAT will be
#'   attached to the beginning of the response string as columns. The default
#'   value is \code{FALSE}. When \code{output_type = "Response_set"},
#'   CAT summary will automatically added to each Response object of the
#'   output within \code{misc} field.
#' @return Depending on the \code{output_type}, the function returns the
#'   response matrix of adaptive tests. If the input is a list of
#'   \code{cat_output}, then the rows will represent examinees
#'   and columns will represent items.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @seealso \code{\link{cat_sim}}
#'
#' @examples
#' n <- 40 # number of items
#' ip <- generate_ip(n = n)
#' cd <- create_cat_design(ip = ip, next_item_rule = 'mfi',
#'                         termination_rule = 'max_item',
#'                         termination_par = list(max_item = 10))
#' cat_data <- cat_sim(true_ability = rnorm(10), cd = cd)
#' resp_set <- get_cat_response_data(cat_sim_output = cat_data, cd)
#' resp_set
#'
#' # Get the examinee_id of third simulee:
#' resp_set[[3]]$examinee_id
#' # Extract the true theta of the third examinee:
#' resp_set[[3]]$true_ability
#' # Extract the final estimated theta of the third examinee:
#' resp_set[[3]]$est_ability
#' # Extract the final standard error of the third examinee:
#' resp_set[[3]]$se
#'
#'
#' # Alternatively, output can be a matrix:
#'
#' resp_matrix <- get_cat_response_data(cat_sim_output = cat_data,
#'                                      output_type = "matrix")
#' resp_matrix
#'
#' # If cat design provided, the matrix columns will be sorted as the
#' # item pool used for the simulation:
#' resp_matrix <- get_cat_response_data(cat_sim_output = cat_data, cd = cd,
#'                                      output_type = "matrix")
#' resp_matrix
#'
#' # Additionally, remove the colums which has all NA values:
#' resp_matrix <- get_cat_response_data(cat_sim_output = cat_data, cd = cd,
#'                                      remove_na = TRUE,
#'                                      output_type = "matrix")
#' resp_matrix
#'
get_cat_response_data <- function(cat_sim_output, cd = NULL,
                                  output_type = c("Response_set", "matrix"),
                                  remove_na = FALSE,
                                  attach_summary = FALSE) {
  # Check whether it is one or multiple CAT outputs
  ip <- NULL
  if (!is.null(cd)) ip <- cd$ip
  if (is(cat_sim_output, "cat_output")) { # There is a single cat_output element
    resp_set <- response_set(lapply(list(cat_sim_output),
                                    get_cat_response_data_single), ip = ip)
  } else if (all(sapply(cat_sim_output, is, "cat_output"))) {
    resp_set <- response_set(lapply(cat_sim_output,
                                    get_cat_response_data_single), ip = ip)
  } else {
    stop("All of the elements of 'cat_sim_output' should be 'cat_output' ",
         "class.")
  }


  output_type <- match.arg(output_type)
  if (output_type == "Response_set") {
    return(resp_set)
  } else if (output_type == "matrix") {
    output <- as.matrix(resp_set, ip = ip)

    if (remove_na & length(cat_sim_output) > 1)
      output <- output[, apply(output, 2, function(x) !all(is.na(x))),
                       drop = FALSE]
    if (attach_summary) {
      cs <- summary(cat_sim_output)
      output <- cbind(cs, output)
    }
    return(output)
  } else {
    stop("Invalid 'output_type'.")
  }
  # if (!is.null(cd) && is.null(cd$ip))
  #   stop("In order to extract a response data, the cat_design object (cd) ",
  #        "should have an item pool.")
  #
  # if (nrows == 1) {
  #   # Get estimate history
  #   eh <- cat_sim_output$est_history
  #   resp <- sapply(eh, "[[", "resp")
  #   names(resp) <- sapply(eh, function(x) x$item$id)
  # } else {
  #   if (!is.null(cd)) {
  #     col_names <- cd$ip$id
  #   } else {
  #     eh <- lapply(cat_sim_output, "[[", "est_history") # list of est_history
  #     col_names <- sort(unique(sapply(do.call("c", eh), function(x)
  #       x$item$item_id)))
  #   }
  #   resp <- data.frame(matrix(NA, ncol = length(col_names),
  #                             nrow = length(cat_sim_output)))
  #   colnames(resp) <- col_names
  #   # Create an empty data.frame.
  #   for (i in 1:length(cat_sim_output)) {
  #     temp_resp <- get_cat_response_data(cat_sim_output[[i]], cd = cd)
  #     resp[i, names(temp_resp)] <- temp_resp
  #   }
  # }
  # if (remove_na)
  #   resp <- Filter(f = function(x) !all(is.na(x)), resp)
  # if (attach_summary) {
  #   cs <- summary(cat_sim_output)
  #   # When there is only one cat output, resp is vector.
  #   if (nrows == 1) resp <- data.frame(t(resp), check.names = FALSE)
  #   resp <- cbind(cs, resp)
  # }
  # return(resp)
}


###############################################################################@
############################# get_cat_administered_items #######################
###############################################################################@
#' Get administered items from a CAT output
#'
#' @description This function returns an item pool object of the
#'   administered items using the items in estimate history. If there is one
#' @param cat_sim_output This is a list object containing elements that are
#' "cat_output" class.
#' @return For \code{cat_output} with only one adaptive test, an
#'   \code{Itempool} class object will be returned. For \code{cat_output} with
#'   more than one adaptive tests, a list of \code{Itempool} class objects will
#'   be returned.
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @examples
#' cd <- create_cat_design(ip = generate_ip(n = 30), next_item_rule = 'mfi',
#'                         termination_rule = 'max_item',
#'                         termination_par = list(max_item = 10))
#' cat_data <- cat_sim(true_ability = rnorm(10), cd = cd)
#' get_cat_administered_items(cat_data)
get_cat_administered_items <- function(cat_sim_output) {
  if (is(cat_sim_output, "cat_output")) { # There is a single cat_output element
    return(get_administered_items_cpp(cat_sim_output$est_history))
  } else if (all(sapply(cat_sim_output, is, "cat_output"))) {
    eh <- lapply(cat_sim_output, "[[", "est_history") # list of est_history
    return(lapply(eh, function(x) get_administered_items_cpp(x)))
  } else
    stop("All of the elements of 'cat_sim_output' should be 'cat_output' ",
         "class.")
}


###############################################################################@
############################# calculate_exposure_rates #########################
###############################################################################@
#' Calculate exposure rate of items for CAT
#' @description This function calculates the exposure rate of items for a
#' CAT. It takes a list of \code{cat_output} objects and \code{cat_design}
#' object and returns exposure rate of each item.
#'
#' @param cat_sim_output This is a list object containing elements that are
#' "cat_output" class.
#' @param cd A \code{cat_design} object that is created by function
#'          \code{create_cat_design}.
#' @param item_ids A vector of Item (or Testlet) ids in the item pool.
#' @return This function returns a numeric vector of each item's exposure rate
#'   where the names of each exposure rate value is the item's id.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @seealso \code{\link{cat_sim}}
#'
#' @examples
#' cd <- create_cat_design(ip = generate_ip(n = 30), next_item_rule = 'mfi',
#'                         termination_rule = 'max_item',
#'                         termination_par = list(max_item = 10))
#' cat_data <- cat_sim(true_ability = rnorm(10), cd = cd)
#' calculate_exposure_rates(cat_data, cd = cd)
#'
calculate_exposure_rates <- function(cat_sim_output, cd = NULL,
                                     item_ids = NULL) {
  # Check whether it is one or multiple CAT outputs
  if (is(cat_sim_output, "cat_output")) { # There is a single cat_output element
    cat_sim_output <- list(cat_sim_output)
  # if it is not a list of cat_output
  } else if (!is.list(cat_sim_output) ||
             !all(sapply(cat_sim_output, is, "cat_output")))
    stop("All of the elements of 'cat_output' should be 'cat_output' class.")

  if (is.null(item_ids)) {
    if (is.null(cd)) {
      stop("Either 'cd' or 'item_ids' should be provided.")
    } else {
      if (is.null(cd$ip))
        stop("In order to calculate exposure rates, the cat_design object (cd)
              should have an item pool.")
      item_ids <- get_slot_itempool_cpp(cd$ip, "id")
    }
  }

  return(calculate_exposure_rates_cpp(item_ids, cat_sim_output))
}


###############################################################################@
############################# calculate_overlap_rates ##########################
###############################################################################@
#' Calculate overlap rate of items for CAT
#' @description This function calculates the overlap rate of items for a
#' CAT. It takes a list of \code{cat_output} objects and \code{cat_design}
#' object and returns exposure rate of each item.
#'
#' @param cat_sim_output This is a list object containing elements that are
#' "cat_output" class.
#' @param cd A \code{cat_design} object that is created by function
#'          \code{create_cat_design}.
#' @param item_ids A vector of item (or Testlet) ids in the item pool.
#' @return This function returns a numeric vector of each item's overlap rate
#'   where the names of each overlap rate value is the item's ID.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @seealso \code{\link{cat_sim}}
#'
#' @examples
#' cd <- create_cat_design(ip = generate_ip(n = 30), next_item_rule = 'mfi',
#'                         termination_rule = 'max_item',
#'                         termination_par = list(max_item = 10))
#' cat_data <- cat_sim(true_ability = rnorm(10), cd = cd)
#' calculate_overlap_rates(cat_data, cd = cd)
#'
calculate_overlap_rates <- function(cat_sim_output, cd = NULL,
                                    item_ids = NULL) {
  # Check whether it is one or multiple CAT outputs
  if (is(cat_sim_output, "cat_output")) { # There is a single cat_output element
    cat_sim_output <- list(cat_sim_output)
  # if it is not a list of cat_output
  } else if (!is.list(cat_sim_output) ||
             !all(sapply(cat_sim_output, is, "cat_output")))
    stop("All of the elements of 'cat_output' should be 'cat_output' class.")
  if (is.null(item_ids)) {
    if (is.null(cd)) {
      stop("Either 'cd' or 'item_ids' should be provided.")
    } else {
      if (is.null(cd$ip))
        stop("In order to calculate overlap rates, the cat_design object (cd)
              should have an item pool.")
      item_ids <- get_slot_itempool_cpp(cd$ip, "id")
    }
  }
  return(calculate_overlap_rates_cpp(item_ids, cat_sim_output))
}



###############################################################################@
############################# score_info #######################################
###############################################################################@
#' Calculate Score Information Function
#'
#' @description This function calculates the score information function of a
#' given CAT test. Ideally, a large number of simulees (say 1,000) will be
#' simulated at each theta level equally spaced along a large theta range (like
#' [-4, 4]). The score information function at each theta will be calculated
#' using the formulas 11-2 and 11-3 presented in Sands,  Waters and McBride
#' (1997, pages 127-128). Also see Lord (1980), Eqn. 10-7.
#'
#' For example if 1000 examinees simulated at each of the following theta
#' values (-3, -2, -1, 0, 1, 2, 3), the function will not calculate score
#' information values at theta = -3 and theta = 3. Score information values
#' at second values to the edges (i.e. theta = -2 and theta = 2) will be
#' calculated using Equation 11-2 of Sands et.al (1997). The rest of the
#' score information values (at theta = -1, 0, 1) will be calculated using
#' equation 11-3 (page 128).
#'
#' @param true_theta A vector of true theta values.
#' @param est_theta A vector of estimated theta values.
#' @param bins The number of bins true theta values should be grouped into.
#'   Ideally, this value is \code{NULL} and equal number of simulees are
#'   already in bins, and within each bin \code{true_theta} values are equal
#'   to each other. If these conditions are not satisfied, a bin value can be
#'   supplied.
#' @return A data frame of true theta values and score information value at
#'   each theta value will be returned.
#'
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @references
#' Lord, F. M. (1980). Applications of item response theory to practical
#' testing problems. Routledge.
#'
#' Sands, W. A., Waters, B. K., & McBride, J. R. (1997). Computerized adaptive
#' testing: From inquiry to operation. American Psychological Association.
#'
#' @examples
#' ip <- generate_ip(n = 30)
#' cd <- create_cat_design(ip = ip, next_item_rule = 'mfi',
#'                         termination_rule = 'max_item',
#'                         termination_par = list(max_item = 10))
#' # The following true_theta example is not ideal. For more informative score
#' # score information functions you can use more bins and more simulees like:
#' # rep(seq(-4, 4, .1), each = 1000)
#' true_theta <- rep(seq(-3, 3, 1), each = 10)
#' cat_data <- cat_sim(true_ability = true_theta, cd = cd)
#' dtf <- summary(cat_data)
#'
#' s_info <- score_info(true_theta = dtf$true_ability,
#'                      est_theta = dtf$est_ability)
#' s_info
#'
score_info <- function(true_theta, est_theta, bins = NULL) {
  if (length(true_theta) != length(est_theta))
    stop("The length of 'true_theta' should be equal to the length of ",
         "'est_theta'. ")

  # Sort both true_theta and est_theta in ascending order
  dtf <- data.frame(true_theta, est_theta)[order(true_theta), ]
  # Find the number of groups
  if (is.null(bins)) {
    temp <- table(true_theta)
    if (all(temp > 1)) {
      if (all(temp == temp[1])) {
        bins <- length(temp)
      } else {
        warning("Please provide a better bin number. By default, bin number ",
                "will be used as 20.")
        bins <- 20
      }
    } else {
      stop("Please provide a valid 'bins' value.")
    }
  }
  # For the formula to work there should be at least 5 bins
  if (bins < 5) stop("Please provide a bin number that is larger than 5.")
  dtf$group <- cut(dtf$true_theta, breaks = bins, labels = 1:bins)

  s_info <- data.frame(true_theta = rep(NA, bins - 2), score_info = NA)

  # Function uses the formula (11-2) for the edges (2 and bins-1) and for the
  # bins in the middle it uses formula (11-3) of Sands, Waters & McBride (1997)
  for (i in 2:(bins - 1)) {
    i_n2 <- i - 2 # n2: negative 2
    i_n1 <- i - 1
    i_p1 <- i + 1 # p1: positive 1
    i_p2 <- i + 2
    theta_n1 <- mean(dtf$true_theta[dtf$group == i_n1], na.rm = TRUE)
    theta <- mean(dtf$true_theta[dtf$group == i], na.rm = TRUE)
    theta_p1 <- mean(dtf$true_theta[dtf$group == i_p1], na.rm = TRUE)
    if (!i %in% c(2, bins - 1)) {
      theta_n2 <- mean(dtf$true_theta[dtf$group == i_n2], na.rm = TRUE)
      theta_p2 <- mean(dtf$true_theta[dtf$group == i_p2], na.rm = TRUE)
    }

    mean_theta_n1 <- mean(dtf$est_theta[dtf$group == i_n1], na.rm = TRUE)
    mean_theta_p1 <- mean(dtf$est_theta[dtf$group == i_p1], na.rm = TRUE)
    if (!i %in% c(2, bins - 1)) {
      mean_theta_n2 <- mean(dtf$est_theta[dtf$group == i_n2], na.rm = TRUE)
      mean_theta_p2 <- mean(dtf$est_theta[dtf$group == i_p2], na.rm = TRUE)
    }

    sd_theta_n1 <- sd(dtf$est_theta[dtf$group == i_n1], na.rm = TRUE)
    sd_theta <-    sd(dtf$est_theta[dtf$group == i], na.rm = TRUE)
    sd_theta_p1 <- sd(dtf$est_theta[dtf$group == i_p1], na.rm = TRUE)
    if (!i %in% c(2, bins - 1)) {
      sd_theta_n2 <- sd(dtf$est_theta[dtf$group == i_n2], na.rm = TRUE)
      sd_theta_p2 <- sd(dtf$est_theta[dtf$group == i_p2], na.rm = TRUE)
    }

    s_info$true_theta[i - 1] <- theta

    if (i %in% c(2, bins - 1)) {
      s_info$score_info[i - 1] <- (mean_theta_p1 - mean_theta_n1)^2 /
        ((theta_p1 - theta_n1)^2 * sd_theta^2)
    } else {
      s_info$score_info[i - 1] <- (
        25 * (mean_theta_p2 +  mean_theta_p1 - mean_theta_n1 -
                mean_theta_n2)^2) /
        ((theta_p2 + theta_p1 - theta_n1 - theta_n2)^2 *
           (sd_theta_n2 + sd_theta_n1 + sd_theta + sd_theta_p1 + sd_theta_p2)^2)
    }
  }
  return(s_info)
}


###############################################################################@
############################# qip_index ########################################
###############################################################################@
#' Calculate Quality of Item Pool Index
#'
#' @description The QIP Index can take values between 0 and 1 and indicates an
#'   item pool’s level of efficiency. A value of 1 signifies an optimum item
#'   pool for that examinee group. If one adds redundant items to an item pool
#'   that cannot be used by the CAT algorithm, the QIP Index will not increase
#'   or will increase minimally. In this sense, the QIP Index is an indicator of
#'   the item pools’ deficiency, instead of redundancy. However, if an exposure
#'   control mechanism is within test specifications, the QIP index can measure
#'   whether the redundancy in the item pool supports the exposure control
#'   method. See Gonulates (2019) for details.
#'
#'   Note that this function will best work with Rasch or 1PL models. It
#'   will not work with polytomous items.
#'
#' @param cat_sim_output This is a list object containing elements that are
#'   \code{cat_output} class.
#' @param summary_func A string representing the function that will be applied
#'   to individual QIP values for a simulee. The default is \code{NULL}, where
#'   all QIP values of each administered item of a simulee will be returned.
#'   Other possible values are: \code{"mean"}, \code{"median"}, \code{"min"},
#'   \code{"max"}. See examples for demonstrations.
#' @param ... Additional arguments that will be passed to the
#'   \code{summary_func}. For example, if \code{summary_func = "quantile"},
#'   probability of the 25th quantile can be specified using the argument
#'   \code{prob = .25}. See examples for demonstrations.
#'
#'   Since \code{...} will be passed to \code{sapply} function,
#'   \code{simplify = FALSE} can be passed to function to get results as list
#'   elements.
#'
#' @return A vector or matrix of QIP values or the summary statistics of QIP
#'   values.
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @references
#' Gönülateş, E. (2019). Quality of Item Pool (QIP) Index: A Novel Approach to
#' Evaluating CAT Item Pool Adequacy. Educational and Psychological Measurement,
#' 79(6), 1133–1155. \url{https://doi.org/10.1177/0013164419842215/}
#'
#' @examples
#'
#' cd <- create_cat_design(ip = generate_ip(n = 30), next_item_rule = 'mfi',
#'                         termination_rule = 'max_item',
#'                         termination_par = list(max_item = 10))
#' cat_output <- cat_sim(true_ability = rnorm(10), cd = cd)
#'
#' qip_index(cat_output)
#'
#' # Return result as list elements
#' qip_index(cat_output, simplify = FALSE)
#'
#' # Summarize QIP values:
#' qip_index(cat_output, summary_func = "mean")
#' qip_index(cat_output, summary_func = "median")
#' qip_index(cat_output, summary_func = "min")
#' qip_index(cat_output, summary_func = "max")
#' qip_index(cat_output, summary_func = "quantile", prob = .25)
#' qip_index(cat_output, summary_func = "quantile", prob = c(.25, .5, .75))
#'
#' qip_index(cat_output, summary_func = "quantile", prob = c(.25, .5, .75),
#'           simplify = FALSE)
#'
#'
#'
qip_index <- function(cat_sim_output, summary_func = NULL, ...) {
  # Check whether it is one or multiple CAT outputs
  if (is(cat_sim_output, "cat_output")) { # There is a single cat_output element
    cat_sim_output <- list(cat_sim_output)
  # if it is not a list of cat_output
  } else if (!is.list(cat_sim_output) ||
             !all(sapply(cat_sim_output, is, "cat_output")))
    stop("All of the elements of 'cat_output' should be 'cat_output' class.")
  result <- sapply(cat_sim_output, qip_index_single,
                   summary_func = summary_func, ...)
  if (inherits(result, "matrix")) result <- t(result)
  return(result)
}


#' QIP index for one 'cat_output' object
#'
#'
#' @noRd
#'
#' @examples
#'
#' cd <- create_cat_design(ip = generate_ip(n = 30), next_item_rule = 'mfi',
#'                         termination_rule = 'max_item',
#'                         termination_par = list(max_item = 10))
#' cat_output <- cat_sim(true_ability = rnorm(1), cd = cd)
#'
#' qip_index_single(cat_output)
#' qip_index_single(cat_output, summary_func = "mean")
#' qip_index_single(cat_output, summary_func = "median")
#' qip_index_single(cat_output, summary_func = "min")
#' qip_index_single(cat_output, summary_func = "max")
#' qip_index_single(cat_output, summary_func = "quantile", prob = .25)
#'
qip_index_single <- function(cat_sim_output, summary_func = NULL, ...) {
  if (!inherits(cat_sim_output, "cat_output")) {
    stop("Invalid 'cat_sim_output' argument. The class of 'cat_output' ",
         "argument should be 'cat_output'.")
  }
  # In case there are any Testlets, only pull items. Then convert them to 1PL
  # since the
  ip_list <- lapply(cat_sim_output$est_history,
                    function(x) convert_model(x$item, "1PL"))
  # Maximum possible information for each item
  max_info <- sapply(ip_list, function(x) info(ip = x, theta = x$b))
  # Information before the administration of the item
  est_before <- sapply(cat_sim_output$est_history, `[[`, "est_before")
  info_before <- sapply(1:length(ip_list), function(i) info(ip = ip_list[[i]],
                                                            theta = est_before[i]))
  qip <- info_before/max_info

  if (is.null(summary_func)) {
    # return(setNames(qip, sapply(ip_list, "slot", name = "item_id")))
    return(setNames(qip, paste0(1:length(qip))))
  } else {
    result <- do.call(summary_func, list(qip, ...))
    # # Add examinee_id if exists
    # if (length(result) == 1 && !is.null(cat_sim_output$examinee_id))
    #   result <- setNames(result, cat_sim_output$examinee_id)
    # Add examinee_id if exists
    if (length(result) == 1) result <- unname(result)
    return(result)
  }
}


