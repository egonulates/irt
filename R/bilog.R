
#' Prepare BILOG-MG data file
#'
#' @description Prepare \code{".dat"} data file for BILOG-MG.
#'
#' @param x Either a \\code{data.frame}, \code{matrix} or
#'   \code{\link{Response_set-class}} object. Each row should represent a
#'   subject (examinee) and each column represent a variable (usually an item).
#'   The response values should be either 0, 1 or \code{NA}.
#' @param items A vector of column names or numbers of the \code{x} that
#'   represents the responses.
#' @param examinee_id_var The column name or number that contains individual
#'   subject IDs. If none is provided (i.e. \code{examinee_id_var = NULL}), the
#'   program will check whether the data provided has row names.
#' @param group_var The column name or number that contains group membership
#'   information if multi-group calibration is desired. Ideally, it grouping
#'   variable is represented by single digit integers. If other type of data
#'   provided, an integer value will automatically assigned to the variables.
#'   The default value is \code{NULL}, where no multi-group analysis is
#'   performed.
#' @param reference_group Represent which group's ability distribution will be
#'   set to mean = 0 and standard deviation = 1. For example, if the value is 1,
#'   then the group whose code is 1 will have ability distribution with mean 0
#'   and standard deviation 1. When groups are assumed to coming from a single
#'   population, set this value to 0.
#'
#'   The default value is \code{NULL}.
#'
#'   This value will be represented in BILOG-MG control file as:
#'   \code{REFERENCE = reference_group}.
#' @param target_path The location where the BILOG-MG data file fill be saved.
#'   For example: \code{file.path(getwd(), "data", "mydata.dat")}.
#' @param create_np_key_file If \code{TRUE}, a file that contains
#'   the non-presented key will be created. This key is the same format as
#'   the corresponding response data file.
#' @param overwrite If TRUE and there is already a BILOG-MG data file in the
#'   target path with the same name, the file will be overwritten.
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' resp <- sim_resp(generate_ip(n = 15), rnorm(200), prop_missing = .2)
#' bilog_create_datafile(x = resp)
#' bilog_create_datafile(x = resp, items = paste0("Item-", 1:7))
#' }
#'
bilog_create_datafile <- function(
  x,
  items = NULL,
  examinee_id_var = NULL,
  group_var = NULL,
  reference_group = NULL,
  target_path = file.path(getwd(), "bilog_data.dat"),
  create_np_key_file = FALSE,
  overwrite = FALSE) {

  if (!(inherits(x, c("matrix", "data.frame", "Response_set"))))
    stop("Invalid data file. Data file should be either a matrix, data.frame ",
         "or a 'Response_set' object.", call. = FALSE)

  # Check path
  target_path <- normalizePath(target_path, mustWork = FALSE)
  target_dir <- dirname(target_path)
  if (!grepl("(.*)\\.dat$", target_path))
    stop("Invalid 'target_path' argument. Please provide a valid file name ",
         "with '.dat' extension as a target_path", call. = FALSE)
  # Make sure the directory exists
  if (!dir.exists(target_dir))
    dir.create(path = target_dir, recursive = TRUE)
  if (!dir.exists(target_dir))
    stop(paste0("The directory for BILOG-MG data file cannot be created at: \n",
                target_dir, "\nPlease create directory manually.",
                call. = FALSE))

  # If x is Response_set, convert it to a data.frame and add examinee_id_var and
  # group_var
  if (is(x, "Response_set")) {
    x_matrix <- as.matrix(x, output = "score")
    if (!is.null(group_var) && is_atomic_vector(group_var)) {
      if (length(group_var) == 1 && is.character(group_var) &&
          is_atomic_vector(do.call("$", list(x = x, name = group_var)))) {
        x_matrix <- cbind(do.call("$", list(x = x, name = group_var)),
                          data.frame(x_matrix))
        colnames(x_matrix)[1] <- group_var
      } else if (length(group_var) == nrow(x_matrix)) {
        x_matrix <- cbind(group_var, data.frame(x_matrix))
        group_var <- "grouping_variable"
        colnames(x_matrix)[1] <- group_var
      } else {
        stop("Invalid 'group_var' value. For 'response_set' objects, please ",
             "attach the group information to the response set by: ",
             "'x$<group_var> <- values'. Please see the examples.")
      }
    }
    x <- x_matrix
  }

  ### Examinee ID ###
  # Create the examinee_id field, if it is NULL, automatically the row names
  # of the data will be the subject ids.
  examinee_id <- rownames(x)
  # Check whether examinee_id_var is valid
  if (!is.null(examinee_id_var) && (
    length(examinee_id_var) != 1 ||
    !(examinee_id_var %in% 1:ncol(x) ||
      (!is.null(colnames(x)) && examinee_id_var %in% colnames(x)))
    ))
    stop("Invalid 'examinee_id_var' argument. 'examinee_id_var' value should ",
         "be a column number or a column name.", call. = FALSE)
  if (!is.null(examinee_id_var)) {
    examinee_id <- paste0(x[, examinee_id_var, drop = TRUE])
    # # Remove 'examinee_id_var" from the response data
    # if (examinee_id_var %in% colnames(x)) {
    #   x <- x[, which(colnames(x) != examinee_id_var)]
    # } else {
    #   x <- x[, -examinee_id_var]
    # }
  }
  if (is.null(examinee_id)) examinee_id <- paste0(1:nrow(x))
  # find the maximum number or characters in an ID
  num_id_char <- max(nchar(examinee_id))
  if (!num_id_char %in% 1:30)
    stop("Invalid IDs. Number of characters alloted to IDs cannot be more ",
         "than 30 characters.", call. = FALSE)
  examinee_id <- sprintf(paste0("%", num_id_char, "s"), examinee_id)

  ### Item IDs ###
  # If items are NULL, then use all of the items.
  if (is.null(items)) {
    # Check if the column names are NULL
    if (is.null(colnames(x)))
      colnames(x) <- paste0("Item-", 1:ncol(x))
    items <- colnames(x)

    # Make sure to remove examinee_id_var
    if (!is.null(examinee_id_var)) {
      if (examinee_id_var %in% items) {
        items <- items[items != examinee_id_var]
      } else {
        items <- items[-examinee_id_var]
      }
    }
    # Make sure to remove group_var
    if (!is.null(group_var)) {
      if (group_var %in% items) {
        items <- items[items != group_var]
      } else {
        items <- items[-group_var]
      }
    }
  }
  # Make sure items are valid
  if (!is.null(items) && (
    any(duplicated(items)) ||
    !(all(items %in% colnames(x)) || all(items %in% 1:ncol(x)) )
    ))
    stop("Invalid 'items' argument. The elements of 'items' argument should ",
         "be a character vector of column names of the 'x'. All elements ",
         "should be unique.", call. = FALSE)


  ### Prepare responses ###
  resp <- apply(x[, items], 2, as.character)
  # Check if the responses are valid:
  if (!all(sapply(unique(as.vector(resp)), function(x) is.na(x) ||
                  x %in% c("0", "1"))))
    stop("Invalid response data. The response values should be either 0, 1 ",
         "or missing (i.e. NA).", call. = FALSE)
  # Convert missing data to "."
  resp <- ifelse(is.na(resp), ".", resp)
  num_of_items <- ncol(resp)
  resp <- apply(resp, 1, paste0, collapse = "")

  ### Group ###
  # group: In BILOG-MG, "the group identifier has to be a single digit
  # (integer), starting with 1."
  group_info <- bilog_create_group_info(x = x, group_var = group_var,
                                  reference_group = reference_group)

  # # group: In BILOG-MG, "the group identifier has to be a single digit
  # # (integer), starting with 1."
  # group <- NULL
  # groups <- NULL
  # num_of_groups <- 0
  # # Check whether group_var is valid
  # if (!is.null(group_var) && (
  #   length(group_var) != 1 || !(group_var %in% 1:ncol(x) ||
  #                               (!is.null(colnames(x)) &&
  #                                group_var %in% colnames(x)))
  #   ))
  #   stop("Invalid 'group_var' argument. 'group_var' value should be ",
  #        "a column number or a column name.", call. = FALSE)
  # if (!is.null(group_var)) {
  #   group_names <- as.character(x[, group_var, drop = TRUE])
  #   unique_groups <- unique(group_names)
  #   if (any(is.na(unique_groups))) {
  #     stop("Grouping variable cannot have missing (NA) values.",
  #          call. = FALSE)
  #   }
  #   group_sizes <- data.frame(table(x[, group_var]))
  #   if (any(nchar(unique_groups) > 8)) {
  #     unique_groups <- substr(unique_groups, 1, 8)
  #     if (length(unique(unique_groups)) != length(unique_groups)) {
  #       stop("Invalid group values. Values that designate group membership ",
  #            "should be up to eight (8) characters and be unique.",
  #            call. = FALSE)
  #     } else
  #       message(paste0("Some of the group names are larger than eight ",
  #                      "characters. Group names are shortened to fit ",
  #                      "within eight characters:\n",
  #                      paste0("\"", unique_groups, "\"", collapse = ", ")))
  #   }
  #   num_of_groups <- length(unique_groups)
  #   # Make sure group variable is integers from 1:9
  #   if (!all(unique_groups %in% paste0(1:num_of_groups))) {
  #     group <- as.character(as.integer(factor(group_names,
  #                                             levels = unique_groups)))
  #   } else group <- group_names
  #   groups <- data.frame(name = unique_groups, code = unique(group),
  #                        n = group_sizes$Freq)
  # }

  ### Prepare the data file and formal statement ###
  # Add ID
  data_text <- paste0(examinee_id, " ")
  formal_statement <- paste0("(", num_id_char, "A1", ",", "1X")

  # Add group if there is
  if (!is.null(group_info$group)) {
    data_text <- paste0(data_text, group_info$group, " ")
    formal_statement <- paste0(formal_statement,  ",I1", ",1X")
  }

  # Create not-presented key file:
  if (create_np_key_file) {
    np_text <- paste0(
      # Add examinee_id
      paste0(rep(" ", num_id_char), collapse = ""), " ",
      # Add groups
      ifelse(group_info$num_of_groups == 0, "", "  "),
      # Add items
      paste0(rep(".", num_of_items), collapse = ""))

    np_key_file_path <- file.path(
      target_dir,
      paste0(gsub("\\.(.*)$", "", basename(target_path)), ".NFN"))
    if (overwrite || !file.exists(np_key_file_path))
      writeLines(text = np_text, con = np_key_file_path)
  } else
    np_key_file_path <- NULL

  # Add response data:
  data_text <- paste0(data_text, resp)
  formal_statement <- paste0(formal_statement,  ",", num_of_items, "A1)")

  if (overwrite || !file.exists(target_path)) writeLines(data_text, target_path)

  return(list(x = x,
              formal_statement = formal_statement,
              data_file_path = target_path,
              num_of_items = num_of_items,
              num_of_groups = group_info$num_of_groups,
              group_info = group_info$group_info,
              reference_group = group_info$reference_group,
              np_key_file_path = np_key_file_path,
              num_id_char = num_id_char))
}




#' Create group information
#'
#' @param x Either a \\code{data.frame}, \code{matrix} or
#'   \code{\link{Response_set-class}} object. Each row should represent a
#'   subject (examinee) and each column represent a variable (usually an item).
#'   The response values should be either 0, 1 or \code{NA}.
#' @param group_var The column name or number that contains group membership
#'   information if multi-group calibration is desired. Ideally, it grouping
#'   variable is represented by single digit integers. If other type of data
#'   provided, an integer value will automatically assigned to the variables.
#'   The default value is \code{NULL}, where no multi-group analysis is
#'   performed.
#'
#' @author Emre Gonulates
#'
#' @noRd
bilog_create_group_info <- function(x = NULL, group_var = NULL,
                              reference_group = NULL) {
  group <- NULL
  group_info <- NULL
  num_of_groups <- 0

  if (is.null(x) || is(x, "Response_set")) {
    return(list(group = group, group_info = group_info,
                num_of_groups = num_of_groups))
  }

  # Check whether group_var is valid
  if (!is.null(group_var) && (
    length(group_var) != 1 || !(group_var %in% 1:ncol(x) ||
                                (!is.null(colnames(x)) &&
                                 group_var %in% colnames(x)))
    ))
    stop("Invalid 'group_var' argument. 'group_var' value should be a column ",
         "number or a column name.", call. = FALSE)
  if (!is.null(group_var)) {
    # Check whether reference_group is NULL, if yes, assign a default
    # reference group:
    group_info <- data.frame(table(name = x[, group_var]))
    colnames(group_info) <- c("name", "n")
    group_info$name <- as.character(group_info$name)

    if (is.null(reference_group)) {
      reference_group <- group_info$name[1]
    } else if (!reference_group %in% group_info$name) {
      stop(paste0("'reference_group' argument should be one of the following",
                  " group names:\n",
                  paste0("\"", group_info$name, "\"", collapse = ",")),
           call. = FALSE)
    }

    group_info$ref <- group_info$name == reference_group
    # put the reference group to the top
    group_info <- group_info[order(group_info$ref, decreasing = TRUE), ]

    group_info$code <- as.character(1:nrow(group_info))

    group_names <- as.character(x[, group_var, drop = TRUE])
    # unique_groups <- group_info$name
    # unique_groups <- unique(group_names)

    if (any(is.na(group_info$name))) {
      stop("Grouping variable cannot have missing (NA) values.", call. = FALSE)
    }
    if (any(nchar(group_info$name) > 8)) {
      group_info$name <- substr(group_info$name, 1, 8)
      if (length(unique(group_info$name)) != length(group_info$name)) {
        stop("Invalid group values. Values that designate group membership ",
             "should be up to eight (8) characters and be unique.",
             call. = FALSE)
      } else
        message(paste0("Some of the group names are larger than eight ",
                       "characters. Group names are shortened to fit ",
                       "within eight characters:\n",
                       paste0("\"", group_info$name, "\"", collapse = ", ")))
    }
    num_of_groups <- nrow(group_info)

    group <- group_info$code[match(group_names, group_info$name)]
    # # Make sure group variable is integers from 1:9
    # if (!all(group_info$name %in% paste0(1:num_of_groups))) {
    #
    #   group <- as.character(as.integer(factor(group_names,
    #                                           levels = group_info$name)))
    # } else group <- group_names
    # group_info <- data.frame(name = group_info$name, code = unique(group),
    #                          n = group_info$Freq)
  }
  return(list(group = group, group_info = group_info,
              num_of_groups = num_of_groups,
              reference_group = reference_group))
}



#' Read the parameters of BILOG-MG Calibration
#'
#' @param par_file Path for a BILOG-MG parameter file with '.PAR' extension.
#' @param items The names of the items. This will be given assigned as the
#'   item_id's of the items. The default value is \code{NULL}, where the item
#'   item_id's assigned by BILOG-MG will be assigned as item parameters.
#' @param model The model of the items. The value is one of the
#'   following:
#'   One-parameter logistic model (\code{"1PL"}),
#'   Two-parameter logistic model (\code{"2PL"}),
#'   Three-parameter logistic model (\code{"3PL"}).
#'
#'   The default value is \code{"3PL"}.
#' @param D Scaling constant. The default value is \code{1}. If the item
#'   parameters needs to be converted to the commonly used normal scale where
#'   \code{D = 1.7} or \code{D = 1.702}, change this value accordingly.
#'
#' @noRd
#'
bilog_read_pars <- function(
  par_file,
  ph1_file,
  items = NULL,
  model = "3PL",
  D = 1
  ) {
  result <- list(ip = NULL,
                 failed_items = NULL)

  if (is.null(D)) D <- 1.7

  # Wait three seconds for the parameter file and
  counter <- 1
  while (!file.exists(par_file) && counter < 4) {
    message(paste0("Waiting for the parameter file... (", counter, ")\n"))
    Sys.sleep(1)
    counter <- counter + 1
  }
  if (!file.exists(par_file))
    stop(paste0("Parameter file cannot be found at: \n\"", par_file, "\"\n"),
         call. = FALSE)
  # Locate the parameter file and read it
  # In BILOG-MG's terms:
  # "a" = slope
  # "b" = threshold
  # "c" = lower asymptote
  pars <- utils::read.fwf(
    file = par_file,
    widths = c(8, 8, rep(10, 13), 4, 1, 1), skip = 4,  header = FALSE,
    col.names = c("item_id", "subtest_name", "intercept", "intercept_se",
                  "a", "a_se", "b", "b_se", "dispersion", "dispersion_se",
                  "c", "c_se", "drift", "drift_se", "unused", "item_location",
                  "answer_key", "dummy_values"
                                 ))

  if (model == "3PL") {
    ipdf <- pars[, c("item_id", "a", "b", "c")]
    ipdf_se <- pars[, c("a_se", "b_se", "c_se")]
  } else if (model == "2PL") {
    ipdf <- pars[, c("item_id", "a", "b")]
    ipdf_se <- pars[, c("a_se", "b_se")]
  } else if (model == "1PL") {
    ipdf <- pars[, c("item_id", "a", "b"), drop = FALSE]
    ipdf_se <- pars[, c("a_se", "b_se"), drop = FALSE]
    model <- "2PL"
  } else if (model == "Rasch") {
    ipdf <- pars[, c("item_id", "b")]
    ipdf_se <- pars[, "b_se", drop = FALSE]
  }
  if (!is.null(items)) ipdf$item_id <- items

  # Check whether all item parameter values are valid:
  ph1_content <- readLines(con = ph1_file)
  invalid_items <- ph1_content[grepl("WILLNOT BE CALIBRATED", ph1_content)]


  if (any(sapply(ipdf[, -1], class) != "numeric") ||
      any(sapply(ipdf_se[, -1], class) != "numeric") ||
      length(invalid_items) > 0) {

    # Get the invalid items in the form:
    # "**** ITEM    6 WITH BISERIAL  R  LESS THAN -0.15 WILLNOT BE CALIBRATED."
    invalid_items <- gsub("(.*)\\*\\*\\*\\* ITEM[ ]+", "", invalid_items)
    invalid_items <- 1:nrow(ipdf) %in% as.numeric(gsub(" (.*)", "",
                                                       invalid_items))
    # Get invalid items that does not have numeric values
    invalid_items <- invalid_items | !apply(
      sapply(ipdf[, -1], function(x) grepl(
        "[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[l]?|[-]?[0-9]+[.]?[0-9]*[ee][0-9]+",
        x)), 1, all)
    invalid_items <- invalid_items | !apply(sapply(ipdf_se[, -1], function(x)
      grepl(
        "[-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[l]?|[-]?[0-9]+[.]?[0-9]*[ee][0-9]+",
        x)), 1, all)
    warning(paste0("\nEstimation failed for following item(s):\n\n",
                   paste0(utils::capture.output(print(ipdf[invalid_items, ])),
                          collapse = "\n"),
                   "\n\nThese items will be remove from the output."))
    result$failed_items <- ipdf[invalid_items, ]
    # Remove invalid items
    ipdf <- ipdf[!invalid_items, ]
    ipdf_se <- ipdf_se[!invalid_items, ]
  }



  # Convert the cleaned data frame to numeric.
  for (i in which(sapply(ipdf[, -1], class) != "numeric"))
    ipdf[, i + 1] <- as.numeric(ipdf[, i + 1])
  for (i in which(sapply(ipdf_se[, -1], class) != "numeric"))
    ipdf_se[, i + 1] <- as.numeric(ipdf_se[, i + 1])

  # if ("a" %in% colnames(ipdf)) ipdf$a <- ipdf$a / D
  ipdf$item_id <- gsub("^\\s+|\\s+$", "", ipdf$item_id)
  result$ip <- itempool(ipdf[, which(colnames(ipdf) %in% c("a", "b", "c")),
                             drop = FALSE],
                        item_id = ipdf$item_id, model = model, D = D,
                        se_parameters = ipdf_se)
  return(result)
}


#' This function reads the next CTT table from the row given
#' @param text BILOG-MG text grabbed
#' @param row The row number to start reading from 'text'
#'
#' @noRd
bilog_read_ctt_table <- function(text, row) {
  sub_text <- text[row:length(text)]
  sub_text <- sub_text[6:(which(grepl(paste0(
    "^ ", paste0(rep("-", 73), collapse = "")), sub_text))[2] - 1)]

  pars <- utils::read.fwf(textConnection(sub_text),
                   widths = c(5, 11, 9, 10, 8, 9, 10, 9),
                   col.names = c("order", "name", "tried", "right", "pvalue",
                                 "logit", "pbis", "bis"),
                   header = FALSE,
                   colClasses = c("integer", "character", "numeric",
                                  "numeric", "numeric", "numeric", "numeric",
                                  "numeric")
                   )
  if (any(pars$pvalue > 1))
    pars$pvalue <- pars$pvalue/100
  pars$name <- gsub("^\\s+|\\s+$", "", pars$name)
  return(pars)
}


#' Read the CTT parameters of BILOG-MG Calibration
#'
#' @param ph1_file Path for a BILOG-MG ".PH1" file which holds CTT statistics.
#' @return A list of CTT parameters. If there are groups, then the CTT
#'   statistics for groups can be found in \code{$group$GROUP-NAME}.
#'   Overall statistics for the whole group is at \code{$overall}.
#'
#' @noRd
bilog_read_ctt <- function(ph1_file) {
  text <- readLines(ph1_file)
  result <- list(overall = NULL)
  # Check whether there is a group
  if (any(grepl("ITEM STATISTICS FOR MULTIPLE GROUPS", text))) {
    ### Multi-group ###
    result$group <- NULL
    # Read the CTT statistics for groups
    for (i in which(grepl("ITEM STATISTICS FOR GROUP:", text))) {
      group_name <- sub("( )*$", "", text[i])
      group_name <- sub("^(.*) ", "", group_name)
      result$group[[group_name]] <- bilog_read_ctt_table(text, i)
    }
    # Read the CTT statistics for the overall test
    i <- which(grepl("ITEM STATISTICS FOR MULTIPLE GROUPS", text))
    result$overall <- bilog_read_ctt_table(text, i)
  } else if (any(grepl("ITEM STATISTICS FOR SUBTEST", text))) {
    ### Single-Group ###
    i <- which(grepl("ITEM STATISTICS FOR SUBTEST", text))
    result$overall <- bilog_read_ctt_table(text, i)
  }
  if (is.null(result$group)) return(result$overall)
  return(result)
}



#' Read posterior distribution points and weights
#'
#' @param ph2_file Path for a BILOG-MG ".PH2" file which holds group means.
#' @param group_info A data frame holding information about group names and
#'   codes
#'
#' @return A data frame for posterior points and weights
#'
#' @noRd
bilog_read_posterior_dist <- function(ph2_file, group_info = NULL) {
  main_text <- readLines(ph2_file, skipNul = TRUE)
  row <- which(grepl(
    pattern = "QUADRATURE POINTS, POSTERIOR WEIGHTS, MEAN AND S\\.D\\.:",
    main_text))

  read_pw <- function(text) {
    temp_pattern <- "^ POINT "
    points <- sapply(unlist(strsplit(gsub(
      temp_pattern, "", text[grepl(temp_pattern, text)]), " ")), as.numeric)
    points <- points[!is.na(points)]

    temp_pattern <- "^ POSTERIOR "
    posterior <- sapply(unlist(strsplit(gsub(
      temp_pattern, "", text[grepl(temp_pattern, text)]), " ")), as.numeric)
    posterior <- posterior[!is.na(posterior)]
    return(data.frame(point = points, weight = posterior))
  }

  if (length(row) == 1) { # Single group calibration
    subtext <- main_text[row:length(main_text)]
    return(read_pw(subtext))
  } else if (length(row) > 1) { # Multi-group calibration
    result <- vector("list", length(row))
    for (i in 1:length(row)) {
      subtext <- main_text[row[i]:ifelse(i == length(row), length(main_text),
                                         row[i + 1] - 1)]
      result[[i]] <- read_pw(subtext)
    }
    if (!is.null(group_info)) names(result) <- group_info$name
    return(result)
  }
  return(NULL)
}


#' Read the group means of multigroup BILOG-MG Calibration
#'
#' @param ph3_file Path for a BILOG-MG ".PH3" file which holds group means.
#' @param group_info A data frame holding information about group names and
#'   codes
#' @return Group means
#'
#' @noRd
bilog_read_group_means <- function(ph3_file, group_info) {
  main_text <- readLines(ph3_file, skipNul = TRUE)
  result <- group_info
  row <- which(grepl(pattern = "PRIOR MEANS AND STANDARD DEVIATIONS",
                     main_text))
  if (length(row) == 1) {
    text <- main_text[row:length(main_text)]
    row <- which(grepl("^ ---------------------------$", text))[1:2]
    text <- text[(row[1] + 1):(row[2] - 1)]
    pars <- utils::read.fwf(textConnection(text), widths = c(3, 13, 10),
                            col.names = c("code", "prior_mean", "prior_sd"))
    if (is.null(result)) return(pars)
    result <- merge(x = result, pars, by = "code")
  }
  row <- which(grepl("SUMMARY STATISTICS FOR SCORE ESTIMATES BY GROUP",
                     main_text))
  if (length(row) > 0) {
    text <- main_text[row:length(main_text)]
    pars <- group_info[, "code", drop = FALSE]
    pars <- pars[order(pars$code), , drop = FALSE]

    temp_pattern <- "^ MEAN:[[:blank:]]*(.*)$"
    pars$mean <- as.numeric(gsub(temp_pattern, "\\1",
                                 text[grepl(temp_pattern, text)]))

    temp_pattern <- "^ S\\.D\\.:[[:blank:]]*(.*)$"
    pars$sd <- as.numeric(gsub(temp_pattern, "\\1",
                               text[grepl(temp_pattern, text)]))
    result <- merge(x = result, pars, by = "code")
  }
  return(result)
}


#' Read the scale scores from BILOG-MG Calibration
#'
#' @param score_file Path for a BILOG-MG ".SCO" file which holds estimated
#'   scale scores.
#' @param x the data file
#' @param examinee_id_var The column name of x that is holding the
#'   examinee_id variable.
#' @param group_var The column name of x that is holding the examinee group
#'   variable
#'
#' @return A data frame consist of scores.
#'
#' @noRd
bilog_read_scores <- function(score_file, x, examinee_id_var = NULL,
                              group_var = NULL) {

  text <- readLines(score_file)
  # Remove first two lines which does not contain any information
  text <- text[-c(1:2)]

  if (nrow(x) == length(text)) {
    scores <- utils::read.delim(textConnection(text), sep = "|", header = FALSE)
    scores <- utils::read.fwf(
      textConnection(scores[, 2]),
      widths = c(6 + 1, 11, 3, 5, 10, 12, 12, 11, 10),
      col.names = c("weight", "test", "tried", "right", "percent",
                    "ability", "se", "prob", "unknown1")
      )
  } else if (nrow(x) == (length(text) / 2)) {
    # Get the width of the first line that contains group number and item_id
    # widths <- c(3, nchar(text[1])-2, 6, 11, 3, 5, 10, 12, 12, 11, 10)
    n <- 1:(length(text)/2)
    # text <- paste(text[2*n-1], text[2*n])
    text <- text[2*n]

    scores <- utils::read.fwf(
      textConnection(text),
      widths = c(6, 11, 3, 5, 10, 12, 12, 11, 10),
      col.names = c("weight", "test", "tried", "right", "percent",
                    "ability", "se", "prob", "unknown1")
                              )
  }
  scores <- scores[, c("tried", "right", "ability", "se", "prob")]
  # Add group info
  if (!is.null(group_var) && group_var %in% names(x)) {
    scores <- cbind(group = x[[group_var]], scores)
  }
  # Add examinee_id info
  if (!is.null(examinee_id_var) && examinee_id_var %in% names(x)) {
    scores <- cbind(examinee_id = x[[examinee_id_var]], scores)
  } else if (!is.null(rownames(x))) {
    scores <- cbind(examinee_id = rownames(x), scores)
  } else if (is(x, "Response_set") && length(x$examinee_id) == length(x)) {
    scores <- cbind(examinee_id = x$examinee_id, scores)
  }

  if (requireNamespace("tibble")) {
    return(tibble::as_tibble(scores))
  } else return(scores)
}

#' This function determines whether the calibration is converged or not.
#'
#' The method in this function is not tested extensively and based on
#' limited experience.
#'
#' @param par_file The parameter file
#' @param ph2_file The file with ".PH2" extension
#' @param ph3_file The file with ".PH3" extension
#'
#' @return TRUE if convergence reached, FALSE if not.
#' @noRd
check_bilog_convergence <- function(par_file, ph2_file, ph3_file) {
  converged <- FALSE

  if (file.exists(ph2_file)) {
    ph2_content <- readLines(con = ph2_file)
    converged <- any(grepl("BYTES OF NUMERICAL WORKSPACE USED",
                                  x = ph2_content)) &&
      !any(grepl("NOTE: CONVERGENCE HAS NOT BEEN REACHED TO CRITERION",
                 x = ph2_content))
  }
  # Check whether b parameters have standard error values
  if (file.exists(par_file)) {
    pars <- utils::read.fwf(
      file = par_file,
      widths = c(8, 8, rep(10, 13), 4, 1, 1), skip = 4,  header = FALSE,
      col.names = c("item_id", "subtest_name", "intercept", "intercept_se",
                    "a", "a_se", "b", "b_se", "dispersion", "dispersion_se",
                    "c", "c_se", "drift", "drift_se", "unused", "item_location",
                    "answer_key", "dummy_values"
                                   ))
    converged <- converged || any(pars$b_se > 0)
  }
  return(converged)
}


#' Wrap text
#'
#' @description This functions wraps text within 80 characters. If it overflows,
#'  the remaining text will be flowed the next line beginning column 1.
#'
#' @param t text
#' @param width total width of the line
#' @param tab whether to add 'tab' string to the beginning of each text, usual
#'        'tab = "    "'.
#' @param skip_tab do not put tab to these lines
#'
#' @noRd
wrap_text <- function(t, width = 80, tab = NULL, skip_tab = 0) {
  # Add tab to all lines except to the first line.
  if (!is.null(tab)) {
    r <- strwrap(t, width = width)
    r <- c(r[skip_tab], paste0(
      tab, strwrap(paste0(r[setdiff(1:length(r), skip_tab)],
                          collapse = "\n"),
                   width = width - nchar(tab) - 1)))
    return(paste0(r, collapse = "\n"))
  }
  r <- gsub(paste0("(.{", width, "})"), "\\1\n", t)
  # do not allow the first character in a line to be a comma ",":
  if (grepl(pattern = "\n,", x = r, fixed = TRUE)) {
    r <- gsub(paste0("(.{", width - 1, "})"), "\\1\n", t)
    r <- gsub(pattern = "\n,", replacement = ",\n", r, fixed = TRUE)
  }
  return(r)
}




#' Item Calibration via BILOG-MG
#'
#' @description \code{est_bilog} runs BILOG-MG in batch mode or reads BILOG-MG
#'   output generated by BILOG-MG program. In the first case, this function
#'   requires BILOG-MG already installed on your computer under
#'   \code{bilog_exe_folder} directory.
#'
#'   In the latter case, where appropriate BILOG-MG files are present (i.e.
#'   \code{"<analysis_name>.PAR"}, \code{"<analysis_name>.PH1"},
#'   \code{"<analysis_name>.PH2"} and \code{"<analysis_name>.PH3"} files exist)
#'   and \code{overwrite = FALSE}, there is no need for BILOG-MG program. This
#'   function can read BILOG-MG output without BILOG-MG program.
#'
#' @param x Either a \code{data.frame}, \code{matrix} or
#'   \code{\link{Response_set-class}} object. When the data is not necessary,
#'   i.e. user only wants to read the BILOG-MG output from the
#'   \code{target_dir}, then this can be set to \code{NULL}.
#' @param model The model of the items. The value is one of the
#'   following:
#'   \describe{
#'     \item{\code{"1PL"}}{One-parameter logistic model.}
#'     \item{\code{"2PL"}}{Two-parameter logistic model.}
#'     \item{\code{"3PL"}}{Three-parameter logistic model.}
#'     \item{\code{"CTT"}}{Return only Classical Test theory statistics such as
#'       p-values, point-biserial and  biserial correlations.}
#'   }
#'
#'   The default value is \code{"3PL"}.
#' @param target_dir The directory/folder where the BILOG-MG analysis and data
#'   files will be saved. The default value is the current working directory,
#'   i.e. \code{get_wd()}.
#' @param analysis_name A short file name that will be used for the data files
#'   created for the analysis.
#' @param items A vector of column names or numbers of the \code{x} that
#'   represents the responses. If, in the syntax file, no entry for item
#'   names are desired, then, simply write \code{items = "none"}.
#' @param examinee_id_var The column name or number that contains individual
#'   subject IDs. If none is provided (i.e. \code{examinee_id_var = NULL}), the
#'   program will check whether the data provided has row names.
#' @param group_var The column name or number that contains group membership
#'   information if multi-group calibration is desired. Ideally, it grouping
#'   variable is represented by single digit integers. If other type of data
#'   provided, an integer value will automatically assigned to the variables.
#'   The default value is \code{NULL}, where no multi-group analysis will be
#'   performed.
#' @param logistic A logical value. If \code{TRUE}, \code{LOGISTIC} keyword will
#'   be added to the BILOG-MG command file which means the calibration will
#'   assume the natural metric of the logistic response function in all
#'   calculations. If \code{FALSE}, the logit is multiplied by D = 1.7 to obtain
#'   the metric of the normal-ogive model. The default value is \code{TRUE}.
#' @param num_of_alternatives An integer specifying the maximum number of
#'   response alternatives in the raw data. \code{1/num_of_alternatives} is
#'   used by the analysis as automatic starting value for estimating the
#'   pseudo-guessing parameters.
#'
#'   The default value is \code{NULL}. In this case, for 3PL, 5 will be used
#'   and for 1PL and 2PL, 1000 will be used.
#'
#'   This value will be represented in BILOG-MG control file as:
#'   \code{NALT = num_of_alternatives}.
#'
#' @param criterion Convergence criterion for EM and Newton iterations. The
#'   default value is 0.01.
#' @param num_of_quadrature The number of quadrature points in MML estimation.
#'   The default value is 81. This value will be represented in BILOG-MG control
#'   file as: \code{NQPT = num_of_quadrature}. The BILOG-MG default value is
#'   20 if there are more than one group, 10 otherwise.
#' @param max_em_cycles An integer (0, 1, ...) representing the maximum number
#'   of EM cycles. This value will be represented in BILOG-MG control file as:
#'   \code{CYCLES = max_em_cycles}.
#'   The default value is 100.
#' @param newton An integer (0, 1, ...) representing the number of Gauss-Newton
#'   iterations following EM cycles. This value will be represented in BILOG-MG
#'   control file as: \code{NEWTON = newton}.
#' @param reference_group Represent which group's ability distribution will be
#'   set to mean = 0 and standard deviation = 1. For example, if the value is 1,
#'   then the group whose code is 1 will have ability distribution with mean 0
#'   and standard deviation 1. When groups are assumed to coming from a single
#'   population, set this value to 0.
#'
#'   The default value is \code{NULL}.
#'
#'   This value will be represented in BILOG-MG control file as:
#'   \code{REFERENCE = reference_group}.
#' @param fix This arguments helps to specify whether the parameters of
#'   specific items are free to be estimated or are to be held fixed at
#'   their starting values. This argument accepts a \code{data.frame} with
#'   an \code{item_id} column in which items for which the item parameters
#'   will be held fixed; \code{a}, \code{b}, \code{c} parameter values. See,
#'   examples section for a demonstration.
#'
#' @param scoring_options A string vector of keywords/options that will be added
#'   to the \code{SCORE} section in BILOG-MG syntax. Set the value of
#'   \code{scoring_options} to \code{NULL} if scoring of individual examinees is
#'   not necessary.
#'
#'   The default value is \code{c("METHOD=1", "NOPRINT")} where scale scores
#'   will be estimated using Maximum Likelihood estimation and the scoring
#'   process will not be printed to the R console (if
#'   \code{show_output_on_console = TRUE}).
#'
#'   The main option to be added to this vector is \code{"METHOD=n"}.
#'   Following options are available:
#'
#'   \describe{
#'     \item{"METHOD=1"}{Maximum Likelihood (ML)}
#'     \item{"METHOD=2"}{Expected a Posteriori (EAP)}
#'     \item{"METHOD=3"}{Maximum a Posteriori (MAP)}
#'   }
#'
#'   In addition to \code{"METHOD=n"} keyword, following keywords can be added:
#'
#'   \code{"NOPRINT"}: Suppresses the display of the scores on the R console.
#'
#'   \code{"FIT"}: likelihood ratio chi-square goodness-of-fit statistic for
#'     each response pattern will be computed.
#'
#'   \code{"NQPT=(list)"}, \code{"IDIST=n"}, \code{"PMN=(list)"},
#'   \code{"PSD=(list)"}, \code{"RSCTYPE=n"}, \code{"LOCATION=(list)"},
#'   \code{"SCALE=(list)"}, \code{"INFO=n"}, \code{"BIWEIGHT"},
#'   \code{"YCOMMON"}, \code{"POP"}, \code{"MOMENTS"},
#'   \code{"FILE"}, \code{"READF"}, \code{"REFERENCE=n"}, \code{"NFORMS=n"}
#'
#'   See BILOG-MG manual for more details about these keywords/options.
#' @param calib_options A string vector of keywords/options that will be added
#'   to the \code{CALIB} section in BILOG-MG syntax in addition to the keywords
#'   \code{NQPT}, \code{CYCLES}, \code{NEWTON}, \code{CRIT}, \code{REFERENCE}.
#'
#'   The default value is \code{c("NORMAL")}.
#'
#'   When \code{"NORMAL"} is included in \code{calib_options}, the prior
#'   distributions of ability in the population is assumed to have normal
#'   distribution.
#'
#'   When \code{"COMMON"} is included in \code{calib_options}, a common value
#'   for the lower asymptote for all items in the 3PL model will be estimated.
#'
#'   If items will be calibrated using \code{"RASCH"} model, set
#'   \code{model = "Rasch"}, instead of adding \code{"RASCH"} keyword to
#'   \code{calib_options}.
#'
#'   Following keywords/options can be added to \code{calib_options}:
#'
#'   \code{"PRINT=n"}, \code{"IDIST=n"}, \code{"PLOT=n"}, \code{"DIAGNOSIS=n"},
#'   \code{"REFERENCE=n"}, \code{"SELECT=(list)"}, \code{"RIDGE=(a,b,c)"},
#'   \code{"ACCEL=n"}, \code{"NSD=n"}, \code{"COMMON"}, \code{"EMPIRICAL"},
#'   \code{"NORMAL"}, \code{"FIXED"}, \code{"TPRIOR"}, \code{"SPRIOR"},
#'   \code{"GPRIOR"}, \code{"NOTPRIOR"}, \code{"NOSPRIOR"}, \code{"NOGPRIOR"},
#'   \code{"READPRIOR"}, \code{"NOFLOAT"}, \code{"FLOAT"}, \code{"NOADJUST"},
#'   \code{"GROUP-PLOT"}, \code{"NFULL"}, \code{"CHI=(a,b)"}.
#'
#'   See BILOG-MG manual for more details about these keywords/options.
#'
#'   NOTE: Do not add any of the following keywords to \code{calib_options}
#'   since they will already be included:
#'
#'   \code{NQPT}, \code{CYCLES}, \code{NEWTON}, \code{CRIT}, \code{REFERENCE}
#'
#' @param prior_ability Prior ability is the quadrature points and weights of
#'   the discrete finite representations of the prior distribution for the
#'   groups. It should be a list in the following form:
#'
#'   \code{list(<GROUP-NAME-1> = list(points = ...., weights = ...),
#'              <GROUP-NAME-2> = list(points = ...., weights = ...),
#'              ...
#'              )}
#'
#'   \code{GROUP-NAME-1} is the name of the first group, \code{GROUP-NAME-2} is
#'   the name of the second group, etc.
#'
#'   See examples section for an example implementation.
#'
#' @param prior_ip Specify priors distributions for item parameters. The
#'   default value is \code{NULL}, where BILOG-MG defaults will be used. In
#'   order to specify priors, a list of one or more of the following elements
#'   needs to be provided:
#'   \describe{
#'     \item{"ALPHA"}{"'alpha' parameters for the beta prior distribution of
#'       lower asymptote (guessing) parameters"}
#'     \item{"BETA"}{"'beta' parameters for the beta prior distribution of
#'       lower asymptote (guessing) parameters."}
#'     \item{"SMU"}{prior means for slope parameters}
#'     \item{"SSIGMA"}{prior standard deviations for slope parameters}
#'     \item{"TMU"}{prior means for threshold parameters}
#'     \item{"TSIGMA"}{prior standard deviations for threshold parameters}
#'     }
#'   Quoted descriptions were taken from BILOG-MG manual.
#'
#'   Here are couple examples:
#'   \code{list(ALPHA = 4, BETA = 3, SMU = 1, SSIGMA = 1.648, TMU = 0,
#'              TSIGMA = 2)}
#'
#'   A very strong prior for guessing which almost fixes all guessing
#'   parameters at 0.2:
#'
#'   \code{list(ALPHA = 1000000, BETA = 4000000)}
#'
#'   Fix guessing at 0.25:
#'   \code{list(ALPHA = 1000000, BETA = 3000000)}
#'
#'   More generally, one can play with the alpha and beta parameters to obtain
#'   desired number considering the mode of beta distribution is:
#'
#'   \deqn{mode = \frac{\alpha - 1}{\alpha + \beta - 2}}
#'
#'   Also, one can set SSIGMA or TSIGMA to a very small value to effectively
#'   fix the item parameters, for example set \code{TSIGMA = 0.005} or
#'   \code{SSIGMA = 0.001} to effectively fix those item parameters. Note that
#'   there might be convergence issues with these restrictions.
#'
#'   Note that a non-null \code{prior_ip} value will automatically add
#'   \code{READPRIOR} option to \code{CALIB} section.
#'
#' @param overwrite If \code{TRUE} and there are already a BILOG-MG analysis
#'   files in the target path with the same name, these file will be
#'   overwritten.
#' @param show_output_on_console logical (not NA), indicates whether to capture
#'   the output of the command and show it on the R console. The default value
#'   is \code{TRUE}.
#' @param bilog_exe_folder The location of the \code{"blm1.exe"},
#'   \code{"blm2.exe"} and \code{"blm3.exe"} files. The default location is
#'   \code{file.path("C:/Program Files/BILOGMG")}.
#'
#' @return A list of following objects:
#'   \describe{
#'     \item{"ip"}{An \code{\link{Itempool-class}} object holding the item
#'       parameters. Please check whether model converged (using
#'       \code{...$converged}) before interpreting/using \code{ip}.
#'       This element will not be created when \code{model = "CTT"}.}
#'     \item{"score"}{A data frame object that holds the number of item
#'       examinee has attempted (\code{tried}), the number of item examinee got
#'       right (\code{right}), the estimated scores of examinees
#'       (\code{ability}), the standard errors of ability estimates (\code{se}),
#'       and the probability of the response string (\code{prob}). This element
#'       will not be created when \code{model = "CTT"}.}
#'     \item{"ctt"}{The Classical Test Theory (CTT) stats such as p-value,
#'       biserial, point-biserial estimated by BILOG-MG. If there are groups,
#'       then the CTT statistics for groups can be found in
#'       \code{ctt$group$GROUP-NAME}. Overall statistics for the whole group is
#'       at \code{ctt$overall}.
#'       }
#'     \item{"failed_items"}{A data frame consist of items that cannot be
#'       estimated.}
#'     \item{"syntax"}{The syntax file.}
#'     \item{"converged"}{A logical value indicating whether a model has been
#'       converged or not. If the value is \code{TRUE}, model has been
#'       converged. This element will not be created when \code{model = "CTT"}.}
#'     \item{"cycle"}{Number of cycles run before calibration converge or fail
#'       to converge.}
#'     \item{"largest_change"}{Largest change between the last two cycles.}
#'     \item{"neg_2_log_likelihood"}{-2 Log Likelihood value. This value is
#'       \code{NULL}, when model does not converge. This element will not be
#'       created when \code{model = "CTT"}.}
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
#' #############################################
#' ############## Example 1 - 2PL ##############
#' #############################################
#' # IRT Two-parameter Logistic Model Calibration
#'
#' # Create responses to be used in BILOG-MG estimation
#' true_theta <- rnorm(4000)
#' true_ip <- generate_ip(n = 30, model = "2PL")
#' resp <- sim_resp(true_ip, true_theta)
#'
#' # The following line will run BILOG-MG, estimate 2PL model and put the
#' # analysis results under the target directory:
#' bilog_calib <- est_bilog(x = resp, model = "2PL",
#'                          target_dir = "C:/Temp/Analysis",
#'                          overwrite = TRUE)
#' # Check whether the calibration converged
#' bilog_calib$converged
#'
#' # Get the estimated item pool
#' bilog_calib$ip
#'
#' # See the BILOG-MG syntax
#' cat(bilog_calib$syntax)
#'
#' # See the classical test theory statistics estimated by BILOG-MG:
#' bilog_calib$ctt
#'
#' # Get -2LogLikelihood for the model (mainly for model comparison purposes):
#' bilog_calib$neg_2_log_likelihood
#'
#' # Get estimated scores
#' head(bilog_calib$score)
#'
#' # Compare true and estimated abilities
#' plot(true_theta, bilog_calib$score$ability, xlab = "True Theta",
#'      ylab = "Estimated theta")
#' abline(a = 0, b = 1, col = "red", lty = 2)
#'
#' # Compare true item parameters
#' plot(true_ip$a, bilog_calib$ip$a, xlab = "True 'a'", ylab = "Estimated 'a'")
#' abline(a = 0, b = 1, col = "red", lty = 2)
#'
#' plot(true_ip$b, bilog_calib$ip$b, xlab = "True 'b'", ylab = "Estimated 'b'")
#' abline(a = 0, b = 1, col = "red", lty = 2)
#'
#' # Note that Bilog-MG centers the ability at mean 0.
#' mean(bilog_calib$score$ability)
#'
#' # Quadrature points and posterior weights:
#' head(bilog_calib$posterior_dist)
#'
#' #############################################
#' ############## Example 2 - EAP ##############
#' #############################################
#' # Getting Expected-a-posteriori theta scores
#' result <- est_bilog(x = resp, model = "2PL",
#'                     scoring_options = c("METHOD=2", "NOPRINT"),
#'                     target_dir = "C:/Temp/Analysis",
#'                     overwrite = TRUE)
#' head(result$score)
#'
#'
#' ###############################################
#' ############## Example 3 - Rasch ##############
#' ###############################################
#' # Rasch Model Calibration
#' true_theta <- rnorm(400)
#' true_ip <- generate_ip(n = 30, model = "Rasch")
#' resp <- sim_resp(true_ip, true_theta)
#'
#' # Run calibration
#' bilog_calib <- est_bilog(x = resp, model = "Rasch",
#'                          target_dir = "C:/Temp/Analysis",
#'                          overwrite = TRUE)
#' bilog_calib$ip
#'
#' plot(true_ip$b, bilog_calib$ip$b, xlab = "True 'b'", ylab = "Estimated 'b'")
#' abline(a = 0, b = 1, col = "red", lty = 2)
#'
#' # Note that the 'b' parameters are rescaled so that their arithmetic mean
#' # equals 0.0.
#' mean(bilog_calib$ip$b)
#'
#'
#' #############################################
#' ############## Example 4 - 3PL ##############
#' #############################################
#' # IRT Three-parameter Logistic Model Calibration
#'
#' # Create responses to be used in BILOG-MG estimation
#' true_theta <- rnorm(4000)
#' true_ip <- generate_ip(n = 30, model = "3PL")
#' resp <- sim_resp(true_ip, true_theta)
#'
#' # The following line will run BILOG-MG, estimate 2PL model and put the
#' # analysis results under the target directory:
#' bilog_calib <- est_bilog(x = resp, model = "3PL",
#'                          target_dir = "C:/Temp/Analysis",
#'                          overwrite = TRUE)
#'
#' bilog_calib$ip
#'
#' #############################################
#' ############## Example 5 - 1PL ##############
#' #############################################
#' # One-Parameter Logistic Model Calibration
#' true_theta <- rnorm(800)
#' true_ip <- generate_ip(n = 30, model = "2PL")
#' # Set 'a' parameters to a fixed number
#' true_ip$a <- 1.5
#' resp <- sim_resp(true_ip, true_theta)
#'
#' # Run calibration
#' bilog_calib <- est_bilog(x = resp, model = "1PL",
#'                          target_dir = "C:/Temp/Analysis",
#'                          overwrite = TRUE)
#' # Note that all 'a' parameter values and all 'se_a' values are the same:
#' bilog_calib$ip
#'
#' plot(true_ip$b, bilog_calib$ip$b, xlab = "True 'b'", ylab = "Estimated 'b'")
#' abline(a = 0, b = 1, col = "red", lty = 2)
#'
#'
#' #############################################################
#' ############## Example 6.1 - Multi-group - 3PL ##############
#' #############################################################
#' # Multi-group IRT calibration - 3PL
#'
#' ## Generate Data ##
#' ip <- generate_ip(n = 35, model = "3PL", D = 1.7)
#' n_upper <- sample(1200:3000, 1)
#' n_lower <- sample(1900:2800, 1)
#' theta_upper <- rnorm(n_upper, 1.5, .25)
#' theta_lower <- rnorm(n_lower)
#' resp <- sim_resp(ip = ip, theta = c(theta_lower, theta_upper))
#' # Create response data where first column group information
#' dt <- data.frame(level = c(rep("Lower", n_lower), rep("Upper", n_upper)),
#'                  resp)
#'
#' ## Run Calibration ##
#' mg_calib <- est_bilog(x = dt, model = "3PL",
#'                       group_var = "level",
#'                       reference_group = "Lower",
#'                       items = 2:ncol(dt), # Exclude the 'group' column
#'                       num_of_alternatives = 5,
#'                       # Use MAP ability estimation.
#'                       # "FIT": calculate GOF for response patterns
#'                       scoring_options = c("METHOD=3", "NOPRINT", "FIT"),
#'                       target_dir = "C:/Temp/Analysis", overwrite = TRUE,
#'                       show_output_on_console = FALSE)
#' # Estimated item pool
#' mg_calib$ip
#' # Print group means
#' mg_calib$group_info
#' # Check Convergence
#' mg_calib$converged
#' # Print estimated scores of first five examinees
#' head(mg_calib$score)
#'
#' # Posterior distributions of 'Lower' (in red)  and 'Upper' group
#' plot(mg_calib$posterior_dist$Upper$point,
#'      mg_calib$posterior_dist$Upper$weight)
#' points(mg_calib$posterior_dist$Lower$point,
#'        mg_calib$posterior_dist$Lower$weight, col = "red")
#'
#'
#' #############################################################
#' ############## Example 6.2 - Multi-group - Response_set #####
#' #############################################################
#' # Multi-group IRT calibration - Response_set 2PL
#'
#' ## Generate Data ##
#' ip <- generate_ip(n = 35, model = "2PL", D = 1.7)
#' n_upper <- sample(1000:2000, 1)
#' n_lower <- sample(1000:2000, 1)
#' resp_set <- generate_resp_set(
#'   ip = ip, theta = c(rnorm(n_lower), rnorm(n_upper, 1.5, .25)))
#' # Attach the group information
#' resp_set$mygroup <- c(rep("Lower", n_lower), rep("Upper", n_upper))
#'
#' ## Run Calibration ##
#' mg_calib <- est_bilog(x = resp_set,
#'                       model = "2PL",
#'                       group_var = "mygroup",
#'                       reference_group = "Lower",
#'                       target_dir = "C:/Temp/Analysis",
#'                       overwrite = TRUE,
#'                       show_output_on_console = FALSE)
#' # Estimated item pool
#' mg_calib$ip
#' # Print group means
#' mg_calib$group_info
#'
#' ###############################################################
#' ############## Example 6.3 - Multi-group - 1PL ################
#' ###############################################################
#' # Multi-group IRT calibration - 1PL
#'
#' ## Generate Data ##
#' n_item <- sample(30:40, 1)
#' ip <- generate_ip(n = n_item, model = "2PL", D = 1.7)
#' ip$a <- 1.25
#' n_upper <- sample(700:1000, 1)
#' n_lower <- sample(1200:1800, 1)
#' theta_upper <- rnorm(n_upper, 1.5, .25)
#' theta_lower <- rnorm(n_lower)
#' resp <- sim_resp(ip = ip, theta = c(theta_lower, theta_upper))
#' # Create response data where first column group information
#' dt <- data.frame(level = c(rep("Lower", n_lower), rep("Upper", n_upper)),
#'                  resp)
#'
#' ## Run Calibration ##
#' mg_calib <- est_bilog(x = dt,
#'                       model = "1PL",
#'                       group_var = "level",
#'                       reference_group = "Lower",
#'                       items = 2:ncol(dt), # Exclude the 'group' column
#'                       target_dir = "C:/Temp/Analysis",
#'                       overwrite = TRUE,
#'                       show_output_on_console = FALSE)
#' # Estimated item pool
#' mg_calib$ip
#' # Print group means
#' mg_calib$group_info
#' # Check Convergence
#' mg_calib$converged
#' # Print estimated scores of first five examinees
#' head(mg_calib$score)
#'
#'
#' ###############################################################
#' ############## Example 6.4 - Multi-group - Prior Ability ######
#' ###############################################################
#' # Multi-group IRT calibration - 3PL with user supplied prior ability
#' # parameters
#' n_item <- sample(40:70, 1)
#' ip <- generate_ip(n = n_item, model = "3PL", D = 1.7)
#' n_upper <- sample(2000:4000, 1)
#' n_lower <- sample(3000:5000, 1)
#' theta_upper <- rgamma(n_upper, shape = 2, rate = 2)
#' # hist(theta_upper)
#' theta_lower <- rnorm(n_lower)
#' true_theta <- c(theta_lower, theta_upper)
#' resp <- sim_resp(ip = ip, theta = true_theta, prop_missing = .2)
#' # Create response data where first column group information
#' dt <- data.frame(level = c(rep("Lower", n_lower), rep("Upper", n_upper)),
#'                  resp)
#'
#' # Set prior ability parameters
#' points <- seq(-4, 4, .1)
#' prior_ability = list(
#'   Lower = list(points = points, weights = dnorm(points)),
#'   # Also try misspecified prior:
#'   # Upper = list(points = points, weights = dnorm(points, 1, .25))
#'   Upper = list(points = points, weights = dgamma(points, 2, 2))
#'   )
#' mg_calib <- est_bilog(x = dt,
#'                       model = "3PL",
#'                       group_var = "level",
#'                       reference_group = "Lower",
#'                       items = 2:ncol(dt), # Exclude the 'group' column
#'                       calib_options = c("IDIST = 2"),
#'                       prior_ability = prior_ability,
#'                       # Use MAP ability estimation.
#'                       scoring_options = c("METHOD=3"),
#'                       target_dir = target_dir,
#'                       overwrite = TRUE,
#'                       show_output_on_console = FALSE)
#'
#' # Check whether model has convergence
#' mg_calib$converged
#'
#' # Group information
#' mg_calib$group_info
#'
#' # Quadrature points and posterior weights:
#' head(mg_calib$posterior_dist$Lower)
#'
#' plot(mg_calib$posterior_dist$Lower$point,
#'      mg_calib$posterior_dist$Lower$weight,
#'      xlab = "Quadrature Points",
#'      ylab = "Weights",
#'      xlim = c(min(c(mg_calib$posterior_dist$Lower$point,
#'                     mg_calib$posterior_dist$Upper$point)),
#'               max(c(mg_calib$posterior_dist$Lower$point,
#'                     mg_calib$posterior_dist$Upper$point))),
#'      ylim = c(min(c(mg_calib$posterior_dist$Lower$weight,
#'                     mg_calib$posterior_dist$Upper$weight)),
#'               max(c(mg_calib$posterior_dist$Lower$weight,
#'                     mg_calib$posterior_dist$Upper$weight))))
#' points(mg_calib$posterior_dist$Upper$point,
#'       mg_calib$posterior_dist$Upper$weight, col = "red")
#'
#' # Comparison of true and estimated item parameters
#' plot(ip$a, mg_calib$ip$a, xlab = "True 'a'", ylab = "Estimated 'a'")
#' plot(ip$b, mg_calib$ip$b, xlab = "True 'b'", ylab = "Estimated 'b'")
#' plot(ip$c, mg_calib$ip$c, xlab = "True 'c'", ylab = "Estimated 'c'")
#'
#' # Ability parameters
#' plot(true_theta, mg_calib$score$ability,
#'      xlab = "True Theta",
#'      ylab = "Estimated Theta")
#' abline(a = 0, b = 1, col = "red")
#'
#'
#'
#' ####################################################################
#' ############## Example 7 - Read Pars without BILOG-MG ##############
#' ####################################################################
#' # When user wants to read BILOG-MG output saved in the directory "Analysis/"
#' # with file names "my_analysis.PH1", "my_analysis.PH2", etc.,
#' use the following syntax to read Bilog output files without running the
#' calibration:
#' # (The following code does not require an installed BILOG-MG program on the
#' #  computer.)
#' result <- est_bilog(target_dir = file.path("Analysis/"), model = "3PL",
#'                     analysis_name = "my_analysis", overwrite = FALSE)
#'
#'
#'
#' ####################################################################
#' ############## Example 8 - Fixed Item Parameters ###################
#' ####################################################################
#' # The idea is to fix individual item parameters to certain values.
#' # If all of values of a certain item parameter(s) need to be fixed,
#' # then, strong priors can also be used. See the documentation for
#' # "prior_ip" argument.
#'
#' # Create responses to be used in BILOG-MG estimation
#' true_theta <- rnorm(3000)
#' true_ip <- generate_ip(n = 30, model = "3PL")
#' resp <- sim_resp(true_ip, true_theta)
#'
#' # Setup the data frame that will hold 'item_id's to be fixed, and the
#' # item parameters to be fixed.
#' fix_pars <- data.frame(item_id = c("Item_5", "Item_4", "Item_10"),
#'                        a = c(1, 1.5, 1.75),
#'                        b = c(-1, 0.25, 0.75),
#'                        c = c(.15, .25, .35))
#'
#' fixed_calib <- est_bilog(x = resp, fix = fix_pars,
#'                          target_dir = "C:/Temp/Analysis", overwrite = TRUE)
#' # Check item parameters for  Item_4, Item_5, Item_10:
#' fixed_calib$ip
#'
#' ######### #########
#' # If only some of the parameters are supplied, the defaults will be used
#' # for the missing parameters. For example, for the example below, the
#' # default 'a' parameter value is 1, and the default 'c' parameter value is
#' # (1/num_of_alternatives) = (1/5) = 0.2.
#' fix_pars2 <- data.frame(item_id = c("Item_1", "Item_2", "Item_3"),
#'                         b = c(-1, 0.25, 0.75))
#'
#' fixed_calib2 <- est_bilog(x = resp, fix = fix_pars2,
#'                           target_dir = "C:/Temp/Analysis", overwrite = TRUE)
#' # Check item parameters for  Item_4, Item_5, Item_10:
#' fixed_calib2$ip
#'
#'
#' ##################################################################
#' ############## Example 9 - 3PL with Common Guessing ##############
#' ##################################################################
#' # IRT Three-parameter Logistic Model Calibration with Common Guessing
#'
#' # Create responses to be used in BILOG-MG estimation
#' true_theta <- rnorm(4000)
#' true_ip <- generate_ip(n = 30, model = "3PL")
#' resp <- sim_resp(true_ip, true_theta)
#'
#' # Run calibration:
#' bilog_calib <- est_bilog(x = resp, model = "3PL",
#'                          target_dir = "C:/Temp/Analysis",
#'                          calib_options = c("NORMAL", "COMMON"),
#'                          overwrite = TRUE)
#'
#' # Note the 'c' parameters
#' bilog_calib$ip
#'
#' ##################################################################
#' ############## Example 10 - 3PL with Fixed Guessing ##############
#' ##################################################################
#' # IRT Three-parameter Logistic Model Calibration with Fixed Guessing
#' # The aim is to fix guessing parameters of all items to a fixed
#' # number like 0.25
#' true_theta <- rnorm(3000)
#' true_ip <- generate_ip(n = 30, model = "3PL")
#' true_ip$c <- 0.25
#' resp <- sim_resp(true_ip, true_theta)
#' prc1 <- est_bilog(x = resp, model = "3PL", target_dir = "C:/Temp/Analysis",
#'                   prior_ip = list(ALPHA = 10000000, BETA = 30000000),
#'                   overwrite = TRUE)
#'
#'
#' } # end dontrun
est_bilog <- function(
  x = NULL,
  model = "3PL",
  target_dir = getwd(),
  analysis_name = "bilog_calibration",
  items = NULL,
  examinee_id_var = NULL,
  group_var = NULL,
  logistic = TRUE,
  num_of_alternatives = NULL,
  criterion = 0.01,
  num_of_quadrature = 81,
  max_em_cycles = 100,
  newton = 20,
  reference_group = NULL,
  fix = NULL,
  scoring_options = c("METHOD=1", "NOPRINT"),
  calib_options = c("NORMAL"),
  prior_ability = NULL,
  prior_ip = NULL,
  overwrite = FALSE,
  show_output_on_console = TRUE,
  bilog_exe_folder = file.path("C:/Program Files/BILOGMG")
  ) {

  result <- list(ip = NULL,
                 score = NULL,
                 ctt = NULL,
                 failed_items = NULL,
                 syntax = NULL,
                 input = c(as.list(environment()), call = match.call())
                 )

  result$input$x <- NULL
  # set the input list
  # result$input <- list(
    # model = model,
    # target_dir = target_dir,
    # analysis_name = analysis_name,
    # items = items,
    # examinee_id_var = examinee_id_var,
    # group_var = group_var,
    # logistic = logistic,
    # num_of_alternatives = num_of_alternatives,
    # criterion = criterion,
    # num_of_quadrature = num_of_quadrature,
    # max_em_cycles = max_em_cycles,
    # newton = newton,
    # reference_group = reference_group,
    # fix = fix,
    # scoring_options = scoring_options,
    # calib_options = calib_options,
    # prior_ability = prior_ability,
    # bilog_exe_folder = bilog_exe_folder
    # )

  if (!model %in% c("CTT", "Rasch", "1PL", "2PL", "3PL"))
    stop("Invalid 'model' argument. 'model' should be either '1PL', '2PL' ",
         "'3PL', 'Rasch' or 'CTT'.", call. = FALSE)


  par_file <- file.path(target_dir, paste0(analysis_name, ".PAR"))
  ph1_file <- file.path(target_dir, paste0(analysis_name, ".PH1"))
  ph2_file <- file.path(target_dir, paste0(analysis_name, ".PH2"))
  ph3_file <- file.path(target_dir, paste0(analysis_name, ".PH3"))

  # If fix is not NULL, this is where all fixed item parameters will be saved.
  fix_file <- file.path(target_dir, paste0(analysis_name, "_fix.PAR"))
  score_file <- file.path(target_dir, paste0(analysis_name, ".SCO"))
  cov_file <- file.path(target_dir, paste0(analysis_name, ".COV"))

  D <- ifelse(logistic, 1, 1.7)

  target_path <- file.path(target_dir, paste0(analysis_name, ".blm"))


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

  if (overwrite || any(!file.exists(c(ph1_file, ph2_file, ph3_file,
                                      par_file)))) {
    # Check whether the filename is less than 128 characters:
    if (nchar(target_path) > 128)
      stop(paste0("\nThe following path for control file is ",
                  nchar(target_path), " character long, i.e. longer than the ",
                  "maximum length of 128 characters:\n\"", target_path,
                  "\"\nPlease choose a shorter 'analysis_name' or change the ",
                  "'target_dir'."), call. = FALSE)
    # Determines the tab size, space added to lines following main analysis
    # commands
    tab <- paste0(rep(" ", nchar("GLOBAL") + 2), collapse = "")
    text <- paste0(analysis_name, "\n\n")

    # If overwrite is TRUE, delete important output files:
    if (overwrite) {
      extensions <- c("blm", "dat", "PH1", "PH2", "PH3", "COV", "NFN", "PAR",
                      "SCO")
      suppressWarnings(file.remove(
        file.path(target_dir, paste0(analysis_name, ".", extensions))))
    }
    # Create the data file:
    data_output <- bilog_create_datafile(
      x = x,
      items = items,
      examinee_id_var = examinee_id_var,
      group_var = group_var,
      reference_group = reference_group,
      target_path = file.path(target_dir, paste0(analysis_name, ".dat")),
      create_np_key_file = TRUE,
      overwrite = overwrite)
    # for non-multigroup data num_of_groups is 0.
    num_of_groups <- data_output$num_of_groups
    group_info <- data_output$group_info
    reference_group <- data_output$reference_group

    # in case x is Response_set, make sure x is matrix after this point.
    x <- data_output$x
    #########################################@###
    ################### GLOBAL ##############@###
    #########################################@###
    temp_text <- wrap_text(paste0(
      ">GLOBAL DFNAME='", data_output$data_file_path, "',\n"))
    # # Add the file name where parameters values that should be fixed saved:
    # if (!is.null(fix)) {
    #   temp_text <- paste0(temp_text, wrap_text(paste0(
    #     tab, "PRNAME='", normalizePath(fix_file, mustWork = FALSE), "',\n")))
    # }
    # Add model
    num_of_pars <- as.integer(gsub("(.*)PL", "\\1", ifelse(
      model %in% c("CTT", "Rasch"), "1PL", model)))
    temp_text <- paste0(temp_text, tab, "NPARM = ", num_of_pars , ",\n", tab)
    # Add Logistic: when added the natural metric of the logistic response
    # function is assumed in all calculations.
    if (logistic) temp_text <- paste0(temp_text, "LOGISTIC,\n", tab)

    # Add SAVE statement
    # This indicates that the SAVE command will follow the GLOBAL command.
    temp_text <- paste0(temp_text, "SAVE;\n")

    text <- c(text, temp_text)

    ########################################@###
    ################### SAVE ###############@###
    ########################################@###
    temp_text <- paste0(">SAVE ")
    tab <- paste0(rep(" ", nchar("SAVE") + 2), collapse = "")
    # Add PARM
    temp_text <- wrap_text(paste0(
      temp_text, "PARM='", normalizePath(par_file, mustWork = FALSE), "',\n"))
    # Add SCORE
    temp_text <- paste0(temp_text, wrap_text(paste0(
      tab, "SCORE='", normalizePath(score_file, mustWork = FALSE), "',\n")))
    # Add COVARIANCE
    temp_text <- paste0(temp_text, wrap_text(paste0(
      tab, "COVARIANCE='", normalizePath(cov_file, mustWork = FALSE), "';\n")))
    # # Add TSTAT
    # temp_text <- paste0(
    #   temp_text, "',\n", tab, "TSTAT = '",
    #   normalizePath(file.path(target_dir, paste0(analysis_name, ".tst")),
    #                 mustWork = FALSE))
    # # Add PDISTRIB
    # temp_text <- paste0(
    #   temp_text, "',\n", tab, "PDISTRIB = '",
    #   normalizePath(file.path(target_dir, paste0(analysis_name, ".dst")),
    #                 mustWork = FALSE))

    # temp_text <- paste0(temp_text, "';\n")

    text <- c(text, temp_text)

    ##########################################@###
    ################### LENGTH ###############@###
    ##########################################@###
    temp_text <- paste0(">LENGTH NITEMS = (", data_output$num_of_items, ");\n")

    text <- c(text, temp_text)

    #########################################@###
    ################### INPUT ###############@###
    #########################################@###
    # This section provides information that describes the raw data file.
    temp_text <- paste0(">INPUT NTOTAL = ", data_output$num_of_items)
    tab <- paste0(rep(" ", nchar("INPUT") + 2), collapse = "")
    # Add number of alternatives
    if (!is.null(num_of_alternatives) && is.numeric(num_of_alternatives))
      temp_text <- paste0(temp_text, ",\n", tab, "NALT = ", num_of_alternatives)

    # Add NFNAME:
    temp_text <- paste0(
      temp_text, ",\n", wrap_text(paste0(tab, "NFNAME = '",
      normalizePath(data_output$np_key_file_path), "',\n")))
    # Add NIDCHAR
    temp_text <- paste0(temp_text, tab,
                        "NIDCHAR = ", data_output$num_id_char)

    # Add number of groups NGROUP:
    if (num_of_groups > 1)
      temp_text <- paste0(temp_text, ",\n", tab,
                          "NGROUP = ", num_of_groups)

    temp_text <- paste0(temp_text, ";\n")

    text <- c(text, temp_text)

    #########################################@###
    ################### ITEMS ###############@###
    #########################################@###
    # Setup the items to be printed.
    if (is.null(items)) {
      items <- colnames(x)

      # Make sure 'items' is valid, i.e. each item is 8 character or less:
      if (!is.null(items) && any(nchar(items) > 8)) {
        stop("Invalid item IDs. Bilog-MG do not accept item IDs that are ",
             "longer than eight characters.")
      }

      # Make sure to remove examinee_id_var
      if (!is.null(examinee_id_var)) {
        if (examinee_id_var %in% items) {
          items <- items[items != examinee_id_var]
        } else {
          items <- items[-examinee_id_var]
        }
      }
      # Make sure to remove group_var
      if (!is.null(group_var)) {
        if (group_var %in% items) {
          items <- items[items != group_var]
        } else {
          items <- items[-group_var]
        }
      }
    } else if (length(items) == 1 && items == "none") {
      items <- NULL
    }
    if (is.null(items)) {
      text <- c(text, paste0(">ITEMS ;\n"))
    } else {
      if (any(nchar(items) > 8)) {
        stop("Invalid item IDs. Bilog-MG do not accept item IDs that are ",
             "longer than eight characters.")
      }
      # temp_text <- wrap_text(paste0(
      #   ">ITEMS INAMES=(", paste0("'", items, "'", collapse = ","), ");\n"))
      temp_text <- paste0(wrap_text(paste0(
        ">ITEMS INAMES=(", paste0("'", items, "'", collapse = ", "), ");"),
        tab = paste0(tab, "        "), skip_tab = 1), "\n")

      text <- c(text, temp_text)
    }

    ########################################@###
    ################### TEST ###############@###
    ########################################@###
    # This section provides information that describes the raw data file.
    temp_text <- paste0(">TEST1 TNAME = 'TEST01'")
    tab <- paste0(rep(" ", nchar("TEST1") + 2), collapse = "")
    temp_text <- paste0(temp_text, ",\n", tab, "INUMBER = (1(1)",
                        data_output$num_of_items, ")")

    # Add item parameters that are fixed:
    if (!is.null(fix)) {
      # check whether fix is a data frame with relevant item_id's and
      # item parameters
      if (!is(fix, "data.frame") ||
          !"item_id" %in% colnames(fix) ||
          # ncol(fix) != num_of_pars + 1 ||
          !any(c("a", "b", "c") %in% colnames(fix))
          )
        stop(paste0("Invalid 'fix' argument. 'fix' should be a data.frame with",
                    " 'item_id' column and ", num_of_pars, " for item ",
                    "parameters that needs to be fixed. "), call. = FALSE)
      if (model == "CTT") {
        stop("Invalid 'model' argument. 'fix' cannot be used when 'model' is ",
             "'CTT'.", call. = FALSE)
      }
      if (!all(fix$item_id %in% items))
        stop("Invalid 'fix' argument. All of the 'item_id' in the 'fix' ",
             "data.frame should correspond to item_id's of the responses.",
             call. = FALSE)
      ### Setup the "FIX = " part; fix_p="fix_pattern" ###
      fix_p <- rle(1 * (items %in% fix$item_id))
      fix_p <- paste0(fix_p$values, "(0)", fix_p$lengths, collapse = ",")
      temp_text <- paste0(temp_text, ",\n",
                          wrap_text(paste0(tab, "FIX = (", fix_p, ")")))

      ### Write the PRNAME file: ###
      temp_par <- data.frame(par = c("a", "b", "c"),
                             bilog_name = c("SLOPE", "THRESHLD", "GUESS"),
                             default = c(1, 0, 0))
      for (i in 1:nrow(temp_par)) {
        if (temp_par$par[i] %in% colnames(fix)) {
          temp <- rep(temp_par$default[i], length(items))
          for (j in 1:nrow(fix)) {
            temp[fix$item_id[j] == items] <- fix[j, temp_par$par[i]]
          }
          temp_text <- paste0(
            temp_text, ",\n", wrap_text(paste0(
              tab, temp_par$bilog_name[i], " = (",
              paste0(temp, collapse = ","), ")")))
          # temp_text <- paste0(
          #   temp_text, ",\n", wrap_text(paste0(
          #     tab, temp_par$bilog_name[i], " = (",
          #     paste0(fix[, temp_par$par[i]], collapse = ","), ")")))
        }
      }
      # fix_copy <- fix
      # fix_copy$item_id <- which(items %in% fix$item_id)
      # fix_copy[] <- lapply(fix_copy, as.character)
      # temp_pattern <- paste0("%", apply(sapply(fix_copy, nchar), 2, max), "s",
      #                        collapse = "   ")
      #
      # # see p.222-223 of BILOT-MG Manual for FIX parameter
      # fix_content <- c(as.character(nrow(fix_copy)),
      #                  do.call(sprintf, c(fmt = temp_pattern, fix_copy)))
      # writeLines(fix_content, fix_file)
    }

    temp_text <- paste0(temp_text, ";\n")
    text <- c(text, temp_text)

    ##########################################@###
    ################### GROUPS ###############@###
    ##########################################@###
    temp_text <- ""
    for (g in seq_len(num_of_groups)) {

      tab <- paste0(rep(" ", nchar("GROUP1") + 2), collapse = "")
      temp_text <- paste0(temp_text,
                          ">GROUP", g, " GNAME = '", group_info$name[g], "', ",
                          "LENGTH = ", data_output$num_of_items, ",\n", tab,
                          "INUMBER = (1(1)", data_output$num_of_items, ")")
      temp_text <- paste0(temp_text, ";\n")
    }
    text <- c(text, temp_text)

    ### Formal Statement ###
    text <- c(text, paste0(data_output$formal_statement, "\n"))

    #########################################@###
    ################### CALIB ###############@###
    #########################################@###
    temp_text <- paste0(">CALIB ")
    tab <- paste0(rep(" ", nchar("CALIB") + 2), collapse = "")

    # Add NQPT
    temp_text <- paste0(temp_text, "NQPT = ", num_of_quadrature)

    # Add CYCLES
    temp_text <- paste0(temp_text, ",\n", tab, "CYCLES = ", max_em_cycles)

    # Add NEWTON
    temp_text <- paste0(temp_text, ",\n", tab, "NEWTON = ", newton)

    # Add CRIT
    temp_text <- paste0(temp_text, ",\n", tab, "CRIT = ", criterion)

    # If model is Rasch, add "RASCH" keyword to calib_options, also make sure
    # "RASCH" keyword appears in calib_options only once.
    if (model == "Rasch") {
      calib_options <- c(calib_options["rasch" != tolower(calib_options)],
                         "RASCH")
    }

    # Add IDIST
    # If prior_ability is not NULL and group_var is not NULL, and "IDIST = 2" is
    # not in the calib_options, then add it.
    if (!is.null(prior_ability) &&
        !is.null(group_var) &&
        !any(grepl("idist[ ]?=", tolower(calib_options))))
      calib_options <- c(calib_options, "IDIST = 2")

    # Add READPRIOR to calib_options if prior_ip is not NULL and valid:
    prior_ip_names <- c("ALPHA", "BETA", "SMU", "SSIGMA", "TMU", "TSIGMA")
    if (!is.null(prior_ip) &&
        is.list(prior_ip) &&
        !is.null(names(prior_ip)) &&
        any(toupper(names(prior_ip)) %in% prior_ip_names)
        ) {
      calib_options <- c(calib_options, "READPRI")
      # if "READPRIOR" is not already in calib_options, add it
      if (!any(grepl("*readpri", tolower(calib_options))))
        calib_options <- c(calib_options, "READPRI")
    }

    # Add calib_options
    if (length(calib_options) > 0)
      for (i in calib_options) temp_text <- paste0(temp_text, ",\n", tab, i)

    # Add REFERENCE
    if (!is.null(reference_group) && num_of_groups > 1) {
      if (length(reference_group) == 1) {
        temp_text <- paste0(
          temp_text, ",\n", tab, "REFERENCE = ",
          group_info$code[group_info$name == reference_group])
      }
    }

    temp_text <- paste0(temp_text, ";\n")

    text <- c(text, temp_text)

    #########################################@###
    ################### QUADS (for CALIB) ###@###
    #########################################@###

    if (!is.null(prior_ability) &&
        !is.null(group_var) &&
        any(grepl("idist[ ]?=[ ]?[12]", tolower(calib_options)))) {
      # Check whether prior_ability list is valid. It should be:
      # (1) a list
      # (2) it's length should be equal to the number of groups.
      # (3) Each element should have two numeric elements named "points" and
      #     "weights".
      # (4) Ideally quadrature points are from lowest to the highest
      # (5) Weights should be positive fractions and summing up to 1.
      if (
        !inherits(prior_ability, "list") ||
        length(prior_ability) != num_of_groups ||
        !all(sapply(prior_ability, inherits, "list")) ||
        # Name of the each list element should be equal to the group names
        !all(names(lapply(prior_ability, names)) %in% group_info$name) ||
        # Each list element should have two named numeric vectors: weights and
        # points
        !all(sapply(prior_ability, function(i) all(c("points", "weights") %in%
                                                  names(i)))) ||
        # Both weights and points should be numeric vectors
        !all(sapply(prior_ability, function(i) all(sapply(i, is.numeric) &
                                                   !sapply(i, is.matrix))))
        )
        stop(paste0(
          "Invalid 'prior_ability' argument. 'prior_ability' should satisfy ",
          "the following conditions: \n",
          "(1) it should be a list with ", num_of_groups, " elements;\n",
          "(2) Each element should be a list object; \n",
          "(3) Name of the each list element should be equal to the ",
          "group names: ", paste0(group_info$name, collapse = ", "), ";\n",
          "(4) Each list element should have two named numeric vectors: ",
          "'list(points = ...,  weights = ...)'."

          ))

      for (g in seq_len(num_of_groups)) {
        tab <- paste0(rep(" ", nchar("QUAD1") + 2), collapse = "")
        temp_g <- prior_ability[[group_info$name[g]]]
        temp_g$points <- gsub("e", "E", format(
          temp_g$points / sum(temp_g$weights), scientific = TRUE))
        temp_g$weights <- gsub("e", "E", format(
          temp_g$weights / sum(temp_g$weights), scientific = TRUE))
        # temp_text <- wrap_text(paste0(
        #   temp_text, ">QUAD", g,
        #   " POINTS = (", paste0(temp_g$points, collapse = " "), "),\n", tab,
        #   "WEIGHTS = (", paste0(temp_g$weights, collapse = " "), ")"),
        #   tab = tab)
        temp_text <- wrap_text(paste0(
          ">QUAD", g,
          " POINTS = (", paste0(temp_g$points, collapse = " "), "),"),
          tab = tab, skip_tab = 1)
        text <- c(text, temp_text)
        temp_text <- wrap_text(paste0(
          "WEIGHTS = (", paste0(temp_g$weights, collapse = " "), ")"),
          tab = tab, skip_tab = 0)
        temp_text <- paste0("\n", temp_text, ";\n")
        # cat(temp_text)
        text <- c(text, temp_text)
      }
    }

    #########################################@###
    ################### PRIORS ##############@###
    #########################################@###
    if (!is.null(prior_ip)) {
      if (!is.list(prior_ip) || is.null(names(prior_ip)) ||
         !any(toupper(names(prior_ip)) %in% prior_ip_names)) {
        stop("Invalid 'prior_ip'. Please provide a valid named list object. ",
             "See the examples at '?est_bilog'.", call. = FALSE)
      }

      tab <- paste0(rep(" ", nchar("PRIORS") + 2), collapse = "")
      names(prior_ip) <- toupper(names(prior_ip))
      # prior_ip <- c(prior_ip, B = 12, SMU = 1)
      # prior_ip <- list(aLPHA = 1, BETA = 2, B = 12, SMU = 1)
      # prior_ip <- list(aLPHA = 1)
      prior_ip <- prior_ip[names(prior_ip) %in% prior_ip_names]
      temp_text <- paste0(">PRIORS ")
      for (i in names(prior_ip)) {
        if (is_single_value(prior_ip[[i]], class = c("numeric", "integer"))) {
          temp <- paste0(i, " = (", prior_ip[[i]], "(0)",
                         data_output$num_of_items, ")")
          if (i == names(prior_ip)[1]) {
            temp_text <- paste0(temp_text, temp, ",\n")
          } else {
            temp_text <- paste0(temp_text, tab, temp, ",\n")
          }
        } else {
          stop("Invalid 'prior_ip'. All elements of the list should be a ",
               "single numeric value. ",
               "See the examples at '?est_bilog'.", call. = FALSE)
        }
      }
      text <- c(text, gsub(",\\n$", ";\n", temp_text))
    }

    #########################################@###
    ################### SCORE ###############@###
    #########################################@###
    if (!is.null(scoring_options) && length(scoring_options) > 0) {
      temp_text <- paste0(">SCORE ")
      tab <- paste0(rep(" ", nchar("SCORE") + 2), collapse = "")
      for (i in 1:length(scoring_options)) {
        temp_text <- paste0(temp_text, ifelse(i == 1, "", paste0(",\n", tab)),
                            scoring_options[i])
      }
      temp_text <- paste0(temp_text, ";\n")
      text <- c(text, temp_text)
    }

    ########################################################################@###
    ################### RUN CALIBRATION ####################################@###
    ########################################################################@###
    result$syntax <- paste0(text, collapse = "\n")
    command <- paste0(
      "cd ", normalizePath(target_dir), " && \"",
      file.path(bilog_exe_folder, "blm1.exe"), "\" ", analysis_name ,
      " NUM=8900 CHAR=2000"
    )
    if (model != "CTT") {
      command <- paste0(
        command, " && \"",
        file.path(bilog_exe_folder, "blm2.exe"), "\" ", analysis_name ,
        " NUM=8900 CHAR=2000 && \"",
        file.path(bilog_exe_folder, "blm3.exe"), "\" ", analysis_name,
        " NUM=8900 CHAR=2000"
      )
    }

    if (overwrite || !file.exists(par_file)) {
      if (!all(file.exists(file.path(bilog_exe_folder,
                                     paste0("blm", 1:3, ".exe"))))) {
        stop(paste0("The required BILOG-MG executable files does not exist at ",
                    "\"", bilog_exe_folder, "\"."),
             call. = FALSE)
      }
      writeLines(text = text, con = target_path, sep = "")
      system("cmd.exe", input = command, wait = TRUE,
             show.output.on.console = show_output_on_console)
      if (show_output_on_console) cat("\n")
    }
  } else if (file.exists(target_path)) {
    group_info <- bilog_create_group_info(x = x, group_var = group_var)
    num_of_groups <- group_info$num_of_groups
    group_info <- group_info$group_info
    result$syntax <- paste0(readLines(con = target_path, skipNul = TRUE),
                            collapse = "\n")
  }

  # Determine whether the model converged or not:
  result$converged <- check_bilog_convergence(par_file = par_file,
                                              ph2_file = ph2_file,
                                              ph3_file = ph3_file)
  if (file.exists(ph2_file)) {
    ph2_content <- readLines(con = ph2_file)
    temp_pattern <- paste0(
      "^ CYCLE([[:blank:]]*)([[:digit:]]*);([[:blank:]]*)",
      "LARGEST CHANGE=([[:blank:]]+)([0-9]*?\\.?[0-9]*?)(.*)$")
    temp <- utils::tail(
      ph2_content[grepl(pattern = temp_pattern, ph2_content)], 1)
    result$cycle <- as.integer(gsub("^ CYCLE (.*);(.*) LARGEST CHANGE= (.*)$",
                                    replacement = "\\1", temp))
    result$largest_change <- as.numeric(gsub(temp_pattern,
                                             replacement = "\\6", temp))

    # If the result converged get the -2 Log Likelihood
    if (result$converged) {
      result$neg_2_log_likelihood <- as.numeric(
        gsub("^ -2 LOG LIKELIHOOD = ", "",
             utils::tail(ph2_content[grepl(pattern = "^ -2 LOG LIKELIHOOD = ",
                                           ph2_content)],1)))

      # Get posterior quadrature points and weights:
      result$posterior_dist <- bilog_read_posterior_dist(
        ph2_file = ph2_file, group_info = group_info)
    } else result$neg_2_log_likelihood <- NULL
  }
  # Read item parameters
  if (model == "CTT") {
    result <- result[-which(names(result) %in%
                              c("ip", "converged", "failed_items", "score"))]
  } else {
    par_results <- bilog_read_pars(
      par_file = par_file,
      ph1_file = ph1_file,
      items = items,
      model = ifelse("COMMON" %in% calib_options, "3PL", model),
      D = D)

    result$ip <- par_results$ip
    result$failed_items <- par_results$failed_items
  }

  result$ctt <- bilog_read_ctt(ph1_file)

  # Check if there is valid group means
  if (model != "CTT" && file.exists(ph3_file) && result$converged) {
    if (num_of_groups > 1) {
      result$group_info <- bilog_read_group_means(
        ph3_file, group_info = group_info)
    # In case reading directly from the Bilog output and data is not available
    } else if (!is.null(result$ctt$group)) {
      result$group_info <- bilog_read_group_means(
        ph3_file, group_info = NULL)
    }
  }

  if (model != "CTT" && !is.null(scoring_options) &&
      (length(scoring_options) > 0) && result$converged) {
    result$score <- bilog_read_scores(score_file, x = x,
                                      examinee_id_var = examinee_id_var,
                                      group_var = group_var)
  }

  attr(result, "class") <- "bilog_output"
  saveRDS(object = result, file = result_fn)
  return(result)
}



###############################################################################@
############################# print.bilog_output ###############################
###############################################################################@
#' Prints \code{bilog_output} objects
#'
#' @param x A \code{bilog_output} object.
#' @param ... further arguments passed to or from other methods.
#'
#' @keywords internal
#'
#' @export
#'
#' @importFrom utils head str
#'
#' @author Emre Gonulates
#'
#' @method print bilog_output
#'
print.bilog_output <- function(x, ...) {
  y <- x
  cat(paste0("Use `cat(..$syntax)` to view the syntax.\n"))
  # Print Input
  cat(paste0("\n$input\n"))
  cat(str(x$input, give.attr = FALSE, no.list = TRUE, give.head = FALSE,
          comp.str = "$"))

  # Other arguments
  class(y) <- NULL
  y$ip <- NULL
  y$group_info <- NULL
  y$score <- NULL
  y$input <- NULL
  y$ctt <- NULL
  y$syntax <- NULL
  y$converged <- NULL
  cat(paste0("\nAdditional output:\n"))
  cat(str(y, give.attr = FALSE, no.list = TRUE, give.head = FALSE,
          comp.str = "$"))

  # CTT
  if (is.data.frame(x$ctt)) {
    cat(paste0("\n$ctt\n"))
    print(head(x$ctt, 5))
    cat(paste0("# ... \n"))
  } else {
    cat(paste0("\n$ctt\n"))
    cat(str(x$ctt, no.list = TRUE, give.head = FALSE, give.attr = FALSE,
            max.level = 1, comp.str = "$"))
  }

  if (x$converged) {
    # Score
    if (!is.null(x$score)) {
      cat(paste0("\n$score\n"))
      print(head(x$score, 5))
      cat(paste0("# ... with ", nrow(x$score) - 5, " more examinees\n"))
    }

    # group_info
    if (!is.null(x$group_info)) {
      cat(paste0("\n$group_info\n"))
      print(x$group_info)
    }

    # IP
    cat(paste0("\n$ip\n"))
    print(x$ip, n = min(x$ip$n$items, 5))
  }

  cat(paste0("\nConverged: ", x$converged, "\n"))
}
