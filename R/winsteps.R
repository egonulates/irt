

############################################################################@###
############################# ws_create_anchor_file ########################@###
############################################################################@###

#' Prepare anchor file for Winsteps calibration
#'
#' @param anchor_info A matrix or data frame that includes the sequence number
#'   and difficulty values of the anchor items. The anchor matrix should
#'   contain at least two columns: (1) either \code{seq} which indicates the
#'   column numbers of the anchor items in the response matrix, i.e. \code{seq}
#'   argument or \code{item_id} column which includes the IDs of anchor items
#'   and (2) \code{b} the item difficulty values.
#'   The default value is \code{NULL}, i.e. no anchor items.
#' @param target_dir The directory where analysis results will be saved.
#'   The default value is \code{getwd()},
#' @param analysis_name A string that will be used for the file names of the
#'   data/control/output files and as the title of the analysis.
#'   The default is \code{"winsteps_analysis"}.
#' @param item_ids A vector of item IDs of the response data. The order of the
#'   item IDs should match the order of columns of the response data. This
#'   vector of item IDs should include anchor items as well. If there is an
#'   'item_id' column in the \code{anchor_info} argument this argument must be
#'   provided and this column is used to locate the column numbers of the anchor
#'   items.
#'
#' @return Function returns either \code{NULL} if there is no anchor items
#'   (i.e. \code{anchor_info} is \code{NULL}) or it returns the file name of
#'   the anchor item file.
#'
#' @noRd
#'
ws_create_anchor_file <- function(anchor_info, target_dir, analysis_name,
                                  item_ids = NULL) {
  anchor_file <- NULL
  if (!is.null(anchor_info)) {
    # Make sure anchor_info is either matrix or data.frame
    if (!inherits(anchor_info, c("matrix", "data.frame"))) {
      stop("Invalid 'anchor_file'. 'anchor_file' should be a data.frame or ",
           "matrix.")
    }
    # Make sure all the required columns are included in the anchor_info
    if (!("b" %in% colnames(anchor_info)) ||
        !any(c("seq", "item_id") %in% colnames(anchor_info))) {
      stop("Invalid 'anchor_file'. 'anchor_file' should  contain at least ",
           "two columns: (1) either 'seq' which indicates the column ",
           "numbers of the anchor items in the response matrix or 'item_id' ",
           "column which includes the IDs of anchor items and (2) 'b' the ",
           "item difficulty values.")
    }

    # If 'seq' is not in the anchor_info, then 'item_ids' argument must be
    # provided
    if (!"seq" %in% colnames(anchor_info) && is.null(item_ids)) {
      stop("Invalid 'item_ids'. When anchor_info do not include 'seq' column ",
           "'item_ids' should be provided.")
    }


    if ("item_id" %in% colnames(anchor_info) && !"seq" %in% colnames(anchor_info)) {
      anchor_info[["seq"]] <- pmatch(anchor_info$item_id, item_ids)
    }


    anchor_file <- paste0(analysis_name, "_anchor.txt")
    text <- paste0(anchor_info$seq, " ", anchor_info$b)
    iafile_name <- file.path(target_dir, anchor_file)
    writeLines(text, con = iafile_name)
  }
  return(anchor_file)
}

############################################################################@###
############################# ws_extract_data ##############################@###
############################################################################@###

#' Extract response/ID data from dataset
#'
#' @description This function extracts following pieces of information from the
#'   given data set:
#'
#'   In the future more fields might be extracted (for example group info).
#'
#' @return A list with the following elements:
#'   \code{$resp} a data frame or matrix of item response data
#'   \code{$examinee_id} a vector of examinee_id's
#'   \code{$misc} a data frame or matrix of additional data such as demographics
#'
#' @noRd
#'
#' @examples
#' x <- sim_resp(ip = generate_ip(n = 8), theta = rnorm(10))
#' x <- as.data.frame(x)
#' x <- cbind(ex_id = rownames(x), x)
#' ws_extract_data(x, examinee_id_var = "ex_id")
#'
#'
#' ws_extract_data(x, examinee_id_var = "ex_id", items = c(2, 5, 6, 9))
#' ws_extract_data(x, examinee_id_var = "ex_id",
#'                 items = c("Item_1", "Item_9", "Item_5"))
ws_extract_data <- function(x, items = NULL, examinee_id_var = NULL,
                            additional_vars = NULL) {
  output <- list()
  remaining_data <- x
  # Extract Examinee IDs
  examinee_ids <- rownames(remaining_data)
  if (!is.null(examinee_id_var) && length(examinee_id_var) == 1) {
    if (is_single_value(examinee_id_var, class = "numeric")) {
      temp <- examinee_id_var
    } else {
      temp <- which(colnames(remaining_data) == examinee_id_var)
    }
    examinee_ids <- remaining_data[[temp]]
    remaining_data <- remaining_data[, -temp, drop = FALSE]
  }
  # Set default examinee IDs
  if (is.null(examinee_ids)) {
    examinee_ids <- paste0("S", 1:nrow(remaining_data))
  }
  output$examinee_id <- examinee_ids

  # Remove additional variables
  misc <- NULL
  if (!is.null(additional_vars)) {
      if (is_atomic_vector(additional_vars, class = "numeric")) {
        temp <- additional_vars
      } else {
        temp <- which(colnames(remaining_data) %in% additional_vars)
      }
    misc <- remaining_data[, temp, drop = TRUE]
    remaining_data <- remaining_data[, -temp, drop = FALSE]
  }
  output$misc <- misc

  # Initially all columns are assumed to be response data
  resp_data <- remaining_data
  if (is_atomic_vector(items, "numeric")) {
    resp_data <- x[, items, drop = FALSE]
  } else if (is_atomic_vector(items, "character")) {
    if (!all(items %in% colnames(x))) {
      stop("Some items cannot be found in the data:\n\"",
           paste0(items[!items %in% colnames(x)], collapse = "\", \""), "\"")
    }
    resp_data <- x[, match(items, colnames(x)), drop = FALSE]
  }
  output$resp <- resp_data

  return(output)
}



############################################################################@###
############################# ws_create_data_file ##########################@###
############################################################################@###


#' Create Winsteps Data File
#'
#' @noRd
#'
ws_create_data_file <- function(x,
                                target_dir = getwd(),
                                data_fn = "winsteps_analysis_data.txt",
                                items = NULL,
                                examinee_id_var = NULL,
                                additional_vars = NULL,
                                verbose = TRUE) {
  if (verbose) {
    cat(paste0("  Creating data file.   (", format(Sys.time(), format = "%X"),
               ")\n"))
  }

  output <- list()

  # From Winsteps manual: "All codes NOT in CODES= are missing value codes. "
  # https://www.winsteps.com/winman/misscore.htm
  # In the program, 'CODES' argument in Winsteps syntax file will only include
  # valid responses, it will not 'missing_code' value.
  missing_code <- "." # String code for missing responses

  if (verbose) {
    cat(paste0("    Extracting data.             (", format(Sys.time(), format = "%X"),
               ")\n"))
  }
  ws_data <- ws_extract_data(x = x, items = items,
                             examinee_id_var = examinee_id_var,
                             additional_vars = additional_vars)
  resp_data <- as.data.frame(ws_data$resp)
  output$num_of_items <- ncol(resp_data)
  output$data_file_name <- data_fn
  # Starting column of item responses
  output$item_start_col <- 1

  # All possible response options
  if (verbose) {
    cat(paste0("    Getting possible options.    (", format(Sys.time(), format = "%X"),
               ")\n"))
  }
  resp_codes <- c()
  for (i in 1:ncol(resp_data)) {
    resp_codes <- c(resp_codes, names(table(resp_data[, i], useNA = "no")))
  }
  resp_codes <- sort(unique(resp_codes))
  # resp_codes <- sort(unique(unlist(lapply(
  #   resp_data, function(y) names(table(y, useNA = "no"))))))

  output$resp_codes <- resp_codes

  output$polytomous <- length(resp_codes) > 2
  # Maximum with of a response option
  max_resp_width <- max(nchar(resp_codes))
  output$max_resp_width <- max_resp_width
  if (is.null(colnames(resp_data))) {
    colnames(resp_data) <- paste0("i", 1:ncol(resp_data))
  }
  output$item_ids <- colnames(resp_data)

  # Maximum Number of characters in examinee ID. If there is no examinee ID
  # variable then use 0 as length. (later there is a test that uses this)
  nchar_examinee_id <- ifelse(is.null(ws_data$examinee_id), 0,
                              max(nchar(ws_data$examinee_id)))

  # Maximum lengths of each column of additional variables
  nchar_misc <- NULL
  if (!is.null(ws_data$misc)) {
    nchar_misc <- sapply(ws_data$misc, function(x) max(nchar(x)))
  }

  # Add response data to the data text
  resp_data[is.na(resp_data)] <- missing_code
  resp_data <- apply(resp_data, 2, function(y)
    sprintf(paste0("%-", max_resp_width, "s"), y))
  data_text <- apply(resp_data, 1, paste0, collapse = "")

  # Add examinee IDs and additional variables to the data text
  # examinee IDs will be the first column of person variables.
  output$examinee_id_width <- 0
  output$person_text_len <- 0
  output$person_start_col <- 0

  person_text <- rep("", length(data_text))
  person_vars <- c()
  if (nchar_examinee_id != 0) {
    if (verbose) {
      cat(paste0("    Adding Examinee IDs.         (", format(Sys.time(), format = "%X"),
                 ")\n"))
    }

    examinee_id_data <- ws_data$examinee_id
    examinee_id_data[is.na(examinee_id_data)] <- missing_code
    examinee_id_data <- sprintf(paste0("%-", nchar_examinee_id, "s"),
                                examinee_id_data)

    output$examinee_id_width <- nchar_examinee_id
    person_vars <- c(person_vars, setNames(nchar_examinee_id, "examinee_id"))
    person_text <- paste0(person_text, examinee_id_data)
  }

  # Add additional variables
  if (any(nchar_misc != 0)) {
    if (verbose) {
      cat(paste0("    Adding additional variables. (", format(Sys.time(), format = "%X"),
                 ")\n"))
    }
    misc_data <- rep("", length(data_text))
    for (i in 1:length(nchar_misc)) {
      misc_data <- paste0(
        misc_data,
        sprintf(paste0(" ", "%-", nchar_misc[i], "s"), ws_data$misc[, i]))
    }
    person_text <- paste0(person_text, misc_data)
    if (is.null(names(nchar_misc))) {
      names(nchar_misc) <- paste0("x", 1:length(nchar_misc))
    }
    person_vars <- c(person_vars, nchar_misc)


  }
  output$person_vars <- person_vars
  output$person_text_len <- nchar(person_text[1])
  output$person_start_col <- nchar(data_text[1]) + 3 + 1 # three is for " | "
  data_text <- paste0(data_text, " | ", person_text)
  # Write data to file
  # For mainly testing reasons, 'data_fn' can be set to NULL. If so, instead of
  # writing data to a text file, it will be added to 'output$data_text'.
  if (is.null(data_fn)) {
    output$data_text <- data_text
  } else {
    if (verbose) {
      cat(paste0("    Writing data file.           (", format(Sys.time(), format = "%X"),
                 ")\n"))
    }
    writeLines(data_text, con = file.path(target_dir, data_fn))
  }

  if (verbose) {
    cat(paste0("  Data file is created.    (", format(Sys.time(), format = "%X"),
               ")\n"))
  }

  return(output)
}

############################################################################@###
############################# ws_create_control_file #######################@###
############################################################################@###


#' Create Winsteps Control File
#'
#' @noRd
#'
ws_create_control_file <- function(
    data_fn,
    item_fn,
    isfile_fn = NULL,
    sfile_fn = NULL,
    person_fn = NULL,
    anchor_fn = NULL,
    target_dir = getwd(),
    control_fn = "winsteps_analysis_control_file.txt",
    title = "Winsteps Analysis",
    item_ids = NULL,
    polytomous = FALSE,
    custom_args = c(
      "TOTALSCORE=YES ; Include extreme responses in reported scores",
      "UDECIMALS=4 ; Number of decimal places reported",
      "PTBISERIAL=YES ; Raw score point-biserial excluding extremes",
      "PVALUE=YES ; report proportion-correct-values"),
    item_start_col = 1,
    num_of_items = 0,
    person_start_col = 0,
    person_text_len = 0,
    person_vars = NULL,
    examinee_id_width = 0,
    max_resp_width = 0,
    resp_codes = c("0", "1")
    ) {


  if (!is_atomic_vector(custom_args, class = "character")) {
    stop("'custom_args' argument in 'est_winsteps()' function should be a ",
         "vector of strings. ", call. = FALSE)
  }

  # If data is polytomous, automatically add "MODELS=R" and "ISGROUPS=0" unless
  # they are already defined in 'custom_args'
  if (polytomous) {
    # If "GROUPS" is not already exists, add "MODELS = R"
    if (!any(grepl("^\\s*models\\s*=", custom_args, ignore.case = TRUE))) {
      custom_args <- c(
        custom_args,
        paste0("MODELS=R; the rating-scale family of models including ",
               "Masters Partial-Credit Model"))
    }
    if (!(any(grepl("^\\s*groups\\s*=", custom_args, ignore.case = TRUE)) |
          any(grepl("^\\s*isgroups\\s*=", custom_args, ignore.case = TRUE))
          )) {
      custom_args <- c(custom_args,
                       "ISGROUPS=0; Masters' Partial Credit Model")
    }
  }

  syntax_text <- c(
    "&INST",
    paste0("TITLE= \"", title, "\""),
    paste0("; Control file created or last modified by 'irt' package at: ",
           Sys.time()),
    paste0("DATA = \"", data_fn, "\" ; Data file name"),
    paste0("ITEM1 = ", item_start_col, " ; Starting column of item responses"),
    paste0("NI = ", num_of_items, " ; Number of items"),

    paste0("CSV = Yes ; facilitate importing the IFILE=, ISFILE=, PFILE=, ",
           "and SFILE= files into a CSV file"),

    paste0("NAME1 = ", person_start_col,
           " ; Starting column for person label in data record"),
    paste0("NAMLEN = ", person_text_len, " ; Length of person label"),
    # paste0("UDECIMALS = ", 4, " ; Number of decimal places reported"),
    paste0("XWIDE = ", max_resp_width,
           " ; Matches the widest data value observed"),
    paste0("CODES = \"", paste0(resp_codes, collapse = " "),
           "\" ; possible values of response data"),
    custom_args,
    # paste0("TOTALSCORE = Yes ; Include extreme responses in reported scores"),
    paste0("; Person Label variables: columns in label: columns in line")
    )

  # Output files
  syntax_text <- c(
    syntax_text,
    paste0("IFILE = \"", item_fn, "\" ; Item parameter estimates file name")
    )

  # Item Structure File for Polytomous items
  if (!is.null(isfile_fn)) {
    syntax_text <- c(
      syntax_text,
      paste0("ISFILE = \"", isfile_fn,
             "\" ; Item Structure file name for polytomous items")
      )
  }

  # Item Structure File for Polytomous items
  if (!is.null(sfile_fn)) {
    syntax_text <- c(
      syntax_text,
      paste0("SFILE = \"", sfile_fn,
             "\" ; Category structure-threshold output file")
      )
  }


  # Person parameters file name
  if (!is.null(person_fn)) {
    syntax_text <- c(
      syntax_text,
      paste0("PFILE = \"", person_fn, "\" ; Person parameter estimates file name")
      )
  }
  # Anchor item file name
  if (!is.null(anchor_fn)) {
    syntax_text <- c(
      syntax_text,
      paste0("IAFILE = \"", anchor_fn, "\" ; Anchor items file name")
      )
  }

  ## Add person variables ##
  syntax_text <- c(
    syntax_text,
    "; Person Label variables: columns in label: columns in line"
    )

  counter <- 1
  for (i in seq_along(person_vars)) {
    syntax_text <- c(
      syntax_text,
      paste0("@", names(person_vars[i]), " = ", counter, "E",
             counter + person_vars[i] - 1, " ;")
      )
    counter <- unname(counter + person_vars[i] + 1)
  }

  ## Add item column names ##
  syntax_text <- c(
    syntax_text,
    "&END ; Item labels follow: columns in label",
    paste0(item_ids, " ;"),
    "END NAMES"
    )

  ## Write Control File ##

  # For mainly testing reasons, 'data_fn' can be set to NULL. If so, instead of
  # writing data to a text file, it will be added to 'output$data_text'.
  if (is.null(data_fn)) {
    return(syntax_text)
  } else {
    writeLines(syntax_text, con = file.path(target_dir, control_fn))
  }
  invisible()
}

############################################################################@###
############################# ws_create_winsteps_batch #####################@###
############################################################################@###

#' Write Winsteps Batch File
#'
#' @noRd
#'
#'
ws_create_winsteps_batch <- function(target_dir, control_fn, output_fn,
                                     batch_fn, winsteps_exe_folder) {
  # Prepare batch file:
  batch_text <- c(paste0(
    "START /WAIT ", winsteps_exe_folder, "\\WINSTEPS BATCH=YES \"",
    normalizePath(file.path(target_dir, control_fn), mustWork = FALSE),
    "\" \"",
    normalizePath(file.path(target_dir, output_fn), mustWork = FALSE),
    "\" TABLES=0" ),
    "EXIT")

  writeLines(batch_text, con = file.path(target_dir, batch_fn))

  invisible()
}


############################################################################@###
############################# ws_read_data_file ############################@###
############################################################################@###

#' Read Fixed-Width Winsteps Data Files
#'
#' @description This function is used to read the fixed with data files for item
#'   and person parameters of Winsteps.
#'
#' @noRd

ws_read_fw_data <- function(path) {
  text <- readLines(path)[-1]

  header_df <- data.frame(name = character(0), width = numeric(0))
  header <- text[1]
  # this will help to determine the width of the last column
  item_id_buffer <- max(nchar(text)) - nchar(header)
  while (nchar(header) > 0) {
    temp <- sub("(^(\\s)?[^ ]+).*", "\\1", header)
    sub("(^(\\s)?[^ ]+).*", "\\1", header)
    header_df <- rbind(header_df, data.frame(name = temp, width = nchar(temp)))
    header <- gsub(temp, "", header, fixed = TRUE)
  }

  header <- gsub("^;", "", text[1])
  header <- paste0(" ", strsplit(header, "\\s")[[1]])

  while (any(grepl("^\\s*$", header))) {
    for (i in 1:(length(header) - 1)) {
      if (grepl("^\\s*$", header[i])) {
        header[i + 1] <- paste0(header[i], header[i+1])
        header <- header[-i]
        break
      }
    }
  }

  header_df <- data.frame(name = gsub("\\s", "", header), width = nchar(header))
  header_df$width[nrow(header_df)] <- header_df$width[nrow(header_df)] +
    item_id_buffer
  output <- utils::read.fwf(file = path, widths = header_df$width,
                            header = FALSE, skip = 2,
                            col.names = header_df$name)
  return(output)
}

############################################################################@###
############################# ws_read_item_file_fwf ########################@###
############################################################################@###

#' Read Raw Winsteps Item File - Fixed Width File
#'
#' @noRd
#'
ws_read_item_file_fwf <- function(target_dir, item_fn) {
  path <- file.path(target_dir, item_fn)
  if (!file.exists(path)) {
    stop("Invalid path. Item file cannot be found inthe following path:\n",
         "  \"", path, "\"")
  }
  output <- ws_read_fw_data(path)
  # Remove trailing spaces from the item ID
  output[["NAME"]] <- gsub("^[ ]?", "", output[["NAME"]])

  # Column names can be found here: https://www.winsteps.com/winman/ifile.htm
  return(output)
}

############################################################################@###
############################# ws_read_item_file_csv ########################@###
############################################################################@###

#' Read Raw Winsteps Item File - CSV File
#'
#' @noRd
#'
ws_read_item_file_csv <- function(target_dir, item_fn) {
  path <- file.path(target_dir, item_fn)
  if (!file.exists(path)) {
    stop("Invalid path. Item file cannot be found inthe following path:\n",
         "  \"", path, "\"")
  }
  output <- utils::read.csv(path, skip = 1)

  # Column names can be found here: https://www.winsteps.com/winman/ifile.htm
  return(output)
}


############################################################################@###
############################# ws_read_sfile_csv ###########################@###
############################################################################@###

#' Read Polytomous Item Threshold Values
#'
#' This file can be used as an input file of the anchored polytomous items.
#'
#' @noRd
#'
ws_read_sfile_csv <- function(target_dir, sfile_fn = NULL) {
  path_sfile_fn <- file.path(target_dir, sfile_fn)
  if (is.null(sfile_fn) ||
      length(path_sfile_fn) == 0 ||
      !file.exists(path_sfile_fn)
      ) {
    return(NULL)
  }
  output_sfile <- utils::read.csv(file = path_sfile_fn, skip = 2)
  colnames(output_sfile) <- c("ENTRY", "category", "threshold")
  return(output_sfile)
}


############################################################################@###
############################# ws_read_isfile_csv ###########################@###
############################################################################@###

#' Read Polytomous Items from Structure Files
#'
#' @noRd
#'
ws_read_isfile_csv <- function(target_dir, isfile_fn = NULL) {
  path_isfile_fn <- file.path(target_dir, isfile_fn)
  # path_sfile_fn <- file.path(target_dir, sfile_fn)
  if (is.null(isfile_fn) ||
      # is.null(sfile_fn) ||
      length(path_isfile_fn) == 0 ||
      # length(path_sfile_fn) == 0 ||
      # !file.exists(path_sfile_fn) ||
      !file.exists(path_isfile_fn)
      ) {
    return(NULL)
  }
  output_isfile <- utils::read.csv(file = path_isfile_fn, skip = 1)

  # Sometimes there might be items with errors
  if (any(grepl(";", output_isfile$ENTRY))) {
    warning(paste0("There might be a problem with the parameter ",
                   "estimation of the following item(s):\n",
                   paste0(output_isfile$ENTRY[grepl(";", output_isfile$ENTRY)],
                          collapse = "\n")))
    output_isfile$ENTRY <- as.integer(gsub(";|\\s", "", output_isfile$ENTRY))
  }

  return(output_isfile)
}


############################################################################@###
############################# ws_read_isfile_fwf ###########################@###
############################################################################@###

#' Read Raw Winsteps Item Structure File - Fixed-Width Format and create
#' polytomous parameters
#'
#' Currently not used
#'
#' @noRd
#'
ws_read_isfile_fwf <- function(target_dir, isfile_fn) {
  path <- file.path(target_dir, isfile_fn)
  if (is.null(isfile_fn) || length(path) == 0 || !file.exists(path)) {
    return(NULL)
    # stop("Invalid path. Item structure file (ISFILE) cannot be found in the ",
    #      "following path:\n", "  \"", path, "\"")
  }
  output <- ws_read_fw_data(path)
  return(output)
}



############################################################################@###
############################# ws_read_poly_pars_csv ########################@###
############################################################################@###

#' Read Polytomous Items from Structure Files
#'
#' @noRd
#'
ws_read_poly_pars_csv <- function(target_dir, item_pars, isfile_fn = NULL,
                                  polytomous_model = "PCM") {
  output_isfile <- ws_read_isfile_csv(target_dir = target_dir,
                                      isfile_fn = isfile_fn)
  if (is.null(output_isfile)) return(output_isfile)

  n_categories <- sum(grepl("^MEASURE", colnames(output_isfile)))

  output <- output_isfile[, grepl("^ENTRY$|^MEASURE|^MAX$",
                                  colnames(output_isfile))]

  if (polytomous_model == "PCM") {
    colnames(output) <- c("ENTRY", "MAX", paste0("b", seq(1, n_categories, 1)))

    if (n_categories > 1) {
      for (i in seq(n_categories, 2)) {
        col_no <- which(colnames(output) == paste0("b", i))
        output[which(output[, "MAX"] < i), col_no] <- NA
      }
    }
    output$model <- NA
    if (any(output$MAX <= 1)) {
      output$b <- output$b1
      output[output$MAX > 1, "b"] <- NA
      output[output$MAX <= 1, "b1"] <- NA
      output[output$MAX <= 1, "model"] <- "Rasch"
    }

    output[output$MAX > 1, "model"] <- "PCM"
  } else if (polytomous_model == "GPCM2") {
    colnames(output) <- c("ENTRY", "MAX", paste0("d", seq(1, n_categories, 1)))

    output <- merge(output, item_pars[, c("ENTRY", "MEASURE")], by = "ENTRY")
    output[["D"]] <- 1
    output[["a"]] <- 1
    colnames(output)[colnames(output) == "MEASURE"] <- "b"

    if (n_categories > 1) {
      for (i in seq(n_categories, 2)) {
        col_no <- which(colnames(output) == paste0("d", i))
        output[which(output[, "MAX"] < i), col_no] <- NA
      }

      for (i in 1:n_categories) {
        output[[paste0("d", i)]] <- output[[paste0("d", i)]] - output[["b"]]
      }
    }

    output$model <- NA
    if (any(output$MAX <= 1)) {
      # output$b <- output$b1
      # output[output$MAX > 1, "b"] <- NA
      output[output$MAX <= 1, "d1"] <- NA
      output[output$MAX <= 1, "a"] <- NA
      output[output$MAX <= 1, "D"] <- NA
      output[output$MAX <= 1, "model"] <- "Rasch"
    }
    output[output$MAX > 1, "model"] <- "GPCM2"
  } else {
    stop("Invalid 'model' argument. Model Should be either 'PCM' or 'GPCM2'.")
  }



  return(output)
}




############################################################################@###
############################# ws_read_poly_pars_fwf ########################@###
############################################################################@###

#' Read Raw Winsteps Item Structure File - Fixed-Width Format
#'
#' Currently not used
#'
#' @noRd
#'
ws_read_poly_pars_fwf <- function(target_dir, isfile_fn) {
  output <- ws_read_isfile_fwf(target_dir = target_dir, isfile_fn = isfile_fn)
  if (is.null(output)) return(NULL)

  output <- output[, grepl("^ENTRY$|^MEASURE|^MAX$", colnames(output))]

  # Sometimes there might be items with errors
  if (any(grepl(";", output$ENTRY))) {
    warning(paste0("There might be a problem with the parameter ",
                   "estimation of the following item(s):\n",
                   paste0(output$ENTRY[grepl(";", output$ENTRY)],
                          collapse = "\n")))
    output$ENTRY <- as.integer(gsub(";|\\s", "", output$ENTRY))
  }

  n_categories <- sum(grepl("^MEASURE", colnames(output)))
  colnames(output) <- c("ENTRY", "MAX", paste0("b", seq(1, n_categories, 1)))

  if (n_categories > 1) {
    for (i in seq(n_categories, 2)) {
      col_no <- which(colnames(output) == paste0("b", i))
      output[which(output[, "MAX"] < i), col_no] <- NA
    }
  }
  output$model <- NA
  if (any(output$MAX <= 1)) {
    output$b <- output$b1
    output[output$MAX > 1, "b"] <- NA
    output[output$MAX <= 1, "b1"] <- NA
    output[output$MAX <= 1, "model"] <- "Rasch"
  }

  output[output$MAX > 1, "model"] <- "PCM"

  return(output)
}



############################################################################@###
############################# ws_create_itempool ###########################@###
############################################################################@###


#' Create an Itempool object from Winsteps Parameters
#'
#' @description This function creates an \code{Itempool} object from Winsteps
#'   calibration results.
#'
#' @param item_pars The raw contents of the item parameter file.
#' @param poly_pars The raw contents of the item structure file.
#'
#' @noRd
#'
ws_create_itempool <- function(item_pars, poly_pars = NULL) {
  # head(item_pars)
  # column_names <- data.frame(
  #   old_name = c("ENTRY", "MEASURE", "ST", "COUNT", "SCORE", "MODLSE", "ERROR",
  #                "IN.MSQ", "INZSTD", "OUTMSQ", "OUTZST", "DISPL", "PTMA",
  #                "WEIGHT", "OBSMA", "EXPMA", "PMA.E", "RMSR", "WMLE",
  #                "INDF", "OUTDF", "G", "M", "R", "NAME"),
  #   new_name = c("ENTRY", "b", "ST", "COUNT", "SCORE", "MODLSE", "MODLSE",
  #                "IN.MSQ", "INZSTD", "OUTMSQ", "OUTZST", "DISPL", "PTMA",
  #                "WEIGHT", "OBSMA", "EXPMA", "PMA.E", "RMSR", "WMLE",
  #                "INDF", "OUTDF", "G", "M", "R", "item_id")
  #   )

  col_names_recodes <- c(MODLSE = "MODLSE", ERROR = "MODLSE",
                         # MEASURE = "b",
                         NAME = "item_id")
  temp_match <- match(names(col_names_recodes), names(item_pars))
  col_names_recodes <- col_names_recodes[!is.na(temp_match)]
  temp_match <- temp_match[!is.na(temp_match)]
  names(item_pars)[temp_match] <- col_names_recodes
  # Keep 'MEASURE' in case there are polytomous items in the item pool
  item_pars[["b"]] <- item_pars[["MEASURE"]]
  if (is.null(poly_pars)) {
    ip <- itempool(item_pars, model = "Rasch")
  } else {
    ip_df <- merge(x = item_pars[, colnames(item_pars) != "b"],
                   y = poly_pars,
                   by = "ENTRY",
                   all.x = TRUE)
    ip <- itempool(ip_df)
  }
  return(ip)
}


############################################################################@###
############################# ws_read_person_file_fwf ######################@###
############################################################################@###

#' Read Raw Winsteps Person File - Fixed-Width-Format
#'
#' @noRd
#'
ws_read_person_file_fwf <- function(target_dir, person_fn) {
  path <- file.path(target_dir, person_fn)
  if (!file.exists(path)) {
    return(NULL)
  }

  output <- ws_read_fw_data(path)

  # Remove trailing spaces from the item ID
  output[["NAME"]] <- gsub("^[ ]?", "", output[["NAME"]])

  # Column names can be found here: https://www.winsteps.com/winman/ifile.htm
  return(output)
}


############################################################################@###
############################# ws_read_person_file_csv ######################@###
############################################################################@###

#' Read Raw Winsteps Person File - CSV
#'
#' @noRd
#'
ws_read_person_file_csv <- function(target_dir, person_fn) {
  path <- file.path(target_dir, person_fn)
  if (!file.exists(path)) {
    return(NULL)
  }

  output <- utils::read.csv(file = path, skip = 1)

  # Remove trailing spaces from the examinee ID
  output[["NAME"]] <- gsub("^[ ]?", "", output[["NAME"]])

  # Column names can be found here: https://www.winsteps.com/winman/ifile.htm
  return(output)
}




############################################################################@###
############################################################################@###
############################# est_winsteps #################################@###
############################################################################@###
############################################################################@###

#' Estimate Rasch Model using Winsteps
#'
#' @description
#' This function serves as an interface to the Winsteps program, allowing for
#' the convenient execution of basic Winsteps calibrations without the need to
#' write Winsteps syntax manually. Please note that a valid installation of
#' Winsteps is necessary for this function to operate. Keep in mind that it is
#' still in beta mode, so exercise caution when using it.
#'
#'
#' @param x A matrix or data frame that contains both response and person data.
#' @param target_dir The directory where the analysis results will be saved.
#'   The default value is \code{getwd()}.
#' @param analysis_name A string that will be used for naming the files (data,
#'   control, output) and as the title of the analysis. The default is
#'   \code{"winsteps_analysis"}.
#' @param items A vector of strings representing item IDs within \code{x} to be
#'   used as response data, or a numeric vector indicating the columns
#'   containing response data. The default value is \code{NULL}, which uses all
#'   columns in \code{x} except those specified in \code{examinee_id_var} and
#'   \code{additional_vars}.
#' @param examinee_id_var A string representing the column name containing
#'   examinee/subject IDs, or the column number of examinee/subject IDs. The
#'   default value is \code{NULL}, assuming no examinee/subject IDs.
#' @param additional_vars A vector of strings or integers representing the
#'   column names or numbers to be included in the Winsteps data file. The
#'   default value is \code{NULL}, meaning no additional columns will be added
#'   to the Winsteps data file. Note that if \code{items} is \code{NULL}, all
#'   variables in the dataset will be treated as response data.
#' @param custom_args A vector containing string elements to be included in
#'   the Winsteps control file. Each element in the vector will be written on a
#'   separate line. Ensure that the vector's contents adhere to valid Winsteps
#'   syntax. The default value is:
#'   \code{c("TOTALSCORE = Yes ; Include extreme responses in reported scores", "UDECIMALS = 4 ; Number of decimal places reported", "PTBISERIAL=YES ; Raw score point-biserial excluding extremes", "PVALUE = YES ; report proportion-correct-values")}.
#' @param anchor_info A matrix or data frame containing the sequence number and
#'   difficulty values of anchor items. The anchor matrix should have at least
#'   two columns: (1) either \code{seq}, indicating the column numbers of the
#'   anchor items in the response matrix, or \code{item_id} column containing
#'   the IDs of anchor items, and (2) \code{b}, the item difficulty values. The
#'   default value is \code{NULL}, meaning no anchor items are used.
#'
#'   Here are some additional commands to consider adding:
#'   \itemize{
#'     \item \code{"EXTRSCORE= 0.5 ; extreme score correction for extreme measures"}
#'     \item \code{"MPROX=10 ; maximum number of PROX iterations"}
#'     \item \code{"CONVERGE=L ; only logit change is used for convergence -- also specify 'LCONV= ' "}
#'     \item \code{"LCONV=0.001 logit change at convergence -- also specify 'CONVERGE=L'"}
#'   }
#' @param polytomous_model A string value that specifies the IRT model for which
#'   the polytomous items will be written. It accepts two values: 'PCM' for the
#'   Partial Credit Model and 'GPCM2' for the Reparameterized Generalized
#'   Partial Credit Model. The default value is "PCM".
#' @param overwrite A logical value. If \code{TRUE}, existing control/data files
#'   will be overwritten. The default value is \code{TRUE}.
#' @param read_person_pars A logical value indication whether to read the
#'   person parameters created (\code{TRUE}) or not (\code{FALSE}). The default
#'   value is \code{TRUE}.
#' @param winsteps_exe_folder The directory containing the Winsteps executable.
#'   The default value is \code{file.path("C:/Winsteps")}.
#' @param verbose If \code{TRUE}, the program will print intermediate steps.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' \dontrun{
#' true_theta <- rnorm(300)
#' ip <- generate_ip(n = 20, model = "Rasch")
#' resp_set <- generate_resp_set(ip = ip, theta = true_theta, prop_missing = .2)
#' resp_matrix <- as.matrix(resp_set)
#'
#' est_pars <- est_winsteps(x = resp_matrix,
#'                          target_dir = "c:/temp/est_winsteps")
#'
#' # Relationship between true and estimated item difficulty parameters
#' plot(x = ip$b, y = est_pars$ip$b, xlab = "True 'b'", ylab = "Estimated 'b'")
#' cor(x = ip$b, y = est_pars$ip$b)
#'
#' # Relationship between true and estimated theta parameters
#' cor(x = true_theta, y = est_pars$raw_person_pars$MEASURE)
#' plot(x = true_theta, y = est_pars$raw_person_pars$MEASURE,
#'      xlab = "True 'b'", ylab = "Estimated 'b'")
#' }
est_winsteps <- function(
  x,
  target_dir = getwd(),
  analysis_name = "winsteps_analysis",
  items = NULL,
  examinee_id_var = NULL,
  additional_vars = NULL,
  custom_args = c("TOTALSCORE=YES ; Include extreme responses in reported scores",
                  "UDECIMALS=4 ; Number of decimal places reported",
                  "PTBISERIAL=YES ; Raw score point-biserial excluding extremes",
                  "PVALUE=YES ; report proportion-correct-values"),
  anchor_info = NULL,
  polytomous_model = c("PCM", "GPCM2"),
  overwrite = TRUE,
  winsteps_exe_folder = file.path("C:/Winsteps"),
  read_person_pars = TRUE,
  verbose = TRUE
  ) {

  if (!is_single_value(x = target_dir, class = "character")) {
    stop("Invalid 'target_dir' value. ")
  }
  if (!is_single_value(x = analysis_name, class = "character")) {
    stop("Invalid 'analysis_name' value. ")
  }
  polytomous_model <- match.arg(polytomous_model)

  # The location where all the output will be written
  output_rds_fn <- file.path(target_dir, paste0(analysis_name, ".RDS"))

  if (!overwrite && file.exists(output_rds_fn)) {
    output <- readRDS(output_rds_fn)
  } else {
    # Create the directory if it doesn't exist but warn if the upper directory
    # does not exists
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)


    ## Create data file ##
    if (verbose) {
      cat(paste0("Calibration started.    (", format(Sys.time(), format = "%X"),
                 ")\n"))
    }
    dt_pars <- ws_create_data_file(x = x,
                                   target_dir = target_dir,
                                   data_fn = paste0(analysis_name, "_data.txt"),
                                   items = items,
                                   examinee_id_var = examinee_id_var,
                                   additional_vars = additional_vars,
                                   verbose = verbose)

    # Create anchor file if it exists
    anchor_fn <- ws_create_anchor_file(anchor_info = anchor_info,
                                       item_ids = dt_pars$item_ids,
                                       target_dir = target_dir,
                                       analysis_name = analysis_name)

    item_fn <- paste0(analysis_name, "_item_pars.csv")
    if (dt_pars$polytomous) {
      isfile_fn <- paste0(analysis_name, "_item_structure.csv")
      sfile_fn <- paste0(analysis_name, "_category_threshold.csv")
    } else {
      isfile_fn <- NULL
      sfile_fn <- NULL
    }
    person_fn <- paste0(analysis_name, "_person_pars.csv")
    control_fn <- paste0(analysis_name, "_control_file.txt")
    output_fn <- paste0(analysis_name, "_output_file.txt")
    batch_fn <- paste0(analysis_name, "_batch_file.bat")

    ## Create Winsteps control file ##
    ws_create_control_file(
      data_fn = dt_pars$data_file_name,
      item_fn = item_fn,
      isfile_fn = isfile_fn,
      sfile_fn = sfile_fn,
      person_fn = person_fn,
      anchor_fn = anchor_fn,
      target_dir = target_dir,
      control_fn = control_fn,
      title = "Winsteps Analysis",
      item_ids = dt_pars$item_ids,
      custom_args = custom_args,
      polytomous = dt_pars$polytomous,
      item_start_col = dt_pars$item_start_col,
      num_of_items = dt_pars$num_of_items,
      person_start_col = dt_pars$person_start_col,
      person_text_len = dt_pars$person_text_len,
      person_vars = dt_pars$person_vars,
      examinee_id_width = dt_pars$examinee_id_width,
      max_resp_width = dt_pars$max_resp_width,
      resp_codes = dt_pars$resp_codes
      )
    if (verbose) {
      cat(paste0("  Control file created. (", format(Sys.time(), format = "%X"),
                 ")\n"))
    }

    # Create Winsteps Batch File
    ws_create_winsteps_batch(target_dir = target_dir,
                             control_fn = control_fn,
                             output_fn = output_fn,
                             batch_fn = batch_fn,
                             winsteps_exe_folder = winsteps_exe_folder)
    if (verbose) {
      cat(paste0("  Batch file created.   (", format(Sys.time(), format = "%X"),
                 ")\n"))
    }

    # Run Winsteps Batch File
    system("cmd.exe",
           input = paste0("cd ", normalizePath(target_dir), " && \"",
                          batch_fn, "\" "),
           wait = TRUE, show.output.on.console = FALSE)

    if (verbose) {
      cat(paste0("  Reading item parameters. (", format(Sys.time(), format = "%X"),
                 ")\n"))
    }

    # Read item parameters
    item_pars <- ws_read_item_file_csv(target_dir = target_dir, item_fn = item_fn)
    # poly_pars <- ws_read_isfile_fwf(target_dir = target_dir, isfile_fn = isfile_fn)
    poly_pars <- ws_read_poly_pars_csv(target_dir = target_dir,
                                       item_pars = item_pars,
                                       isfile_fn = isfile_fn,
                                       polytomous_model = polytomous_model)
    ip <- ws_create_itempool(item_pars, poly_pars)

    if (read_person_pars) {
      if (verbose) {
        cat(paste0("  Reading person parameters. (", format(Sys.time(), format = "%X"),
                   ")\n"))
      }
      # Read person parameters
      person_pars <- ws_read_person_file_csv(target_dir = target_dir,
                                             person_fn = person_fn)

    } else {
      person_pars <- NULL
    }

    if (verbose) {
      cat(paste0("Calibration completed.  (", format(Sys.time(), format = "%X"),
                 ")\n"))
    }

    output <- list(raw_item_pars = item_pars)

    if (dt_pars$polytomous) {
      output[["raw_isfile"]] <- ws_read_isfile_csv(target_dir,
                                                   isfile_fn = isfile_fn)
      output[["raw_sfile"]] <- ws_read_sfile_csv(target_dir, sfile_fn = sfile_fn)
    }

    if (read_person_pars) {
      output[["raw_person_pars"]] <- person_pars
    }

    output[["ip"]] <- ip
    saveRDS(output, output_rds_fn)
  }

  return(output)

}
