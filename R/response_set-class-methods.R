

###############################################################################@
############################# .create_response_set_from_list ###################
###############################################################################@
.create_response_set_from_list <- function(x, ip = NULL, misc = NULL) {

  # Check 'x' argument
  if (!inherits(x, "list") || !all(sapply(x, is, "Response")))
    stop("Invalid 'x' argument. 'x' should be a list of 'Response' objects.")

  item_ids <- NULL
  testlet_ids <- NULL

  response_list <- x

  # Setup item_ids and testlet_ids
  if (is.null(ip)) {
    testlet_ids <- lapply(lapply(x, as.data.frame), function(i)
      if ("testlet_id" %in% colnames(i)) i[, c("item_id", "testlet_id")] else
        cbind(i[, "item_id", drop  = FALSE], testlet_id = as.character(NA)))
    testlet_ids <- do.call(rbind, testlet_ids)
    testlet_ids <- testlet_ids[!duplicated(testlet_ids), ]
    if (any(duplicated(testlet_ids$item_id))) {
      temp <- testlet_ids[testlet_ids$item_id %in% testlet_ids$item_id[
        duplicated(testlet_ids$item_id)], ]
      stop("Invalid 'item_id' or 'testlet_id's. Each item should be ",
           "associated with a unique testlet.\n",
           paste0(utils::capture.output(print(temp)), collapse = "\n"))
    }
    item_ids <- testlet_ids$item_id
    if (all(is.na(testlet_ids$testlet_id))) testlet_ids <- NULL else
      testlet_ids <- testlet_ids$testlet_id
  } else {
    # If there is a valid item pool, the order of @item_id slot should follow
    # the order of the items in the item pool.
    testlet_ids <- as.data.frame(ip)
    # Discard items in the item pool that are not in the responses
    item_ids <- unique(do.call("c", lapply(x, function(x) x@item_id)))
    if (!all(item_ids %in% ip$item_id))
      stop("Invalid 'item_id'. All of the 'item_id's should be in 'ip'. ",
           "Following items are not in 'ip': \"",
           paste0(item_ids[!item_ids %in% ip$item_id], collapse = ", "), "\"")
    testlet_ids <- testlet_ids[testlet_ids$item_id %in% item_ids, ]
    item_ids <- testlet_ids$item_id
    testlet_ids <- switch(("testlet_id" %in% colnames(testlet_ids)) + 1,
                          NULL, testlet_ids$testlet_id)
  }

  result <- new("Response_set", response_list = name_examinees(response_list),
                item_id = item_ids, testlet_id = testlet_ids, misc = misc)
  if (!is.null(ip)) check_validity_response_set_cpp(resp_set = result, ip = ip)
  return(result)
}


###############################################################################@
############################# .create_response_set_from_wide_format ############
###############################################################################@
.create_response_set_from_wide_format <- function(
  x, ip = NULL, examinee_id_var = NULL, misc = NULL, fill_na_score = NULL) {

  item_ids <- NULL
  testlet_ids <- NULL
  # In case it is 'tibble', make it a data frame
  if (inherits(x, "tbl_df")) x <- as.data.frame(x)

  if (!is.null(examinee_id_var) &&
      (!is(examinee_id_var, "character") || length(examinee_id_var) != 1 ||
       !examinee_id_var %in% colnames(x)) )
    stop(paste0("Invalid \"examinee_id_var\" argument. The value should be one ",
                "of the column names of 'x'."))

  # Get examinee_ids if it is available
  if (!is.null(examinee_id_var)) {
    examinee_ids <- x[, examinee_id_var, drop = TRUE]
    x <- x[, -which(colnames(x) == examinee_id_var)]
  } else if (!is.null(rownames(x)) &&
             !all(rownames(x) == paste(1:nrow(x)))) {
    examinee_ids <- rownames(x)
  } else {# set default examinee ids:
    examinee_ids <- NULL# paste0("S", 1:nrow(x))
  }

  item_ids <- colnames(x)

  if (is.null(item_ids)) {
    # Check whether an itempool available and if yes whether it has the
    # same length as the number of columns of the x
    if (!is.null(ip)) { # Try to get item_ids from the item pool object
      if (ip$n$items == ncol(x)) {
        item_ids <- ip$item_id
      } else {
        stop("Invalid 'ip'. The number of items in the item pool does not",
             "match with the number of columns of the data 'x'.")
      }
    } else {# Give default column names: "Item_1", "Item_2", ...
      item_ids <- paste0("Item_", 1:ncol(x))
    }
    colnames(x) <- item_ids
  }

  if (!is.null(fill_na_score)) {
    # populate columns that are in item pool but not in the response matrix
    if (!is.null(ip)) {
      ip_id_diff <- setdiff(ip$resp_id, item_ids)
      x <- cbind(x, matrix(NA, ncol = length(ip_id_diff), nrow = nrow(x),
                           dimnames = list(rownames(x), ip_id_diff)))
      item_ids <- colnames(x)
    }
    x[is.na(x)] <- fill_na_score
  }
  # Check whether there are testlets in the item pool and responses. If there
  # are, automatically add testlet info to the responses
  if (!is.null(ip) && any(sapply(ip@item_list, class) == "Testlet")) {
    testlet_ids <- matrix(
      merge(data.frame(item_id = rep(item_ids, each = nrow(x))),
            data.frame(ip)[, c("item_id", "testlet_id")], by.x = "item_id",
            sort = FALSE)[, "testlet_id"], ncol = ncol(x))
  }

  # Check whether the elements of x is numeric or character. If they are all
  # character, then assume they are raw responses, if they are all numeric
  # then assume they are all score values, otherwise raise an error.

  if (all(apply(x, 2, is.numeric))) {
    score_matrix <- TRUE
  } else if (all(apply(x, 2, is.character))) {
    score_matrix <- FALSE # i.e. raw response matrix or data.frame
  } else {
    stop("Invalid 'x'. All values in 'x' should be numeric for score data ",
         "and character for raw response data.")
  }

  response_list <- vector("list", nrow(x))
  for (i in 1:nrow(x)) {
    response_list[[i]] <- tryCatch({
      response(
        score = switch(score_matrix + 1, NULL,
                       unname(unlist(x[i, !is.na(x[i, , drop = TRUE])]))),
        raw_response = switch(
          score_matrix + 1,
          unname(unlist(x[i, !is.na(x[i, , drop = TRUE])])), NULL),
        item_id = item_ids[!is.na(x[i, ])],
        testlet_id = switch((is.null(testlet_ids) ||
                               all(is.na(testlet_ids[i, ]))) + 1,
                            testlet_ids[i, !is.na(x[i, , drop = TRUE])], NULL),
        examinee_id = examinee_ids[i]
        )
      }, error = function(e) {
        if (grepl("Either 'score' or 'raw_response' should be provided.",
                  e$message)) {
          stop(paste0("All scores/raw_responses cannot be NA. Please check ",
                      "row ", i, " of 'x'."))
        } else if (grepl("Score should be a valid atomic vector", e$message)) {
          stop(paste0("Invalid 'x'. There are no valid responses in row ", i,
                      "."))
        } else {
          stop(e$message, call. = FALSE)
        }
      })
  }

  # response_list <- tryCatch({
  #   sapply(1:nrow(x), function(i)
  #     response(
  #       score = switch(
  #         score_matrix + 1, NULL,
  #         unname(unlist(x[i, !is.na(x[i, , drop = TRUE])]))),
  #       raw_response = switch(
  #         score_matrix + 1,
  #         unname(unlist(x[i, !is.na(x[i, , drop = TRUE])])), NULL),
  #       item_id = item_ids[!is.na(x[i, ])],
  #       testlet_id = switch((is.null(testlet_ids) ||
  #                              all(is.na(testlet_ids[i, ]))) + 1,
  #                           testlet_ids[i, !is.na(x[i, , drop = TRUE])], NULL),
  #       examinee_id = examinee_ids[i]
  #       )
  #     )
  #   }, error = function(e) {
  #     if (grepl("Either 'score' or 'raw_response' should be provided.",
  #               e$message)) {
  #       stop("All scores/raw_responses cannot be NA. Please check 'x'. ")
  #     } else stop(e$message, call. = FALSE)
  # })


  # Setup 'testlet_id' slot
  if (!is.null(testlet_ids)) {
   testlet_ids <- ip$testlet_id[match(item_ids, ip$item_id)]
  }

  result <- new("Response_set", response_list = name_examinees(response_list),
                item_id = item_ids, testlet_id = testlet_ids, misc = misc)
  if (!is.null(ip)) check_validity_response_set_cpp(resp_set = result, ip = ip)
  return(result)
}


###############################################################################@
############################# .create_response_set_from_long_format ############
###############################################################################@
.create_response_set_from_long_format <- function(
  x,
  ip =  NULL,
  examinee_id_var = NULL,
  testlet_id_var = NULL,
  item_id_var = NULL,
  score_var = NULL,
  raw_response_var = NULL,
  order_var = NULL,
  response_time_var = NULL,
  misc_var = NULL,
  misc_unique_var = NULL,
  misc = NULL,
  fill_na_score = NULL) {


  item_ids <- NULL
  testlet_ids <- NULL

  # In case it is 'tibble', make it a data frame
  if (inherits(x, "tbl_df")) x <- as.data.frame(x)

  CALL = as.list(match.call()[-1])
  # Check variable name arguments
  for (i in c("score_var", "examinee_id_var", "item_id_var", "testlet_id_var",
              "raw_response_var", "order_var", "response_time_var")) {
    if (i %in% names(CALL) &&
        !is.null(eval(CALL[[i]])) &&
        (!is(eval(CALL[[i]]), "character") ||
         length(eval(CALL[[i]])) != 1 ||
         !eval(CALL[[i]]) %in% colnames(x)
         )
        )
      stop(paste0("Invalid \"", i, "\" argument. The value should be one ",
                  "of the column names of 'x'."
                  ))
  }

  if (inherits(x, "matrix"))
    stop(paste0("Invalid 'data_format'. Please use 'data_format = ",
                "\"wide\"' when 'x' argument is a 'matrix'."))
  # Three variables should exist in the data.frame:
  if (is.null(examinee_id_var) || is.null(item_id_var))
    stop("When 'data_format = \"long\", 'examinee_id_var' and 'item_id_var' ",
         "should be supplied.")
  if (is.null(score_var) && is.null(raw_response_var))
    stop("When 'data_format = \"long\", either 'score_var' or ",
         "'raw_response_var' should be supplied.")
  # Check whether column names provided are valid
  for (col in c("examinee_id_var", "testlet_id_var", "item_id_var",
                "score_var", "raw_response_var", "order_var",
                "response_time_var", "misc_var", "misc_unique_var")) {
    temp <- eval(as.name(col))
    if (!is.null(temp) && !all(temp %in% colnames(x)))
      stop(paste0("Invalid '", col, "'. Following column name(s) ",
                  "cannot be found in 'x': ",
                  paste0(temp[!temp %in% colnames(x)], collapse = ", ")))
  }
  # Fill the unanswered scores with NA.
  if (!is.null(fill_na_score)) {
    if (is.null(ip)) {
      full_resp <- setNames(expand.grid(unique(x[[examinee_id_var]]),
                                        unique(x[[item_id_var]])),
                            c(examinee_id_var, item_id_var))
      temp_by <- c(examinee_id_var, item_id_var)
      if (!is.null(testlet_id_var)) {
        testlet_ids <- x[, c(item_id_var, testlet_id_var)]
        testlet_ids <- testlet_ids[!duplicated(testlet_ids), ]
        full_resp <- merge(full_resp, testlet_ids, by = item_id_var)
        temp_by <- c(temp_by, testlet_id_var)
        item_ids <- testlet_ids[[item_id_var]]
        testlet_ids <- testlet_ids[[testlet_id_var]]
      }
    } else {
      full_resp <- setNames(expand.grid(unique(x[[examinee_id_var]]),
                                        ip$item_id),
                            c(examinee_id_var, item_id_var))
      temp_by <- c(examinee_id_var, item_id_var)
      if (!is.null(testlet_id_var)) {
        testlet_ids <- data.frame(ip)[, c("item_id", "testlet_id")]
        colnames(testlet_ids) <- c(item_id_var, testlet_id_var)

        # testlet_ids <- x[, c(item_id_var, testlet_id_var)]
        # testlet_ids <- testlet_ids[!duplicated(testlet_ids), ]
        # temp <- unique(testlet_ids[[testlet_id_var]])
        # temp <- temp[!is.na(temp)]
        # if (!all(temp %in% ip$testlet_id[!is.na(ip$testlet_id)]))
        #   stop("Invalid 'testlet_id's. There are some testlet_id's in the ",
        #        "response data that are not in the item pool.")
        full_resp <- merge(full_resp, testlet_ids, by = item_id_var)
        temp_by <- c(temp_by, testlet_id_var)
        item_ids <- testlet_ids[[item_id_var]]
        testlet_ids <- testlet_ids[[testlet_id_var]]
      }
    }
    x <- merge(x = x, y = full_resp, by = temp_by, all = TRUE)
    x[[score_var]][is.na(x[[score_var]])] <- fill_na_score
  }

  # Check whether there are testlets in the item pool and responses. If there
  # are, automatically add testlet info to the responses
  if (!is.null(ip) && any(sapply(ip@item_list, class) == "Testlet") &&
      is.null(testlet_id_var)) {
    testlet_ids <- data.frame(ip)[, c("item_id", "testlet_id")]
    colnames(testlet_ids) <- c(item_id_var, "testlet_id")
    x <- merge(x, testlet_ids, by.x = item_id_var, sort = FALSE)
    testlet_id_var <- "testlet_id"

    # Only include items that are actually in the responses
    testlet_ids <- testlet_ids[testlet_ids[[item_id_var]] %in%
                                 unique(x[[item_id_var]]), ]
    item_ids <- testlet_ids[[item_id_var]]
    testlet_ids <- testlet_ids[[testlet_id_var]]
  }
  x_split <- split(x, f = factor(x[[examinee_id_var]],
                                 # Make sure the order of examinee's in
                                 # Response_set follows the same order as data
                                 levels = unique(x[[examinee_id_var]]),
                                 ordered = TRUE))

  # !!!DONT DELETE the following five lines:
  # # If there is an ip, sort the items for the according to the order in ip.
  # # NOTE: I decided not to implement is because it may wreck the item
  # #       administration order
  # if (!is.null(ip)) x_split <- lapply(
  #   x_split, function(k) k[order(match(k$item_id, ip$item_id)), ])

  response_list <- sapply(
    x_split,
    function(r) response(
      examinee_id = r[[examinee_id_var]][1],
      item_id = r[[item_id_var]],
      testlet_id = switch(is.null(testlet_id_var) + 1,
                          r[[testlet_id_var]], NULL),
      # score = r[[score_var]],
      score = switch(is.null(score_var) + 1, r[[score_var]], NULL),
      raw_response = switch(is.null(raw_response_var) + 1,
                            r[[raw_response_var]], NULL),
      order = switch(is.null(order_var) + 1, r[[order_var]], NULL),
      response_time = switch(is.null(response_time_var) + 1,
                             r[[response_time_var]], NULL),
      misc = c(switch(is.null(misc_var) + 1, as.list(r[misc_var]), NULL),
               switch(is.null(misc_unique_var) + 1,
                      lapply(r[misc_unique_var], unique), NULL))
      ))
  # Setup item_ids
  if (is.null(item_ids)) item_ids <- unique(x[[item_id_var]])
  if (!is.null(ip)) { # if there is an itempool, use it's order for @item_id
                      # slot.
    item_ids <- ip$item_id[ip$item_id %in% item_ids]
  }
  # Setup testlet_ids
  # if (!is.null(testlet_ids)) {
  #   testlet_ids <- testlet_ids$testlet_id[match(item_ids, testlet_ids$item_id)]
  # } else
  if (is.null(testlet_ids) && !is.null(testlet_id_var)) {
    testlet_ids <- x[, c(item_id_var, testlet_id_var)]
    testlet_ids <- testlet_ids[!duplicated(testlet_ids), ]
    if (any(duplicated(testlet_ids[[item_id_var]])))
      stop("Invalid 'item_id' or 'testlet_id's. Each item should be ",
           "associated with a unique testlet.")
    testlet_ids <- testlet_ids[[testlet_id_var]][
      match(item_ids, testlet_ids[[item_id_var]])]
  }
  result <- new("Response_set", response_list = name_examinees(response_list),
                item_id = item_ids, testlet_id = testlet_ids, misc = misc)
  if (!is.null(ip)) check_validity_response_set_cpp(resp_set = result, ip = ip)
  return(result)
}


###############################################################################@
############################# response_set #####################################
###############################################################################@

#' Create \code{\link{Response_set-class}} object
#'
#' @description This function creates a \code{\link{Response_set-class}} object
#'   from various types of data sets. Currently following scenarios are
#'   supported:
#'
#'
#' @param x A \code{matrix} or \code{data.frame} holding item scores. See the
#'   description about the options. Additionally, it can be a list of
#'   \code{\link{Response-class}} objects.
#' @param data_format A string value representing the format of the data
#'   \code{x} supplied. The default value is \code{"wide"}.
#'   The following options are available:
#'   \describe{
#'     \item{"wide"}{\code{x} can be in wide format data where a \code{matrix}
#'       or \code{data.frame} where rows represents examinees and columns
#'       represent items. Each row will be converted to a
#'       \code{\link{Response-class}} object.
#'
#'       If the columns has names (and an \code{\link{Itempool-class}} object
#'       has not been supplied), then the \code{item_id}s will be supplied by
#'       the column names. If neither column names nor an
#'       \code{\link{Itempool-class}} object supplied, default \code{item_id}s
#'       will be given.
#'
#'       If rows has names, those will be used as \code{examinee_id}s.
#'     }
#'     \item{"long"}{\code{x} can be in long format where \code{data.frame}
#'       with at least three columns: (1) a column for \code{examinee_id}, (2) a
#'       column for \code{item_id} and (3) a column for either \code{score}s or
#'       \code{raw_response}s. Additional columns can be added such as
#'       \code{testlet_id}, item \code{order}, \code{response_time}.
#'     }
#'   }
#'
#' @param ip Optionally an \code{\link{Itempool-class}} object that is holding
#'   the item parameters can be supplied to check whether Response_set object
#'   created is compatible with the \code{\link{Itempool-class}} object.
#' @param score_var A string for the column name that holds examinee
#'   scores, if \code{x} is in long format.
#' @param examinee_id_var A string for the column name that holds examinee
#'   ids, if \code{x} is in long format.
#' @param item_id_var A string for the column name that holds item
#'   ids, if \code{x} is in long format.
#' @param testlet_id_var A string for the column name that holds testlet
#'   ids, if \code{x} is in long format.
#' @param raw_response_var A string for the column name that holds raw
#'   responses of the examinees, if \code{x} is in long format.
#' @param order_var A string for the column name that holds the administration
#'   order of items, if \code{x} is in long format.
#' @param response_time_var A string for the column name that holds response
#'   time information of the items, if \code{x} is in long format.
#' @param misc_var A string for the column names that are holding the
#'   miscellaneous information of the items. Available only when \code{x} is
#'   in long format. Within an examinee, if there is additional information
#'   for each item (for example, item's type, item's reading level, examinee's
#'   raw response to an item, whether an item is operational or not,
#'   the date/time item is administered, ratings of multiple raters, etc.),
#'   in the dataset, this information can be passed. Later in the code,
#'   such information can be extracted by \code{$} operator. See examples.
#' @param misc_unique_var A string for the column names that are holding the
#'   miscellaneous information of the items. Different than \code{misc_var},
#'   these columns are assumed to be the same within an examinee, so only the
#'   unique value of this column within an examinee will be saved. Examples of
#'   variables for this column is gender, race, ability score, school of the
#'   examinee that will not vary from one item to another within an examinee.
#'   The argument is only available when \code{data_format = "long"}.
#' @param misc A list of miscellaneous variables that needs to be added to
#'   the \code{Response_set} object.
#' @param fill_na_score If some examinees do not answer all items, the value
#'   \code{fill_na_score} will be replaced by the scores of unanswered items.
#'   If an \code{ip} value provided, 'all items' will be all of the items in
#'   the item pool. Otherwise, all items will be the list of all unique
#'   \code{item_id} values.
#'
#'   Currently, this feature only works when \code{x} is a data frame or matrix.
#'
#' @return A \code{\link{Response_set-class}} object.
#'
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' ##### Wide format data #####
#' ## Example 1
#' x_wide <- matrix(sample(0:1, 35, TRUE), nrow = 7, ncol = 5)
#' response_set(x_wide)
#'
#' ## Example 2
#' ip <- generate_ip(n = 6)
#' # simulate responses for 10 examinees
#' resp_matrix <- sim_resp(ip = ip, theta = rnorm(10), prop_missing = .2,
#'                         output = "matrix")
#' # convert it to tibble
#' resp_wide <- as.data.frame(resp_matrix)
#' resp_wide$stu_id <- rownames(resp_matrix)
#' # Create a Response_set object:
#' resp_set <- response_set(resp_wide, data_format = "wide", ip = ip,
#'                          examinee_id_var = "stu_id")
#' # Retrieve examinee ids:
#' resp_set$examinee_id
#' # Fourth examinee:
#' resp_set[[4]]
#' # Scores of 6th examinee
#' resp_set[[6]]$score
#'
#'
#' ##### Long format data #####
#' x_long <- data.frame(examinee_id = c("stu1", "stu1", "stu1", "stu2", "stu2"),
#'                      item_id = c("i1", "i2", "i4", "i1", "i2"),
#'                      scr = c(0, 1, 0, 1, 0),
#'                      rwscore = c("A", "D", "B", "C", "D"),
#'                      resptime = c(33, 55, 22, 66, 31),
#'                      # These will be passed to misc
#'                      item_type = c("MC", "MC", "MS", "SA", "MC"),
#'                      lexile_level = c(1, 4, 3, 2, 1),
#'                      word_count = c(123, 442, 552, 342, 666),
#'                      ability = c(1.1, 1.1, 1.1, -.2, -.2),
#'                      grade = c("7", "7", "7", "11", "11")
#'                      )
#'
#' resp_set <- response_set(x = x_long,
#'                          data_format = "long",
#'                          examinee_id_var = "examinee_id",
#'                          item_id_var = "item_id",
#'                          score_var = "scr",
#'                          raw_response_var = "rwscore",
#'                          response_time_var ="resptime",
#'                          misc_var = c("item_type", "lexile_level"),
#'                          misc_unique_var = c("ability", "grade")
#'                          )
#'
#' resp_set[[1]]  # Response of the first examinee
#' resp_set$item_type  # extract item_type of each examinee
#' resp_set$grade  # extract grade of each examinee
#'
#' # Also, additional examinee level miscellaneous information can be added:
#' resp_set$gender <- c("M", "F")
#' resp_set[[2]]$gender  # access second examinee's gender.
#' resp_set$gender
#'
#' # Fill missing values with 0.
#' response_set(x = x_long,
#'              data_format = "long",
#'              examinee_id_var = "examinee_id",
#'              item_id_var = "item_id",
#'              score_var = "scr",
#'              raw_response_var = "rwscore",
#'              response_time_var ="resptime",
#'              misc_var = c("item_type", "lexile_level"),
#'              fill_na_score = 0
#'              )
response_set <- function(x,
                         data_format = "wide",
                         ip =  NULL,
                         examinee_id_var = NULL,
                         testlet_id_var = NULL,
                         item_id_var = NULL,
                         score_var = NULL,
                         raw_response_var = NULL,
                         order_var = NULL,
                         response_time_var = NULL,
                         misc_var = NULL,
                         misc_unique_var = NULL,
                         misc = NULL,
                         fill_na_score = NULL) {

  # Check 'ip' argument
  if (!is.null(ip) && !is(ip, "Itempool"))
    stop("Invalid 'ip' argument. 'ip' should be an Itempool object.")

  # Check 'x' argument
  if (!(inherits(x, c("matrix", "data.frame", "list", "Response",
                      "Response_set"))))
    stop("Invalid 'x' argument. 'x' should be a matrix, data.frame or a ",
         "list of 'Response' objects.")
  ################# Response ###############################################@###
  if (is(x, "Response_set")) {
    return(x)
  } else if (is(x, "Response")) {
    return(.create_response_set_from_list(x = list(x), ip = ip, misc = misc))
  ################# list of Responses ######################################@###
  } else if (inherits(x, "list")) {
    return(.create_response_set_from_list(x = x, ip = ip, misc = misc))
  ################# wide ###################################################@###
  } else if (data_format == "wide") {
    return(.create_response_set_from_wide_format(
      x = x, ip = ip, examinee_id_var = examinee_id_var, misc = misc,
      fill_na_score = fill_na_score))
  ################# long ###################################################@###
  } else if (data_format == "long") {
    return(.create_response_set_from_long_format(
      x = x, ip = ip, examinee_id_var = examinee_id_var,
      testlet_id_var = testlet_id_var, item_id_var =  item_id_var,
      score_var = score_var, raw_response_var = raw_response_var,
      order_var = order_var, response_time_var = response_time_var,
      misc_var = misc_var, misc_unique_var = misc_unique_var, misc = misc,
      fill_na_score = fill_na_score))
  } else
    stop("Invalid 'data_format' argument. The 'data_format' should be ",
         "either \"wide\" or \"long\".")
}

###############################################################################@
############################# name_examinees ###################################
###############################################################################@
#' Give Response class elements a unique examinee ID.
#'
#' @description This function gives unique examinee ID's to elements of Response
#'   class. If there is no examinee ID's specified for an
#'   \code{\link{Response-class}} object, a default ID will be given to that
#'   object. If some elements already have ID's, uniqueness of the ID's will be
#'   checked. If they are not unique, an error will be issued. If some are
#'   unique and some are empty, a default ID will be given to the empty ones.
#' @param response_list A list consist of \code{\link{Response-class}}
#'
#' @return A list with \code{\link{Response-class}} objects, all of which
#'   have examinee_ids.
#'
#' @include response-class.R
#' @include response_set-class.R
#'
#' @author Emre Gonulates
#'
#' @keywords internal
#'
#' @noRd
#'
name_examinees <- function(response_list)
{
  # Stop if all elements of the list is not Response objects
  stopifnot(all(sapply(response_list, function(x) is(x, "Response"))))

  examinee_ids <- lapply(response_list, slot, "examinee_id")
  if (any(duplicated(unlist(examinee_ids))))
    stop("Invalid response list. There are duplicated examinee ID's.")
  if (any(sapply(examinee_ids, is.null))) { # there are some NULL examinee_ids
    examinee_ids[sapply(examinee_ids, is.null)] <- setdiff(
      paste0("S", 1:length(examinee_ids)), unlist(examinee_ids))[
        1:sum(sapply(examinee_ids, is.null))]
    # Set examinee ids
    for (i in 1:length(response_list)) {
      response_list[[i]]@examinee_id <- examinee_ids[[i]]
    }
  }
  names(response_list) <- unlist(examinee_ids)
  return(response_list)
}


###############################################################################@
############################# as.data.frame (Response_set) #####################
###############################################################################@
#' Convert a \code{\link{Response_set-class}} object into a long
#' format \code{data.frame}
#'
#' @param x A \code{\link{Response_set-class}} object
#' @param ... additional arguments
#' @param row.names \code{NULL} or a character vector giving the row names for
#'   the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting
#'   column names
#' @param ... additional arguments
#'
#' @include response-class-methods.R
#' @include response_set-class.R
#'
#' @export
#'
#' @author Emre Gonulates
#'
as.data.frame.Response_set <- function(x, row.names = NULL, optional = FALSE,
                                       ...) {
  args <- list(...)

  result <- lapply(x, as.data.frame)

  col_names <- lapply(result, names)
  col_names <- Reduce(union, col_names[order(sapply(col_names, length),
                                             decreasing = TRUE)])
  result <- lapply(
    result, function(r) {
      temp <- setdiff(col_names, names(r))
      temp <- setNames(rep(list(rep(NA, nrow(r))), length(temp)), temp)
      if (length(temp) > 0) do.call(
        "data.frame", lapply(c(r, temp), function(i)
          if (is.list(i)) I(i) else i)) else r
    })

  return(do.call(rbind, c(result, make.row.names = FALSE))[, col_names])
}



###############################################################################@
############################# as.matrix (Response_set) #########################
###############################################################################@
#' Convert a \code{\link{Response_set-class}} object into a \code{matrix}
#'
#' @description This function converts \code{\link{Response_set-class}} objects
#'   to a \code{matrix} object.
#'
#' @param x A \code{\link{Response_set-class}} object
#' @param ... additional arguments
#' @param output Contents of the matrix. The default value is \code{"score"}.
#'   Other options are:
#'   \describe{
#'     \item{"score"}{Matrix of item scores. }
#'     \item{"raw_response"}{Matrix of raw responses.}
#'     \item{"item_id"}{Matrix of item ids.}
#'     \item{"testlet_id"}{Matrix of testlet ids.}
#'     \item{"response_time"}{Matrix of response times.}
#'     \item{"order"}{Matrix of item orders.}
#'     \item{misc}{If all responses has the same \code{'misc'} field, then
#'       the matrix of that misc field can be extracted.}
#'   }
#' @param ip An \code{\link{Itempool-class}} object to use for adding item_id's
#'   as column names. If there are items that are in the item pool but not in
#'   the response data, those items will be added and all values will be
#'   \code{NA}.
#'
#'
#' @return A matrix of examinee item scores within each row and items in each
#'   column.
#'
#' @include response_set-class.R
#'
#' @importFrom methods slot
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' ip <- generate_ip(n = 15)
#' resp_set <- generate_resp_set(ip = ip, theta = rnorm(30), prop_missing = .5)
#' # Matrix of item scores
#' as.matrix(resp_set)
#'
#' # If the item pool object provided, the column names will have the same
#' # order as the item order in item pool
#' as.matrix(resp_set, ip = ip)
#'
#' # Matrix of raw responses
#' as.matrix(resp_set, output = "raw_response")
#'
#' # Matrix of item order
#' as.matrix(resp_set, output = "order")
#'
#' # Matrix of item ids
#' as.matrix(resp_set, output = "item_id")
#'
#'
setMethod("as.matrix", "Response_set", function(x, ..., output = "score",
                                                ip = NULL)  {
  # Get Item ids of all responses
  if (is.null(ip)) item_ids <- x@item_id else item_ids <- ip$item_id
  # An empty data frame with all item_id's
  df <- matrix(NA, nrow = 1,  ncol = length(item_ids),
               dimnames = list(NULL, item_ids))

  if (output %in% c("score", "raw_response", "item_id", "testlet_id",
                    "response_time", "order")) {
    result <- do.call(rbind, lapply(x@response_list, function(r) {
      x <- slot(r, output)
      if (is.null(x)) df else df[, slot(r, "item_id")] <- x
      df
      }))
  } else {
    tryCatch({
    result <- do.call(rbind, lapply(x@response_list, function(r) {
      x <- slot(r, "misc")[[output]]
      if (is.null(x)) df else df[, slot(r, "item_id")] <- x
      df
      }))
    },
    error = function(e) {
      stop(paste0("Unable to extract \"", output, "\". Make sure each ",
                  "Response element has \"", output, "\" slot. "),
           call. = FALSE)
    })
    if (all(is.na(result)))
      stop(paste0("Unable to extract \"", output, "\". Make sure each ",
                  "Response element has \"", output, "\" slot. "),
           call. = FALSE)
  }
  rownames(result) <- get_examinee_id_response_set_cpp(x)
  return(result)
})


###############################################################################@
############################# as.list (Response_set) ###########################
###############################################################################@
#' This function converts Response_set objects to a list object
#'
#' @param x an \code{\link{Response_set-class}} to be coerced to a list object
#' @param ... Additional parameters to be passed to the function.
#'
#' @return A list object with elements from \code{\link{Response-class}}
#'   objects.
#'
#' @include response_set-class.R
#'
#' @export
#'
#' @author Emre Gonulates
#'
as.list.Response_set <- function(x, ...) return(x@response_list)


###############################################################################@
############################# .print.Response_set ##############################
###############################################################################@
.print_response_set <- function(x, ..., n = NULL, n_items = NULL,
                                base_print = FALSE) {
  tryCatch(validObject(x),
           error = function(e) stop(
             paste0("This is not a valid 'Response_set' object:\n", e$message),
             call. = FALSE))

  print_tibble <- !base_print &&
    requireNamespace("pillar", quietly = TRUE) &&
    requireNamespace("tibble", quietly = TRUE)

  num_of_resp <- length(x@response_list)
  temp_text <- paste0("A 'Response_set' of ", num_of_resp, " response",
                      ifelse(num_of_resp == 1, "", "s"), ".\n\n")
  if (print_tibble) {
    cat(pillar::style_subtle(temp_text))
  } else cat(temp_text)

  # Determine the number or responses to be printed
  if (!is.null(n)) n <- as.integer(n)
  if (!is_single_value(n, class = "integer"))
    n <- ifelse(num_of_resp <= 20, num_of_resp, 10)

  # TODO: maybe only first responses can be converted to matrix. But if
  # the responses towards the end has more responses to different item than
  # the previous ones, fewer items might be displayed.
  result <- as.matrix(x[1:n], output = "score")

  if (all(is.na(result))) result <- as.matrix(x[1:n], output = "raw_response")

  num_of_items <- ncol(result)

  # Set the number of columns to view
  if (!is.null(n_items)) n_items <- as.integer(n_items)
  if (!is_single_value(n_items, class = "integer")) {
    n_items <- min(10, num_of_items)
  } else n_items <- min(num_of_items, n_items)

  if (n < 1) {
    result <- ""
  } else if (n < num_of_resp) {
    result <- result[1:n, , drop = FALSE]
  }

  if (print_tibble) {
    setup_tbl <- pillar::tbl_format_setup(
      tibble::as_tibble(result, rownames = switch(is.null(rownames(result)) + 1,
                                                  "examinee_id", NULL)),
      width = NULL, n = n, max_extra_cols = NULL, max_footer_lines = NULL)
    n_items <- num_of_items - setup_tbl$extra_cols_total
    print(setup_tbl$body)

  }
  if (n < 1) {
    text_after <- paste0()
  } else if (n < num_of_resp) {
    text_after <- paste0("# ... with ", num_of_resp - n, " more responses")
  } else {
    text_after <- paste0()
  }

  if (n_items < num_of_items) {
    max_cols_to_display <- 50 + n_items
    temp <- setdiff(1:min(num_of_items, max_cols_to_display), 1:n_items)
    text_after <- paste0(
      text_after, ", and ", prettyNum(num_of_items - n_items, big.mark = ","),
      " more items:\n", paste0(colnames(result)[temp], collapse = ", "),
      ifelse((length(temp) + n_items) < num_of_items, "...", ""))
  }

  if (!print_tibble) {
    result <- result[, 1:n_items]
    print(result)
  }

  # Format text after
  if (length(text_after) > 0) {
    if (print_tibble) {
      cat(pillar::style_subtle(text_after))
    } else {
      text_after <- format_text(text_after, fg = "light gray", italic = TRUE)
      cat(text_after, "\n", sep = "")
    }
  }

}


###############################################################################@
############################# print.Response_set ###############################
###############################################################################@
#' Print a \code{\link{Response_set-class}} object
#'
#' @param x An \code{\link{Response_set-class}} object that will be showed.
#' @param ... Additional arguments. For example, an argument \code{n = 14},
#'   will print 14 items to the console.
#'
#' @export
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
#'
setMethod("print", "Response_set", function(x, ...)  {
  args <- list(...)
  .print_response_set(x = x,
                      n = switch("n" %in% names(args), args$n, NULL),
                      base_print = ifelse("base_print" %in% names(args),
                                          args$base_print, FALSE))
  })


###############################################################################@
############################# show.Response_set ################################
###############################################################################@
#' Show a \code{\link{Response_set-class}} object
#'
#' @param object An \code{\link{Response_set-class}} object that will be showed.
#'
#' @export
#'
#' @rdname show
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
setMethod("show", "Response_set", function(object) {
  .print_response_set(object)}
  )



###############################################################################@
############################# $ method #########################################
###############################################################################@
#' Get slots of the a \code{\link{Response_set-class}} object.
#'
#' @param x An \code{\link{Response_set-class}} object.
#' @param name Name of the parameter.
#'   Available values:
#'   \describe{
#'     \item{\strong{\code{'response_list'}}}{Extract Response objects
#'       as a list.}
#'     \item{\strong{\code{'item_id'}}}{Extract unique list of item IDs that
#'       are in the response set.}
#'     \item{\strong{\code{'testlet_id'}}}{Extract unique list of testlet IDs
#'       that are in the response set.}
#'     \item{\strong{\code{'misc'}}}{Extract 'misc' field.}
#'     \item{\strong{\code{'score'}}}{Return a score matrix of responses}
#'     \item{\strong{\code{'raw_response'}}}{Return a raw score matrix of
#'       responses}
#'   }
#'
#'
#' @return See the 'name' argument above for possible return values.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @include response_set-class.R
#'
#' @examples
#' resp <- sim_resp(ip = generate_ip(), theta = rnorm(5),
#'                  output = "response_set")
#' resp$response_list
#'
setMethod(
  "$", "Response_set",
  function(x, name) {
    result <- NULL
    switch(
      name,
      "response_list" = return(x@response_list),
      "item_id" = return(x@item_id),
      "testlet_id" = return(x@testlet_id),
      "misc" = return(x@misc),
      "examinee_id" = return(unname(sapply(x@response_list, slot,
                                           "examinee_id"))),
      "score" = return(as.matrix(x, output = "score")),
      "raw_response" = return(as.matrix(x, output = "raw_response")),
      # The default checks the names of the misc field
      {
        if (!is.null(x@misc) && name %in% names(x@misc)) {
          return(x@misc[[name]])
        } else {
          result <- sapply(x@response_list, function(i) i@misc[[name]])
          if (all(sapply(result, is.null))) {
            result <- NULL
          } else result <- stats::setNames(result, x$examinee_id)
        }
      }
    )
    return(result)
  }
)


###############################################################################@
############################# $<- method (Response_set) ########################
###############################################################################@
#' Set values to components of 'Response_set' class objects
#'
#' @param x \code{\link{Response_set-class}} object.
#' @param name Name of the parameter or component. Currently only
#'          \code{response_list}, \code{misc} are available.
#' @param value The new value that will be assigned.
#'
#' @return This operation will return an \code{\link{Response_set-class}}
#'   object.
#'
#' @importFrom methods slot<-
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @include response_set-class.R
#'
setMethod(
  "$<-", "Response_set",
  function(x, name, value) {
    switch(name,
           "response_list" = {
             if (!is.list(value) || !all(sapply(value, is, "Response")))
               stop("Invalid value. 'response_list' should be a list of ",
                    " Response objects.", call. = FALSE)
             x@response_list <- value
           },
           "examinee_id" = {
             n <- length(x)
             if (length(value) == n && is.atomic(value) && !is.matrix(value) &&
                 (is.character(value) || is.integer(value))) {
               for (i in 1:n) x@response_list[[i]]@examinee_id = value[i]
             } else stop(paste0("Invalid value. 'examinee_id' should be a ",
                                "integer or character vector of length ", n,
                                "."), call. = FALSE)
           },
           "misc" = {
             if (!is.list(value))
               stop("Invalid value. 'misc' should be a list.", call. = FALSE)
             x@misc <- value
           }, {# If the '$name' does not exist first check whether the length
               # of the value is equal to the number of responses. If yes,
               # then create an misc element within each Response with this
               # '$name'.Otherwise, add this value as a misc field to the
               # Response set object.
             if (length(value) == length(x)) { # add it to each Response element
               resp_list <- x@response_list
               resp_list <- lapply(1:length(resp_list), function(i) {
                 resp_list[[i]]@misc[[name]] <- value[i]; resp_list[[i]]})
               x@response_list <- resp_list
               # if 'name' already exists update it in Response_set@misc
               # otherwise create a new 'name' element in Response_set@misc.
             } else {
               if (is.null(x@misc)) x@misc <- list()
               x@misc[[name]] <- value
             }
           }
    )
    validObject(x)
    return(x)
  }
)


###############################################################################@
############################# length (Response_set) ############################
###############################################################################@
#' Find the length of a \code{\link{Response_set-class}} object
#'
#' @param x an \code{\link{Response_set-class}} object
#'
#' @export
#'
#' @rdname length
#'
#' @author Emre Gonulates
#'
setMethod(f = "length", signature = "Response_set",
          definition = function(x) length(x@response_list))


###############################################################################@
############################# [ (Response_set) #################################
###############################################################################@
#' Subset \code{Response_set} objects
#'
#' @param x A \code{\link{Response_set-class}} object from which to extract
#'   element(s) or in which to replace element(s).
#' @param i indices specifying elements to extract or replace.
#' @param j This will not be used in \code{\link{Response_set-class}} objects.
#' @param ... Parameters to be passed to the function.
#' @param drop (From R manual:) For matrices and arrays. If TRUE the result is
#' coerced to the lowest possible dimension (see the examples). This only works
#' for extracting elements, not for the replacement. See drop for further
#' details.
#'
#' @return An \code{\link{Response_set-class}} object.
#'
#' @include response_set-class.R
#'
#' @export
#'
#' @keywords internal
#'
#' @rdname subset-response_set
#'
#' @author Emre Gonulates
#'
#' @examples
#' resp_set <- sim_resp(ip = generate_ip(n = 12), theta = rnorm(10),
#'                      output = "response_set")
#' resp_set[1]
#' # Create an Response_set using the first and third element:
#' resp_set[c(1, 3)] # Order is important
#' resp_set[c(3, 1)]
#' resp_set[-2]
#' resp_set[c(TRUE, FALSE, TRUE)]
#' resp_set[c("S2", "S1")]
#' # Recycle, i.e. get all elements
#' resp_set[TRUE]
#' # Recycle, i.e. get all even elements
#' resp_set[c(FALSE, TRUE)]
#'
#' # Use logical expressions
#' resp_set[resp_set$examinee_id %in% c("S3", "S8")]
setMethod(
  "[", c(x = "Response_set", i = "ANY", j = "missing", drop = "ANY"),
  function(x, i, j, ..., drop = TRUE)
    {
      # If (1) all of the elements of i are character,
      #    (2) examinee_id's of response_list is not null
      #    (2) none duplicated and
      #    (3) all of them are examinee ids:
      if (is.character(i)) {
        if (!any(duplicated(i)) && all(i %in% unlist(x$examinee_id)) &&
            !all(sapply(x$examinee_id, is.null))) {
          x@response_list <- x@response_list[
            sapply(i, function(k) which(k == x$examinee_id))]
        } else
          stop("Failed to subset using the given vector. Please use ",
               "valid examinee ID's. There are either duplicated ID's ",
               "or non-existent ID's in the subsetting vector provided.",
               call. = FALSE)
      } else if (is.logical(i) || is.numeric(i)) {
        # If there are NA values in the index then (1) if indices are
        # numeric, remove NAs. (2) if indices are logical, convert NAs to
        # FALSE
        if (any(is.na(i))) {
          if (is.numeric(i)) i <- i[!is.na(i)] else i[is.na(i)] <- FALSE
        }
        x@response_list <- x@response_list[i]
      }
      tryCatch({
        validObject(x)
        },
        error = function(e) {
          if (grepl("Response set cannot be empty.", e$message))
            stop("The selection did not match any Response objects.",
                 call. = FALSE)
          else stop(e$message, call. = FALSE)
        })
      return(x)
      })



###############################################################################@
############################# [[ (Response_set) ################################
###############################################################################@
#' Subset \code{Response_set} objects
#'
#' @param x A \code{\link{Response_set-class}} object from which to extract
#'   Response(s) or in which to replace element(s).
#' @param i indices specifying elements to extract or replace.
#' @param j This will not be used in \code{\link{Response_set-class}} objects.
#' @param ... Additional parameters to be passed to the function.
#'
#' @return A \code{\link{Response-class}} object.
#'
#' @include response_set-class.R
#'
#' @export
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
#' @examples
#' resp_set <- sim_resp(ip = generate_ip(n = 12), theta = rnorm(10),
#'                      output = "response_set")
#' resp_set[[2]]
setMethod("[[", c("Response_set", "numeric", "missing"),
          function(x, i, j, ...)
          {
            result <- tryCatch({
              x@response_list[[i]]
              },
              error = function(e) {
                if (grepl("subscript out of bounds", e$message))
                  stop(paste0(
                    "Subscript out of bounds. Please use an index between ",
                    "1 and ", length(x), "."), call. = FALSE)
                NULL
              }
              )
            return(result)
          })



###############################################################################@
############################# [[<- (Response_set) ##############################
###############################################################################@
#' Set the elements of an \code{Response_set} objects.
#'
#' @param x \code{\link{Response-class}} object.
#' @param i indices specifying elements to extract or replace.
#' @param j This will not be used in \code{\link{Response_set-class}} objects.
#' @param value An \code{\link{Response-class}} object.
#' @param ... Additional parameters to be passed to the function.
#'
#' @return An updated \code{\link{Response_set-class}} object.
#'
#' @include response_set-class.R
#'
#' @export
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
#' @examples
#'
#' resp_set <- sim_resp(ip = generate_ip(n = 12), theta = rnorm(10),
#'                      output = "response_set")
#' r1 <- response(score = c(0, 1), examinee_id = "Stu-22",
#'                item_id = c("Item_1", "Item_2"))
#' resp_set[[2]] <- r1
setMethod("[[<-", signature = c("Response_set", "numeric", "missing"),
          function(x, i, j, value)
            {
            if (!is(value, "Response"))
              stop("Invalid assignment. The value shuld be a Response object.")
            x@response_list[[i]] <- value
            validObject(x)
            return(x)
          })



###############################################################################@
############################# concatenation of 'Response' objects ##############
###############################################################################@
.concatenate.Response.Response_set.obj <- function(x, ...) {
  args = list(x, ...)
  if (!all(sapply(args, inherits, c("Response", "Response_set"))))
    stop("All of the elements should be 'Response' or 'Response_set' class.",
         .call = FALSE)

  response_list <- list()


  element_no <- 0
  for (i in 1:length(args)) {
    if (is(args[[i]], "Response")) {
      element_no <- element_no + 1
      response_list[[element_no]] = args[[i]]
    } else if (is(args[[i]], "Response_set")) {
      for (j in 1:length(args[[i]])) {
        element_no <- element_no + 1
        response_list[[element_no]] <- args[[i]]@response_list[[j]]
      }
    }
  }
  # Set examinee_id's if they are missing
  response_list <- name_examinees(response_list)
  return(response_set(response_list))
  # return(methods::new(Class = "Response_set", response_list = response_list))
}


#' Concatenate \code{Response} and/or \code{Response_set} objects
#'
#' @description This function concatenates \code{Response} and/or
#'   \code{Response_set} objects and returns a \code{\link{Response_set-class}}
#'   object.
#'
#' If the elements do not have examinee ID fields, function will assign
#' default ids.
#'
#' @param x A list consist of \code{\link{Response-class}} or
#'   \code{\link{Response_set-class}} objects.
#' @param ... Additional arguments
#'
#' @return A \code{\link{Response_set-class}} object.
#'
#' @include response-class.R
#' @include response_set-class.R
#'
#' @rdname c
#'
#' @method c Response
#'
#' @export
#'
#' @author Emre Gonulates
#'
setMethod("c", signature(x = "Response"),
          .concatenate.Response.Response_set.obj)

#' @rdname c
#' @method c Response_set
setMethod("c", signature(x = "Response_set"),
          .concatenate.Response.Response_set.obj)




###############################################################################@
############################# convert_to_resp_set ##############################
###############################################################################@
#' Convert a matrix or data.frame to a Response_set object
#'
#' @description This is an internal function that will be used at the
#'   beginning of a function call to convert a matrix or data.frame object to
#'   a \code{\link{Response_set-class}} object. If it fails, it will raise
#'   an error.
#'
#' @param resp An object that will be converted to a response set
#' @param ip An \code{\link{Itempool-class}} object. This will help to supply
#'   item ID's and to check the correspondence between supplied \code{resp} and
#'   the itempool.
#' @param enforce_data_type It can take three possible values:
#'   \describe{
#'     \item{"any"}{The resulting \code{Response_set} object can either have
#'       "raw_response", "score" or both. }
#'     \item{"score"}{All \code{Response} objects within the resulting
#'       \code{Response_set} object should have valid 'score' slots.
#'     }
#'     \item{"raw_response"}{All \code{Response} objects within the resulting
#'       \code{Response_set} object should have valid 'raw_score' slots.
#'     }
#'   }
#' @param object_name When calling this function within another function,
#'   this string value is the name of the argument that represent 'resp'. This
#'   value will be supplied to error messages to make them more targetted.
#'
#' @return A \code{\link{Response_set-class}} object.
#'
#' @noRd
#'
#' @author Emre Gonulates
#'
convert_to_resp_set <- function(resp, ip = NULL, enforce_data_type = "score",
                                object_name = "resp") {
  error_message <- paste0(
    "Invalid response pattern. '", object_name, "' cannot be converted to ",
    "a Response_set object. Please try 'response_set()' ",
    "function to create a Response_set object and supply ",
    "it to this function.")
  if (is(resp, "Response_set")) {
    resp_set <- resp
    if (is(ip, "Itempool") && !all(resp_set$item_id %in% ip$item_id)) {
      stop(paste0("Invalid 'ip'. All of the items in the response data ",
                  "should be in the item pool, ip."), call. = FALSE)
    }
  } else if (is.numeric(resp) && is.atomic(resp) && !is.matrix(resp)) {
    resp_set <- tryCatch({
      if (is.null(ip)) {
        response_set(response(resp))
      } else {
        response_set(list(response(resp, item_id = ip$resp_id)), ip = ip)
      }
    }, error = function(e) {
      if (grepl(paste0("Invalid 'score' vector. The length of 'score' ",
                       "vector should equal to the length of 'item_id'"),
                e$message)) {
        stop(paste0("Invalid '", object_name, "'. The length of the response ",
                    "pattern should be equal to the number of items."),
             call. = FALSE)
      } else if (grepl(paste0("Invalid \"item_id\"s. \"item_id\"s cannot ",
                              "be duplicated."), e$message)) {
        stop(paste0("'", object_name, "' and 'ip' do not correspond. ",
                    "Make sure the number ",
                    "of item resonses and item pool size corresponds.\n"),
             call. = FALSE)
      } else {
        stop(paste0(error_message, "\n\n", e), call. = FALSE)
      }
    })
  } else {
    resp_set <- tryCatch({
      response_set(resp, ip = ip)
    }, error = function(e) {
      stop(paste0(error_message, "\n\n",  e), call. = FALSE)
    })
  }
  if (enforce_data_type != "any") {
    if (enforce_data_type == "score") {
      if (any(apply(is.na(resp_set$score), 1, all)))
        stop(paste0(
          "Invalid '", object_name, "'. In the resulting Response_set ",
          "object at least for one examinee there are no valid scores."))
    } else if (enforce_data_type == "raw_response") {
      if (any(apply(is.na(resp_set$raw_response), 1, all)))
        stop(paste0(
          "Invalid '", object_name, "'. In the resulting Response_set ",
          "object at least for one examinee there are no valid scores."))
    } else stop("Invalid 'enforce_data_type'. Please choose one of the ",
                "following values: 'any', 'score' or 'raw_response'")
  }
  return(resp_set)
}




