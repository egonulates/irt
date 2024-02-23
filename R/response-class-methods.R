



#' Create a Response object from a vector of responses
#'
#' @param score A numeric vector holding the scores given to items.
#' @param examinee_id Examinee/Subject/Student ID. A character string to identify
#'   an examinee.
#' @param item_id A character vector holding the item IDs.
#' @param testlet_id A character vector holding the testlet IDs that given item
#'   belongs. It can be \code{NULL} if none of the items belongs to any testlet.
#'   Items that do not belong to any testlet should be represented by \code{NA}.
#' @param raw_response A vector of strings holding the raw responses to items.
#' @param order An integer vector representing the administration order of an
#'   item.
#' @param response_time A numeric vector representing the response times. By
#'   default, numbers are assumed to represent seconds.
#' @param misc A list that will hold miscellaneous information about the
#'   responses. For example, \code{misc = list(item_role = c("O", "O", "O","F"))}
#'   will hold whether administered item is a field test or an operational test
#'   item.
#'
#' @export
#'
#' @author Emre Gonulates
#'
response <- function(score = NULL, examinee_id = NULL, item_id = NULL,
                     raw_response = NULL, testlet_id = NULL, order = NULL,
                     response_time = NULL, misc = NULL) {
  if (is.null(score) && is.null(raw_response))
    stop("Either 'score' or 'raw_response' should be provided.")

  if (!is.null(score) && (!is.atomic(score) || is.matrix(score) ||
                          length(score) == 0) )
    stop("Invalid 'score'. Score should be a valid atomic vector.")
  if (!is.null(raw_response) && (!is.atomic(raw_response) ||
                                 is.matrix(raw_response) ||
                                 length(raw_response) == 0))
    stop("Invalid 'raw_response'. Raw responses should be a valid atomic ",
         "vector.", call. = FALSE)

  if (!is.null(order)) order <- as.integer(order)

  # If there are any missing values in score or raw_response,
  # they will be removed
  if (!is.null(score)) {
    nonmissing_indices <- !is.na(score)
  } else if (!is.null(raw_response)) {
    nonmissing_indices <- !is.na(raw_response)
  }

  # There must be item_ids
  if (is.null(item_id)) {
    item_id <- paste0("Item_", 1:sum(nonmissing_indices))
  } else item_id <- item_id[nonmissing_indices]


  new("Response",
      score = switch(is.null(score) + 1, score[nonmissing_indices], NULL),
      examinee_id = examinee_id,
      raw_response = switch(is.null(raw_response) + 1,
                            raw_response[nonmissing_indices], NULL),
      item_id = item_id,
      testlet_id = switch(is.null(testlet_id) + 1,
                          testlet_id[nonmissing_indices], NULL),
      order = switch(is.null(order) + 1, order[nonmissing_indices], NULL),
      response_time = switch(is.null(response_time) + 1,
                             response_time[nonmissing_indices], NULL),
      misc = misc
      )
}




###############################################################################@
############################# as.data.frame (Response) #########################
###############################################################################@
#' Convert a \code{\link{Response-class}} object into a \code{data.frame}.
#'
#' @description This function converts \code{\link{Response-class}} objects to a
#'   \code{data.frame} object.
#'
#' @param x An \code{\link{Response-class}} object
#' @param row.names \code{NULL} or a character vector giving the row names for
#'   the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting
#'   column names
#' @param ... additional arguments
#' @param attach_unique_misc If \code{TRUE}, the elements of the \code{misc}
#'   slot that have lengths one will be attached to the data frame returned.
#'   The default is \code{TRUE}.
#'
#' @return A data frame of item_ids/responses/scores within each row.
#'
#' @export
#'
#' @include response-class.R
#'
#' @author Emre Gonulates
#'
#' @examples
#'
#' resp <- response(examinee_id = "Stu12",
#'                  item_id = c("Item1", "Item2", "Item3", "Item4"),
#'                  score = c(0, 1, 1, 1),
#'                  raw_response = c("B", "A", "D", "Right Angle"),
#'                  order = c(1L, 2L, 3L, 4L),
#'                  misc = list(item_role = c("F", "O", "O", "O"),
#'                              lexile_level = c(1, 4, 3, 1),
#'                              item_type = c("MC", "MC", "MS", "SA"),
#'                              test_date = as.Date("2021-11-21"),
#'                              Form = "Test Form 001",
#'                              theta = 2.2))
#' as.data.frame(resp)
#'
#' # Do not include misc fields whose lengths are not equal to the number of
#' # items
#' as.data.frame(resp, attach_unique_misc = FALSE)
#'
as.data.frame.Response <- function(x, row.names = NULL, optional = FALSE, ...,
                                   attach_unique_misc = TRUE)
{
  args <- list(...)
  n_items <- length(x@item_id)
  col_names <- setdiff(slotNames(x), c("misc", "examinee_id"))
  result <- setNames(lapply(col_names, slot, object = x), col_names)
  result <- result[!sapply(result, is.null)]
  if (!is.null(x@examinee_id))  {
    result <- c(examinee_id = list(rep(x@examinee_id, n_items)), result)
  }
  if (!is.null(x@misc)) {
    # Add vector misc elements which have the same number of elements as
    # the number of items.
    temp <- which(sapply(x@misc, length) == n_items &
            sapply(x@misc, is_atomic_vector) & names(x@misc) != "")
    result <- c(result, x@misc[temp])

    # Add vector misc elements whose lengths are 1.
    if (attach_unique_misc) {
      temp <- which(sapply(x@misc, length) == 1 &
              sapply(x@misc, is_atomic_vector) & names(x@misc) != "")
      if (length(temp) > 0) {
        result <- c(result,lapply(x@misc[temp], rep, each = n_items))
      }
    }
  }

  return(as.data.frame(do.call(cbind.data.frame, result)))
  # result <- as.data.frame(do.call(cbind.data.frame, list(
  #   item_id = x@item_id,
  #   testlet_id =  x@testlet_id,
  #   score = x@score,
  #   raw_response = x@raw_response,
  #   order = x@order,
  #   response_time = x@response_time
  #   )))
  # if (!is.null(x@misc))
  #   for (i in which(sapply(x@misc, length) == nrow(result) &
  #                   sapply(x@misc, function(y) (is.numeric(y) ||
  #                          is.character(y) || is.integer(y) || is.logical(y)) &&
  #                          !inherits(y, "matrix")) &
  #                   names(x@misc) != ""))
  #   result[, names(x@misc)[i]] <- x@misc[[i]]
  # return(result)
}



###############################################################################@
############################# .print.Response ##################################
###############################################################################@

.print.Response <- function(x, ..., n = NULL, base_print = FALSE) {
  x_df <- as.data.frame(x, attach_unique_misc = FALSE)
  # Remove 'examinee_id' column
  x_df <- x_df[, colnames(x_df) != "examinee_id"]
  n_items <- length(x) # or nrow(x_df)
  score_type <- ifelse("score" %in% colnames(x_df), "scores", "raw responses")
  # Make singular or plural
  score_type <- ifelse(n_items == 1, gsub("s$", "", score_type), score_type)

  print_tibble <- !base_print &&
    requireNamespace("pillar", quietly = TRUE) &&
    requireNamespace("tibble", quietly = TRUE)

  print_text <- function(text) {
    if (print_tibble) {
      cat(pillar::style_subtle(text))
    } else cat(text)
  }
  print_text(paste0("A 'Response' object with ", n_items, " ", score_type,
                    ".\n"))

  if (!is.null(x@examinee_id))
    print_text(paste0("Examinee ID: \"", x@examinee_id, "\"\n"))

  # Set the number of rows to print
  if (is.null(n)) {
    n <- ifelse(n_items <= 20, n_items, 10)
  } else if (is.infinite(n) | n > n_items) n <- length(x)

  text_after <- ""
  if (n_items > n)
    text_after <- paste0("# ... with ", n_items - n, " more ", score_type)
  if (print_tibble) {
    setup_tbl <- pillar::tbl_format_setup(
      tibble::as_tibble(x_df),
      width = NULL, n = n, max_extra_cols = NULL, max_footer_lines = NULL)
    print(setup_tbl$body)
    if (setup_tbl$extra_cols_total > 0) {
      footer_extra_col_text <- paste0(
        setup_tbl$extra_cols_total, " more variable",
        ifelse(setup_tbl$extra_cols_total > 1, "s", ""), ": '",
        paste0(names(setup_tbl$extra_cols), collapse = "', '"), "'")
      if (text_after == "") {
        text_after <- paste0("# ... with ", footer_extra_col_text)
      } else {
        text_after <- paste0(text_after, ", and ", footer_extra_col_text)
      }
    }

  } else {
    print(x_df[1:n, , drop = FALSE])
  }

  print_text(paste0(text_after, ifelse(text_after == "", "", "\n")))
    # cat(format_text(paste0("# ... with ", n_items - n, " more ", score_type,
    #                        "\n"), fg = "light gray", italic = TRUE))

  # Print unique misc values or misc values whose lengths are not equal
  # to the number of items
  temp <- x@misc[!names(x@misc) %in% colnames(x_df)]
  # factors are printed as integers when using 'str', convert them to
  # "character" to show their values properly
  temp <- lapply(temp,
                 function(k) if (inherits(k, "factor")) as.character(k) else k)
  if (length(temp) > 0) {
    print_text(ifelse(any(names(x@misc) %in% colnames(x_df)),
                      "\nAdditional 'misc' fields:\n",
                      "\n'misc' fields:\n"))
    temp <- paste0(utils::capture.output(str(
      temp, max.level = 1, give.head = FALSE, indent.str = "  ",
      comp.str = " ", no.list = TRUE)), collapse = "\n")
    print_text(temp)
  }
}



###############################################################################@
############################# print.Response ###################################
###############################################################################@
#' Show an \code{\link{Response-class}} object
#'
#' @param x A \code{\link{Response-class}} object that will be printed.
#' @param ... Additional arguments. For example, an argument \code{n = 14},
#'   will print 14 rows of the responses/scores.
#'
#' @export
#'
#' @include response-class.R
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
setMethod("print", "Response", function(x, ...)  {
  args <- list(...)
  .print.Response(x = x, n = switch("n" %in% names(args), args$n, NULL))
  })


###############################################################################@
############################# show.Response ####################################
###############################################################################@
#' Show an \code{\link{Response-class}} object
#'
#' @param object An \code{\link{Response-class}} object that will be showed.
#'
#' @export
#'
#' @rdname show
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
setMethod("show", "Response", function(object) {.print.Response(object)})



###############################################################################@
############################# $ method #########################################
###############################################################################@
#' Get slots of the an \code{\link{Response-class}} object.
#'
#' @param x An \code{\link{Response-class}} object.
#' @param name Name of the parameter.
#'   Available values:
#'   \describe{
#'     \item{\strong{\code{'examinee_id'}}}{Extract Examinee/Subject/Student
#'       ID.}
#'     \item{\strong{\code{'item_id'}}}{Extract item ids}
#'     \item{\strong{\code{'testlet_id'}}}{Extract testlet IDs, if there is
#'       any.}
#'     \item{\strong{\code{'score'}}}{Extract item scores.}
#'     \item{\strong{\code{'raw_response'}}}{Extract raw responses.}
#'     \item{\strong{\code{'order'}}}{Extract item order.}
#'     \item{\strong{\code{'response_time'}}}{Extract response times.}
#'     \item{\strong{\code{'misc'}}}{Extract 'misc' field.}
#'   }
#'
#'
#' @return See the 'name' argument above for possible return values.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @include response-class.R
#'
#' @examples
#' resp <- response(score = c(0, 1, 0), examinee_id = "Ex-412",
#'                  item_id = c("I1", "I2", "I3"),
#'                  raw_response = c("B", "D", "A"),
#'                  order = 1:3,
#'                  response_time = c(66, 23, 89),
#'                  misc = list(form = "A1",
#'                              operational = c(TRUE, TRUE, FALSE))
#'                  )
#' resp$score
#' resp$item_id
#' resp$examinee_id
#' resp$raw_response
#' resp$order
#' resp$response_time
#' resp$misc
#' resp$misc$form
#' resp$form
#'
setMethod(
  "$", "Response",
  function(x, name) {
    switch(
      name,
      "score" = return(x@score),
      "item_id" = return(x@item_id),
      "testlet_id" = return(x@testlet_id),
      "examinee_id" = return(x@examinee_id),
      "raw_response" = return(x@raw_response),
      "order" = return(x@order),
      "response_time" = return(x@response_time),
      "misc" = return(x@misc),
      # The default checks the names of the misc field
      {
        if (!is.null(x@misc) && name %in% names(x@misc)) return(x@misc[[name]])
      }
    )
    return(NULL)
  }
)


###############################################################################@
############################# $<- method (Response) ############################
###############################################################################@
#' Set values to components of 'Response' class objects
#'
#' @param x \code{\link{Response-class}} object.
#' @param name Name of the parameter or component. Following are available:
#'   \describe{
#'     \item{\strong{\code{'examinee_id'}}}{Set Examinee/Subject/Student ID.}
#'     \item{\strong{\code{'item_id'}}}{Set item ids.}
#'     \item{\strong{\code{'testlet_id'}}}{Set testlet IDs.}
#'     \item{\strong{\code{'score'}}}{Set item scores.}
#'     \item{\strong{\code{'raw_response'}}}{Set raw responses.}
#'     \item{\strong{\code{'order'}}}{Set item order.}
#'     \item{\strong{\code{'response_time'}}}{Set response times.}
#'     \item{\strong{\code{'misc'}}}{Set 'misc' field.}
#'     \item{\strong{\code{...}}}{Any value that does not match the names above,
#'       will be added to the misc field of the Response. }
#'
#'   }
#'
#' @param value The new value that will be assigned.
#'
#' @return This operation will return an \code{\link{Response-class}} object.
#'
#' @importFrom methods slot<-
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @include response-class.R
#'
#' @examples
#' resp <- response(score = c(0, 1, 0))
#' resp
#' resp$examinee_id <- "Stu-123"
#' resp$item_id <- c("i14", "i4", "i9")
#' resp$raw_response <- c("D", "D", "C")
#' resp$order <- c(4L, 3L, 1L)
#' resp$misc <- list(Form = "A1", operational = c(TRUE, TRUE, FALSE))
#' resp
#'
#' # Add any other named element:
#' resp$content <- c("Alg", "Alg", "Geo")
#' resp
#' resp$misc
setMethod(
  "$<-", "Response",
  function(x, name, value) {
    switch(name,
           "examinee_id"= {x@examinee_id <- value},
           "item_id"= {x@item_id <- value},
           "testlet_id"= {x@testlet_id <- value},
           "score"= {x@score <- value},
           "raw_response"= {x@raw_response <- value},
           "order"= {x@order <- value},
           "response_time"= {x@response_time <- value},
           "misc" = {x@misc <- value},
           # if no slot name matches, then a value will be added to the misc
           # slot.
           {
             if (is.null(x@misc)) x@misc <- list()
             x@misc[[name]] <- value
           }
    )
    validObject(x)
    return(x)
  }
)


###############################################################################@
############################# length (Response) ################################
###############################################################################@
#' Find the length of an \code{\link{Response-class}} object
#'
#' @param x an \code{\link{Response-class}} object
#'
#' @export
#'
#' @rdname length
#'
#' @author Emre Gonulates
#'
#' @examples
#' r <- response(sample(0:1, 22, TRUE))
#' length(r)
#'
setMethod(f = "length", signature = "Response",
          definition = function(x) length(x@item_id))


###############################################################################@
############################# convert_to_response ##############################
###############################################################################@
#' Convert matrix or data.frames to a Response_set object.
#'
#' @description This is an internal function that will be used at the
#'   beginning of a function call to convert a response string object to
#'   a Response object. If it fails, it will raise an error.
#'
#' @param resp An object that will be converted to a Response
#'
#' @return A \code{\link{Response}} object.
#'
#' @noRd
#'
#' @author Emre Gonulates
#'
# Convert resp into "Response_set" object
convert_to_response <- function(resp, ip = NULL, object_name = "resp") {
  error_message <- paste0(
    "Invalid response pattern. '", object_name, "' cannot be converted to ",
    "a Response object. Please try 'response()' function to create a Response ",
    "object and supply it to this function.")

  if (is(resp, "Response")) {
    return(resp)
  } else if (is(resp, "Response_set") && length(resp) == 1) {
    return(resp[[1]])
  } else if (is(resp, "Response_set")) {
    stop(paste0("Invalid '", object_name, "'. Please select one Response ",
                "object by `", object_name, "[[i]]`, where ",
                "'i' is an integer representing an examinee."))
  } else if (is.numeric(resp) && is.atomic(resp) && !is.matrix(resp)) {
    output <- tryCatch({
      if (is.null(ip)) {
        response(score = resp)
      } else {
        response(score = resp, item_id = ip$resp_id)
      }
    }, error = function(e) {
      stop(paste0(error_message, "\n\n", e), call. = FALSE)
    })
  } else {
    output <- tryCatch({
      response(resp)
    }, error = function(e) {
      stop(paste0(error_message, "\n\n", e), call. = FALSE)
    })
  }
  return(output)
}
