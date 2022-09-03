setClassUnion(name = "listNULL", members = c("list","NULL"))
setClassUnion(name = "characterNULL", members = c("character","NULL"))
setClassUnion(name = "integercharacterNULL", members = c("integer", "character",
                                                         "NULL"))
setClassUnion(name = "numericNULL", members = c("numeric","NULL"))
setClassUnion(name = "integerNULL", members = c("integer","NULL"))



################################################################################
############################# Response class ##################################@
###########################################################################@####
#' An S4 class representing responses of a single examinee
#'
#' @slot examinee_id Examinee/Subject/Student ID. A string or an integer to
#'   identify an examinee.
#' @slot item_id A character vector holding the item IDs.
#' @slot testlet_id A character vector holding the testlet IDs that given item
#'   belongs. It can be \code{NULL} if none of the items belongs to any testlet.
#'   Items that do not belong to any testlet should be represented by \code{NA}.
#' @slot score A numeric vector holding the scores given to items.
#' @slot raw_response A vector of strings holding the raw responses to items.
#' @slot order An integer vector representing the administration order of an
#'   item.
#' @slot response_time A numeric vector representing the response times. By
#'   default, numbers are assumed to represent seconds.
#' @slot misc A list that will hold miscellaneous information about the
#'   responses. For example, code{misc = list(item_role = c("O", "O", "O","F"))}
#'   will hold whether administered item is a field test or an operational test
#'   item.
#'
#' @export
#'
#' @author Emre Gonulates
#'

setClass(Class = "Response",
         slots = c(examinee_id = "integercharacterNULL",
                   item_id = "character",
                   testlet_id = "characterNULL",
                   score = "numericNULL",
                   raw_response = "characterNULL",
                   order = "integerNULL",
                   response_time = "numericNULL",
                   misc = "listNULL")
         )


################################################################################
############################# initialize (Response) ###########################@
###########################################################################@####
#' @noRd
#' @title This function initializes the \code{Response} object.
#'
#' @importFrom methods callNextMethod
#'
#' @author Emre Gonulates
#'
setMethod("initialize", "Response",
          function(.Object,
                   examinee_id = NULL,
                   item_id = "Item_1",
                   testlet_id = NULL,
                   score = NULL,
                   raw_response = NULL,
                   order = NULL,
                   response_time = NULL,
                   misc = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@examinee_id <- examinee_id
  .Object@item_id <- item_id
  .Object@testlet_id <- testlet_id
  .Object@score <- score
  .Object@raw_response <- raw_response
  .Object@order <- order
  .Object@response_time <- response_time
  .Object@misc <- misc
  # Check validity of the object
  validObject(.Object)
  .Object
})



################################################################################
############################# setValidity (Response) ##########################@
###########################################################################@####
#' @noRd
#' @title This function sets the validity rules for \code{Response} object.
#'
#' @name Response-class::setValidity()
#'
#' @param Class The class of the object.
#'
setValidity(
  Class = "Response",
  function(object) {

    if (is.null(object@score) && is.null(object@raw_response))
      stop("Response object should have either a valid 'score' or ",
           "'raw_response'.")

    if (!is.null(object@score) &&
        (length(object@score) != length(object@item_id)) )
      stop("'item_id' and 'score' should have the same lengths.")

    if (!is.null(object@raw_response) &&
        (length(object@raw_response) != length(object@item_id)) )
      stop("'item_id' and 'raw_response' should have the same lengths.")

    ###################### Check 'examinee_id' #############################@###
    # Make sure the ID is a string with length 1 (or NULL)
    if (!is.null(object@examinee_id) && (length(object@examinee_id) != 1)) {
      stop("Invalid 'examinee_id'.", call. = FALSE)
    }

    ###################### Check 'item_id' #################################@###
    item_ids <- object@item_id
    if (!is.null(item_ids)) {
      test <- any(duplicated(item_ids))
      if (any(duplicated(item_ids)))
        stop("Invalid \"item_id\"s. \"item_id\"s cannot be duplicated.",
             call. = FALSE)
      # This variable will hold the slot name from which 'n_items' obtained
      indicator <- 'item_id'
      n_items <- length(object@item_id)
    } else n_items <- 0

    ###################### Check 'score' ###################################@###
    if (!is.null(object@score)) {
      if (n_items == 0) {
        n_items <- length(object@score)
        indicator <- 'score'
      } else if (length(object@score) != n_items) {
        stop(paste0("Invalid 'score' vector. The length of 'score' vector ",
                    "should equal to the length of '", indicator, "' vector."))
      }
      if (any(is.na(object@score)))
        stop("Invalid 'score' vector. Score values cannot be missing.")

    }
    ###################### Check 'raw_response' #############################@###
    if (!is.null(object@raw_response)) {
      if (n_items == 0) {
        n_items <- length(object@raw_response)
        indicator <- 'raw_response'
      } else if (length(object@raw_response) != n_items) {
        stop(paste0("Invalid 'raw_response' vector. The length of ",
                    "'raw_response' vector should equal to the length of '",
                    indicator, "' vector."))
      }
    }

    ###################### Check 'order' ###################################@###
    if (!is.null(object@order)) {
      if (n_items != 0 && length(object@order) != n_items)
        stop(paste0("Invalid 'order' vector. The length of 'order' ",
                    "vector should equal to the length of '", indicator,
                    "' vector."))

    # 2020-05-09: Disabled this feature because, there might be some items
    # (like testlet items) that can be administered at the same time and have
    # the same item order.
    # if (any(duplicated(object@order)))
    #   stop(paste0("Invalid 'order' vector. The values of 'order' vector ",
    #               "cannot be duplicated."))

      # This is not necessary because item order should be different in case
      # field test items or some other items omitted from Response object
      # if (!all(sort(object@order) == 1L:length(object@order)))
      #   stop(paste0("Invalid 'order' vector. The values of 'order' vector ",
      #               "should start from 1 and go up the the number of items."))

    }

    ###################### Check 'response_time' ###########################@###
    if (!is.null(object@response_time)) {
      if (n_items != 0 && length(object@response_time) != n_items)
        stop(paste0("Invalid 'response_time' vector. The length of ",
                    "'response_time' vector should equal to the length of '",
                    indicator, "' vector."))
      if (any(!is.na(object@response_time) & object@response_time < 0))
        stop(paste0("Invalid 'response_time' vector. The values of ",
                    "'response_time' vector should be larger than 0."))

    }
  }
)

