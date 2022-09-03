
setClassUnion(name = "listNULL", members = c("list","NULL"))
setClassUnion(name = "ItempoolNULL", members = c("Itempool","NULL"))


################################################################################
############################# Response_set class ##############################@
###########################################################################@####
#' An S4 class representing responses of a set of examinees
#'
#' @slot response_list A list of \code{\link{Response-class}} objects. If
#'   the \code{examinee_id} slots of \code{\link{Response-class}} objects are
#'   not \code{NULL}, there cannot be duplicates.
#' @slot item_id A character vector of \code{Item} ID's in the
#'  \code{\link{Response-class}} objects. The order of this item_id will be
#'  used when converting \code{\link{Response_set-class}} objects to a matrix.
#' @slot testlet_id A character vector of \code{Testlet} ID's in the
#'  \code{\link{Response-class}} objects.
#' @slot misc This slot will hold any other information about the response
#'   set.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#'
setClass(Class = "Response_set",
         slots = c(response_list = "list",
                   item_id = "character",
                   testlet_id = "characterNULL",
                   misc = "listNULL"),
         )


################################################################################
############################# initialize (Response_set) #######################@
###########################################################################@####
#' @noRd
#' @title This function initializes the \code{\link{Response-class}} object.
#'
#' @importFrom methods callNextMethod
#'
#' @author Emre Gonulates
#'
setMethod("initialize", "Response_set",
          function(.Object,
                   response_list = list(),
                   item_id = character(),
                   testlet_id = NULL,
                   misc = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@response_list <- response_list
  .Object@item_id <- item_id
  .Object@testlet_id <- testlet_id
  .Object@misc <- misc
  # Check validity of the object
  validObject(.Object)
  .Object
})



################################################################################
############################# setValidity (Response_set) ######################@
###########################################################################@####
#' @noRd
#' @title This function sets the validity rules for \code{Response_set} object.
#'
#' @name Response_set-class::setValidity()
#'
#' @param Class The class of the object.
#'
setValidity(
  Class = "Response_set",
  function(object) {
    # ###################### Check 'ip' ####################################@###
    #

    # ###################### Check 'response_list' #########################@###
    response_list <- object@response_list
    # All of the elements of response_list should be a Response set object
    if (!all(sapply(response_list, is, "Response")) ||
        length(response_list) == 0)
      stop("Invalid 'response_list'. All of the elements of 'response_list' ",
           "list should be a Response class object.")

    # Check whether all of the examinee_id's are unique
    examinee_ids <- unlist(sapply(response_list, function(x) x@examinee_id))
    if (any(duplicated(examinee_ids)))
      stop(paste0("Invalid 'response_list'. The 'examinee_id's of the ",
                  "response object cannot be duplicated. The following ",
                  "'examinee_id' are duplicated: \n",
                  paste0("'", examinee_ids[duplicated(examinee_ids)], "'",
                         collapse = ", ")), call. = FALSE)

    item_ids <- unique(do.call("c",
                               lapply(response_list, function(x) x@item_id)))
    if (length(object@item_id) < length(item_ids) ||
        !all(item_ids %in% object@item_id))
      stop("Invalid 'item_id' slot. All of the Response item_id's should ",
           "be in 'item_id' slot.")
    # All of the item_id's of responses should be within 'item_id' slot of

    # Response_set object.
      # # All of the item_ids should be within the item_ids' of the item pool.
      # if (!is.null(object@ip)) {
      #   ip <- object@ip
      #   # lapply is used instead of sapply because item numbers may differ
      #   # between examinees.
      #   item_ids <- lapply(response_list, function(x) x@item_id)
      #   ip_resp_ids <- ip$resp_id
      #   if (!all(sapply(item_ids, function(x) all(x %in% ip_resp_ids))))
      #     stop(paste0("Invalid 'response_list'. All of the items ID's within ",
      #                 "the responses should match the item ID's of the item ",
      #                 "pool object."), call. = FALSE)
      # }



    # ###################### Check 'misc' ###########################@###
  }
)


