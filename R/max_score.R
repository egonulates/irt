

###############################################################################@
############################# max_score (generic) ##############################
###############################################################################@
#' Calculate the maximum possible score
#'
#' @param ip An \code{\link{Item-class}} or an \code{\link{Itempool-class}}
#'   object containing the item parameters.
#' @param resp A \code{\link{Response-class}} or
#'   \code{\link{Response_set-class}} object.
#' @param sum If \code{TRUE}, when \code{ip} is an \code{\link{Itempool-class}}
#'   object the individual maximum possible scores of items will be summed.
#'   This argument will be ignored when \code{resp} is not \code{NULL}.
#'
#' @return Maximum possible score of each item
#'
#' @include item-class.R
#' @include itempool-class.R
#' @include response-class.R
#' @include response_set-class.R
#'
#' @author Emre Gonulates
#'
setGeneric("max_score", function(ip, resp = NULL, sum = TRUE)
  {standardGeneric("max_score")})


###############################################################################@
############################# max_score (Item) #################################
###############################################################################@
#' @export
#' @rdname max_score
#'
setMethod(
  f = "max_score", signature = c(ip = "Item"),
  function(ip, resp = NULL, sum = TRUE) {
    get_max_possible_score_item_cpp(ip)
  }
)


###############################################################################@
############################# max_score (Itempool) #############################
###############################################################################@
#' @export
#' @rdname max_score
#'
setMethod(
  f = "max_score", signature = c(ip = "Itempool"),
  function(ip, resp = NULL, sum = TRUE) {
    if (!is.null(resp)) {
      if (is(resp, "Response")) {
        resp <- response_set(resp)
      } else if (!is(resp, "Response_set")) {
        resp <- tryCatch({
          response_set(resp, ip = ip)
          }, warning = function(w) {
            warning(w, call. = FALSE)
          }, error = function(e) {
            stop(paste0("Invalid 'resp'. 'resp' cannot be converted to a ",
                        "Response_set object."), call. = FALSE)
          })
      }
    }

    if (is.null(resp)) {
      result <- get_max_possible_score_itempool_cpp(ip)
      if (sum) result <- sum(result)
    } else if (is(resp, "Response_set")) {
      result <- max_score_response_set_cpp(resp_set = resp, ip = ip)
    } else {
      stop(paste0("Invalid 'resp'. "))
    }
    return(result)
  }
)
