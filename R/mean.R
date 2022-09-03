
###############################################################################@
############################# mean (generic) ###################################
###############################################################################@
setGeneric("mean",
           function(x, ...) standardGeneric("mean"))

###############################################################################@
############################# mean (Item) ######################################
###############################################################################@
#' Calculate the expected value of an Item
#'
#' @description \code{mean} Returns the expected value of an item for given
#'   parameters for a given ability or abilities, i.e. \eqn{\theta}.
#'
#' @param x An \code{\link{Item-class}} object containing the item parameters.
#' @param ... Additional parameters. Specifically \code{theta} argument is
#'   required. \code{theta} should be a numeric vector of ability parameters.
#'
#' @return Item expected values at given theta(s) values will be returned.
#'
#' @include item-class.R
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @rdname mean.Item
#'
#' @examples
#' itm <- generate_item(model = "Rasch")
#' mean(itm, theta = 1)
#' mean(itm, -1.2)
#'
#' itm <- generate_item(model = "GPCM", n_categories = 5)
#' mean(itm, theta = 1.5)
#' mean(itm, 0.2)
setMethod(
  f = "mean", signature = c(x = "Item"),
  function(x, ...){
    stop("This model has not been implemented in 'mean()' function yet.")
  }
)

############################# .mean_item ######################################
#' This function is general method for all 4PM models and GPCM, PCM, GRM, GPCM2
#' @noRd
#'
.mean_item <- function(ip, ...) {
  args <- list(...)
  theta <- args[["theta"]]
  if (is.null(theta) && length(args) > 0 && is.numeric(args[[1]]) &&
      !is.matrix(args[[1]]) && is.atomic(args[[1]])) {
    theta <- args[[1]]
  } else if (!is.numeric(theta) || is.matrix(theta) || !is.atomic(theta))
    stop("A valid 'theta' argument should be provided like ",
         "`mean(x = ..., theta = ...)`. 'theta' represents ability values and ",
         "it should be numeric vector.")
  return(sapply(theta, prob_bare_item_cpp, item = ip, derivative = 0, resp = -9,
                expected_value = TRUE))
}


###############################################################################@
############################# mean (Rasch) #####################################
###############################################################################@
#' @export
#' @rdname mean.Item
#'
setMethod(
  f = "mean", signature = c(x = "Rasch"),
  function(x, ...) {.mean_item(ip = x, ...)})

###############################################################################@
############################# mean (1PL) #######################################
###############################################################################@
#' @export
#' @rdname mean.Item
#'
setMethod(
  f = "mean", signature = c(x = "1PL"),
  function(x, ...) {.mean_item(ip = x, ...)})

###############################################################################@
############################# mean (2PL) #######################################
###############################################################################@
#' @export
#' @rdname mean.Item
#'
setMethod(
  f = "mean", signature = c(x = "2PL"),
  function(x, ...) {.mean_item(ip = x, ...)})


###############################################################################@
############################# mean (3PL) #######################################
###############################################################################@
#' @export
#' @rdname mean.Item
#'
setMethod(
  f = "mean", signature = c(x = "3PL"),
  function(x, ...) {.mean_item(ip = x, ...)})


###############################################################################@
############################# mean (4PL) #######################################
###############################################################################@
#' @export
#' @rdname mean.Item
#'
setMethod(
  f = "mean", signature = c(x = "4PL"),
  function(x, ...) {.mean_item(ip = x, ...)})

###############################################################################@
############################# mean (GPCM) ######################################
###############################################################################@
#' @export
#' @rdname mean.Item
#'
setMethod(
  f = "mean", signature = c(x = "GPCM"),
  function(x, ...) {.mean_item(ip = x, ...)})

###############################################################################@
############################# mean (GPCM2) #####################################
###############################################################################@
#' @export
#' @rdname mean.Item
#'
setMethod(
  f = "mean", signature = c(x = "GPCM2"),
  function(x, ...) {.mean_item(ip = x, ...)})

###############################################################################@
############################# mean (GRM) #######################################
###############################################################################@
#' @export
#' @rdname mean.Item
#'
setMethod(
  f = "mean", signature = c(x = "GRM"),
  function(x, ...) {.mean_item(ip = x, ...)})


###############################################################################@
############################# mean (PCM) #######################################
###############################################################################@
#' @export
#' @rdname mean.Item
#'
setMethod(
  f = "mean", signature = c(x = "PCM"),
  function(x, ...) {.mean_item(ip = x, ...)})




###############################################################################@
############################# mean (Itempool) ##################################
###############################################################################@
#' Calculate the expected value of an Itempool
#'
#' @description \code{mean} Returns the expected values of each item in an
#'   \code{\link{Itempool-class}} object for a given ability or abilities,
#'   i.e. \eqn{\theta}.
#'
#' @param x An \code{\link{Itempool-class}} object containing the item
#'   parameters.
#' @param ... Additional parameters. Specifically \code{theta} argument is
#'   required. \code{theta} should be a numeric vector of ability parameters.
#'
#' @return Item expected values at given theta values will be returned.
#'
#' @include item-class.R
#' @include itempool-class.R
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @rdname mean.Itempool
#'
#' @examples
#' ip <- generate_ip(model = "2PL")
#' mean(ip, theta = 1.2)
#' mean(ip, 1.2)
#'
#' ip <- generate_ip(model = "GPCM")
#' mean(ip, theta = -0.37)
#' mean(ip, -1.55)
setMethod(
  f = "mean", signature = c(x = "Itempool"),
  function(x, ...){
    result <- sapply(x$item_list, function(i) mean(x = i, ...))
    if (inherits(result, "matrix")) {
      colnames(result) <- x$item_id
      # rownames(result) <- names(theta)
    } else {
      names(result) <- x$id
    }
    return(result)
  }
)



###############################################################################@
############################# mean (Testlet) ###################################
###############################################################################@
#' Calculate the expected value of an Testlet
#'
#' @description \code{mean} Returns the expected values of each item in an
#'   \code{\link{Testlet-class}} object for a given ability or abilities,
#'   i.e. \eqn{\theta}.
#'
#' @param x A \code{\link{Testlet-class}} object containing the item
#'   parameters.
#' @param ... Additional parameters. Specifically \code{theta} argument is
#'   required. \code{theta} should be a numeric vector of ability parameters.
#'
#' @return Item expected values at given theta values will be returned.
#'
#' @include item-class.R
#' @include itempool-class.R
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @rdname mean.Testlet
#'
#' @examples
#' t1 <- generate_testlet()
#' mean(t1, theta = -1.1)
#' mean(t1, -1.1)
#'
setMethod(
  f = "mean", signature = c(x = "Testlet"),
  function(x, ...){
    mean(x@item_list, ...)
  }
)
