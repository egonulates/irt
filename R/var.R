
###############################################################################@
############################# var (generic) ####################################
###############################################################################@
setGeneric("var",
           function(x, y = NULL, na.rm = FALSE, use) standardGeneric("var"))



###############################################################################@
############################# var (Item) ######################################
###############################################################################@
#' Calculate the variance of an Item
#'
#' @description \code{var} Returns the variance of  an item or multiple items
#'   with given parameters for a given ability or abilities, i.e. \eqn{\theta}.
#'
#' @param x An \code{\link{Item-class}} or an \code{\link{Itempool-class}}
#'   object containing the item parameters.
#' @param y A numeric vector containing the ability parameters (i.e. theta).
#' @param na.rm Ignored for \code{var(Item, ...)}
#' @param use Ignored for \code{var(Item, ...)}
#'
#' @return Item variances at given theta will be returned.
#'
#' @include item-class.R
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @rdname var.Item
#'
setMethod(
  f = "var", signature = c(x = "Item"),
  function(x, y = NULL, na.rm = FALSE, use){
    stop("This model has not been implemented in 'var()' function yet.")
  }
)

############################# .var_4pm_item ####################################
#' This function is general method for all 4PM models
#' @noRd
#'
.var_4pm_item <- function(ip, theta) {
  if (!is.numeric(theta) || is.matrix(theta) || !is.atomic(theta)) {
    stop("Invalid 'theta' argument. 'theta' represents theta values and ",
         "it should be numeric vector.")
  }
  # Expected value can only be calculated when derivative = 0
  p <- prob_4pm_item_cpp(theta = theta, item = ip)
  return(p * (1 - p))
}


############################# .var_poly_item ####################################
#' This function is general method for all 4PM models
#' @noRd
#'
.var_poly_item <- function(ip, theta) {
  if (!is.numeric(theta) || is.matrix(theta) || !is.atomic(theta)) {
    stop("Invalid 'theta' argument. 'theta' represents theta values and ",
         "it should be numeric vector.")
  }
  # Expected value can only be calculated when derivative = 0
  p <- prob(ip = ip, theta = theta)[, -1]
  max_score <- ip$max_score
  rowSums(p * matrix((1:max_score)^2, nrow = length(theta),
                     ncol = max_score, byrow = TRUE)) - mean(ip, theta)
}




###############################################################################@
############################# var (Rasch) ######################################
###############################################################################@
#' @export
#' @rdname var.Item
#'
setMethod(
  f = "var", signature = c(x = "Rasch"),
  function(x, y = NULL, na.rm = FALSE, use) {.var_4pm_item(ip = x, theta = y)})

###############################################################################@
############################# var (1PL) ########################################
###############################################################################@
#' @export
#' @rdname var.Item
#'
setMethod(
  f = "var", signature = c(x = "1PL"),
  function(x, y = NULL, na.rm = FALSE, use) {.var_4pm_item(ip = x, theta = y)})

###############################################################################@
############################# var (2PL) ########################################
###############################################################################@
#' @export
#' @rdname var.Item
#'
setMethod(
  f = "var", signature = c(x = "2PL"),
  function(x, y = NULL, na.rm = FALSE, use) {.var_4pm_item(ip = x, theta = y)})


###############################################################################@
############################# var (3PL) ########################################
###############################################################################@
#' @export
#' @rdname var.Item
#'
setMethod(
  f = "var", signature = c(x = "3PL"),
  function(x, y = NULL, na.rm = FALSE, use) {.var_4pm_item(ip = x, theta = y)})


###############################################################################@
############################# var (4PL) ########################################
###############################################################################@
#' @export
#' @rdname var.Item
#'
setMethod(
  f = "var", signature = c(x = "4PL"),
  function(x, y = NULL, na.rm = FALSE, use) {.var_4pm_item(ip = x, theta = y)})


###############################################################################@
############################# var (GRM) ########################################
###############################################################################@
#' @export
#' @rdname var.Item
#'
setMethod(
  f = "var", signature = c(x = "GRM"),
  function(x, y = NULL, na.rm = FALSE, use) {.var_poly_item(ip = x, theta = y)})


###############################################################################@
############################# var (PCM) ########################################
###############################################################################@
#' @export
#' @rdname var.Item
#'
setMethod(
  f = "var", signature = c(x = "PCM"),
  function(x, y = NULL, na.rm = FALSE, use) {.var_poly_item(ip = x, theta = y)})


###############################################################################@
############################# var (GPCM) #######################################
###############################################################################@
#' @export
#' @rdname var.Item
#'
setMethod(
  f = "var", signature = c(x = "GPCM"),
  function(x, y = NULL, na.rm = FALSE, use) {.var_poly_item(ip = x, theta = y)})




###############################################################################@
############################# var (GPCM2) ######################################
###############################################################################@
#' @export
#' @rdname var.Item
#'
setMethod(
  f = "var", signature = c(x = "GPCM2"),
  function(x, y = NULL, na.rm = FALSE, use) {.var_poly_item(ip = x, theta = y)})


###############################################################################@
############################# var (Itempool) ###################################
###############################################################################@
#' Calculate the variances of items in an Itempool
#'
#' @description \code{var} Returns the variance of each item of an
#'   \code{\link{Itempool-class}} object for a given ability or abilities,
#'   i.e. \eqn{\theta}.
#'
#' @param x An \code{\link{Itempool-class}} object containing the item
#'   parameters.
#' @param y A numeric vector containing the ability parameters (i.e. theta).
#' @param na.rm Ignored for \code{var(Itempool, ...)}
#' @param use Ignored for \code{var(Itempool, ...)}
#'
#' @return Item variances at given theta will be returned.
#'
#' @include item-class.R
#' @include itempool-class.R
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @rdname var.Itempool
#'
setMethod(
  f = "var", signature = c(x = "Itempool"),
  function(x, y = NULL, na.rm = FALSE, use) {
    result <- sapply(x$item_list, function(i) var(x = i, y = y))
    if (inherits(result, "matrix")) {
      colnames(result) <- x$id
      rownames(result) <- names(y)
    } else {
      names(result) <- x$id
    }
    return(result)
  }
)


###############################################################################@
############################# var (Testlet) ####################################
###############################################################################@
#' Calculate the variances of items in a Testlet
#'
#' @param x An \code{\link{Testlet-class}} object containing the item
#'   parameters of the testlet.
#' @param y A numeric vector containing the ability parameters (i.e. theta).
#' @param na.rm Ignored for \code{var(Testlet, ...)}
#' @param use Ignored for \code{var(Testlet, ...)}
#'
#' @return Item variances at given theta will be returned.
#'
#' @include item-class.R
#' @include itempool-class.R
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @rdname var.Testlet
#'
setMethod(
  f = "var", signature = c(x = "Testlet"),
  function(x, y = NULL, na.rm = FALSE, use) {
    var(x@item_list, y = y)
  }
)



