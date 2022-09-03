
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% prob %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


###############################################################################@
############################# prob (generic) ###################################
###############################################################################@
#' Calculate the probability of a correct response
#'
#' @description \code{prob} Returns the probability of correct respond to an
#'   item or multiple items with given parameters for a given ability or
#'   abilities, i.e. \eqn{\theta}. For polytomous models, where there are
#'   multiple possible responses, probability of each response category will be
#'   returned.
#'
#' @param ip An \code{\link{Item-class}}, or an \code{\link{Itempool-class}} or
#'  \code{\link{Testlet-class}}  object containing the item parameters.
#' @param theta An object containing the ability parameters.
#' @param derivative Whether to calculate the first or second derivative of
#'   probability of a response.
#'   \describe{
#'     \item{\code{0}}{No derivative will be calculated. This is the default
#'       value.}
#'     \item{\code{1}}{Calculate the first derivative.}
#'     \item{\code{2}}{Calculate the second derivative.}
#'   }
#'
#' @return Item probabilities at given theta will be returned.
#'
#'
#' @include item-class.R
#' @include itempool-class.R
#'
#' @author Emre Gonulates
#'
setGeneric("prob", function(ip, theta, derivative = 0)
  {standardGeneric("prob")})


###############################################################################@
############################# prob (Item) ######################################
###############################################################################@
#' @export
#' @useDynLib irt
#' @rdname prob
#'
setMethod(
  f = "prob", signature = c(ip = "Item"),
  function(ip, theta, derivative = 0){
    stop("This model has not been implemented in 'prob()' function yet.")
  }
)


############################# .prob_4pm_item ###################################
#' This function is general method for all 4PM models
#' @noRd
#'
.prob_4pm_item <- function(ip, theta, derivative = 0) {
  if (!derivative %in% 0:2)
    stop("Invalid 'derivative'. 'derivative' value can be 0, 1 or 2.")

  p <- prob_4pm_item_cpp(theta = theta, item = ip, derivative = derivative)
  if (derivative == 0) q <- 1 - p else q <- -p
  return(matrix(c(q, p), ncol = 2, dimnames = list(names(theta), 0:1)))
  # return(stats::setNames(prob_4pm_item_cpp(
  #   theta = theta, item = ip, derivative = derivative), ip@item_id))
}


############################# .prob_poly_item ##################################
#' This function is general method for GRM, PCM, GPCM, GPCM2 models
#' @noRd
#'
.prob_poly_item <- function(ip, theta, derivative = 0) {
  # Expected value can only be calculated when derivative = 0
  if (!derivative %in% 0:2)
    stop("Invalid 'derivative'. 'derivative' value can be 0, 1 or 2.")

  result <- sapply(theta, prob_poly_bare_cpp, item = ip,
                   derivative = derivative, resp = -9, expected_value = FALSE)
  result <- t(result)
  dimnames(result) <- list(names(theta), paste0(0:(ncol(result) - 1)))
  return(result)
}


###############################################################################@
############################# prob (Rasch) #####################################
###############################################################################@
#' @export
#' @useDynLib irt
#' @rdname prob
#'
#' @examples
#' theta <- rnorm(1)
#' item1 <- generate_item(model = "Rasch")
#'
#' # Probability of correct response
#' prob(item1, theta)
#'
#' # First derivative of probability of correct response:
#' prob(item1, theta, derivative = 1)
#'
#' # Second derivative of probability of correct response:
#' prob(item1, theta, derivative = 2)
#'
#'
#' # Multiple theta values
#' theta_n <- rnorm(5)
#'
#' prob(item1, theta_n)
#' prob(item1, theta_n, derivative = 1)
#' prob(item1, theta_n, derivative = 2)
#'
setMethod(
  f = "prob", signature = c(ip = "Rasch"),
  function(ip, theta, derivative = 0) {
    .prob_4pm_item(ip = ip, theta = theta, derivative = derivative)
  }
)

###############################################################################@
############################# prob (1PL) #######################################
###############################################################################@
#' @export
#' @useDynLib irt
#' @rdname prob
#'
#' @examples
#' theta <- rnorm(1)
#' item1 <- generate_item(model = "1PL")
#'
#' # Probability of correct response
#' prob(item1, theta)
#'
#' # First derivative of probability of correct response:
#' prob(item1, theta, derivative = 1)
#'
#' # Second derivative of probability of correct response:
#' prob(item1, theta, derivative = 2)
#'
#'
#' # Multiple theta values
#' theta_n <- rnorm(5)
#'
#' prob(item1, theta_n)
#' prob(item1, theta_n, derivative = 1)
#' prob(item1, theta_n, derivative = 2)
#'
setMethod(
  f = "prob", signature = c(ip = "1PL"),
  function(ip, theta, derivative = 0){
    .prob_4pm_item(ip = ip, theta = theta, derivative = derivative)
  }
)



###############################################################################@
############################# prob (2PL) #######################################
###############################################################################@
#' @export
#' @useDynLib irt
#' @rdname prob
#'
#' @examples
#' theta <- rnorm(1)
#' item1 <- generate_item(model = "2PL")
#'
#' # Probability of correct response
#' prob(item1, theta)
#'
#' # First derivative of probability of correct response:
#' prob(item1, theta, derivative = 1)
#'
#' # Second derivative of probability of correct response:
#' prob(item1, theta, derivative = 2)
#'
#'
#' # Multiple theta values
#' theta_n <- rnorm(5)
#'
#' prob(item1, theta_n)
#' prob(item1, theta_n, derivative = 1)
#' prob(item1, theta_n, derivative = 2)
#'
setMethod(
  f = "prob", signature = c(ip = "2PL"),
  function(ip, theta, derivative = 0){
    .prob_4pm_item(ip = ip, theta = theta, derivative = derivative)
  }
)


###############################################################################@
############################# prob (3PL) #######################################
###############################################################################@
#' @export
#' @useDynLib irt
#' @rdname prob
#'
#' @examples
#' theta <- rnorm(1)
#' item1 <- generate_item(model = "3PL")
#'
#' # Probability of correct response
#' prob(item1, theta)
#'
#' # First derivative of probability of correct response:
#' prob(item1, theta, derivative = 1)
#'
#' # Second derivative of probability of correct response:
#' prob(item1, theta, derivative = 2)
#'
#'
#' # Multiple theta values
#' theta_n <- rnorm(5)
#'
#' prob(item1, theta_n)
#' prob(item1, theta_n, derivative = 1)
#' prob(item1, theta_n, derivative = 2)
#'
setMethod(
  f = "prob", signature = c(ip = "3PL"),
  function(ip, theta, derivative = 0){
    .prob_4pm_item(ip = ip, theta = theta, derivative = derivative)
  }
)

###############################################################################@
############################# prob (4PL) #######################################
###############################################################################@
#' @export
#' @useDynLib irt
#' @rdname prob
#'
#' @examples
#' theta <- rnorm(1)
#' item1 <- generate_item(model = "4PL")
#'
#' # Probability of correct response
#' prob(item1, theta)
#'
#' # First derivative of probability of correct response:
#' prob(item1, theta, derivative = 1)
#'
#' # Second derivative of probability of correct response:
#' prob(item1, theta, derivative = 2)
#'
#'
#' # Multiple theta values
#' theta_n <- rnorm(5)
#'
#' prob(item1, theta_n)
#' prob(item1, theta_n, derivative = 1)
#' prob(item1, theta_n, derivative = 2)
#'
setMethod(
  f = "prob", signature = c(ip = "4PL"),
  function(ip, theta, derivative = 0){
    .prob_4pm_item(ip = ip, theta = theta, derivative = derivative)
  }
)


###############################################################################@
############################# prob (GRM) #######################################
###############################################################################@
#' @export
#' @useDynLib irt
#' @rdname prob
#'
#' @examples
#' theta <- rnorm(1)
#' item1 <- generate_item(model = "GRM")
#'
#' # Probability of correct response
#' prob(item1, theta)
#'
#' # First derivative of probability of correct response:
#' prob(item1, theta, derivative = 1)
#'
#' # Multiple theta values
#' theta_n <- rnorm(5)
#'
#' prob(item1, theta_n)
#' prob(item1, theta_n, derivative = 1)
#'
#'
#' item4 <- generate_item(model = "GRM", n_categories = 5)
#' prob(item4, theta)
#'
setMethod(
  f = "prob", signature = c(ip = "GRM"),
  function(ip, theta, derivative = 0){
    .prob_poly_item(ip = ip, theta = theta, derivative = derivative)
  }
)

###############################################################################@
############################# prob (PCM) #######################################
###############################################################################@
#' @export
#' @useDynLib irt
#' @rdname prob
#'
#' @examples
#' # Partial Credit Model
#' theta <- rnorm(1)
#' item1 <- generate_item(model = "PCM")
#'
#' # Probability of correct response
#' prob(item1, theta)
#'
#' # First derivative of probability of correct response:
#' prob(item1, theta, derivative = 1)
#'
#' # Second derivative of probability of correct response:
#' prob(item1, theta, derivative = 2)
#'
#'
#' # Multiple theta values
#' theta_n <- rnorm(5)
#'
#' prob(item1, theta_n)
#' prob(item1, theta_n, derivative = 1)
#' prob(item1, theta_n, derivative = 2)
#'
#'
#' item3 <- generate_item(model = "GPCM2", n_categories = 3)
#' prob(item3, theta)
#'
setMethod(
  f = "prob", signature = c(ip = "PCM"),
  function(ip, theta, derivative = 0){
    .prob_poly_item(ip = ip, theta = theta, derivative = derivative)
  }
)



###############################################################################@
############################# prob (GPCM) ######################################
###############################################################################@
#' @export
#' @useDynLib irt
#' @rdname prob
#'
#' @examples
#' theta <- rnorm(1)
#' item1 <- generate_item(model = "GPCM")
#'
#' # Probability of correct response
#' prob(item1, theta)
#'
#' # First derivative of probability of correct response:
#' prob(item1, theta, derivative = 1)
#'
#' # Second derivative of probability of correct response:
#' prob(item1, theta, derivative = 2)
#'
#'
#' # Multiple theta values
#' theta_n <- rnorm(5)
#'
#' prob(item1, theta_n)
#' prob(item1, theta_n, derivative = 1)
#' prob(item1, theta_n, derivative = 2)
#'
#' # Probability of each response category for Generalized Partial Credit Model
#' item2 <- generate_item(model = "GPCM", n_categories = 4)
#' prob(item2, theta)
#'
#' # First derivative of each response category
#' prob(item2, theta, derivative = 1)
#'
#' # Second derivative of each response category
#' prob(item2, theta, derivative = 2)
#'
setMethod(
  f = "prob", signature = c(ip = "GPCM"),
  function(ip, theta, derivative = 0){
    .prob_poly_item(ip = ip, theta = theta, derivative = derivative)
  }
)



###############################################################################@
############################# prob (GPCM2) #####################################
###############################################################################@
#' @export
#' @useDynLib irt
#' @rdname prob
#'
#' @examples
#' theta <- rnorm(1)
#' item1 <- generate_item(model = "GPCM2")
#'
#' # Probability of correct response
#' prob(item1, theta)
#'
#' # First derivative of probability of correct response:
#' prob(item1, theta, derivative = 1)
#'
#' # Second derivative of probability of correct response:
#' prob(item1, theta, derivative = 2)
#'
#'
#' # Multiple theta values
#' theta_n <- rnorm(5)
#'
#' prob(item1, theta_n)
#' prob(item1, theta_n, derivative = 1)
#' prob(item1, theta_n, derivative = 2)
#'
setMethod(
  f = "prob", signature = c(ip = "GPCM2"),
  function(ip, theta, derivative = 0){
    .prob_poly_item(ip = ip, theta = theta, derivative = derivative)
  }
)



###############################################################################@
############################# prob (Itempool) ##################################
###############################################################################@
#' @export
#'
#' @rdname prob
#'
#' @examples
#'
#' theta <- rnorm(1)
#' ip <- generate_ip(model = "3PL")
#'
#' # Probability of correct response
#' prob(ip, theta)
#'
#' # First derivative of probability of correct response:
#' prob(ip, theta, derivative = 1)
#'
#' # Second derivative of probability of correct response:
#' prob(ip, theta, derivative = 2)
#'
#' # Multiple theta
#' theta_n <- rnorm(3)
#' prob(ip, theta_n)
#' prob(ip, theta_n, derivative = 1)
#' prob(ip, theta_n, derivative = 2)
#'
#' # Extract probabilities of correct response (i.e. response is "1")
#' sapply(prob(ip, theta_n), `[`, TRUE, "1")
#' # Probabilities of incorrect response
#' sapply(prob(ip, theta_n), `[`, TRUE, "0")
#'
#' # Probability of each response category for Generalized Partial Credit Model
#' ip <- generate_ip(model = "GPCM", n = 4, n_categories = c(3, 4, 6, 5))
#' prob(ip, theta)
#'
#' # First derivative of each response category
#' prob(ip, theta, derivative = 1)
#'
#' # Second derivative of each response category
#' prob(ip, theta, derivative = 2)
#'
#' # Probability of a mixture of items models
#' ip <- generate_ip(model = c("GPCM", "2PL", "3PL", "GPCM"),
#'                   n_categories = c(4, 2, 2, 3))
#' prob(ip, theta)
#'
#' # Multiple theta
#' prob(ip, theta_n)
#'
#' # Extract probabilities of score "2" for each theta value
#' sapply(prob(ip, theta_n), `[`, TRUE, "2")
#'
setMethod(
  f = "prob", signature = c(ip = "Itempool"),
  function(ip,  theta, derivative = 0){
    # Expected value can only be calculated when derivative = 0
    if (!derivative %in% 0:2)
      stop("Invalid 'derivative'. 'derivative' value can be 0, 1 or 2.")

    result <- lapply(theta, prob_bare_itempool_cpp, ip = ip,
                     derivative = derivative, expected_value = FALSE)
    names(result) <- names(theta)
    if (length(result) == 1) result <- result[[1]]
    # # If the Model is one of the following: "1PM" "irt2PM" "irt3PM" "irt4PM"
    # if (all(ip$model %in% UNIDIM_DICHO_MODELS)) {
    #   result <- prob_4pm_itempool_cpp(theta = theta, ip = ip,
    #                                   derivative = derivative)
    #   dimnames(result) <- list(names(theta), ip$resp_id)
    # } else if (all(ip$model %in% names(PMODELS)[
    #   sapply(PMODELS, function(x) x$model_family == "MIRT")])) {
    #   return(prob_mirt_itempool_cpp(theta = theta, ip = ip,
    #                                 derivative = derivative))
    # } else if (length(theta) == 1) {
    #
    #   # result <- prob_bare_itempool_cpp(theta, ip, derivative, FALSE)
    #   # dimnames(result) <- list(ip$resp_id, paste0(0:(ncol(result) - 1)))
    # } else
    #   stop("prob() function cannot be run on item sets that are composed of ",
    #        "unidimensional and multidimensional items or item sets with ",
    #        "polytomous items with more than one theta. This model has not ",
    #        "been implemented in 'prob()' function yet.")
    return(result)
  }
)


###############################################################################@
############################# prob (Testlet) ###################################
###############################################################################@
#' @export
#'
#' @rdname prob
#'
#' @examples
#' theta <- rnorm(1)
#' t1 <- generate_testlet(model_items = "3PL")
#'
#' # Probability of correct response
#' prob(t1, theta)
#'
#' # First derivative of probability of correct response:
#' prob(t1, theta, derivative = 1)
#'
#' # Second derivative of probability of correct response:
#' prob(t1, theta, derivative = 2)
#'
setMethod(
  f = "prob", signature = c(ip = "Testlet"),
  function(ip,  theta, derivative = 0){
    return(prob(ip = ip@item_list, theta = theta, derivative = derivative))
  }
)


###############################################################################@
############################# prob (REST) ######################################
###############################################################################@
#' @export
#'
#' @rdname prob
setMethod(
  f = "prob", signature = c(ip = "numMatDfListChar"),
  function(ip, theta, derivative = 0) {
    if (inherits(ip, "numeric")) {
      return(prob(ip = itempool(ip), theta = theta, derivative = derivative))
    } else if (inherits(ip, c("data.frame", "matrix", "list"))) {
      return(prob(ip = itempool(ip), theta = theta, derivative = derivative))
    } else {
      stop("Cannot convert object to an 'Item' or an 'Itempool' object. ",
           "Please provide a valid 'Item' or 'Itempool' object using either ",
           "'item()' or 'itempool()' function.")
    }
  }
)

