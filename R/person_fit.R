
###############################################################################@
############################# person_fit (generic) #############################
###############################################################################@
#' Calculate person-fit indices
#' @description
#' \code{person_fit} calculates the fit of a person to a given psychometric
#' model.
#'
#'
#' @param ip An \code{\link{Item-class}}, \code{\link{Itempool-class}} or a
#'   \code{\link{Testlet-class}} object.
#' @param resp A vector of item responses.
#' @param theta An vector containing ability parameters.
#' @param type The type of the person-fit index.
#'
#' @return A vector of person-fit index values.
#'
#' @include item-class.R
#' @include itempool-class.R
#' @include item-class-methods.R
#' @include itempool-class-methods.R
#' @include response_set-class.R
#'
#' @author Emre Gonulates
#'
#' @rdname person_fit
#'
setGeneric("person_fit", function(resp, ip, theta, type = "lz")
{standardGeneric("person_fit")})



###############################################################################@
############################# person_fit (Response_set) ########################
###############################################################################@
#' @export
#'
#' @rdname person_fit
#'
setMethod(
  f = "person_fit", signature = c(resp = "Response_set", ip = "Itempool"),
  function(resp, ip, theta, type = "lz"){
    if (type == "lz") {
      return(lz_response_set_cpp(resp, theta, ip))
    } else
      stop("This method has not been implemented yet.")
  }
)

###############################################################################@
############################# person_fit (Itempool) ############################
###############################################################################@
#' @export
#'
#' @rdname person_fit
#'
setMethod(
  f = "person_fit", signature = c(ip = "Itempool"),
  function(resp, ip, theta, type = "lz"){
    if (type == "lz") {
      resp <- response_set(resp, ip = ip)
      return(lz_response_set_cpp(resp, theta, ip))
    } else
      stop("This method has not been implemented yet.")
  }
)


###############################################################################@
############################# person_fit (Testlet) #############################
###############################################################################@
#' @export
#'
#' @rdname person_fit
#'
setMethod(
  f = "person_fit", signature = c(ip = "Testlet"),
  function(resp, ip, theta, type = "lz"){
    return(person_fit(ip = ip@item_list, resp = response_set(resp),
                      theta = theta, type = type))
  }
)



###############################################################################@
############################# cusum_single #####################################
###############################################################################@

# The function that calculates the C+ (Cp) and C- (Cn)
calculate_c <- function(t, method = "T1") {
  if (method %in% paste0("T", 1:8)) {
    n <- length(t)
    Cp  <- rep(0, n+1)
    Cn <- rep(0, n+1)
    for (i in 2:(n+1)) {
      Cp[i] <- max(0, t[i-1] + Cp[i-1])
      Cn[i] <- min(0, t[i-1] + Cn[i-1])
    }
    return(data.frame(Cp = Cp[-1], Cn = Cn[-1]))
  }
  return(NULL)
}


#' CUSUM based statistics for one examinee
#'
#' @param ip An itempool object
#' @param resp a response vector, where the order of items represent the
#'   administration order.
#' @param theta A vector or length 1 or length equal to the number of items
#'   administered.
#' @param method Method of calculating the CUSUM statistic. Choices are:
#'   \code{"T1"}, \code{"T2"}, \code{"T3"}, \code{"T4"}, \code{"T5"},
#'   \code{"T6"}, \code{"T7"} and \code{"T8"}. \code{"T1"} through \code{"T4"}
#'   uses the ability estimate each stage of the test. \code{"T5"} through
#'   \code{"T8"} uses the final ability estimate and needs only one theta
#'   estimate.
#' @param initial_theta_est For CAT, the initial theta estimate of an examinee.
#'   For CAT, if \code{theta = NULL}, this initial theta estimate will be used
#'   to calculate \eqn{T_1}, \eqn{C^-_1} and \eqn{C^+_1}. For CAT, this value
#'   should be provided. By this way for the calculation of \eqn{T_1},
#'   \eqn{C^-_1} and \eqn{C^+_1}, \eqn{\hat \theta_{n-1}} will be used.
#'
#'   If it's value is  \code{NULL} and \code{theta = NULL}, then for the
#'   calculation of \eqn{T_1}, \eqn{C^-_1}  and \eqn{C^+_1},
#'   \eqn{\hat \theta_{n}} will be used.
#'
#'   The default value is \code{NULL}
#'
#' @return The function will return a data frame consist of two columns:
#'   \code{Cp} column for \eqn{C^+} values and \code{Cn} column for \eqn{C^-}
#'   values.
#'
#' @references
#' van Krimpen-Stoop, E. M. L. A., & Meijer, R. R. (2000). Detecting
#'   person-misfit in adaptive testing using statistical process control
#'   techniques. In W. J. van der Linden & C. A. W. Glas (Eds.),
#'   Computerized adaptive testing: Theory and practice (pp. 210–219).
#'   Kluwer. https://doi.org/10.1007/0-306-47531-6_11
#'
#' Xiaofeng Yu & Ying Cheng (2020): A Comprehensive Review and Comparison of
#'   CUSUM and Change-Point-Analysis Methods to Detect Test Speededness,
#'   Multivariate Behavioral Research, DOI: 10.1080/00273171.2020.1809981
#'
#' @include item-class.R
#' @include itempool-class.R
#' @include item-class-methods.R
#' @include itempool-class-methods.R
#' @include response_set-class.R
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @examples
#' # Example from Table 1 (p.4) of Yu and Cheng (2020):
#' ip <- itempool(a = c(0.976, 0.973, 0.871, 0.768, 0.94, 1.109, 1.063, 0.888,
#'                      0.648, 0.733, 0.8, 0.823, 0.611, 0.965, 1.052, 0.937,
#'                      0.894, 0.72, 0.686, 0.608),
#'                b = c(-0.693, 0.6, -0.607, -0.637, -1.095, -0.202, -0.679,
#'                      0.058, -0.822, -0.768, -0.737, -1.158, -0.294, -0.856,
#'                      -0.833, -0.613, -0.151, -0.614, -0.07, -0.806),
#'                c = c(0.371, 0.224, 0.159, 0.377, 0.159, 0.146, 0.181, 0.251,
#'                      0.179, 0.214, 0.312, 0.224, 0.246, 0.225, 0.155, 0.166,
#'                      0.456, 0.327, 0.112, 0.169),
#'                D = 1.7)
#' resp <- c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1)
#' theta <- -0.06
#'
#' cusum_single(ip, resp, theta, method = "T5")
#'
cusum_single <- function(ip, resp, theta = NULL, method = "T1",
                         initial_theta_est = NULL) {
  ip <- itempool(flatten_itempool_cpp(ip))

  if (!all(ip$model %in% UNIDIM_DICHO_MODELS)) {
    stop(paste0(
      "This function is only available when all of the items in the item ",
      "pool are unidimensional dichotomous models: ",
      paste0("'", UNIDIM_DICHO_MODELS, "'", collapse = ", "), "."))
  }

  n <- ip$n$items
  if (inherits(resp, "Response")) {
    ip <- ip[resp@item_id]
    resp <- resp@score
  }
  if (!is.null(dim(resp)) && length(resp) != n)
    stop("'resp' should be a vector with the same length as the number of ",
         "items in the item pool.")


  # Set theta
  if (method %in% c("T1", "T2", "T3", "T4")) {
    # Set initial theta estimate
     if (is.null(theta)) {
       # Estimate ability after each administration of the item.
       theta <- sapply(1:(n), function(x)
         est_ability(ip = ip[1:x], resp = resp[1:x], method = "eap")$est)

       # Discard the final theta estimate, since the formulas uses
       # "theta_(k-1)" for CAT, otherwise (i.e. initial theta estimate is NULL),
       # use "theta_(k)"
       if (!is.null(initial_theta_est) &&
           length(initial_theta_est) == 1 &&
           is.numeric(initial_theta_est))
         theta <- c(initial_theta_est, theta[-length(theta)])
     } else if (!is.numeric(theta) || length(theta) != n) {
       stop(paste0("Invalid 'theta' argument. Please provide a numeric ",
                   "'theta' vector with length ", n, " where ",
                   "each element represent the ability estimate before the ",
                   "administration of the item."))
     }
  } else if (method %in% c("T5", "T6", "T7", "T8")) {
     if (is.null(theta)) {
       # Estimate ability at the end of the test.
       theta <- est_ability(ip = ip, resp = resp, method = "eap")$est
     } else if (!is.numeric(theta) || length(theta) != 1) {
       stop(paste0("Invalid 'theta' argument. Please provide a numeric ",
                   "'theta' value with length 1 where the value represent the",
                   "ability estimate at the end of the test."))
     }
  }

  if (method %in% c("T1", "T2", "T3", "T4")) {
    p <- stats::setNames(sapply(1:n, function(i)
      prob(ip = ip[[i]], theta = theta[i])[, 2]), ip$item_id)
    p <- ifelse(resp == 1, p, 1 - p)
    t1 <- (1/n) * (resp - p)
  } else if (method %in% c("T5", "T6", "T7", "T8")) {
    p <- prob(ip = ip, theta = theta)[, 2]
    p <- ifelse(resp == 1, p, 1 - p)
    t5 <- (1/n) * (resp - p)
  }

  switch(method,
    T1 = {
      t <- t1
      },
    T2 = {
      t <- t1 * (p * (1 - p))^(-0.5)
      },
    T3 = {
      # Calculate the test information function of a test containing the items
      # administered up to and including stage "k-1", evaluated at "theta_{k-1}"
      info <- sapply(1:n, function(i) info(ip = ip[1:i], theta = theta[i],
                                           tif = TRUE))
      t <- t1 * info^(-0.5)
      },
    T4 = {
      t <- sqrt(1:length(t1)) * t1
      },
    T5 = {
      t <- t5
    },
    T6 = {
      t <- t5 * (p * (1 - p))^(-0.5)
    },
    T7 = {
      # Calculate the test information function of a test containing the items
      # administered up to and including stage evaluated at the final estimated
      # ability
      info <- sapply(1:n, function(i) info(ip = ip[1:i], theta = theta,
                                           tif = TRUE))
      t <- t5 * info^(-0.5)
    },
    T8 = {
      t <- sqrt(1:length(t1)) * t5
    },
  )
  return(calculate_c(t, method = method))
}


