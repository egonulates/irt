

#' Item Characteristic Curve Estimation using Kernel Smoothing
#'
#' @param resp A response matrix where each row is the responses of an examinee
#'   and each column represents an item.
#'
#'   \code{resp} does not necessarily be a matrix. It can be \code{data.frame}
#'   or any other object that can be convertible to matrix using
#'   \code{as.matrix} function.
#'
#'   \code{resp} can contain missing responses.
#'
#' @param h The bandwidth parameter that controls the amount of smoothing.
#'   A small value will decrease the bias whereas increase the sampling
#'   variability. For a standard normally distributed \code{criterion} and
#'   Gaussian kernel smoothing function,
#'   \eqn{h = 0.2} is recommended for large sample sizes (like 3000),
#'   \eqn{h = 0.3} is recommended for medium sample sizes (like 500), and
#'   \eqn{h = 0.4} is recommended for small sample sizes (like 100), and
#'
#'   The default value is \eqn{1.06  \sigma(criterion) n_{examinees}^{-1/5}}.
#'
#' @param kernel_func Choice of kernel function. Possible choices are:
#'
#'   \itemize{
#'     \item{"gauss"}{Gaussian kernel. \eqn{f(x) = e^{-u^2/2}}.}
#'     \item{"unif"}{Uniform kernel. \eqn{f(x) = 0.5, |u| < 0.5}, else 0.}
#'     \item{"quadratic"}{Quadratic kernel. \eqn{f(x) = 0.75(1-u^2), |u| < 1},
#'          else 0.}
#'     \item{Custom Function}{You can provide a custom kernel function object.
#'       The function should be maximum at \eqn{u = 0} and gets closer to 0
#'       on either side.}
#'   }
#'
#'   The default value is \code{"gauss"}, i.e. Gaussian kernel function.
#'
#' @param criterion The ability estimates for each examinee. The default is
#'   \code{NULL} where the abilities will be estimated from the sum scores.
#'   First sum scores will be calculated, then the rank of each examinee's
#'   sum score will be calculated. These ranks will be divided by the number
#'   of examinees plus 1 in order to get values between 0 and 1. Finally, these
#'   values will be put on standard normal scale (by inverse CDF).
#'
#' @param points The points at which the item characteristic curve will be
#'   calculated. The default value is \code{points = seq(-3, 3, 0.05)}.
#'
#'
#' @return A \code{list} with following elements will be returned:
#'   \itemize{
#'     \item{\code{points}}{The quadrature points at which ICC is calculated.}
#'     \item{\code{icc}}{A matrix where each cell represents probability of
#'       selecting a response (for dichotomous models, probability of correct
#'       response). Items are on columns and quadrature points are on rows.}
#'     \item{\code{se}}{A matrix of standard errors of each point of
#'       \code{icc}. This matrix has the same dimension as \code{icc}.}
#'     \item{\code{criterion}}{The criterion values used for examinees. If
#'       \code{criterion = NULL} these numbers will be based on sum scores.}
#'     \item{\code{h}}{The bandwidth parameter.}
#'   }
#'
#'
#' @importFrom stats approx qnorm sd
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @examples
#' ip <- generate_ip(model = "3PL", n = 50)
#' true_theta <- rnorm(10000)
#' resp <- sim_resp(ip = ip, theta = true_theta, prop_missing = 0.3)
#'
#' kern_output <- ks(resp)
#'
#' # Plot ICC
#' i <- 12 # select an item to plot
#' x <- kern_output$icc[, i]
#' se <- kern_output$se[, i]
#' p <- prob(ip = ip[i], theta = kern_output$points)
#' p <- sapply(p, `[`, 2) # get the probability of correct responses
#'
#' graph_data <- data.frame(
#'   theta = kern_output$points,
#'   icc = x,
#'   ci_low = sapply(x - qnorm(.975) * se, function(x) max(x, 0)),
#'   ci_high = sapply(x + qnorm(.975) * se, function(x) min(x, 1)),
#'   p = p)
#'
#' \dontrun{
#' p <- ggplot(data = graph_data) +
#'   geom_line(aes(x = theta, y = icc), color = "blue", alpha = .7, size = 1) +
#'   geom_line(aes(x = theta, y = p), color = "red", size = 1, alpha = .7) +
#'   geom_ribbon(data = graph_data,
#'               aes(x = theta, ymin = ci_low, ymax = ci_high),
#'               alpha = .25) +
#'   ylim(0, 1) +
#'   labs(x = "Theta", y = "Probability",
#'        title = "Item Characteristic Curve") +
#'   theme_bw()
#'
#' p
#' }
#'
ks <- function(resp, h = NULL, kernel_func = "gauss",
               criterion = NULL,
               points = seq(-3, 3, 0.05)) {

  # Check whether resp is valid:
  if (!inherits(resp, "matrix")) {
    tryCatch(
      resp <- as.matrix(resp),
      error = function(e) {
        stop(paste0("Please provide a valid matrix for 'resp' argument: \n", e))
      }
    )
  }


  # Points on criterion scale where the probabilities will be calculated
  # delta <- 0.05
  # points <- seq(-3, 3, delta)
  n_points <- length(points)

  n_items <- ncol(resp)
  n_examinees <- nrow(resp)


  # Check whether criterion is valid:
  if (!is.null(criterion) && (!is.numeric(criterion) ||
                              length(criterion) != n_examinees))
    stop("Invalid 'criterion' argument. Please provide a numeric vector ",
         "that has the same number or elements as the number of rows of ",
         "'resp' argument.")

  # Calculate the transformed rank score if no criterion provided
  if (is.null(criterion)) {
    sum_score <- rowSums(resp, na.rm = TRUE)
    ts_rank <- rank(sum_score)/(n_examinees + 1)
    criterion <- qnorm(ts_rank)
  }

  # Set bandwidth h
  if (is.null(h)) {
    h <- 1.06 * sd(criterion) * n_examinees^(-.2)
  } else if (!is.numeric(h) || length(h) != 1 || h <= 0)
    stop("Invalid 'h' value. Please provide a value larger 0.")

  icc <- data.frame(matrix(0, nrow = n_points, ncol = n_items,
                           dimnames = list(NULL, colnames(resp))),
                    check.names = FALSE)

  # weight <- matrix(0, n_points, n_examinees)
  # for (i in 1:n_points) {
  #   kernel <- kf((points[i] - criterion)/h, method = kernel_func)
  #   weight[i, ] <- kernel/sum(kernel)
  #   for (j in 1:n_items) {
  #     non_na_resp_indices <- !is.na(resp[, j])
  #     icc[i, j] <- sum(kernel[non_na_resp_indices] *
  #                        resp[non_na_resp_indices, j])/
  #       sum(kernel[non_na_resp_indices])
  #   }
  # }


  # Set kernel Function
  available_kernels <- c("gauss", "unif", "quadratic")
  if (is.character(kernel_func) && length(kernel_func) == 1 &&
      kernel_func %in% available_kernels) {
    kf <- switch(
      kernel_func,
      gauss = function(x) exp(-.5 * x^2),
      unif = function(x) ifelse(abs(x) <= 1, 0.5, 0),
      quadratic = function(x) ifelse(abs(x) <= 1, 0.75 * (1 - x^2), 0))
  } else if (is.function(kernel_func)) {
    kf <- kernel_func
  } else
    stop(paste0("Invalid \"kernel_func\" argument. Please provide a valid ",
                "function with only one argument or ",
                "select one of the following kernel functions: ",
                paste0("\"", available_kernels, "\"", collapse = ", "), "."))


  weight <- matrix(0, n_points, n_examinees)

  for (i in 1:n_points)
    weight[i, ] <- kf((points[i] - criterion)/h)

  # Since each item has different pattern of missing responses, each item
  # should have a different weight matrix. 'w_list' holds the weight matrix
  # of each item.
  # All weight matrices will be the same for each item, only difference is
  # the weights of examinees where examinee did not respond for an item will
  # be removed and the sum of kernel will be calculated using non-missing
  # response weights instead of all weights.
  w_list <- lapply(vector("list", n_items), function(x) weight)

  for (i in 1:n_items) {
    w_list[[i]][, is.na(resp[, i])] <- 0
    w_list[[i]] <- w_list[[i]]/rowSums(w_list[[i]])
  }

  resp_non_na <- resp
  resp_non_na[is.na(resp)] <- 0
  for (i in 1:n_items) {
    icc[, i] <- w_list[[i]] %*% matrix(resp_non_na[, i], ncol = 1)[,1]
  }


  icc_hat <- matrix(0, n_examinees, n_items)
  # Find the total scores
  for (j in 1:n_items) {
    icc_hat[, j] <- approx(points, icc[, j], criterion)$y
    icc_hat[is.na(icc_hat[, j]), j] <- 0
  }

  se <- matrix(0, n_points, n_items)
  x <- (icc_hat * (1 - icc_hat))

  for (i in 1:n_items) {
    se[, i] <- sqrt(w_list[[i]]^2 %*% x[, i, drop = FALSE])
  }

  result <- list(points = points, icc = icc, se = se, criterion = criterion,
                 h = h)
  class(result) <- c("ks_output", class(result))
  return(result)
}



