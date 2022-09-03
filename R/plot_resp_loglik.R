
############################# get_data_plot_resp_loglik ####################%###

#' Get data for plot_resp_loglik()
#'
#' @noRd
get_data_plot_resp_loglik <- function(ip, resp, theta_range = c(-4,4),
                                      likelihood = FALSE) {
  # Convert ip to Itempool object
  if (!is(ip, "Itempool"))
    tryCatch(
      {ip <- itempool(ip)},
      error = function(cond) {
        message("\nip cannot be converted to an 'Itempool' object. \n")
        stop(cond)
      })
  resp <- convert_to_response(resp = resp, ip = ip, object_name = "resp")

  theta_range <- process_theta_range(theta_range, n_theta = 201)
  theta <- theta_range$theta
  # Prepare a dataset for plot
  value <- sapply(theta, FUN = function(x) resp_loglik(ip = ip, resp = resp,
                                                       theta = x))
  gd <- data.frame(theta = theta)
  if (likelihood) gd$value <- exp(value) else gd$value <- value
  return(gd)
}




############################################################################%###
############################# plot_resp_loglik #############################%###
############################################################################%###

#' Plot the Log-Likelihood of a response string
#' @description
#' \code{plot_resp_loglik} plots the log-likelihood of a response string.
#'
#' @param ip An \code{\link{Itempool-class}} class object.
#' @param resp The response string or a \code{\link{Response-class}} class
#'   object.
#' @param theta_range Either (a) a numeric vector of length two where the values
#'   are minimum and maximum theta values, or, (b) a numeric vector of length
#'   more than two where values represents the theta values that will be
#'   plotted.
#' @param title Title of the plot. If the value is \code{NULL},
#'   the plot title will be suppressed.
#' @param likelihood If \code{TRUE}, likelihood function will be plotted
#'          instead of log-likelihood graph. Default value is \code{FALSE}.
#' @param show_estimate If \code{TRUE} the maximum likelihood ability estimate
#'          will be shown. The default value is \code{TRUE}.
#' @param base_r_graph If \code{TRUE} function will plot graphs using base R
#'   graphics. If \code{FALSE} the function will check whether 'ggplot2' package
#'   is installed. If it is installed, it will use 'ggplot2' package for the
#'   plot. The default value is \code{FALSE}.
#' @param suppress_plot If \code{FALSE} the function will print the plot. If
#'          \code{TRUE}, function will return the plot object. Default value is
#'          \code{FALSE}.
#' @param text_size The overall text size of the axis and titles. The default
#'          value is 12.
#' @param ... Additional arguments passed to annotate.
#'
#' @return Depending on the value of \code{suppress_plot} function either prints
#' the Log-likelihood function of the response string or returns the plot
#' object.
#'
#' @export
#' @author Emre Gonulates
#'
#' @section To-do:
#' \itemize{
#' \item Make it to plot multiple test information functions. You can input a
#' list each of which contains item parameters. And the name of the test also.
#' }
#' @examples
#' \dontrun{
#' ip <- generate_ip(n = 9)
#' resp_set <- generate_resp_set(ip = ip, theta = rnorm(10))
#'
#' # Plot second item's response log-likelihood function
#' plot_resp_loglik(ip, resp_set[[2]])
#'
#' # Plot response likelihood function of second item
#' plot_resp_loglik(ip, resp_set[[2]], likelihood = TRUE)
#'
#' # Plot using base r graphics
#' plot_resp_loglik(ip, resp_set[[2]], likelihood = TRUE, base_r_graph = TRUE)
#'
#' # Suppress the MLE estimate
#' plot_resp_loglik(ip, resp_set[[4]], show_estimate = FALSE)
#' }
plot_resp_loglik <- function(ip, resp, theta_range = c(-5,5), title = "",
                             likelihood = FALSE, show_estimate = TRUE,
                             base_r_graph = FALSE, suppress_plot = FALSE,
                             text_size = 12, ...) {

  theta_range <- process_theta_range(theta_range, n_theta = 201)
  theta <- theta_range$theta
  theta_range <- theta_range$range
  gd <- get_data_plot_resp_loglik(ip = ip, resp = resp,
                                  theta_range = theta,
                                  likelihood = likelihood)
  if (!is.null(title) && title == "")
    title <- ifelse(likelihood, "Likelihood Graph", "Log-Likelihood Graph")
  if (show_estimate) {
    xIntercept <- gd$theta[which.max(gd$value)]
    yIntercept <- min(gd$value)
  }
  x_label <- expression("Theta ("*theta*")")
  y_label <- ifelse(likelihood, "Likelihood", "Log-Likelihood")
  ### ggplot2 ###
  if (!base_r_graph && requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(gd, ggplot2::aes_string(x = 'theta', y = 'value')) +
      ggplot2::geom_line(size = 1, color = "blue") +
      ggplot2::labs(x = x_label, y = y_label, title = title)
    if (show_estimate)
      p <- p + ggplot2::geom_vline(xintercept = xIntercept,
                                   linetype = "dashed") +
        ggplot2::annotate(
          "text", x = xIntercept, y = yIntercept,
          label = paste("hat(theta) == ", xIntercept),
          hjust = ifelse(xIntercept > stats::median(theta_range), 1.1, -.1),
          parse = T, ...)
    p <- p + ggplot2::theme_bw() +
	  ggplot2::theme(text = ggplot2::element_text(size = text_size))
    if (suppress_plot) return(p) else print(p)
  ### Base R Graphics ###
  } else {
    old_par <- graphics::par(no.readonly = TRUE) # Save old settings

    plot(x = gd$theta,
         y = gd$value,
         ylab = y_label,
         xlab = x_label,
         main = title,
         panel.first = graphics::grid(),
         col = "blue",
         type = "l")
    if (show_estimate) {
      graphics::abline(v = xIntercept, lty = 2)
      graphics::text(x = xIntercept, y = min(gd$value),
                     pos = ifelse(stats::median(gd$theta) < xIntercept, 2, 4),
                     offset = 0.1, labels = bquote(hat(theta) == .(xIntercept)))
    }
    # dev.off()
    p <- grDevices::recordPlot()
    graphics::par(old_par) # Restore to old settings
    return(invisible(p))
  }
}



