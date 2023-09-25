
############################# get_data_plot_ks #############################%###

#' Get data for plot_ks()
#'
#' @return A data.frame
#' @noRd
get_data_plot_ks <- function(ks_output, item_no, ci = .95, ip = NULL) {
  x <- ks_output$icc[, item_no]
  gd <- data.frame(theta = ks_output$points, icc = x)
  if (!is.null(ci)) {
    se <- ks_output$se[, item_no]
    ci <- 1 - (1 - ci)/2
    gd$ci_low <- sapply(x - qnorm(ci) * se, function(x) max(x, 0))
    gd$ci_high <- sapply(x + qnorm(ci) * se, function(x) min(x, 1))
  }
  if (!is.null(ip)) {
    if (is(ip, "Itempool")) {
      gd$p <- prob(ip = ip[[item_no]], theta = ks_output$points)[, 2]
    } else if (is(ip, "Item")) {
      gd$p <- prob(ip = ip, theta = ks_output$points)[, 2]
    } else {
      warning("'ip' should be either an 'Itempool' or 'Item' object.")
      ip <- NULL
    }
  }
  gd
}




############################################################################%###
############################# plot.ks_output ###############################%###
############################################################################%###
#' Plot Item Fit using Kernel-Smoothing
#'
#' @param x The output of \code{ks()} function. If this will be provided
#'   the function will run much faster.
#' @param item_no The order (i.e. column number) of the item to be plotted.
#' @param ip An \code{\link{Itempool-class}} or \code{\link{Item-class}}
#'  object if expected probabilities are plotted.
#' @param title Title of the plot. If the value is \code{NULL},
#'   the plot title will be suppressed.
#' @param ci It is either a number indicating the confidence interval that will
#'   be plotted around the item fit line or \code{NULL} if no confidence
#'   interval should be plotted. The default value is 0.95, i.e. 95% confidence
#'   interval will be plotted.
#' @param base_r_graph If \code{TRUE} function will plot graphs using base R
#'   graphics. If \code{FALSE} the function will check whether 'ggplot2' package
#'   is installed. If it is installed, it will use 'ggplot2' package for the
#'   plot. The default value is \code{FALSE}.
#' @param suppress_plot If \code{FALSE} the function will print the plot. If
#'          \code{TRUE}, function will return the plot object. Default value is
#'          \code{FALSE}.
#' @param ... further arguments.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' # Generate responses
#' ip <- generate_ip()
#' resp <- sim_resp(ip = ip, theta = rnorm(500), prop_missing = .2)
#' # Run kernel smoothing
#' ks_data <- ks(resp)
#' # Plot first item
#' plot(ks_data, item_no = 1)
#' # Plot second item with expected probability value
#' plot(ks_data, item_no = 2, ip = ip)
#'
#' plot(ks_data, item = 2, ip = ip[[2]])
plot.ks_output <- function(x, item_no, ip = NULL,
                           title = "",
                           ci = 0.95,
                           base_r_graph = FALSE,
                           suppress_plot = FALSE, ...) {
  if (missing(item_no) || !is_single_value(item_no, c("numeric", "integer"))) {
    stop("Invalid 'item_no'. Please provide a valid item number to be plotted.")
  }

  gd <- get_data_plot_ks(ks_output = x, item_no = item_no, ci = ci, ip = ip)
  x_label <- expression("Theta ("*theta*")")
  y_label <- "Probability"
  y_lim <- c(0, 1)

  if (!is.null(title) && title == "") {
    title <- "Non-Parametric Fit"
  }
  ### ggplot2 ###
  if (!base_r_graph && requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(data = gd) +
      ggplot2::geom_line(ggplot2::aes(x = .data$theta, y = .data$icc),
                         color = "blue", alpha = .7, linewidth = 1)
    # Add ICC
    if (!is.null(ip))
      p <- p +
      ggplot2::geom_line(ggplot2::aes(x = .data$theta, y = .data$p),
                         color = "red", linewidth = 1, alpha = .7)

    # Add confidence bands
    if (!is.null(ci)) {
      p <- p +
        ggplot2::geom_ribbon(data = gd,
                             ggplot2::aes(x = .data$theta, ymin = .data$ci_low,
                                          ymax = .data$ci_high), alpha = .25)
    }
    p <- p +
      ggplot2::labs(x = x_label, y = y_label, title = title) +
      ggplot2::ylim(y_lim) +
      ggplot2::theme_bw()
    if (suppress_plot) return(p) else print(p)
  ### Base R graphics ###
  } else {
    old_par <- graphics::par(no.readonly = TRUE) # Save old settings
    x_lim <- c(min(gd$theta), max(gd$theta))
    plot(x = gd$theta, y = gd$icc,
         xlim = x_lim,
         ylim = y_lim,
         ylab = y_label,
         xlab = x_label,
         main = title,
         col = "blue",
         panel.first = graphics::grid(),
         type = "l")

    # Add ICC
    if (!is.null(ip))
      graphics::lines(x = gd$theta, y = gd$p, type = "l", col = "tomato1")

    # Add confidence bands
    if (!is.null(ci)) {
      graphics::polygon(x = c(gd$theta, rev(gd$theta)),
                        y = c(gd$ci_low, rev(gd$ci_high)),
                        col = grDevices::adjustcolor("lightgray", alpha.f = .3),
                        border = NA)
    }
    # dev.off()
    p <- grDevices::recordPlot()
    graphics::par(old_par) # Restore to old settings
    return(invisible(p))
  }
}

