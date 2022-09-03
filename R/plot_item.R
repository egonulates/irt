
############################################################################@###
############################# plot.Item ####################################@###
############################################################################@###
#' Plot Item Characteristic Curve of an \code{Item} object
#' @description
#' \code{plot.Item} Plots the item characteristic curve for dichotomous items
#' and category response functions for polytomous items.
#'
#' @param x An \code{\link{Item-class}} object.
#' @param theta_range Either (a) a numeric vector of length two where the values
#'   are minimum and maximum theta values, or, (b) a numeric vector of length
#'   more than two where values represents the theta values that will be
#'   plotted.
#' @param title Title of the plot. By default if the item is 1-4PM IRT model
#'          then the title will be "Item Characteristic Curve" if the item
#'          follows Graded Response Model the title will be
#'          "Category Response Functions". Set it to \code{NULL} to suppress the
#'          title.
#' @param suppress_plot If \code{FALSE} the function will print the plot. If
#'          \code{TRUE}, function will return the plot object. Default value is
#'          \code{FALSE}. Function cannot suppress plot when
#'          \code{base_r_graph = TRUE}, but graph still can be saved in a
#'          variable.
#' @param category_names If the model used is 'GRM' (Graded Response Model)
#'          these names will serve as category names. For example,
#'          c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree").
#'          The default is \code{FALSE} where the default category scores
#'          will be printed. If the value is \code{NULL} no legend will be
#'          printed but the categories will be printed differently.
#' @param legend_title The title of the plot's legend.
#' @param base_r_graph If \code{TRUE} function will plot graphs using base R
#'   graphics. If \code{FALSE} the function will check whether 'ggplot2' package
#'   is installed. If it is installed, it will use 'ggplot2' package for the
#'   plot. The default value is \code{FALSE}.
#' @param ... Additional arguments that will be passed to \code{geom_line}
#'
#' @return Depending on the value of \code{suppress_plot} function either prints
#' the item characteristic curve or returns the plot object.
#'
#' @export
#'
#' @importFrom stats reshape runif
#'
#' @include plot_itempool.R
#'
#' @author Emre Gonulates
#'
#' @examples
#' plot(x = item(b = 0.3, D = 1, model = "1PL"))
#'
#' itm1 <- item(a = 1.2, b = 0.3, c = .2, model = "3PL")
#' plot(itm1)
#' plot(item(a = 1.2, b = 0.3, c = .2, d = .89, D = 1))
#'
#' # Use base R graphics for the plot
#' plot(itm1, base_r_graph = TRUE)
#'
#' # Plot Graded Response Model
#' itm2 <- item(a = 0.902, b = c(-1.411, 0.385, 1.79), model = "GRM")
#' plot(itm2)
#' plot(itm2, category_names = c("Strongly Disagree", "Disagree", "Agree",
#'                               "Strongly Agree"))
#'
#' plot(itm2, category_names = c("Strongly Disagree", "Disagree", "Agree",
#'                               "Strongly Agree"), base_r_graph = TRUE)
#'
#' # A Graded Response Model item with two categories (i.e. 2PL item):
#' itm3 <- item(a = 0.8, b = 1, model = "GRM")
#' plot(itm3, category_names = c("Incorrect", "Correct"),
#'      legend_title = "Response")
#'
#' \dontrun{
#' # Change the y-axis label (Only available if 'ggplot2' is installed)
#' # plot(itm3, suppress_plot = TRUE) + ylab("New Label")
#' }
#'
plot.Item <- function(x, theta_range = c(-4,4), title = "",
                      suppress_plot = FALSE, category_names = FALSE,
                      legend_title = NULL, base_r_graph = FALSE, ...) {
  theta_range <- process_theta_range(theta_range, n_theta = 501)
  theta <- theta_range$theta
  theta_range <- theta_range$range
  x_label <- expression("Theta ("*theta*")")
  y_lim <- c(0, 1)
  # Prepare plot data:
  if (x$model %in% UNIDIM_POLY_MODELS) {
    icc <- prob(x, theta = theta)
    icc <- stats::setNames(data.frame(icc), paste0(0:(ncol(icc) - 1)))
    if (is.null(category_names) ||
        # if the category_names are FALSE.
        (all(is.logical(category_names)) && !all(category_names))) {
      category_labels <- colnames(icc)
    } else category_labels <- category_names

    if (!is.null(title) && title == "") title <- "Category Response Functions"
    y_label <- "Probability of Response"
    if (is.null(legend_title)) legend_title <- "Category"
  } else if (x$model %in% UNIDIM_DICHO_MODELS) {
    icc <- data.frame(theta = theta, p = prob(ip = x, theta = theta)[, 2])

    y_label <- "Probability of Correct Response"
    if (!is.null(title) && title == "") title <- "Item Characteristic Curve"
  } else stop("This model has not been implemented yet.", call. = FALSE)


  # Plot the graphs
  # check if ggplot exists
  if (!base_r_graph && requireNamespace("ggplot2", quietly = TRUE)) {
    if (x$model %in% UNIDIM_POLY_MODELS) {
      # Create long format ICC data:
      icc <- reshape(icc, varying = list(colnames(icc)),
                     v.names = 'p', ids = theta, idvar = 'theta',
                     timevar = 'Category', times = category_labels,
                     direction = 'long')
      # Order categories so that the order correctly captured in graph legend.
      icc$Category <- factor(icc$Category, levels = category_labels,
                             ordered = TRUE)

      p <- ggplot2::ggplot(
        data = icc,
        ggplot2::aes_string(x = 'theta', y = 'p', color = 'Category')) +
        # Do not show legend if legend_title is NULL
        ggplot2::geom_line(..., show.legend = !is.null(category_names)) +
        ggplot2::labs(x = x_label, y = y_label, title = title) +
        ggplot2::theme(text = ggplot2::element_text(size = 18)) +
        ggplot2::scale_x_continuous(
          breaks = seq(from = ceiling(theta_range[1]),
                       to = floor(theta_range[2]), 1)) +
        ggplot2::ylim(y_lim) +
        ggplot2::scale_color_discrete(name = legend_title) +
        ggplot2::guides(colour = ggplot2::guide_legend(
          override.aes = list(alpha = 1, size = 4))) +
        ggplot2::theme_bw()
    } else if (x$model %in% UNIDIM_DICHO_MODELS) {
      # If there is only one item do not print out the legend
      p <- ggplot2::ggplot(data = icc,
                           ggplot2::aes_string(x = "theta", y = "p")) +
        ggplot2::geom_line(...) +
        ggplot2::labs(x = x_label, y = y_label, title = title) +
        ggplot2::ylim(y_lim) +
        ggplot2::scale_x_continuous(
          breaks = seq(from = ceiling(theta_range[1]),
                       to = floor(theta_range[2]), 1)) +
        ggplot2::theme(text = ggplot2::element_text(size = 18)) +
        ggplot2::theme_bw()
    } else stop("This model has not been implemented yet.", call. = FALSE)
    if (suppress_plot) return(p) else print(p)
  } else {# ggplot is not available
    if (x$model %in% UNIDIM_POLY_MODELS) {
      old_par <- graphics::par(no.readonly = TRUE) # Save old settings
      # dev.off()
      graphics::par(mar = c(5.1, 4.1, 4.1,
                  3 + .4 * max(nchar(c(legend_title, category_labels)))))
      graphics::matplot(x = theta, y = icc,
              ylab = y_label,
              xlab = x_label,
              main = title,
              ylim = y_lim,
              type = "l",
              col = 1:ncol(icc),
              lty = 1,
              panel.first = graphics::grid())
      graphics::legend("topleft", category_labels, col = 1:ncol(icc), lty = 1,
                       xpd = TRUE, inset = c(1, 0), bty = "n",
                       title = legend_title)

      p <- grDevices::recordPlot()
      graphics::par(old_par) # Restore to old settings
      return(invisible(p))
    } else if (x$model %in% UNIDIM_DICHO_MODELS) {
      plot(x = icc$theta, y = icc$p, type = "l",
           ylab = y_label,
           xlab = x_label,
           ylim = y_lim,
           main = title,
           panel.first = graphics::grid())
      p <- grDevices::recordPlot()
      return(invisible(p))
    } else stop("This model has not been implemented yet.", call. = FALSE)
  }
}




