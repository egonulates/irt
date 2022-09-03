

#' Get data for plot_info()
#'
#' @include plot_itempool.R
#'
#' @noRd
get_data_plot_info <- function(ip, theta_range = c(-5, 5),
                               separate_testlet = TRUE, tif = FALSE) {
  # in case there are testlets
  if (separate_testlet) ip <- itempool(flatten_itempool_cpp(ip))
  item_ids <- ip$id

  theta_range <- process_theta_range(theta_range, n_theta = 301)
  theta <- theta_range$theta
  theta_range <- theta_range$range

  info_data <- info(ip = ip, theta = theta, tif = tif)
  if (tif) colnames(info_data) <- "info" else
    colnames(info_data) <- item_ids
  info_data <- data.frame(cbind(theta = theta, info_data), check.names = FALSE)
  if (!tif)
    info_data <- reshape(data = info_data,
                         direction = "long",
                         varying = list(item_ids),
                         v.names = "info",
                         timevar = "item_id",
                         times = item_ids,
                         idvar = "theta",
                         new.row.names = sequence(length(theta) * length(ip)))
  return(info_data)
}



############################################################################%###
############################# plot_info ####################################%###
############################################################################%###
#' Plot Item Information Function
#' @description
#' \code{plot_info} Plots the item information function.
#'
#' @param ip An \code{\link{Item-class}} or \code{\link{Itempool-class}}
#'   object.
#' @param tif If \code{TRUE} a test information plot will be plotted. The
#'          default value is \code{FALSE}.
#' @param theta_range Either (a) a numeric vector of length two where the values
#'   are minimum and maximum theta values, or, (b) a numeric vector of length
#'   more than two where values represents the theta values that will be
#'   plotted.
#' @param focus_item If one or more items information graphs needed to be
#'   focused whereas rest of the items' information functions needed to be on
#'   the background, provide item numbers or item ID's to be focused.
#' @param title Title of the plot. If the value is \code{NULL},
#'   the plot title will be suppressed.
#' @param base_r_graph If \code{TRUE} function will plot graphs using base R
#'   graphics. If \code{FALSE} the function will check whether 'ggplot2' package
#'   is installed. If it is installed, it will use 'ggplot2' package for the
#'   plot. The default value is \code{FALSE}.
#' @param suppress_plot If \code{FALSE} the function will print the plot. If
#'          \code{TRUE}, function will return the plot object. Default value is
#'          \code{FALSE}.
#' @param separate_testlet A logical value indicating whether to separate
#'   items within testlets or not. If \code{TRUE}, information values of all
#'   items within the testlet are plotted separately. if \code{FALSE},
#'   information functions of items within testlets are combined (like test
#'   information function) and plotted that way along with standalone items.
#' @param ... Extra parameters that will pass to \code{geom_line}.
#'
#' @return Depending on the value of \code{suppress_plot} function either prints
#' the item information function or returns the plot object.
#'
#' @export
#'
#'
#'
#' @include plot_itempool.R
#' @include misc.R
#'
#' @author Emre Gonulates
#'
#' @examples
#' # Plot the information function of an item
#' plot_info(item(b = 1))
#'
#' # Plot information function(s) of an Itempool object
#' n <- sample(10:20,1)
#' ip <- generate_ip()
#' plot_info(ip)
#' plot_info(ip, tif = TRUE)
#' plot_info(ip, tif = TRUE, theta_range = c(-3, 3))
#' # Focus on one item
#' plot_info(ip, focus_item = "Item_2")
#'
#' # Base R Graphics
#' plot_info(ip, base_r_graph = TRUE)
#' plot_info(ip, focus_item = "Item_2", base_r_graph = TRUE)
#'
#' # Plot information with focus on a specific item(s)
#' plot_info(ip, focus_item = "Item_1")
#' plot_info(ip, focus_item = 3)
#' # plot_info(ip, focus_item = c(2, 8))
#' # plot_info(ip, focus_item = c("Item_5", "Item_6"))
#'
#' plot_info(ip, focus_item = 7, alpha = .7, color = "gray")
#'
#' plot_info(ip, focus_item = "Item_3", color = "green", base_r_graph = TRUE)
#'
#'
#' # Information Plots with Testlets
#' ip <- c(testlet(itempool(b = c(-1, 1), item_id = c("t1-i1", "t1-i2"),
#'                          D = 1.702), testlet_id = "t1"),
#'         testlet(itempool(b = c(-2, 0, 2),
#'                          item_id = c("t2-i1", "t2-i2", "t2-i3"),
#'                          D = 1.702), testlet_id = "t2"),
#'         item(b = -1.5, item_id = "i1", D = 1.702),
#'         item(b = 0.25, item_id = "i2", D = 1.702),
#'         item(b = 1.5, item_id = "i3", D = 1.702))
#' plot_info(ip)
#' plot_info(ip, separate_testlet = FALSE)
plot_info <- function(ip, tif = FALSE, theta_range = c(-5,5), focus_item = NULL,
                      title = "", suppress_plot = FALSE, base_r_graph = FALSE,
                      separate_testlet = TRUE, ...)
{
  args <- list(...)
  # Convert ip to Itempool object
  if (!is(ip, "Itempool"))
    tryCatch(
      {ip <- itempool(ip)},
      error = function(cond) {
        message("\nip cannot be converted to an 'Itempool' object. \n")
        stop(cond)
      })

  focus_item <- check_focus_item(focus_item, ip = ip, single_value = FALSE)
  if (tif && !is.null(focus_item)) {
    message("When 'tif = TRUE', focus_item cannot be plotted.")
    focus_item <- NULL
  }

  # Set default title
  if (!is.null(title) && title == "")
    title = ifelse(
      tif, "Test Information Function",
      paste0("Item Information Function",
             ifelse(is.null(focus_item), "",
                    paste0(" for '", paste0(focus_item, collapse = "', '"),
                           "'")))
      )

  theta_range <- process_theta_range(theta_range, n_theta = 501)
  theta <- theta_range$theta
  theta_range <- theta_range$range
  info_data <- get_data_plot_info(ip = ip, theta_range = theta, tif = tif,
                                  separate_testlet = separate_testlet)

  x_label <- expression("Theta ("*theta*")")
  y_label <- ifelse(tif, "Test Information", "Information")
  legend_title <- ifelse(separate_testlet, "Item ID", "ID")

  ### ggplot2 ###
  if (!base_r_graph && requireNamespace("ggplot2", quietly = TRUE)) {
    ## Without Focus Item ##
    if (is.null(focus_item)) {
      if (tif || ip$n$items == 1) {
        p <- ggplot2::ggplot(data = info_data,
                             ggplot2::aes_string(x = 'theta', y = 'info'))
      } else
        p <- ggplot2::ggplot(data = info_data,
                             ggplot2::aes_string(x = 'theta', y = 'info',
                                                 color = 'item_id'))
      p <- p +
        ggplot2::geom_line(...) +
        ggplot2::labs(x = x_label, y = y_label, title = title,
                      color = ifelse(tif, NA, legend_title)) +
        ggplot2::theme(text = ggplot2::element_text(size = 18))
      if (!tif)
        p <- p +
        ggplot2::guides(colour = ggplot2::guide_legend(
          override.aes = list(alpha = 1, size = 4)))
    ## Focus Item ##
    } else {
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = info_data[!info_data$item_id %in% focus_item, ],
          mapping = ggplot2::aes_string(x = "theta", y = "info",
                                        group = "item_id"),
          color = ifelse("color" %in% names(args), args$color, "tomato1"),
          alpha = ifelse("alpha" %in% names(args), args$alpha, 0.4)) +
        ggplot2::geom_line(
          data = info_data[info_data$item_id %in% focus_item, ],
          mapping = ggplot2::aes_string(x = "theta", y = "info",
                                        group = "item_id")) +
        ggplot2::labs(x = x_label, y = y_label, title = title)
    }

    p <- p + ggplot2::theme_bw()
    if (suppress_plot) return(p) else print(p)
  ### Base R Graphics ###
  } else {
    old_par <- graphics::par(no.readonly = TRUE) # Save old settings
    y_lim = c(0, max(info_data$info))

    if (is.null(focus_item)) {
      if (tif) {
        plot(x = info_data$theta, y = info_data$info,
             xlim = theta_range,
             ylab = y_label,
             xlab = x_label,
             ylim = y_lim,
             main = title,
             panel.first = graphics::grid(),
             type = "l")
      } else {
        item_ids <- unique(info_data$item_id)
        # Make sure black is the first color
        cl <- c("#0000FF", setdiff(grDevices::rainbow(length(item_ids)),
                                   "#0000FF"))
        if (ip$n$items > 1)
          graphics::par(mar = c(5.1, 4.1, 4.1, 3 + .6 * max(nchar(item_ids))))
        plot(0, 0,
             xlim = theta_range,
             ylim = y_lim,
             ylab = y_label,
             xlab = x_label,
             main = title,
             panel.first = graphics::grid(),
             type = "n")
        for (i in seq_along(item_ids)) {
          temp <- info_data[info_data$item_id == item_ids[i], ]
          graphics::lines(x = temp$theta, y = temp$info, col = cl[i], lty = 1,
                lwd = ifelse(item_ids[i] %in% focus_item, 2, 1))
        }
        if (ip$n$items > 1)
          graphics::legend("topleft", item_ids, col = cl, lty = 1, xpd = TRUE,
                           inset = c(1, 0), bty = "n", title = legend_title)
      }
    } else {
      item_ids <- unique(info_data$item_id)
      plot(0, 0,
           xlim = theta_range,
           ylim = y_lim,
           ylab = y_label,
           xlab = x_label,
           main = title,
           panel.first = graphics::grid(),
           type = "n")
      for (i in seq_along(item_ids)) {
        if (item_ids[i] %in% focus_item) next
        temp <- info_data[info_data$item_id == item_ids[i], ]
        graphics::lines(x = temp$theta, y = temp$info, lty = 1,
                        col = ifelse("color" %in% names(args), args$color,
                                     "tomato1"))
      }
      for (i in seq_along(focus_item)) {
        temp <- info_data[info_data$item_id == focus_item[i], ]
        graphics::lines(x = temp$theta, y = temp$info, col = "black",  lty = 1,
                        lwd = 2)
      }
    }
    # dev.off()
    p <- grDevices::recordPlot()
    graphics::par(old_par) # Restore to old settings
    return(invisible(p))
  }
}

