#' Helper function to convert a theta range to theta values
#'
#' @description  The function is mainly an argument checker. It gets a theta
#'   range and or theta values and checks whether it is acceptable. Then, if
#'   theta range is based on only two values then create a theta values between
#'   those two values.
#'
#' @param theta_range Either (a) a numeric vector of length two where the values
#'   are minimum and maximum theta values, or, (b) a numeric vector of length
#'   more than two where values represents the theta values that will be
#'   plotted.
#' @param n_theta If \code{theta_range} is composed of two numbers, this
#'   number will set the number of theta values that will be created between
#'   the theta range (including the theta range values).
#'
#' @return A numeric vector of theta values.
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
process_theta_range <- function(theta_range, n_theta = 500) {

  if (!is_atomic_vector(theta_range, class = c("numeric", "integer"))) {
    stop("Invalid 'theta_range' values. Please provide a numeric vector.")
  }
  if (length(theta_range) == 2) {
    theta <- seq(from = theta_range[1], to = theta_range[2], length.out = n_theta)
  } else if (length(theta_range) > 2) {
    theta <- theta_range
    theta_range = range(theta)
  } else
    stop("Invalid 'theta_range' values. Please provide a numeric vector of ",
         "length two or more.")
  return(list(theta = theta, range = theta_range))
}


#' Check Focus Item
#' Check Focus Item
#'
#' @param single_value If TRUE, focus_item needs to be a single value, i.e.
#'   a vector of length 1.
#'
#' @description This function checks whether a focus item is a valid item_id
#' or a valid item number.
#'
#' @include misc.R
#'
#' @return An item_id value that is one of \code{ip$resp_id}
#'
#' @author Emre Gonulates
#'
#' @noRd
check_focus_item <- function(focus_item, ip, single_value = TRUE) {
  if (!is(ip, "Itempool")) stop("Invalid 'ip'.")
  # check_single_focus_item <- function()
  if (is.null(focus_item))  {
    return(focus_item)
  } else if (is_single_value(focus_item,
                             class = c("numeric", "integer", "character"))) {
    if (is.character(focus_item) && focus_item %in% ip$resp_id) {
      return(focus_item)
    } else if (is.numeric(focus_item) && focus_item %in% 1:ip$n$items) {
      return(ip$resp_id[focus_item])
    }
  } else if (!single_value && is_atomic_vector(
    focus_item, class = c("numeric", "integer", "character"),
    accept_na = FALSE)) {
    if (all(is.character(focus_item)) && all(focus_item %in% ip$resp_id)) {
      return(focus_item)
    } else if (all(is.numeric(focus_item)) &&
               all(focus_item %in% 1:ip$n$items)) {
      return(ip$resp_id[focus_item])
    }
  }
  stop("Invalid 'focus_item' value. Please provide an item's ID.")
}




############################################################################%###
############################# plot_itempool_tcc ################################
############################################################################%###

############################# get_data_plot_itempool_tcc ###################%###

#' Get data for plot.Itempool(..., type = "tcc")
#'
#' @noRd
get_data_plot_itempool_tcc <- function(ip, theta_range = c(-4,4),
                                       tcc_prop_corr = FALSE) {
  theta_range <- process_theta_range(theta_range, n_theta = 501)
  theta <- theta_range$theta
  # If there are testlets, ignore them and only plot items within testlets
  ip <- itempool(flatten_itempool_cpp(ip = ip))
  p <- mean(ip, theta = theta)
  tcc <- data.frame(theta = theta, p = rowSums(p))

  if (tcc_prop_corr) tcc$p <- tcc$p / max_score(ip)
  return(tcc)
}


############################# plot_itempool_tcc ############################%###
#' Plot plot.Itempool(..., type = "tcc")
#'
#' @noRd
plot_itempool_tcc <- function(ip,
                              theta_range = c(-4,4),
                              title = "",
                              tcc_prop_corr = FALSE,
                              suppress_plot = FALSE,
                              base_r_graph = FALSE,
                              y_lim = NULL,
                              ...) {
  args <- list(...)

  if (!all(ip$item_model %in% c(UNIDIM_DICHO_MODELS, UNIDIM_POLY_MODELS))) {
    stop(paste0("'tcc' type plot cannot be used for this model: ",
                paste0(unique(ip$model[!ip$model %in% c(UNIDIM_DICHO_MODELS,
                                                        UNIDIM_POLY_MODELS)]),
                       collapse = ", ")))
  }
  if (!is.null(title) && title == "") title <- "Test Characteristic Curve"
  y_label <- ifelse(tcc_prop_corr, "Expected Proportion Correct",
                   "Expected Score")

  theta_range <- process_theta_range(theta_range, n_theta = 501)
  theta <- theta_range$theta
  theta_range <- theta_range$range

  ymax <- ifelse(tcc_prop_corr, 1, max_score(ip))
  if (is.null(y_lim)) y_lim <- c(0, ymax)
  x_label <- expression("Theta ("*theta*")")
  gd <- get_data_plot_itempool_tcc(ip = ip, theta_range = theta,
                                   tcc_prop_corr = tcc_prop_corr)
  ### ggplot2 ###
  if (!base_r_graph && requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(data = gd, ggplot2::aes_string(x = "theta", y = "p")) +
      ggplot2::geom_line(...) +
      ggplot2::labs(x = x_label, y = y_label, title = title) +
      ggplot2::ylim(y_lim)
    if (theta_range[2] - theta_range[1] > 2) {
      p <- p + ggplot2::scale_x_continuous(breaks = seq(
        ceiling(theta_range[1]),floor(theta_range[2]), by = 1))
    }
    p <- p +
      ggplot2::theme(text = ggplot2::element_text(size = 18)) +
      ggplot2::theme_bw()
    if (suppress_plot) return(p) else print(p)

  ### Base R graphics ###
  } else {
    old_par <- graphics::par(no.readonly = TRUE) # Save old settings
    plot(x = gd$theta,
         y = gd$p,
         ylab = y_label,
         xlab = x_label,
         main = title,
         ylim = y_lim,
         type = "l",
         lty = 1,
         panel.first = graphics::grid())
    # dev.off()
    p <- grDevices::recordPlot()
    graphics::par(old_par) # Restore to old settings
    return(invisible(p))
  }
}

############################################################################%###
############################# plot_itempool_icc ################################
############################################################################%###

############################# get_data_plot_itempool_icc ###################%###
#' Get data for plot.Itempool(..., type = "icc")
#'
#' @noRd
get_data_plot_itempool_icc <- function(ip, theta_range = c(-4,4)) {
  theta_range <- process_theta_range(theta_range, n_theta = 501)
  theta <- theta_range$theta

  # If there is testlets, ignore them and only plot items within testlets
  ip <- itempool(flatten_itempool_cpp(ip = ip))

  gd <- mean(ip, theta = theta)
  gd <- stats::reshape(
    cbind.data.frame(theta, gd),
    direction = "long",
    varying = list(colnames(gd)),
    idvar = "theta",
    v.names = "p",
    times = colnames(gd),
    timevar = "item_id")
  rownames(gd) <- NULL
  return(gd)
}


############################# plot_itempool_icc ############################%###
#' Plot plot.Itempool(..., type = "icc")
#'
#' @noRd
plot_itempool_icc <- function(ip,
                              theta_range = c(-4,4),
                              focus_item = NULL,
                              title = "",
                              legend_title = NULL,
                              suppress_plot = FALSE,
                              base_r_graph = FALSE,
                              y_lim = NULL,
                              ...) {
  args <- list(...)

  if (!all(ip$item_model %in% c(UNIDIM_DICHO_MODELS, UNIDIM_POLY_MODELS))) {
    stop(paste0("'icc' type plot cannot be used for this model: ",
                paste0(unique(ip$model[!ip$model %in% c(UNIDIM_DICHO_MODELS,
                                                        UNIDIM_POLY_MODELS)]),
                       collapse = ", ")))
  }

  if (any(ip$model %in% UNIDIM_POLY_MODELS)) {
    message("For polytomous items, expected values of items at each theta ",
            "point will be plotted. Option characteristic curves or an item ",
            "can be seen by plotting individual items using 'plot(item)' ",
            "function.")
  }

  y_label <- "Probability of Correct Response"
  x_label <- expression("Theta ("*theta*")")
  if (is.null(legend_title)) legend_title <- "Item ID"

  if (is.null(y_lim)) y_lim <- c(0, max(max_score(ip, sum = FALSE)))
  focus_item <- check_focus_item(focus_item, ip = ip)

  if (!is.null(title) && title == "")
    title <- ifelse(is.null(focus_item), "Item Characteristic Curve",
                    paste0("Item Characteristic Curve for '", focus_item, "'"))

  theta_range <- process_theta_range(theta_range, n_theta = 501)
  theta <- theta_range$theta
  theta_range <- theta_range$range
  gd <- get_data_plot_itempool_icc(ip = ip, theta_range = theta)

  ### ggplot2 ###
  if (!base_r_graph && requireNamespace("ggplot2", quietly = TRUE)) {
    # If there is only one item do not print out the legend
    if (is.null(focus_item)) {
      p <- ggplot2::ggplot(
        data = gd,
        ggplot2::aes_string(x = "theta", y = "p", color = "item_id")) +
        ggplot2::geom_line()
    } else {
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = gd[!gd$item_id %in% focus_item, ],
          ggplot2::aes_string(x = "theta", y = "p", group = "item_id"),
          alpha = .4, color = "tomato1") +
        ggplot2::geom_line(
          data = gd[gd$item_id %in% focus_item, ],
          ggplot2::aes_string(x = "theta", y = "p", group = "item_id"),
          color = "black")
    }
    p <- p +
      ggplot2::ylim(y_lim) +
      ggplot2::labs(x = x_label, y = y_label, title = title,
                    color = legend_title)
      if (theta_range[2] - theta_range[1] > 2) {
        p <- p + ggplot2::scale_x_continuous(breaks = seq(
          ceiling(theta_range[1]),floor(theta_range[2]), by = 1))
      }
      p <- p + ggplot2::guides(colour = ggplot2::guide_legend(
        override.aes = list(alpha = 1, size = 4))) +
      ggplot2::theme(text = ggplot2::element_text(size = 18)) +
      ggplot2::theme_bw()

    if (suppress_plot) return(p) else print(p)
  ### Base R graphics ###
  } else {
    item_ids <- unique(gd$item_id)
    old_par <- graphics::par(no.readonly = TRUE) # Save old settings

    if (is.null(focus_item)) {
      cl <- grDevices::rainbow(length(item_ids))
      graphics::par(mar = c(5.1, 4.1, 4.1,
                  3 + .6 * max(nchar(item_ids))))

      plot(0, 0,
           xlim = theta_range,
           ylim = y_lim,
           ylab = y_label,
           xlab = x_label,
           main = title,
           panel.first = graphics::grid(),
           type = "n")
      for (i in seq_along(item_ids)) {
        temp <- gd[gd$item_id == item_ids[i], ]
        graphics::lines(x = temp$theta, y = temp$p, col = cl[i], lty = 1,
              lwd = ifelse(item_ids[i] %in% focus_item, 2, 1))
      }
      graphics::legend("topleft", item_ids, col = cl, lty = 1, xpd = TRUE,
             inset = c(1, 0), bty = "n", title = legend_title)
    } else {
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
        temp <- gd[gd$item_id == item_ids[i], ]
        graphics::lines(x = temp$theta, y = temp$p, col = "tomato1", lty = 1)
      }
      for (i in seq_along(focus_item)) {
        temp <- gd[gd$item_id == focus_item[i], ]
        graphics::lines(x = temp$theta, y = temp$p, col = "black",  lty = 1,
                        lwd = 2)
      }
    }

    # dev.off()
    p <- grDevices::recordPlot()
    graphics::par(old_par) # Restore to old settings
    return(invisible(p))
  }
}


############################################################################%###
############################# plot_itempool_pars ###############################
############################################################################%###


############################# get_data_plot_itempool_pars ##################%###
#' Get data for plot.Itempool(..., type = "pars")
#'
#' @noRd
get_data_plot_itempool_pars <- function(ip) {
  # If there are testlets, ignore them and only plot items within testlets
  ip <- itempool(flatten_itempool_cpp(ip = ip))
  ip_df <- as.data.frame(ip)
  col_names <- setdiff(unique(unlist(sapply(ip$item_list, get_slot_names_item,
                                            type = "pars_df"))), "D")
  # multi_model <- any(ip_df$model != ip_df$model[1])
  # if (multi_model) { # items from multiple models
  #   ip_df <- ip_df[, c("item_id", "model", col_names)]
  # } else { # all item models are the same
  #   ip_df <- ip_df[, c("item_id", col_names)]
  # }
  ip_df <- reshape(ip_df[, c("item_id", "model", col_names)],
                   direction = "long",
                   varying = list(col_names),
                   v.names = "value",
                   times = col_names,
                   timevar = "par_name")
  ip_df <- ip_df[!is.na(ip_df$value), ]
  rownames(ip_df) <- NULL
  return(ip_df)
}



############################# plot_itempool_pars ###########################%###
#' Plot plot.Itempool(..., type = "pars")
#'
#' @noRd
plot_itempool_pars <- function(ip,
                               title = "",
                               suppress_plot = FALSE,
                               focus_item = NULL,
                               base_r_graph = FALSE,
                               ...) {
  args <- list(...)
  # Check focus item and convert it to an item_id value if it is numeric
  focus_item <- check_focus_item(focus_item, ip = ip)

  ip_df <- get_data_plot_itempool_pars(ip)
  multi_model <- any(ip$model != ip$model[1])

  if (!is.null(title) && title == "") {
    title <- ifelse(is.null(focus_item), "Parameter Values",
                    paste0("Parameter Values for Item '", focus_item, "'"))
  }

  # First try ggplot2:
  if (!base_r_graph && requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_boxplot(data = ip_df,
                            mapping = ggplot2::aes_string(x = "value"),
                            # outlier.shape = NA,
                            outlier.alpha = 0, color = "blue", size = 1) +

      ggplot2::geom_dotplot(
        data = ip_df, mapping = ggplot2::aes_string(x = "value"),
        binaxis = 'x', binpositions = "all", stackdir = 'centerwhole',
        method = "histodot", dotsize = ifelse("dotsize" %in% names(args),
                                              args$dotsize, 1), alpha = .4)
    if (!is.null(focus_item))
      p <- p +
        ggplot2::geom_point(data = ip_df[ip_df$item_id == focus_item, ],
                            mapping = ggplot2::aes_string(x = "value", y = 0),
                            size = 4, color = "red", shape = 18)

    if (multi_model) {
      p <- p + ggplot2::facet_wrap(~model + par_name, scales = "free")
    } else {
      p <- p + ggplot2::facet_wrap(~par_name, scales = "free")
    }
    p <- p + ggplot2::ggtitle(title) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_blank(),
                       axis.ticks.y = ggplot2::element_blank())
    if (suppress_plot) return(p) else suppressMessages(print(p))
  } else {# Try base R graphics
    gd <- split(ip_df, list(ip_df$model, ip_df$par_name), drop = TRUE)
    temp_col_num <- min(3, length(gd))  # number of columns of the graph
    old_par <- graphics::par(no.readonly = TRUE) # Save old settings

    graphics::par(mfrow = c(ceiling(length(gd) / temp_col_num), temp_col_num),
        mar = c(3, 1, 2, 1))
    for (i in 1:length(gd)) {
      temp_gd <- gd[[i]]
      graphics::boxplot(
        temp_gd$value, horizontal = TRUE, xlim = c(.75, 1.25),
        main = paste0(ifelse(multi_model, paste0(temp_gd$model[1], " - "), ""),
                      temp_gd$par_name[1]))
      # Points
      graphics::stripchart(temp_gd$value,
                 method = "jitter",
                 pch = 19,
                 cex = 1,
                 # col = 4,
                 col = grDevices::rgb(red = 0, green = 0, blue = 1,
                                      alpha = 0.25),
                 vertical = FALSE,
                 add = TRUE)

      if (!is.null(focus_item)) {
        temp <- temp_gd[temp_gd$item_id == focus_item, ]
        if (nrow(temp) == 1) {
          graphics::points(x = temp$value, y = 1, cex = 2, col = "red", pch = 18)
        }
      }
    }
    # dev.off()
    p <- grDevices::recordPlot()
    graphics::par(old_par) # Restore to old settings
    return(invisible(p))
  }
}


############################################################################%###
############################# plot_itempool_hist ###############################
############################################################################%###

############################# plot_itempool_hist ###########################%###
#' Plot plot.Itempool(..., type = "pars")
#'
#' @noRd
plot_itempool_hist <- function(ip,
                               title = "",
                               suppress_plot = FALSE,
                               base_r_graph = FALSE,
                               ...) {
  args <- list(...)
  # Check focus item and convert it to an item_id value if it is numeric

  ip_df <- get_data_plot_itempool_pars(ip)
  multi_model <- any(ip$model != ip$model[1])

  if (!is.null(title) && title == "") title <- "Parameter Values"

  # First try ggplot2:
  if (!base_r_graph && requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot() +
      ggplot2::geom_histogram(data = ip_df,
                              mapping = ggplot2::aes_string(x = "value"),
                              bins = 25, color = "black", fill = "gray")
    if (multi_model) {
      p <- p + ggplot2::facet_wrap(~model + par_name, scales = "free")
    } else {
      p <- p + ggplot2::facet_wrap(~par_name, scales = "free")
    }
    p <- p + ggplot2::ggtitle(title) +
        ggplot2::theme_bw()
    if (suppress_plot) return(p) else suppressMessages(print(p))
  } else {# Try base R graphics
    gd <- split(ip_df, list(ip_df$model, ip_df$par_name), drop = TRUE)
    temp_col_num <- min(3, length(gd))  # number of columns of the graph
    old_par <- graphics::par(no.readonly = TRUE) # Save old settings

    graphics::par(mfrow = c(ceiling(length(gd) / temp_col_num), temp_col_num),
        mar = c(3, 1, 2, 1))
    for (i in 1:length(gd)) {
      temp_gd <- gd[[i]]
      graphics::hist(
        temp_gd$value,
        main = paste0(ifelse(multi_model, paste0(temp_gd$model[1], " - "), ""),
                      temp_gd$par_name[1]))
    }
    # dev.off()
    p <- grDevices::recordPlot()
    graphics::par(old_par) # Restore to old settings
    return(invisible(p))
  }
}







############################################################################%###
############################################################################%###
############################# plot.Itempool ####################################
############################################################################%###
############################################################################%###

#' Plot Item Characteristic Curves or Test Characteristic Curve of an
#' \code{Itempool} object
#'
#' @description
#' \code{plot.Itempool} plots the item characteristic curves (item response
#' curves) or test characteristic curve of an \code{\link{Itempool-class}}
#' object.
#'
#' @param x An \code{\link{Itempool-class}} object.
#' @param theta_range Either a numeric vector of length two setting the
#'   boundaries of x-axis, e.g. \code{c(-4, 4)}, or, a numeric vector that is
#'   includes the theta values  that will be plotted, e.g.
#'   \code{seq(-3, 3, by = 0.1)}.
#' @param type The type of the graph. The default value is \code{"icc"}.
#'   Available options are:
#'   \describe{
#'     \item{\strong{\code{"icc"}}}{Plot item characteristic curve of each item}
#'     \item{\strong{\code{"tcc"}}}{Plot test characteristic curve}
#'     \item{\strong{\code{"hist"}}}{Plot histograms of item parameters}
#'     \item{\strong{\code{"pars"}}}{Plot dot plot of item parameters}
#'   }
#' @param tcc_prop_corr If \code{TRUE}, test characteristic curve will be
#'          show the proportion correct of the test (i.e. the range of y-axis
#'          will be 0-1 instead of 0 to the number of items).
#' @param focus_item A character string of the 'item_id' of the item to be
#'   focused. If \code{type = "pars"}, this item will be shown with a
#'   red dot to distinguish it from others.
#' @param title Title of the plot. Set \code{title = NULL} to suppress the plot
#'   title. The default is \code{""}.
#'   If \code{type = "tcc"} and \code{title = ""}, title will be 'Test
#'   Characteristic Curve'. If \code{type = "icc"} and \code{title = ""}, title
#'   will be 'Item Characteristic Curve'. If \code{type = "hist"} or
#'   \code{type = "pars"} and \code{title = ""}, title will be
#'   'Parameter Values'.
#' @param suppress_plot If \code{FALSE} the function will print the plot. If
#'          \code{TRUE}, function will return the plot object. Default value is
#'          \code{FALSE}.
#' @param legend_title The title of the plot's legend.
#' @param base_r_graph If \code{TRUE} function will plot graphs using base R
#'   graphics. If \code{FALSE} the function will check whether 'ggplot2' package
#'   is installed. If it is installed, it will use 'ggplot2' package for the
#'   plot. The default value is \code{FALSE}.
#' @param y_lim A numeric vector of length two representing the lower and
#'   upper bound of y-axis.
#' @param ... Additional arguments that will be passed to \code{geom_line}
#'
#' @return Depending on the value of \code{suppress_plot} function either prints
#' the item characteristic curve or returns the plot object.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' ip <- generate_ip(n = sample(10:15,1))
#' plot(ip)
#'
#' # Additional arguments will passed to geom_line
#' plot(ip, size = .25, alpha = 0.3)
#'
#' # Set the boundaries of the graph
#' plot(ip, theta_range = c(-2, 2))
#' # alternatively provide theta values
#' plot(ip, theta_range = seq(-6, 6, by = 0.25))
#'
#'
#' # Test Characteristic Curve
#' plot(ip, type = "tcc")
#'
#' # Proportion correct for test characteristic curve
#' plot(ip, type = "tcc", tcc_prop_corr = TRUE)
#'
#' # Plot histogram of item parameters
#' plot(ip, type = "hist")
#'
#'
#' \dontrun{
#' # Item parameter summary
#' ip <- generate_ip(n = 200)
#' plot(ip, type = "pars")
#' plot(ip, type = "pars", dotsize = .75)
#' plot(ip, type = "pars", focus_item = "Item_22")
#' # Use base R graphics
#' plot(ip, type = "pars", base_r_graph = TRUE)
#'
#' # # Remove the legend altogether
#' # plot(ip, suppress_plot = TRUE) + ggplot2::theme(legend.position="none")
#' # # Change the labels:
#' # plot(ip, suppress_plot = TRUE) +
#' #   ylab("Probability") + xlab("Ability Score")
#' }
#'
plot.Itempool <- function(x,
                          theta_range = c(-4,4),
                          type = "icc",
                          tcc_prop_corr = FALSE,
                          focus_item = NULL,
                          title = "",
                          suppress_plot = FALSE,
                          legend_title = NULL,
                          base_r_graph = FALSE,
                          y_lim = NULL,
                          ...) {
  if (type == "pars") {
    plot_itempool_pars(ip = x, title = title,
                       suppress_plot = suppress_plot, focus_item = focus_item,
                       base_r_graph = base_r_graph, ...)
  } else if (type == "hist") {
    plot_itempool_hist(ip = x, title = title,
                       suppress_plot = suppress_plot,
                       base_r_graph = base_r_graph, ...)
  } else if (type == "tcc") {
    plot_itempool_tcc(ip = x, theta_range = theta_range, title = title,
                      tcc_prop_corr = tcc_prop_corr,
                      suppress_plot = suppress_plot,
                      base_r_graph = base_r_graph, y_lim = y_lim, ...)
  } else if (type == "icc") {
    plot_itempool_icc(ip = x,
                      focus_item = focus_item,
                      theta_range = theta_range,
                      title = title,
                      legend_title = legend_title,
                      suppress_plot = suppress_plot,
                      base_r_graph = base_r_graph,
                      y_lim = y_lim,
                      ...)
  } else {
    stop("Invalid 'type' value. Please choose one of the following: ",
         "'icc', 'tcc' or 'pars'.")
  }
}



#' #########################################################################%###
#' ############################# hist.Itempool #############################%###
#' #########################################################################%###
#' #' Plot Test Information Function
#' #' @description
#' #' \code{hist.Itempool} Plots the histogram of the item pool.
#' #'
#' #' @param ip An \code{\link{Itempool-class}} object.
#' #' @param parameter Which item pool parameter to plot.
#' #'                  The choices are: 'a': item discrimination parameter, $a$.
#' #'                                   'b': item difficulty parameter, $b$.
#' #'                                   'c': pseudo-guessing parameter, $c$.
#' #'                  The default value is 'b'.
#' #' @param D Scaling constant.
#' #' @param xlim If
#' #' @param separateContent Whether to separate the content areas in the
#' #'                        histogram. If there are no content areas in the
#' #'                        item pool, there won't be any separation.
#' #'                        Default is \code{FALSE}.
#' #' @param alpha The transparency of the bins.It takes values between 0 and 1.
#' #'              By default, if \code{separateContent} is \code{TRUE} then
#' #'              the value will be 0.5, it will be 1 otherwise.
#' #' @param addTIFline If \code{TRUE}, a test information line will be added
#' #'                   to the histogram. There will be secondary y-axis to the
#' #'                   right of the graph. If \code{separateContent} is
#' #'                   \code{TRUE}, the the TIF will be added to the each
#' #'                   content area. The default is \code{FALSE}.
#' #' @param suppress_plot If \code{FALSE} the function will print the plot. If
#' #'          \code{TRUE}, function will return the plot object. Default value
#' #'           is \code{FALSE}.
#' #' @param ... The arguments that will be passed to the \code{plotHistogram}
#' #'            function.
#' #'
#' #' @return Depending on the value of \code{suppress_plot} function either
#' #' prints a histogram of item pool or returns the plot object.
#' #'
#' #' @export
#' #' @author Emre Gonulates
#' #' @seealso \code{\link{plotHistogram}}
#' #'
#' #' @section To-do:
#' #' \itemize{
#' #'   \item Add title example.
#' #'   \item Add suppress_plot=FALSE example.
#' #' }
#' #' @examples
#' #' \donttest{
#' #' n <- sample(10:50,1);   D = 1
#' #' ip <- itempool(data.frame(a = runif(n, .5, 2), b = rnorm(n),
#' #'                              c = runif(n, 0, .3)))
#' #' plotItemPool(ip)
#' #'
#' #' n <- c(15, 40, 10)
#' #' ip <- itempool(data.frame(
#' #'   a = runif(sum(n), 0.75, 2),
#' #'   b = c(sort(rnorm(n = n[1], mean = 1, sd = 0.3)),
#' #'         sort(rnorm(n = n[2], mean = 0, sd = 0.5)),
#' #'         sort(rnorm(n = n[3], mean = -1, sd = 0.4))),
#' #'   c = runif(sum(n), 0.01, 0.3)
#' #'   ),
#' #'   item_id = paste0("Item_",1:sum(n)),
#' #'   content = c(rep("Trigonometry", n[1]), rep("Algebra", n[2]),
#' #'               rep("Arithmetic", n[3]))
#' #' )
#' #'
#' #' plotItemPool(ip = ip)
#' #' plotItemPool(ip = ip, binWidth = 0.05)
#' #' plotItemPool(ip = ip, separateContent = TRUE)
#' #' plotItemPool(ip = ip, separateContent = TRUE, addTIFline = TRUE)
#' #' plotItemPool(ip = ip, addTIFline = TRUE)
#' #' plotItemPool(ip = ip, variableLabel = "Item Difficulty Parameter")
#' #' plotItemPool(ip = ip, parameter = 'c', binWidth = 0.025)
#' #' plotItemPool(ip = ip, parameter = 'c', separateContent = TRUE, alpha = .2)
#' #' plotItemPool(ip = ip, parameter = 'a', binWidth = 0.025)
#' #' plotItemPool(ip = ip, parameter = 'a', binWidth = 0.05,
#' #'              separateContent = T)
#' #' plotItemPool(ip = ip, separateContent = T,
#' #'              addTextBox = c("n", 'meanX', 'sdX'))
#' #' plotItemPool(ip = ip, separateContent = TRUE, theme = "theme_bw")
#' #'
#' #' p <- plotItemPool(ip = ip, suppress_plot = TRUE)
#' #'
#' #' # Add another item pool
#' #' ip2 <- itempool(data.frame(a = runif(n[2], .5, 2), b = rnorm(n[2]),
#' #'                               c = runif(n[2], 0, .3)))
#' #' plotItemPool(ip = ip2, binWidth = 0.05)
#' #' plotItemPool(ip = ip2, addTIFline = TRUE,
#' #'              variableLabel = "Item Difficulty")
#' #' plotItemPool(ip = ip2, binWidth = 0.1, seperateContent = TRUE)
#' #' }
#' hist.Itempool <- function(ip, parameter = 'b', xlim = NULL,
#'                           separateContent = FALSE, alpha = NULL,
#'                           addTIFline = FALSE, suppress_plot = FALSE, ...) {
#'   # Convert ip to Itempool object
#'   if (!is(ip, "Itempool"))
#'     tryCatch(
#'       ip <- itempool(ip),
#'       error = function(cond) {
#'         message("\nip cannot be converted to an 'Itempool' object. \n")
#'         stop(cond)
#'       })
#'   argList <- list()
#'   # Determine x-axis label
#'   if (!hasArg(variableLabel)) {
#'     if (parameter == 'b') {variableLabel <- "Item Difficulty (b)"
#'     } else if (parameter == 'a') {variableLabel <- "Item Discrimination (a)"
#'     } else if (parameter == 'c') variableLabel <- "Guessing Parameter (c)"
#'     argList <- c(argList, variableLabel = variableLabel)
#'   }
#'   # Determine bin width
#'   if (!hasArg(binWidth)) {
#'     if (parameter == 'b') {binWidth <- 0.1
#'     } else if (parameter == 'a') {binWidth <- 0.05
#'     } else if (parameter == 'c') binWidth <- 0.02
#'     argList <- c(argList, binWidth = binWidth)
#'   }
#'   ipData <- data.frame(getParameters(object = ip,
#'                                      parameterNames = parameter))
#'   colnames(ipData) <- parameter
#'   # Decide whether to separate histogram by content
#'   separateContent <- separateContent & !is.null(ip$content)
#'   # Add content column if separateContent is TRUE
#'   if (separateContent)
#'     ipData$content <- ip$content
#'   alpha <- ifelse(is.null(alpha), ifelse(separateContent, 0.5, 1), alpha)
#'   # Here, since I'm defining variableLabel and binWidth within this function,
#'   # to prevent any clash, I redefined '...'.
#'   # https://stackoverflow.com/a/17390262/2275286
#'   inargs <- list(...)
#'   argList[names(inargs)] <- inargs
#'   if (requireNamespace("ggplot2", quietly = TRUE)) {
#'     p <- do.call(plotHistogram,
#'                  c(list(data = ipData, variable = parameter, alpha = alpha,
#'                         group = switch(separateContent + 1, NULL, "content"),
#'                         legendTitle = switch(separateContent + 1, NULL,
#'                                              "Content"),
#'                         suppress_plot = FALSE), argList)
#'                  )
#'     if  (parameter == 'b' & addTIFline)
#'     {
#'       if (is.null(xlim))
#'         xlim <- seq(from = -4, to = 4, length.out = 201)
#'       else xlim <- seq(from = xlim[1], to = xlim[2], length.out = 201)
#'       if (separateContent)
#'       {
#'         infoData <- data.frame(theta = numeric(0), information = numeric(0),
#'                                content = character(0))
#'         for (i in sort(unique(ip$content)))
#'           infoData <- rbind(infoData, data.frame(
#'             theta = xlim,
#'             information = info(ip = getContentFast(ip = ip, content = i),
#'                                theta = xlim, tif = TRUE),
#'             content = i))
#'       } else {
#'         infoData <- data.frame(theta = xlim, information = info(
#'           ip = ip, theta = xlim, tif = TRUE))
#'       }
#'       # Find the highest point in histogram
#'       max_hist <- max(ggplot_build(p)$data[[1]]$count)
#'       # Find the highest point in info graph
#'       max_info <- max(infoData$information)
#'       # Find the ratio of max_info/max_hist:
#'       expansion_ratio <- round(max_info/max_hist, 1)
#'       infoData$information <- infoData$information/expansion_ratio
#'
#'       # https://stackoverflow.com/q/3099219/2275286
#'       p <- p + ggplot2::geom_line(data = infoData, ggplot2::aes_string(
#'         x = 'theta', y = 'information',
#'         color = switch(separateContent + 1, NULL, "content")), size = 1) +
#'         scale_y_continuous(name = "Count", sec.axis = sec_axis(
#'           ~.*expansion_ratio, name = "Item Pool Information")) +
#'         ggplot2::scale_color_discrete(name = "Content")
#'     }
#'     if (suppress_plot) return(p) else print(p)
#'   }
#' }

