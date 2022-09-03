
.get_x_axis_scale <- function(x_axis_scale = NULL, ip = NULL,
                              criterion = NULL) {
  if (is.null(x_axis_scale)) {
    if (is.null(criterion) && is.null(ip)) x_axis_scale <- "percent" else
      x_axis_scale <- "criterion"
  } else if (x_axis_scale == "criterion" && (is.null(criterion) &&
                                             is.null(ip))) {
    message("x_axis_scale = 'criterion' is available only with valid ",
            "'criterion' or 'ip'. x_axis_scale is set to 'percent'.")
    x_axis_scale <- "percent"
  } else if (!x_axis_scale %in% c("percent", "criterion", "number")) {
    stop("Invalid 'x_axis_scale' value.")
  }
  return(x_axis_scale)
}


############################# get_data_plot_distractor_icc #################%###

#' Get data for plot_distractor_icc()
#'
#' @noRd
get_data_plot_distractor_icc <- function(raw_resp, item, key = NULL, bins = 10,
                                         x_axis_scale = NULL, ip = NULL,
                                         criterion = NULL, x_lim = NULL,
                                         add_icc = FALSE) {
  if (is(raw_resp, "Response_set")) {
    raw_resp <- as.matrix(raw_resp, ip = ip, output = "raw_response")
  }
  x_axis_scale <- .get_x_axis_scale(x_axis_scale = x_axis_scale, ip = ip,
                                    criterion = criterion)

  # raw_resp <- convert_to_resp_set(resp = raw_resp, ip = ip,
  #                                 object_name = "resp",
  #                                 enforce_data_type = "raw_response")

  # raw_resp should be a matrix or data.frame
  if (!inherits(raw_resp, c("matrix", "data.frame")))
    stop("'raw_resp' should be a matrix or a data.frame object.")
  # There should be at least two bins and bins should be a number.
  if (!is.numeric(bins) || as.integer(bins) != bins || bins < 2)
    stop("'bins' should be an integer larger than 2.")
  # Check 'item' argument if it is character it should be an item ID, if it
  # is a number it should be a
  item_id <- NULL
  if (is.numeric(item) && (length(item) == 1) && (item <= ncol(raw_resp))) {
    item_col_no <- item
    item_id <- ifelse(is.null(colnames(raw_resp)), item,
                      colnames(raw_resp)[item])
  } else if (is.character(item) && (length(item) == 1) &&
             item %in% colnames(raw_resp)) {
    if (item %in% colnames(raw_resp)) {
      item_col_no <- which(item == colnames(raw_resp))
    } else {
      item_col_no <- which(item == colnames(raw_resp))
    }
    item_id <- item
    if (!is.null(ip) && !item_id %in% ip$item_id) {
      stop("Invalid 'item_id'. 'item_id' is not in the item pool.",
           call. = FALSE)
    }
  } else stop("Invalid 'item' argument. Please provide a valid column ",
              "number, column name or the item 'ID'. Only provide one 'item'.")


  # Valid key
  if (is.null(key) && !is.null(ip)) key <- ip$key
  if (length(key) != ncol(raw_resp))
    stop("Invalid 'key'. The length of 'key' should be the same as the number ",
         "of columns of 'raw_resp'.")

  # This conversion is necessary to implement a subsetting method that is
  # valid for both matrix, data.frame and tibbles.
  if (inherits(raw_resp, "matrix"))
    raw_resp <- data.frame(raw_resp, check.names = FALSE)

  # Sort the data using the total scores and extract only the relevant column
  # and the total score
  # Check criterion and Itempool
  if (is.numeric(criterion) && length(criterion) == nrow(raw_resp)) {
    gd_pre <- data.frame(item_id = item_id, ts = criterion)
  } else if (is(ip, "Itempool")) {
    message("Estimating the criterion variable using EAP estimation. ",
            "'criterion' values can be provided to speed up the function. ")
    gd_pre <- data.frame(item_id = item_id, ts = est_ability(
      ip = ip, resp = score_raw_resp(raw_resp, key), method = "eap")$est)
  } else {# Calculate the sum scores
    gd_pre <- data.frame(item_id = item_id,
                     ts = rowSums(score_raw_resp(raw_resp, key), na.rm = TRUE))
  }
  gd_pre$response <- ifelse(raw_resp[[item_col_no]] == key[item_col_no],
                            paste0(key[item_col_no], "*"),
                            raw_resp[[item_col_no]])
  # Remove rows where response is NA
  gd_pre <- gd_pre[!is.na(gd_pre$response), ]
  gd_pre <- gd_pre[!is.na(gd_pre$ts) & !is.nan(gd_pre$ts), ]

  # Decrease bin number until there are no empty bins
  repeat {
    q <- stats::quantile(
      gd_pre$ts,
      probs = seq(0, 1, length.out = bins + 1))[-c(1, bins + 1)]
    if (any(duplicated(q)) || any(range(gd_pre$ts, na.rm = TRUE) %in% q)) {
      bins <- bins - 1
      next
    }
    break
  }

  if (x_axis_scale == "percent") {
    names(q) <- paste0(round(as.numeric(gsub("[%]", "", names(q)))), "%")
    temp_labels <- paste0(c("0", names(q)), " - ", c(names(q), "100%"))
  } else if (x_axis_scale == "number") {
    temp_labels <- paste0(1:bins)
  } else if (x_axis_scale == "criterion") {
    temp_labels <- unname(round((c(q, max(gd_pre$ts)) +
                                   c(min(gd_pre$ts), q)) / 2, 2))
  } else stop("Invalid 'x_axis_scale' value.")

  q <- c(min(gd_pre$ts, na.rm = TRUE), q, max(gd_pre$ts, na.rm = TRUE))

  gd_pre$bin <- cut(gd_pre$ts, breaks = q, include.lowest = TRUE,
                    labels = temp_labels)
  gd <- as.data.frame(t(prop.table(table(Response = gd_pre$response,
                                         bin = gd_pre$bin), margin = 2)))
  gd <- cbind(item_id = item_id, gd)
  # gd$incorrect <- !grepl("[*]$", gd$Response)
  gd$correct <- factor(ifelse(grepl("[*]$", gd$Response),
                              "Correct", "Incorrect"))


  if (x_axis_scale == "criterion") {
    gd$bin <- as.numeric(as.character(gd$bin))
  }

  return(gd)
}



############################################################################%###
############################# plot_distractor_icc ##########################%###
############################################################################%###
#' Plot Empirical Item or Test characteristic curve
#' @description
#' \code{plot_empirical_icc} plots empirical item or test characteristic curve.
#'
#' @param raw_resp Raw response matrix.
#' @param item The column number, column name or the 'ID' of the  the item that
#'   should be plotted.
#' @param key A vector of answer key. If \code{key = NULL}, the function will
#'   check whether the item pool has keys by checking \code{ip$key} and raise
#'   an error if \code{ip$key} is not valid.
#' @param ip An \code{\link{Itempool-class}} object that is needed for some
#'   plots. If \code{ip} provided and \code{criterion} is not provided, then
#'   ability will be estimated using EAP method with prior mean 0 and prior
#'   standard deviation of 1. This is a slower method depending on the size of
#'   the data. Also, the key for items can be provided via \code{ip$key}.
#' @param criterion A vector of examinee abilities. If \code{criterion} values
#'   provided the bins are formed using them instead of sum scores.
#' @param bins An integer larger than 2 representing of ability groups examinees
#'   should be grouped into. The default is \code{10}. The maximum value of
#'   \code{bins +  1} is the number of possible total scores.
#' @param x_axis_scale Set the scale of the x-axis. The default value is
#'   \code{NULL}. For if sum score is used scale will be defaulted to
#'   \code{"percent"}, Otherwise if valid \code{criterion} or \code{ip}
#'   arguments provided  the scale defaults to \code{"criterion"}.
#'   \describe{
#'     \item{\strong{\code{"percent"}}}{Percent interval.}
#'     \item{\strong{\code{"number"}}}{Numbers between 1 and \code{bins}.}
#'     \item{\strong{\code{"criterion"}}}{Criterion values equally divided into
#'     bins. the middle value of the bin is shown in the x-axis. For example, if
#'     \code{bins = 10}, the first tick of the x-axis will be the mean of
#'     minimum criterion value and tenth percentile criterion value.}
#'     }
#' @param add_icc If \code{TRUE}, adds item characteristic curve to the plot.
#'   Only available if a valid item pool object (\code{ip}) is provided and
#'   \code{x_axis_scale = "criterion"}. The default value is \code{FALSE}.
#' @param title Title of the plot. If the value is \code{NULL}, the plot title
#'   will be suppressed.
#' @param n_dodge The number of lines the x-axis tick labels should be written
#'   to. This is especially useful if the x-axis tick labels overlap with each
#'   other. The default value is \code{1}, which means all of the labels are
#'   written on the same line.
#' @param x_lim The limits of x axis in the form \code{c(-4, 4)}. Only available
#'   when \code{x_axis_scale = "criterion"}. The default value is \code{NULL}
#'   where the limits will be the minimum and maximum 'criterion' values.
#' @param base_r_graph If \code{TRUE} function will plot graphs using base R
#'   graphics. If \code{FALSE} the function will check whether 'ggplot2' package
#'   is installed. If it is installed, it will use 'ggplot2' package for the
#'   plot. The default value is \code{FALSE}.
#' @param suppress_plot If \code{FALSE} the function will print the plot. If
#'          \code{TRUE}, function will return the plot object. Default value is
#'          \code{FALSE}.
#' @param ... Extra parameters that will pass to \code{geom_line}.
#'
#' @return Depending on the value of \code{suppress_plot} function either prints
#' the proportion of examinees in each bin respond to each distractor or
#' returns the plot object.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' n_item <- 10 # sample(8:12, 1)
#' n_theta <- 10000 # sample(100:200, 1)
#' raw_resp <- matrix(sample(LETTERS[1:4], n_item * n_theta, replace = TRUE),
#'                    nrow = n_theta, ncol = n_item,
#'                    dimnames = list(paste0("Examinee-", 1:n_theta),
#'                                    paste0("Item_", 1:n_item)))
#' key <- sample(LETTERS[1:4], n_item, replace = TRUE)
#' plot_distractor_icc(raw_resp, 3, key)
#' # Change the number of bins
#' plot_distractor_icc(raw_resp, 3, key, bins = 15)
#'
plot_distractor_icc <- function(raw_resp, item, key = NULL,
                                ip = NULL, criterion = NULL,
                                bins = 10, x_axis_scale = NULL,
                                add_icc = FALSE,
                                title = "", n_dodge = 1, x_lim = NULL,
                                base_r_graph = FALSE,
                                suppress_plot = FALSE, ...) {

  # Set the default x_axis_scale:
  x_axis_scale <- .get_x_axis_scale(x_axis_scale = x_axis_scale, ip = ip,
                                    criterion = criterion)
  # Set the x-axis label
  if (x_axis_scale == "criterion" || !is.null(criterion) || !is.null(ip)) {
    x_label <- "Ability Group (Criterion Score)"
  } else x_label <- "Ability Group (Sum Score)"
  y_label <- "Proportion"

  gd <- get_data_plot_distractor_icc(
    raw_resp = raw_resp, item = item, key = key, bins = bins,
    x_axis_scale = x_axis_scale, ip = ip, criterion = criterion)

  # Set the graph title:
  if (!is.null(title) && title == "")
    title <- paste0("Trace Lines for ", ifelse(
      is.numeric(gd$item_id), paste0("Item ", gd$item_id[1]),
      paste0("'", gd$item_id[1], "'")))

  responses <- sort(unique(gd$Response))
  resp_colors <- stats::setNames(ifelse(grepl("\\*", responses), "black", "*"),
                                 responses)
  resp_colors[resp_colors == "*"] <- c("tomato1", "cyan", "coral4")
  line_types <- stats::setNames(ifelse(grepl("\\*", responses), 1, 2),
                                responses)
  y_lim <- c(0, 1)

  # Decide whether to add ICC
  add_icc <- add_icc && x_axis_scale == "criterion" && is(ip, "Itempool")

  if (add_icc) {
    x_lim <- switch(is.null(x_lim) + 1, x_lim, c(min(gd$bin, na.rm = TRUE),
                                                 max(gd$bin, na.rm = TRUE)))
    theta_interval <- seq(from  = x_lim[1], to = x_lim[2], by = 0.01)
    icc_data <- data.frame(
      criterion = theta_interval,
      p = prob(ip = ip[[which(gd$item_id[1] == ip$resp_id)]],
               theta = theta_interval)[, 2])
  }


  ### ggplot2 ###
  if (!base_r_graph && requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(gd, ggplot2::aes_string(x = "bin", y = "Freq",
                                                 color = "Response")) +
      ggplot2::geom_point(alpha = 0.5, size = 2) +
      ggplot2::geom_line(ggplot2::aes_string(linetype = "Response",
                                             group = "Response"),
                size = 1, alpha = 0.75, ...)  +
      ggplot2::scale_linetype_manual(values = line_types, name = "Response") +
      ggplot2::scale_color_manual(values = resp_colors, name = "Response")
        # guide = "none",


    if (x_axis_scale != "criterion") {
      p <- p + ggplot2::scale_x_discrete(
        guide = ggplot2::guide_axis(n.dodge = n_dodge))
    } else {
      x_lim <- switch(is.null(x_lim) + 1, x_lim, c(min(gd$bin, na.rm = TRUE),
                                                   max(gd$bin, na.rm = TRUE)))
      p <- p + ggplot2::xlim(x_lim)
    }

    if (add_icc) {
      p <- p + ggplot2::geom_line(
        data = icc_data,
        mapping = ggplot2::aes_string(x = "criterion", y = "p"),
        color = "red", alpha = .5, size = 1)
    }

    p <- p +
      ggplot2::labs(title = title, x = x_label, y = y_label) +
      ggplot2::ylim(y_lim) +
      ggplot2::theme_bw()
    if (suppress_plot) return(p) else print(p)
  ### Base R Graphics ###
  } else {
    old_par <- graphics::par(no.readonly = TRUE) # Save old settings

    graphics::par(mar = c(5.1, 4.1, 4.1, 6.1))
    graphics::plot.default(
      x = gd$bin,
      y = rep(0, nrow(gd)),
      ylab = y_label,
      xlab = x_label,
      ylim = y_lim,
      main = title,
      panel.last = graphics::grid(),
      xaxt = "n",
      type = "n")

    # Add distractor plots
    for (i in sequence(responses)) {
      temp <- gd[gd$Response == responses[i], ]
      graphics::lines(x = temp$bin, y = temp$Freq, type = "l",
                      col = resp_colors[i], lty = line_types[i])
      graphics::points(x = temp$bin, y = temp$Freq, col = resp_colors[i],
                       lty = line_types[i])
    }
    if (add_icc) {
      graphics::lines(x = icc_data$criterion, y = icc_data$p, type = "l",
            col = "red", lty = 1)
    }
    graphics::axis(side = 1, at = as.numeric(temp$bin), labels = temp$bin)

    graphics::legend("topleft", legend = as.character(responses), col = resp_colors, lty = line_types,
                     xpd = TRUE, inset = c(1, 0), lwd = 2, bty = "n",
                     title = "Response")

    # dev.off()
    p <- grDevices::recordPlot()
    graphics::par(old_par) # Restore to old settings
    return(invisible(p))
  }
}

