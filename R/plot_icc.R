
############################################################################%###
############################# get_item_id_col_num ##########################%###
############################################################################%###
#' Get item ID and item column number using response matrix and 'item'
#' information.
#' 'item' argument should be either (1) a single string representing
#' the item's ID or (2) a single integer representing the column number
#' (3)
#'
#' @noRd
get_item_id_col_num <- function(resp, item, ip = NULL) {
  item_id <- NULL
  if (!inherits(resp, c("matrix", "data.frame")))
    stop("Invalid 'resp' argument. 'resp' should be a matrix or a
         data.frame object.")
  if (is.numeric(item) && (length(item) == 1) && (item <= ncol(resp))) {
    item_col_no <- item
    if (!is.null(colnames(resp))) {
      item_id <- colnames(resp)[item]
    } else if (!is.null(ip) && item %in% ip$item_id) {
      item_id <- ip$item_id[item]
    }
  } else if (is.character(item) && (length(item) == 1) &&
            (item %in% colnames(resp) ||
             (inherits(ip, "Itempool") && item %in% ip$resp_id))) {
    if (item %in% colnames(resp)) {
      item_col_no <- which(item == colnames(resp))
    } else {
      item_col_no <- which(item == ip$resp_id)
    }
    item_id <- item
  } else
    stop("Invalid 'item' argument. Please provide a valid column number, ",
         "column name or the item 'ID'. Only provide one 'item'.",
         call. = FALSE)
  return(list(item_id = item_id, col_num = item_col_no))
}


############################################################################%###
############################# get_data_plot_empirical_icc ##################%###
############################################################################%###
#' Gets data for Empirical Item Characteristic Curve
#'
#' @noRd
#'
get_data_plot_empirical_icc <- function(resp, item, ip, theta = NULL,
                                        bins = 10, binwidth = NULL) {
  # There should be at least two bins and bins should be a number.
  if (is.null(binwidth) &&
      (!is.numeric(bins) || as.integer(bins) != bins || bins < 2))
    stop("'bins' should be an integer larger than 2.")

  ### Itempool checks ###
  supported_models <- c(UNIDIM_DICHO_MODELS, UNIDIM_POLY_MODELS)
  if (!is(ip, "Itempool")) {
    stop(paste0("Invalid 'ip'. A valid Itempool object is necessary."))
    # Currently only dichotomous IRT models supported.
  } else if (!all(ip$item_model %in% supported_models))  {
    stop(paste0("Currently, only following models are supported by this ",
                "function: ",
                paste0("'", supported_models, "'", collapse = ", ")),
         call. = FALSE)
  }

  # resp should be a matrix or data.frame
  resp_set <- convert_to_resp_set(resp = resp, ip = ip)
  resp <- as.matrix(resp_set, ip = ip)

  item <- get_item_id_col_num(resp = resp, item = item, ip = ip)

  ### theta checks ###
  # Check whether graph type requires a theta:
  if (!is.numeric(theta)) {
    # Check whether theta can be estimated
    if (nrow(resp) > 1000)
      message(paste0("Estimating theta. This funtion will run faster if ",
                     "'theta' is provided to the function. Try running:\n\n",
                     "    theta <- est_ability(ip = ip, resp = resp, ",
                     "method = \"eap\")$est\n\nand supplying the resulting ",
                     "'theta' variable to the funciton."),
         call. = FALSE)
    theta <- est_ability(ip = ip, resp = resp, method = "eap")$est
  }

  # Theta length should be equal to the number of rows of the resp.
  if (length(theta) != nrow(resp))
    stop("Invalid 'theta'. The length of theta should be equal to the ",
         "number of columns of 'resp'.", call. = FALSE)

  # Convert to data.frame without changing the column names
  gd <- data.frame(resp, check.names = FALSE)
  # Add total score
  gd$ts <- theta

  # Sort the data using the total scores and extract only the relevant column
  # and the total score
  gd <- gd[order(gd$ts), c(item$col_num, ncol(gd))]
  colnames(gd) <- c("response", "ts")
  # Remove rows where response is NA
  gd <- gd[!is.na(gd$response), ]
  gd <- gd[!is.na(gd$ts), ]

  if (is.null(binwidth) || !(is.numeric(binwidth) && length(binwidth) == 1)) {
    repeat {
      q <- stats::quantile(
        gd$ts, probs = seq(0, 1, length.out = bins + 1))[-c(1, bins + 1)]
      if (any(duplicated(q)) || any(range(gd$ts, na.rm = TRUE) %in% q)) {
        bins <- bins - 1
        next
      }
      break
    }
    temp_labels <- unname(round((c(q, max(gd$ts)) + c(min(gd$ts), q)) / 2, 2))
    bins <- c(min(gd$ts, na.rm = TRUE), q, max(gd$ts, na.rm = TRUE))
  } else {
    bins <- seq(from = floor(10 * min(gd$ts))/10,
                to = ceiling(10 * max(gd$ts)) / 10, by =  binwidth)
    temp_labels <- (bins[-1] + bins[-length(bins)]) / 2
  }

  gd$bin <- cut(gd$ts, breaks = bins, include.lowest = TRUE,
                  labels = temp_labels)
  gd$response <- factor(gd$response, ordered = TRUE)

  temp_freq_total <- as.data.frame(table(bin = gd$bin),
                                   responseName = "total_freq")
  # temp_freq_items <- as.data.frame(table(bin = gd$bin, response = gd$response))
  gd <- as.data.frame(t(prop.table(table(Response = gd$response, bin = gd$bin),
                                   margin = 2)), responseName = "Observed")

  gd <- merge(gd, temp_freq_total, by = "bin")

  # If there are just two categories, remove the first category.
  if (length(unique(gd$Response)) == 2)
    gd <- gd[gd$Response == sort(unique(gd$Response))[2], ]

  # Remove the bins that do not have any observations/responses
  gd <- gd[!is.nan(gd$Observed),]

  ip_standalone <- flatten_itempool_cpp(ip)
  gd$Expected <- resp_lik(
    ip = ip_standalone[[item$item_id]],
    resp = as.integer(as.character(gd$Response)),
    theta = as.numeric(as.character(gd$bin)))
  gd <- stats::reshape(
    data = gd, varying = c("Observed", "Expected"), v.names = "pvalue",
    direction = "long", timevar = "Type",
    times = c("Observed", "Expected"))
  gd$item_id <- NULL
  gd$bin <- as.numeric(as.character(gd$bin))
  gd <- gd[order(gd$Type, gd$bin), ]
  rownames(gd) <- NULL
  gd <- cbind(item_id = item$item_id, gd)
  return(gd)
}



############################################################################%###
############################# plot_empirical_icc ###########################%###
############################################################################%###
#' Plot Empirical Item characteristic curve
#'
#' @description
#' \code{plot_emprical_icc} plots empirical item characteristic curve.
#' It plots observed p-values vs. expected p-values grouped into bins based
#' theta scores (or any score supplied). Optionally, provide \code{theta}
#' vector, otherwise examinee abilities will be estimated by
#' \code{est_ability(..., type = "eap")}. This will slow down the plotting
#' function.
#' @param resp Response matrix.
#' @param item The column number, column name or the 'ID' of the  the item that
#'   should be plotted.
#' @param ip An \code{\link{Itempool-class}} object that is needed for some
#'   plots.
#' @param theta A vector of examinee abilities.
#' @param bins An integer larger than 2 representing of ability groups examinees
#'   should be grouped into. The default is \code{10}. The maximum value of
#'   \code{bins +  1} is the number of possible total scores.
#' @param binwidth This determines  the width of each bin of the theta scale.
#'   Within each bin, there might be different number of examinees.
#' @param title Title of the plot. The default value is \code{""}, where title
#'   of the plot will be "Trace Plot of <Item ID>". If the value is \code{NULL},
#'   the plot title will be suppressed.
#' @param suppress_plot If \code{FALSE} the function will print the plot. If
#'          \code{TRUE}, function will return the plot object. Default value is
#'          \code{FALSE}.
#' @param base_r_graph If \code{TRUE} function will plot graphs using base R
#'   graphics. If \code{FALSE} the function will check whether 'ggplot2' package
#'   is installed. If it is installed, it will use 'ggplot2' package for the
#'   plot. The default value is \code{FALSE}.
#' @param ... Extra parameters that will pass to \code{geom_line}.
#'
#' @return Depending on the value of \code{suppress_plot} function either prints
#' the empirical item characteristic curve or returns the plot object.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' ip <- generate_ip(model = c("3PL", "GRM"), n = 20)
#' true_theta <- rnorm(2000)
#' resp <- generate_resp_set(ip = ip, theta = true_theta)
#'
#' plot_empirical_icc(resp, "Item_3", ip = ip, theta = true_theta)
#' plot_empirical_icc(resp, 3, ip = ip, theta = true_theta)
#' # Change the number of bins
#' plot_empirical_icc(resp, 3, ip = ip, theta = true_theta, bins = 10)
#' # Fixed bin width
#' plot_empirical_icc(resp, 3, ip = ip, theta = true_theta, binwidth = .2)
#'
#' # Plot GRM item's ICC
#' plot_empirical_icc(resp, "Item_4", ip = ip, theta = true_theta)
#' plot_empirical_icc(resp, "Item_4", ip = ip, theta = true_theta, binwidth = .2)
#'
plot_empirical_icc <- function(resp, item, ip, theta = NULL, bins = 10,
                               binwidth = NULL, title = "",
                               suppress_plot = FALSE,
                               base_r_graph = FALSE,
                               ...) {

  gd <- get_data_plot_empirical_icc(resp = resp, item = item, ip = ip,
                                    theta = theta, bins = bins,
                                    binwidth = binwidth)
  x_label <- expression("Theta ("*theta*")")
  y_label <- ifelse(length(unique(gd$Response)) > 2,
                    "Proporition of Response",
                    "Proportion Correct")

  # Set the graph title:
  if (!is.null(title) && title == "")
    title <- paste0("Trace Graph for '", gd$item_id[1], "'")

  ### ggplot2 ###
  if (!base_r_graph && requireNamespace("ggplot2", quietly = TRUE)) {
    if (length(unique(gd$Response)) > 2) {
      p <- ggplot2::ggplot(gd, ggplot2::aes(
        x = .data$bin, y = .data$pvalue, color = .data$Response,
        linetype = .data$Type,
        group = interaction(.data$Type, .data$Response))) +
        ggplot2::geom_point(
          data = gd[gd$Type == "Observed", ],
          mapping = ggplot2::aes(
            size = switch(is.null(binwidth) + 1, .data$total_freq,  NULL)),
          alpha = .5) +
        ggplot2::geom_line(...)
    } else {
      p <- ggplot2::ggplot(gd, ggplot2::aes(x = .data$bin, y = .data$pvalue,
                                            color = .data$Type)) +
        ggplot2::geom_point(
          data = gd[gd$Type == "Observed", ],
          mapping = ggplot2::aes(
            x = .data$bin, y = .data$pvalue,
            size = switch(is.null(binwidth) + 1, .data$total_freq,  NULL),
            group = "Type"),
          alpha = .5) +
        ggplot2::geom_line(ggplot2::aes(group = .data$Type), ...)
    }

    p <- p +
      ggplot2::labs(x = x_label, y = y_label, title = title) +
      ggplot2::ylim(c(0, 1)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.title = ggplot2::element_blank())
    if (suppress_plot) return(p) else print(p)
  ### Base R graphics ###
  } else {
    old_par <- graphics::par(no.readonly = TRUE) # Save old settings

    if (length(unique(gd$Response)) > 2) {
      graphics::par(mar = c(5.1, 4.1, 4.1, 7.1))
      types <- c("Expected", "Observed")
      responses <- unique(gd$Response)
      cl <- grDevices::rainbow(length(responses))
      plot(x = 0,
           y = 0,
           type = "n",
           main = title,
           ylab = y_label,
           xlab = x_label,
           panel.first = graphics::grid(),
           xlim = c(min(gd$bin), max(gd$bin)),
           ylim = c(0, 1))
      for (i in seq_along(types)) {
        for (j in seq_along(responses)) {
          xx <- gd$bin[gd$Type == types[i] & gd$Response == responses[j]]
          yy <- gd$pvalue[gd$Type == types[i] & gd$Response == responses[j]]
          graphics::lines(x = xx, y = yy, col = cl[j], lty = i)
          if (types[i] == "Observed")
            graphics::points(x = xx, y = yy, col = cl[j])
        }
      }
      graphics::legend("topleft", legend = types,
                       lty = 1:2, xpd = TRUE, y.intersp = 2,
                       inset = c(1, 0), bty = "n")
      graphics::legend("left", legend = responses,
                       col = cl, lty = 1, xpd = TRUE, y.intersp = 2,
                       title = "Response",
                       inset = c(1, 0), bty = "n")
    } else {
      graphics::par(mar = c(5.1, 4.1, 4.1, 7.1))
      cl <- c("#F8766D", "#00BA38")

      plot(x = gd$bin[gd$Type == "Observed"],
           y = gd$pvalue[gd$Type == "Observed"],
           type = "p", col = cl[2],
           main = title,
           ylab = y_label,
           xlab = x_label,
           panel.first = graphics::grid(),
           ylim = c(0, 1))
      graphics::lines(x = gd$bin[gd$Type == "Expected"],
                      y = gd$pvalue[gd$Type == "Expected"], col = cl[1])
      graphics::lines(x = gd$bin[gd$Type == "Observed"],
                      y = gd$pvalue[gd$Type == "Observed"], col = cl[2])

      graphics::legend("topleft", c("Expected", "Observed"),
                       col = cl, lty = 1, xpd = TRUE, y.intersp = 2,
                       inset = c(1, 0), bty = "n")

    }
    # dev.off()
    p <- grDevices::recordPlot()
    graphics::par(old_par) # Restore to old settings
    return(invisible(p))
  }
}




############################################################################%###
############################# get_data_plot_empirical_icc2 #################%###
############################################################################%###

get_data_plot_empirical_icc2 <- function(resp, item, bins = 10,
                                         binwidth = NULL, ip = NULL,
                                         x_axis_scale = NULL, theta = NULL) {
  # resp should be a matrix or data.frame
  if (!inherits(resp, c("matrix", "data.frame")))
    stop("'resp' should be a matrix or a data.frame object.")
  # There should be at least two bins and bins should be a number.
  if (is.null(binwidth) &&
      (!is.numeric(bins) || as.integer(bins) != bins || bins < 2))
    stop("'bins' should be an integer larger than 2.")

  if (!is.null(binwidth) && !(is.numeric(binwidth) && length(binwidth) == 1 &&
                              binwidth > 0)) {
    stop("Invalid 'binwidth'. Please provide a positive numeric value.",
         call. = FALSE)
  }

  item <- get_item_id_col_num(resp = resp, item = item, ip = ip)

  ### x_axis_scale
  # Set the default value for x_axis_scale if it is NULL
  if (is.null(x_axis_scale)) {
    x_axis_scale <- ifelse(is.null(binwidth), "percent", "theta")
  }
  if (!is.null(binwidth)) x_axis_scale <- "theta"
  # if (x_axis_scale == "theta" && !is(ip, "Itempool"))
  #   stop("When x_axis_scale is 'theta', a valid item pool ('ip') should be ",
  #        "provided.")

  ### theta checks
  # "eicc" will be calculated using theta estimates instead of total raw scores
  # if 'ip' is available.
  if (!is.numeric(theta)) {
    if (is(ip, "Itempool")) {
      theta <- est_ability(ip = ip, resp = resp, method = "eap")$est
    } else {
      theta <- rowSums(resp, na.rm = TRUE)
    }
  }
  # Theta length should be equal to the number of rows of the resp.
  if (!is.null(theta) && length(theta) != nrow(resp))
    stop("The length of theta should be equal to the number of columns of ",
         "'resp'.")

  gd <- data.frame(resp, check.names = FALSE)
  # Add total score
  if (is.null(theta)) {
    gd$ts <- rowSums(gd, na.rm = TRUE)
  } else gd$ts <- theta
  # Sort the data using the total scores and extract only the relevant column
  # and the total score
  gd <- gd[order(gd$ts), c(item$col_num, ncol(gd))]
  colnames(gd) <- c("response", "ts")
  # Remove rows where response is NA
  gd <- gd[!is.na(gd$response), ]

  if (is.null(binwidth)) {
    repeat {
      q <- stats::quantile(
        gd$ts, probs = seq(0, 1, length.out = bins + 1))[-c(1, bins + 1)]
      if (any(duplicated(q)) || any(range(gd$ts, na.rm = TRUE) %in% q)) {
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
    } else if (x_axis_scale == "theta") {
      temp_labels <- unname(round((c(q, max(gd$ts)) +
                                     c(min(gd$ts), q)) / 2, 2))
    } else stop("Invalid 'x_axis_scale' value.")

    q <- c(min(gd$ts, na.rm = TRUE), q, max(gd$ts, na.rm = TRUE))
    gd$bin <- cut(gd$ts, breaks = q, include.lowest = TRUE,
                    labels = temp_labels)
    gd$response <- factor(gd$response, ordered = TRUE)


    temp_freq_total <- as.data.frame(table(bin = gd$bin),
                                     responseName = "total_freq")
    # temp_freq_items <- as.data.frame(table(bin = gd$bin,
    #                                        response = gd$response))
    gd <- as.data.frame(t(prop.table(table(Response = gd$response,
                                           bin = gd$bin), margin = 2)),
                        responseName = "Observed")

    gd <- merge(gd, temp_freq_total, by = "bin")

    # If there are just two categories, remove the first category.
    if (length(unique(gd$Response)) == 2)
      gd <- gd[gd$Response == sort(unique(gd$Response))[2], ]

    # Set x-axis label
    x_label <- ifelse(is.null(theta), "Ability Group (Raw Score)",
                      "Ability Group (Theta Score)")
  } else {
    bins <- seq(from = floor(10 * min(gd$ts))/10,
                to = ceiling(10 * max(gd$ts)) / 10, by =  binwidth)

    temp_labels <- (bins[-1] + bins[-length(bins)]) / 2
    gd$bin <- cut(gd$ts, breaks = bins, include.lowest = TRUE,
                    labels = temp_labels)

    gd$response <- factor(gd$response, ordered = TRUE)

    temp_freq_total <- as.data.frame(table(bin = gd$bin),
                                     responseName = "total_freq")

    gd <- as.data.frame(t(prop.table(table(Response = gd$response,
                                           bin = gd$bin), margin = 2)),
                        responseName = "Prop")

    # If there are just two categories, remove the first category.
    if (length(unique(gd$Response)) == 2)
      gd <- gd[gd$Response == unique(gd$Response)[2], ]

    gd <- merge(gd, temp_freq_total, by = "bin")
    colnames(gd) <- c("bin", "Response", "Observed", "Freq")

    # If there are any NaN values, remove them
    gd <- gd[!is.nan(gd$Observed), ]

    if (is(ip, "Itempool")) {
      temp_ip <- flatten_itempool_cpp(ip)
      temp_itm <- temp_ip[[item$item_id]]
      gd$Expected <- resp_lik(
        ip = temp_itm,
        resp = as.integer(as.character(gd$Response)),
        theta = as.numeric(as.character(gd$bin)))
      gd <- stats::reshape(
        data = gd, varying = c("Observed", "Expected"), v.names = "pvalue",
        direction = "long", timevar = "Type", times = c("Observed", "Expected"))
    }
    gd$bin <- as.numeric(as.character(gd$bin))
  }
  gd <- gd[order(gd$bin), ]
  colnames(gd)[1] <- x_axis_scale
  return(gd)
}



############################################################################%###
############################# plot_empirical_icc2 ##########################%###
############################################################################%###
#' Plot Empirical Item Characteristic Curve
#' @description
#' \code{plot_emprical_icc} plots empirical item characteristic curve.
#'  Examinees will be put into bins based on their total raw scores and the
#'  proportion of examinees who correctly answered an item for each bin will be
#'  plotted.
#'
#' @param resp Response matrix.
#' @param item The column number, column name or the 'ID' of the  the item that
#'   should be plotted.
#' @param bins An integer larger than 2 representing of ability groups examinees
#'   should be grouped into. The default is \code{10}. The maximum value of
#'   \code{bins +  1} is the number of possible total scores.
#' @param binwidth If 'theta' scale is used, the \code{binwidth} determines
#'   the width of each bin of the theta scale. Within each bin, there might be
#'   different number of examinees.
#' @param ip An \code{\link{Itempool-class}} object needs to be provided if
#'   expected ICC desired.
#' @param theta A vector of examinee abilities.
#' @param title Title of the plot. The default value is \code{""}.
#' @param n_dodge The number of lines the x-axis tick labels should be written
#'   to. This is especially useful if the x-axis tick labels overlap with each
#'   other. The default value is \code{1}, which means all of the labels are
#'   written on the same line.
#' @param suppress_plot If \code{FALSE} the function will print the plot. If
#'          \code{TRUE}, function will return the plot object. Default value is
#'          \code{FALSE}.
#' @param x_axis_scale Set the scale of the x-axis. The default value is
#'   \code{NULL}. For total score it will be defaulted to \code{"percent"}.
#'   \describe{
#'     \item{\strong{\code{"percent"}}}{Percent interval.}
#'     \item{\strong{\code{"number"}}}{Numbers between 1 and \code{bins}}
#'     \item{\strong{\code{"theta"}}}{Theta values equally divided into bins.
#'     the middle value of the bin is shown in the x-axis. For example, if
#'     \code{bins = 10}, the first tick of the x-axis will be the mean of
#'     minimum theta value and tenth percentile theta value.}
#'     }
#'
#' @param ... Extra parameters that will pass to \code{geom_line}.
#'
#' @return Depending on the value of \code{suppress_plot} function either prints
#' the empirical item or test characteristic curve or returns the plot object.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' ip <- generate_ip(model = c("3PL", "GRM"), n = 20)
#' true_theta <- rnorm(2000)
#' resp <- sim_resp(ip = ip, theta = true_theta)
#'
#' # Provide item ID
#' plot_empirical_icc2(resp = resp, item = "Item_5")
#' # Provide item number
#' plot_empirical_icc2(resp, item = 3)
#' # Change x-axis scale
#' plot_empirical_icc2(resp, item = 3, x_axis_scale = "number")
#' # Change number of bins and x-axis scale
#' plot_empirical_icc2(resp, item = 3, bins = 11, x_axis_scale = "theta")
#' # Use bin width
#' plot_empirical_icc2(resp, item = 3, binwidth = 2)
#' # Use theta scores instead of raw scores
#' plot_empirical_icc2(resp, item = 3, binwidth = .2, ip = ip,
#'                     theta = true_theta)
#'
#' # A GRM item
#' plot_empirical_icc2(resp, item = 4)
#' plot_empirical_icc2(resp, item = 4, x_axis_scale = "percent")
#' plot_empirical_icc2(resp, item = 4, x_axis_scale = "number")
#' plot_empirical_icc2(resp, item = 4, binwidth = 4)
#' # Use raw score and custom binwidth
#' plot_empirical_icc2(resp, item = 4, x_axis_scale = "percent", binwidth = 4)
#' # Use theta score
#' plot_empirical_icc2(resp, item = 4, binwidth = .2, ip = ip,
#'                     theta = true_theta)
#' # Add arguments for 'geom_line'
#' plot_empirical_icc2(resp, item = 4, binwidth = .2, ip = ip,
#'                     theta = true_theta, size = 1, alpha = .25)
#'
plot_empirical_icc2 <- function(resp, item, bins = 10, binwidth = NULL,
                                ip = NULL, theta = NULL, title = "",
                                suppress_plot = FALSE,
                                x_axis_scale = NULL, n_dodge = 1, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("This function requires 'ggplot2' package. Please install it using ",
         "'install.packages(\"ggplot2\")'")
  }

  if (inherits(resp, "Response_set")) {
    resp <- as.matrix(resp)
  }

  gd <- get_data_plot_empirical_icc2(resp = resp, item = item, bins = bins,
                                     binwidth = binwidth, ip = ip,
                                     x_axis_scale = x_axis_scale, theta = theta)

  x_axis_scale <- colnames(gd)[1]

  # Set x-axis label
  x_label <- ifelse(is.null(theta), "Ability Group (Raw Score)",
                    "Ability Group (Theta Score)")

  if (is.null(binwidth)) {
    if (length(unique(gd$Response)) > 1) {
      p <- ggplot2::ggplot(gd, ggplot2::aes(
        x = .data[[x_axis_scale]], y = .data$Observed, color = .data$Response)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_line(ggplot2::aes(group = .data$Response), ...)
    } else {
      p <- ggplot2::ggplot(gd, ggplot2::aes(
        x = .data[[x_axis_scale]], y = .data$Observed, group = 1)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_line(...)
    }
  } else {
    if (is(ip, "Itempool")) {
      if (length(unique(gd$Response)) > 2) {
        p <- ggplot2::ggplot(gd, ggplot2::aes(
          x = .data[[x_axis_scale]], y = .data$pvalue, color = .data$Response,
          linetype = .data$Type)) +
          ggplot2::geom_point(data = gd[gd$Type == "Observed", ],
                              ggplot2::aes(size = .data$Freq), alpha = .5) +
          ggplot2::geom_line(...)
      } else {
        p <- ggplot2::ggplot(gd, ggplot2::aes(
          x = .data[[x_axis_scale]], y = .data$pvalue, color = .data$Type)) +
          ggplot2::geom_point(data = gd[gd$Type == "Observed", ],
                              mapping = ggplot2::aes(size = .data$Freq),
                              shape = 20, alpha = .5) +
          ggplot2::geom_line(ggplot2::aes(group = .data$Type), ...)
      }
    } else {# No itempool, i.e. no Expected ICC
      if (length(unique(gd$Response)) > 2) {
        p <- ggplot2::ggplot(gd, ggplot2::aes(
          x = .data[[x_axis_scale]], y = .data$Observed, color = .data$Response)) +
          ggplot2::geom_point(ggplot2::aes(size = .data$Freq)) +
          ggplot2::geom_line(...)
      } else {
        p <- ggplot2::ggplot(gd, ggplot2::aes(
          x = .data[[x_axis_scale]], y = .data$Observed)) +
          ggplot2::geom_point(ggplot2::aes(size = .data$Freq)) +
          ggplot2::geom_line(...)
      }
    }
    # Set x-axis label
    x_label <- expression("Theta ("*theta*")")
  }

  # Add common graph elements
  p <- p +
    ggplot2::labs(x = x_label, y = "Proportion Correct", title = title)
    ggplot2::guides(x = ggplot2::guide_axis(n.dodge = n_dodge,
                                            check.overlap = TRUE)) +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  if (suppress_plot) return(p) else print(p)
}


