
get_data_plot_cat_output <- function(cat_output, plot_b) {
  # Convert ip to Itempool object
  if (!is(cat_output, "cat_output"))
    stop("'co' should be a 'cat_output' object. Please provide the output of ",
         "the 'cat_sim' function.")
  # if (!is.null(cd) && !inherits(cd, "cat_design"))
  #   stop("'cd' should be a 'cat_design' object. Please run ",
  #        "'create_cat_design' function.")

  administered_ip <- get_cat_administered_items(cat_output)
  if (!all(administered_ip$model %in% UNIDIM_DICHO_MODELS)) {
    stop(paste0(
      "This function is only available when all of the items in the item ",
      "pool are unidimensional dichotomous models: ",
      paste0("'", UNIDIM_DICHO_MODELS, "'", collapse = ", "), "."))
  }

  response_labels <- c("Incorrect","Correct")
  co_summary <- print(cat_output, silent = TRUE)
  output <- cbind(item_no = 0:(nrow(co_summary)-1),
                  co_summary[, c("est_before", "se_before", "resp")])
  output$resp <- factor(output$resp, levels = c(0,1), labels = response_labels)
  colnames(output) <- c("item_no", "est", "se", "resp")

  output <- rbind(output, data.frame(item_no = nrow(co_summary),
                                     est = utils::tail(co_summary$est_after, 1),
                                     se = utils::tail(co_summary$se_after, 1),
                                     resp = NA))
  if (plot_b) {
    if (all(administered_ip$model %in% UNIDIM_DICHO_MODELS)) {
      output$b <- c(administered_ip$b, NA)
    } else {
      message(paste0(
        "'plot_b' is only available when all of the items in the item ",
        "pool is unidimensional dichotomous models: ",
        paste0("'", UNIDIM_DICHO_MODELS, "'", collapse = ", "), "."))
    }
  }
  return(output)
}


############################################################################@###
############################# plot.cat_output ##############################@###
############################################################################@###

#' Plot progress of a CAT algorithm for one examinee
#' @description
#' \code{plot.cat_output} Plots the progress of CAT for one examinee.
#'
#' @param x A "cat_output" object that is output of \code{\link{cat_sim}}
#' function for one examinee.
#' @param ... Additional arguments.
#' @param plot_b If \code{TRUE}, 'b' parameters of the administered items will
#'   be plotted along with intermediate theta estimates. The default value is
#'   \code{TRUE}.
#' @param se_band A logical value. If \code{TRUE}, a standard error band
#'   is added around the estimated theta values. At each stage one standard
#'   error of that stage is added to and subtracted from the ability estimate
#'   at that stage. The default value is \code{TRUE}.
#' @param horizontal_line An option to add a horizontal line. Provide either one
#'          of these or a list of a combination of these (except \code{NULL}).
#'          \itemize{
#'            \item{"true_theta"}{Add a horizontal line for true theta.
#'              Default option.}
#'            \item{"final_theta"}{Add a horizontal line at final theta
#'              (ability) estimate}
#'            \item{NULL}{No horizontal line added.}
#'          }
#' @param title Title of the Plot
#' @param suppress_plot If \code{FALSE} the function will print the plot. If
#'          \code{TRUE}, function will return the plot object. Default value is
#'          \code{FALSE}.
#' @param base_r_graph Currently this function only works with package
#'   'ggplot2'.
#'
#'   If \code{TRUE} function will plot graphs using base R
#'   graphics. If \code{FALSE} the function will check whether 'ggplot2' package
#'   is installed. If it is installed, it will use 'ggplot2' package for the
#'   plot. The default value is \code{FALSE}.
#'
#' @return Depending on the value of \code{printPlot} function either prints
#' the CAT progress plot or returns the plot object.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' cd <- create_cat_design(ip = generate_ip(n = 100))
#' co <- cat_sim(true_ability = rnorm(1), cd = cd)
#'
#' plot(co)
#'
#' # Suppress item difficulties
#' plot(co, plot_b = FALSE)
#'
#' # Suppress Standard Error Band
#' plot(co, se_band = FALSE)
#'
#' # Add final theta estimate line
#' plot(co, horizontal_line = "final_theta")
#' plot(co, horizontal_line = "true_theta")
#'
#' # Change Title
#' plot(co, title = "CAT Progress for Examinee ABC")
#'
#' \dontrun{
#' # Change Text Size
#' plot(co) + theme(text=element_text(size=20))
#'
#' # Change x-axis label
#' plot(co) + xlab("My New X Axis Label")
#'
#' # Change y limits of the graph
#' plot(co) + coord_cartesian(ylim = c(-5,5))
#'
#' # Change legend position
#' plot(co) + theme(legend.position="none")
#' plot(co) + theme(legend.position="left")
#'
#' # Add a horizontal line
#' plot(co) + geom_hline(yintercept = -1, color = "red", linetype = 5)
#' }
plot.cat_output <- function(x,
                            ...,
                            plot_b = TRUE,
                            se_band = TRUE,
                            # textBox = c("finalThetaEst", "finalSE"),
                            horizontal_line = "true_theta",
                            # textBoxLocation = "bottomright",
                            # yAxisLimit = "optimized",
                            title = "CAT Progress",
                            suppress_plot = FALSE,
                            base_r_graph = FALSE)
{
  args <- list(...)
  co_summary <- get_data_plot_cat_output(cat_output = x, plot_b = plot_b)
  x_label <- "Item Number"
  y_label <- "Theta Estimate"
  color_label <- "Response"
  # y_lim <- c(min(co_summary$est), max(co_summary$est))

  ### ggplot2 ###
  if (!base_r_graph && requireNamespace("ggplot2", quietly = TRUE)) {
    segment_df <- co_summary[1:(nrow(co_summary) - 1),
                             c("item_no", "est", "resp")]
    colnames(segment_df) <- c("x", "y", "resp")
    segment_df$xend <- 1:nrow(segment_df)
    segment_df$yend <- co_summary$est[-1]

    p <- ggplot2::ggplot(
      co_summary,
      ggplot2::aes(x = .data$item_no, y = .data$est,  group = NA)) +
      ggplot2::geom_segment(
        data = segment_df,
        ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend,
                     yend = .data$yend, color = .data$resp),
        linetype = "solid") +
      ggplot2::geom_point(ggplot2::aes(color = .data$resp), size = 2.5)
    # Standard Error Band
    if (se_band) {
      se_df <- co_summary[-1, c("item_no", "est", "se")]
      se_df$ymin <- se_df$est - se_df$se
      se_df$ymax <- se_df$est + se_df$se
      p <- p +
        ggplot2::geom_ribbon(data = se_df,
                             ggplot2::aes(ymin = .data$ymin, ymax = .data$ymax),
                             alpha = .2)
    }

    # Item Difficulty
    if (plot_b) {
      b_df <- co_summary[-nrow(co_summary), c("item_no", "b")]
      b_df$label <- "b"
      p <- p +
        ggplot2::geom_text(
          data = b_df,
          ggplot2::aes(x = .data$item_no, y = .data$b, label = .data$label),
          color = "blue", size = 3)
    }

    # Add Final Theta Point
    p <- p +
      ggplot2::geom_point(
        data = data.frame(x = utils::tail(co_summary$item_no, 1),
                          y = utils::tail(co_summary$est, 1)),
        ggplot2::aes(x = .data$x, y = .data$y), color = "black", shape = 15,
        size = 3)

    # Horizontal Lines
    if (!is.null(horizontal_line)) {
      for (hl in horizontal_line) {
        if (hl == "true_theta" && !is.na(unlist(x$true_ability))) {
          temp <- data.frame(yint = x$true_ability[[1]])
          temp2 <- "True Ability"
          p <- p +
            ggplot2::geom_hline(
              data = temp,
              ggplot2::aes(yintercept = .data$yint, linetype = temp2),
              linewidth = .25, color = "black")

        } else if (hl == "final_theta") {
          temp <- data.frame(yint = utils::tail(co_summary$est, 1))
          temp2 <- "Final Ability Est."
          p <- p +
            ggplot2::geom_hline(
              data = temp,
              ggplot2::aes(yintercept= .data$yint, linetype = temp2),
              linewidth = .25, color = "cyan")
        }
      }

    }

    p <- p +
      ggplot2::scale_color_manual(color_label,
                         values = c(Correct = "green3", Incorrect = "tomato")) +
      ggplot2::scale_linetype_manual(NULL,
                            values = rep("dashed", length(horizontal_line))) +
      # Make sure the numbers on x axis are not decimals:
      ggplot2::scale_x_continuous(breaks = function(x) unique(floor(pretty(
        seq(0, max(x)), n = 8)))) +
      ggplot2::labs(x = x_label,
           y = y_label,
           title = title,
           linetype = "",
           color = color_label)
    p <- p + ggplot2::theme_bw()
    if (suppress_plot) return(p) else print(p)
  } else {
    warning("This graph requires 'ggplot2'. Base R version has not been ",
            "implemented yet")
  }
}

