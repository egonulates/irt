
#' Item Parameter Drift via D2
#'
#' @description
#' This function assesses item parameter drift using the method outlined in
#' Wells et al. (2014). It involves comparing the Item Characteristic Curves
#' (ICCs) of item parameters from two different item pools. This is also
#' referred to as WRMSD (Weighted Root Mean Squared Difference).
#'
#' Determining the significance of D2 in identifying item drift lacks strict
#' thresholds. It is advisable to adopt a comprehensive approach that takes into
#' account other measures. Nevertheless, it is worthwhile to consider the square
#' root of D2 (\code{sqrt(D2)}) value. As a general rule of thumb, for
#' dichotomous items, a square root of D2 value greater than 0.1 may signal a
#' need for closer examination.
#'
#' For polytomous items with two thresholds (or three score categories), a
#' square root of D2 value surpassing 0.15 warrants attention. For those with
#' three thresholds (or four score categories), if the square root of D2 is
#' greater than 0.225, for items with four thresholds (or five score
#' categories), a square root of D2 value larger than 0.3 may be indicative of
#' item drift. Finally, for items with five thresholds (or six score
#' categories), a square root of D2 value greater than 0.375 should prompt
#' further investigation into potential item drift.
#'
#' @param ip1 Itempool object representing the first calibration.
#' @param ip2 Itempool object representing the second calibration.
#' @param theta A numeric vector containing the quadrature points.
#' @param weights A numeric vector containing the weights assigned to the
#'   quadrature points. The length of this vector should match the length of the
#'   \code{theta} argument.
#' @param anchor_item_ids A character vector containing the IDs of anchor items.
#'   If set to \code{NULL}, it is assumed that all items are considered anchor
#'   items.
#'
#' @noRd
#'
#' @author Emre Gonulates
#'
#' @references
#' Wells, C. S., Hambleton, R. K., Kirkpatrick, R., & Meng, Y. (2014).
#' An examination of two procedures for identifying consequential item
#' parameter drift. Applied Measurement in Education, 27, 214–231.
#'
#' Li, Y. (2012), Examining the Impact of Drifted Polytomous Anchor Items on
#' Test Characteristic Curve (TCC) Linking and IRT True Score Equating. ETS
#' Research Report Series, 2012: i-22.
#' https://doi.org/10.1002/j.2333-8504.2012.tb02291.x
#'
#' @examples
#' ip1 <- generate_ip(n = 20)
#' ip2 <- ip1
#' # add a small nuisance to item difficulty parameters
#' ip2$b <- ip1$b + runif(20, -.5, .5)
#'
#' theta <- seq(-4, 4, 0.2)
#' weights <- dnorm(theta)
#' ipd_d2(ip1, ip2, theta = theta, weights = weights)
#'
#' ipd_d2(ip1, ip2, theta = theta, weights = weights,
#'   anchor_item_ids = c("Item_2", "Item_6", "Item_9", "Item_13"))
#'
#' ### Polytomous items items
#' n_item <- 30
#' models <- sample(c("3PL", "GPCM2"), n_item, TRUE)
#' new_ip <- generate_ip(model = models, D = 1.702)
#' old_ip <- data.frame(new_ip)
#' old_ip$a <- old_ip$a + round(runif(n_item, min = -.5, max = .5), 2)
#' old_ip$b <- old_ip$b + round(runif(n_item, min = -.75, max = .75), 2)
#' old_ip$d1 <- old_ip$d1 + round(runif(n_item, min = -.75, max = .75), 2)
#' old_ip$d2 <- old_ip$d2 + round(runif(n_item, min = -.75, max = .75), 2)
#' old_ip$d3 <- old_ip$d3 + round(runif(n_item, min = -.75, max = .75), 2)
#' old_ip <- itempool(old_ip)
#'
#' ipd_d2(ip1 = old_ip, ip2 = new_ip, theta = theta, weights = weights)

ipd_d2 <- function(ip1, ip2, theta = seq(-4, 4, 0.1),
                   weights = stats::dnorm(seq(-4, 4, 0.1)),
                   anchor_item_ids = NULL) {

  if (!inherits(ip1, "Itempool") || !inherits(ip2, "Itempool"))  {
    stop("Invalid input for 'ip1' and/or 'ip2'. Both 'ip1' and 'ip2' should ",
         "be of type 'Itempool'.")
  }

  if (!is_atomic_vector(theta, "numeric")) {
    stop("Invalid 'theta'. 'theta' should be a numeric vector.")
  }
  if (!is_atomic_vector(weights, "numeric") ||
      length(weights) != length(theta)) {
    stop("Invalid 'weights'. 'weights' should be a numeric vector that has ",
         "the same length as 'theta' argument.")
  }

  # Check if anchor item ids is not null. If it is not, pull those items
  if (!is.null(anchor_item_ids)) {
    if (!is_atomic_vector(anchor_item_ids, "character"))  {
      stop("Invalid 'anchor_item_ids'. 'anchor_item_ids' should be a ",
           "character vector.")
    }
    # Check if anchor_item_ids appeared in both item pools. If not raise an
    # error.
    if (!all(anchor_item_ids %in% ip2$resp_id) ||
        !all(anchor_item_ids %in% ip1$resp_id))
      stop("All 'anchor_item_ids' should be in both ip2 and ip1.")
    ip2 <- ip2[anchor_item_ids %in% ip2$resp_id]
    ip1 <- ip1[anchor_item_ids %in% ip1$resp_id]
  } else if (length(ip1) != length(ip2)) {
    stop("The lengths of 'ip1' and 'ip2' are different. When ",
         "'anchor_item_ids' are not specified, all items are assumed to ",
         "be anchor items and the lenghts of ip1 and ip2 should be the same.")
  } else if (all(ip1$resp_id %in% ip2$resp_id)) {
    anchor_item_ids <- ip1$resp_id
  } else {
    warning("Item IDs of ip1 and ip2 differs. Item order within the item ",
            "pool will be used for checking item parameter drift.")
  }

  weights <- weights/(sum(weights))

  d2 <- function(item1, item2, theta, weights) {
    p1 <- mean(item1, theta = theta)
    p2 <- mean(item2, theta = theta)
    return(sum(weights * (p1 - p2)^2))
  }
  ip1 <- flatten_itempool_cpp(ip1)
  ip2 <- flatten_itempool_cpp(ip2)
  result <- c()
  if (is.null(anchor_item_ids)) {
    for (i in seq_len(length(ip1))) {
      result <- c(result, d2(item1 = ip1[[i]], item2 = ip2[[i]], theta = theta,
                             weights = weights))
    }
    result <- setNames(result, names(ip1))
  } else {
    for (item_id in anchor_item_ids) {
      result <- c(result, d2(item1 = ip1[[item_id]], item2 = ip2[[item_id]],
                             theta = theta, weights = weights))

    }
    result <- setNames(result, anchor_item_ids)
  }
  return(result)
}


#' Item Parameter Drift via Robust z
#'
#' @description
#' This function evaluates item parameter drift using the approach described in
#' Huynh and Meyer (2010).
#'
#' @param ip1 An Itempool object for the first calibration.
#' @param ip2 An Itempool object for the second calibration.
#' @param anchor_item_ids A character vector containing the IDs of anchor
#'   items. If set to \code{NULL}, it is assumed that all items are
#'   considered anchor items.
#' @param alpha A numeric value ranging from 0 to 1. The two-tailed critical
#'   value is employed to identify unstable items. For instance, if we calculate
#'   the critical value using \code{qnorm(1-alpha/2)} (which equals 1.96 when
#'   \eqn{alpha = 0.05}), items with absolute robust-z values exceeding this
#'   threshold will be marked as unstable.
#' @param iqr_type An integer indicating the choice of quantile algorithm. Refer
#'   to the \code{?quantile} function's \code{type} argument for more details.
#'   For instance, SAS's default quantile algorithm, \code{QNTLDEF=5},
#'   corresponds to \code{iqr_type = 2} in R. The default value is
#'   \code{iqr_type = 7}.
#' @param exclude_unstable A logical value. The default is \code{TRUE}, meaning
#'   that unstable items flagged by Robust-z for 'a' are excluded from Robust-z
#'   'b' or threshold parameter calculations. If set to \code{FALSE}, unstable
#'   items will be included in the Robust-z calculation for 'b' and threshold
#'   parameters. Note that changing \code{exclude_unstable} parameter does not
#'   affect the calculation of the 'A' constant for the Robust-z for 'b'
#'   parameter calculation. The unstable items flagged by Robust-z for 'a' are
#'   always excluded from this ('A') calculation.
#'
#'
#' @noRd
#'
#' @author Emre Gonulates
#'
#' Huynh, Huynh and Meyer, Patrick (2010) "Use of Robust z in Detecting
#' Unstable Items in Item Response Theory Models,"
#' \emph{Practical Assessment, Research, and Evaluation}: Vol. 15 , Article 2.
#' <doi:10.7275/ycx6-e864>
#'
#' @examples
#' ##### Robust z #####
#' # The example from Huynh and Meyer (2010)
#' ip1 <- c(itempool(
#'   a = c(0.729, 0.846, 0.909, 0.818, 0.742, 0.890, 1.741, 0.907, 1.487, 1.228,
#'         0.672, 1.007, 1.016, 0.776, 0.921, 0.550, 0.624, 0.984, 0.506, 0.594,
#'         0.687, 0.541, 0.691, 0.843, 0.530, 0.462, 1.007, 0.825, 0.608, 1.177,
#'         0.900, 0.861, 0.843, 1.404, 0.446, 1.014, 1.632, 0.831, 1.560, 0.798),
#'   b = c(1.585, 0.635, -0.378, -0.100, -0.195, 0.749, 1.246, 1.016, -0.234,
#'         0.537, 0.070, 1.985, 1.101, -0.742, 0.463, -0.060, 0.477, 1.084,
#'         -2.340, 1.068, -0.055, -1.045, 1.859, 0.645, -0.689, -2.583, 1.922,
#'         0.709, 0.499, 1.973, 0.104, 0.809, 0.640, 0.247, 0.820, 1.837,
#'         2.129, 1.012, 1.774, 0.095),
#'   c = c(0.134, 0.304, 0.267, 0.176, 0.215, 0.194, 0.267, 0.159, 0.095,
#'         0.197, 0.089, 0.272, 0.229, 0.159, 0.162, 0.100, 0.259, 0.167,
#'         0.000, 0.242, 0.323, 0.000, 0.196, 0.189, 0.000, 0.000, 0.334,
#'         0.538, 0.125, 0.511, 0.192, 0.353, 0.103, 0.241, 0.245, 0.118,
#'         0.155, 0.132, 0.215, 0.148),
#'   model = "3PL"),
#'   item(a = 0.561, b = c(0.784, -0.113, 1.166), model = "GPCM"),
#'   item(a = 0.745, b = c(3.687, 2.506, -0.001), model = "GPCM"))
#'
#' ip2 <- c(itempool(
#'   a = c(0.650, 0.782, 0.816, 0.787, 0.611, 0.888, 1.192, 0.589, 1.211,
#'         0.742, 0.526, 0.690, 0.996, 0.816, 0.781, 0.507, 0.378, 0.976,
#'         0.473, 0.364, 0.585, 0.566, 0.511, 0.718, 0.354, 1.080, 0.840,
#'         0.865, 0.528, 0.814, 0.555, 0.701, 0.530, 1.220, 0.344, 0.966,
#'         1.044, 0.358, 1.192, 0.615),
#'   b = c(0.676, -0.525, -1.749, -1.092, -1.619, -0.406, -0.132, 0.006,
#'         -1.352, -0.872, -1.242, 0.873, 0.239, -2.038, -0.487, -1.372,
#'         -1.492, 0.214, -4.537, 0.220, -0.686, -2.394, 0.747, -0.467,
#'         -3.629, -5.000, 0.927, 0.305, -0.839, 1.270, -1.618, -0.091,
#'         -1.228, -1.019, -1.453, 1.090, 1.743, -1.436, 1.024, -1.358),
#'   c = c(0.110, 0.316, 0.161, 0.149, 0.145, 0.200, 0.243, 0.059, 0.081,
#'         0.075, 0.028, 0.267, 0.242, 0.189, 0.184, 0.121, 0.000, 0.170,
#'         0.000, 0.151, 0.383, 0.000, 0.195, 0.177, 0.000, 0.000, 0.352,
#'         0.647, 0.116, 0.501, 0.000, 0.286, 0.000, 0.248, 0.064, 0.150,
#'         0.126, 0.000, 0.187, 0.007),
#'   model = "3PL"),
#'   item(a = 0.486, b = c(-0.539, -1.489, -0.052), model = "GPCM"),
#'   item(a = 0.737, b = c(2.599, 1.250, -1.209), model = "GPCM"))
#' ipd_robustz(ip1, ip2)
#'
ipd_robustz <- function(ip1, ip2, anchor_item_ids = NULL, alpha = 0.01,
                        iqr_type = 7, exclude_unstable = TRUE) {

  if (!inherits(ip1, "Itempool") || !inherits(ip2, "Itempool"))  {
    stop("Invalid input for 'ip1' and/or 'ip2'. Both 'ip1' and 'ip2' should ",
         "be of type 'Itempool'.")
  }

  # Check if anchor item ids is not null. If it is not, pull those items
  if (!is.null(anchor_item_ids)) {
    # Check if anchor_item_ids appeared in both item pools. If not raise an
    # error.
    if (!all(anchor_item_ids %in% ip2$resp_id) ||
        !all(anchor_item_ids %in% ip1$resp_id))
      stop("All 'anchor_item_ids' should be in both ip2 and ip1.")

    ip2 <- ip2[anchor_item_ids]
    ip1 <- ip1[anchor_item_ids]
  }

  ip1_df <- as.data.frame(ip1)
  ip2_df <- as.data.frame(ip2)


  # All items are assumed to be anchor items, then
  # Make sure the item pool sizes and item pool item_ids are matching.
  if (is.null(anchor_item_ids)) {
    if (nrow(ip1_df) != nrow(ip2_df)) {
      stop("Invalid 'anchor_item_ids'. If 'anchor_item_ids' is set to NULL, ",
           "it means that all items are considered anchor items. In this ",
           "case, both 'ip1' and 'ip2' should have the same number of items. ",
           "Please ensure that the sizes of 'ip1' and 'ip2' match."
           )
    }

    if (!all(ip1_df$item_id == ip2_df$item_id)) {
      warning("The item IDs in the item pools do not align. Please ensure ",
              "that the item IDs and their order match in both item pools. ",
              "Otherwise, the results may not be accurate.")
    }
  }

  # If item models do not match raise a warning
  if (all(ip1_df$model != ip2_df$model)) {
    warning("The psychometric models of 'ip1' and 'ip2' do not match. This ",
            "could potentially lead to inaccurate results.")
  }

  output <- list()
  a1 <- ip1_df$a
  a2 <- ip2_df$a
  if ((is.null(a2) || any(is.na(a2))) ||
      (is.null(a1) || any(is.na(a1))))
    stop("In the item pools, there are some items that do not have 'a' ",
         "parameters.")
  a_diff <- log(a2) - log(a1)
  temp_z <- (a_diff - stats::median(a_diff, na.rm = TRUE)) /
    (0.74 * stats::IQR(a_diff, na.rm = TRUE, type = iqr_type))
  output$a$robust_z <- stats::setNames(temp_z, ip1_df$item_id)
  output$a$cor <- stats::cor(a2, a1, use = "pairwise.complete.obs")
  output$a$sd_ratio <- stats::sd(a2, na.rm = TRUE)/stats::sd(a1, na.rm = TRUE)

  cv <- stats::qnorm(1 - alpha/2) # two tailed critical value
  output$a$unstable <- ip1_df$item_id[!is.na(temp_z) & abs(temp_z) >= cv]

  # Select only stable items to calculate the linking constant
  A <- exp(mean(a_diff[!ip1_df$item_id %in% output$a$unstable], na.rm = TRUE))

  # ip1_list <- flatten_itempool_cpp(ip1)
  # ip2_list <- flatten_itempool_cpp(ip2)

  excluded_items <- NULL
  if (exclude_unstable) excluded_items <- output$a$unstable

  # par <- "d3"
  # par <- "b"
  for (par in c("b",
                "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10",
                "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10"
                )) {
    if (!par %in% colnames(ip1_df)) next
    par1 <- ip1_df[[par]]
    par2 <- ip2_df[[par]]
    par_diff <- stats::setNames(par1 - A * par2, ip1_df$item_id)
    temp_median <- stats::median(
      par_diff[setdiff(names(par_diff), excluded_items)], na.rm = TRUE)
    temp_iqr <- stats::IQR(par_diff[setdiff(names(par_diff), excluded_items)],
                           na.rm = TRUE, type = iqr_type)
    temp_z <- (par_diff - temp_median) / (0.74 * temp_iqr)
    output[[par]]$robust_z <- temp_z
    output[[par]]$unstable <- names(temp_z)[!is.na(temp_z) & abs(temp_z) >= cv]
  }

  # b1 <- b2 <- c()
  # for (i in seq_along(ip1_list)) {
  #   temp_b <- ip1_list[[i]]$b
  #   b1 <- c(b1, stats::setNames(
  #     temp_b, paste0(ip1_list[[i]]$item_id, if (length(temp_b) > 1)
  #       paste0(".", 1:length(temp_b)) else NULL)))
  #
  #   temp_b <- ip2_list[[i]]$b
  #   b2 <- c(b2, stats::setNames(
  #     temp_b, paste0(ip1_list[[i]]$item_id, if (length(temp_b) > 1)
  #       paste0(".", 1:length(temp_b)) else NULL)))
  #
  # }
  # b_diff <- b1 - A * b2
  # output$b$robust_z <- (b_diff - stats::median(b_diff)) /
  #   (0.74 * stats::IQR(b_diff, type = iqr_type))
  # output$b$unstable <- names(output$b$robust_z)[output$b$robust_z >= cv]

  return(output)
}



#' Item Parameter Drift
#'
#' @description
#' This function identifies items that have become unstable, meaning their
#' item parameter values have shifted, within two specified sets of items.
#'
#' @param ip1 An Itempool object for the first calibration.
#' @param ip2 An Itempool object for the second calibration.
#' @param method The method for analyzing item parameter drift.
#'   \describe{
#'     \item{"robust-z"}{Robust-Z method based on the Huynh and Meyer (2010).}
#'     \item{"d2"}{D2 method assesses item parameter drift using the method
#'       outlined in Wells et al. (2014). It involves comparing the Item
#'       Characteristic Curves (ICCs) of item parameters from two different
#'       item pools. This is also referred to as WRMSD (Weighted Root Mean
#'       Squared Difference).
#'
#'       There are no strict thresholds for determining the significance of D2
#'       in identifying item drift. A comprehensive approach considering other
#'       measures is recommended. Nevertheless, as a general guideline, for
#'       dichotomous items, a D2 value greater than 0.1 may warrant further
#'       scrutiny. For polytomous items with two thresholds (or three score
#'       categories), a D2 value exceeding 0.15, for those with three thresholds
#'       (or four score categories), a D2 value greater than 0.225, for those
#'       with four thresholds (or five score categories), a D2 value larger than
#'       0.3, and for items with five thresholds (or six score categories), a D2
#'       value larger than 0.375 may be indicative of item drift and should be
#'       investigated further.
#'     }
#'   }
#' @param anchor_item_ids A character vector containing the IDs of anchor
#'   items. If set to \code{NULL}, it is assumed that all items are
#'   considered anchor items.
#' @param alpha A numeric value ranging from 0 to 1. Only needed when
#'   \code{method = "robust-z"}. The two-tailed critical value is employed to
#'   identify unstable items. For instance, if we calculate the critical value
#'   using \code{qnorm(1-alpha/2)} (which equals 1.96 when \eqn{alpha = 0.05}),
#'   items with absolute robust-z values exceeding this threshold will be marked
#'   as unstable.
#' @param iqr_type An integer indicating the choice of quantile algorithm. Refer
#'   to the \code{?quantile} function's \code{type} argument for more details.
#'   For instance, SAS's default quantile algorithm, \code{QNTLDEF=5},
#'   corresponds to \code{iqr_type = 2} in R. The default value is
#'   \code{iqr_type = 7}.
#'
#' @param theta A numeric vector containing the quadrature points. Only needed
#'   when \code{method = "d2"}.
#' @param weights A numeric vector containing the weights assigned to the
#'   quadrature points. The length of this vector should match the length of the
#'   \code{theta} argument. Only needed when \code{method = "d2"}
#'
#' @return Return a list depending on the method:
#'   \describe{
#'     \item{robust-z}{
#'       \describe{
#'         \item{\code{output$a$cor}}{Correlation between two sets of \eqn{a}
#'           parameters.}
#'         \item{\code{output$a$sd_ratio}}{The ratio of the standard deviation
#'           of \code{ip2} to the standard deviation of \code{ip1}.}
#'         \item{\code{output$a$robust_z}}{Robust-z statistic values for each
#'           item's discrimination parameter.}
#'         \item{\code{output$a$unstable}}{Item IDs that were flagged when the
#'           robust-z statistic value for \eqn{a} parameters exceeded the
#'           absolute value of the critical value
#'           (i.e., \code{qnorm(1-alpha/2)}).}
#'         \item{\code{output$b$robust_z}}{Robust-z statistic values for each
#'           item's difficulty or threshold parameter. If an item has multiple
#'           threshold parameters, robust z statistics will be calculated for
#'           each one.}
#'         \item{\code{output$b$unstable}}{Item IDs that were flagged if the
#'           robust-z statistic for difficulty/threshold parameters exceeded
#'           the absolute value of the critical value
#'           (i.e., \code{qnorm(1-alpha/2)}).}
#'       }
#'     }
#'     \item{d2}{A numeric vector containing the differences between the ICCs
#'       of each item.
#'     }
#'   }
#'
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @references
#' Huynh, Huynh and Meyer, Patrick (2010) "Use of Robust z in Detecting
#' Unstable Items in Item Response Theory Models,"
#' \emph{Practical Assessment, Research, and Evaluation}: Vol. 15 , Article 2.
#' <doi:10.7275/ycx6-e864>
#'
#' @examples
#' ##### Robust-z #####
#' # The example from Huynh and Meyer (2010)
#' ip1 <- c(itempool(
#'   a = c(0.729, 0.846, 0.909, 0.818, 0.742, 0.890, 1.741, 0.907, 1.487, 1.228,
#'         0.672, 1.007, 1.016, 0.776, 0.921, 0.550, 0.624, 0.984, 0.506, 0.594,
#'         0.687, 0.541, 0.691, 0.843, 0.530, 0.462, 1.007, 0.825, 0.608, 1.177,
#'         0.900, 0.861, 0.843, 1.404, 0.446, 1.014, 1.632, 0.831, 1.560, 0.798),
#'   b = c(1.585, 0.635, -0.378, -0.100, -0.195, 0.749, 1.246, 1.016, -0.234,
#'         0.537, 0.070, 1.985, 1.101, -0.742, 0.463, -0.060, 0.477, 1.084,
#'         -2.340, 1.068, -0.055, -1.045, 1.859, 0.645, -0.689, -2.583, 1.922,
#'         0.709, 0.499, 1.973, 0.104, 0.809, 0.640, 0.247, 0.820, 1.837,
#'         2.129, 1.012, 1.774, 0.095),
#'   c = c(0.134, 0.304, 0.267, 0.176, 0.215, 0.194, 0.267, 0.159, 0.095,
#'         0.197, 0.089, 0.272, 0.229, 0.159, 0.162, 0.100, 0.259, 0.167,
#'         0.000, 0.242, 0.323, 0.000, 0.196, 0.189, 0.000, 0.000, 0.334,
#'         0.538, 0.125, 0.511, 0.192, 0.353, 0.103, 0.241, 0.245, 0.118,
#'         0.155, 0.132, 0.215, 0.148),
#'   model = "3PL"),
#'   item(a = 0.561, b = c(0.784, -0.113, 1.166), model = "GPCM"),
#'   item(a = 0.745, b = c(3.687, 2.506, -0.001), model = "GPCM"))
#'
#' ip2 <- c(itempool(
#'   a = c(0.650, 0.782, 0.816, 0.787, 0.611, 0.888, 1.192, 0.589, 1.211,
#'         0.742, 0.526, 0.690, 0.996, 0.816, 0.781, 0.507, 0.378, 0.976,
#'         0.473, 0.364, 0.585, 0.566, 0.511, 0.718, 0.354, 1.080, 0.840,
#'         0.865, 0.528, 0.814, 0.555, 0.701, 0.530, 1.220, 0.344, 0.966,
#'         1.044, 0.358, 1.192, 0.615),
#'   b = c(0.676, -0.525, -1.749, -1.092, -1.619, -0.406, -0.132, 0.006,
#'         -1.352, -0.872, -1.242, 0.873, 0.239, -2.038, -0.487, -1.372,
#'         -1.492, 0.214, -4.537, 0.220, -0.686, -2.394, 0.747, -0.467,
#'         -3.629, -5.000, 0.927, 0.305, -0.839, 1.270, -1.618, -0.091,
#'         -1.228, -1.019, -1.453, 1.090, 1.743, -1.436, 1.024, -1.358),
#'   c = c(0.110, 0.316, 0.161, 0.149, 0.145, 0.200, 0.243, 0.059, 0.081,
#'         0.075, 0.028, 0.267, 0.242, 0.189, 0.184, 0.121, 0.000, 0.170,
#'         0.000, 0.151, 0.383, 0.000, 0.195, 0.177, 0.000, 0.000, 0.352,
#'         0.647, 0.116, 0.501, 0.000, 0.286, 0.000, 0.248, 0.064, 0.150,
#'         0.126, 0.000, 0.187, 0.007),
#'   model = "3PL"),
#'   item(a = 0.486, b = c(-0.539, -1.489, -0.052), model = "GPCM"),
#'   item(a = 0.737, b = c(2.599, 1.250, -1.209), model = "GPCM"))
#' ipd(ip1, ip2)
#'
#' ##### D2 #####
#' ip1 <- generate_ip(n = 20)
#' ip2 <- ip1
#' # add a small nuisance to item difficulty parameters
#' ip2$b <- ip1$b + runif(20, -.5, .5)
#'
#' theta <- seq(-4, 4, 0.2)
#' weights <- dnorm(theta)
#' ipd(ip1, ip2, theta = theta, weights = weights)
#'
#' # Calculate for only certain items
#' ipd(ip1, ip2, theta = theta, weights = weights,
#'   anchor_item_ids = c("Item_2", "Item_6", "Item_9", "Item_13"))
#'
#' ### Polytomous items items
#' n_item <- 30
#' models <- sample(c("3PL", "GPCM2"), n_item, TRUE)
#' new_ip <- generate_ip(model = models, D = 1.702)
#' old_ip <- data.frame(new_ip)
#' old_ip$a <- old_ip$a + round(runif(n_item, min = -.5, max = .5), 2)
#' old_ip$b <- old_ip$b + round(runif(n_item, min = -.75, max = .75), 2)
#' old_ip$d1 <- old_ip$d1 + round(runif(n_item, min = -.75, max = .75), 2)
#' old_ip$d2 <- old_ip$d2 + round(runif(n_item, min = -.75, max = .75), 2)
#' old_ip$d3 <- old_ip$d3 + round(runif(n_item, min = -.75, max = .75), 2)
#' old_ip <- itempool(old_ip)
#'
#' ipd(ip1 = old_ip, ip2 = new_ip, theta = theta, weights = weights)
#'
ipd <- function(ip1, ip2, method = "robust-z", anchor_item_ids = NULL,
                # Robust-z
                alpha = 0.01,
                iqr_type = 7,
                # D2
                theta = seq(-4, 4, 0.1),
                weights = stats::dnorm(seq(-4, 4, 0.1))
                ) {
  if (method == "robust-z") {
    output <- ipd_robustz(ip1 = ip1, ip2 = ip2,
                          anchor_item_ids = anchor_item_ids, alpha = alpha,
                          iqr_type = iqr_type)
  } else if (method == "d2") {
    output <- ipd_d2(ip1 = ip1, ip2 = ip2,
                     anchor_item_ids = anchor_item_ids, theta = theta,
                     weights = weights)
  } else
    stop("This method has not been implemented yet.", call. = FALSE)
  return(output)
}
