

#' check "group" argument of DIF function
#'
#' @description This function checks the 'group' and 'focal_name' arguments of
#'   the DIF functions.
#'   It does the following:
#'   * Convert the \code{resp} to a data.frame object, if it is matrix or
#'     Resp_set object.
#'     - Check whether it is data.frame or matrix.
#'   * Check \code{group} argument:
#'     - It can be a column of the \code{resp} data.frame
#'     - It can be a vector. If so, check whether it has the same length of
#'       the number of rows of the \code{resp}.
#'     - Add this grouping variable to the response data.
#'   * Check \code{focal_name} argument.
#'     - Make sure it is one of the elements of the grouping variable.
#'     - All of the other values will be marked as "Reference Group"
#'
#' @noRd
#'
dif_helper_group_name <- function(resp, group, focal_name) {
  # Convert the resp to data frame.

}


###############################################################################@
############################# dif_mh_dich ######################################
###############################################################################@
#' Calculate DIF for Dichotomous items Using Mantel–Haenszel
#'
#' @param resp A response matrix, \code{Response_set} object, data frame that
#'   contains examinee response, group variable (optional), criterion
#'   variable (optional). Rows are assumed to be examinees and columns are for
#'   items if it is a matrix or data.frame.
#' @param group It can be (a) a string representing column of \code{resp} that
#'   is holding the grouping variable, (b) A vector of values with the same
#'   length as the number of rows of \code{resp}.
#' @param focal_name A single value that represents the focal group. It should
#'   be one of the values of the grouping variable. All of the other values
#'   within the grouping variable will be assumed to be in the reference group.
#'
#'
#' @noRd
dif_mh_dich <- function(resp, group, focal_name) {
  result <- data.frame(item_id = paste0("Item", 1:ncol(resp)),
                       mh_statistic = NA, chisq = NA, p_value = NA,
                       ETS = NA, ETS_class = NA)
  if (!is.null(colnames(resp))) result$item_id <- colnames(resp)
  total_score <- rowSums(resp, na.rm = TRUE)
  K <- sort(unique(total_score))
  group[group == focal_name] <- "focal"
  group[group != "focal"] <- "reference"
  group <- as.factor(group)

  for (itm in seq_len(ncol(resp))) { # itm = item
    n_r1 <- rep(NA, length(K))
    n_r0 <- rep(NA, length(K))
    n_f1 <- rep(NA, length(K))
    n_f0 <- rep(NA, length(K))
    n_r <- rep(NA, length(K))
    n_f <- rep(NA, length(K))
    n_0 <- rep(NA, length(K))
    n_1 <- rep(NA, length(K))
    n <- rep(NA, length(K))
    for (k in seq_len(length(K))) {
      temp <- which(total_score == K[k])
      temp_resp <- factor(resp[temp, itm], levels = unique(resp[, itm]))
      temp <- stats::addmargins(table(group[temp], temp_resp))
      # Number of examinees in the reference group with correct responses
      n_r1[k] <- temp["reference", "1"]
      # Number of examinees in the reference group with incorrect responses
      n_r0[k] <- temp["reference", "0"]
      # Number of examinees in the focal group with correct responses
      n_f1[k] <- temp["focal", "1"]
      # Number of examinees in the focal group with incorrect responses
      n_f0[k] <- temp["focal", "0"]
      # Total number of examinees in the reference group
      n_r[k] <- temp["reference", "Sum"]
      # Total number of examinees in the focal group
      n_f[k] <- temp["focal", "Sum"]
      # Total number of incorrect responses
      n_0[k] <- temp["Sum", "0"]
      # Total number of correct responses
      n_1[k] <- temp["Sum", "1"]
      # Total number of responses
      n[k] <- temp["Sum", "Sum"]
    }
    valid <- n > 1
    alpha_mh <- sum((n_r1 * n_f0)/n) / sum((n_r0 * n_f1)/n)
    mu <- ((n_r * n_1) / n)[valid]
    v <- (n_r * n_f * n_1 * n_0 / (n^2*(n - 1)))[valid]
    result$mh_statistic[itm] <- sum((n_r1 * n_f0 / n)[valid]) /
      sum((n_r0 * n_f1 / n)[valid])
    result$chisq[itm] <- (abs(sum(n_r1[valid]) - sum(mu)) - 0.5)^2 / sum(v)
  }
  result$ETS <- -2.35 * log(result$mh_statistic)
  result$p_value <- 1 - stats::pchisq(q = result$chisq, df = 1)
  result$ETS_class <- ifelse(abs(result$ETS) < 1, "A",
                             ifelse(abs(result$ETS) < 1.5, "B", "C"))
}




###############################################################################@
############################# dif ##############################################
###############################################################################@
#' Evaluate Differential Item Functioning (DIF) of a test
#'
#' @description
#' \code{dif} evaluates Differential Item Functioning (DIF) of a test.
#'
#'
#' @param resp A matrix of item responses.
#' @param group Group membership
#' @param focal_name In the group variable, the value that represents the focal
#'   group.
#' @param ip An \code{\link{Itempool-class}} object.
#' @param type The type of the DIF method.
#'
#' @return A data.frame of DIF values.
#'
#' @include item-class.R
#' @include itempool-class.R
#' @include item-class-methods.R
#' @include itempool-class-methods.R
#'
#' @author Emre Gonulates
#'
#'
#'
dif <- function(resp, group, focal_name, ip = NULL, type = "mh") {
  type <- tolower(match.arg(type))
  if (type == "mh") {
    result <- data.frame(item_id = paste0("Item", 1:ncol(resp)), mh_statistic = NA,
                         chisq = NA, p_value = NA, ETS = NA, ETS_class = NA)
    if (!is.null(colnames(resp))) result$item_id <- colnames(resp)
    total_score <- rowSums(resp, na.rm = TRUE)
    K <- sort(unique(total_score))
    group[group == focal_name] <- "focal"
    group[group != "focal"] <- "reference"
    group <- as.factor(group)

    for (i in seq_len(ncol(resp))) { # i = item
      n_r1 <- rep(NA, length(K))
      n_r0 <- rep(NA, length(K))
      n_f1 <- rep(NA, length(K))
      n_f0 <- rep(NA, length(K))
      n_r <- rep(NA, length(K))
      n_f <- rep(NA, length(K))
      n_0 <- rep(NA, length(K))
      n_1 <- rep(NA, length(K))
      n <- rep(NA, length(K))
      for (k in seq_len(length(K))) {
        temp <- which(total_score == K[k])
        temp_resp <- factor(resp[temp, i], levels = unique(resp[, i]))
        temp <- stats::addmargins(table(group[temp], temp_resp))
        n_r1[k] <- temp["reference", "1"]
        n_r0[k] <- temp["reference", "0"]
        n_f1[k] <- temp["focal", "1"]
        n_f0[k] <- temp["focal", "0"]
        n_r[k] <- temp["reference", "Sum"]
        n_f[k] <- temp["focal", "Sum"]
        n_0[k] <- temp["Sum", "0"]
        n_1[k] <- temp["Sum", "1"]
        n[k] <- temp["Sum", "Sum"]
      }
      temp <- n > 1
      result$mh_statistic[i] <- sum((n_r1 * n_f0 / n)[temp]) / sum((n_r0 * n_f1 / n)[temp])
      mu <- (n_r * n_1 / n)[temp]
      v <- (n_r * n_f * n_1 * n_0 / (n^2*(n - 1)))[temp]
      result$chisq[i] <- (abs(sum(n_r1[temp]) - sum(mu)) - 0.5)^2 / sum(v)
    }
    result$ETS <- -2.35 * log(result$mh_statistic)
    result$p_value <- 1 - stats::pchisq(q = result$chisq, df = 1)
    result$ETS_class <- ifelse(abs(result$ETS) < 1, "A",
                               ifelse(abs(result$ETS) < 1.5, "B", "C"))
  }
  return(result)
}



###############################################################################@
############################# area_between_icc #################################
###############################################################################@
#' Calculate the area between two ICC curves
#'
#' @description This function calculates the area between two item
#'   characteristic curves (ICC) for unidimensional dichotomous IRT models.
#'
#'   There are two types of area calculation methods. The first one is
#'   \code{type = "exact"} where the exact area from negative infinity to
#'   positive infinity between the two ICC curves will be calculated.
#'   This method implements the approach in Raju's 1988 paper. This
#'   method works for 'Rasch', '1PL', '2PL', '3PL' models but when the
#'   pseudo-guessing parameters of the items differ for '3PL' model, the
#'   area will be infinity. In such cases it is advisable to use
#'   \code{type = "closed"}.
#'
#'   The area can only be calculated for 'Rasch', '1PL', '2PL', '3PL' or
#'   '4PL' models.
#'
#' @param ... An \code{\link{Itempool-class}} object; or a combination of
#'   \code{\link{Item-class}} and \code{\link{Testlet-class}} objects.
#' @param type A string representing the method that will be used to calculate
#'   the area between two ICC curves. Available values are:
#'   \describe{
#'     \item{\code{"exact"}}{The exact area between the whole theta scale
#'       \code{-Inf} and \code{Inf}. This method implements Raju's (1988)
#'       approach. When the pseudo-guessing parameters of the items differ
#'       for '3PL' model, the area will be infinity. See Raju (1988) for
#'       details.}
#'     \item{\code{"closed"}}{The area within a closed interval defined by
#'       \code{theta_range} argument will be calculated. This method always
#'       returns a finite value. See Kim and Cohen (1991) for details.}
#'   }
#'   The default method is \code{"closed"}.
#' @param theta_range A numeric vector of length two with the first element
#'   smaller than the second element. The values define the boundaries in which
#'   the area between two ICC's will be calculated. The default value is
#'   \code{c(-5, 5)}.
#' @param signed_area A logical value for whether the signed or unsigned area
#'   between two curves will be calculated. When \code{signed = TRUE}, the
#'   area under the second item is subtracted from the area under the first
#'   item. The result can be negative if the first item is mostly under
#'   the second item. When \code{signed = FALSE}, the distance between two ICC
#'   curves will be calculated. The default value is \code{signed = TRUE}.
#'
#' @return A matrix where the values in cells are the areas between items.
#'   The rows represent the first item and the columns represents the second
#'   item and the area of second item is subtracted from the first item when
#'   "signed" area is desired. For example, the value corresponding to the cell
#'   where row is for "Item_4" and column is for "Item_2", the value in the cell
#'   is the area of "Item_4 - Item_2".
#' @references
#' Kim, S.-H., & Cohen, A. S. (1991). A comparison of two area measures for
#' detecting differential item functioning. Applied Psychological Measurement,
#' 15(3), 269–278.
#'
#' Raju, N. S. (1988). The area between two item characteristic curves.
#' Psychometrika, 53(4), 495–502.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' # Closed area example:
#' ip <- generate_ip(model = c("3PL", "3PL", "3PL"))
#' # plot(ip) # See the ICCs
#' area_between_icc(ip, type = "closed")
#' area_between_icc(ip, type = "closed", signed_area = TRUE)
#'
#' # The result is infinite because 'c' parameters are not equal
#' area_between_icc(ip, type = "exact")
#'
#' # Exact area example:
#' ip <- generate_ip(model = c("2PL", "2PL", "2PL"))
#' area_between_icc(ip, type = "exact")
#' area_between_icc(ip, type = "exact", signed_area = TRUE)
#' # The 'closed' area is very close to the 'exact' area with a wide theta range
#' area_between_icc(ip, type = "closed", theta_range = c(-10, 10))
#'
area_between_icc <- function(..., type = c("closed", "exact"),
                             theta_range = c(-5, 5), signed_area = FALSE) {
  args <- list(...)
  type <- match.arg(type)
  error_invalid_ip <- paste0("Invalid arguments. Please provide an Itempool ",
                             "or at least two Item objects.")
  if (length(args) == 0) stop(error_invalid_ip)
  x <- args[[1]]
  item_list <- NULL
  if (is(x, "Itempool")) {
    item_list <- flatten_itempool_cpp(x)
  } else if (inherits(x, c("Item", "Testlet"))) {
    item_list_indicator <- sapply(args, function(m) is(m, "Item") |
                                    is(m, "Testlet"))
    item_list <- flatten_itempool_cpp(itempool(args[item_list_indicator]))
  } else stop(error_invalid_ip)
  # if (length(item_list) < 2) stop(error_invalid_ip)
  if (type == "closed") {
    return(outer(item_list, item_list, function(x,y) vapply(
      seq_along(x), function(i) area_between_icc_closed_cpp(
        x[[i]],y[[i]], signed_area = signed_area, theta_range = theta_range),
      numeric(1))))
  } else if (type == "exact") {
    return(outer(item_list, item_list, function(x,y) vapply(
      seq_along(x), function(i) area_between_icc_exact_cpp(
        x[[i]],y[[i]], signed_area = signed_area), numeric(1))))
  }
}


