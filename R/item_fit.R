

###############################################################################@
############################# .item_fit_sort_examinees #########################
###############################################################################@
#' Sort examinees into approximately equally sized n_groups
#'
#' @description
#' This function separates examinees into approximately equally sized n_groups
#' and returns a data frame with following columns:
#' "examinee_id", "theta", "group"
#' From Yen (1981): " To calculate Q1, examinees are rank ordered on the
#' basis of their trait estimates and then divided into 10 cells with
#' approximately equal numbers of examinees per cell." (p.246)
#'
#' @param resp_set A \code{\link{Response_set-class}} object containing the
#'   item responses.
#' @param theta An vector containing ability parameters.
#' @param n_groups An integer representing the number of groups of examinees.
#'
#' @noRd
.item_fit_sort_examinees <- function(resp_set, theta, n_groups) {
  examinee_ids <- resp_set$examinee_id
  df <- data.frame(examinee_id = examinee_ids, theta = theta)
  df <- df[order(df$theta), ]
  df$group <- as.integer(cut(1:nrow(df), breaks = n_groups))
  # put into the original order
  df <- df[match(examinee_ids, df$examinee_id), ]
  return(df)
}




###############################################################################@
############################# .item_fit_q1 #####################################
###############################################################################@
.item_fit_q1 <- function(
    ip,
    resp,
    theta = NULL,
    item_ids = NULL,
    n_groups = NULL
  ) {

  # Convert resp into "Response_set" object
  if (is(resp, "Response_set")) resp_set <- resp else
    resp_set <- convert_to_resp_set(resp, ip)
  # The length of theta and resp should be the same, else raise an error
  if (!is.null(theta) && length(resp_set) != length(theta))
    stop("The length of 'theta' and 'resp' should be the same.", call. = FALSE)

  resp_matrix <- as.data.frame(as.matrix(resp_set, ip = ip))
  resp_matrix <- cbind(data.frame(examinee_id = resp_set$examinee_id, theta = theta),
                       resp_matrix)

  # Check whether all items are dichotomous.
  if (!all(ip$model %in% UNIDIM_DICHO_MODELS))
    stop("item_fit(): For 'Q1' statistic, all items should be dichotomous items and ",
         "item pool parameters should follow one of the following models: ",
         paste0(paste0("\"", UNIDIM_DICHO_MODELS, "\""), collapse = ", "), ".",
         call. = FALSE)

  # The default number of groups is 10.
  if (!is.null(n_groups)) n_groups <- as.integer(n_groups)
  if (!is_single_value(n_groups, class = "integer")) n_groups <- 10
  if (is.null(theta) || length(resp) != length(theta)) {
    theta <- est_ability(resp = resp, ip = ip, method = "ml")$est
  }

  if (!is_atomic_vector(item_ids) || !all(item_ids %in% ip$resp_id)) {
    item_ids <- ip$resp_id
  }

  # DeMars (2005, p.43): "The degrees of freedom are equal to the number of
  # score intervals multiplied by one less than the number of categories. When
  # there are only two categories (a dichotomous item), this fit index reduces
  # to the index available from BILOG (Mislevy & Bock, 1990), with degrees of
  # freedom equal to the number of score interval groups. Some other fit indices
  # correct the degrees of freedom for the number of item parameters estimated.
  # For example, Yen’s (1981) Q1 index, proposed for use with dichotomous items,
  # involves subtracting 3 from the degrees of freedom for the three-parameter
  # logistic model (3-PL), 2 for the two- parameter logistic model (2-PL), and 1
  # for the one-parameter logistic model (1-PL). "
  m <- sapply(PMODELS[ip$model[item_ids]], function(x) length(unlist(
    sapply(x$parameters, function(t) t$se))))
  result <- data.frame(item_id = item_ids, Q1 = NA, df = n_groups - m)

  for (item_id in item_ids) {
    # Select rows with non missing theta and item_id values
    df <- resp_matrix[!is.na(resp_matrix[item_id]) & !is.na(resp_matrix[["theta"]]),
                      c("examinee_id", "theta", item_id)]
    names(df) <- c("examinee_id", "theta", "resp")

    # From Yen (1981): " To calculate Q1, examinees are rank ordered on the
    # basis of their trait estimates and then divided into 10 cells with
    # approximately equal numbers of examinees per cell." (p.246)

    # Put examinees into ability groups based on n_groups
    df <- df[order(df$theta), ]
    df$group <- as.integer(cut(1:nrow(df), breaks = n_groups))

    # Calculate expected values
    df$prob <- mean(x = ip[item_id], theta = df$theta)[, 1]
    O <- stats::aggregate(x = df$resp, by = list(group = df$group), FUN = mean)
    E <- stats::aggregate(x = df$prob, by = list(group = df$group), FUN = mean)
    Ng <- stats::aggregate(x = df$resp, by = list(group = df$group), FUN = length)
    q1 <- sum(Ng$x * (O$x - E$x)^2 / (E$x * (1 - E$x)))

    result[result$item_id == item_id, "Q1"] <- q1
  }

  result$p_value <- stats::pchisq(result$Q1, df = result$df,
                                  lower.tail = FALSE)
  return(result)
}



###############################################################################@
############################# item_fit #########################################
###############################################################################@
#' Calculate item-fit indices
#' @description
#' \code{item_fit} calculates the fit of an item to a given psychometric model.
#'
#'
#' @param ip An \code{\link{Itempool-class}} object.
#' @param resp A \code{\link{Response_set-class}} object, \code{matrix} or
#'   \code{data.frame} containing the item responses.
#' @param theta An vector containing ability parameters. When
#'   \code{type = "Q1"} and \code{theta = NULL} or an invalid \code{theta}
#'   vector provided, theta values will be estimated using item parameters and
#'   responses. In order to speed up the function for large data sets, theta
#'   values can be supplied. The order of the theta vector should be the same
#'   of the rows of the response data.
#' @param type The type of the item-fit index. Currently the following indices
#'   are available:
#'   \describe{
#'     \item{"Q3"}{Yen's Q3 index (Yen, 1984)}
#'     \item{"Q1"}{Yen's Q1 index (Yen, 1981). Only available for unidimensional
#'       dichotomous items.}
#'     \item{"G2"}{PARSCALE's fit statistic. See DeMars (2005) for details.}
#'   }
#'
#' The default value is \code{"Q1"}.
#'
#' @param item_id A string vector that is holding the ID's of the item for
#'   which item fit should be calculated. The default value is \code{NULL}
#'   where item fit statistic of all items will be calculated.
#' @param n_groups An integer representing the number of groups of examinees.
#'   When \code{type = "Q1"} and \code{n_groups = NULL}, the default value
#'   will be 10 (as specified in Yen (1981)). For example, if there are
#'   900 examinees, when \code{n_groups = 10}, first examinees will be sorted
#'   according to their theta scores and separated into 10 equally sized groups
#'   of approximately 90 examinees each. The same default value is used when
#'   \code{type = "G2"}.
#'
#' @return A vector of item-fit index values for \code{Q1} and \code{G2}.
#'   A correlation matrix will be returned for \code{Q3}.
#'
#' @details
#' # Yen's Q3
#'
#' The details of Yen's Q3 can be found in Yen (1984). It is mainly used as a
#' measure of local dependence between two set of items.
#'
#'
#' # Yen's Q1
#'
#' The details of Yen's Q1 can be found in Yen (1981). Please note that Q1
#' can have inflated Type-I error rates (Orlando & Thissen, 2000).
#'
#' # PARSCALE's G2
#'
#' PARSCALE's fit statistic G2 is explained in Kang and Chen (2008) and
#' DeMars (2005) in detail. DeMars also detailed the situations when G2 index
#' yields inflated Type-I error rates. Specifically, she did not recommend this
#' index for short tests.
#'
#'
#' @include item-class.R
#' @include itempool-class.R
#' @include item-class-methods.R
#' @include itempool-class-methods.R
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @references
#'
#' DeMars, C. E. (2005). Type I error rates for PARSCALE's fit index.
#'   Educational and psychological measurement, 65(1), 42-50.
#'
#' Kang, T., & Chen, T. T. (2008). Performance of the generalized S-X2 item
#'   fit index for polytomous IRT models.
#'   *Journal of Educational Measurement*, 45(4), 391–406.
#'   <doi:10.1111/j.1745-3984.2008.00071.x>
#'
#' Orlando, M., & Thissen, D. (2000). New item fit indices for dichotomous
#'   item response theory models. Applied Psychological Measurement, 24, 50–64.
#'
#' Yen, W. M. (1981). Using simulation results to choose a latent trait model.
#'   *Applied Psychological Measurement*, 5(2), 245–262.
#'   <doi:10.1177/014662168100500212>
#'
#' Yen, W. M. (1984). Effects of local item dependence on the fit and equating
#'   performance of the three-parameter logistic model.
#'   *Applied Psychological Measurement*, 8(2), 125–145.
#'
#' @examples
#' ip <- generate_ip(model = "3PL", n = 10)
#' theta <- rnorm(1000)
#' resp <- sim_resp(ip = ip, theta = theta, output = "response_set")
#'
#' ### Yen's Q1 ###
#' # Calculate Yen's Q1 for all items
#' item_fit(ip = ip, resp = resp, theta = theta, type = "Q1")
#'
#' # Calculate Yen's Q1 for only selected items
#' item_fit(ip = ip, resp = resp, theta = theta, type = "Q1",
#'          item_id = c("Item_3", "Item_5"))
#'
#' # Change the number of groups examinees will be separated into:
#' item_fit(ip = ip, resp = resp, theta = theta, type = "Q1", n_groups = 15)
#'
item_fit <- function(ip, resp, theta = NULL, type = "Q1", item_id = NULL,
                     n_groups = NULL) {

  # Convert resp into "Response_set" object
  if (is(resp, "Response_set")) resp_set <- resp else
    resp_set <- convert_to_resp_set(resp, ip)

  if (tolower(type) == "q3") {
    # Convert resp into "Response_set" object
    if (is.null(theta) || length(resp_set) != length(theta)) {
      theta <- est_ability(resp = resp_set, ip = ip, method = "ml")$est
    }
    resp_matrix <- as.matrix(resp_set, ip = ip)
    expected_score <- mean(x = ip, theta = theta)
    d <- resp_matrix - expected_score
    return(stats::cor(d, use = "pairwise.complete.obs"))
    ############### Q1 #####################################################@###
  } else if (tolower(type) == "q1") {
    return(.item_fit_q1(ip = ip, resp = resp_set, theta = theta,
                        item_id = item_id, n_groups = n_groups))
    ############### G2 #####################################################@###
  } else if (tolower(type) == "g2") {
    # Check whether all items are dichotomous.
    if (!all(ip$model %in% c(UNIDIM_DICHO_MODELS, UNIDIM_POLY_MODELS)))
      stop("For 'G2' statistic, all items should follow one of the following ",
           "models: ", paste0(paste0("\"", c(UNIDIM_DICHO_MODELS, UNIDIM_POLY_MODELS),
                                     "\""), collapse = ", "), ".")

    # The default number of groups is 10.
    if (!is.null(n_groups)) n_groups <- as.integer(n_groups)
    if (!is_single_value(n_groups, class = "integer")) n_groups <- 10
    if (is.null(theta) || length(resp_set) != length(theta)) {
      theta <- est_ability(resp = resp, ip = ip, method = "ml")$est
    }

    if (!is_atomic_vector(item_id) || !all(item_id %in% ip$resp_id)) {
      item_id <- ip$resp_id
    }
    df <- .item_fit_sort_examinees(resp_set = resp_set, theta = theta,
                                   n_groups = n_groups)
    result <- rep(0, length(item_id))

    max_scores <- max_score(ip = ip[item_id], sum = FALSE)

    for (g in sort(unique(df$group))) {
      indices <- which(df$group == g)
      temp_resp <- as.matrix(resp_set[indices], ip = ip)[, item_id,
                                                         drop = FALSE]
      na_indices <- is.na(temp_resp)

      temp_theta <- matrix(df$theta[indices], nrow = length(indices),
                           length(item_id))
      temp_theta[na_indices] <- NA
      mean_theta <- colMeans(temp_theta, na.rm = TRUE)
      Ng <- colSums(!na_indices)
      for (i in 1:length(item_id)) {
        temp_scores <- 0:max_scores[i]
        P <- resp_lik(ip = ip[[i]], resp = temp_scores,
                      theta = rep(mean_theta[i], length(temp_scores)))
        r <- table(factor(temp_resp[, i], levels = temp_scores, ordered = TRUE))
        temp_result <- r/(Ng[i] * P)
        result[i] <- result[i] + sum(r * ifelse(temp_result > 0,
                                                log(temp_result), 0))
      }
    }
    result <- result * 2
    # From DeMars (2005, p. 43): "The degrees of freedom are equal to the number
    # of score intervals multiplied by one less than the number of categories."
    # Number of categories = (max_scores + 1)
    # One less then number of categories = max_scores
    result <- data.frame(item_id = item_id, G2 = result,
                         df = max_scores * n_groups)
    result$p_value <- stats::pchisq(result$G2, df = result$df,
                                    lower.tail = FALSE)
    return(result)
  } else {
    stop("This method has not been implemented yet.", call. = FALSE)
  }
}
