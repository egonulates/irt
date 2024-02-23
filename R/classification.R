
############################################################################@###
################### partition_scores ###########################################
############################################################################@###

partition_scores <- function(score, cut_scores, cat_labels,
                             include.lowest = TRUE,
                             right = FALSE) {
  breaks <- cut_scores
  if (!is.infinite(breaks[1])) breaks <- c(-Inf, breaks)
  if (!is.infinite(utils::tail(breaks, 1))) breaks <- c(breaks, Inf)
  cut(score,
      breaks = breaks,
      include.lowest = include.lowest,
      right = right, ordered_result = TRUE, labels = cat_labels)
}


############################################################################@###
################### ca_perf_cat ################################################
############################################################################@###

#' Calculate performance categories of examinees using scores and cut scores
#'
#' @param theta A numeric vector representing the abilities of examinees.
#' @param theta_cs A numeric vector representing the theta scale cut scores.
#'   Do not include \code{-Inf} or \code{Inf}.
#' @param raw_score A numeric vector representing the summed scores of
#'   examinees.
#' @param raw_cs A numeric vector representing the raw (summed-score) cut
#'   scores. Do not include \code{0} or maximum possible score.
#' @param perf_categories An integer vector representing the performance
#'   categories of examinees. The number 1 should represent the lowest category.
#'   For example if there are three cut scores the valid values can only be:
#'   0, 1, 2 and 3.
#' @param cat_labels A string vector representing the labels of the categories.
#'   The length of the vector should be one more than the length of the
#'   cut scores. The default value is \code{NULL} where the categories will be
#'   labeled as 1, 2, ..., (number of cut scores plus one). For example, if
#'   there are three cut scores category labels can be:
#'   \code{c("Unsatisfactory", "Basic", "Mastery", "Advanced")}.
#'
#' @return Return an integer vector of the categories examinee scores falls
#'   into. The first (lowest catetory) will be 1 and it will go up the
#'   number of cut scores plus one.
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
#' @examples
#' ip <- generate_ip(model = sample(c("GPCM", "2PL"), 10, TRUE))
#' theta <- seq(-2, 2, .2)
#' theta_cs <- c(-1, 0, 1.5) # Theta cut scores
#' raw_cs <- round(rsss(ip = ip, scale_score = theta_cs)) # Raw cut scores
#' raw_score <- round(rsss(ip = ip, scale_score = theta))
#'
#' cbind(theta, raw_score,
#'       tt = ca_perf_cat(theta = theta, theta_cs = theta_cs),
#'       tr = ca_perf_cat(theta = theta, raw_cs = raw_cs, ip = ip),
#'       rr = ca_perf_cat(raw_score = raw_score, raw_cs = raw_cs),
#'       rt = ca_perf_cat(raw_score = raw_score, theta_cs = theta_cs, ip = ip)
#'       )
#'
#' # Assign labels:
#' ca_perf_cat(theta = theta, theta_cs = theta_cs)
#' ca_perf_cat(theta = theta, theta_cs = theta_cs,
#'             cat_labels = c("Unsatisfactory", "Basic", "Mastery", "Advanced"))
#'
ca_perf_cat <- function(theta = NULL, theta_cs = NULL, raw_score = NULL,
                        raw_cs = NULL, ip = NULL, perf_categories = NULL,
                        cat_labels = NULL) {

  if (!is.null(perf_categories)) {
    return(perf_categories)
  }

  if (!is.null(raw_cs)) {
    n_categories <- length(raw_cs) + 1
  } else if (!is.null(theta_cs)) {
    n_categories <- length(theta_cs) + 1
  } else {
    stop("Either 'theta_cs' or 'raw_cs' should be provided.")
  }

  if (is.null(cat_labels)) {
    cat_labels <- seq(1, n_categories)
  } else if (length(cat_labels) != n_categories) {
    stop("Invalid 'cat_labels'. The length of 'cat_labels' should be equal ",
         "to the number of cut scores plus one.")
  }

  if (is.null(theta)) {
    if (is.null(raw_score)) {
      stop("Either 'theta' or 'raw_score' should be provided.")
    }
    # Check cut scores
    if (!is.null(raw_cs)) {
      return(partition_scores(raw_score, raw_cs, cat_labels))
    } else if (!is.null(theta_cs)) {
      # Convert theta cut scores to raw cut scores
      if (!is(ip, "Itempool")) {
        stop("A valid 'ip' is necessary to calculate the cut scores.")
      }
      raw_cs <- rsss(ip = ip, scale_score = theta_cs)
      return(partition_scores(raw_score, raw_cs, cat_labels))
    } else {
      stop("Either 'theta_cs' or 'raw_cs' should be provided.")
    }
  } else {
    # Check cut scores
    if (!is.null(theta_cs)) {
      return(partition_scores(theta, theta_cs, cat_labels))
    } else if (!is.null(raw_cs)) {
      if (!is(ip, "Itempool")) {
        stop("A valid 'ip' is necessary to calculate the cut scores.")
      }
      # Convert theta scores to raw scores:
      raw_score <- rsss(ip = ip, scale_score = theta)
      return(ca_perf_cat(raw_cs = raw_cs, raw_score = raw_score,
                         cat_labels = cat_labels))
    } else {
      stop("Either 'theta_cs' or 'raw_cs' should be provided.")
    }
  }
}


############################################################################@###
################### ca_perf_cat_matrix #########################################
############################################################################@###

#' Calculate an indicator matrix for the performance category of examinees
#'
#' @param theta A numeric vector representing the abilities of examinees.
#' @param theta_cs A numeric vector representing the theta scale cut scores.
#'   Do not include \code{-Inf} or \code{Inf}.
#' @param raw_score A numeric vector representing the summed scores of
#'   examinees.
#' @param raw_cs A numeric vector representing the raw (summed-score) cut
#'   scores. Do not include \code{0} or maximum possible score.
#' @param perf_categories An integer vector representing the performance
#'   categories of examinees. The number 1 should represent the lowest category.
#'   For example if there are three cut scores the valid values can only be:
#'   0, 1, 2 and 3.
#'
#' @return A indicator matrix of 0's and 1's. Each row represent an examinee.
#'   The value 1 in a row indicates the category of the examinee falls into.
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
#' @examples
#' # Use theta and cut_scores
#' ca_perf_cat_matrix(theta = seq(-2.5, 2.5, .5) , theta_cs = c(-1, 0, 2))
#' ca_perf_cat_matrix(raw_score = seq(0, 10, 1) , raw_cs = c(3, 7))
#'
#' # Assign labels:
#' ca_perf_cat_matrix(theta = seq(-2.5, 2.5, .5) , theta_cs = c(-1, 0, 2),
#'                    cat_labels = c("Unsatisfactory", "Basic", "Mastery",
#'                    "Advanced"))
#'
#' # Use perf_categories
#' perf_categories <- sample(1:4, 15, TRUE)
#' ca_perf_cat_matrix(perf_categories = perf_categories)
#'
ca_perf_cat_matrix <- function(theta = NULL, theta_cs = NULL,
                               raw_score = NULL, raw_cs = NULL,
                               ip = NULL, perf_categories = NULL,
                               cat_labels = NULL) {
  categories <- ca_perf_cat(theta = theta, theta_cs = theta_cs,
                            raw_score = raw_score, raw_cs = raw_cs,
                            ip = ip, perf_categories = perf_categories,
                            cat_labels = cat_labels)
  if (!is.null(perf_categories)) {
    num_of_categories <- length(unique(perf_categories))
  } else if (!is.null(raw_cs)) {
    num_of_categories <- length(raw_cs) + 1
  } else if (!is.null(theta_cs)) {
    num_of_categories <- length(theta_cs) + 1
  } else
    stop("'num_of_categories' cannot be created. A valid 'theta_cs' or ",
         "'raw_cs' or 'perf_categories' is necessary.")
  result <- matrix(0, nrow = length(categories),
                   ncol = num_of_categories,
                   dimnames = list(names(theta), cat_labels))
  result[cbind(seq_along(categories), categories)] <- 1
  return(result)
}


############################################################################@###
################### ca_irt_ca_cc ###############################################
############################################################################@###

#' Calculate classification accuracy and consistency for IRT based methods
#'
#' @param cat_prob A matrix representing falling into a category probilities of
#'   examinees. Rows are for examinees, columns are for categories.
#' @param theta_cs A numeric vector representing the theta scale cut scores.
#'   Do not include \code{-Inf} or \code{Inf}.
#' @param theta A numeric vector representing the abilities of examinees. This
#'   vector will be used to get performance category of each examinee if
#'   \code{perf_categories} is \code{NULL}. The default value is \code{NULL}.
#'   Either \code{theta} or \code{perf_categories} should be provided.
#' @param perf_categories An integer  vector representing the performance
#'   categories of examinees. The number 1 should represent the lowest category.
#'   For example if there are three cut scores the valid values can only be: 0,
#'   1, 2 and 3. This  vector will be used \code{theta} is \code{NULL}. The
#'   default value is \code{NULL}. Either \code{theta} or \code{perf_categories}
#'   should be provided.
#'
#' @return A list of following elements:
#'   \describe{
#'     \item{\code{category_prob}}{A numeric vector representing the
#'       performance category classification probabilities of each examinee.}
#'     \item{\code{ca}}{Marginal (overall) classification accuracy index}
#'     \item{\code{cc}}{Marginal (overall) classification consistency index}
#'     \item{\code{ind_cs_ca}}{Individual cut score classification accuracy
#'       indices. This value will only be calculated when there are more than
#'       one cut score.}
#'     \item{\code{ind_cs_ca}}{Individual cut score classification consistency
#'       indices. This value will only be calculated when there are more than
#'       one cut score.}
#'   }
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
ca_irt_ca_cc <- function(cat_prob, theta_cs, theta = NULL, raw_cs = NULL,
                         perf_categories = NULL, ip = NULL, cat_labels = NULL) {
  output <- list()
  # Create an indicator matrix of 0-1's where a 1 in a row indicates an
  # examinees performance category
  cat_indicator <- ca_perf_cat_matrix(theta = theta, theta_cs = theta_cs,
                                      raw_cs = raw_cs, ip = ip,
                                      perf_categories = perf_categories,
                                      cat_labels = cat_labels)
  # Probabilities of each examinee being categorized into the designated
  # category.
  output$category_prob <- rowSums(cat_prob * cat_indicator, na.rm = TRUE)
  # Overall classification accuracy
  output$ca <- mean(output$category_prob, na.rm = TRUE)
  # Classification consistency
  output$cc <- mean(rowSums(cat_prob * cat_prob), na.rm = TRUE)

  # Calculate classification accuracy at individual cut scores
  if (length(theta_cs) > 1 && !is.null(theta)) {
    output$ind_cs_ca <- c()
    output$ind_cs_cc <- c()
    for (i in seq_along(theta_cs)) {
      # Category probabilities of examinees at one cut score
      temp_cat_prob <- data.frame(
        below = rowSums(cat_prob[, 1:i, drop = FALSE]),
        above = rowSums(cat_prob[, seq(i + 1, ncol(cat_prob)), drop = FALSE]))
      temp_cat_indicator <- ca_perf_cat_matrix(theta = theta,
                                               theta_cs = theta_cs[i])
      temp_category_prob <- rowSums(temp_cat_prob * temp_cat_indicator)
      temp_ca <- mean(temp_category_prob, na.rm = TRUE)
      temp_cc <- mean(rowSums(temp_cat_prob * temp_cat_prob), na.rm = TRUE)

      output$ind_cs_ca <- c(output$ind_cs_ca,
                            setNames(temp_ca, paste0("ca_", i, "_vs_", i + 1)))
      output$ind_cs_cc <- c(output$ind_cs_cc,
                            setNames(temp_cc, paste0("cc_", i, "_vs_", i + 1)))
    }
  }
  return(output)
}


############################################################################@###
################### ca_irt_rudner ##############################################
############################################################################@###

#' Calculate Rudner's expected classification accuracy and consistency index
#'
#' @description This function calculates the expected classification accuracy
#'   and consistency index developed by Rudner (2000, 2005) and described in
#'   Wyse & Hao (2012).
#'
#' @param theta A numeric vector representing the abilities of examinees.
#' @param se A numeric vector representing the standard error of ability
#'   estimates.
#' @param theta_cs A numeric vector representing the theta scale cut scores.
#'   Do not include \code{-Inf} or \code{Inf}.
#' @param perf_categories An integer vector representing the performance
#'   categories of examinees. The number 1 should represent the lowest category.
#'   For example if there are three cut scores the valid values can only be:
#'   0, 1, 2 and 3. This is optional and default value is \code{NULL}. If the
#'   value is \code{NULL}, \code{theta} values will be used to calculate
#'   performance categories.
#'
#' @return A list of following elements:
#'   \describe{
#'     \item{\code{category_prob}}{A numeric vector representing the
#'       performance category classification probabilities of each examinee.}
#'     \item{\code{ca}}{Marginal (overall) classification accuracy index}
#'     \item{\code{cc}}{Marginal (overall) classification consistency index}
#'     \item{\code{ind_cs_ca}}{Individual cut score classification accuracy
#'       indices. This value will only be calculated when there are more than
#'       one cut score.}
#'   }
#'
#' @references
#' Rudner, L. M. (2000). Computing the expected proportions of misclassified
#' examinees. Practical Assessment, Research, and Evaluation, 7(1), 14.
#'
#' Rudner, L. M. (2005). Expected classification accuracy. Practical Assessment,
#' Research, and Evaluation, 10(1), 13.
#'
#' Wyse, A. E., & Hao, S. (2012). An evaluation of item response theory
#' classification accuracy and consistency indices. Applied Psychological
#' Measurement, 36(7), 602-624.
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
#' @examples
#' result <- ca_irt_rudner(theta = rnorm(100),
#'                         se = runif(100, .3, .6),
#'                         theta_cs = c(-1, 0, 1.5))
#' # Category probability
ca_irt_rudner <- function(theta, se, theta_cs, perf_categories = NULL,
                          cat_labels = NULL) {
  cs <- c(-Inf, theta_cs, Inf)
  num_of_examinees <- length(theta)
  n <- length(theta)
  # matrix that holds the expected probabilities of each examinee falling
  # into a  category. Rows examinees, columns categories
  cat_prob <- matrix(NA, nrow = num_of_examinees, ncol = length(theta_cs) + 1,
                   dimnames = list(names(theta), NULL))
  # Calculate the expected probabilities for each cut score
  for (i in seq(2, length(cs))) {
    cat_prob[, i - 1] <- stats::pnorm(cs[i], theta, se) -
      stats::pnorm(cs[i - 1], theta, se)
  }

  return(ca_irt_ca_cc(cat_prob = cat_prob, theta_cs = theta_cs,
                      theta = theta, perf_categories = perf_categories,
                      cat_labels = cat_labels))
}


############################################################################@###
################### ca_irt_guo #################################################
############################################################################@###

#' Calculate Guo's classification accuracy and consistency indices
#'
#' @param ip An \code{\link{Itempool-class}} object. Item pool parameters can
#'   be composed of any combination of unidimensional dichotomous or polytomous
#'   items.
#' @param resp A \code{\link{Response_set-class}}, \code{matrix} or a
#'   \code{data.frame} object that holds responses. If \code{matrix} or a
#'   \code{data.frame} provided, they will be converted to a
#'   \code{\link{Response_set-class}}, .
#' @param theta_cs A numeric vector representing the theta scale cut scores.
#'   Do not include \code{-Inf} or \code{Inf}.
#' @param theta A numeric vector representing the abilities of examinees. This
#'   vector will be used to get performance category of each examinee if
#'   \code{perf_categories} is \code{NULL}. The default value is \code{NULL}.
#'   Either \code{theta} or \code{perf_categories} should be provided.
#' @param perf_categories An integer  vector representing the performance
#'   categories of examinees. The number 1 should represent the lowest category.
#'   For example if there are three cut scores the valid values can only be: 0,
#'   1, 2 and 3. This  vector will be used \code{theta} is \code{NULL}. The
#'   default value is \code{NULL}. Either \code{theta} or \code{perf_categories}
#'   should be provided.
#' @param n_theta An integer representing the number of equally spaced theta
#'   points between cut scores. The default value is 100. Use larger values to
#'   increase accuracy but larger numbers will also slow the speed of
#'   calculation.
#' @param theta_lower_bound A number representing the lower bound for cut
#'   scores. The default value is -6.
#' @param theta_upper_bound A number representing the upper bound for cut
#'   scores. The default value is 6.
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
#' @references
#' Guo, F. (2006). Expected classification accuracy using the latent
#' distribution. Practical Assessment, Research, and Evaluation, 11(1), 6.
#'
#' Wyse, A. E., & Hao, S. (2012). An evaluation of item response theory
#' classification accuracy and consistency indices. Applied Psychological
#' Measurement, 36(7), 602-624.
#'
#' @examples
#' ip <- generate_ip(model = sample(c("GPCM", "2PL"), 20, TRUE))
#' n_examinee <- 100
#' theta <- rnorm(n_examinee)
#' resp_set <- generate_resp_set(ip = ip, theta = theta, prop_missing = .2)
#' theta_est <- est_ability(resp = resp_set, ip = ip, method = "eap")
#' theta_est <- theta_est$est
#' theta_cs <- c(-1, 0, 1.5)
#' result <- ca_irt_guo(ip = ip, resp = resp_set, theta_cs = theta_cs,
#'                      theta = theta_est)
#' result$ca # Classification accuracy index
#' result$cc # Classification consistency index
#'
#' # Relationship between classification accuracy and theta estimates
#' plot(x = theta_est, y = result$category_prob, xlab = "Theta",
#'      ylab = "Classification Probability", ylim = c(0, 1))
#'
ca_irt_guo <- function(ip, resp, theta_cs, theta = NULL,
                       perf_categories = NULL, n_theta = 100,
                       theta_lower_bound = -6, theta_upper_bound = 6,
                       cat_labels = NULL) {
  if (is.null(theta) && is.null(perf_categories))
    stop("Either 'theta' or 'perf_categories' should be provided.")

  resp_set <- response_set(resp, ip = ip)
  num_of_categories <- length(theta_cs) + 1
  num_of_examinees <- length(resp_set)
  cs <- c(theta_lower_bound, theta_cs, theta_upper_bound)

  # cat_prob matrix will hold performance category probabilities for each
  # examinee.
  cat_prob <- matrix(NA, ncol = num_of_categories, nrow = num_of_examinees,
                     dimnames = list(resp_set$examinee_id, 1:num_of_categories))
  for (i in 1:(length(cs) - 1)) {
    # Each row correspond one of the n_theta theta values, and columns represent
    # the examinees.
    cat_prob[, i] <- colSums(sapply(resp$response_list, function(x) sapply(
      seq(cs[i], cs[i + 1], length.out = n_theta),
      resp_lik_response_cpp, resp = x, ip = ip)))
  }
  cat_prob <- cat_prob / matrix(rowSums(cat_prob), nrow = num_of_examinees,
                                ncol = num_of_categories)

  return(ca_irt_ca_cc(cat_prob = cat_prob, theta_cs = theta_cs,
                      theta = theta, perf_categories = perf_categories,
                      cat_labels = cat_labels))
}

############################################################################@###
################### ca_irt_recursive_cond_category_prob ########################
############################################################################@###

#' Calculate conditional category probabilities using recursive method
#'
#' @description This function calculates conditional category probability of a
#'   theta value at each category. This function corresponds to the Equation 2
#'   of Lee (2010, p.3).
#'
#' @param ip An \code{\link{Itempool-class}} object. Item pool parameters can
#'   be composed of any combination of unidimensional dichotomous or polytomous
#'   items.
#' @param theta A numeric vector of examinee ability (theta) values.
#' @param raw_cs A sorted (ascending order) numeric vector of summed-score cut
#'   score values. Do not include 0 or the maximum possible score of the test
#'   in this vector.
#'
#' @return This function returns a data frame where rows represent categories
#'   examinees categorized into and columns represent the theta values. For
#'   example, the value in cell \code{[1, 2]} represent the probability of
#'   the second examinee (with theta value \code{theta[2]}) categorized into
#'   the first category. Note that if \code{raw_cs} has two cut scores, there
#'   will be three categories that examinees can be categorized into.
#'
#' @references
#' Lee, W. C. (2010). Classification consistency and accuracy for complex
#' assessments using item response theory. Journal of Educational Measurement,
#' 47(1), 1-17.
#'
#' @noRd
#'
#' @author Emre Gonulates
#'
#' @examples
#' ip <- generate_ip(model = sample(c("GPCM", "2PL"), 15, TRUE))
#' theta <- round(rnorm(5), 1)
#' raw_cut_scores <- c(5, 11)
#' ca_irt_recursive_cond_category_prob(ip, theta, raw_cut_scores)
#'
ca_irt_recursive_cond_category_prob <- function(ip, theta, raw_cs,
                                                cat_labels = NULL) {
  max_scr <- max_score(ip)
  ss_prob <- data.frame(raw_score = seq(0, max_scr))
  ss_prob$cs_group <- ca_perf_cat(raw_score = ss_prob$raw_score,
                                  raw_cs = raw_cs, cat_labels = cat_labels)
  ss_prob <- cbind(ss_prob, prob_sum_score(ip = ip, theta = theta))

  result <- matrix(NA, nrow = length(theta), ncol = length(raw_cs) + 1,
                   dimnames = list(names(theta), seq(1, length(raw_cs) + 1)))
  for (i in 1:ncol(result)) {
   result[, i] <- colSums(ss_prob[ss_prob$cs_group == i, -c(1:2), drop = FALSE])
  }
  return(result)
}



############################################################################@###
################### ca_irt_recursive ###########################################
############################################################################@###

#' Calculate classification accuracy/consistency using recursive method
#'
#' @param ip An \code{\link{Itempool-class}} object. Item pool parameters can
#'   be composed of any combination of unidimensional dichotomous or polytomous
#'   items.
#' @param theta A numeric vector of examinee ability (theta) values.
#' @param theta_cs A numeric vector representing the theta scale cut scores.
#'   Do not include \code{-Inf} or \code{Inf}.
#' @param raw_cs A sorted (ascending order) numeric vector of summed-score cut
#'   score values. Do not include 0 or the maximum possible score of the test
#'   in this vector.
#' @param perf_categories An integer  vector representing the performance
#'   categories of examinees. The number 1 should represent the lowest category.
#'   For example if there are three cut scores the valid values can only be: 0,
#'   1, 2 and 3. This  vector will be used \code{theta} is \code{NULL}. The
#'   default value is \code{NULL}. Either \code{theta} or \code{perf_categories}
#'   should be provided.
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
#' @examples
#' ip <- generate_ip(model = sample(c("GPCM", "2PL"), 10, TRUE))
#' theta <- seq(-3, 3, .2)
#' theta_cs <- c(-1, 0, 1.5)
#' raw_cs <- rsss(ip = ip, scale_score = theta_cs)
#' raw_score <- round(rsss(ip = ip, scale_score = theta))
#'
#' ca_irt_recursive(ip = ip, theta = theta, theta_cs = theta_cs)
#' ca_irt_recursive(ip = ip, theta = theta, raw_cs = raw_cs)
#' ca_irt_recursive(ip = ip, theta = theta, raw_cs = round(raw_cs))
ca_irt_recursive <- function(ip, theta, theta_cs = NULL, raw_cs = NULL,
                             perf_categories = NULL, cat_labels = NULL) {
  ### Check Arguments ###
  if (!inherits(ip, "Itempool"))
    stop("Invalid 'ip'. 'ip' should be an 'Itempool' object.")
  if (is.null(raw_cs) && is.null(theta_cs)) {
    stop("Either 'raw_cs' or 'theta_cs' should be provided.")
  }

  # If cut scores are given in theta scale, convert them to raw score metric
  if (is.null(raw_cs)) {
    raw_cs <- rsss(ip = ip, scale_score = theta_cs)
  }

  # Calculate conditional category probabilities of theta values
  cat_prob <- ca_irt_recursive_cond_category_prob(ip = ip, theta = theta,
                                                  raw_cs = raw_cs,
                                                  cat_labels = cat_labels)

  return(ca_irt_ca_cc(cat_prob = cat_prob, theta_cs = theta_cs,
                      raw_cs = raw_cs, theta = theta, ip = ip,
                      perf_categories = perf_categories,
                      cat_labels = cat_labels))
}


############################################################################@###
################### classification_agreement_index #############################
############################################################################@###

#' Calculate agreement index
#'
#' @param true_score A numeric vector of examinees' true score values.
#'   Values can be in theta scale or summed scores.
#' @param estimated_score A numeric vector of examinees' estimated score values.
#'   Values can be in theta scale or summed scores.
#' @param cut_scores A sorted (ascending order) numeric vector of  cut score
#'   values. Values can be in theta scale or summed scores. Do not include 0 or
#'   the maximum possible score of the test.
#' @param cat_labels A string vector representing the labels of the categories.
#'   The length of the vector should be one more than the length of the
#'   cut scores. The default value is \code{NULL} where the categories will be
#'   labeled as 1, 2, ..., (number of cut scores plus one). For example, if
#'   there are three cut scores category labels can be:
#'   \code{c("Unsatisfactory", "Basic", "Mastery", "Advanced")}.
#'
#' @return A list of following elements:
#'   \describe{
#'     \item{\code{ca_table}}{A classification table where rows are true
#'       categories and columns are estimated categories. The values are the
#'       number of examinees.}
#'     \item{\code{ca_table}}{A classification table where rows are true
#'       categories and columns are estimated categories. The values are the
#'       proportion of examinees.}
#'     \item{\code{ca}}{Classification agreement index}
#'   }
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @examples
#'
#' ip <- generate_ip(model = sample(c("GPCM", "2PL"), 20, TRUE))
#' n_examinee <- 1000
#' true_theta <- rnorm(n_examinee)
#' observed_theta <- true_theta + runif(n_examinee, -.5, .5)
#' theta_cs <- c(-1, 0, 1.5)
#' raw_cs <- round(rsss(ip = ip, scale_score = theta_cs))
#' true_raw_score <- round(rsss(ip = ip, scale_score = true_theta))
#' observed_raw_score <- round(rsss(ip = ip, scale_score = observed_theta))
#'
#' # Theta scores
#' classification_agreement_index(true_score = true_theta,
#'                                estimated_score = observed_theta,
#'                                cut_scores = theta_cs)
#' # Summed scores
#' classification_agreement_index(true_score = true_raw_score,
#'                                estimated_score = observed_raw_score,
#'                                cut_scores = raw_cs)
#' # Add labels
#' classification_agreement_index(true_score = true_theta,
#'                                estimated_score = observed_theta,
#'                                cut_scores = theta_cs,
#'                                cat_labels = c("Unsatisfactory", "Basic",
#'                                               "Mastery", "Advanced"))
#'
classification_agreement_index <- function(true_score, estimated_score,
                                           cut_scores, cat_labels = NULL) {
  true_cat <- ca_perf_cat(theta = true_score, theta_cs = cut_scores,
                          cat_labels = cat_labels)
  estimated_cat <- ca_perf_cat(theta = estimated_score, theta_cs = cut_scores,
                               cat_labels = cat_labels)

  output <- list()
  ca_table <- table(true_cat, estimated_cat)
  # Classification accuracy table
  output$ca_table <- ca_table
  output$ca_table_prop <- ca_table/sum(ca_table)
  output$ca <- sum(diag(ca_table))/sum(ca_table)

  # TODO: Create confusion matrix for each cut score

  return(output)
}


############################################################################@###
################### classification_indices #####################################
############################################################################@###

#' Calculate classification accuracy and consistency
#'
#' @param method The method of classification accuracy and consistency
#'  calculation method. Following methods are available:
#'
#'   \describe{
#'     \item{\code{'rudner'}}{
#'       Rudner (2000, 2005) based classification accuracy and consistency
#'       indices.
#'
#'       Following values should be provided for this method:
#'       \code{theta, se, theta_cs}.
#'       Following values can optionally be provided for this method:
#'       \code{perf_categories, cat_labels}.
#'     }
#'     \item{\code{'guo'}}{Guo (2006) based classification accuracy and
#'       consistency indices.
#'
#'       Note that calculation times can be long for this method. The value of
#'       \code{n_theta} can be decreased to speed up the function but this
#'       will reduce the accuracy of the index.
#'
#'       Following values should be provided for this method:
#'       \code{ip, resp, theta_cs} and either one of \code{theta} or
#'       \code{perf_categories}.
#'       Following values can optionally be provided for this method:
#'       \code{n_theta, theta_lower_bound, theta_upper_bound, cat_labels}.
#'     }
#'     \item{\code{'recursive'}}{Lee (2010) based classification
#'       accuracy and consistency indices.
#'
#'       Following values should be provided for this method:
#'       \code{ip, theta} and either one of these \code{theta_cs, raw_cs}.
#'       Following values can optionally be provided for this method:
#'       \code{perf_categories, cat_labels}.
#'     }
#'   }
#'
#' @param ip An \code{\link{Itempool-class}} object. Item pool parameters can be
#'   composed of any combination of unidimensional dichotomous or polytomous
#'   items. Required for \code{"guo"} and \code{"recursive"} methods.
#' @param theta A numeric vector representing the abilities of examinees.
#'   Required for \code{'rudner'} and \code{'recursive'} method. For
#'   \code{"guo"} method, this vector will be used to get performance category
#'   of each examinee if \code{perf_categories} is \code{NULL}. The default
#'   value is \code{NULL}. For \code{method = "guo"} either \code{theta} or
#'   \code{perf_categories} should be provided.
#' @param theta_cs A sorted (ascending order) numeric vector representing the
#'   theta scale cut scores. Do not include \code{-Inf} or \code{Inf}. Required
#'   for \code{'rudner'} and \code{'guo'} method; required for 'recursive' if
#'   \code{raw_cs} is not provided.
#' @param raw_cs A sorted (ascending order) numeric vector of summed-score cut
#'   score values. Do not include 0 or the maximum possible score of the test in
#'   this vector. Required for 'recursive' method if 'theta_cs' is not provided.
#' @param resp A \code{\link{Response_set-class}}, a \code{matrix} or a
#'   \code{data.frame} object that holds responses. If \code{matrix} or a
#'   \code{data.frame} provided, they will be converted to a
#'   \code{\link{Response_set-class}}. Required for \code{'guo'} method.
#' @param se A numeric vector representing the standard errors of ability
#'   estimates. Required for \code{'rudner'} method.
#' @param perf_categories An integer  vector representing the performance
#'   categories of examinees. The number 1 should represent the lowest category.
#'   For example if there are three cut scores the valid values can only be: 0,
#'   1, 2 and 3. This  vector will be used \code{theta} is \code{NULL}. The
#'   default value is \code{NULL}. Either \code{theta} or \code{perf_categories}
#'   should be provided. Can optional be provided for all methods.
#' @param n_theta An integer representing the number of equally spaced theta
#'   points between cut scores. The default value is 100. Use larger values to
#'   increase accuracy but larger numbers will also slow the speed of
#'   calculation. Can optionally be provided for the \code{'guo'} method.
#' @param theta_lower_bound A number representing the lower bound for cut
#'   scores. The default value is -6. Can optionally be provided for the
#'   \code{'guo'} method.
#' @param theta_upper_bound A number representing the upper bound for cut
#'   scores. The default value is 6. Can optionally be provided for the
#'   \code{'guo'} method.
#' @param cat_labels A string vector representing the labels of the categories.
#'   The length of the vector should be one more than the length of the cut
#'   scores. The default value is \code{NULL} where the categories will be
#'   labeled as 1, 2, ..., (number of cut scores plus one). For example, if
#'   there are three cut scores category labels can be:
#'   \code{c("Unsatisfactory", "Basic", "Mastery", "Advanced")}. Can optional be
#'   provided for all methods.
#'
#' @return A list of following elements:
#'   \describe{
#'     \item{\code{category_prob}}{A numeric vector representing the
#'       performance category classification probabilities of each examinee.}
#'     \item{\code{ca}}{Marginal (overall) classification accuracy index}
#'     \item{\code{cc}}{Marginal (overall) classification consistency index}
#'     \item{\code{ind_cs_ca}}{Individual cut score classification accuracy
#'       indices. This value will only be calculated when there are more than
#'       one cut score.}
#'     \item{\code{ind_cs_ca}}{Individual cut score classification consistency
#'       indices. This value will only be calculated when there are more than
#'       one cut score.}
#'   }
#'
#'
#' @references
#' Guo, F. (2006). Expected classification accuracy using the latent
#' distribution. Practical Assessment, Research, and Evaluation, 11(1), 6.
#'
#' Lee, W. C. (2010). Classification consistency and accuracy for complex
#' assessments using item response theory. Journal of Educational Measurement,
#' 47(1), 1-17.
#'
#' Rudner, L. M. (2000). Computing the expected proportions of misclassified
#' examinees. Practical Assessment, Research, and Evaluation, 7(1), 14.
#'
#' Rudner, L. M. (2005). Expected classification accuracy. Practical Assessment,
#' Research, and Evaluation, 10(1), 13.
#'
#' Wyse, A. E., & Hao, S. (2012). An evaluation of item response theory
#' classification accuracy and consistency indices. Applied Psychological
#' Measurement, 36(7), 602-624.
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @examples
#'
#' ip <- generate_ip(model = sample(c("GPCM", "2PL"), 20, TRUE))
#' n_examinee <- 100
#'
#' true_theta <- rnorm(n_examinee)
#' resp_set <- generate_resp_set(ip = ip, theta = true_theta, prop_missing = .2)
#' theta_est <- est_ability(resp = resp_set, ip = ip, method = "eap")
#' se <- theta_est$se
#' theta_est <- theta_est$est
#' raw_score <- est_ability(resp = resp_set, method = "sum_score")$est
#'
#' # Cut score
#' theta_cs <- c(-1, 0, 1.5)
#' raw_cs <- round(rsss(ip = ip, scale_score = theta_cs))
#'
#' # Rudner (2000, 2005) based indices:
#' classification_indices(method = "rudner", theta = theta_est, se = se,
#'                        theta_cs = theta_cs)
#'
#' # Guo (2006) based indices:
#' classification_indices(method = "guo", ip = ip, resp = resp_set,
#'                        theta = theta_est, theta_cs = theta_cs)
#'
#' # Recursive method based indices:
#' classification_indices(method = "recursive", ip = ip, theta = theta_est,
#'                        theta_cs = theta_cs)
#' # Use raw score cut scores with recursive method
#' classification_indices(method = "recursive", ip = ip, theta = theta_est,
#'                        raw_cs = raw_cs)
#'
classification_indices <- function(
  method = "recursive",
  ip = NULL, # Required for 'guo' and 'recursive' methods
  theta = NULL, # Required for 'rudner' and 'recursive' method; required for
                # 'guo' if perf_categories is not provided
  theta_cs = NULL, # Required for 'rudner' and 'guo' method; required for
                   # 'recursive' if 'raw_cs' is not provided
  raw_cs = NULL, # Should be provided for 'recursive' method if 'theta_cs' is
                 # not provided
  resp = NULL,  # Required for 'guo' method
  se = NULL, # Required for 'rudner' method
  perf_categories = NULL, # Optional for all methods
  n_theta = 100, # Optionally provided for 'guo' method.
  theta_lower_bound = -6, # Optionally provided for 'guo' method.
  theta_upper_bound = 6, # Optionally provided for 'guo' method.
  cat_labels = NULL # Optional for all methods
  ) {

  if (method == "recursive") {
    return(ca_irt_recursive(ip = ip, theta = theta, theta_cs = theta_cs,
                            raw_cs = raw_cs, perf_categories = perf_categories,
                            cat_labels = cat_labels))
  } else if (method == "rudner") {
    return(ca_irt_rudner(theta = theta, se = se, theta_cs,
                         perf_categories = perf_categories,
                         cat_labels = cat_labels))
  } else if (method == "guo") {
    return(ca_irt_guo(ip = ip, resp = resp, theta_cs, theta = theta,
                      perf_categories = perf_categories, n_theta = n_theta,
                      theta_lower_bound = theta_lower_bound,
                      theta_upper_bound = theta_upper_bound,
                      cat_labels = cat_labels))
  } else
    stop("Invalid 'method'. This method has not been implemented yet.")
}



############################################################################@###
################### kappa_coef #################################################
############################################################################@###
#' Calculate Cohen's Kappa Coefficient
#'
#' @description This function calculates weighted or unweighted Kappa
#'   coefficient for two sets of ratings. Kappa coefficient quantifies the
#'   agreement between two sets of ratings (like two raters) beyond what is
#'   expected by chance. It can be used as a measure of inter-rater reliability.
#'
#'   If the ratings are ordinal (for example Likert scale), weighted kappa
#'   coefficient can be used. Weighted Kappa penalizes the larger discrepancies
#'   between raters. More emphasis is put to large differences between rating
#'   and small emphasis will be put on smaller differences. The available
#'   weighting options are \code{"linear"} and \code{"quadratic"}. By default
#'   the function calculates \code{"unweighted"} Kappa coefficient.
#'
#' @param x A matrix/data.frame with two columns where each column contains
#'   a set of ratings. When \code{weights = "linear"} or
#'   \code{weights = "quadratic"}, each row should be an ordered factor or
#'   numbers. The rows with missing values (i.e. \code{NA}) will be removed
#'   from the analysis.
#' @param weights Either a string representing the weighting method. Or, a
#'   square matrix of weights that will be applied to the cross table
#'   (assuming the ratings are ordered factors or numeric).
#'   There are three possible weighting methods (aside from the custom
#'   weights method):
#'   \describe{
#'     \item{\code{'unweighted'}}{
#'       This is the original Kappa coefficient where no weighting applied.
#'       This is the default method. This method is appropriate for both
#'       nominal (i.e. unordered) data or ordinal (i.e. ordered) data.
#'     }
#'     \item{\code{'linear'}}{Linear weights applied.}
#'     \item{\code{'quadratic'}}{Quadratic weights applied. }
#'   }
#'
#' @return A Kappa coefficient which is a number between -1 and 1. 1 means
#'   perfect agreement between ratings. 0 means agreement between rating is no
#'   better than agreement one would get merely by chance. Negative values means
#'   the agreement is even worse than one would get by chance.
#'
#' @references
#' Cohen, Jacob (1960). "A coefficient of agreement for nominal scales".
#' Educational and Psychological Measurement. 20 (1): 37–46.
#'
#' Sim, J., & Wright, C. C. (2005). The kappa statistic in reliability studies:
#' use, interpretation, and sample size requirements. Physical therapy, 85(3),
#' 257-268.
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @examples
#'
#' #########  Example 1  #########
#' # Hypothetical data from Sim and Wright (2005), Table 1
#' # "Diagnostic Assessments of Relevance of Lateral Shift by 2 Clinicians"
#' dtf <- data.frame(c1 = c(rep("Relevant", 22), rep("Relevant", 2),
#'                          rep("Not Relevant", 4), rep("Not Relevant", 11)),
#'                   c2 = c(rep("Relevant", 22), rep("Not Relevant", 2),
#'                          rep("Relevant", 4), rep("Not Relevant", 11)))
#' kappa_coef(dtf)
#'
#' #########  Example 2  #########
#' # Hypothetical data from Sim and Wright (2005), p.260, Table 2
#' pain_raw <- data.frame(t1 = c(rep("No Pain", 15 + 3 + 1 + 1),
#'                               rep("Mild Pain", 4 + 18 + 3 + 2),
#'                               rep("Moderate Pain", 4 + 5 + 16 + 4),
#'                               rep("Severe Pain", 1 + 2 + 4 + 17)),
#'                        t2 = c(rep("No Pain", 15), rep("Mild Pain", 3),
#'                               rep("Moderate Pain", 1), rep("Severe Pain", 1),
#'                               rep("No Pain", 4), rep("Mild Pain", 18),
#'                               rep("Moderate Pain", 3), rep("Severe Pain", 2),
#'                               rep("No Pain", 4), rep("Mild Pain", 5),
#'                               rep("Moderate Pain", 16), rep("Severe Pain", 4),
#'                               rep("No Pain", 1), rep("Mild Pain", 2),
#'                               rep("Moderate Pain", 4), rep("Severe Pain", 17))
#'   )
#' # Since data is ordinal, convert columns to ordinal factors:
#' ordered_levels <- c("No Pain", "Mild Pain", "Moderate Pain", "Severe Pain")
#' pain_ordered <- data.frame(
#'   t1 = factor(pain_raw$t1, levels = ordered_levels, ordered = TRUE),
#'   t2 = factor(pain_raw$t2, levels = ordered_levels, ordered = TRUE))
#' table(pain_ordered)
#'
#' # Unweighted Kappa Coefficient
#' kappa_coef(pain_ordered)
#' # Kappa Coefficient with linear weights
#' kappa_coef(pain_ordered, weights = "linear")
#' # Kappa Coefficient with quadratic weights
#' kappa_coef(pain_ordered, weights = "quadratic")
#'

kappa_coef <- function(x, weights = "unweighted") {

  if (!inherits(x, c("data.frame", "matrix")) || ncol(x) != 2) {
    stop("Invalid 'x'. 'x' should be a 'matrix' or 'data.frame' with 2 ",
         "(two) columns.")
  }
  # remove rows with missing values
  x <- x[stats::complete.cases(x),]
  if (nrow(x) == 0) stop("Data has too many missingness. ",
                         "Kappa cannot be calculated.")

  if (inherits(x, "data.frame")) {
    r1 <- x[[1]]
    r2 <- x[[2]]
  } else { # matrix
    r1 <- x[, 1]
    r2 <- x[, 2]
  }
  levels <- unique(c(r1, r2))

  # Levels will be use to reconstruct an ordered factor later on
  if (is.numeric(levels) || is.ordered(levels)) {
    levels <- sort(levels)
  }
  ncat <- length(levels) # number of categories

  if (ncat == 1 & all(r1 == r2)) {
    return(1)
  }

  w <- NULL

  if (is.null(weights)) weights <- "unweighted"
  if (is.matrix(weights)) {
    if (dim(weights) == 2 && ncol(weights) == ncat && nrow(weights) == ncat &&
        is.numeric(weights)) {
      w <- weights
      weights <- "custom"
    } else stop(paste0(
      "Invalid 'weights'. If custom weights are provided, the dimension of ",
      "weight matrix should be ", ncat, "x", ncat, "."))
  }

  # Check whether 'weights' argument is valid
  if (!(is.character(weights) &&
        length(weights) == 1 &&
        tolower(weights) %in% c("custom", "unweighted", "linear", "quadratic"))) {
    weights <- "unweighted"
  } else {
    weights <- tolower(weights)
  }

  # If weights are one of the "custom", "linear", "quadratic", each column
  # should be ordered factor or numeric, otherwise raise a warning.
  if (weights %in% c("custom", "linear", "quadratic") &&
      (!(is.numeric(r1) || is.ordered(r1)) ||
       !(is.numeric(r2) || is.ordered(r2)))
      ) {
    warning("When 'weights' argument is 'linear', 'quadratic', or custom, ",
            "ratings should be an ordered factor or numeric.")
  }


  # converted to data frame because if x is a matrix, converting it to a factor
  # will
  xord <- as.data.frame(x) # Ordinal version of x will be saved in xord

  # Converting to ordinal because if there are missing levels in one of the
  # factors, 'ctab()' function will still populate empty levels.
  for (i in 1:ncol(xord)) {
    xord[, i] <- factor(xord[, i], levels = levels, ordered = TRUE)
  }

  # Setup the weight matrix
  if (weights == "unweighted") {
    # Set the weights for unweighted Kappa
    w <- diag(ncat)
  } else if (weights %in% c("linear", "quadratic")) {
    # Make sure that each column of 'x' is ordered factor or has numeric values
    # for weighted Kappa calculation
    w <- matrix(0, ncol = ncat, nrow = ncat)
    for (i in 1:ncat) {
      for (j in 1:ncat) {
        if (weights == "linear") {
          w[i, j] <- 1 - abs(i - j)/(ncat - 1)
        } else if (weights == "quadratic") {
          w[i, j] <- 1 - ((i - j)/(ncat - 1))^2
        }
      }
    }
  }

  ctab <- stats::xtabs(data = xord, drop.unused.levels = FALSE)
  ctest <- suppressWarnings(stats::chisq.test(ctab, correct = FALSE))
  # ctest <- suppressWarnings(chisq.test(x = xord[, 1], y = xord[, 2],
  #                                      correct = FALSE))

  result <- sum(w * ctest$observed - w * ctest$expected) /
    (sum(ctest$observed) - sum(w * ctest$expected))

  result
}


