

#' Item Analysis Function
#'
#' @param resp A \code{\link{Response_set-class}} object, \code{matrix} or
#'   \code{data.frame} containing the item responses.
#' @param criterion Provide a continuous criterion variable such as a total
#'   raw score, or theta score that will be used in the calculation of
#'   correlation calculations. If this value is \code{NULL}, the total score
#'   will be used.
#' @param ip An \code{\link{Itempool-class}} object. This will help function
#'   in two ways. First, if the \code{resp} is
#'   a \code{\link{Response_set-class}} object, the function will help the
#'   responses to be arranged in the same order as \code{ip}. Second, if there
#'   are polytomous items in the data, \code{ip} will help finding the maximum
#'   values of each item. Otherwise, the maximum values each item can take
#'   will be calculated using data, which may be fallible.
#' @param stats A vector of string containing the columns/statistics to be
#'   calculated. \code{'item_id'} column will be added by default. Some or all
#'   of the following columns can be added to the output:
#'   \code{c("n", "pval", "pbis", "bis", "pbis_adj", "bis_adj")}.
#'   Please see the 'value' section below to see the details of these columns.
#'   By default, all of the columns above will be calculated.
#' @param suppress_output If \code{TRUE}, the function will suppress
#'   console output. Default value is \code{FALSE}
#'
#' @return A data.frame with following columns:
#'   \describe{
#'     \item{'item_id'}{Item ID.}
#'     \item{'n'}{Number of examinees responded this item.}
#'     \item{'pval'}{p-value, proportion of examinees correctly answered items.
#'       If there are polytomous items in the data, p-value will be calculated
#'       by dividing the mean of the scores for the item by the maximum
#'       possible score of the item.}
#'     \item{'pval_unadj'}{Unadjusted p-value, this is the mean of item scores
#'       that is not adjusted for the maximum possible score as \code{'pval'}
#'       column does. For dichotomous items, this will be the same as
#'       \code{'pval'} column.}
#'     \item{'pbis'}{Point biserial correlation.}
#'     \item{'bis'}{Biserial correlation.}
#'     \item{'pbis_adj'}{Point biserial correlation between item and total score
#'       without this item. Note that this stat is only available when
#'       criterion is \code{NULL}.}
#'     \item{'bis_adj'}{Biserial correlation between item and total score
#'       without this item. Note that this stat is only available when
#'       criterion is \code{NULL}.}
#'   }
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' theta <- rnorm(100)
#' ip <- generate_ip(n = 20)
#' resp <- sim_resp(ip = ip, theta = theta, prop_missing = .2)
#' # Item analysis based on total scores
#' item_analysis(resp)
#' # Item analysis based on theta scores
#' item_analysis(resp, criterion = theta)
#'
item_analysis <- function(resp, criterion = NULL, ip = NULL,
                          stats = c("n", "pval", "pbis", "bis",
                                     "pbis_adj", "bis_adj"),
                          suppress_output = FALSE) {
  output_possible_values <- c("n", "pval", "pbis", "bis", "pbis_adj", "bis_adj",
                              "pval_unadj")
  if (!all(stats %in% output_possible_values)) {
    stop("Invalid 'stats' value. Please only use one of the following ",
         "values: ",
         paste0("\"", output_possible_values, "\"", collapse = ", "))
  }

  if (is(resp, "Response_set"))
    resp <- as.matrix(resp, ip = ip, output = "score")
  # Check compatibility of item pool and
  if (!is.null(ip)) {
    if (!is(ip, "Itempool")) {
      stop("Invalid 'ip'. 'ip' should be an 'Itempool' object.")
    }
    if (ncol(resp) != ip$n$items) {
      stop("Invalid 'ip'. 'ip' should have the same number of elements as ",
           "the 'resp'.")
    }
    if (!is.null(colnames(resp)) && !all(ip$resp_id == colnames(resp))) {
      stop("Invalid 'ip'. The order of responses in 'resp' and item order in ",
           "are not matching.")
    }
  }
  if (is.null(ip)) {
    # Note that the following method is fallible and assumes at least one
    # data point is the maximum possible score.
    max_scores <- apply(resp, 2, max, na.rm = TRUE)
    # If the scores of an item is all 0s, then the maximum score will be 1
    # by default.
    max_scores <- sapply(max_scores, max, 1)
  } else {
    max_scores <- max_score(ip, sum = FALSE)
  }
  # Calculate the number of non-missing responses
  output <- data.frame(row.names = paste0(1:ncol(resp)))
  # Add item ids
  if (is.null(colnames(resp))) output$item_id <- rep(NA, ncol(resp)) else
    output$item_id <- colnames(resp)
  # Add number of
  if ("n" %in% stats)
    output$n <- colSums(!is.na(resp))
  # Calculate unadjusted p-value
  pval_unadj <- colMeans(resp, na.rm = TRUE)
  if ("pval_unadj" %in% stats)  output$pval_unadj <- pval_unadj

  # Calculate the p-value
  if ("pval" %in% stats) output$pval <- pval_unadj / max_scores

  if (is.null(criterion)) theta <- rowSums(resp, na.rm = TRUE) else
    theta <- criterion
  # Calculate point-biserial correlation
  if ("pbis" %in% stats)
    output$pbis <- apply(resp, 2, biserial, theta, "point-biserial")
  # Calculate biserial correlation
  if ("bis" %in% stats)
    output$bis <- apply(resp, 2, biserial, theta, "default")

  if (is.null(criterion)) {
    # Calculate corrected point-biserial correlation, i.e. remove the item from
    # the calculation of the total score.
    if ("pbis_adj" %in% stats)
      output$pbis_adj <- sapply(1:ncol(resp), function(i)
        biserial(score = resp[, i],
                 criterion = rowSums(resp[, -i], na.rm = TRUE),
                 method = "point-biserial"))

    if ("bis_adj" %in% stats)
      output$bis_adj <- sapply(1:ncol(resp), function(i)
        biserial(score = resp[, i],
                 criterion = rowSums(resp[, -i], na.rm = TRUE),
                 method = "default"))
  }
  if (requireNamespace("tibble")) {
    return(tibble::as_tibble(output))
  } else return(output)
}


#' Calculate point-biserial correlation
#'
#' @param score Item scores of each examinee for which point-biserial
#'   correlation will be calculated
#' @param criterion Total score of each examinee
#'
#' @return Point-biserial correlation value
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
point_biserial <- function(score, criterion) {
  # Only use the complete observations
  complete_obs <- !(is.na(score) | is.na(criterion))
  score <- score[complete_obs]
  criterion <- criterion[complete_obs]
  # Mean of total scores of  examinees who correctly answered the item
  mu_correct <- mean(criterion[score == 1], na.rm = TRUE)
  mu_all <- mean(criterion, na.rm = TRUE)
  # Item difficulty
  p <- mean(score, na.rm = TRUE)
  # Use n instead of (n-1) for the calculation of standard deviation
  # sigma <- stats::sd(criterion, na.rm = TRUE)
  sigma <- sqrt(sum((criterion - mean(criterion))^2)/length(criterion))
  return((mu_correct - mu_all) * sqrt(p/(1 - p)) / sigma)
}

#' Calculate biserial correlation
#'
#' @param score Item scores of each examinee for which biserial correlation will
#'   be calculated
#' @param criterion Total score of each examinee
#' @param method Type of the biserial correlation calculation method.
#'   \describe{
#'     \item{\strong{"default"}}{The most common way to calculate biserial
#'     correlation. }
#'     \item{\strong{"point-biserial"}}{Calculate point-biserial correlation. }
#'     \item{\strong{"clemans-lord"}}{Modified biserial correlation value based
#'       on Clemans (1958) and Lord (1962).}
#'     \item{\strong{"brogden"}}{Modified biserial correlation value based on:
#'       Brogden (1949)}
#'     \item{\strong{"rank"}}{Rank biserial correlation value based on Cureton
#'       (1968).}
#'     }
#'
#' @return Biserial correlation value
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @references
#' Brogden, H. E. (1949). A new coefficient: Application to biserial
#'   correlation and to estimation of selective efficiency. Psychometrika, 14,
#'   169-182.
#'
#' Clemans, W. V. (1958) An index of item-criterion relationship. Educational
#'   and Psychological Measurement, 18, 167-172.
#'
#' Cureton, E. E. (1968). Rank biserial correlation when ties are present.
#'   Educational and Psychological Measurement, 28, 77-79.
#'
#' Kraemer, H. C. (1981). Modified biserial correlation coefficients.
#'   Psychometrika, 46(3), 275-282.
#'
#' Lord, F. M. (1963). Biserial estimates of correlation. Psychometrika, 28,
#'   81–85.
#'
#' @examples
#' # The example is from Salkind, Rasmussen (2007) Encyclopedia of measurement
#' # and statistics, pages 94-97
#' score <- c(rep(0, 16), rep(1, 22))
#' total_score <- c(87, 90, 94, 94, 97, 103, 103, 104, 106, 108, 109, 109, 109,
#'                  112, 119, 132, 100, 103, 103, 106, 112, 113, 114, 114, 118,
#'                  119, 120, 120, 124, 133, 135, 135, 136, 141, 155, 157, 159,
#'                  162)
#' # Calculate biserial correlation
#' biserial(score, total_score)
#' # Calculate point-biserial correlation
#' biserial(score, total_score, method = "point-biserial")
#' # Calculate modified biserial correlation (based on Brogden (1949))
#' biserial(score, total_score, method = "brogden")
#' # Calculate modified biserial correlation (Clemans-Lord)
#' biserial(score, total_score, method = "clemans-lord")
#'
biserial <- function(score, criterion, method = "default") {
  if (method == "point-biserial")
    return(stats::cor(score, criterion, use = "pairwise.complete.obs"))

  # Only use the complete observations
  complete_obs <- !(is.na(score) | is.na(criterion))
  score <- score[complete_obs]
  criterion <- criterion[complete_obs]

  # n <- sum(!is.na(score))
  n <- as.double(length(score))

  if (method == "rank") {
    # Kraemer pointed out that Cureton's (1958, 1964) rank biserial correlation
    # coefficient "essentially replaces observations with their ranks and then
    # applies Brogden's approach. " (p.280)
    criterion <- rank(criterion)
    method <- "brogden"
  }
  if (method == "clemans-lord") {
    dev <- criterion - mean(criterion)
    num <- sum(score * dev)
    if (num < 0)
      return(-(num / sum((1 - sort(score, decreasing = TRUE)) * dev)))
    # For positive values Lord's modification is the same as Brogden.
    return(sum(score * dev) /  sum(sort(score, decreasing = TRUE) * dev))
  }
  if (method == "brogden") {
    dev <- criterion - mean(criterion)
    return(sum(score * dev) /  sum(sort(score, decreasing = TRUE) * dev))
  }

  # Mean of total scores of  examinees who correctly answered the item
  mu1 <- mean(criterion[score == 1], na.rm = TRUE)
  mu0 <- mean(criterion[score == 0], na.rm = TRUE)
  n1 <- as.double(sum(score == 1, na.rm = TRUE))
  n0 <- as.double(sum(score == 0, na.rm = TRUE))
  # # The following is also valid calculation but the above one is shorter.
  # if (method == "brogden") {
  #   ranked <- sort(criterion, decreasing = TRUE)
  #   D <-  mean(ranked[1:n1]) - mean(ranked[(n1+1):(n0+n1)])
  #   return((mu1 - mu0) / D)
  # }
  sigma <- stats::sd(criterion, na.rm = TRUE)
  u <- stats::dnorm(stats::qnorm(n1/n))
  return(((mu1 - mu0) / sigma) * (n1 * n0 / (u * n^2)))
}



#' Distractor Analysis Function
#'
#' @param resp It can be either a \code{\link{Response_set-class}} object with
#'   valid raw responses; or, a \code{matrix} or \code{data.frame} containing
#'   the raw item responses.
#' @param key The answer key for the responses. Keys can also be provided via
#'   \code{ip} argument.
#' @param ip An \code{\link{Itempool-class}} object that contains the keys of
#'   the items. The program will look check whether a \code{ip$misc$key} is
#'   specified for all items. Valid keys should be provided via \code{ip} if
#'   \code{key} argument is \code{NULL}.
#' @param criterion Provide a continuous criterion variable such as a total
#'   raw score, or theta score that will be used in the calculation of
#'   correlation calculations. If this value is \code{NULL}, the total score
#'   will be used.
#'
#' @return A data.frame with following columns
#'   \describe{
#'     \item{'item_id'}{Item identifier}
#'     \item{'key'}{Answer key}
#'     \item{'option'}{The selected option}
#'     \item{'n'}{Number of subjects/examinees answered this item}
#'     \item{'prop'}{Observed proportions of the choice.}
#'     \item{'bis'}{Biserial correlation between the examinees selected
#'           the choice and the total scores.}
#'     \item{'pbis'}{Point-biserial correlation between the
#'           examinees selected the choice and the total scores.}
#'     \item{'bis_adj'}{Biserial correlation between item and total score
#'       without this item. Sum scores will be used in the calculation of
#'       'bis_adj' even 'criterion' is provided. }
#'     \item{'pbis_adj'}{Point-biserial correlation between item and total score
#'       without this item. Sum scores will be used in the calculation of
#'       'bis_adj' even 'criterion' is provided.}
#'   }
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' n_item <- 10 # sample(8:12, 1)
#' n_theta <- 50 # sample(100:200, 1)
#' raw_resp <- matrix(sample(LETTERS[1:4], n_item * n_theta, replace = TRUE),
#'                    nrow = n_theta, ncol = n_item,
#'                    dimnames = list(paste0("Examinee-", 1:n_theta),
#'                                    paste0("Item-", 1:n_item)))
#' # Add some missing responses
#' raw_resp[sample(1:length(raw_resp), round(length(raw_resp)*.1))] <- NA
#' # Prepare answer key
#' key <- sample(LETTERS[1:4], n_item, replace = TRUE)
#'
#' # Run distractor analysis:
#' da <- distractor_analysis(resp = raw_resp, key = key)
#'
#'
distractor_analysis <- function(resp, key = NULL, ip = NULL, criterion = NULL) {
  if (inherits(resp, "matrix")) {
    raw_resp <- as.data.frame(resp)
  } else if (inherits(resp, "date.frame")) {
    raw_resp <- resp
  } else if (is(resp, "Response_set")) {
    raw_resp <- data.frame(as.matrix(resp, ip = ip, output = "raw_response"),
                           check.names = FALSE)
    if (all(is.na(raw_resp)))
      stop("'resp' object do not have any raw responses.")
  }

  # Determine the item IDs
  if (!is.null(colnames(raw_resp))) {
    item_ids <- colnames(raw_resp)
  } else if (!is.null(ip)) {
    # Here it is assumed that the item order of ip is the same as the column
    # order or raw_resp or resp
    item_ids <- ip$resp_id
    if (ncol(raw_resp) != length(item_ids)) {
      stop("Invalid 'ip' or 'resp'. The number of items 'ip' and the number ",
           "of columns in 'resp' are not equal.")
    }
    colnames(raw_resp) <- item_ids
  } else  {
    item_ids <- paste0("Item_", sapply(
      1:ncol(raw_resp), sprintf,
      fmt = paste0("%0", floor(log10(ncol(raw_resp))) + 1, "d")))
    colnames(raw_resp) <- item_ids
  }


  if (is.null(key)) {
    if (is.null(ip) || is.null(ip$key) || length(ip$key) != ip$n$items)
      stop("Please provide a valid key either via 'key' argument or via ",
           "'ip' argument.")
    key <- ip$key
  }
  #   stop("Invalid 'raw_resp'. Please provide a matrix or a data.frame")

  # possible_options <- unique(as.vector(unlist(raw_resp))) # Very very slow
  # possible_options <- unique(as.vector(apply(raw_resp, 2, unique))) # Slow
  possible_options <- unique(rapply(raw_resp, unique, how = "unlist"))
  possible_options <- sort(possible_options[!is.na(possible_options)])

  ### Calculate the observed proportion of each choice option
  choice_props <- t(sapply(lapply(raw_resp, factor, levels = possible_options),
                           function(x) prop.table(table(x))))
  choice_props <- cbind.data.frame(item_id = item_ids,
                                   n = as.integer(colSums(!is.na(raw_resp))),
                                   key = key, choice_props)

  # ### Calculate the observed proportion of each choice option
  # choice_props <- t(apply(raw_resp, 2, function(x) prop.table(table(x))))
  # # Add total number or examinees answered that item:
  # choice_props <- cbind.data.frame(
  #   n = apply(raw_resp, 2, function(x) sum(!is.na(x))), choice_props)

  ### Calculate the biserial correlation of each choice option.
  # Calculate the score matrix
  if (is(resp, "Response_set") && !is.null(resp$score)) {
    scores <- as.matrix(resp, output = "score", ip = ip)
  } else scores <- score_raw_resp(raw_resp, key)
  choice_bis <- data.frame(item_id = item_ids, key = key,
                           # row.names = colnames(raw_resp),
                           check.names = FALSE)
  choice_pbis <- data.frame(item_id = item_ids, key = key,
                            # row.names = colnames(raw_resp),
                           check.names = FALSE)
  choice_bis_adj <- data.frame(item_id = item_ids, key = key,
                               # row.names = colnames(raw_resp),
                               check.names = FALSE)
  choice_pbis_adj <- data.frame(item_id = item_ids, key = key,
                                # row.names = colnames(raw_resp),
                                check.names = FALSE)

  # If not adjusted use the same total_score for all items
  sum_score <- rowSums(scores, na.rm = TRUE)
  if (is.null(criterion)) total_score <- sum_score else total_score <- criterion

  for (po in possible_options) {
    choice_bis[[po]] <- apply((raw_resp == po) * 1, 2, biserial, total_score)
    choice_pbis[[po]] <- apply((raw_resp == po) * 1, 2, point_biserial,
                               total_score)
  }

  total_score <- sum_score
  total_score <- matrix(total_score, ncol = ncol(scores), nrow = nrow(scores))
  total_score <- ifelse(is.na(raw_resp), total_score, total_score - scores)
  for (po in possible_options) {
    choice_bis_adj[[po]] <- sapply(1:ncol(raw_resp), function(i) biserial(
      score = (raw_resp[, i] == po) * 1, criterion = total_score[, i]))
    choice_pbis_adj[[po]] <- sapply(1:ncol(raw_resp), function(i)
      point_biserial(score = (raw_resp[, i] == po) * 1,
                     criterion = total_score[, i]))
  }

  output <- stats::reshape(
    choice_props,
    direction = "long",
    varying = list(setdiff(colnames(choice_props), c("item_id", "n", "key"))),
    v.names = "prop",
    idvar = c("item_id", "n", "key"),
    timevar = "option",
    times = setdiff(colnames(choice_props), c("item_id", "n", "key"))
  )
  for (df in c("bis", "pbis", "bis_adj", "pbis_adj")) {
    temp_df <- get(paste0("choice_", df))
    temp <- stats::reshape(
      temp_df,
      direction = "long",
      varying = list(setdiff(colnames(temp_df), c("item_id", "key"))),
      v.names = df,
      idvar = c("item_id", "key"),
      timevar = "option",
      times = setdiff(colnames(temp_df), c("item_id", "key"))
    )
    output <- merge(x = output, y = temp, by = c("item_id", "key", "option"),
                    sort = FALSE)
  }

  # Sort columns first based on item_id, then for options
  output <- output[order(match(output$item_id, item_ids),
                         match(output$option, possible_options)), ]
  if (requireNamespace("tibble")) {
    return(tibble::as_tibble(output))
  } else {
    rownames(output) <- NULL
    return(output)
  }

  # output <- list(prop = choice_props, biserial = choice_bis,
  #                point_biserial = choice_pbis)
  # if (!suppress_output) print(output)
  # output$scores <- scores
  #
  # return(invisible(output))
}



#' Score Raw Responses
#'
#' @param raw_resp A \code{matrix} or \code{data.frame} containing the item
#'   responses.
#' @param key The answer key for the responses
#'
#' @return Scored response matrix
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
score_raw_resp <- function(raw_resp, key) {
  output <- t(apply(raw_resp, 1, function(x) as.integer(x == key)))
  colnames(output) <- colnames(raw_resp)
  return(output)
}





###############################################################################@
############################# marginal_reliability #############################
###############################################################################@
#' Calculate Marginal Reliability Empirically
#'
#' @param ts_var A single numeric value representing the true score variance
#'   of theta.
#' @param se A vector of standard error of ability estimates or conditional
#'   standard error of measurement values corresponding to each \code{theta}
#'   value.
#' @param theta A vector of theta values.
#' @param ip An \code{\link{Itempool-class}} object. If \code{se = NULL} but
#'   a valid \code{theta} vector is provided, the \code{se} values will be
#'   estimated using \code{ip} and \code{theta} via \code{se = 1/sqrt(info)}.
#'
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
marginal_reliability_empirical <- function(ts_var = NULL, se = NULL,
                                           theta = NULL, ip = NULL)
{
  if (!is.null(ts_var) && !is.null(se)) {
    # See Kim (2012), Eqn. 17
    # Green, Bock et al. Eqn. 7
    return((ts_var - mean(se, na.rm = TRUE)^2) / ts_var)
  } else if (!is.null(theta) && !is.null(se)) {
    return(marginal_reliability_empirical(ts_var = var(theta, na.rm = TRUE),
                                          se =  se))
  } else if (!is.null(ip) && !is.null(theta)) {
    se <- 1/sqrt(info(ip = ip, theta = theta, tif = TRUE)[, 1])
    return(marginal_reliability_empirical(ts_var = var(theta, na.rm = TRUE),
                                          se = se))
  } else
    stop("Invalid inputs. Please provide either one of the following ",
         "argument sets: (se and ts_var) or (se and theta) or (theta and ip).")
}


#' Calculate Marginal Reliability
#'
#' @description This function calculates marginal reliability of a given test.
#'
#' @param ts_var A single numeric value representing the true score variance
#'   of theta.
#' @param se A vector of standard error of ability estimates or conditional
#'   standard error of measurement values corresponding to each \code{theta}
#'   value.
#' @param theta A vector of theta values.
#' @param ip An \code{\link{Itempool-class}} object. If \code{se = NULL} but
#'   a valid \code{theta} vector is provided, the \code{se} values will be
#'   estimated using \code{ip} and \code{theta} via \code{se = 1/sqrt(info)}.
#' @param method The method for calculating empirical reliability. Currently,
#'   only \code{theoretical} is available.
#'
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
#' @examples
#' ip <- generate_ip(n = 30)
#' true_theta <- rnorm(1000)
#' resp_set <- generate_resp_set(ip = ip, theta = true_theta, prop_missing = .2)
#' est <- est_ability(resp = resp_set, ip = ip, method = "ml")
#' theta <- est$est
#' se <- est$se
#'
#' # Use true Score variance (ts_var) and standard errors (se)
#' marginal_reliability(ts_var = sd(true_theta)^2, se = se)
#'
#' # Use theta estimates and standard error:
#' marginal_reliability(theta = theta, se = se)
#'


marginal_reliability <- function(ip = NULL, theta = NULL, se = NULL,
                                 ts_var = NULL, method = "empirical") {
  if (method == "empirical") {
    return(marginal_reliability_empirical(ts_var = ts_var, se = se,
                                          theta = theta, ip = ip))
  } else stop("Invalid 'method'. This method has not been implemented yet.")
}
