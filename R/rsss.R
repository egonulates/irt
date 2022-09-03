
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% rsss %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' Convert raw score to scale score and vice versa
#'
#' @param ip An \code{\link{Itempool-class}} object.
#' @param raw_score A value (or vector of values) representing raw score(s).
#' @param scale_score A value (or vector of values) representing scale score(s).
#' @param theta_range The limits of the scale score. The default is
#'   \code{c(-5, 5)}.
#'
#' @return A vector of raw or scale scores.
#'
#'
#' @importFrom stats uniroot
#' @export
#'
#' @author Emre Gonulates
#'
rsss <- function(ip, raw_score = NULL, scale_score = NULL,
                 theta_range = c(-5, 5)) {
  result <- NULL
  if (!is.null(raw_score)) {
    max_possible_score <- sum(ip$item_max_score)
    if (any(raw_score > max_possible_score) || any(raw_score < 0))
      stop(paste0("Raw score values cannot larger than the maximum possible ",
                  "raw score of ", max_possible_score, " or smaller than 0."))
    raw_score_to_scale_score <- Vectorize(function(y) {
      uniroot(f = function(x) {sum(mean(ip, theta = x)) - y},
              lower = sort(theta_range)[1], upper = sort(theta_range)[2])$root})
    result <- tryCatch(
      raw_score_to_scale_score(raw_score),
      error = function(e) {
        if (grepl(pattern = "values at end points not of opposite sign",
                  e$message))
          stop(paste0(
            "\nScale score cannot be calculated for some raw scores. Please ",
            "provide a wider 'theta_range' than c(", theta_range[1], " ,",
            theta_range[2], "). \nOr, remove raw score(s): ",
            paste0(c(switch(0 %in% raw_score, 0, NULL),
                     switch(max_possible_score %in% raw_score,
                            max_possible_score, NULL)), collapse = ", "), "."),
               call. = FALSE)
      }
    )
  } else if (!is.null(scale_score) && all(sapply(scale_score, is.numeric))) {
    scale_score_to_raw_score <- Vectorize(function(x) {
      sum(mean(ip, theta = x))
    })
    result <- scale_score_to_raw_score(x = scale_score)
  }
  return(result)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% prob_sum_score %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' Calculate summed-score probabilities
#'
#' @description This function calculates all summed-score probabilities
#'   of a given theta value(s) using recursive algorithm described in
#'   Thissen, Pommerich, Billeaud and Williams (1995). This function is the
#'   extension of the recursive algorithm proposed by Lord and Wingersky (1984)
#'   to polytomous items.
#'
#' @param ip An \code{\link{Itempool-class}} object. Item pool parameters can
#'   be composed of any combination of unidimensional dichotomous or polytomous
#'   items.
#' @param theta A numeric vector representing the theta values at which the sum
#'   score probabilities will be calculated.
#' @param theta_pdf A numeric vector with the same length of \code{theta}
#'   argument representing the density values of each theta value. The
#'   resulting probabilities will be weighted by these values. The default
#'   value is \code{NULL} where the resulting probabilities will not be
#'   weighted.
#'
#' @return A matrix containing the probabilities of each possible sum score.
#'   Each row represent a sum score and each column represent the theta value
#'   provided by \code{theta} argument.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @references
#' Kolen, M. J., & Brennan, R. L. (2014). Test equating, scaling, and linking:
#' Methods and practices. Springer Science & Business Media.
#'
#' Lord, F. M., & Wingersky, M. S. (1984). Comparison of IRT true-score and
#' equipercentile observed-score" equatings". Applied Psychological Measurement,
#' 8(4), 453-461.
#'
#' Thissen, D., Pommerich, M., Billeaud, K., & Williams, V. S. (1995). Item
#' response theory for scores on tests including polytomous items with ordered
#' responses. Applied Psychological Measurement, 19(1), 39-49.
#'
#' @examples
#' ### Example with weighting ###
#' ip <- generate_ip(model = sample(c("GPCM", "2PL"), 10, TRUE))
#' theta <- c(-3, -1.2, 0.5, 3)
#' prob_sum_score(ip, theta = theta)
#' # Most probable sum scores:
#' apply(prob_sum_score(ip, theta = theta), MARGIN = 2, which.max) - 1
#' \dontrun{
#'    plot(ip, type = "tcc", suppress_plot = TRUE) +
#'      ggplot2::geom_vline(xintercept = theta, lty = "dashed")
#' }
#' ### Example from Kolen and Brennan (2014) ###
#' # Item parameters from Kolen and Brennan (2014), p.175, Table 6.1.
#' ip <- itempool(a = c(1.30, .6, 1.7),
#'                b = c(-1.30, -.10, .9),
#'                c = c(.1, .17, .18),
#'                D = 1.7)
#' prob(ip, theta = c(-2, 1))
#' # IRT observed score distribution using recursive formula from
#' # Kolen and Brennan (2014), p.200, Table 6.4.
#' # Numbers are not exactly the same as Kolen and Brennan since due to
#' # rounding applied to the numbers in the book.
#' prob_sum_score(ip, theta = -2)
#'
#'
#' ### Example from Thissen, Pommerich, Billeaud and Williams (1995) ###
#' # Replicating Thissen et al. (1995) example, p.43-44, Table 1.
#' i1 <- item(a = .5, b = -1)
#' i2 <- item(a = 1, b = 0)
#' i3 <- item(a = 1.5, b = 1)
#' ip <- c(i1, i2, i3) # combine items to form an item pool
#' theta <- -3:3 # Quadrature points
#'
#' prob_sum_score(ip, theta)
#'
#' # Item parameters in Table 2
#' i1 <- item(a = 1.87, b = c(.65, 1.97, 3.14), model = "GRM")
#' i2 <- item(a = 2.66, b = c(.12, 1.57, 2.69), model = "GRM")
#' i3 <- item(a = 1.24, b = c(.08, 2.03, 4.30), model = "GRM")
#' ip <- c(i1, i2, i3)
#' delta <- 0.01
#' theta <- seq(-3, 3, delta)
#'
#' x <- prob_sum_score(ip = ip, theta = theta, theta_pdf = dnorm(theta))
#'
#' # Figure 1
#' plot(x = theta, y = x[2, ], type = "l", ylab = "Posterior Density",
#'      xlab = "Theta",
#'      main = paste0("Posterior Distribution for all Examinees Obtaining ",
#'                    "a Summed Score of 1"))
#'
#' # Table 3, column "Modeled Score Group Proportion"
#' rowSums(x)/sum(rowSums(x))
#'
prob_sum_score <- function(ip, theta, theta_pdf = NULL) {
  zero_matrix <- matrix(0, nrow = max_score(ip) + 1, ncol = length(theta),
                        dimnames = list(0:max_score(ip),
                                        paste0(round(theta, 5))))
  # In l_post, the cell (1, 1) represents, the probability of total score = 0
  # for theta = -3.
  l_post <- zero_matrix
  # Possible scores of the first item
  scr_i <- 0:max_score(ip[[1]])
  # Probability of each score
  p <- sapply(theta,
              function(x) sapply(scr_i, resp_lik, ip = ip[[1]], theta = x))
  l_post[seq_along(scr_i), ] <- p
  if (ip$n$items > 1) {
    for (i in 2:ip$n$items) {
      scr_i <- 0:max_score(ip[[i]])
      p <- sapply(theta, function(x) sapply(scr_i, resp_lik, ip = ip[[i]],
                                            theta = x))
      scr_pre <- 0:max_score(ip[1:(i - 1)])
      l_pre <- l_post[seq_along(scr_pre), , drop = FALSE]
      l_post <- zero_matrix

      for (j in 1:(max_score(ip[[i]]) + 1)) {
        temp <- j:(j + length(scr_pre) - 1)
        l_post[temp, ] <- l_post[temp, ] +
          l_pre * matrix(p[j, ], nrow = nrow(l_pre), ncol = ncol(p),
                         byrow = TRUE)
      }
    }
  }
  # The rows of L_post represent each sum score and column represent each
  # theta quadrature point. The rows range from 0 to maximum possible score
  # of the test. For example, fifth row has the probabilities of sum score 4,
  # at each theta quadrature point.

  if (is.null(theta_pdf)) {
    return(l_post)
  } else {
    if (length(theta_pdf) !=  length(theta) || any(theta_pdf > 1) ||
        any(theta_pdf < 0))
      stop("Invalid 'theta_pdf'. Please provide a valid 'theta_pdf'. The ",
           "length of 'theta_pdf' should be equal to the length of ",
           "'theta'. All of the values should be between 0 and 1. ")
    return(matrix(theta_pdf, nrow = nrow(l_post), ncol = length(theta_pdf),
                  byrow = TRUE) * l_post)
  }
}
