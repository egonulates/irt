

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% est_ability %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Estimate Examinee Ability
#'
#' @description
#' This function estimates examinee ability using different methods, including
#' Owen's Bayesian estimation, Maximum Likelihood estimation,
#' Maximum-a-Posteriori and Expected-a-Posteriori.
#'
#' @param resp A \code{\link{Response_set-class}}, \code{matrix} or a
#'   \code{data.frame} object holding responses. Missing responses are excluded
#'   from the ability estimation.
#' @param ip An \code{\link{Item-class}}, \code{\link{Itempool-class}} or a
#'   \code{\link{Testlet-class}} object. If \code{ip} is not an
#'   \code{\link{Itempool-class}} object, the function attempts to convert it.
#'   While the default is \code{NULL}, this argument is required for all methods
#'   except when \code{method = "sum_score"}.
#' @param method The method used for ability estimation. The default is
#'   \code{"eap"}.
#'
#'   Available methods:
#'   \describe{
#'     \item{\strong{\code{'sum_score'}}}{Basic sum (raw) score of responses.}
#'     \item{\strong{\code{'owen'}}}{Owen's Bayesian Ability Estimation.
#'
#'       This method is suitable for dichotomous IRT models (e.g., 'Rasch',
#'       '1PL', '2PL', '3PL' and '4PL'). Testlet groupings are ignored and items
#'       within testlets are treated as standalone items.
#'
#'       Formulas were implemented in Owen (1975) and Vale (1977). The original
#'       formulation does not include the D parameter. If \code{D = 1}, the
#'       original solution is obtained. If \code{D = 1.7}, the \code{a}
#'       parameter is multiplied by this number.
#'
#'       The user needs to provide prior parameters, i.e., \code{prior_pars}.
#'       These should be a numeric vector of length two, with the first
#'       component as the prior mean and the second as the prior standard
#'       deviation (not variance). For example, if the prior mean is 0.1 and the
#'       prior standard deviation is 2, set the prior parameters as
#'       \code{prior_pars = c(0.1, 2)}.
#'
#'     \item{\strong{\code{'ml'}}}{Maximum Likelihood Ability Estimation
#'       via Newton-Raphson Algorithm.}
#'     \item{\strong{\code{'eap'}}}{Expected-a-Posteriori Ability Estimation.
#'       Prior information must be provided for this function. The number of
#'       quadrature points can also be specified using the argument
#'       \code{number_of_quads}. }
#'     \item{\strong{\code{'map'}} or \strong{\code{'bm'}}}{Maximum-a-Posteriori
#'       Ability Estimation (or Bayes Modal estimation). Prior information must
#'       be provided for this function. Currently, only \code{'norm'} prior
#'       distribution is available.}
#'   }
#' @param ... Additional arguments passed to specific methods.
#' @param prior_dist The shape of the prior distribution. Available options are:
#'   \describe{
#'     \item{'norm'}{Normal distribution}
#'     \item{'unif'}{Uniform distribution}
#'     \item{'t'}{t distribution}
#'     \item{'cauchy'}{Cauchy distribution}
#'   }
#'   The default value is \code{'norm'}.
#' @param prior_pars Parameters of the prior distribution. Default value is
#'   \code{c(0, 1)}, where 0 is the mean and 1 is the standard deviation of the
#'   default normal prior distribution. For example, uniform prior parameter can
#'   be set as \code{c(a, b)} where \code{a} is the minimum value and \code{b}
#'   is the maximum value. For \code{t} distribution, prior parameter can be set
#'   as \code{df} to represent the degree of freedom. For Cauchy distribution,
#'   prior parameters can be set as \code{c(location, scale)}.
#'
#'   If method is \code{"owen"}, provide \code{c(<Prior Mean>, <Prior SD>)}.
#'
#' @param theta_range The limits of the ability estimation scale. The estimation
#'   result will be bounded within this interval. Default is \code{c(-5, 5)}.
#' @param number_of_quads Number of quadratures. The default value is 41. As
#'   this number increases, the precision of the estimate will also increase.
#' @param tol The precision level of ability estimate. The final ability
#'   estimates will be rounded to remove precision smaller than the \code{tol}
#'   value. Default is \code{1e-06}.
#' @param output_type A string specifying the output type of the function.
#'   Default is \code{"list"}. Options include:
#'   \describe{
#'     \item{"list"}{Function returns a \code{list} object with elements
#'       \code{est} and \code{se}.}
#'     \item{"data.frame"}{Function returns a \code{data.frame} object with
#'       columns \code{examinee_id}, \code{est} and \code{se}.}
#'     \item{"tibble"}{If the \code{tibble} package is available, the function
#'       returns a \code{tibble} object with columns \code{examinee_id},
#'       \code{est} and \code{se}.}
#'   }
#'
#' @return \code{est} The estimated examinee abilities. If the response vector
#'   for a subject contains all \code{NA}s, then \code{est} will be \code{NA} to
#'   differentiate from cases where all answers are incorrect.
#' @return \code{se} The standard errors of the ability estimates. For
#'   \code{"sum_score"} method, all standard errors will be \code{NA}. For
#'   Bayesian methods (like EAP, MAP or Owen's), this value is the square root
#'   of the posterior variance.
#'
#'
#' @author Emre Gonulates
#' @export
#'
#' @references
#' Owen, R. J. (1975). A Bayesian sequential procedure for quantal response in
#' the context of adaptive mental testing. Journal of the American Statistical
#' Association, 70(350), 351-356.
#'
#' Vale, C. D., & Weiss, D. J. (1977). A Rapid Item-Search Procedure for
#' Bayesian Adaptive Testing. Research Report 77-4. Minneapolis, MN.
#'
#' @examples
#' ip <- generate_ip(n = 7)
#' resp <- sim_resp(ip, theta = rnorm(3))
#'
#' ### EAP estimation ###
#' est_ability(resp, ip)
#' est_ability(resp, ip, number_of_quads = 81)
#' # The default prior_dist is 'norm'. prior_pars = c(mean, sd)
#' est_ability(resp, ip, prior_pars = c(0, 3))
#' # prior_pars = c(min, max)
#' est_ability(resp, ip, prior_dist = 'unif',  prior_pars = c(-3, 3))
#' # prior_pars = c(df)
#' est_ability(resp, ip, prior_dist = 't',  prior_pars = 3)
#' # prior_pars = c(location, scale)
#' est_ability(resp, ip, prior_dist = 'cauchy',  prior_pars = c(0, 1))
#'
#'
#' ### MAP estimation (Bayes Modal estimation) ###
#' est_ability(resp, ip, method = "map")
#' # The default prior_dist is 'norm'. prior_pars = c(mean, sd)
#' est_ability(resp, ip, method = "map", prior_pars = c(0, 2))
#'
#'
#' ### Maximum Likelihood estimation ###
#' est_ability(resp, ip, method = 'ml')
#' est_ability(resp, ip, method = 'ml', tol = 1e-8)
#' est_ability(resp = rep(1, length(ip)), ip, method = 'ml')
#' est_ability(resp = rep(1, length(ip)), ip, method = 'ml',
#'             theta_range = c(-3, 3))
#'
#' ### Owen's Bayesian ability estimation ###
#' est_ability(resp, ip, method = 'owen')
#' est_ability(resp, ip, method = 'owen', prior_pars = c(0, 3))
#'
#'
#'
est_ability <- function(
    resp,
    ip = NULL,
    method = c("eap", "ml", "map", "bm", "owen", "sum_score"),
    ...,
    prior_dist = c("norm", "unif", "lnorm", "gamma", "t", "cauchy"),
    prior_pars = c(0, 1),
    theta_range = c(-5, 5),
    number_of_quads = 41,
    tol = 1e-6,
    output_type = c("list", "data.frame", "tibble")) {
  # args <- list(...)
  method <- tolower(method)
  method <- match.arg(method)

  prior_dist <- tolower(prior_dist)
  prior_dist <- match.arg(prior_dist)

  output_type <- tolower(output_type)
  output_type <- match.arg(output_type)
  # Check item pool:
  if (!is.null(ip) && !is(ip, "Itempool")) ip <- itempool(ip)
  if (is.null(ip) && method != "sum_score") {
    stop(paste0("Invalid 'ip'. For '", method, "', ip cannot be NULL."))
  }

  # Convert resp to Response_set object. For sum_score, conversion is not
  # necessary if resp is already a data.frame or 'matrix'.
  if (method == "sum_score" &&
      (is.matrix(resp) || inherits(resp, "data.frame"))) {
       resp_set <- resp

    # if the 'resp' satisfies this conditions, directly run cpp function
  } else if (method == "eap" &&
             (is.matrix(resp) || inherits(resp, "data.frame")) &&
             (ncol(resp) == ip$n$items) &&
             all(apply(resp, 2, is.numeric))
             ) {
    est <- est_ability_eap_cpp(
      resp = as.matrix(resp), ip = ip, theta_range = theta_range,
      no_of_quadrature = number_of_quads, prior_dist = prior_dist,
      prior_par = prior_pars)

    temp_row_names <- rownames(resp)
    if (!is.null(temp_row_names)) {
      est$est <- stats::setNames(est$est, temp_row_names)
      est$se <- stats::setNames(est$se, temp_row_names)
    }
    se <- est$se
    est <- est$est

    # est$est <- round(est$est, floor(abs(log10(tol))))
    # est$se <- round(est$se, floor(abs(log10(tol))))
    # return(est)

    method <- "eap_matrix" # to prevent getting into "switch" statement below
    resp_set <- resp
  } else {
    resp_set <- convert_to_resp_set(resp = resp, ip = ip, object_name = "resp")
  }

  switch(
    method,
    "sum_score" = {
      if (is(resp_set, "Response_set")) {
        resp_matrix <- as.matrix(resp_set, output = "score")
      # When method = "sum_score" and resp is matrix/data.frame, resp was
      # previously assigned to resp_set.
      } else resp_matrix <- resp_set
      se <- est <- setNames(rep(as.numeric(NA), nrow(resp_matrix)),
                            rownames(resp_matrix))
      est <- rowSums(resp_matrix, na.rm = TRUE)

    },
    "owen" = {
      # This method cannot be used for models other than dichotomous IRT models.
      if (!all(ip$item_model %in% UNIDIM_DICHO_MODELS))
        stop(paste0("Owen's Bayesian ability estimation method can only ",
                    "be used for dichotomous IRT models: ",
                    paste0(UNIDIM_DICHO_MODELS, collapse = ", "), "."))
      resp_matrix <- as.matrix(resp_set, ip = ip, output = "score")

      output <- sapply(apply(resp_matrix, 1, function(r) est_ability_owen_cpp(
        ip = ip, resp = r, m0 = prior_pars[1], v0 = prior_pars[2]^2)
        ), function(x) x)
      est <- unlist(output[1, ])
      se <- unlist(output[2, ])
    },
    "ml" = {
      # ip_list <- flatten_itempool_cpp(ip)
      n_examinee <- length(resp_set)
      se <- est <- rep(NA, n_examinee)

      est <- sapply(resp_set@response_list, function(r)
        stats::optimize(f = resp_loglik_response_cpp, resp = r, ip = ip,
                        lower = theta_range[1], upper = theta_range[2],
                        tol = tol/10, maximum = TRUE)$maximum)

      # est <- sapply(resp_set@response_list, function(r)
      #   stats::optimize(f = function(x) resp_loglik_response_cpp(
      #     theta = x, resp = r, ip = ip),
      #     interval = theta_range, maximum = TRUE, tol = tol*.5)$maximum)

      # For 3PL model `optimize` function sometimes finds the local maximum
      # instead of local minimum. The following piece tacles with this
      if ("3PL" %in% ip$item_model) {
        theta_bins <- seq(from = theta_range[1], to = theta_range[2],
                          by = min(.5, diff(theta_range)/2))
        n_bin <- length(theta_bins)
        # Log-likelihood value at the estimated theta
        ll_est <- round(resp_loglik_response_set_cpp(
          resp_set = resp_set, ip = ip, theta = est), 4)
        # Log-likelihood at each bin value
        ll_bins <- sapply(theta_bins, function(x) resp_loglik_response_set_cpp(
          resp_set = resp_set, ip = ip, theta = rep(x, n_examinee)))
        if (n_examinee == 1) ll_bins <- matrix(ll_bins, nrow = 1)
        ll_flag <- apply(round(ll_bins, 4), 2, function(x) x > ll_est)
        if (n_examinee == 1) ll_flag <- matrix(ll_flag, nrow = 1)
        ll_flag <- apply(ll_flag, 1, which)
        if (n_examinee == 1) ll_flag <- list(as.vector(ll_flag))
        problematic_cases <- which(sapply(ll_flag, length) > 0)
        if (length(problematic_cases) > 0) {
          new_theta_ranges <- lapply(
            ll_flag[problematic_cases], function(x)
              c(theta_bins[max(1, min(x) - 1)],
                theta_bins[min(n_bin, max(x) + 1)]))
          est[problematic_cases] <- sapply(
            seq_along(problematic_cases),
            function(i)
              stats::optimize(
                f = resp_loglik_response_cpp,
                resp = resp_set@response_list[[problematic_cases[i]]],
                ip = ip,
                lower = new_theta_ranges[[i]][1],
                upper = new_theta_ranges[[i]][2],
                tol = tol * .1, maximum = TRUE)$maximum)
        }

        # ll_lb <- resp_loglik_response_set_cpp(
        #   resp_set = resp_set, theta = rep(theta_range[1], n), ip = ip)
        # ll_ub <- resp_loglik_response_set_cpp(
        #   resp_set = resp_set, theta = rep(theta_range[2], n), ip = ip)
        # ll_est <- resp_loglik_response_set_cpp(
        #   resp_set = resp_set, theta = est, ip = ip)
        # est <- ifelse(ll_est >= ll_ub & ll_est >= ll_lb, est,
        #               ifelse(ll_ub >= ll_est, theta_range[2], theta_range[1]))
      }

      # est <- stats::optim(par = 0,
      #                     fn = function(x) -irt:::resp_loglik_response_cpp(
      #                       theta = x, resp = r, ip_list = ip_list),
      #                     method = "Brent",
      #                     lower = theta_range[1], upper = theta_range[2],
      #                     control = list(factr = tol))$par

      # est <- apply(resp, 1, function(r) stats::optimize(f = function(x)
      #   resp_loglik_itempool_cpp(resp = matrix(r, nrow = 1), theta = x,
      #                            ip = ip),
      #   interval = theta_range, maximum = TRUE, tol = tol*.5)$maximum)

      if (any(ip$model == "RTM")) { # Use Hessian for Rasch Testlet Model
        min_func <- function(theta, resp, ip) {
         -1 * resp_loglik_response_cpp(theta = theta, resp = resp, ip = ip)
        }

        for (i in 1:length(est)) {
          se[i] <-  1/sqrt(stats::optimHess(par = est[i], fn = min_func,
                                            resp = resp_set[[i]], ip = ip)[1])
        }
      } else {
        se <- 1/sqrt(info_response_set_cpp(theta = est, resp_set = resp_set,
                                           ip = ip, tif = TRUE)[, 1])
      }
    },
    # "ml_parallel" = {
    #   ip_list <- flatten_itempool_cpp(ip)
    #   se <- est <- rep(NA, length(resp_set))
    #
    #   max_cores <- parallel::detectCores()
    #   n_cores <- NULL
    #   n_cores <- ifelse(is.null(n_cores), max_cores, min(max(n_cores, 1),
    #                                                      max_cores))
    #   optim_func <- function(r, ip_list, theta_range, tol) {
    #     stats::optimize(f = function(x) irt:::resp_loglik_response_cpp(
    #       theta = x, resp = r, ip_list = ip_list),
    #       interval = theta_range, maximum = TRUE, tol = tol*.5)$maximum
    #   }
    #   cl <- parallel::makeCluster(n_cores)
    #   est <- parallel::parSapplyLB(
    #     cl = cl, X = resp_set@response_list, FUN = optim_func,
    #     ip_list = ip_list, theta_range = theta_range, tol =  tol)
    #   parallel::stopCluster(cl)
    #   se <- 1/sqrt(info_response_set_cpp(theta = est, resp_set = resp_set,
    #                                      ip = ip, tif = TRUE)[, 1])
    # },
    "eap" = {
      output <- est_ability_eap_response_set_cpp(
        resp_set = resp_set, ip = ip, theta_range = theta_range,
        no_of_quadrature = number_of_quads,
        prior_dist = prior_dist,
        prior_par = prior_pars)
      # output <- est_ability_eap_cpp(
      #   resp = resp, ip = ip, theta_range = theta_range,
      #   no_of_quadrature = number_of_quads,
      #   prior_dist = prior_dist, prior_par = prior_pars)
      est <- output$est
      se <- output$se
    },
    "bm" = ,  # Alternatively use "BM", Bayes Modal
    "map" = {
      if (prior_dist != "norm") {
        stop("Invalid 'prior_dist'. 'map' ability estimation method ",
             "currently can only be used with normally distributed prior, ",
             "i.e. please set prior_dist = 'norm'.")
      }

      # Check prior_pars
      if (!is_atomic_vector(prior_pars, class = c("integer", "numeric")) ||
          (length(prior_pars) != 2) || (prior_pars[2] < 0))
        stop("Invalid 'prior_pars'. 'prior_pars' for normal distribution ",
             "('norm') should be a numeric vector of length two. The first ",
             "element should be the mean and second element should be the ",
             "standard deviation.")

      temp <- est_ability_map_response_set_cpp(
        resp_set = resp_set, ip = ip, prior_dist = prior_dist,
        prior_par = prior_pars, theta_range = theta_range)
      est <- temp$est
      se <- temp$se

    }
    # ,
    # stop("This method has not been implemented yet.")
    )

  # Round numbers up to tolerance
  if (method != "sum_score") {
    est <- round(est, floor(abs(log10(tol))))
    se <- round(se, floor(abs(log10(tol))))
  }

  # # Convert all NA response strings's est and se to NA
  # all_na_rows <- apply(is.na(resp), 1, all)
  # est[all_na_rows] <- NA
  # se[all_na_rows] <- NA

  # Set examinee names
  if ((!is.atomic(resp) || is.matrix(resp)) && is(resp_set, "Response_set")) {
    est <- stats::setNames(est, resp_set$examinee_id)
    se <- stats::setNames(se, resp_set$examinee_id)
  }
  if (output_type == "list") {
    result <- list(est = est, se = se)
  } else if (output_type %in% c("data.frame", "tibble")) {
    result <- data.frame(examinee_id = NA, est = est, se = se)
    result$examinee_id <- names(est)
    if (output_type == "tibble" && requireNamespace("tibble")) {
      result <- tibble::as_tibble(result)
    }
  }

  return(result)
}

