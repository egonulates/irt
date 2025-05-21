

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
#' `est_ability()` estimates the ability (latent trait level) of examinees based
#' on their responses to test items using various Item Response Theory (IRT)
#' methods. It supports both classical estimation techniques, like Maximum
#' Likelihood (ML), and Bayesian approaches, such as Expected-a-Posteriori
#' (EAP), Maximum-a-Posteriori (MAP), and Owen’s Bayesian Method. These methods
#' have been widely studied and used in psychometric research to infer examinee
#' traits accurately.
#'
#' The function is versatile and can handle different models (e.g., dichotomous
#' and polytomous) and item formats, making it suitable for a range of
#' psychometric applications. Moreover, the inclusion of different prior
#' distributions and flexible output formats enhances the adaptability of this
#' function for both research and applied testing scenarios.
#'
#'
#' @details
#' Estimating examinee ability is a core component of IRT, which models the
#' probability of a correct response as a function of the examinee's ability and
#' item parameters. Each method offered by `est_ability()` provides unique
#' advantages:
#'
#' - **Owen’s Bayesian Method**: This method is a sequential Bayesian
#'   estimation technique, particularly effective for dichotomous IRT models
#'   such as the Rasch model and its extensions (e.g., 2PL, 3PL).
#'   Introduced by Owen (1975), it uses prior information to iteratively update
#'   ability estimates, making it suitable for adaptive testing and scenarios
#'   where prior knowledge about the examinee population is available. The
#'   method requires prior parameters, specified as the mean and standard
#'   deviation of the prior distribution. Vale & Weiss (1977) later optimized
#'   this approach for faster computations in adaptive testing.
#'
#' - **Maximum Likelihood Estimation (ML)**: A traditional and widely-used
#'   method in psychometrics, ML maximizes the likelihood of the observed
#'   response pattern given the ability parameter. It is most effective when
#'   item response data are well-aligned with the underlying IRT model. However,
#'   in models like the 3PL, ML can sometimes converge to local maxima,
#'   particularly when the guessing parameter is non-negligible
#'   (Baker & Kim, 2004). To mitigate this, the algorithm in `est_ability()`
#'   performs checks to ensure the global maximum is identified, even if this
#'   requires additional computational effort.
#'
#' - **Expected-a-Posteriori (EAP) Estimation**: This Bayesian method
#'   calculates the posterior mean of the ability distribution, integrating over
#'   a specified grid of quadrature points. It provides stable estimates,
#'   especially when dealing with sparse data or extreme response patterns
#'   (Bock & Aitkin, 1981). The precision of the EAP estimate depends on the
#'   number of quadrature points, with more points offering higher accuracy but
#'   increased computational cost.
#'
#' - **Maximum-a-Posteriori (MAP) or Bayes Modal (BM) Estimation**: MAP
#'   estimation finds the mode of the posterior distribution, providing a
#'   Bayesian alternative to ML that incorporates prior information. It is
#'   particularly useful in situations where prior beliefs about the ability
#'   distribution can improve estimation accuracy, such as in small-sample
#'   testing scenarios (Samejima, 1997). In `est_ability()`, only a normal prior
#'   distribution is currently supported for MAP estimation.
#'
#' - **Sum Score**: A simple and intuitive method that calculates the
#'   raw (sum) score of the responses. While this method does not consider item
#'   parameters or provide uncertainty estimates, it is useful for preliminary
#'   analyses or when IRT modeling is not feasible.
#'
#' @param resp A \code{\link{Response_set-class}}, \code{matrix}, or a
#'   \code{data.frame} object holding examinee responses. Missing values
#'   (\code{NA}s) in the response data are excluded from the ability estimation.
#'
#' @param ip An \code{\link{Item-class}}, \code{\link{Itempool-class}}, or
#'   \code{\link{Testlet-class}} object representing item parameters. If this is
#'   not provided as an \code{\link{Itempool-class}} object, the function will
#'   attempt to convert it. Note: This argument is mandatory for all methods
#'   except \code{"sum_score"}.
#' @param method A character string specifying the method for ability
#'   estimation. Options include:
#'   \describe{
#'     \item{\strong{"sum_score"}}{Calculates the raw score of responses,
#'      without modeling item characteristics. Useful for exploratory purposes
#'      or when IRT modeling is unnecessary.
#'     }
#'     \item{\strong{"owen"}}{Owen's Bayesian Ability Estimation, suited for
#'       dichotomous IRT models (e.g., Rasch, 2PL, 3PL, 4PL). This method is
#'       based on sequential Bayesian updates and requires specifying
#'       \code{prior_pars} (mean and standard deviation of the prior
#'       distribution). Refer to Owen (1975) for theoretical foundations and
#'       Vale & Weiss (1977) for practical applications in adaptive testing.
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
#'     }
#'     \item{\strong{"ml"}}{Maximum Likelihood Estimation. Effective for data
#'       that align well with IRT models. For models using the 3PL, the
#'       estimation algorithm may occasionally converge on a local maximum
#'       instead of the global maximum. In such cases, the estimation process
#'       may take longer as it attempts to locate the global maximum,
#'       potentially outside the defined \code{theta_range}. Additionally, if
#'       the global maximum lies outside the specified \code{theta_range}, the
#'       standard error will be calculated at the nearest boundary within the
#'       \code{theta_range}, even though the likelihood of the response may not
#'       be maximized at that point.
#'     }
#'     \item{\strong{"eap"}}{Expected-a-Posteriori Estimation. This method uses
#'       numerical integration over a specified grid of quadrature points, with
#'       higher numbers of points yielding more precise estimates (Bock &
#'       Aitkin, 1981). Prior information must be provided for this function.
#'       The number of quadrature points can also be specified using the
#'       argument \code{number_of_quads}.
#'     }
#'     \item{\strong{"map"}  or \strong{"bm"}}{Maximum-a-Posteriori
#'       (or Bayes Modal) Estimation. Uses prior information to provide robust
#'       ability estimates, especially useful for small samples (Samejima,
#'       1997). Only a normal prior is currently supported.}
#'   }
#'   Default is \code{"eap"}.
#' @param ... Additional arguments passed to the specific estimation methods.
#'
#' @param prior_dist A character string indicating the shape of the prior
#'   distribution. Options include:
#'   \describe{
#'     \item{\strong{"norm"}}{Normal distribution (default).}
#'     \item{\strong{"unif"}}{Uniform distribution.}
#'     \item{\strong{"t"}}{t-distribution, offering flexibility in
#'       modeling heavy tails.}
#'     \item{\strong{"cauchy"}}{Cauchy distribution, known for heavy tails and
#'       robustness to outliers.}
#'   }
#' @param prior_pars A numeric vector specifying the parameters of the prior
#'   distribution. The default is \code{c(0, 1)}, representing a mean of 0 and a
#'   standard deviation of 1 for the normal prior. Different distributions
#'   require different parameterizations:
#'   \itemize{
#'     \item For \code{"norm"}: \code{c(mean, sd)}.
#'     \item For \code{"unif"}: \code{c(min, max)}.
#'     \item For \code{"t"}: Degrees of freedom \code{df}.
#'     \item For \code{"cauchy"}: \code{c(location, scale)}.
#'   }
#'   If method is \code{"owen"}, provide \code{c(<Prior Mean>, <Prior SD>)}.
#'
#' @param theta_range A numeric vector of length two, setting the bounds of the
#'   ability scale. This range ensures that estimates remain within realistic
#'   limits. Default is \code{c(-5, 5)}.
#' @param number_of_quads An integer specifying the number of quadrature points
#'   for EAP estimation. More points increase precision but also computational
#'   load. Default is 41.
#' @param tol A numeric value indicating the tolerance for precision in ability
#'   estimates. Smaller values yield more precise results. The final ability
#'   estimates will be rounded to remove precision smaller than the \code{tol}
#'   value. Default is \code{1e-6}.
#'
#'
#' @param output_type A character string specifying the format of the output.
#'   Options are:
#'   \describe{
#'     \item{"list"}{Returns a \code{list} containing \code{est} (ability
#'       estimates) and \code{se} (standard errors).}
#'     \item{"data.frame"}{Returns a \code{data.frame} with columns
#'       \code{examinee_id}, \code{est}, and \code{se}.}
#'     \item{"tibble"}{Returns a \code{tibble} (if the \code{tibble} package
#'       is installed) with the same structure as \code{data.frame}.}
#'   }
#'   Default is \code{"list"}.
#'
#' @return A \code{list}, \code{data.frame}, or \code{tibble}, depending on
#'   \code{output_type} argument, containing:
#'   \describe{
#'     \item{est}{Estimated abilities. \code{NA} is returned if all responses for an examinee are \code{NA}.}
#'     \item{se}{Standard errors of the estimates. For \code{"sum_score"},
#'     standard errors are \code{NA}. Bayesian methods (EAP, MAP or Owen's)
#'     provide the square root of the posterior variance.}
#'   }
#'
#'
#' @author Emre Gonulates
#' @export
#'
#' @references
#' - Baker, F. B., & Kim, S.-H. (2004). \emph{Item Response Theory: Parameter Estimation Techniques}. New York: CRC Press.
#' - Bock, R. D., & Aitkin, M. (1981). Marginal maximum likelihood estimation of item parameters: Application of an EM algorithm. \emph{Psychometrika}, 46(4), 443–459.
#' - Owen, R. J. (1975). A Bayesian sequential procedure for quantal response in the context of adaptive mental testing. \emph{Journal of the American Statistical Association}, 70(350), 351-356.
#' - Samejima, F. (1997). Graded response model. In W. J. van der Linden & R. K. Hambleton (Eds.), \emph{Handbook of Modern Item Response Theory} (pp. 85-100). New York: Springer.
#' - Vale, C. D., & Weiss, D. J. (1977). A rapid item-search procedure for Bayesian adaptive testing. Research Report 77-4. Minneapolis, MN: University of Minnesota.
#'
#' @examples
#' # Load or generate an example item pool
#' ip <- generate_ip(n = 10)  # Generating an item pool with 10 items
#'
#' # Simulate responses for three examinees with true abilities from a normal distribution
#' resp <- sim_resp(ip, theta = rnorm(3))
#'
#' ### Example 1: EAP Estimation ###
#' # Basic EAP estimation with default parameters
#' eap_results <- est_ability(resp, ip)
#' print(eap_results)
#'
#' # Increasing the number of quadrature points for higher precision
#' eap_results_high_precision <- est_ability(resp, ip, number_of_quads = 81)
#' print(eap_results_high_precision)
#'
#' # Specifying a different normal prior: mean = 0, sd = 3
#' eap_with_custom_prior <- est_ability(resp, ip, prior_pars = c(0, 3))
#' print(eap_with_custom_prior)
#'
#' # Using a uniform prior distribution between -3 and 3
#' eap_with_uniform_prior <- est_ability(
#'   resp, ip, prior_dist = 'unif', prior_pars = c(-3, 3))
#' print(eap_with_uniform_prior)
#'
#' # Using a t-distribution with 3 degrees of freedom as the prior
#' eap_with_t_prior <- est_ability(
#'   resp, ip, prior_dist = 't', prior_pars = 3)
#' print(eap_with_t_prior)
#'
#' # Using a Cauchy prior with location = 0 and scale = 1
#' eap_with_cauchy_prior <- est_ability(
#'   resp, ip, prior_dist = 'cauchy', prior_pars = c(0, 1))
#' print(eap_with_cauchy_prior)
#'
#' ### Example 2: MAP Estimation (Bayes Modal) ###
#' # Basic MAP estimation using a normal prior with mean = 0 and sd = 2
#' map_results <- est_ability(resp, ip, method = "map", prior_pars = c(0, 2))
#' print(map_results)
#'
#' # Trying a different theta range for MAP estimation
#' map_with_theta_range <- est_ability(
#'   resp, ip, method = "map", theta_range = c(-3, 3))
#' print(map_with_theta_range)
#'
#' ### Example 3: Maximum Likelihood Estimation (ML) ###
#' # Basic ML estimation
#' ml_results <- est_ability(resp, ip, method = "ml")
#' print(ml_results)
#'
#' # Increasing the tolerance for higher precision in ML estimation
#' ml_high_precision <- est_ability(resp, ip, method = "ml", tol = 1e-8)
#' print(ml_high_precision)
#'
#' # Handling extreme cases where all responses are correct
#' all_correct_resp <- matrix(1, nrow = 1, ncol = ncol(resp))
#' ml_all_correct <- est_ability(all_correct_resp, ip, method = "ml")
#' print(ml_all_correct)
#'
#' # Restricting the theta range in cases of extreme response patterns
#' ml_with_restricted_range <- est_ability(
#'   all_correct_resp, ip, method = "ml", theta_range = c(-3, 3))
#' print(ml_with_restricted_range)
#'
#' ### Example 4: Owen's Bayesian Estimation ###
#' # Basic Owen's Bayesian estimation with default prior parameters
#' owen_results <- est_ability(resp, ip, method = "owen")
#' print(owen_results)
#'
#' # Providing custom prior parameters: mean = 0, sd = 3
#' owen_with_custom_prior <- est_ability(resp, ip, method = "owen", prior_pars = c(0, 3))
#' print(owen_with_custom_prior)
#'
#' ### Example 5: Sum Score Method ###
#' # Calculating the raw sum score without considering item parameters
#' sum_score_results <- est_ability(resp, ip, method = "sum_score")
#' print(sum_score_results)
#'
#' ### Example 6: Customizing Output Format ###
#' # Returning results as a data.frame
#' df_results <- est_ability(resp, ip, output_type = "data.frame")
#' print(df_results)
#'
#' # Returning results as a tibble (if the tibble package is available)
#' if (requireNamespace("tibble", quietly = TRUE)) {
#'   tibble_results <- est_ability(resp, ip, output_type = "tibble")
#'   print(tibble_results)
#' }
#'
#' ### Example 7: Handling Missing Data ###
#' # Simulating a response matrix with some missing values
#' resp_with_nas <- resp
#' resp_with_nas[1, 2] <- NA  # Introducing a missing response
#' eap_with_nas <- est_ability(resp_with_nas, ip, method = "eap")
#' print(eap_with_nas)
#'
#' ### Example 8: Batch Processing for Large Datasets ###
#' # Simulating a larger set of responses for 100 examinees
#' large_resp <- sim_resp(ip, theta = rnorm(100))
#' large_ml_results <- est_ability(large_resp, ip, method = "ml")
#' head(large_ml_results$est)  # Displaying only the first few estimates
#'
#' # EAP estimation for a large dataset with high precision
#' large_eap_results <- est_ability(
#'   large_resp, ip, method = "eap", number_of_quads = 81)
#' head(large_eap_results$est)
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
      # instead of local minimum. The following section tackles this
      if ("3PL" %in% ip$item_model) {
        theta_bins <- seq(from = -4, to = 4,
                          by = min(.5, diff(theta_range)/2))

        extreme_theta_values <- c(5, 7.5, 10, 25, 50, 100, 1000)
        theta_bins <- sort(c(-extreme_theta_values, theta_bins,
                             extreme_theta_values))
        #  theta_bins,
        #  extreme_theta_values[extreme_theta_values > theta_range[2]],
        # -extreme_theta_values[-extreme_theta_values < theta_range[1]]))

        n_bin <- length(theta_bins)
        # Log-likelihood value at the estimated theta
        ll_est <- resp_loglik_response_set_cpp(
          resp_set = resp_set, ip = ip, theta = est)
        # Log-likelihood at each bin value
        ll_bins <- sapply(theta_bins, function(x) resp_loglik_response_set_cpp(
          resp_set = resp_set, ip = ip, theta = rep(x, n_examinee)))
        if (n_examinee == 1) ll_bins <- matrix(ll_bins, nrow = 1)
        ll_flag <- apply(ll_bins, 2, function(x) x > ll_est)
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

          est[problematic_cases] <- ifelse(
            est[problematic_cases] > theta_range[2], theta_range[2],
            est[problematic_cases])
          est[problematic_cases] <- ifelse(
            est[problematic_cases] < theta_range[1], theta_range[1],
            est[problematic_cases])
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

