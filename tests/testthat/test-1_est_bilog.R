
#' Check to see whether the BILOG-MG is installed in this machine. If no, skip
#' the tests on this file.
#'
#' @noRd
skip_if_bilog_exe_not_found <- function() {
  bilog_exe_paths <- c(file.path("C:/Program Files/BILOGMG"))
  if (!any(dir.exists(bilog_exe_paths)))
    skip("Bilog is not installed on this compter.")
}


# Set this TRUE to skip all of the tests in this file.
skip_tests <- TRUE

target_dir <- "C:/Temp/testthat-bilog"

############################################################################@###
############################################################################@###
################### est_bilog ##################################################
############################################################################@###
############################################################################@###


test_that("est_bilog", {
  skip_if(skip_tests)
  skip_if_bilog_exe_not_found()

  # model = "3PL"
  # analysis_name = "bilog_calibration"
  # items = NULL
  # examinee_id_var = NULL
  # group_var = NULL
  # logistic = TRUE
  # num_of_alternatives = NULL
  # overwrite = TRUE
  # criterion = 0.01
  # num_of_quadrature = 31
  # max_em_cycles = 100
  # newton = 20
  # reference_group = NULL
  # fix = NULL
  # scoring_options = c("METHOD=1", "NOPRINT")
  # calib_options = c("NORMAL")
  # bilog_exe_folder = file.path("C:/Program Files/BILOGMG")
  # show_output_on_console = TRUE


  # -------------------------------------------------------------------------- #
  # fit 2PL model
  n_item <- sample(30L:36L, 1)
  n_theta <- sample(1900L:2200L, 1)
  true_ip <- generate_ip(n = n_item, model = "2PL")
  resp <- sim_resp(ip = true_ip, theta = rnorm(n_theta), prop_missing = .1)

  # The following line will run BILOG-MG, estimate 2PL model and put the
  # analysis results under the target directory:
  c1 <- est_bilog(x = resp,
                  model = "2PL",
                  target_dir = target_dir, overwrite = TRUE,
                  show_output_on_console = FALSE)

  if (c1$converged) { # these tests are valid only if there is convergence
    expect_s4_class(c1$ip, "Itempool")
    expect_identical(nrow(c1$score), n_theta)
    expect_identical(nrow(c1$ctt), n_item)

    expect_s3_class(c1$posterior_dist, "data.frame")
    expect_identical(nrow(c1$posterior_dist),
                     as.integer(c1$input$num_of_quadrature))
    expect_identical(sum(c1$posterior_dist$weight), 1, tolerance = 0.01)
  }

  # -------------------------------------------------------------------------- #
  # model = "CTT"
  c2 <- est_bilog(x = resp,
                  model = "CTT",
                  target_dir = target_dir,
                  overwrite = TRUE,
                  show_output_on_console = FALSE)
  expect_null(c2$ip)
  expect_null(c2$converged)
  expect_null(c2$score)
  expect_identical(nrow(c2$ctt), n_item)
  expect_identical(c2$input$model, "CTT")
  expect_identical(c2$input$target_dir, target_dir)

  # -------------------------------------------------------------------------- #
  # Rasch model
  n_theta <- sample(1000L:2000L, 1)
  n_item <- sample(20L:36L, 1)
  ip <- generate_ip(n = n_item, model = "2PL")
  ip$a <- 1.5
  resp <- sim_resp(ip = ip, theta = rnorm(n_theta), prop_missing = .2,
                   output = "response_set")
  c_rasch <- est_bilog(x = resp,
                       model = "Rasch",
                       target_dir = target_dir,
                       overwrite = TRUE,
                       num_of_quadrature = 77,
                       show_output_on_console = FALSE)

  if (c_rasch$converged) { # these tests are valid only if there is convergence
    expect_s4_class(c_rasch$ip, "Itempool")
    expect_true(all(c_rasch$ip$model == "Rasch"))
    expect_identical(nrow(c_rasch$score), n_theta)
    expect_identical(nrow(c_rasch$ctt), n_item)

    expect_s3_class(c_rasch$posterior_dist, "data.frame")
    expect_identical(nrow(c_rasch$posterior_dist),
                     as.integer(c_rasch$input$num_of_quadrature))
    expect_identical(sum(c_rasch$posterior_dist$weight), 1, tolerance = 0.01)
  }

  # -------------------------------------------------------------------------- #
  # 1PL model
  c_1pl <- est_bilog(x = resp,
                     model = "1PL",
                     target_dir = target_dir, overwrite = TRUE,
                     show_output_on_console = FALSE)
  if (c_1pl$converged) { # these tests are valid only if there is convergence
    expect_true(c_1pl$converged)
    expect_s4_class(c_1pl$ip, "Itempool")
    expect_true(all(c_1pl$ip$model == "2PL"))
  }

  # -------------------------------------------------------------------------- #
  # Model with response as tibble
  ip <- generate_ip(n = 20)
  resp_set <- generate_resp_set(ip = ip, theta = rnorm(800), prop_missing = .2)
  resp <- as.matrix(resp_set)
  resp <- tibble::as_tibble(resp)
  resp$examinee_id <- resp_set$examinee_id
  c3 <- est_bilog(x = resp,
                  model = "3PL",
                  examinee_id_var = "examinee_id",
                  target_dir = target_dir,
                  logistic = TRUE,
                  calib_options = c("NORMAL"),
                  overwrite = TRUE,
                  show_output_on_console = FALSE)
  expect_identical(c3$score$examinee_id, resp$examinee_id)


  # -------------------------------------------------------------------------- #
  # Failing Calibration - Some items will failed to converge
  n_theta <- sample(200L:300L, 1)
  n_item <- sample(10L:26L, 1)
  resp <- matrix(sample(0:1, n_theta*n_item, T), ncol = n_item)
  resp[sample(1:(n_theta*n_item), round(n_theta * n_item * .6))] <- NA
  resp[2:nrow(resp), 2] <- NA
  expect_warning(cfail <- est_bilog(x = resp,
                                   max_em_cycles = 10,
                                   criterion = 0.00001,
                                   target_dir = target_dir,
                                   overwrite = TRUE,
                                   show_output_on_console = FALSE))
  expect_false(is.null(cfail$failed_items))


  # -------------------------------------------------------------------------- #
  # All Item IDs should have 8 or less characters.
  true_theta <- rnorm(4000)
  true_ip <- generate_ip(n = 30, model = "2PL",
                         item_id = paste0("ID12345", 11:40))
  resp <- sim_resp(true_ip, true_theta)

  expect_error(est_bilog(x = resp, model = "2PL",
                         target_dir = "C:/Temp/Analysis", overwrite = TRUE),
               regexp = "Invalid item IDs.")

  # -------------------------------------------------------------------------- #
  # Fix error: when response data contains examinee ID variable or group
  # variable that has a column name larger than 8 characters, but all item
  # IDs are less than 8 characters, the function gives a false error.
  true_theta <- rnorm(4000)
  true_ip <- generate_ip(n = 30, model = "2PL")
  resp <- sim_resp(true_ip, true_theta)
  resp <- cbind.data.frame(examinee_id = paste0("s", 1:nrow(resp)), resp)

  output <- est_bilog(x = resp, model = "2PL", examinee_id_var = "examinee_id",
                      target_dir = "C:/Temp/Analysis", overwrite = TRUE)
  expect_s3_class(output, "bilog_output")


  # -------------------------------------------------------------------------- #
  # # Compare 1PL with Rasch
  # ip <- generate_ip(n = 30, model = "1PL")
  # resp_set <- generate_resp_set(ip = ip, theta = rnorm(800), prop_missing = .2)
  # c4 <- est_bilog(x = resp_set,
  #                 model = "2PL",
  #                 target_dir = target_dir,
  #                 prior_ip = list(SMU = 1, SSIGMA = .94),
  #                 calib_options = c("NORMAL"),
  #                 overwrite = TRUE,
  #                 show_output_on_console = FALSE)

})



############################################################################@###
############################################################################@###
################### est_bilog - PRIORS for Item Parameters #####################
############################################################################@###
############################################################################@###

test_that("est_bilog - PRIORS for Item Parameters", {
  skip_if(skip_tests)
  skip_if_bilog_exe_not_found()

  # Fixed guessing parameters
  true_theta <- rnorm(3000)
  true_ip <- generate_ip(n = 30, model = "3PL")
  true_ip$c <- 0.25
  resp <- sim_resp(true_ip, true_theta)
  prc1 <- est_bilog(x = resp, model = "3PL", target_dir = target_dir,
                    prior_ip = list(ALPHA = 10000000, BETA = 30000000),
                    show_output_on_console = FALSE, overwrite = TRUE)
  if (prc1$converged) { # these tests are valid only if there is convergence
    expect_true(all(prc1$ip$c == 0.25))
    expect_true(all(prc1$ip$se_c == prc1$ip$se_c[1]))
  }


  # -------------------------------------------------------------------------- #
  # Fix all parameters.
  true_theta <- rnorm(3000)
  true_ip <- generate_ip(n = 30, model = "2PL")
  true_ip$a <- 1
  true_ip$c <- .25
  resp <- generate_resp_set(ip = true_ip, theta = true_theta)
  # prc2 <- est_bilog(x = resp, model = "2PL", target_dir = target_dir,
  #                   prior_ip = list(SSIGMA = 0.05),
  #                   show_output_on_console = FALSE, overwrite = TRUE)
  # (prc2 <- est_bilog(x = resp, model = "3PL", target_dir = target_dir,
  #                   # max_em_cycles = 500,
  #                   # criterion = 0.1,
  #                   prior_ip = list(ALPHA = 10000000,
  #                                   # SMU = 1,
  #                                   # SSIGMA = 0.15,
  #                                   BETA = 30000000),
  #                   calib_options = c("NORMAL", "GPRIOR", "COMMON"),
  #                   show_output_on_console = FALSE, overwrite = TRUE))
  #
  # (bc <- est_bilog(x = resp, model = "Rasch", target_dir = target_dir,
  #                 calib_options = c("NORMAL", "COMMON"),
  #                 show_output_on_console = FALSE, overwrite = TRUE))
  #
  # (bc <- est_bilog(x = resp, model = "Rasch", target_dir = target_dir,
  #                 calib_options = c("NORMAL", "GPRIOR"),
  #                 prior_ip = list(ALPHA = 10000000,
  #                                 BETA = 30000000),
  #                 show_output_on_console = FALSE, overwrite = TRUE))
  #
  # (bc <- est_bilog(x = resp, model = "Rasch", target_dir = target_dir,
  #                 calib_options = c("NORMAL", "GPRIOR", "COMMON"),
  #                 prior_ip = list(ALPHA = 10000000,
  #                                 BETA = 30000000),
  #                 show_output_on_console = FALSE, overwrite = TRUE))


})



############################################################################@###
############################################################################@###
################### est_bilog - Fixed-Parameter Calibration ####################
############################################################################@###
############################################################################@###

test_that("est_bilog - Fixed-Parameter Calibration", {
  skip_if(skip_tests)
  skip_if_bilog_exe_not_found()

  n_theta <- sample(1000L:2000L, 1)
  n_item <- sample(20L:36L, 1)
  ip <- generate_ip(n = n_item)
  resp <- sim_resp(ip = ip, theta = rnorm(n_theta), prop_missing = .2,
                   output = "response_set")

  fix_pars <- data.frame(item_id = c("Item_1", "Item_9", "Item_3"),
                         a = c(.75, 1.25, 1.75),
                         b = c(-1, 0.25, 0.75),
                         c = c(.15, .25, .35))

  c3 <- est_bilog(x = resp,
                  fix = fix_pars,
                  target_dir = target_dir,
                  overwrite = TRUE,
                  show_output_on_console = FALSE)
  if (c3$converged) { # these tests are valid only if there is convergence
    expect_identical(c3$ip$c["Item_1"], 0.15, ignore_attr = TRUE)
    expect_identical(c3$ip$c["Item_9"], 0.25, ignore_attr = TRUE)
    expect_identical(c3$ip$c["Item_3"], 0.35, ignore_attr = TRUE)
  }

})



############################################################################@###
############################################################################@###
################### est_bilog - Multi-Group Calibration ########################
############################################################################@###
############################################################################@###

test_that("est_bilog - Multi-Group Calibration", {
  skip_if(skip_tests)
  skip_if_bilog_exe_not_found()

  # Multi-group IRT calibration - 3PL
  n_item <- sample(30:40, 1)
  ip <- generate_ip(n = n_item, model = "3PL", D = 1.7)
  n_upper <- sample(2200:4000, 1)
  n_lower <- sample(2900:3800, 1)
  theta_upper <- rnorm(n_upper, 1.5, .25)
  theta_lower <- rnorm(n_lower)
  resp <- sim_resp(ip = ip, theta = c(theta_lower, theta_upper))
  # Create response data where first column group information
  dt <- data.frame(level = c(rep("Lower", n_lower), rep("Upper", n_upper)),
                   resp)
  dt <- dt[sample(1:nrow(dt)), ]

  mg_calib <- est_bilog(x = dt,
                        model = "3PL",
                        group_var = "level",
                        reference_group = "Lower",
                        items = 2:ncol(dt), # Exclude the 'group' column
                        num_of_alternatives = 5,
                        # Use MAP ability estimation.
                        # "FIT": calculate GOF for response patterns
                        scoring_options = c("METHOD=3", "NOPRINT", "FIT"),
                        target_dir = target_dir,
                        overwrite = TRUE,
                        show_output_on_console = FALSE)
  if (mg_calib$converged) { # these tests are valid only if there is convergence
    # Estimated item pool
    expect_s4_class(mg_calib$ip, "Itempool")
    # Print group means
    expect_identical(sort(mg_calib$group_info$name), sort(c("Lower", "Upper")))
    # Check Convergence
    expect_true(mg_calib$converged)
    # Print estimated scores of first five examinees
    expect_true(all(mg_calib$score$ability > -10 & mg_calib$score$ability < 10))
    expect_true(all(mg_calib$score$tried == n_item))

    # Group distribution info
    expect_identical(length(mg_calib$posterior_dist), 2L)
    expect_identical(names(mg_calib$posterior_dist), mg_calib$group_info$name)
    expect_identical(sum(mg_calib$posterior_dist[[1]]$weight), 1,
                     tolerance = 0.01)
    expect_identical(sum(mg_calib$posterior_dist[[2]]$weight), 1,
                     tolerance = 0.01)
    # Check group_info
    expect_identical(mg_calib$group_info$n[mg_calib$group_info$name == "Lower"],
                     n_lower)
    expect_identical(mg_calib$group_info$n[mg_calib$group_info$name == "Upper"],
                     n_upper)
    expect_true(mg_calib$group_info$ref[mg_calib$group_info$name == "Lower"])
  }


  # -------------------------------- #
  # Multi-group IRT calibration - with "overwrite = FALSE"
  # This test depends on previous tests
  mg_calib_2 <- est_bilog(x = dt,
                          model = "3PL",
                          group_var = "level",
                          reference_group = "Lower",
                          items = 2:ncol(dt), # Exclude the 'group' column
                          num_of_alternatives = 5,
                          # Use MAP ability estimation.
                          # "FIT": calculate GOF for response patterns
                          scoring_options = c("METHOD=3", "NOPRINT", "FIT"),
                          target_dir = target_dir,
                          overwrite = FALSE,
                          show_output_on_console = FALSE)

  if (mg_calib$converged) { # these tests are valid only if there is convergence
    # Estimated item pool
    expect_identical(mg_calib$ip, mg_calib_2$ip)
    expect_identical(mg_calib$input[!names(mg_calib$input) %in%
                                      c("overwrite", "call")],
                     mg_calib_2$input[!names(mg_calib_2$input) %in%
                                        c("overwrite", "call")])
    expect_identical(mg_calib$group_info, mg_calib_2$group_info)
    expect_identical(mg_calib$score, mg_calib_2$score)
    expect_identical(mg_calib$posterior_dist, mg_calib_2$posterior_dist)
  }

  # -------------------------------------------------------------------------- #
  # Multi-group IRT calibration - 1PL
  n_item <- sample(30:40, 1)
  ip <- generate_ip(n = n_item, model = "2PL", D = 1.7)
  ip$a <- 1.25
  n_upper <- sample(1200:3000, 1)
  n_lower <- sample(1900:2800, 1)
  theta_upper <- rnorm(n_upper, 1.5, .25)
  theta_lower <- rnorm(n_lower)
  resp <- sim_resp(ip = ip, theta = c(theta_lower, theta_upper),
                   prop_missing = .2)
  # Create response data where first column group information
  dt <- data.frame(level = c(rep("Lower", n_lower), rep("Upper", n_upper)),
                   resp)
  dt <- dt[sample(1:nrow(dt)), ]

  mg_calib <- est_bilog(x = dt,
                        model = "1PL",
                        group_var = "level",
                        reference_group = "Lower",
                        items = 2:ncol(dt), # Exclude the 'group' column
                        num_of_alternatives = 5,
                        # Use MAP ability estimation.
                        # "FIT": calculate GOF for response patterns
                        scoring_options = c("METHOD=3", "NOPRINT", "FIT"),
                        target_dir = target_dir,
                        overwrite = TRUE,
                        show_output_on_console = FALSE)
  if (mg_calib$converged) { # these tests are valid only if there is convergence
    # Estimated item pool
    expect_s4_class(mg_calib$ip, "Itempool")
    # Print group means
    expect_identical(sort(mg_calib$group_info$name), sort(c("Lower", "Upper")))
    # Check Convergence
    expect_true(mg_calib$converged)
    # Print estimated scores of first five examinees
    expect_true(all(mg_calib$score$ability > -10 & mg_calib$score$ability < 10))
    expected_raw <- apply(resp, 1, function(i) sum(!is.na(i)))
    observed_raw <- mg_calib$score[order(match(mg_calib$score$examinee_id,
                                               names(expected_raw))), "tried"]
    expect_true(all(expected_raw == observed_raw))
    # Check group_info
    expect_identical(mg_calib$group_info$n[mg_calib$group_info$name == "Lower"],
                     n_lower)
    expect_identical(mg_calib$group_info$n[mg_calib$group_info$name == "Upper"],
                     n_upper)
    expect_true(mg_calib$group_info$ref[mg_calib$group_info$name == "Lower"])
  }

  # -------------------------------------------------------------------------- #
  # TODO: what happens if there are NA values in group variable

  # -------------------------------------------------------------------------- #
  # TODO: what happens if there are group_var is integers/numeric


  # Multi-group IRT calibration - Rasch
  n_item <- sample(30:40, 1)
  ip <- generate_ip(n = n_item, model = "Rasch", D = 1.7)
  n_upper <- sample(1200:3000, 1)
  n_lower <- sample(1900:2800, 1)
  theta_upper <- rnorm(n_upper, 1.5, .25)
  theta_lower <- rnorm(n_lower)
  resp <- sim_resp(ip = ip, theta = c(theta_lower, theta_upper),
                   prop_missing = .2)
  # Create response data where first column group information
  dt <- data.frame(level = c(rep("Lower", n_lower), rep("Upper", n_upper)),
                   resp)
  dt <- dt[sample(1:nrow(dt)), ]

  mg_calib <- est_bilog(x = dt,
                        model = "Rasch",
                        group_var = "level",
                        reference_group = "Lower",
                        items = 2:ncol(dt), # Exclude the 'group' column
                        num_of_alternatives = 5,
                        # Use MAP ability estimation.
                        # "FIT": calculate GOF for response patterns
                        scoring_options = c("METHOD=3", "NOPRINT", "FIT"),
                        target_dir = target_dir,
                        overwrite = TRUE,
                        show_output_on_console = FALSE)
  if (mg_calib$converged) { # these tests are valid only if there is convergence
    # Estimated item pool
    expect_s4_class(mg_calib$ip, "Itempool")
    # Print group means
    expect_identical(sort(mg_calib$group_info$name), sort(c("Lower", "Upper")))
    # Check Convergence
    expect_true(mg_calib$converged)
    # Print estimated scores of first five examinees
    expect_true(all(mg_calib$score$ability > -10 & mg_calib$score$ability < 10))
    expected_raw <- apply(resp, 1, function(i) sum(!is.na(i)))
    observed_raw <- mg_calib$score[order(match(mg_calib$score$examinee_id,
                                               names(expected_raw))), "tried"]
    expect_true(all(expected_raw == observed_raw))


    # Check group_info
    expect_identical(mg_calib$group_info$n[mg_calib$group_info$name == "Lower"],
                     n_lower)
    expect_identical(mg_calib$group_info$n[mg_calib$group_info$name == "Upper"],
                     n_upper)
    expect_true(mg_calib$group_info$ref[mg_calib$group_info$name == "Lower"])
  }


  # -------------------------------------------------------------------------- #
  # Multi-group IRT calibration - 3PL with prior ability parameters
  n_item <- sample(30:40, 1)
  ip <- generate_ip(n = n_item, model = "3PL", D = 1.7)
  n_upper <- sample(1000:2000, 1)
  n_lower <- sample(2900:3800, 1)
  theta_upper <- rgamma(n_upper, shape = 2, rate = 2)
  # hist(theta_upper)
  theta_lower <- rnorm(n_lower)
  resp <- sim_resp(ip = ip, theta = c(theta_lower, theta_upper))
  # Create response data where first column group information
  dt <- data.frame(level = c(rep("Lower", n_lower), rep("Upper", n_upper)),
                   resp)
  dt <- dt[sample(nrow(dt)), ]
  points <- seq(-4, 4, .1)
  prior_ability = list(
    Lower = list(points = points, weights = dnorm(points)),
    Upper = list(points = points, weights = dgamma(points, 2, 2))
    )
  mg_calib <- est_bilog(x = dt,
                        model = "3PL",
                        group_var = "level",
                        reference_group = "Lower",
                        items = 2:ncol(dt), # Exclude the 'group' column
                        calib_options = c("IDIST = 2"),
                        prior_ability = prior_ability,
                        # Use EAP ability estimation.
                        scoring_options = c("METHOD=2", "NOPRINT", "FIT"),
                        target_dir = target_dir,
                        overwrite = TRUE,
                        show_output_on_console = FALSE)
  if (mg_calib$converged) { # these tests are valid only if there is convergence
    # Estimated item pool
    expect_s4_class(mg_calib$ip, "Itempool")
    # Print group means
    expect_identical(sort(mg_calib$group_info$name), sort(c("Lower", "Upper")))
    # Check Convergence
    expect_true(mg_calib$converged)

    expect_identical(as.integer(unname(rowSums(dt[,-1]))), mg_calib$score$right)
    expect_identical(rownames(dt), mg_calib$score$examinee_id)
    expect_identical(dt$level, mg_calib$score$group)
  }


  # -------------------------------------------------------------------------- #
  # Multi-group IRT calibration - 2PL with Response_set
  n_item <- sample(30:40, 1)
  ip <- generate_ip(n = n_item, model = "2PL", D = 1.7)
  n_upper <- sample(2200:4000, 1)
  n_lower <- sample(2900:3800, 1)
  theta_upper <- rnorm(n_upper, 1.5, .25)
  theta_lower <- rnorm(n_lower)
  resp_set <- generate_resp_set(ip = ip, theta = c(theta_lower, theta_upper))
  resp_set$group <- c(rep("Lower", n_lower), rep("Upper", n_upper))

  mg_calib <- est_bilog(x = resp_set,
                        model = "2PL",
                        group_var = "group",
                        reference_group = "Lower",
                        num_of_alternatives = 5,
                        # Use MAP ability estimation.
                        # "FIT": calculate GOF for response patterns
                        scoring_options = c("METHOD=3", "NOPRINT", "FIT"),
                        target_dir = target_dir,
                        overwrite = TRUE,
                        show_output_on_console = FALSE)
  if (mg_calib$converged) { # these tests are valid only if there is convergence
    # Estimated item pool
    expect_s4_class(mg_calib$ip, "Itempool")
    # Print group means
    expect_identical(sort(mg_calib$group_info$name), sort(c("Lower", "Upper")))
    # Print estimated scores of first five examinees
    expect_true(all(mg_calib$score$ability > -10 & mg_calib$score$ability < 10))
    expect_true(all(mg_calib$score$tried == n_item))

    # Group distribution info
    expect_identical(length(mg_calib$posterior_dist), 2L)
    expect_identical(names(mg_calib$posterior_dist), mg_calib$group_info$name)
    expect_equal(sum(mg_calib$posterior_dist[[1]]$weight), 1, tolerance = 0.01)
    expect_equal(sum(mg_calib$posterior_dist[[2]]$weight), 1, tolerance = 0.01)
    # Check group_info
    expect_identical(mg_calib$group_info$n[mg_calib$group_info$name == "Lower"],
                     n_lower)
    expect_identical(mg_calib$group_info$n[mg_calib$group_info$name == "Upper"],
                     n_upper)
    expect_true(mg_calib$group_info$ref[mg_calib$group_info$name == "Lower"])
  }

})

############################################################################@###
############################################################################@###
################### est_bilog - Common Guessing ################################
############################################################################@###
############################################################################@###

test_that("est_bilog - Common Guessing", {
  skip_if(skip_tests)
  skip_if_bilog_exe_not_found()

  # 1PL with Common Guessing
  true_theta <- rnorm(4000)
  true_ip <- generate_ip(n = 30, model = "3PL")
  resp <- sim_resp(true_ip, true_theta)

  # Run calibration:
  bc1 <- est_bilog(x = resp, model = "1PL", target_dir = target_dir,
                   calib_options = c("NORMAL", "COMMON"),
                   show_output_on_console = FALSE, overwrite = TRUE)

  if (bc1$converged) { # these tests are valid only if there is convergence
    expect_true(all(bc1$ip$a == bc1$ip$a[1]))
    expect_true(all(bc1$ip$c == bc1$ip$c[1]))
    expect_true(all(bc1$ip$model == "3PL"))
  }

  # -------------------------------------------------------------------------- #
  # Rasch with Common Guessing
  bc <- est_bilog(x = resp, model = "Rasch", target_dir = target_dir,
                  calib_options = c("NORMAL", "COMMON"),
                  show_output_on_console = FALSE, overwrite = TRUE)

  if (bc$converged) { # these tests are valid only if there is convergence
    expect_true(all(bc$ip$a == 1))
    expect_true(all(bc$ip$c == bc$ip$c[1]))
    expect_true(all(bc$ip$c > 0))
    expect_true(all(bc$ip$model == "3PL"))
  }

  # -------------------------------------------------------------------------- #
  # 2PL with Common Guessing
  bc2 <- est_bilog(x = resp, model = "2PL", target_dir = target_dir,
                   calib_options = c("NORMAL", "COMMON"),
                   show_output_on_console = FALSE, overwrite = TRUE)

  if (bc2$converged) { # these tests are valid only if there is convergence
    expect_true(all(bc2$ip$c == bc2$ip$c[1]))
    expect_true(all(bc2$ip$model == "3PL"))
  }

  # -------------------------------------------------------------------------- #
  # 3PL with Common Guessing
  bc3 <- est_bilog(x = resp, model = "3PL", target_dir = target_dir,
                   calib_options = c("NORMAL", "COMMON"),
                   show_output_on_console = FALSE, overwrite = TRUE)

  if (bc3$converged) { # these tests are valid only if there is convergence
    expect_true(all(bc3$ip$c == bc3$ip$c[1]))
    expect_true(all(bc3$ip$model == "3PL"))
  }

})



############################################################################@###
############################################################################@###
############################# est_bilog - Response_set #########################
############################################################################@###
############################################################################@###

test_that("est_bilog - Response_set", {
  skip_if(skip_tests)
  skip_if_bilog_exe_not_found()

  # -------------------------------------------------------------------------- #
  # Item responses are in Response_set format
  n_theta <- sample(1000L:2000L, 1)
  n_item <- sample(20L:36L, 1)
  ip <- generate_ip(n = n_item)
  resp <- sim_resp(ip = ip, theta = rnorm(n_theta), prop_missing = .2,
                   output = "response_set")
  c3 <- est_bilog(x = resp,
                  model = "3PL",
                  target_dir = target_dir,
                  overwrite = TRUE,
                  show_output_on_console = FALSE)

  if (c3$converged) { # these tests are valid only if there is convergence
    expect_s4_class(c3$ip, "Itempool")
    expect_true(all(c3$ip$model == "3PL"))
    expect_identical(nrow(c3$score), n_theta)
    expect_identical(c3$score$examinee_id, resp$examinee_id)
    expect_identical(nrow(c3$ctt), n_item)
  }

  # -------------------------------------------------------------------------- #
  # Test Response_set object.
  n_item <- sample(30L:36L, 1)
  n_theta <- sample(1900L:2200L, 1)
  true_ip <- generate_ip(n = n_item, model = "2PL")
  resp_set <- sim_resp(ip = true_ip, theta = rnorm(n_theta), prop_missing = .1,
                       output = "response_set")
  resp_set$examinee_id <- paste0("Subj-", 1:n_theta)
  c1 <- est_bilog(x = resp_set,
                  model = "2PL",
                  target_dir = target_dir, overwrite = TRUE,
                  show_output_on_console = FALSE)

  expect_true(c1$converged)
  expect_s4_class(c1$ip, "Itempool")
  expect_identical(nrow(c1$score), n_theta)
  expect_identical(nrow(c1$ctt), n_item)
  expect_true(all(c1$score$examinee_id == resp_set$examinee_id))

  # This was a bug, when 'overwrite = FALSE', the subject vector
  c2 <- est_bilog(x = resp_set,
                  model = "2PL",
                  target_dir = target_dir, overwrite = FALSE,
                  show_output_on_console = FALSE)
  expect_true(all(c2$score$examinee_id == resp_set$examinee_id))


})

############################################################################@###
############################################################################@###
################### est_bilog - Save RDS #######################################
############################################################################@###
############################################################################@###

test_that("est_bilog - Save RDS", {
  skip_if(skip_tests)
  skip_if_bilog_exe_not_found()

  n_item <- sample(30L:36L, 1)
  n_theta <- sample(900L:1200L, 1)
  true_ip <- generate_ip(n = n_item, model = "1PL")
  resp_set <- sim_resp(ip = true_ip, theta = rnorm(n_theta),
                       output = "response_set")
  c1 <- est_bilog(x = resp_set,
                  model = "1PL",
                  target_dir = target_dir,
                  overwrite = TRUE,
                  show_output_on_console = FALSE)

  # This was a bug, when 'overwrite = FALSE', the subject vector
  c2 <- est_bilog(x = resp_set,
                  model = "1PL",
                  target_dir = target_dir,
                  overwrite = FALSE,
                  show_output_on_console = FALSE)
  expect_identical(c1, c2)

  c3 <- est_bilog(x = resp_set,
                  model = "2PL",
                  target_dir = target_dir,
                  overwrite = FALSE,
                  show_output_on_console = FALSE)
  expect_false(identical(c2, c3))
})




############################################################################@###
############################################################################@###
################### bilog_create_group_info ####################################
############################################################################@###
############################################################################@###

test_that("bilog_create_group_info", {
  skip_if(skip_tests)
  skip_if_bilog_exe_not_found()

  n_item <- sample(30:40, 1)
  ip <- generate_ip(n = n_item, model = "Rasch", D = 1.7)
  n_upper <- sample(1200:3000, 1)
  n_lower <- sample(1900:2800, 1)
  theta_upper <- rnorm(n_upper, 1.5, .25)
  theta_lower <- rnorm(n_lower)
  resp <- sim_resp(ip = ip, theta = c(theta_lower, theta_upper),
                   prop_missing = .2)
  # Create response data where first column group information
  dt <- data.frame(level = c(rep("Lower", n_lower), rep("Upper", n_upper)),
                   resp)

  group_info <- bilog_create_group_info(x = dt, group_var = "level")
  expect_identical(group_info$num_of_groups, 2L)
  expect_identical(length(group_info$group), nrow(dt))
  expect_identical(group_info$group_info$name, unique(dt$level))
  expect_identical(group_info$group_info$code, as.character(1:2))
  expect_identical(group_info$group_info$n[2], sum(dt$level == "Upper"))

  group_info <- bilog_create_group_info(x = dt, group_var = NULL)
  expect_identical(group_info$num_of_groups, 0)
  expect_null(group_info$group)
  expect_null(group_info$group_info)


  group_info <- bilog_create_group_info(x = NULL, group_var = NULL)
  expect_identical(group_info$num_of_groups, 0)
  expect_null(group_info$group)
  expect_null(group_info$group_info)

})


############################################################################@###
############################################################################@###
################### wrap_text ##################################################
############################################################################@###
############################################################################@###

test_that("wrap_text", {

  # -------------------------------------------------------------------------- #
  # tab is NULL, no tab prefix
  text <- "abc"
  expect_equal(wrap_text(text), text)
  # Check 'tab' argument
  tab <- "   "
  expect_equal(wrap_text(text, tab = tab), paste0(tab, text))

  # Check 'width' argument


  paste0(rep("123.4", 20), collapse = ", ")
  nchar(paste0("SLOPE = (", paste0(rep("1.1", 17), collapse = ","), "),"))

})


############################################################################@###
############################################################################@###
################### bilog_read_scores ##########################################
############################################################################@###
############################################################################@###

test_that("bilog_read_scores", {

  # -------------------------------------------------------------------------- #
  # 2PL analysis with two lines of scores
  test_dir <- test_path("data_for_tests", "Bilog", "2021-04-18 2PL")
  score_file <- file.path(test_dir, "SIM_2PL_250-1.SCO")
  data_fn <- file.path(test_dir, "test_data.RDS")
  if (dir.exists(test_dir) && file.exists(score_file) && file.exists(data_fn)) {
    x <- readRDS(data_fn)
    # saveRDS(as.data.frame(x), file.path(test_dir, "test_data.RDS"))
    scores <- bilog_read_scores(score_file = score_file, x = x,
                                examinee_id_var = "examinee_id",
                                group_var = NULL)
    expect_identical(scores$examinee_id, x$examinee_id)
    expect_identical(scores$right, as.integer(rowSums(x[, -1])))
    expect_identical(scores$tried, rep(50L, 250))
    expect_identical(scores$ability[1:5],
                     c(-1.431269, 0.977889, 0.594281, 1.872952, 3.084368))

  }

  # -------------------------------------------------------------------------- #
  # 3PL analysis with two lines of scores
  test_dir <- test_path("data_for_tests", "Bilog", "2021-04-18 3PL")
  score_file <- file.path(test_dir, "SIM_3PL_250-1.SCO")
  data_fn <- file.path(test_dir, "test_data.RDS")
  if (dir.exists(test_dir) && file.exists(score_file) && file.exists(data_fn)) {
    x <- readRDS(data_fn)
    # saveRDS(as.data.frame(x), file.path(test_dir, "test_data.RDS"))
    scores <- bilog_read_scores(score_file = score_file, x = x,
                                examinee_id_var = "examinee_id",
                                group_var = NULL)
    expect_identical(scores$examinee_id, x$examinee_id)
    expect_identical(scores$right, as.integer(rowSums(x[, -1])))
    expect_identical(scores$tried, rep(50L, 250))
    expect_identical(scores$ability[1:5],
                     c(-1.598929, 1.001463, 0.576924, 1.508691, 2.503783))

  }

  # -------------------------------------------------------------------------- #
  # 1PL analysis with one line of scores
  test_dir <- test_path("data_for_tests", "Bilog", "2022-10-24 1PL")
  data_fn <- file.path(test_dir, "test_data.RDS")
  score_file <- file.path(test_dir, "FORM_A_1PL_N250_REP006.SCO")
  if (dir.exists(test_dir) && file.exists(score_file) && file.exists(data_fn)) {
    x <- readRDS(data_fn)
    scores <- bilog_read_scores(score_file = score_file, x = x,
                                examinee_id_var = "examinee_id",
                                group_var = NULL)
    expect_identical(scores$examinee_id, x$examinee_id)
    expect_identical(scores$right, as.integer(rowSums(x[, -1])))
    expect_identical(scores$tried, rep(50L, 250))
    expect_identical(scores$ability[1:5],
                     c(-1.199542, -1.085691, -1.199542, -0.971757, -1.313617))
    examinee_id_var <- "examinee_id"
  }
})



############################################################################@###
############################################################################@###
################### bilog_create_datafile ##################################@###
############################################################################@###
############################################################################@###

# test_that("bilog_create_datafile", {

  # skip_if_bilog_exe_not_found()
  # expect_identical("Uncomment below", "Uncomment below")
  # skip("Skip 'bilog_create_datafile'.")
  # ip <- generate_ip(n = 30, model = "2PL")
  # resp <- sim_resp(ip, rnorm(4000), prop_missing = .2)
  #
  # # Use all of the data in the analysis
  # data_output <- bilog_create_datafile(x = resp)
  #
  # # Only use a subset of items in the analysis.
  # bilog_create_datafile(data = resp, items = paste0("Item-", 1:7))
  #
  # # Response data without column names
  # resp_matrix <- matrix(sample(0:1, 500, T), ncol=10)
  # bilog_create_datafile(data = resp_matrix)
  #
  # # 'items' can be column numbers
  # bilog_create_datafile(data = resp_matrix, items = 1:4)
  #
  #
  # ### Invalid items argument
  # # Duplicated items
  # expect_error(
  #   bilog_create_datafile(data = resp, items = c("Item0", "Item-2","Item-2")),
  #   "Invalid 'items' argument.")
  #
  # # Invalid column names given in 'items' argument
  # expect_error(bilog_create_datafile(data = resp, items = paste0("I-", 1:7)),
  #              "Invalid 'items' argument.")
  #
  # # cat(est_bilog(data = data), sep = "")
  #
  # D <- 1.7
  # ip <- generate_ip(n = 30, model = "2PL", D = D)
  # resp <- sim_resp(ip, rnorm(4000))
  # ip_est <- est_bilog(x = resp, model = "2PL", D = D,
  #                     target_dir = "C:/Users/EGonulates/Desktop/NF2")
  # cbind(a_est = ip_est$a, a = ip$a, a_new = ip_est$a/D)
  # cbind(b_est = ip_est$b, b = ip$b)
  # plot(x = ip$a, y = ip_est$a)
  # abline(0,1)
  #
  #
  # # Multi Group
  # require(irt)
  # D = 1.7
  # n_item <- sample(40:50, 1)
  # ip <- generate_ip(n = n_item, D= D)
  # n_upper <- sample(1200:3000, 1)
  # n_lower <- sample(1900:2800, 1)
  # theta_upper <- rnorm(n_upper, 1.5, .25)
  # theta_lower <- rnorm(n_lower)
  # resp <- sim_resp(ip = ip, theta = c(theta_lower, theta_upper))
  # dt <- data.frame(group = c(rep("Lower", n_lower), rep("Upper", n_upper)),
  #                  resp)
  # head(dt)
  #
  # x = dt
  # model = "3PL"
  # items = colnames(x)[-1]
  # group_var = "group"
  # target_dir = "C:/Users/EGonulates/Desktop/NF2"
  # scoring_method = 3
  # reference_group = "Upper"
  # overwrite = TRUE
  # analysis_name = "bilog_calibration"
  # examinee_id_var = NULL
  # num_of_alternatives = NULL
  # overwrite = TRUE
  # criterion = 0.01
  # common_guessing = FALSE
  # num_of_quadrature = 81
  # max_em_cycles = 100
  # newton = 20
  # normal_ability = TRUE
  # bilog_exe_folder = file.path("C:/Program Files/BILOGMG")
  #
  #
  # data_output <- bilog_create_datafile(
  #   x = x, items = items, examinee_id_var = examinee_id_var,
  #   group_var = group_var,
  #   target_path = file.path(target_dir, paste0(analysis_name, ".dat")),
  #   create_np_key_file = TRUE,
  #   overwrite = TRUE)
  #
  #
  # ip_est <- est_bilog(x = x, model = model, items = items,
  #                     group_var = group_var,
  #                     D = D,
  #                     target_dir = target_dir,
  #                     scoring_method = scoring_method,
  #                     reference_group = reference_group,
  #                     max_em_cycles = 500,
  #                     num_of_quadrature = 81,
  #                     overwrite = overwrite)
  # cbind(a_est = ip_est$ip$a, a = ip$a, a_new = ip_est$ip$a * D)
  #
  # ip_est$ctt$overall
  # ip_est$group_info
# })
