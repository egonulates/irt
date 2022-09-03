
#' Check to see whether the flexMIRT is installed in this machine. If no, skip
#' the tests on this file.
#'
#' @noRd
skip_if_flexmirt_exe_not_found <- function() {
  tryCatch(find_flexmirt_exe(flexmirt_exe = NULL),
           error = function(err) skip("Bilog is not installed on this compter.")
    )
}



# Set this TRUE to skip all of the tests in this file.
skip_tests <- TRUE

target_dir <- "C:/Temp/testthat-flexmirt"

############################################################################@###
############################################################################@###
################### flexmirt_create_data #######################################
############################################################################@###
############################################################################@###


test_that("flexmirt_create_data", {
  skip_if(skip_tests)
  skip_if_bilog_exe_not_found()


  # Save 2PL data
  n_item <- sample(30L:36L, 1)
  n_theta <- sample(500L:1200L, 1)
  true_ip <- generate_ip(n = n_item, model = "2PL")
  resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
                                prop_missing = .1)

  resp <- as.matrix(resp_set, output = "score")
  # resp <- sim_resp(ip = true_ip, theta = rnorm(n_theta), prop_missing = .1)


  # x = resp
  # target_dir = target_dir
  # analysis_name = "flexMIRT_calibration"
  # item_ids = NULL
  # model = "3PL"
  # examinee_id_var = NULL
  # group_var = NULL
  # n_categories = NULL
  # group_var = NULL
  # overwrite = FALSE

  analysis_name <- "flexMIRT_calibration_resp"
  # Use response matrix:
  result <- flexmirt_create_data(
    x = resp,
    target_dir = target_dir,
    analysis_name = analysis_name,
    item_ids = NULL,
    model = "3PL",
    examinee_id_var = NULL,
    group_var = NULL,
    overwrite = FALSE)

  expect_equal(names(result), "Group1") # Default group name:
  expect_equal(result$Group1$item_info$item_id, colnames(resp))
  expect_true(all(result$Group1$item_info$model == "3PL"))
  expect_true(all(result$Group1$item_info$model_syntax == "ThreePL"))
  expect_true(all(result$Group1$item_info$num_of_categories == 2))
  expect_equal(result$Group1$data_file_name,
               file.path(target_dir, paste0(analysis_name, ".dat")))
  expect_equal(result$Group1$num_of_examinees, n_theta)
  expect_equal(result$Group1$examinee_id_var, "examinee_id")

  # Use Response_set
  result <- flexmirt_create_data(
    x = resp_set,
    target_dir = target_dir,
    item_ids = NULL,
    model = NULL,  # <- Different than previous test
    examinee_id_var = NULL,
    group_var = NULL,
    overwrite = FALSE)
  expect_equal(names(result), "Group1") # Default group name:
  expect_equal(result$Group1$item_info$item_id, resp_set$item_id)
  expect_true(all(result$Group1$item_info$model == "3PL"))
  expect_true(all(result$Group1$item_info$model_syntax == "ThreePL"))
  expect_true(all(result$Group1$item_info$num_of_categories == 2))
  expect_equal(result$Group1$data_file_name,
               file.path(target_dir, paste0("flexMIRT_calibration", ".dat")))
  expect_equal(result$Group1$num_of_examinees, n_theta)
  expect_equal(result$Group1$examinee_id_var, "examinee_id")
})


############################################################################@###
############################################################################@###
################### flexmirt_create_syntax #####################################
############################################################################@###
############################################################################@###

test_that("flexmirt_create_syntax", {
  skip_if(skip_tests)
  skip_if_bilog_exe_not_found()

  # Save 2PL data
  n_item <- sample(30L:36L, 1)
  n_theta <- sample(500L:1200L, 1)
  true_ip <- generate_ip(n = n_item, model = "2PL")
  resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
                                prop_missing = .1)
  resp <- as.matrix(resp_set, output = "score")

  x = resp
  model = "3PL"
  target_dir = target_dir
  analysis_name = "flexMIRT_calibration"
  item_ids = NULL
  examinee_id_var = NULL
  group_var = NULL
  scoring_method = "EAP" # EAP/MAP/ML/SSC/MI
  flexmirt_exe  = NULL
  overwrite = FALSE

  result <- flexmirt_create_syntax(
    x = resp,
    model = "3PL",
    target_dir = target_dir,
    analysis_name = "flexMIRT_calibration",
    item_ids = NULL,
    examinee_id_var = NULL,
    group_var = NULL,
    scoring_method = "EAP", # EAP/MAP/ML/SSC/MI
    overwrite = FALSE
    )

})



############################################################################@###
############################################################################@###
################### est_flexmirt ###############################################
############################################################################@###
############################################################################@###

test_that("est_flexmirt", {
  skip_if(skip_tests)
  skip_if_bilog_exe_not_found()

  # library(irt)
  # target_dir <- "C:/Temp/testthat-flexmirt"

  ##########################################################################@###
  ################# 2PL Calibration ########################################@###
  ##########################################################################@###
  # Save 2PL data
  n_item <- 30 # sample(30L:36L, 1)
  n_theta <- sample(5000L:12000L, 1)
  true_ip <- generate_ip(n = n_item, model = "2PL", D = 1)
  resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
                                prop_missing = .1)
  resp <- as.matrix(resp_set, output = "score")

  result <- est_flexmirt(
    x = resp,
    model = "2PL",
    target_dir = target_dir,
    analysis_name = "flexMIRT_calibration",
    item_ids = NULL,
    examinee_id_var = NULL,
    group_var = NULL,
    scoring_method = "EAP", # EAP/MAP/ML/SSC/MI
    flexmirt_exe  = NULL,
    # additional_constraints = "Coeff Group1, (Item_1-Item_30), Slope, 1.7;",
    additional_options = "NormalMetric3PL = Yes;",
    overwrite = TRUE
    )

  expect_s4_class(result$ip, "Itempool")
  expect_identical(result$ip$item_id, true_ip$item_id)
  expect_true(cor(result$ip$a, true_ip$a) > .95)
  expect_true(cor(result$ip$b, true_ip$b) > .95)
  lm(formula = estimated ~ true, data = data.frame(true = true_ip$a,
                                                   estimated = result$ip$a))
  eap_est <- est_ability(resp = resp_set, ip = result$ip)
  expect_identical(result$score$theta, unname(eap_est$est), tolerance = 1e-4)
  expect_identical(result$score$se, unname(eap_est$se), tolerance = 1e-4)

  ##########################################################################@###
  ################# 3PL Calibration with D = 1.7 ###########################@###
  ##########################################################################@###
  n_item <- 30 # sample(30L:36L, 1)
  n_theta <- sample(7000L:12000L, 1)
  true_theta <- rnorm(n_theta)
  true_ip <- generate_ip(n = n_item, model = "2PL", D = 1.7)
  resp_set <- generate_resp_set(ip = true_ip, theta = true_theta,
                                prop_missing = .1)
  resp <- as.matrix(resp_set, output = "score")

  result <- est_flexmirt(
    x = resp,
    model = "3PL",
    D = 1.7,
    target_dir = target_dir,
    analysis_name = "flexMIRT_calibration",
    item_ids = NULL,
    examinee_id_var = NULL,
    group_var = NULL,
    scoring_method = "EAP", # EAP/MAP/ML/SSC/MI
    flexmirt_exe  = NULL,
    additional_constraints = c(
      "Prior Group1, (Item_1-Item_30), Slope : logNormal(0, 0.5);",
      "Prior Group1, (Item_1-Item_30), Intercept : Normal(0, 2);",
      "Prior Group1, (Item_1-Item_30), Guessing : Beta(1.0,4.0);"
    ),
    additional_options = "NormalMetric3PL = Yes;",
    overwrite = TRUE
    )

  expect_s4_class(result$ip, "Itempool")
  expect_identical(result$ip$item_id, true_ip$item_id)
  expect_true(cor(result$ip$a, true_ip$a) > .95)
  expect_true(cor(result$ip$b, true_ip$b) > .95)
  temp <- lm(formula = estimated ~ true,
             data = data.frame(true = true_ip$a, estimated = result$ip$a))
  expect_true(abs(1 - temp$coefficients[2]) < 0.01) # D = 1.7
  eap_est <- est_ability(resp = resp_set, ip = result$ip)
  expect_identical(result$score$theta, unname(eap_est$est), tolerance = 1e-3)
  expect_identical(result$score$se, unname(eap_est$se), tolerance = 1e-3)


  ##########################################################################@###
  ################# Mixed 2PL and GPCM #####################################@###
  ##########################################################################@###

  n_item <- 30 # sample(30L:36L, 1)
  n_theta <- sample(5000L:12000L, 1)
  true_ip <- generate_ip(n = n_item,
                         model = sample(c("2PL", "GPCM"), n_item, TRUE),
                         n_categories = sample(3:5, n_item, TRUE), D = 1)
  resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
                                prop_missing = .1)
  resp <- as.matrix(resp_set, output = "score")

  result <- est_flexmirt(
    x = resp,
    model = true_ip$model,
    target_dir = target_dir,
    analysis_name = "flexMIRT_calibration",
    item_ids = NULL,
    examinee_id_var = NULL,
    group_var = NULL,
    scoring_method = "ML", # EAP/MAP/ML/SSC/MI
    flexmirt_exe  = NULL,
    # additional_constraints = "Coeff Group1, (Item_1-Item_30), Slope, 1.7;",
    additional_options = c("MinMLscore = -5;", "MaxMLscore = 5;"),
    overwrite = TRUE
    )

  expect_s4_class(result$ip, "Itempool")
  expect_identical(result$ip$item_id, true_ip$item_id)
  expect_true(cor(result$ip$a, true_ip$a) > .95)
  ability_est <- est_ability(resp = resp_set, ip = result$ip, method = "ml",
                             theta_range = c(-5, 5))
  expect_identical(result$score$theta, unname(ability_est$est),
                   tolerance = 1e-4)
  # There was some discrepancy in SE at extreme values
  # expect_identical(result$score$se, unname(ability_est$se), tolerance = 0.01)
  # tibble::tibble(expected_theta = result$score$theta,
  #                observed_theta = ability_est$theta,
  #                expected_se = result$score$se,
  #                observed_se = ability_est$se) %>%
  #   dplyr::mutate(diff = abs(expected_se - observed_se)) %>%
  #   dplyr::filter(diff > 0.0001)

  ##########################################################################@###
  ################# Mixed 2PL, 3PL and GRM #################################@###
  ##########################################################################@###
  n_item <- 50 # sample(30L:36L, 1)
  n_theta <- sample(5000L:12000L, 1)
  true_ip <- generate_ip(n = n_item,
                         model = sample(c("3PL", "2PL", "GRM"), n_item, TRUE),
                         n_categories = sample(3:5, n_item, TRUE), D = 1)
  resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
                                prop_missing = .1)
  resp <- as.matrix(resp_set, output = "score")

  result <- est_flexmirt(
    x = resp,
    model = true_ip$model,
    target_dir = target_dir,
    analysis_name = "flexMIRT_calibration",
    item_ids = NULL,
    examinee_id_var = NULL,
    group_var = NULL,
    scoring_method = "ML", # EAP/MAP/ML/SSC/MI
    flexmirt_exe  = NULL,
    # additional_constraints = "Coeff Group1, (Item_1-Item_30), Slope, 1.7;",
    additional_options = c("MinMLscore = -5;", "MaxMLscore = 5;"),
    overwrite = TRUE
    )

  expect_s4_class(result$ip, "Itempool")
  expect_identical(result$ip$item_id, true_ip$item_id)
  expect_true(cor(result$ip$a, true_ip$a) > .75)
  # expect_true(cor(result$ip$b, true_ip$b, use = "pairwise.complete.obs") > .75)
  # tibble::tibble(item_id = true_ip$item_id, expected = true_ip$b,
  #                observed = result$ip$b) %>%
  #   dplyr::mutate(diff = abs(expected - observed)) %>%
  #   dplyr::filter(diff > 0.1)
  ability_est <- est_ability(resp = resp_set, ip = result$ip, method = "ml",
                             theta_range = c(-5, 5))
  expect_identical(result$score$theta, unname(ability_est$est),
                   tolerance = 1e-4)



  ##########################################################################@###
  ##########################################################################@###
  ##########################################################################@###

  # lower_fn <- function(data, mapping, ...){
  #   require(ggplot2)
  #   ggplot(data = data, mapping = mapping) +
  #     geom_point(alpha = .4) +
  #     # Add LOESS line
  #     geom_smooth(method = loess, formula = "y ~ x", alpha = .6, color = "red",
  #                 se = FALSE, ...) +
  #     # Add y = x line
  #     geom_abline(intercept = 0, slope = 1, alpha = .6, color = "blue",  ...)
  # }
  # GGally::ggpairs(tibble::tibble(true = true_ip$a, estimated = result$ip$a),
  #                 lower = list(continuous = lower_fn))
  # GGally::ggpairs(tibble::tibble(true = true_ip$b, estimated = result$ip$b),
  #                 lower = list(continuous = lower_fn))

  lm(formula = estimated ~ true, data = tibble::tibble(true = true_ip$a,
                                                       estimated = result$ip$a))
  # GGally::ggpairs(tibble::tibble(true = true_ip$c, estimated = result$ip$c))

  tibble(result$score$theta, )
  GGally::ggpairs(tibble::tibble(true = result$score$theta,
                                 estimated = x$est),
                  lower = list(continuous = lower_fn))



  # 3PL #
  n_item <- 30 # sample(30L:36L, 1)
  n_theta <- sample(9000L:12000L, 1)
  true_ip <- generate_ip(n = n_item, model = "3PL", D = 1.702)
  resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
                                prop_missing = .1)
  resp <- as.matrix(resp_set, output = "score")
  result <- est_flexmirt(
    x = resp,
    model = "3PL",
    target_dir = target_dir,
    analysis_name = "flexMIRT_calibration",
    item_ids = NULL,
    D = 1.702,
    examinee_id_var = NULL,
    group_var = NULL,
    additional_constraints = c(
      "Prior Group1, (Item_1-Item_30), Guessing : Beta(1.0,4.0);" #,
      # "Prior Group1, (Item_1-Item_30), Slope : logNormal(0, 0.5);"
    ),
    additional_options = "NormalMetric3PL = Yes;",
    scoring_method = NULL, # EAP/MAP/ML/SSC/MI
    flexmirt_exe  = NULL,
    overwrite = TRUE
    )


  # Mixed 2PL and 3PL items
  n_item <- sample(30L:36L, 1)
  n_theta <- sample(3500L:7200L, 1)
  true_ip <- generate_ip(n = n_item,
                         model = sample(c("2PL", "3PL"), n_item, TRUE))
  resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
                                prop_missing = .1)
  resp <- as.matrix(resp_set, output = "score")

  result <- est_flexmirt(
    x = resp,
    model = true_ip$model,
    target_dir = target_dir,
    analysis_name = "flexMIRT_calibration",
    item_ids = NULL,
    examinee_id_var = NULL,
    group_var = NULL,
    scoring_method = "EAP", # EAP/MAP/ML/SSC/MI
    flexmirt_exe  = NULL,
    overwrite = TRUE
    )

  # GRM items
  n_item <- sample(30L:36L, 1)
  n_theta <- sample(1500L:3200L, 1)
  true_ip <- generate_ip(n = n_item, model = "GRM",
                         n_categories = sample(3:6, n_item, replace = TRUE))
  resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
                                prop_missing = .1)
  resp <- as.matrix(resp_set, output = "score")

  result <- est_flexmirt(
    x = resp,
    model = "GRM",
    target_dir = target_dir,
    analysis_name = "flexMIRT_calibration",
    item_ids = NULL,
    examinee_id_var = NULL,
    group_var = NULL,
    scoring_method = "EAP", # EAP/MAP/ML/SSC/MI
    flexmirt_exe  = NULL,
    overwrite = TRUE
    )


  # Mixed GRM and 2PL items
  n_item <- sample(30L:36L, 1)
  n_theta <- sample(2500L:5200L, 1)
  true_ip <- generate_ip(n = n_item,
                         model = sample(c("GRM", "2PL"), n_item, TRUE),
                         n_categories = sample(3:6, n_item, replace = TRUE))
  resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
                                prop_missing = .1)
  resp <- as.matrix(resp_set, output = "score")

  result <- est_flexmirt(
    x = resp,
    model = true_ip$model,
    target_dir = target_dir,
    analysis_name = "flexMIRT_calibration",
    item_ids = NULL,
    examinee_id_var = NULL,
    group_var = NULL,
    scoring_method = "EAP", # EAP/MAP/ML/SSC/MI
    flexmirt_exe  = NULL,
    overwrite = TRUE
    )



  # Mixed GRM and 3PL items
  n_item <- sample(30L:36L, 1)
  n_theta <- sample(1500L:3200L, 1)
  true_ip <- generate_ip(n = n_item,
                         model = sample(c("GRM", "3PL"), n_item, TRUE),
                         n_categories = sample(3:6, n_item, replace = TRUE))
  resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
                                prop_missing = .1)
  resp <- as.matrix(resp_set, output = "score")

  result <- est_flexmirt(
    x = resp,
    model = true_ip$model,
    target_dir = target_dir,
    analysis_name = "flexMIRT_calibration",
    item_ids = NULL,
    examinee_id_var = NULL,
    group_var = NULL,
    scoring_method = "EAP", # EAP/MAP/ML/SSC/MI
    flexmirt_exe  = NULL,
    overwrite = TRUE
    )


  # 1PL data
  n_item <- sample(30L:36L, 1)
  n_theta <- sample(500L:1200L, 1)
  true_ip <- generate_ip(n = n_item, model = "1PL")
  resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
                                prop_missing = .1)
  resp <- as.matrix(resp_set, output = "score")


  result <- est_flexmirt(
    x = resp,
    model = "1PL",
    target_dir = target_dir,
    analysis_name = "flexMIRT_calibration",
    item_ids = NULL,
    examinee_id_var = NULL,
    group_var = NULL,
    scoring_method = "EAP", # EAP/MAP/ML/SSC/MI
    flexmirt_exe  = NULL,
    overwrite = TRUE
    )


  # Rasch data
  n_item <- sample(30L:36L, 1)
  n_theta <- sample(1500L:2200L, 1)
  true_ip <- generate_ip(n = n_item, model = "Rasch")
  true_ip$b <- rnorm(n_item, 0.5, 0.5)
  true_theta <- rnorm(n_theta)
  resp_set <- generate_resp_set(ip = true_ip, theta = true_theta,
                                prop_missing = .1)
  resp <- as.matrix(resp_set, output = "score")


  result <- est_flexmirt(
    x = resp,
    model = "Rasch",
    target_dir = target_dir,
    analysis_name = "flexMIRT_calibration",
    item_ids = NULL,
    examinee_id_var = NULL,
    group_var = NULL,
    scoring_method = "EAP", # EAP/MAP/ML/SSC/MI
    flexmirt_exe  = NULL,
    overwrite = TRUE
    )
  # -------------------------------------------------------------------------- #
  # If scoring method is NULL. no scores will be created
  n_item <- sample(30L:36L, 1)
  n_theta <- sample(600L:1200L, 1)
  true_ip <- generate_ip(n = n_item, model = "3PL", D = 1)
  resp_set <- generate_resp_set(ip = true_ip, theta = rnorm(n_theta),
                                prop_missing = .1)
  resp <- as.matrix(resp_set, output = "score")

  result <- est_flexmirt(
    x = resp,
    model = "2PL",
    target_dir = target_dir,
    analysis_name = "flexMIRT_calibration",
    item_ids = NULL,
    examinee_id_var = NULL,
    group_var = NULL,
    scoring_method = NULL, # EAP/MAP/ML/SSC/MI
    flexmirt_exe  = NULL,
    overwrite = TRUE
    )
  expect_null(result$score)



  # mean(result$score$theta)
  # sd(result$score$theta)
  # mean(result$ip$b)
  # sd(result$ip$b)
  #
  # cor(true_ip$b, result$ip$b)
  # cor(true_theta, result$score$theta)
  #
  # library(ggplot2)
  # ggplot(data.frame(true = true_ip$b, estimated = result$ip$b),
  #        aes(x = true, y = estimated))  +
  #   geom_point() +
  #   geom_abline(slope = 1, intercept = 0)
  #
  # ggplot(data.frame(true = true_theta, estimated = result$score$theta),
  #        aes(x = true, y = estimated))  +
  #   geom_point(alpha = .3) +
  #   geom_abline(slope = 1, intercept = 0, color = "red")
})
