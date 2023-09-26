# library(testthat)

#' Check to see whether the IRTPRO is installed in this machine. If no, skip
#' the tests on this file.
#'
#' @noRd
skip_if_irtpro_exe_not_found <- function() {
  irtpro_exe_paths <- c(file.path("C:/Program Files/IRTPRO 6.0"))
  if (!any(dir.exists(irtpro_exe_paths)))
    skip("IRTPRO is not installed on this compter.")
}


# Set this TRUE to skip all of the tests in this file.
skip_tests <- TRUE

target_dir <- "C:/Temp/testthat-irtpro"


############################################################################@###
############################# irtpro_create_data_file ######################@###
############################################################################@###


test_that("irtpro_create_data_file", {
  skip_on_cran()
  skip_if(skip_tests)
  skip_if_irtpro_exe_not_found()

  # -------------------------------------------------------------------------- #
  n_examinee <- 1000
  models <- sample(c("3PL", "2PL", "GPCM2"), 30, TRUE)
  resp <- sim_resp(generate_ip(model = models),
                   rnorm(n_examinee), prop_missing = .2)
  resp <- cbind.data.frame(examinee_id = paste0("Ex", 1:n_examinee),
                           group = sample(c("A", "B"), n_examinee, TRUE),
                           resp)
  # saveRDS(list(resp = resp, models = models),
  #         file = file.path("irt/tests/testthat/data_for_tests",
  #                          "irtpro_mixed_models_resp_data_001.RDS"))

  output <- irtpro_create_data_file(
    x = resp,
    items = NULL,
    model = NULL,
    examinee_id_var = "examinee_id",
    group_var = "group",
    reference_group = "A",
    target_path = file.path(target_dir, "irtpro_data.ssig"),
    overwrite = TRUE)

  expect_true(file.exists(file.path(target_dir, "irtpro_data.ssig")))
  # x = resp
  # items = NULL
  # model = "3PL"
  # examinee_id_var = "examinee_id"
  # group_var = "group"
  # reference_group = "A"
  # target_path = file.path("C:/temp/irtpro1", "irtpro_data.ssig")
  # irtpro_exe_dir = file.path("C:/Program Files/IRTPRO 6.0")
  # overwrite = TRUE



  # -------------------------------------------------------------------------- #


})



############################################################################@###
############################# est_irtpro ###################################@###
############################################################################@###

test_that("est_irtpro", {
  skip_on_cran()
  skip_if(skip_tests)
  skip_if_irtpro_exe_not_found()

  # -------------------------------------------------------------------------- #
  # Mixed models IRTPRO estimation
  test_fn <- test_path("data_for_tests",
                       "irtpro_mixed_models_resp_data_001.RDS")
  if (file.exists(test_fn)) {
    test_data <- readRDS(test_fn)

    # target_dir <- "C:/Temp/testthat-irtpro"
    analysis_name <- "irtpro_mixed_calib_01"
    D <- 1.702

    output_mixed <- est_irtpro(
      x = test_data$resp,
      items = colnames(test_data$resp)[-c(1:2)],
      model = test_data$models,
      D = D,
      analysis_name = analysis_name,
      examinee_id_var = "examinee_id",
      group_var = "group",
      target_dir = target_dir,
      estimation_method = "BAEM",
      estimation_args = list(`E-Step` = c(500, 1e-005),
                             SE = "S-EM",
                             `M-Step` = c(500, 1e-009),
                             Quadrature = c(49, 6),
                             SEM = 0.001,
                             SS = 1e-005),
      scoring_method = "MAP",
      scoring_args = list(Mean = 0, SD = 1),
      misc_args = list(Decimal = 4, Processors = 1, `Min Exp` = 1),
      print_extra = c("StdRes", "CTLD", "M2", "GOF", "Loadings", "P-Nums",
                      "Diagnostic"),
      constraints = NULL,
      priors = data.frame(
        model = c("1PL", "2PL", "2PL", "3PL", "3PL", "3PL"),
        parameter = c("Intercept[0]", "Slope[0]", "Intercept[0]",
                      "Slope[0]", "Intercept[0]", "Guessing[0]"),
        prior_dist = c("Normal", "Lognormal", "Normal", "Lognormal", "Normal",
                       "Beta"),
        prior_par_1 = c(0, 0, 0, 0, 0, 4),
        prior_par_2 = c(2, 1, 2, 1, 2, 16)
        ),
      overwrite = TRUE)

    expect_true(file.exists(file.path(target_dir,
                                      paste0(analysis_name, ".rds"))))
    # item pool
    expect_equal(output_mixed$ip[[19]]$c, 0.2137, tolerance = 1e-4)
    expect_equal(output_mixed$ip[[26]]$a, 1.1346/D, tolerance = 1e-4)
    expect_equal(output_mixed$ip[[30]]$b, 0.9054, tolerance = 1e-4)
    expect_equal(output_mixed$ip[[12]]$a, 0.9141/D, tolerance = 1e-4)
    expect_equal(output_mixed$ip[[13]]$b, -0.4052, tolerance = 1e-4)
    expect_equal(output_mixed$ip[[9]]$d[2], -0.4615, tolerance = 1e-4)
    expect_equal(output_mixed$ip[[13]]$d[3], 1.7012, tolerance = 1e-4)
    expect_equal(output_mixed$ip[[1]]$D, D, tolerance = 1e-4)

    # Scores
    i <- sample(1:nrow(test_data$resp), 1)
    expect_equal(output_mixed$score$examinee_id[i], test_data$resp$examinee_id[i])
    expect_equal(output_mixed$score$ability[487], -0.682, tolerance = 1e-4)
    expect_equal(output_mixed$score$ability[712], 1.175, tolerance = 1e-4)

    expect_equal(output_mixed$score$se[608], 0.327, tolerance = 1e-4)
    expect_equal(output_mixed$score$se[219], 0.402, tolerance = 1e-4)
  }




  # -------------------------------------------------------------------------- #

})

