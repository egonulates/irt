
# require(tibble)
# library(testthat)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############################# est_ability ######################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# The following test set is for overall functioning of est_ability.
test_that("est_ability", {
  # Check whether the function can also accept a 'tibble' as resp.
  ip <- itempool(b = rnorm(4))
  resp <- tibble::as_tibble(sim_resp(ip = ip, theta = rnorm(10)))
  expect_type(est_ability(ip = ip, resp = resp, method = "ml")$est, "double")

  # -------------------------------------------------------------------------- #
  # Ability estimate output will have the names of the response matrix.
  ip <- itempool(b = rnorm(2))
  theta_names <- paste0("Examinee-", 1:3)
  theta <- rnorm(3)
  names(theta) <- theta_names
  resp <- sim_resp(ip = ip, theta = theta)
  est <- est_ability(ip = ip, resp = resp, method = "ml")
  expect_identical(names(est$est), theta_names)
  expect_identical(names(est$est), theta_names)
  expect_identical(names(est$se), theta_names)

  # -------------------------------------------------------------------------- #
  # Check whether both outputs, se and est, are vectors.
  n_examinee <- sample(10:20, 1)
  ip <- itempool(a = rlnorm(10, 0, .3), b = rnorm(10))
  resp <- sim_resp(ip = ip, theta = rnorm(n_examinee))
  est <- est_ability(ip = ip, resp = resp, method = "ml")
  expect_type(est$est, "double")
  expect_type(est$se, "double")
  expect_true(is.null(dim(est$est)))
  expect_true(is.null(dim(est$se)))
  expect_identical(length(est$est), n_examinee)
  expect_identical(length(est$se), n_examinee)

  # -------------------------------------------------------------------------- #
  # If resp cannot be converted to a Response_set object, an appropriate
  # error should be raised.
  x_long <- data.frame(examinee_id = c("stu1", "stu1", "stu1", "stu2", "stu2"),
                       item_id = c("i1", "i2", "i4", "i1", "i2"),
                       scr = c(0, 1, 0, 1, 0),
                       rwscore = c("A", "D", "B", "C", "D"),
                       resptime = c(33, 55, 22, 66, 31),
                       # These will be passed to misc
                       item_type = c("MC", "MC", "MS", "SA", "MC"),
                       word_count = c(123, 442, 552, 342, 666),
                       # This will be ignored
                       lexile_level = c(1, 4, 3, 2, 1)
                       )
  expect_error(
    est_ability(ip = ip, resp = x_long, method = "ml"),
    paste0("Invalid response pattern. 'resp' cannot be converted to a ",
           "Response_set object."))


  # -------------------------------------------------------------------------- #
  # Standard errors should be different for the same response string but
  # different NA values.
  n_items <- 10 # Number of items
  n_theta <- 6
  ip <- itempool(a = rlnorm(n_items, 0, .25), b = rnorm(n_items))
  resp <- matrix(rep(sim_resp(ip, theta = rnorm(1, 0, .25)), n_theta),
                 nrow = n_theta, byrow = TRUE)
  for (i in 2:n_theta)  # Impose some missingness to data.
    resp[i, 1:(i - 1)] <- NA
  est <- est_ability(ip = ip, resp = resp, method = "ml")
  # Only the first and second row should be the same
  expect_identical(sum(duplicated(est$se)), 0L)
  # The following fails when all correct responses.
  # expect_identical(sum(duplicated(est$est)), 0)
  # # Standard errors should increase because smaller number of items
  # se <- est$se
  # for (i in 3:N) expect_lt(se[i-1], se[i])

  # -------------------------------------------------------------------------- #
  # If response string is all missing, i.e. all NA, then ability estimate and
  # standard error should be NA as well.
  expect_error(est_ability(ip = item(b = rnorm(1)), resp = NA, method = 'owen'),
               # "All scores/raw_responses cannot be NA.",
               )
  # expect_true(is.na(est$est))
  # expect_true(is.na(est$se))
  expect_error(est_ability(ip = item(b = rnorm(1)), resp = NA, method = 'ml'),
               # "All scores/raw_responses cannot be NA."
               )
  # expect_true(is.na(est$est))
  # expect_true(is.na(est$se))
  expect_error(est_ability(ip = itempool(b = rnorm(10)), resp = rep(NA, 10),
                           method = 'ml'),
               # "All scores/raw_responses cannot be NA."
               )
  # expect_true(is.na(est$est))
  # expect_true(is.na(est$se))

  # -------------------------------------------------------------------------- #
  # If ip is not an item or Itempool object, it will be converted to an
  # Itempool object:
  ip <- list(item(b = rnorm(1)), item(b = rnorm(1)))
  resp <- sim_resp(ip = ip, theta = rnorm(10))
  expect_type(est_ability(ip = ip, resp = resp, method = "ml")$est, "double")

  # -------------------------------------------------------------------------- #
  # Function can deal with missing responses without an issue. If there are
  # missing responses they should be ignored.
  # Generate an item pool
  n <- sample(10:20, 1)
  ip <- itempool(a = runif(n, .7, 1.5), b = runif(n, -2, 2))
  ip_list <- ip$item_list
  resp <- sim_resp(ip = ip, theta = rnorm(1, 0, .4))
  # Randomly drop two items and create an item pool
  selected_items <- sample(1:n, 2)
  ip_small <- itempool(ip_list[-selected_items])
  resp_small <- resp[, -selected_items]
  resp_na <- resp
  resp_na[, selected_items] <- NA
  # Test for ML
  est_small <- est_ability(ip = ip_small, resp = resp_small, method = 'ml')
  est_na <- est_ability(ip = ip, resp = resp_na, method = 'ml')
  # TODO: there shouldn't be a need for "tolerance = 1e-6"
  expect_identical(est_small$est, est_na$est, tolerance = 1e-5,
                   ignore_attr = TRUE)
  expect_identical(est_small$se, est_na$se, tolerance = 1e-5,
                   ignore_attr = TRUE)
  # Test for EAP
  est_small <- est_ability(ip = ip_small, resp = resp_small, method = 'eap')
  est_na <- est_ability(ip = ip, resp = resp_na, method = 'eap')
  expect_identical(est_small$est, est_na$est, tolerance = 1e-5,
                   ignore_attr = TRUE)
  expect_identical(est_small$se, est_na$se, tolerance = 1e-5,
                   ignore_attr = TRUE)
  # Test for Owen
  est_small <- est_ability(ip = ip_small, resp = resp_small, method = 'owen')
  est_na <- est_ability(ip = ip, resp = resp_na, method = 'owen')
  expect_identical(est_small$est, est_na$est, tolerance = 1e-5,
                   ignore_attr = TRUE)
  expect_identical(est_small$se, est_na$se, tolerance = 1e-5,
                   ignore_attr = TRUE)

  ## Missing response to one item should return NA ##
  expect_error(est_ability(ip = generate_item(), resp = NA, method = 'ml'),
               # "All scores/raw_responses cannot be NA.",
               )
  # expect_true(is.na(est$est))
  # expect_true(is.na(est$se))
  expect_error(est_ability(ip = generate_item(), resp = NA, method = 'eap'),
               # "All scores/raw_responses cannot be NA.",
               )
  # expect_true(is.na(est$est))
  # expect_true(is.na(est$se))
  expect_error(est_ability(ip = generate_item(), resp = NA, method = 'owen'),
               # "All scores/raw_responses cannot be NA.",
               )
  # expect_true(is.na(est$est))
  # expect_true(is.na(est$se))


  ## All Missing responses should return NA ##
  n <- sample(10:20, 1)
  ip <- generate_ip(n = n, model = "2PL")
  resp <- rep(NA, n)
  expect_error(est_ability(ip = ip, resp = resp, method = 'ml'),
               # "All scores/raw_responses cannot be NA.",
               )
  # expect_true(is.na(est$est))
  # expect_true(is.na(est$se))
  expect_error(est_ability(ip = ip, resp = resp, method = 'eap'),
               # "All scores/raw_responses cannot be NA."
               )
  # expect_true(is.na(est$est))
  # expect_true(is.na(est$se))
  expect_error(est_ability(ip = ip, resp = resp, method = 'owen'),
               # "All scores/raw_responses cannot be NA."
               )
  # expect_true(is.na(est$est))
  # expect_true(is.na(est$se))

  # -------------------------------------------------------------------------- #
  # If a response string is all NA's, the 'est' and 'se' should be NA as well.
  n_items <- 5
  n_examinee <- 4
  ip <- itempool(a = runif(n_items, .7, 1.5), b = runif(n_items, -2, 2))
  resp <- sim_resp(ip = ip, theta = rnorm(n_examinee, 0, .4))
  resp[1, ] <- 0
  resp[2, ] <- NA
  resp[3, sample(1:n_items, 2)] <- NA

  # Test for all estimation methods
  # TODO: Create a more user friendly error message for the following error
  expect_error(est_ability(ip = ip, resp = resp, method = "ml"))
  expect_error(est_ability(ip = ip, resp = resp, method = "owen"))
  expect_error(est_ability(ip = ip, resp = resp, method = "map"))
  # for EAP function returns NA
  observed <- est_ability(ip = ip, resp = resp, method = "eap")
  expect_true(is.na(observed$est[2]))
  expect_true(is.na(observed$se[2]))

  # When calculating 'sum_score', if all responses are NA, the sum score is 0.
  expected <- est_ability(ip = ip, resp = resp, method = "sum_score")
  expect_identical(expected$est[2], c(S2 = 0))


  # -------------------------------------------------------------------------- #
  ## "output" argument tests
  ip <- generate_ip(model = "3PL")
  n_examinee <- sample(5:15, 1)
  examinee_ids <- paste0("Exm-", sample(11:99, n_examinee))
  true_theta <- setNames(round(rnorm(n_examinee), 3), examinee_ids)
  resp_set <- generate_resp_set(ip = ip, theta = true_theta, prop_missing = .2)
  resp_matrix <- as.matrix(resp_set)
  for (m in c("eap", "ml", "owen", "sum_score", "map")) {
    # "list"
    observed <- est_ability(resp = resp_set, ip = ip, method = m,
                            output_type = "list")
    expect_type(observed, "list")
    expect_equal(names(observed), c("est", "se"))
    expect_equal(names(observed$est), examinee_ids)
    expect_equal(names(observed$se), examinee_ids)
    expect_type(observed$se, "double")

    # "data.frame"
    observed <- est_ability(resp = resp_set, ip = ip, method = m,
                            output_type = "data.frame")
    expect_s3_class(observed, "data.frame")
    expect_equal(names(observed), c("examinee_id", "est", "se"))
    expect_equal(observed$examinee_id, examinee_ids)
    expect_type(observed$se, "double")

    # "tibble"
    observed <- est_ability(resp = resp_set, ip = ip, method = m,
                            output_type = "tibble")
    expect_s3_class(observed, "tbl_df")
    expect_equal(names(observed), c("examinee_id", "est", "se"))
    expect_equal(observed$examinee_id, examinee_ids)
    expect_type(observed$se, "double")

    # "list" - resp_matrix
    observed <- est_ability(resp = resp_matrix, ip = ip, method = m,
                            output_type = "list")
    expect_type(observed, "list")
    expect_equal(names(observed), c("est", "se"))
    expect_equal(names(observed$est), examinee_ids)
    expect_equal(names(observed$se), examinee_ids)
    expect_type(observed$se, "double")

    # "data.frame"
    observed <- est_ability(resp = resp_matrix, ip = ip, method = m,
                            output_type = "data.frame")
    expect_s3_class(observed, "data.frame")
    expect_equal(names(observed), c("examinee_id", "est", "se"))
    expect_equal(observed$examinee_id, examinee_ids)
    expect_type(observed$se, "double")

    # "tibble"
    observed <- est_ability(resp = resp_matrix, ip = ip, method = m,
                            output_type = "tibble")
    expect_s3_class(observed, "tbl_df")
    expect_equal(names(observed), c("examinee_id", "est", "se"))
    expect_equal(observed$examinee_id, examinee_ids)
    expect_type(observed$se, "double")
  }



  ### Mixture of Models ###
  # -------------------------------------------------------------------------- #
  # Function can deal with the ability estimation of mixture of item models
  # Here the item pool consist of two 2PL, three GRM and two GPCM items.
  # This test checks whether the log likelihood is calculated correctly.
  D = 1.702
  ip = itempool(data.frame(a = c(0.983, 1.03), b = c(-0.581, 1.643)),
                   D = D, model = '2PL', item_id = paste0('irt', 1:2))
  # GRM
  item1 = item(a = 1.027, b = c(-2.439, -0.21, 0.693), D = D, model = 'GRM')
  item2 = item(a = 1.024, b = c(0.241, 0.257, 0.311), D = D, model = 'GRM')
  item3 = item(a = 0.896, b = c(-0.225, 0.458, 0.764), D = D, model = 'GRM')
  ip = c(ip, itempool(c(item1, item2, item3), item_id = paste0('grm', 1:3)))
  item1 = item(a = 0.927, b = c(2.086, 0.107), D = D, model = 'GPCM')
  item2 = item(a = 1.201, b = c(-0.349, 1.162), D = D, model = 'GPCM')
  ip = c(ip, itempool(c(item1, item2), item_id = paste0('gpcm', 1:2)))
  resp = c(0, 0, 3, 2, 1, 0, 2)
  est <- est_ability(ip = ip, resp = resp, method = 'ml')
  expect_false(is.na(est$est))
  expect_false(is.na(est$se))
  # Sum score calculated correctly
  expect_identical(est_ability(ip = ip, resp = resp, method = 'sum_score')$est,
                   sum(resp, na.rm = TRUE), ignore_attr = TRUE)

  # -------------------------------------------------------------------------- #
  # Owen's ability estimation cannot be used models other than dichotomous items
  expect_error(est_ability(ip = ip, resp = resp, method = 'owen'),
               regexp = paste0("Owen's Bayesian ability estimation method can ",
                               "only be used for dichotomous IRT models"))

  # -------------------------------------------------------------------------- #
  # Ability Estimation with mixed model items
  i1 <- item(b = rnorm(1), item_id = "i1")
  i2 <- item(a = rlnorm(1, 0, .3), b = rnorm(1), c = .2, item_id = "i2")
  i3 <- item(a = rlnorm(1, 0, .3), b = sort(runif(3)), item_id = "i3")
  ip <- c(i1, i2, i3)
  n_examinee <- 4
  resp <- sim_resp(ip = ip, theta = rnorm(n_examinee))
  observed <- est_ability(ip = ip, resp = resp, method = "ml")
  observed <- est_ability(ip = ip, resp = resp, method = "eap")

  # -------------------------------------------------------------------------- #
  # Ability Estimation with the testlets
  t1 <- testlet(itempool(b = rnorm(2), item_id = c("t1-i1", "t1-i2")),
                   testlet_id = "t1")
  t2 <- testlet(itempool(a = rlnorm(3, 0, .3), b = rnorm(3),
                                item_id = c("t2-i1", "t2-i2", "t2-i3")),
                testlet_id = "t2")
  i1 <- item(b = rnorm(1), item_id = "i1")
  i2 <- item(a = rlnorm(1, 0, .3), b = rnorm(1), c = .2, item_id = "i2")
  i3 <- item(a = rlnorm(1, 0, .3), b = sort(runif(3)), item_id = "i3")
  ip_with_testlet <- c(t1, t2, i1, i2, i3)
  ip_wo_testlet <- c(t1@item_list, t2@item_list, i1, i2, i3)
  n_examinee <- 4
  resp <- sim_resp(ip = ip_with_testlet, theta = rnorm(n_examinee))
  # ML
  observed <- est_ability(ip = ip_with_testlet, resp = resp, method = "ml")
  expected <- est_ability(ip = ip_wo_testlet, resp = resp, method = "ml")
  expect_identical(expected, observed, tolerance = 1e-8)
  # # EAP
  # observed <- est_ability(ip = ip, resp = resp, method = "eap")
  # expected <- est_ability(ip = ip1, resp = resp, method = "eap")
  # expect_identical(expected, observed)

  # -------------------------------------------------------------------------- #
  # The estimate numbers are rounded to the tolerance level.
  n_items <- sample(5:10, 1)
  ip <- itempool(b = rnorm(n_items))
  resp <- rep(1, times = n_items)
  est <- est_ability(ip = ip, resp = resp, method = "ml",
                     tolerance = 10e-13)$est
  for (tol in 10^(-seq(1, 7, 1))) {
    est1 <- est_ability(ip = ip, resp = resp, method = "ml",
                        tolerance = tol)$est
    expect_identical(est, est1, tolerance = tol)
    # cat(tol, est, est1, "\n", sep = "\t")
  }
  resp <- sim_resp(ip = ip, theta = rnorm(1))
  est <- est_ability(ip = ip, resp = resp, method = "ml",
                     tolerance = 10e-13)$est
  for (tol in 10^(-seq(1, 7, 1))) {
    est1 <- est_ability(ip = ip, resp = resp, method = "ml",
                        tolerance = tol)$est
    expect_identical(est, est1, tolerance = tol)
    # cat(tol, est, est1, "\n", sep = "\t")
  }

  # -------------------------------------------------------------------------- #
  # The name of the method can be capitalized in any way
  n_items <- sample(5:10, 1)
  ip <- itempool(b = rnorm(n_items))
  resp <- sim_resp(ip = ip, theta = rnorm(2))
  est <- est_ability(ip = ip, resp = resp, method = "ml")$est
  expect_identical(est, est_ability(ip = ip, resp = resp, method = "ML")$est)
  expect_identical(est, est_ability(ip = ip, resp = resp, method = "mL")$est)
  est <- est_ability(ip = ip, resp = resp, method = "eap")$est
  expect_identical(est, est_ability(ip = ip, resp = resp, method = "EAP")$est)
  expect_identical(est, est_ability(ip = ip, resp = resp, method = "EaP")$est)

  # -------------------------------------------------------------------------- #
  # The name of the prior_dist can be capitalized in any way
  est <- est_ability(ip = ip, resp = resp, method = "eap",
                     prior_dist = "norm")$est
  expect_identical(est, est_ability(ip = ip, resp = resp, method = "eap",
                     prior_dist = "NORM")$est)
  expect_identical(est, est_ability(ip = ip, resp = resp, method = "eap",
                     prior_dist = "norM")$est)

  # -------------------------------------------------------------------------- #
  # Check whether a Response object is accepted in est_ability() function
  ip <- generate_ip()
  resp <- sim_resp(ip = ip, theta = rnorm(1), output = "response_set")[[1]]
  expect_true(is.list(est_ability(resp = resp, ip = ip)))

  # -------------------------------------------------------------------------- #
  # All items in the response matrix should be also in the item pool object.
  ip <- generate_ip()
  resp_set <- generate_resp_set(ip = ip, theta = rnorm(10))
  resp <- sim_resp(ip = ip, theta = rnorm(10))
  ip_short <- ip[-1]
  expect_error(est_ability(resp = resp_set, ip = ip_short, method = "ml"),
               paste0("Invalid 'ip'. All of the items in the response data ",
                      "should be in the item pool, ip."))
  expect_error(est_ability(resp = resp, ip = ip_short, method = "ml"),
               paste0("Invalid 'ip'. All of the items in the response data ",
                      "should be in the item pool, ip."))


  # -------------------------------------------------------------------------- #
  # Response_set, matrix and data.frame all works the same
  ip <- generate_ip()
  true_theta <- rnorm(20)
  resp <- sim_resp(ip = ip, theta = true_theta, prop_missing = .3)
  resp_df <- as.data.frame(resp)
  resp_set <- response_set(resp, ip = ip)
  expect_identical(resp, as.matrix(resp_set))

  observed_resp <- est_ability(ip = ip, resp = resp, method = "eap")
  observed_resp_set <- est_ability(ip = ip, resp = resp_set, method = "eap")
  observed_resp_df <- est_ability(ip = ip, resp = resp_df, method = "eap")
  expect_equal(observed_resp, observed_resp_df)
  expect_equal(observed_resp, observed_resp_set)

  observed_resp <- est_ability(ip = ip, resp = resp, method = "ml")
  observed_resp_set <- est_ability(ip = ip, resp = resp_set, method = "ml")
  observed_resp_df <- est_ability(ip = ip, resp = resp_df, method = "ml")
  expect_equal(observed_resp, observed_resp_df)
  expect_equal(observed_resp, observed_resp_set)

  observed_resp <- est_ability(ip = ip, resp = resp, method = "sum_score")
  observed_resp_set <- est_ability(ip = ip, resp = resp_set,
                                   method = "sum_score")
  observed_resp_df <- est_ability(ip = ip, resp = resp_df, method = "sum_score")
  expect_equal(observed_resp, observed_resp_df)
  expect_equal(observed_resp, observed_resp_set)

  observed_resp <- est_ability(ip = ip, resp = resp, method = "owen")
  observed_resp_set <- est_ability(ip = ip, resp = resp_set, method = "owen")
  observed_resp_df <- est_ability(ip = ip, resp = resp_df, method = "owen")
  expect_equal(observed_resp, observed_resp_df)
  expect_equal(observed_resp, observed_resp_set)

  observed_resp <- est_ability(ip = ip, resp = resp, method = "map")
  observed_resp_set <- est_ability(ip = ip, resp = resp_set, method = "map")
  observed_resp_df <- est_ability(ip = ip, resp = resp_df, method = "map")
  expect_equal(observed_resp, observed_resp_df)
  expect_equal(observed_resp, observed_resp_set)

  # -------------------------------------------------------------------------- #
  # Ability Estimation with the testlets
  ip_testlet <- c(generate_testlet(n = 4, item_id_preamble = "t1"),
                  generate_testlet(n = 2, item_id_preamble = "t2"),
                  generate_testlet(n = 3, item_id_preamble = "t3"),
                  generate_ip(n = 5))
  ip_standalone <- itempool(irt:::flatten_itempool_cpp(ip_testlet))
  true_theta <- rnorm(20)
  resp_set_testlet <- generate_resp_set(ip = ip_testlet, theta = true_theta,
                                        prop_missing = .2)
  resp <- as.matrix(resp_set_testlet)
  resp_set_standalone <- response_set(x = resp, data_format = "wide",
                                      ip = ip_standalone)
  observed_testlet <- est_ability(resp = resp_set_testlet, ip = ip_testlet,
                                  method = "eap")
  observed_standalone <- est_ability(resp = resp_set_standalone,
                                     ip = ip_standalone, method = "eap")
  observed_resp <- est_ability(resp = resp, ip = ip_standalone, method = "eap")

  expect_identical(observed_standalone, observed_testlet)
  expect_identical(observed_resp, observed_testlet, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  # Ability Estimation with the testlets
  ip <- c(generate_testlet(n = sample(2:6, 1), item_id_preamble = "t1"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t2"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t3"),
          generate_ip(n = sample(6:12, 1)))
  ip_standalone <- itempool(irt:::flatten_itempool_cpp(ip))
  true_theta <- rnorm(20)
  resp <- sim_resp(ip = ip, theta = true_theta, prop_missing = .3)
  resp_set <- response_set(resp, ip = ip)
  resp_set_sa <- response_set(resp, ip = ip_standalone)
  expect_identical(resp, as.matrix(resp_set))
  expect_identical(resp, as.matrix(resp_set_sa))

  # Test EAP
  observed_resp <- est_ability(ip = ip, resp = resp, method = "eap")
  observed_resp_set <- est_ability(ip = ip, resp = resp_set, method = "eap")
  observed_resp_sa <- est_ability(ip = ip_standalone, resp = resp,
                                  method = "eap")
  observed_resp_sa_2 <- est_ability(ip = ip_standalone, resp = resp_set_sa,
                                    method = "eap")
  expect_equal(observed_resp, observed_resp_sa)
  expect_equal(observed_resp, observed_resp_set)
  expect_equal(observed_resp_set, observed_resp_sa)
  expect_equal(observed_resp_sa_2, observed_resp_sa)
  expect_equal(observed_resp_set, observed_resp_sa_2)


  observed_resp <- irt:::est_ability_eap_cpp(
    resp = resp, ip = ip, theta_range = c(-5, 5), no_of_quadrature = 41,
    prior_dist = "norm", prior_par = c(0, 1))
  observed_resp_set <- irt:::est_ability_eap_response_set_cpp(
    resp_set = resp_set, ip = ip, theta_range = c(-5, 5), no_of_quadrature = 41,
    prior_dist = "norm", prior_par = c(0, 1))
  expect_equal(observed_resp, observed_resp_set)

  # Compare a single Response and response vector
  i <- sample(nrow(resp), 1)
  observed_resp_vec <- irt:::est_ability_eap_single_examinee_cpp(
    resp = resp[i, ], ip = ip, theta_range = c(-5, 5), no_of_quadrature = 41,
    prior_dist = "norm", prior_par = c(0, 1))
  observed_response <- irt:::est_ability_eap_response_cpp(
    resp = resp_set[[i]], ip = ip, theta_range = c(-5, 5), no_of_quadrature = 41,
    prior_dist = "norm", prior_par = c(0, 1))
  expect_equal(observed_resp_vec, observed_response)



  # Test ML
  observed_resp <- est_ability(ip = ip, resp = resp, method = "ml")
  observed_resp_sa <- est_ability(ip = ip_standalone, resp = resp,
                                  method = "ml")
  observed_resp_set <- est_ability(ip = ip, resp = resp_set, method = "ml")
  observed_resp_sa_2 <- est_ability(ip = ip_standalone, resp = resp_set_sa,
                                    method = "ml")
  expect_equal(observed_resp, observed_resp_sa, tolerance = 1e-6)
  expect_equal(observed_resp, observed_resp_set)
  expect_equal(observed_resp_set, observed_resp_sa, tolerance = 1e-6)
  expect_equal(observed_resp_sa_2, observed_resp_sa)
  expect_equal(observed_resp_set, observed_resp_sa_2, tolerance = 1e-6)

  # Test Sum Score
  observed_resp <- est_ability(ip = ip, resp = resp, method = "sum_score")
  observed_resp_set <- est_ability(ip = ip, resp = resp_set, method = "sum_score")
  observed_resp_sa <- est_ability(ip = ip_standalone, resp = resp,
                                  method = "sum_score")
  observed_resp_sa_2 <- est_ability(ip = ip_standalone, resp = resp_set_sa,
                                    method = "sum_score")
  expect_equal(observed_resp, observed_resp_sa)
  expect_equal(observed_resp, observed_resp_set)
  expect_equal(observed_resp_set, observed_resp_sa)
  expect_equal(observed_resp_sa_2, observed_resp_sa)
  expect_equal(observed_resp_set, observed_resp_sa_2)

  # Test Owen's Bayesian scoring
  observed_resp <- est_ability(ip = ip, resp = resp, method = "owen")
  observed_resp_set <- est_ability(ip = ip, resp = resp_set, method = "owen")
  observed_resp_sa <- est_ability(ip = ip_standalone, resp = resp,
                                  method = "owen")
  observed_resp_sa_2 <- est_ability(ip = ip_standalone, resp = resp_set_sa,
                                    method = "owen")
  expect_equal(observed_resp, observed_resp_sa)
  expect_equal(observed_resp, observed_resp_set)
  expect_equal(observed_resp_set, observed_resp_sa)
  expect_equal(observed_resp_sa_2, observed_resp_sa)
  expect_equal(observed_resp_set, observed_resp_sa_2)

  # Test MAP
  observed_resp <- est_ability(ip = ip, resp = resp, method = "map")
  observed_resp_sa <- est_ability(ip = ip_standalone, resp = resp,
                                  method = "map")
  observed_resp_set <- est_ability(ip = ip, resp = resp_set, method = "map")
  observed_resp_sa_2 <- est_ability(ip = ip_standalone, resp = resp_set_sa,
                                    method = "map")
  expect_equal(observed_resp, observed_resp_sa)
  expect_equal(observed_resp, observed_resp_set)
  expect_equal(observed_resp_set, observed_resp_sa)
  expect_equal(observed_resp_sa_2, observed_resp_sa)
  expect_equal(observed_resp_set, observed_resp_sa_2)


})


###############################################################################@
############################# est_ability - sum_score ##########################
###############################################################################@

test_that("est_ability - sum_score", {

  # Sum score calculated correctly
  n <- sample(10:20, 1)
  ip <- itempool(a = runif(n, .7, 1.5), b = runif(n, -2, 2))
  resp <- sim_resp(ip = ip, theta = rnorm(8, 0, .4))
  resp[sample(1:prod(dim(resp)), 9)] <- NA
  observed <- est_ability(ip = ip, resp = resp, method = 'sum_score')
  expected <- rowSums(resp, na.rm = TRUE)
  expect_identical(observed$est, expected)
  expect_true(all(is.na(observed$se)))

  # -------------------------------------------------------------------------- #
  # Sum score do not require an item pool
  n <- sample(10:20, 1)
  ip <- generate_ip(n = n, model = sample(c("3PL", "GRM"), n, replace = TRUE))
  resp_set <- generate_resp_set(ip = ip, theta = rnorm(20), prop_missing = .2)
  resp <- as.matrix(resp_set)
  observed <- est_ability(resp = resp, method = 'sum_score')
  expected <- rowSums(resp, na.rm = TRUE)
  expect_identical(observed$est, expected)
  expect_true(all(is.na(observed$se)))

})

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############################# est_ability_owen #################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_that("est_ability_owen (Item)", {
  ##  Single theta
  est <- est_ability(ip = item(b = -1.480974, D = 1.611941), resp = 1,
                     method = 'owen', prior_pars = c(1, sqrt(.5)))
  expect_identical(est$est, 1.045586, ignore_attr = FALSE, tolerance = 1e-5)
  expect_identical(est$se, sqrt(0.4796707), ignore_attr = FALSE,
                   tolerance = 1e-5)

  est <- est_ability(ip = item(b = -0.6397636, D = 1.166008), resp = 1,
                     method = 'owen', prior_pars = c(1, sqrt(.5)))
  expect_identical(est$est, 1.080177, ignore_attr = FALSE, tolerance = 1e-5)
  expect_identical(est$se, sqrt(0.458222), ignore_attr = FALSE,
               tolerance = 1e-5)

  # -------------------------------------------------------------------------- #
  # Another single item example
  m0 <- -1.055991
  v0 <- 1.424225
  item <- item(a = 1.4792, b = 0.8699, c = 0.1767892, D = 1)
  est <- est_ability(ip = item, resp = 0, method = 'owen',
                     prior_pars = c(m0, sqrt(v0)))
  expect_identical(est$est, -1.224033, tolerance = 1e-5, ignore_attr = TRUE)
  expect_identical(est$se, sqrt(1.150978), tolerance = 1e-5, ignore_attr = TRUE)

  # -------------------------------------------------------------------------- #
  ##  Multiple theta raises error
  ip <- item(b = rnorm(1))
  resp <- sim_resp(ip = ip, theta = rnorm(10))
  expect_error(
    est_ability(ip = ip, resp = resp, method = 'owen'),
    regexp = paste0("'resp' and 'ip' do not correspond."))
})

############################# est_ability_owen (Itempool) ######################
test_that("est_ability_owen (Itempool)", {
  # Single Item
  est <- est_ability(ip = itempool(a = 0.6129655, b = -0.660839, c = 0.01146235,
                                      D = 2.960664), resp = 1,
                     method = 'owen', prior_pars = c(1, sqrt(.5)))
  expect_identical(est$est, 1.059813, ignore_attr = FALSE, tolerance = 1e-5)
  expect_identical(est$se, sqrt(0.4943381), ignore_attr = FALSE,
               tolerance = 1e-5)

  # -------------------------------------------------------------------------- #
  # One item and multiple theta raises error when resp is not a matrix.
  ip <- itempool(b = rnorm(1))
  resp <- as.vector(sim_resp(ip = ip, theta = rnorm(10)))
  expect_error(est_ability(ip = ip, resp = resp, method = 'owen'),
               regexp = paste0("'resp' and 'ip' do not correspond."))
  # If resp is a matrix, no error will be raised
  expect_identical(length(est_ability(ip = ip, resp = matrix(resp, ncol = 1),
                                  method = 'owen')$est), 10L)

  # -------------------------------------------------------------------------- #
  # Multiple Items
  ip <- itempool(
    a = c(1.34730418561958, 0.838341692695394, 0.670091423671693,
          0.910351292463019, 0.841347561450675, 1.19060949212871,
          1.00658598938026),
    b = c(1.90739340746829, -0.108147490438633, -0.797692163077999,
          -0.0608525829373425, -1.4585962922829, 0.864645431892862,
          -0.26818135533447),
    c = c(0.152733101486228, 0.168994629196823, 0.160417800606228,
          0.0402568220626563, 0.0588782917242497, 0.174779037642293,
          0.274252452305518),
    D = 1.715379, item_id = paste0("Item-",1:7), content = rep("Algebra", 7))
  resp <- c(0, 1, 1, 1, 1, 1, 1)
  est <- est_ability(ip = ip, resp = resp, method = 'owen',
                     prior_pars = c(1, sqrt(.5)))
  expect_identical(est$est, 1.298185, ignore_attr = FALSE, tolerance = 1e-5)
  expect_identical(est$se, sqrt(0.3571816), ignore_attr = FALSE,
                   tolerance = 1e-5)

  # -------------------------------------------------------------------------- #
  # One can either use an informative prior obtained by previous responses
  # 1 to n-1 or all item responses from 1 to n.
  m0 <- rnorm(1)
  v0 <- runif(1, .5, 2)
  ip <- generate_ip(n = 10)
  resp <- sim_resp(ip = ip, theta = rnorm(1))[1,]
  temp_est <- est_ability(ip = ip, resp = resp, method = "owen",
                          prior_pars = c(m0, sqrt(v0)))
  # add a new item and response
  item <- generate_item()
  resp_new <- sample(0:1, 1)
  # Two types of estimae: all items vs informative prior. Both should be the
  # same.
  est1 <- est_ability(ip = item, resp = resp_new, method = "owen",
                      prior_pars = c(temp_est$est, sqrt(temp_est$se^2)))
  est2 <- est_ability(ip = c(ip, item), resp = c(resp, resp_new),
                      method = "owen", prior_pars = c(m0, sqrt(v0)))
  expect_identical(est1$est, est2$est, tolerance = 1e-5)
  expect_identical(est1$se, est2$se, tolerance = 1e-5)

  # -------------------------------------------------------------------------- #
  # Another example for one item
  m0 <- 0.531
  v0 <- 1.376
  ip <- new("3PL", a = 0.8581, b = 0.53, c = 0.1084, D = 1, se_a = NULL,
            se_b = NULL, se_c = NULL, item_id = NULL, content = NULL,
            misc = NULL)
  resp <- 1
  temp_est <- est_ability(ip = ip, resp = resp, method = "owen",
                          prior_pars = c(m0, sqrt(v0)))
  expect_equal(temp_est$est, 1.064898, tolerance = 1e-6)
  expect_equal(temp_est$se^2, 1.090684, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  # Another example for one item
  m0 <- 0.374
  v0 <- 0.655
  ip <- new("3PL", a = 0.6492, b = -1.4123, c = 0.0053, D = 1, se_a = NULL,
            se_b = NULL, se_c = NULL, item_id = NULL, content = NULL,
            misc = NULL)
  resp <- 0
  temp_est <- est_ability(ip = ip, resp = resp, method = "owen",
                          prior_pars = c(m0, sqrt(v0)))
  expect_equal(temp_est$est, -0.20814, tolerance = 1e-5)
  expect_equal(temp_est$se^2, 0.5410755, tolerance = 1e-6)

})


###############################################################################@
############################# est_ability - ML #################################
###############################################################################@
test_that("est_ability - ML", {
  ip <- itempool(
    a = c(
      0.884197125327773, 0.641740406746976, 0.637108941096812,
      1.20244530553464, 1.34323987562675, 1.474029551493, 1.42986021412071,
      0.564554519834928, 0.579683377640322, 0.667258570319973,
      1.34760631772224, 1.075640355004, 1.45692114031408, 0.607514199451543,
      0.666346082813106, 1.8072933035437, 1.30535634083208, 1.93822752987035,
      1.73588501708582, 1.60583402158227, 1.09267979150172, 0.540868601528928,
      1.54821544664446, 1.16912602749653, 1.09414781618398, 1.40611228963826,
      0.920170411816798, 1.88622517616022, 0.689107569050975,
      0.861420561093837, 0.525530818500556, 0.844860291108489,
      1.73652094230056, 1.23069458303507, 1.73296662454959, 1.64101757179014,
      1.76887892151717, 0.639569879742339),
    b = c(
      -0.383924056368274, -1.53373225397389, -1.97827922812951,
      0.51093189825887, -1.481247183071, 0.282873812035544, -1.50036749678062,
      -1.14698898614752, 0.604867489398726, 1.39114876765744,
      -0.323809346783167, -0.762786729612589, 0.449696800365297,
      1.99408201166263, 0.263998428457945, 2.68167705276291, -0.780200871008166,
      0.424690332217461, 0.629369734172185, 1.52097072996385,
      -1.23893482185556, -1.72437664723673, -1.6663494787618,
      0.1500752471024, 0.573747122347429, -0.694487697707527,
      0.101807324722603, 0.923059152892394, -1.08240830943348,
      0.228139311222657, 0.634357465817595, -0.324786430696751,
      1.20426233805235, -0.422862774355197, 0.588777621523254,
      -0.546680192400087, 0.970457541815255, -0.199916207031426),
    c = c(
      0.159727674443275, 0.181966272555292, 0.132189845200628,
      0.0441575736505911, 0.207619160204194, 0.145414250926115,
      0.224728510645218, 0.273771667736582, 0.192767260316759,
      0.137916084146127, 0.280767961777747, 0.0396517782937735,
      0.0979830316035077, 0.294935363531113, 0.094270147732459,
      0.197115818294697, 0.16417963730637, 0.102108501945622,
      0.225032159895636, 0.125950632593594, 0.210073017189279,
      0.123149601859041, 0.0160024092765525, 0.214372928347439,
      0.118859681440517, 0.088402352691628, 0.089805198716931,
      0.238103704573587, 0.0694027771940455, 0.100334205455147,
      0.259730131994002, 0.268449394055642, 0.159935772232711,
      0.275540588493459, 0.0413019084138796, 0.113490730267949,
      0.142325833230279, 0.135479651740752),
    D = 1
  )
  resp <- c(1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1,
            1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1)
  # resp_set <- response_set(matrix(resp, nrow = 1), ip = ip)
  est <- est_ability(ip = ip, resp = resp, method = 'ml')
  expect_identical(est$est, setNames(0.135097, "S1"), tolerance = 1e-3,
                   ignore_attr = FALSE)
  expect_identical(est$se, 0.3734483, tolerance = 1e-4, ignore_attr = FALSE)

  # -------------------------------------------------------------------------- #
  # Multiple response patterns
  theta <- rnorm(3)
  resp <- sim_resp(ip = ip, theta = theta)
  # resp_set <- response_set(resp, ip)
  est <- est_ability(ip = ip, resp = resp, method = 'ml')
  expect_identical(length(est$est), 3L)
  expect_identical(length(est$se), 3L)

  # -------------------------------------------------------------------------- #
  # All correct and all incorrect responses should be equal to the theta_range
  # values.
  ip <- generate_ip(model = "Rasch")
  resp_set <- generate_resp_set(ip = ip, theta = c(-10000, 10000))
  theta_range <- c(-4, 4)
  for (tol in 10^(-1 * c(2:10))) {
    est <- est_ability(ip = ip, resp = resp_set, method = 'ml', tol = tol,
                       theta_range = theta_range)$est
    est <- unname(est)
    expect_equal(est, theta_range, tolerance = 1e-6)
  }

  # -------------------------------------------------------------------------- #
  # One item and multiple theta raises error when resp is not a matrix.
  ip <- itempool(b = rnorm(1))
  resp <- as.vector(sim_resp(ip = ip, theta = rnorm(10)))
  expect_error(est_ability(ip = ip, resp = resp, method = 'ml'),
               regexp = paste0("'resp' and 'ip' do not correspond."))
  # If resp is a matrix, no error will be raised
  expect_identical(length(est_ability(ip = ip, resp = matrix(resp, ncol = 1),
                                      method = 'ml')$est), 10L)

  # -------------------------------------------------------------------------- #
  # Example from Baker Ch.5, p.86-88.
  # http://echo.edres.org:8080/irt/baker/chapter5.pdf
  ip <- itempool(a = c(1, 1.2, .8), b = c(-1, 0, 1), D = 1)
  resp <- c(1, 0, 1)
  est <- est_ability(ip = ip, resp = resp, method = 'ml')
  # TODO: tolerance should be 1e-4, not 1e-3, try to make this work
  expect_identical(est$est, setNames(.3249, "S1"), tolerance = 1e-3,
                   ignore_attr = FALSE)
  expect_identical(est$se, 1.23, tolerance = 1e-2, ignore_attr = FALSE)


  # -------------------------------------------------------------------------- #
  # For all correct and incorrect responses, the difference between the
  # estimate and the theta boundary should be smaller than the tolerance
  n_item <- sample(10:20, 1)
  ip <- itempool(a = rlnorm(n_item, 0, .3), b = rnorm(n_item), D = 1)
  resp1 <- rep(1, n_item)
  resp0 <- rep(0, n_item)
  tol <- runif(1, 1e-14, 1e-2)
  min_theta <- -4
  max_theta <- 4
  est1 <- est_ability(ip = ip, resp = resp1, method = 'ml',
                      theta_range = c(min_theta, max_theta), tolerance = tol)
  expect_true(max_theta - est1$est < tol)
  est0 <- est_ability(ip = ip, resp = resp0, method = 'ml',
                      theta_range = c(min_theta, max_theta), tolerance = tol)
  expect_true(min_theta - est0$est < tol)

  # -------------------------------------------------------------------------- #
  # A response pattern with a decimal number is acceptable.
  ip <- itempool(b = c(-0.442, -0.1172, 0.12614, -0.1912, -0.0452, -0.0771,
                           0.12607, -0.1798, 0.03754, -0.283, -0.2239),
                     D = 1)
  resp <- rep(1, 11)
  for (i in 1:length(resp)) {
    resp2 <- resp
    resp2[i] <- 0.5
    est <- est_ability(ip = ip, resp = resp2, method = 'ml')
    # cat("Responses: ",  paste0(resp2, collapse = ", "), "\n")
    # cat(paste0("Theta Estimate = ",  est$est, ",  SE = ", est$se, "\n\n" ))
    expect_identical(est$est, setNames(2.94, "S1"), tolerance = 1e-2,
                     ignore_attr = FALSE)
  }


  # -------------------------------------------------------------------------- #
  # A test case
  ip <- itempool(a = c(1.6807, 1.4203, 1.3582, 0.8569, 0.7989, 1.1182, 0.8105),
                 b = c(0.2829, 1.0154, 0.8887, 1.4957, 1.4821, 0.3831, 0.6588),
                 c = c(0.0970, 0.0971, 0.0748, 0.2116, 0.2184, 0.0163, 0.0174),
                 D = 1)
  resp <- c(1, 1, 1, 1, 0, 1, 1)
  # plot_resp_loglik(ip = ip, resp = resp)
  est <- est_ability(ip = ip, resp = resp, method = "ml")
  expect_identical(unname(est$est), 2.72882, tolerance = 1e-4)
  expect_identical(est$se, 1.240068, tolerance = 1e-4)

  # -------------------------------------------------------------------------- #

  # # The following code finds item pool and response string pairs where
  # # est_ability function fails.
  # tol <- 0.01
  # theta_grid <- seq(from = -10, to = 10, by = tol)
  # counter <- 0
  # while (TRUE) {
  #   cat(paste0(sprintf("%4d -  ", counter <- counter + 1),
  #              format(Sys.time(), format = "%X"), "\n"))
  #   ip <- generate_ip(n = sample(10:20, 1), model = "3PL")
  #   true_theta <- rnorm(100)
  #   resp <- sim_resp(ip = ip, theta = true_theta, output = "response_set")
  #   est <- est_ability(resp = resp, ip = ip, method = "ml",
  #   theta_range = c(-10, 10))$est
  #   for (i in 1:length(resp)) {
  #     temp_est <- theta_grid[which.max(sapply(theta_grid, resp_loglik,
  #                                             ip = ip, resp = resp[[i]]))]
  #     if (abs(temp_est - est[i]) > (10 * tol)) {
  #       plot_resp_loglik(ip = ip, resp = resp[[i]], theta_range = c(-10, 10))
  #       beepr::beep(1)
  #       dput(resp[[i]])
  #       dput(ip)
  #       stop(paste0("\nFound!"))
  #     }
  #   }
  # }

  r <- new("Response", examinee_id = "S642",
           item_id = c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5",
                       "Item_6", "Item_7", "Item_8", "Item_9", "Item_10"),
           testlet_id = NULL, score = c(0L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 1L),
           raw_response = NULL, order = 1:10, response_time = NULL, misc = NULL)
  x <- new("Itempool",
           item_list = list(
             Item_1 = new("3PL", a = 1.5962, b = -0.0067, c = 0.0033, D = 1,
                          se_a = NULL, se_b = NULL, se_c = NULL,
                          item_id = "Item_1", content = NULL, misc = NULL),
             Item_2 = new("3PL", a = 0.9581, b = 0.4018, c = 0.1982, D = 1,
                          se_a = NULL, se_b = NULL, se_c = NULL,
                          item_id = "Item_2", content = NULL, misc = NULL),
             Item_3 = new("3PL", a = 1.3958, b = 1.4774, c = 0.1886, D = 1,
                          se_a = NULL, se_b = NULL, se_c = NULL,
                          item_id = "Item_3", content = NULL, misc = NULL),
             Item_4 = new("3PL", a = 1.5551, b = -0.0584, c = 0.1115, D = 1,
                          se_a = NULL, se_b = NULL, se_c = NULL,
                          item_id = "Item_4", content = NULL, misc = NULL),
             Item_5 = new("3PL", a = 1.6921, b = 0.1922, c = 0.0732, D = 1,
                          se_a = NULL, se_b = NULL, se_c = NULL,
                          item_id = "Item_5", content = NULL, misc = NULL),
             Item_6 = new("3PL", a = 0.7827, b = -1.2223, c = 0.1938, D = 1,
                          se_a = NULL, se_b = NULL, se_c = NULL,
                          item_id = "Item_6", content = NULL, misc = NULL),
             Item_7 = new("3PL", a = 0.6187, b = -0.6108, c = 0.1513, D = 1,
                          se_a = NULL, se_b = NULL, se_c = NULL,
                          item_id = "Item_7", content = NULL, misc = NULL),
             Item_8 = new("3PL", a = 0.6648, b = 0.3511, c = 0.199, D = 1,
                          se_a = NULL, se_b = NULL, se_c = NULL,
                          item_id = "Item_8", content = NULL, misc = NULL),
             Item_9 = new("3PL", a = 0.9227, b = -1.5175, c = 0.212, D = 1,
                          se_a = NULL, se_b = NULL, se_c = NULL,
                          item_id = "Item_9", content = NULL, misc = NULL),
             Item_10 = new("3PL", a = 0.9755, b = -0.4005, c = 0.157, D = 1,
                           se_a = NULL, se_b = NULL, se_c = NULL,
                           item_id = "Item_10", content = NULL, misc = NULL)
             ),
           misc = NULL)

  observed <- est_ability(resp = response_set(r), ip = x, method = "ml",
                          theta_range = c(-5, 5))$est
  # sapply(c(-5, observed), resp_loglik, ip = x, resp = r)
  expect_identical(unname(observed), -5,
                   info = "This is a known bug that needs to be fixed soon!")


  # -------------------------------------------------------------------------- #
  r <- new("Response", examinee_id = "S15", item_id = c("Item_1", "Item_2",
  "Item_3", "Item_4", "Item_5", "Item_6", "Item_7", "Item_8", "Item_9",
  "Item_10", "Item_11", "Item_12", "Item_13", "Item_14", "Item_15",
  "Item_16", "Item_17", "Item_18"), testlet_id = NULL, score = c(1L,
  0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L,
  0L), raw_response = NULL, order = 1:18, response_time = NULL,
      misc = NULL)
  x <- new("Itempool", item_list = list(Item_1 = new("3PL", a = 0.969,
      b = 0.9786, c = 0.1874, D = 1, se_a = NULL, se_b = NULL,
      se_c = NULL, item_id = "Item_1", content = NULL, misc = NULL),
      Item_2 = new("3PL", a = 0.9468, b = -0.9774, c = 0.2414,
          D = 1, se_a = NULL, se_b = NULL, se_c = NULL, item_id = "Item_2",
          content = NULL, misc = NULL), Item_3 = new("3PL", a = 1.2925,
          b = 0.1185, c = 0.1834, D = 1, se_a = NULL, se_b = NULL,
          se_c = NULL, item_id = "Item_3", content = NULL, misc = NULL),
      Item_4 = new("3PL", a = 0.6005, b = -0.1643, c = 0.0278,
          D = 1, se_a = NULL, se_b = NULL, se_c = NULL, item_id = "Item_4",
          content = NULL, misc = NULL), Item_5 = new("3PL", a = 1.046,
          b = -0.6026, c = 0.1159, D = 1, se_a = NULL, se_b = NULL,
          se_c = NULL, item_id = "Item_5", content = NULL, misc = NULL),
      Item_6 = new("3PL", a = 1.3144, b = -0.0333, c = 0.2262,
          D = 1, se_a = NULL, se_b = NULL, se_c = NULL, item_id = "Item_6",
          content = NULL, misc = NULL), Item_7 = new("3PL", a = 0.7247,
          b = -1.4337, c = 0.162, D = 1, se_a = NULL, se_b = NULL,
          se_c = NULL, item_id = "Item_7", content = NULL, misc = NULL),
      Item_8 = new("3PL", a = 1.1262, b = -0.8723, c = 0.0244,
          D = 1, se_a = NULL, se_b = NULL, se_c = NULL, item_id = "Item_8",
          content = NULL, misc = NULL), Item_9 = new("3PL", a = 1.1604,
          b = 0.8296, c = 0.0142, D = 1, se_a = NULL, se_b = NULL,
          se_c = NULL, item_id = "Item_9", content = NULL, misc = NULL),
      Item_10 = new("3PL", a = 1.9168, b = -0.1796, c = 0.174,
          D = 1, se_a = NULL, se_b = NULL, se_c = NULL, item_id = "Item_10",
          content = NULL, misc = NULL), Item_11 = new("3PL", a = 0.8503,
          b = -1.5656, c = 0.2195, D = 1, se_a = NULL, se_b = NULL,
          se_c = NULL, item_id = "Item_11", content = NULL, misc = NULL),
      Item_12 = new("3PL", a = 1.4175, b = 0.4762, c = 0.0303,
          D = 1, se_a = NULL, se_b = NULL, se_c = NULL, item_id = "Item_12",
          content = NULL, misc = NULL), Item_13 = new("3PL", a = 1.3567,
          b = 0.2714, c = 0.2198, D = 1, se_a = NULL, se_b = NULL,
          se_c = NULL, item_id = "Item_13", content = NULL, misc = NULL),
      Item_14 = new("3PL", a = 0.8433, b = -0.3574, c = 0.0728,
          D = 1, se_a = NULL, se_b = NULL, se_c = NULL, item_id = "Item_14",
          content = NULL, misc = NULL), Item_15 = new("3PL", a = 0.8377,
          b = -0.0228, c = 0.1961, D = 1, se_a = NULL, se_b = NULL,
          se_c = NULL, item_id = "Item_15", content = NULL, misc = NULL),
      Item_16 = new("3PL", a = 0.5828, b = -0.6004, c = 0.0451,
          D = 1, se_a = NULL, se_b = NULL, se_c = NULL, item_id = "Item_16",
          content = NULL, misc = NULL), Item_17 = new("3PL", a = 0.8662,
          b = 0.2746, c = 0.1924, D = 1, se_a = NULL, se_b = NULL,
          se_c = NULL, item_id = "Item_17", content = NULL, misc = NULL),
      Item_18 = new("3PL", a = 1.1107, b = 0.3376, c = 0.1457,
          D = 1, se_a = NULL, se_b = NULL, se_c = NULL, item_id = "Item_18",
          content = NULL, misc = NULL)), misc = NULL)

  # plot_resp_loglik(ip = ip, resp = resp, theta_range = c(-10, 0))

  observed <- est_ability(resp = response_set(r), ip = x, method = "ml",
                          theta_range = c(-10, 10))$est
  # sapply(c(-10, observed), resp_loglik, ip = x, resp = r)
  expect_identical(unname(observed), -10,
                   info = "This is a known bug that needs to be fixed soon!")

  # -------------------------------------------------------------------------- #
  # Test problematic cases - 01 - Item pool with 3PL and GPCM items
  test_fn <- test_path("data_for_tests",
                       "ability_estimation_mle_test_cases_001.rds")
  if (file.exists(test_fn)) {
    test_data <- readRDS(test_fn)
    observed <- est_ability(resp = test_data$resp_set, ip = test_data$ip,
                            method = "ml", theta_range = c(-15, 15))$est
    expect_identical(observed, test_data$expected_theta, tolerance = 0.0001)

    # plot_resp_loglik(ip = test_data$ip, resp = test_data$resp_set[[1]],
    #                  theta_range = c(-5, -2))
  }

  # -------------------------------------------------------------------------- #
  # Test problematic cases - 02 - all 3PL items
  test_fn <- test_path("data_for_tests",
                       "ability_estimation_mle_test_cases_002.rds")
  if (file.exists(test_fn)) {
    test_data <- readRDS(test_fn)
    observed <- est_ability(resp = test_data$resp_set, ip = test_data$ip,
                            method = "ml", theta_range = c(-5, 5))$est
    expect_identical(unname(observed),
                     test_data$expected_theta, tolerance = 0.0001)
    # Check est_ability_4pm_nr_itempool_cpp
    observed <- irt:::est_ability_4pm_nr_itempool_cpp(
      resp = test_data$resp_set[[1]]$score,
      ip = test_data$ip)
    expect_identical(unname(observed),
                     test_data$expected_theta, tolerance = 0.0001)
  }


  # -------------------------------------------------------------------------- #
  # Test problematic cases - 03 - all 3PL items
  test_fn <- test_path("data_for_tests",
                       "ability_estimation_mle_test_cases_003.rds")
  if (file.exists(test_fn)) {
    test_data <- readRDS(test_fn)
    observed <- est_ability(resp = test_data$resp_set, ip = test_data$ip,
                            method = "ml", theta_range = c(-5, 5))$est
    expect_identical(unname(observed),
                     test_data$expected_theta, tolerance = 0.0001)
    observed <- irt:::est_ability_4pm_nr_itempool_cpp(
      resp = test_data$resp_set[[1]]$score,
      ip = test_data$ip)
    expect_identical(unname(observed),
                     test_data$expected_theta, tolerance = 0.0001)
  }



})





###############################################################################@
############################# est_ability - MAP ################################
###############################################################################@

# This is the previously used function that use optimize.
est_ability_map_optimize <- function(resp_set, ip, prior_mean,
                                     prior_sigma, theta_range = c(-5, 5),
                                     tol = 1e-5) {
  se <- est <- rep(NA, length(resp_set))
  map_func <- function(theta, resp, ip, mu, sigma) {
    irt:::resp_lik_response_cpp(theta = theta, resp = resp, ip = ip) *
      stats::dnorm(x = theta, mean = mu, sd = sigma)
  }
  est <- sapply(resp_set@response_list, function(r)
    stats::optimize(f = map_func, resp = r, ip = ip, mu = prior_mean,
                    sigma = prior_sigma,
                    lower = theta_range[1], upper = theta_range[2],
                    tol = tol, maximum = TRUE)$maximum)

  se <- 1/sqrt(irt:::info_response_set_cpp(
    theta = est, resp_set = resp_set, ip = ip, tif = TRUE)[, 1] +
      1/prior_sigma^2)
  return(list(est = est, se = se))
}


test_that("est_ability - MAP", {
  # 3PL Model
  resp <- structure(
    c(0L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L,
      1L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, 1L, 1L,
      1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L),
    .Dim = c(3L, 14L),
    .Dimnames = list(
      c("S1", "S2", "S3"),
      c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5", "Item_6", "Item_7",
        "Item_8", "Item_9", "Item_10", "Item_11", "Item_12", "Item_13",
        "Item_14")
      )
    )
  ip <- structure(
          list(
            a = c(1.4093, 1.0278, 0.99, 1.0274, 1.0322, 1.2889, 0.9358, 1.495,
                  0.7372, 0.9165, 0.6396, 1.0053, 1.1146, 1.5933),
            b = c(-0.1222, 0.5685, -1.6704, 1.8111, 1.9413, 0.5208, 1.0443,
                  -0.4855, 0.2508, -0.5439, 0.7352, -0.9241, -0.599, -2.0255),
            c = c(0.2208, 0.168, 0.0123, 0.1449, 0.2047, 0.065, 0.0115, 0.004,
                  0.1416, 0.0993, 0.2941, 0.0842, 0.0327, 0.1421)),
          class = "data.frame", row.names = c(NA, -14L))
  ip <- itempool(ip, model = "3PL")
  resp_set <- response_set(resp)


  expected <- setNames(c(-0.99008915, -0.46481537, 0.25390415), rownames(resp))
  observed <- est_ability(resp = resp_set, ip = ip, method = "map", tol = 1e-8)
  expect_identical(expected, observed$est, tolerance = 1e-5)
  # SE
  expected <- setNames(c(0.56138196, 0.53827696, 0.53941983), rownames(resp))
  expect_identical(expected, observed$se, tolerance = 1e-5)

  # -------------------------------------------------------------------------- #
  # 4PL Model
  ip <- structure(
          list(
            a = c(
              1.0024, 0.8675, 0.8434, 1.3284, 0.7782, 1.3841, 0.6448, 0.803,
              0.8533, 1.2117, 0.5812, 1.0735, 1.1772, 1.1419, 1.6035, 1.3212,
              0.8517),
            b = c(
              0.6139, 1.1639, -0.4019, 1.312, 1.3703, -0.0481, 1.9143, 0.1211,
              -0.1158, 0.048, -0.7269, -1.0172, -0.0437, -1.0548, -0.8865,
              0.7124, 0.9817),
            c = c(
              0.0372, 0.2364, 0.1647, 0.0344, 0.2576, 0.0762, 0.1803, 0.2365,
              0.0899, 0.2232, 0.2531, 0.2931, 0.1914, 0.2329, 0.0826, 0.0982,
              0.0348),
            d = c(
              0.9274, 0.9399, 0.9065, 0.9723, 0.9814, 0.9703, 0.9299, 0.9025,
              0.9444, 0.9416, 0.9773, 0.9215, 0.9228, 0.9397, 0.9131, 0.9067,
              0.9327)),
          class = "data.frame", row.names = c(NA, -17L))
  resp <- structure(
            c(0L, 1L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 1L, 0L,
              0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
              0L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 1L,
              0L, 0L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 0L,
              1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 0L, 1L,
              0L, 0L, 0L, 1L, 1L),
            .Dim = c(5L, 17L),
            .Dimnames = list(
              c("S1", "S2", "S3", "S4", "S5"),
              c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5", "Item_6",
                "Item_7", "Item_8", "Item_9", "Item_10", "Item_11", "Item_12",
                "Item_13", "Item_14", "Item_15", "Item_16", "Item_17")))

  ip <- itempool(ip, model = "4PL")
  resp_set <- response_set(resp)


  expected <- c(S1 = -1.16111859, S2 = -0.23158822, S3 = 0.05350245,
                S4 = -0.42399905, S5 = 0.4991991)
  observed <- est_ability(resp = resp_set, ip = ip, method = "map", tol = 1e-8)
  expect_identical(expected, observed$est, tolerance = 1e-5)
  # Alternatively "bm" (Bayes Modal) can be used as a name
  observed <- est_ability(resp = resp_set, ip = ip, method = "bm", tol = 1e-8)
  expect_identical(expected, observed$est, tolerance = 1e-5)
  observed <- est_ability(resp = resp_set, ip = ip, method = "BM", tol = 1e-8)
  expect_identical(expected, observed$est, tolerance = 1e-5)


  # -------------------------------------------------------------------------- #
  # 4PL Model with non-default prior
  ip <- structure(
          list(
            a = c(
              0.845, 0.9351, 1.0451, 1.1999, 1.115, 0.734, 1.6079, 1.0508,
              1.0578, 0.9483, 0.7482, 2.0618, 1.8155
            ),
            b = c(
              -0.5999, -1.4125, 1.59, -1.2872, 1.4371, 0.2475, -0.3255, -0.986,
              0.6565, -0.6284, 0.64, -2.3758, -0.397
            ),
            c = c(
              0.2669, 0.0473, 0.2934, 0.0817, 0.0149, 0.0073, 0.1018, 0.2049,
              0.0714, 0.2238, 0.0017, 0.2114, 0.2591
            ),
            d = c(
              0.9093, 0.9367, 0.932, 0.967, 0.9001, 0.9791, 0.9832, 0.9219,
              0.9747, 0.964, 0.9225, 0.9109, 0.9875
            )
          ),
          class = "data.frame", row.names = c(NA, -13L)
        )
  resp <- structure(
            c(
              0L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L,
              1L, 1L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L,
              1L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 0L, 0L, 1L, 1L,
              0L, 1L, 0L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 0L,
              1L
            ),
            .Dim = c(5L, 13L),
            .Dimnames = list(
              c("S1", "S2", "S3", "S4", "S5"),
              c(
                "Item_1", "Item_2", "Item_3", "Item_4", "Item_5", "Item_6",
                "Item_7", "Item_8", "Item_9", "Item_10", "Item_11", "Item_12",
                "Item_13"
              )
            )
          )

  ip <- itempool(ip, model = "4PL")
  resp_set <- response_set(resp)

  expected <- c(S1 = -1.07115417, S2 = 0.46658809, S3 = 0.93300496,
                S4 = -1.34632891, S5 = -0.49037309)
  observed <- est_ability(resp = resp_set, ip = ip, method = "map",
                          prior_pars = c(-.2, 0.81), tol = 1e-8)
  expect_identical(expected, observed$est, tolerance = 1e-5)

  # SE
  expected <- setNames(c(0.55891302, 0.55029574, 0.58501851, 0.57770596,
                         0.5241226), rownames(resp))
  expect_identical(expected, observed$se, tolerance = 1e-5)



  # -------------------------------------------------------------------------- #
  # Test "est_ability_map_response_set_cpp" function


  for (i in 1:3) {
    ip <- generate_ip(model = sample(c("3PL", "4PL"), 1))
    resp_set <- generate_resp_set(ip = ip, theta = rnorm(sample(10:20, 1)),
                                  prop_missing = .1)
    prior_mean <- runif(1, -1, 1)
    prior_sd <- runif(1, .5, 2)


    expected <- est_ability_map_optimize(
      resp_set = resp_set, ip = ip, prior_mean = prior_mean,
      prior_sigma = prior_sd, theta_range = c(-5, 5), tol = 1e-6)

    observed <- irt:::est_ability_map_response_set_cpp(
      resp_set = resp_set, ip = ip, prior_dist = "norm",
      prior_par = c(prior_mean, prior_sd), theta_range = c(-5, 5), tol = 1e-6)

    expect_equal(unname(expected$est), observed$est, tolerance = 1e-5)
    expect_equal(unname(expected$se), observed$se, tolerance = 1e-5)

    observed <- est_ability(resp = resp_set, ip = ip, method = "MAP",
                            prior_pars = c(prior_mean, prior_sd), tol = 1e-6)

    expect_equal(expected$est, observed$est, tolerance = 1e-5)
    expect_equal(expected$se, observed$se, tolerance = 1e-5)
  }
})


###############################################################################@
############################# est_ability_map_response_cpp #####################
###############################################################################@
test_that("est_ability_map_response_cpp", {

  # 2PL example
  # Example on Rose, N. (2010), p.11
  resp <- response(score = c(1, 1, 0, 0, 1))
  ip <- itempool(a = c(1, 2, 0.5, 1, 2), b = c(-1, -0.5, 0, 0.5, 1), D = 1)

  observed <- est_ability_map_response_cpp(
    resp = resp, ip = ip, prior_dist = "norm", prior_par = c(0, 1),
    initial_theta = 0, tol = 0.0005)
  # Check theta estimate
  expect_equal(observed$est, 0.7259562, tolerance = 1e-5)
  expect_equal(observed$se, 0.6135844, tolerance = 1e-5)

  # -------------------------------------------------------------------------- #
  # 3PL
  ipdf <- structure(list(item_id = c(
    "Item_1", "Item_2", "Item_3", "Item_4", "Item_5", "Item_6", "Item_7",
    "Item_8", "Item_9", "Item_10", "Item_11", "Item_12", "Item_13", "Item_14",
    "Item_15", "Item_16", "Item_17", "Item_18", "Item_19", "Item_20"),
    model = c("3PL", "3PL", "3PL", "3PL", "3PL", "3PL", "3PL", "3PL", "3PL",
              "3PL", "3PL", "3PL", "3PL", "3PL", "3PL", "3PL", "3PL", "3PL",
              "3PL", "3PL"),
    a = c(0.9207, 1.7345, 1.366, 1.394, 0.8558, 1.0796, 0.917,
          1.0019, 0.5253, 0.8122, 0.871, 1.2784, 1.0618, 1.5898, 1.1348,
          1.0926, 1.7152, 0.7639, 0.993, 0.4401),
    b = c(-0.9446, 1.4636, 1.043, -1.5267, -1.594, 0.6748, 1.5447, -0.5962,
          2.7508, -0.4635, -0.8652, -0.1037, 0.1181, 0.1457, 1.6503, 0.0905,
          -0.8042, -0.8331, -0.7324, 0.1453),
    c = c(0.1175, 0.0406, 0.239, 0.0823, 0.0521, 0.0563, 0.2574, 0.2393,
          0.2795, 0.092, 0.2068, 0.2715, 0.0843, 0.1521, 0.0761, 0.1959,
          0.2255, 0.0542, 0.2501, 0.26),
    D = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
    class = "data.frame", row.names = c(NA, -20L))
  ip <- itempool(ipdf)
  resp <- new("Response", examinee_id = "S1",
              item_id = c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5",
                          "Item_6", "Item_7", "Item_8", "Item_9", "Item_10",
                          "Item_11", "Item_12", "Item_13", "Item_14", "Item_15",
                          "Item_16", "Item_17", "Item_18", "Item_19",
                          "Item_20"),
              testlet_id = NULL,
              score = c(1L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 1L,
                        1L, 0L, 0L, 0L, 1L, 1L, 0L, 1L),
              raw_response = c("A", "A", "A", "C", "D", "D", "D", "C", "D",
                               "D", "D", "D", "D", "D", "B", "B", "B", "B",
                               "D", "D"),
              order = 1:20, response_time = NULL, misc = NULL)

  observed <- est_ability_map_response_cpp(
    resp = resp, ip = ip, prior_dist = "norm", prior_par = c(0, 1),
    initial_theta = 0, tol = 0.0000001)
  # Check theta estimate
  expect_equal(observed$est, -0.28584, tolerance = 1e-5)
  expect_equal(observed$se, 0.4948627, tolerance = 1e-5)

})


###############################################################################@
############################# est_ability_map_single_examinee_cpp ##############
###############################################################################@

test_that("est_ability_map_single_examinee_cpp", {

  ip <- generate_ip()
  resp <- generate_resp(ip = ip, theta = rnorm(1))[[1]]
  resp_vector <- resp$score

  observed <- est_ability_map_single_examinee_cpp(
    resp = resp_vector, ip = ip, prior_dist = "norm", prior_par = c(0, 1),
    initial_theta = 0, tol = 0.000005)

  # First check
  expected <- est_ability_map_response_cpp(
    resp = resp, ip = ip, prior_dist = "norm", prior_par = c(0, 1),
    initial_theta = 0, tol = 0.000005)
  expect_equal(observed$est, expected$est, tolerance = 1e-5)
  expect_equal(observed$se, expected$se, tolerance = 1e-5)

  # Second check
  expected <- est_ability_map_optimize(resp_set = response_set(resp), ip = ip,
                                       prior_mean = 0, prior_sigma = 1,
                                       tol = 0.000005)
  expect_equal(setNames(observed$est, "S1"), expected$est, tolerance = 1e-5)
  # TODO: Make the following line like the line above, i.e. account for name
  expect_equal(observed$se, expected$se, tolerance = 1e-5)

})


###############################################################################@
############################# est_ability - EAP ################################
###############################################################################@
test_that("est_ability - EAP", {
  # -------------------------------------------------------------------------- #
  a = c(0.604, 1.945, 1.174, 0.577, 0.722, 1.265, 0.837, 1.752, 1.409, 1.73,
        0.781, 0.527, 1.767, 0.911, 1.648, 1.703, 1.027, 0.947)
  b = c(-1.88, -1.274, -0.993, -0.956, -0.869, -0.621, -0.612, -0.42, -0.393,
        -0.357, -0.232, 0.033, 0.197, 0.331, 0.437, 0.486, 0.804, 1.885)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b), D = D, model = '2PL',
                   item_id = paste0('i', 1:18))
  resp = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1)
  expected <- 1.566382
  object <- est_ability(ip = ip, resp = resp, method = 'eap')
  expect_identical(object$est, expected, tolerance = 1e-3)

  # -------------------------------------------------------------------------- #
  a = c(0.684, 0.618, 0.824, 1.698, 1.334, 1.179, 1.697, 1.898, 0.772, 0.702,
        0.897, 0.6, 1.881, 1.655, 1.102, 1.514, 0.777, 1.605)
  b = c(-1.841, -1.799, -1.767, -1.629, -0.991, -0.781, -0.688, -0.574, -0.367,
        -0.328, -0.212, 0.044, 0.13, 0.195, 1.213, 1.355, 1.702, 1.893)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b), D = D, model = '2PL',
                   item_id = paste0('i', 1:18))
  resp = c(1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0)
  expected <- 0.383088
  object <- est_ability(ip = ip, resp = resp, method = 'eap')
  expect_identical(object$est, expected, tolerance = 1e-3)

  # # ------------------------------------------------------------------------ #
  # ip_dtf <- data.frame(
  #   a = c(0.82628, 1.18005, 0.93197, 1.09279, 0.62252, 0.55811, 0.54016,
  #         0.76499, 1.10399, 0.6466,  0.58406, 0.93681, 0.85042, 0.58623,
  #         0.47125, 0.57258),
  #   b = c(0.3248, 1.58142, 0.9219, 1.84474, 1.25307, 0.9381, 0.40202, 1.15291,
  #         2.75536, 1.45367, 1.67435, 1.9451, 1.20863, 1.66219, 1.87079,
  #         1.35354),
  #   c = c(0, 0.15767, 0.2311, 0.27658, 0, 0.13462, 0.12783, 0.27903, 0.32545,
  #         0.20548, 0.24894, 0.12961, 0.13346, 0.21813, 0, 0))
  # ip <- itempool(ip_dtf, D = 1.7)
  # resp <- rep(1, nrow(ip_dtf))
  # expected <- 2.5227
  # observed <- est_ability(ip = ip, resp = resp, method = 'eap',
  #                         theta_range = c(-3, 3),
  #                         number_of_quads = 10, tolerance = 0.0001)
  # expect_identical(observed$est, expected, tolerance = 1e-3)

  # -------------------------------------------------------------------------- #
  a = c(1.753, 1.295, 1.712, 1.408, 0.9, 0.568, 1.145, 1.407, 1.818, 0.842,
        1.404, 1.086, 0.999, 0.926, 1.689, 0.929, 1.597)
  b = c(-1.968, -1.663, -1.566, -1.454, -0.993, -0.883, -0.73, -0.573, -0.172,
        0.352, 0.947, 1.085, 1.192, 1.245, 1.689, 1.708, 1.713)
  c = c(0.334, 0.172, 0.131, 0.224, 0.297, 0.113, 0.315, 0.084, 0.312, 0.271,
        0.076, 0.082, 0.2, 0.223, 0.293, 0.077, 0.124)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b, c), D = D, model = '3PL',
                   item_id = paste0('i', 1:17))
  resp = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1)
  expected <- 1.9033609
  object <- est_ability(ip = ip, resp = resp, method = 'eap')
  expect_identical(object$est, expected, tolerance = 1e-3)
  # TODO: tolerance should be 1e-3 not 1e-2
  expect_identical(object$se, 0.4766623, tolerance = 1e-2)

  # -------------------------------------------------------------------------- #
  # Check very low ability is limited
  ip <- itempool(b = rep(-5, 20), D = 1.702)
  resp <- rep(0, 20)
  expected <- -4.223
  est <- est_ability(resp = resp, ip = ip, method = "eap",
                     theta_range = c(expected, 5))
  expect_equal(est$est, expected, tolerance = 1e-3)


  # -------------------------------------------------------------------------- #
  # For a repeating response string, all of the ability estimates should be
  # the same.
  n <- sample(10:20, 1)
  ip <- itempool(b = rnorm(8))
  resp <- matrix(rep(sim_resp(ip = ip, theta = rnorm(1)), n * 2), nrow = n * 2,
                 byrow = TRUE)
  est <- est_ability(ip = ip, resp = resp, method = 'eap')
  expect_true(length(unique(est$est)) == 1)
  expect_true(length(unique(est$se)) == 1)

  # -------------------------------------------------------------------------- #
  # Multiple response patterns
  n <- 18L
  theta <- rnorm(n)
  ip <- itempool(b = rnorm(8))
  resp <- sim_resp(ip = ip, theta = theta)
  est <- est_ability(ip = ip, resp = resp, method = 'eap')
  expect_identical(length(est$est), n)
  for (i in 1:n) {
    temp_est <- est_ability(ip = ip, resp = resp[i,], method = 'eap')
    expect_identical(temp_est$est, est$est[i], tolerance = 1e-5,
                     ignore_attr = TRUE)
    expect_identical(temp_est$est, est$est[i], tolerance = 1e-5,
                     ignore_attr = TRUE)
    expect_identical(temp_est$se, est$se[i], tolerance = 1e-6,
                     ignore_attr = TRUE)
  }

  # -------------------------------------------------------------------------- #
  # Test cases - 01 - EAP estimates where expected values were obtained via
  # flexMIRT program
  test_fn <- test_path("data_for_tests",
                       "ability_estimation_eap_test_cases_fm_001.rds")
  if (file.exists(test_fn)) {
    test_data <- readRDS(test_fn)
    # for speed purposes, only sample 50 simulees out of 1000
    sample_n <- sample(length(test_data$resp_set), 50)
    observed <- est_ability(resp = test_data$resp_set[sample_n],
                            ip = test_data$ip, method = "eap",
                            number_of_quads = test_data$number_of_quad)
    expect_identical(unname(observed$est),
                     test_data$expected_theta[sample_n], tolerance = 1e-5)
    expect_identical(unname(observed$se),
                     test_data$expected_se[sample_n], tolerance = 1e-5)
  }

  # -------------------------------------------------------------------------- #
  # Another check for multiple response strings
  ipdf <- data.frame(
    a = c(
      0.6106, 1.3991, 1.0061, 0.6659, 0.874, 0.5626, 1.0832, 0.7894, 0.8363,
      0.8899, 1.6604, 1.0747, 0.9662, 1.0012, 0.8011, 0.7443, 1.3273, 1.0538,
      1.1126, 1.0076, 0.6941, 0.5767, 1.396
    ),
    b = c(
      -1.0953, -1.9567, -0.0958, 0.6412, -1.7381, -0.4611, -0.5347, -1.4265,
      0.3377, 2.1225, 0.3225, 0.4031, 0.296, -0.1087, -0.7493, 0.1027, -1.4886,
      -0.2322, -0.4247, -0.9931, -1.9974, -1.7525, 0.2514
    ),
    c = c(
      0.2428, 0.2605, 0.2875, 0.2952, 0.1921, 0.2779, 0.0417, 0.1499, 0.1628,
      0.1415, 0.0733, 0.2742, 0.2323, 0.2398, 0.0216, 0.0043, 0.0065, 0.0847,
      0.1073, 0.2904, 0.0741, 0.2835, 0.0819
    ),
    row.names = c(
      "Item_1", "Item_2", "Item_3", "Item_4", "Item_5", "Item_6", "Item_7",
      "Item_8", "Item_9", "Item_10", "Item_11", "Item_12", "Item_13", "Item_14",
      "Item_15", "Item_16", "Item_17", "Item_18", "Item_19", "Item_20",
      "Item_21", "Item_22", "Item_23"
    )
  )
  resp <- structure(
    c(
      1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
      1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0,
      0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1,
      1, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0,
      0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0,
      0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1,
      1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0,
      1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0,
      1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1,
      1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1
    ),
    .Dim = c(12L, 23L),
    .Dimnames = list(
      c(
        "S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10", "S11",
        "S12"
      ),
      c(
        "Item_1", "Item_2", "Item_3", "Item_4", "Item_5", "Item_6", "Item_7",
        "Item_8", "Item_9", "Item_10", "Item_11", "Item_12", "Item_13",
        "Item_14", "Item_15", "Item_16", "Item_17", "Item_18", "Item_19",
        "Item_20", "Item_21", "Item_22", "Item_23"
      )
    )
  )
  ip <- itempool(ipdf)
  resp_set <- response_set(resp, ip = ip)
  prior_pars <- c(0, 1.7)
  observed <- est_ability(resp = resp_set, ip = ip, method = "eap",
                          prior_pars = prior_pars, theta_range = c(-4, 4),
                          number_of_quads = 61)

  expected <- list(
    est = c(-0.4271, 0.3683, -0.0127, 0.0317, 1.551, -0.3411, 0.6763,
            0.596, 1.0763, 0.8501, -3.9377, 3.1117),
    se = c(0.5203, 0.5311, 0.532, 0.5168, 0.668, 0.539, 0.5558, 0.5505,
           0.5941, 0.5814, 0.8836, 0.9791))
  expect_equal(unname(observed$est), expected = expected$est, tolerance = 1e-3)
  expect_equal(unname(observed$se), expected = expected$se, tolerance = 3e-3)

  # -------------------------------------------------------------------------- #
  # One item and multiple theta raises error when resp is not a matrix.
  ip <- itempool(b = rnorm(1))
  resp <- as.vector(sim_resp(ip = ip, theta = rnorm(10)))
  expect_error(est_ability(ip = ip, resp = resp, method = 'eap'),
               regexp = paste0("'resp' and 'ip' do not correspond."))
  # If resp is a matrix, no error will be raised
  expect_identical(length(est_ability(ip = ip, resp = matrix(resp, ncol = 1),
                                  method = 'eap')$est), 10L)

  # -------------------------------------------------------------------------- #
  # A response string with missing responses can be handled correctly.
  n_items <- 10
  ip <- itempool(a = rlnorm(n_items), b = rnorm(n_items))
  resp <- sim_resp(ip = ip, theta = rnorm(1, 0, .2))[1, ]
  missing_indices <- rep(F, n_items)
  missing_indices[sample(1:n_items, 3)] <- TRUE
  resp[missing_indices] <- NA
  ip_new <- ip[!missing_indices]
  resp_new <- resp[!missing_indices]
  est1 <- est_ability(ip = ip, resp = resp, method = "eap")
  est2 <- est_ability(ip = ip_new, resp = resp_new, method = "eap")
  expect_identical(est1$est, est2$est)
  expect_identical(est1$se, est2$se)

  # -------------------------------------------------------------------------- #
  # eap with response matrix or data frame works as if it is a Response_set
  ip  <- generate_ip()
  resp_set <- generate_resp_set(ip = ip, theta = rnorm(10), prop_missing = .2)
  resp_matrix <- as.matrix(resp_set)
  resp_df <- as.data.frame(resp_matrix)

  observed_resp_set <- est_ability(resp = resp_set, ip = ip, method = "eap")
  observed_resp_matrix <- est_ability(resp = resp_matrix, ip = ip,
                                      method = "eap")
  observed_resp_df <- est_ability(resp = resp_df, ip = ip, method = "eap")
  expect_identical(observed_resp_set, observed_resp_matrix, tolerance = 1e-6)
  expect_identical(observed_resp_set, observed_resp_df, tolerance = 1e-6)
  expect_identical(observed_resp_df, observed_resp_matrix)

})


###############################################################################@
############################# est_ability_eap_single_examinee_cpp ##############
###############################################################################@
test_that("est_ability_eap_single_examinee_cpp", {
  ip <- generate_ip(n = sample(20:40, 1))
  resp_set <- generate_resp_set(ip = ip, theta = rnorm(1), prop_missing = .2)
  resp <- as.matrix(resp_set, ip = ip)[1, ]
  prior_pars <- c(runif(1, -.5, .5), runif(1, 0.2, 1.5))
  nqp <- sample(40:70, 1)
  expected <- est_ability(resp = resp, ip = ip, method = "eap",
                          prior_pars = prior_pars, no_of_quadrature = nqp)
  observed <- irt:::est_ability_eap_single_examinee_cpp(
    resp = resp, ip = ip, no_of_quadrature = nqp, prior_par = prior_pars)
  expect_equal(expected$est, observed$est, tolerance = 1e-3)
  expect_equal(expected$se, observed$se, tolerance = 1e-3)
})

###############################################################################@
############################# est_ability_eap_response_set_cpp #################
###############################################################################@
test_that("est_ability_eap_response_set_cpp", {
  ip <- generate_ip(n = 10)
  theta <- rnorm(5)
  resp <- sim_resp(ip = ip, theta = theta, prop_missing = .2)
  resp_set <- response_set(resp, ip = ip)

  expected <- irt:::est_ability_eap_cpp(
    resp = resp, ip = ip, theta_range = c(-5, 5), no_of_quadrature = 61,
    prior_dist = "norm", prior_par = c(0, 1))
  observed <- irt:::est_ability_eap_response_set_cpp(
    resp = resp_set, ip = ip, theta_range = c(-5, 5), no_of_quadrature = 61,
    prior_dist = "norm", prior_par = c(0, 1))

  expect_identical(expected$est, observed$est, tolerance = 1e-4)
  expect_identical(expected$se, observed$se, tolerance = 1e-4)

  # microbenchmark::microbenchmark(
  #   old = irt:::est_ability_eap_cpp(
  #     resp = resp, ip = ip, theta_range = c(-5, 5), no_of_quadrature = 61,
  #     prior_dist = "norm", prior_par = c(0, 1)),
  #   new = irt:::est_ability_eap_response_set_cpp(
  #     resp = resp_set, ip = NULL, theta_range = c(-5, 5),
  #     no_of_quadrature = 61,
  #     prior_dist = "norm", prior_par = c(0, 1)),
  #   times = 1000)

  # Unit: milliseconds
  # expr    min     lq     mean  median      uq      max neval cld
  #  old 4.2124 4.8987 5.869103 5.54060 6.35010  20.2398  1000  a
  #  new 4.8772 5.7121 6.909870 6.42225 7.35045 137.6635  1000   b
})


###############################################################################@
############################# est_ability_4pm_nr_response_cpp ##################
###############################################################################@

test_that("est_ability_4pm_nr_response_cpp", {
  ip <- generate_ip(n = 15)
  theta <- rnorm(1)
  resp <- sim_resp(ip = ip, theta = theta)
  resp_object <- response_set(resp, ip = ip)@response_list[[1]]

  expected <- irt:::est_ability_4pm_nr_itempool_cpp(
    resp = resp, ip = ip, theta_range = c(-5, 5))
  observed <- irt:::est_ability_4pm_nr_response_cpp(
    resp = resp_object, ip = ip, theta_range = c(-5, 5))
  observed_2 <- est_ability(resp = resp_object, ip = ip, method = "ml")
  expect_identical(expected, observed)
  expect_equal(observed, unname(observed_2$est), tolerance = 1e-4)


  # -------------------------------------------------------------------------- #
  ip <- generate_ip(n = 15, model = "GPCM")
  theta <- rnorm(1)
  resp_set <- sim_resp(ip = ip, theta = theta, output = "response_set")
  response <- resp_set[[1]]

  # irt:::resp_loglik_response_cpp(theta = 0.5, resp = response,
  #                                ip = ip, derivative = 1)
  # irt:::resp_loglik_response_cpp(theta = 0.5, resp = response,
  #                                ip = ip, derivative = 2)

  # observed <- irt:::est_ability_4pm_nr_response_cpp(
  #   resp = response, ip = ip, theta_range = c(-5, 5))


  # # Response object should have valid 'item_id' slot.
  # resp_object <- response(resp[1,])
  # resp_object@item_id <- NULL
  # expect_error(irt:::est_ability_4pm_nr_response_cpp(
  #   resp = resp_object, ip = ip, theta_range = c(-5, 5)),
  #   "Invalid 'resp'. 'resp' should have valid 'item_id' slot.")


})
