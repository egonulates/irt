
# library(testthat)

###############################################################################@
############################# item_analysis ####################################
###############################################################################@

test_that("Test item_analysis function", {
  n_item <- 10 # sample(8:12, 1)
  n_theta <- 100 # sample(100:200, 1)
  ip <- itempool(b = rnorm(n_item))
  theta <- setNames(rnorm(n_theta), paste0("Ex-", 1:n_theta))
  resp <- sim_resp(ip = ip, theta = theta)
  # Add some missing responses
  resp[sample(1:length(resp), round(length(resp)*.1))] <- NA
  # Run item analysis:
  ia <- item_analysis(resp = resp)
  # Calculate the p-value
  i <- sample(1:n_item, 1)
  pval <- sum(resp[, i], na.rm = TRUE)/sum(!is.na(resp[, i]))
  expect_identical(ia$pval[i], pval)


  # -------------------------------------------------------------------------- #
  # Function can run for Response_set objects
  n_item <- sample(10:20, 1)
  n_theta <- sample(100:200, 1)
  ip <- generate_ip(n = n_item)
  resp_set <- generate_resp_set(ip = ip, theta = rnorm(n_theta))
  ia <- item_analysis(resp = resp_set)
  expect_true(is.data.frame(ia))

  # Test 'stats'
  ia <- item_analysis(resp = resp_set, stats = "n")
  expect_true(all(c("item_id", "n") %in% colnames(ia)))
  expect_true(!any(c("pval", "pbis", "bis", "pbis_adj", "bis_adj") %in%
                     colnames(ia)))
  # no output, just item_id
  ia <- item_analysis(resp = resp_set, stats = c())
  expect_true(all(c("item_id") %in% colnames(ia)))
  expect_true(!any(c("pval", "n", "pbis", "bis", "pbis_adj", "bis_adj") %in%
                     colnames(ia)))

  ia <- item_analysis(resp = resp_set, stats = c("pval", "n", "pbis_adj"))
  expect_true(all(c("item_id","pval", "n", "pbis_adj") %in% colnames(ia)))
  expect_true(!any(c("pbis", "bis", "bis_adj") %in% colnames(ia)))

  # -------------------------------------------------------------------------- #
  # Item analysis with polytomous items, pvalues will have a range of 0-1
  # instead of 0-max_score()
  n_item <- sample(10:20, 1)
  n_theta <- sample(100:200, 1)
  ip <- generate_ip(model = sample(c("GRM", "3PL", "GRM", "3PL"), n_item, TRUE),
                    n_categories = sample(3:7, n_item, TRUE), )
  resp_set <- generate_resp_set(ip = ip, theta = rnorm(n_theta),
                                prop_missing = .2)
  resp_matrix <- as.matrix(resp_set)
  ia <- item_analysis(resp = resp_set, ip = ip, stats = c("pval", "pval_unadj"))
  # Check GRM item
  i <- sample(which(ip$model == "GRM"), 1)
  itm <- ip[[i]]
  expected_mean <- mean(resp_matrix[, itm$item_id == colnames(resp_matrix)],
                   na.rm =  TRUE)
  # Check pval
  expect_equal(
    object = ia[ia$item_id == itm$item_id, "pval", drop = TRUE],
    expected = expected_mean/max_score(itm), tolerance = 1e-8)
  expect_true(all(ia$pval <= 1))
  # Check pval_unadj
  expect_equal(
    object = ia[ia$item_id == itm$item_id, "pval_unadj", drop = TRUE],
    expected = expected_mean, tolerance = 1e-8)

  # Check 3PL item
  i <- sample(which(ip$model == "3PL"), 1)
  itm <- ip[[i]]
  expected <- mean(resp_matrix[, itm$item_id == colnames(resp_matrix)],
                   na.rm =  TRUE)
  observed <- ia[ia$item_id == itm$item_id, "pval", drop = TRUE]
  expect_identical(observed, expected, tolerance = 1e-8)

})

###############################################################################@
############################# distractor_analysis ##############################
###############################################################################@
test_that("Test distractor_analysis function", {
  n_item <- 10 # sample(8:12, 1)
  n_theta <- 50 # sample(100:200, 1)
  raw_resp <- matrix(sample(LETTERS[1:4], n_item * n_theta, replace = TRUE),
                     nrow = n_theta, ncol = n_item,
                     dimnames = list(paste0("Examinee-", 1:n_theta),
                                     paste0("Item-", 1:n_item)))
  # Add some missing responses
  raw_resp[sample(1:length(raw_resp), round(length(raw_resp)*.1))] <- NA
  key <- sample(LETTERS[1:4], n_item, replace = TRUE)
  da <- distractor_analysis(resp = raw_resp, key = key)
  temp <- unique(da[, c("item_id", "key")])
  expected <- temp$key[match(colnames(raw_resp), temp$item_id)]
  expect_identical(key, expected) # Check the key column

  # OBSOLETE: 2021-08-09
  # # Check whether responses scored correctly
  # for (i in 1:n_item) { # i is for item number
  #   j <- sample(1:n_theta, 1) # j is for examinee number
  #   expect_identical(1L * (raw_resp[j, i] == key[i]), da$scores[j, i])
  # }

  scores <- score_raw_resp(raw_resp, key)
  for (item_col in sample(1:n_item, 5)) {
    choice <- sample(key, 1)
    # Get the row to be checked
    temp <- da[(da$item_id == colnames(raw_resp)[item_col]) & (da$option == choice),]
    # Check the number of occurrences
    expect_identical(sum(!is.na(raw_resp[, item_col])), temp$n)
    # Check choice proportion
    expect_identical(sum(raw_resp[, item_col] == choice, na.rm = TRUE) /
                       sum(!is.na(raw_resp[, item_col])), temp$prop)
    # Check biserial correlation
    expect_identical(temp$bis,
                     biserial(score = 1 * (raw_resp[, item_col] == choice),
                              criterion =  rowSums(scores, na.rm = TRUE)))

    # Check distractor analysis with adjustment
    ts <- rowSums(scores[, -item_col], na.rm = TRUE)
    score <- (raw_resp[, item_col] == choice) * 1
    expect_identical(biserial(score = score, criterion = ts), temp$bis_adj)
    expect_identical(point_biserial(score = score, criterion = ts),
                     temp$pbis_adj)
  }

  # -------------------------------------------------------------------------- #
  # when key's provided via ip
  n_item <- sample(25:35, 1)
  n_theta <- sample(200:450, 1)
  ip <- generate_ip(n = n_item, model = "3PL")
  possible_options <- LETTERS[1:sample(4:6, 1)]
  ip$possible_options <- rep(list(possible_options), n_item)
  ip$key <- sample(possible_options, n_item, TRUE)
  theta <- rnorm(n_theta)
  resp_set <- generate_resp_set(ip = ip, theta = theta, prop_missing = .2)
  da <- distractor_analysis(resp = resp_set, ip = ip)
  raw_resp <- as.matrix(resp_set, output = "raw_response", ip = ip)

  criterion <- est_ability(resp = resp_set, ip = ip, method = "sum_score")$est
  for (i in sample(1:n_item, 10)) {
    # Check whether proportion of an option calculated correctly
    opt <- sample(possible_options, 1)
    temp <- da[(da$item_id == colnames(raw_resp)[i]) & (da$option == opt),]
    expected <- raw_resp[, i]
    expected <- expected[!is.na(expected)]
    expected <- sum((expected == opt) * 1) / length(expected)
    expect_identical(temp$prop, expected)

    # Check whether biserial correlations correctly calculated
    expected <- 1 * (raw_resp[, i] == opt)
    expected <- biserial(score = expected, criterion = criterion)
    expect_identical(temp$bis, expected)

    # Check whether point-biserial correlations correctly calculated
    expected <- 1 * (raw_resp[, i] == opt)
    expected <- point_biserial(score = expected, criterion = criterion)
    expect_identical(temp$pbis, expected)
  }
})



###############################################################################@
############################# biserial #########################################
###############################################################################@
test_that("Test biserial function", {
  # -------------------------------------------------------------------------- #
  # Check biserial correlation
  # The example is from page 72 of: Lewis-Beck, Michael S., Alan E. Bryman, and
  # Tim Futing Liao. 2004. The SAGE Encyclopedia of Social Science Research
  # Methods. Thousand Oaks, CA: SAGE Publications.
  score <- c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1,
             1, 1, 1, 0, 1, 1, 1, 1)
  total_score <- c(8, 12, 6, 12, 8, 8, 8, 11, 13, 4, 14, 13, 10, 9, 8, 33, 28,
                   29, 30, 29, 28, 33, 32, 32, 33, 34, 35, 34, 38, 37)
  expect_identical(biserial(score = score, criterion = total_score),
                   expected = 0.8449, tolerance = 0.001)

  # -------------------------------------------------------------------------- #
  # Check biserial and point-biserial correlation
  # The example is from Salkind, Rasmussen (2007) Encyclopedia of measurement
  # and statistics, pages 94-97
  score <- c(rep(0, 16), rep(1, 22))
  total_score <- c(87, 90, 94, 94, 97, 103, 103, 104, 106, 108, 109, 109, 109,
                   112, 119, 132, 100, 103, 103, 106, 112, 113, 114, 114, 118,
                   119, 120, 120, 124, 133, 135, 135, 136, 141, 155, 157, 159,
                   162)
  expect_identical(biserial(score = score, criterion = total_score),
                   expected = 0.703, tolerance = 0.015)
  expect_identical(biserial(score = score, criterion = total_score,
                            method = "point-biserial"),
                   expected = 0.557, tolerance = 0.001)

  # -------------------------------------------------------------------------- #
  # Check biserial and point-biserial correlation, biserial_brogden
  #
  # The example is from Kotz (2005) Encyclopedia of Statistical Sciences,
  # pages 577-580
  # This exact same example is also on SAS support page:
  #   https://support.sas.com/kb/24/991.html
  score <- c(1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0,
             0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1)
  total_score <- c(14.8, 13.8, 12.4, 10.1, 7.1, 6.1, 5.8, 4.6, 4.3, 3.5, 3.3,
                   3.2, 3, 2.8, 2.8, 2.5, 2.4, 2.3, 2.1, 1.7, 1.7, 1.5, 1.3,
                   1.3, 1.2, 1.2, 1.1, 0.8, 0.7, 0.6, 0.5, 0.2, 0.2, 0.1)
  expect_identical(biserial(score = score, criterion = total_score),
                   expected = 0.45369, tolerance = 0.0001)
  expect_identical(biserial(score = score, criterion = total_score,
                            method = "point-biserial"),
                   expected = 0.36149, tolerance = 0.0001)
  expect_identical(biserial(score = score, criterion = total_score,
                        method = "rank"),
                   expected = 0.4359, tolerance = 0.0001)
  expect_identical(biserial(score = score, criterion = total_score,
                            method = "brogden"),
                   expected = 0.50, tolerance = 0.01)

  # biserial_cpp(score = score, criterion = total_score,
  #              method = "clemans-lord")
  # biserial_cpp(score = score, criterion = total_score,
  #              method = "brogden")
  # biserial_cpp(score = score, criterion = total_score,
  #              method = "rank")
  # biserial_cpp(score = score, criterion = total_score,
  #              method = "default")
  # n <- 1e6
  # score = sample(0:1, n, replace = TRUE)
  # total_score = sample(10:31, n, replace = TRUE)
  # microbenchmark::microbenchmark(
  #   R = biserial(score = score, criterion = total_score),
  #   Cpp = biserial(score = score, criterion = total_score)
  #   )
})




###############################################################################@
############################# marginal_reliability_empirical ###################
###############################################################################@

test_that("marginal_reliability_empirical", {

  ip <- generate_ip(n = 15)
  true_theta <- rnorm(500)
  resp_set <- generate_resp_set(ip = ip, theta = true_theta)
  est <- est_ability(resp = resp_set, ip = ip, method = "eap")

  observed <- marginal_reliability_empirical(theta = est$est, se = est$se)
  expect_true(observed <= 1)


  expected <- marginal_reliability_empirical(ts_var = var(est$est), se = est$se)
  expect_identical(expected, observed)

})
