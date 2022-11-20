

############################################################################@###
################### ca_perf_cat ################################################
############################################################################@###
test_that("ca_perf_cat", {

  ip <- generate_ip(model = sample(c("GPCM", "2PL"), 10, TRUE))
  n_examinee <- 10
  true_theta <- sort(round(rnorm(n_examinee), 3))
  theta_cs <- c(-1, 0, 1.5)

  resp_set <- generate_resp_set(ip = ip, theta = true_theta, prop_missing = .2)
  dt <- data.frame(true_theta = true_theta)

  # Theta score
  dt$observed_theta <- round(est_ability(resp = resp_set, ip = ip)$est, 3)
  # Cut score
  raw_cs <- round(rsss(ip = ip, scale_score = theta_cs))
  # Raw score
  dt$true_raw_score <- round(rsss(ip = ip, scale_score = true_theta))
  dt$observed_raw_score <- est_ability(resp = resp_set,
                                       method = "sum_score")$est

  cat1 <- ca_perf_cat(theta = dt$true_theta, theta_cs = theta_cs)
  cat2 <- ca_perf_cat(theta = dt$true_theta, raw_cs = raw_cs, ip = ip)
  cat3 <- ca_perf_cat(raw_score = dt$observed_raw_score, raw_cs = raw_cs,
                      ip = ip)
  # cbind(dt, cat1, cat2, cat3)
  expect_true(all(cat1 %in% seq(length(theta_cs) + 1)))
  expect_true(all(cat2 %in% seq(length(theta_cs) + 1)))
  expect_true(all(cat3 %in% seq(length(theta_cs) + 1)))


  # -------------------------------------------------------------------------- #
  ### Example 2 ###
  ip <- generate_ip(model = sample(c("GPCM", "2PL"), 10, TRUE))
  theta <- seq(-3, 3, .2)
  theta <- rnorm(50)
  theta_cs <- c(-1, 0, 1.5)
  raw_cs <- rsss(ip = ip, scale_score = theta_cs)
  raw_score <- round(rsss(ip = ip, scale_score = theta))

  tt <- ca_perf_cat(theta = theta, theta_cs = theta_cs)
  tr <- ca_perf_cat(theta = theta, raw_cs = raw_cs, ip = ip)
  rr <- ca_perf_cat(raw_score = raw_score, raw_cs = raw_cs)
  rt <- ca_perf_cat(raw_score = raw_score, theta_cs = theta_cs, ip = ip)
  # cbind(theta, raw_score, tt, tr, rr, rt)

  expect_true(all(tt %in% seq(length(theta_cs) + 1)))
  expect_true(all(tr %in% seq(length(theta_cs) + 1)))
  expect_true(all(rr %in% seq(length(theta_cs) + 1)))
  expect_true(all(rt %in% seq(length(theta_cs) + 1)))
  expect_equal(tt, tr)
  expect_equal(rt, rr)

})

# test_that("ca_irt_recursive", {
#
#   ip <- generate_ip(model = sample(c("GPCM", "2PL"), 20, TRUE))
#   n_examinee <- 1000
#   theta <- rnorm(n_examinee)
#   resp_set <- generate_resp_set(ip = ip, theta = theta, prop_missing = .2)
#   theta_est <- est_ability(resp = resp_set, ip = ip, method = "eap")
#   se <- theta_est$se
#   theta_est <- theta_est$est
#   cut_scores <- c(-1, 0, 1.5)
#   cut_scores <- c(-2, 0, 2.5)
#
#   ca_irt_recursive(ip = ip, theta = theta, theta_cs = cut_scores)
#   ca_irt_rudner(theta = theta_est, se = se, cut_scores = cut_scores)
#
# })
#
#
#
# test_that("ca_irt_rudner", {
#
#   ip <- generate_ip(model = sample(c("GPCM", "2PL"), 20, TRUE))
#   n_examinee <- 1000
#   theta <- rnorm(n_examinee)
#   resp_set <- generate_resp_set(ip = ip, theta = theta, prop_missing = .2)
#   theta_est <- est_ability(resp = resp_set, ip = ip, method = "eap")
#   se <- theta_est$se
#   theta_est <- theta_est$est
#   cut_scores <- c(-1, 0, 1.5)
#   cut_scores <- c(-2, 0, 2.5)
#   ca_irt_rudner(theta = theta_est, se = se, cut_scores = cut_scores)
#
# })
#
#
# test_that("ca_irt_guo", {
#
#   ip <- generate_ip(model = sample(c("GPCM", "2PL"), 20, TRUE))
#   n_examinee <- 10
#   theta <- rnorm(n_examinee)
#   resp_set <- generate_resp_set(ip = ip, theta = theta, prop_missing = .2)
#   theta_est <- est_ability(resp = resp_set, ip = ip, method = "eap")
#   theta_est <- theta_est$est
#   cut_scores <- c(-1, 0, 1.5)
#   cut_scores <- c(-2, 0, 2.5)
#
#   ca_irt_guo(ip = ip, resp = resp_set, cut_scores = cut_scores,
#              theta = theta_est)
#
#
# })



############################################################################@###
################### classification_agreement_index #############################
############################################################################@###
test_that("classification_agreement_index", {

  cs <- c(3.5, 7.5) # Cut score
  #           Level 1            Level 2         Level 3
  ts <- c(1, 1, 2, 2, 3, 3,   4, 5, 6, 6, 7,   8, 8, 9, 9) # True score
  es <- c(1, 2, 8, 5, 3, 6,   3, 6, 8, 2, 7,   2, 5, 9, 9) # Estimated score
  result <- classification_agreement_index(true_score = ts,
                                           estimated_score = es,
                                           cut_scores = cs)
  expect_equal(result$ca_table[1, 1], 3)
  expect_equal(result$ca_table[1, 2], 2)
  expect_equal(result$ca_table[1, 3], 1)
  expect_equal(result$ca_table[2, 1], 2)
  expect_equal(result$ca_table[2, 2], 2)
  expect_equal(result$ca_table[2, 3], 1)
  expect_equal(result$ca_table[3, 1], 1)
  expect_equal(result$ca_table[3, 2], 1)
  expect_equal(result$ca_table[3, 3], 2)

  expect_equal(result$ca_table_prop[1, 1], 3/15)
  expect_equal(result$ca_table_prop[1, 2], 2/15)
  expect_equal(result$ca_table_prop[1, 3], 1/15)
  expect_equal(result$ca_table_prop[2, 1], 2/15)
  expect_equal(result$ca_table_prop[2, 2], 2/15)
  expect_equal(result$ca_table_prop[2, 3], 1/15)
  expect_equal(result$ca_table_prop[3, 1], 1/15)
  expect_equal(result$ca_table_prop[3, 2], 1/15)
  expect_equal(result$ca_table_prop[3, 3], 2/15)

  expect_equal(result$ca, sum(diag(result$ca_table))/15)


  # -------------------------------------------------------------------------- #
  ip <- generate_ip(model = sample(c("GPCM", "2PL"), 20, TRUE))
  n_examinee <- 1000
  true_theta <- rnorm(n_examinee)
  observed_theta <- true_theta + runif(n_examinee, -.5, .5)
  theta_cs <- c(-1, 0, 1.5)
  raw_cs <- round(rsss(ip = ip, scale_score = theta_cs))
  true_raw_score <- round(rsss(ip = ip, scale_score = true_theta))
  observed_raw_score <- round(rsss(ip = ip, scale_score = observed_theta))

  # Theta scores
  result <- classification_agreement_index(true_score = true_theta,
                                           estimated_score = observed_theta,
                                           cut_scores = theta_cs)
  expect_true(result$ca > 0.5 & result$ca < 1)
  expect_true(all(result$ca_table >= 0 & result$ca_table < n_examinee))
  expect_true(all(result$ca_table_prop >= 0 & result$ca_table_prop <= 1))
  # Summed scores
  result <- classification_agreement_index(true_score = true_raw_score,
                                 estimated_score = observed_raw_score,
                                 cut_scores = raw_cs)
  expect_true(result$ca > 0.5 & result$ca < 1)
  expect_true(all(result$ca_table >= 0 & result$ca_table < n_examinee))
  expect_true(all(result$ca_table_prop >= 0 & result$ca_table_prop <= 1))

  # Add labels
  cat_labels = c("Unsatisfactory", "Basic", "Mastery", "Advanced")
  result <- classification_agreement_index(true_score = true_theta,
                                 estimated_score = observed_theta,
                                 cut_scores = theta_cs,
                                 cat_labels = cat_labels)
  expect_true(result$ca > 0.5 & result$ca < 1)
  expect_true(all(result$ca_table >= 0 & result$ca_table < n_examinee))
  expect_true(all(result$ca_table_prop >= 0 & result$ca_table_prop <= 1))

  expect_equal(colnames(result$ca_table), cat_labels)
  expect_equal(rownames(result$ca_table), cat_labels)

})


############################################################################@###
################### classification_indices #####################################
############################################################################@###


test_that("classification_indices", {

  ip <- generate_ip(model = sample(c("GPCM", "2PL"), 20, TRUE))
  n_examinee <- 100

  examinee_ids <- paste0("Subj-", 1:n_examinee)
  true_theta <- setNames(rnorm(n_examinee), examinee_ids)
  resp_set <- generate_resp_set(ip = ip, theta = true_theta, prop_missing = .2)
  theta_est <- est_ability(resp = resp_set, ip = ip, method = "eap")
  se <- theta_est$se
  theta_est <- theta_est$est
  raw_score <- est_ability(resp = resp_set, method = "sum_score")$est

  # Cut score
  theta_cs <- c(-1, 0, 1.5)
  raw_cs <- round(rsss(ip = ip, scale_score = theta_cs))

  # Rudner based indices:
  result <- classification_indices(method = "rudner", theta = theta_est,
                                   se = se, theta_cs = theta_cs)
  expect_true(result$ca > 0.5 & result$ca < 1)
  expect_true(result$cc > 0.5 & result$cc < 1)
  expect_true(all(result$category_prob > 0.1 & result$category_prob < 1))
  expect_true(all(result$ind_cs_ca > 0.5 & result$ind_cs_ca < 1))
  expect_equal(names(result$category_prob), examinee_ids)

  # Guo based indices:
  result <- classification_indices(method = "guo", ip = ip, resp = resp_set,
                                   theta = theta_est, theta_cs = theta_cs)
  expect_true(result$ca > 0.5 & result$ca < 1)
  expect_true(result$cc > 0.5 & result$cc < 1)
  expect_true(all(result$category_prob > 0.1 & result$category_prob < 1))
  expect_true(all(result$ind_cs_ca > 0.5 & result$ind_cs_ca < 1))
  expect_equal(names(result$category_prob), examinee_ids)

  # Recursive method based indices:
  result <- classification_indices(method = "recursive", ip = ip,
                                   theta = theta_est, theta_cs = theta_cs)
  expect_true(result$ca > 0.5 & result$ca < 1)
  expect_true(result$cc > 0.5 & result$cc < 1)
  expect_true(all(result$category_prob > 0.1 & result$category_prob < 1))
  expect_true(all(result$ind_cs_ca > 0.5 & result$ind_cs_ca < 1))
  expect_equal(names(result$category_prob), examinee_ids)

  # Use raw score cut scores with recursive method
  result <- classification_indices(method = "recursive", ip = ip,
                                   theta = theta_est, raw_cs = raw_cs)
  expect_true(result$ca > 0.5 & result$ca < 1)
  expect_true(result$cc > 0.5 & result$cc < 1)
  expect_true(all(result$category_prob > 0.1 & result$category_prob < 1))
  expect_true(all(result$ind_cs_ca > 0.5 & result$ind_cs_ca < 1))
  expect_equal(names(result$category_prob), examinee_ids)

})


############################################################################@###
################### kappa_coef #################################################
############################################################################@###


test_that("kappa_coef", {
  # Examples are from:
  # Julius Sim, Chris C Wright, The Kappa Statistic in Reliability Studies: Use,
  # Interpretation, and Sample Size Requirements, Physical Therapy, Volume 85,
  # Issue 3, 1 March 2005, Pages 257–268, https://doi.org/10.1093/ptj/85.3.257

  # -------------------------------------------------------------------------- #
  # Sim and Wright (2005), Table 1
  dtf <- data.frame(r1 = c(rep("Relevant", 22), rep("Relevant", 2),
                           rep("Not Relevant", 4), rep("Not Relevant", 11)),
                    r2 = c(rep("Relevant", 22), rep("Not Relevant", 2),
                           rep("Relevant", 4), rep("Not Relevant", 11)))
  observed <- kappa_coef(dtf)
  expect_equal(observed, 0.67, tolerance = 0.01)

  # -------------------------------------------------------------------------- #
  # Sim and Wright (2005), Table 2
  pain_raw <- data.frame(t1 = c(rep("No Pain", 15 + 3 + 1 + 1),
                                rep("Mild Pain", 4 + 18 + 3 + 2),
                                rep("Moderate Pain", 4 + 5 + 16 + 4),
                                rep("Severe Pain", 1 + 2 + 4 + 17)),
                         t2 = c(rep("No Pain", 15), rep("Mild Pain", 3),
                                rep("Moderate Pain", 1), rep("Severe Pain", 1),
                                rep("No Pain", 4), rep("Mild Pain", 18),
                                rep("Moderate Pain", 3), rep("Severe Pain", 2),
                                rep("No Pain", 4), rep("Mild Pain", 5),
                                rep("Moderate Pain", 16), rep("Severe Pain", 4),
                                rep("No Pain", 1), rep("Mild Pain", 2),
                                rep("Moderate Pain", 4), rep("Severe Pain", 17))
    )
  observed <- kappa_coef(pain_raw)
  expect_equal(observed, 0.55, tolerance = 0.01)

  # Make them ordered
  ordered_pain_levels <- c("No Pain", "Mild Pain", "Moderate Pain",
                           "Severe Pain")
  pain_ordered <- data.frame(
    t1 = factor(pain_raw$t1, levels = ordered_pain_levels, ordered = TRUE),
    t2 = factor(pain_raw$t2, levels = ordered_pain_levels, ordered = TRUE))

  # Test 'unweighted'
  observed <- kappa_coef(pain_ordered)
  expect_equal(observed, 0.55, tolerance = 0.01)
  # Test 'linear'
  observed <- kappa_coef(pain_ordered, weights = "linear")
  expect_equal(observed, 0.61, tolerance = 0.01)
  # Test 'quadratic'
  observed <- kappa_coef(pain_ordered, weights = "quadratic")
  expect_equal(observed, 0.67, tolerance = 0.01)

  # -------------------------------------------------------------------------- #
  # Sim and Wright (2005), Table 2, convert to integers
  dtf <- sapply(pain_ordered, function(x) as.integer(x))
  # Test 'linear'
  observed <- kappa_coef(dtf, weights = "linear")
  expect_equal(observed, 0.61, tolerance = 0.01)
  # Test 'quadratic'
  observed <- kappa_coef(dtf, weights = "quadratic")
  expect_equal(observed, 0.67, tolerance = 0.01)

  # -------------------------------------------------------------------------- #
  # Function works with tibble
  pain_tibble <- tibble::as_tibble(pain_ordered)
  # Test 'unweighted'
  observed <- kappa_coef(x = pain_tibble)
  expect_equal(observed, 0.55, tolerance = 0.01)
  # Test 'linear'
  observed <- kappa_coef(pain_tibble, weights = "linear")
  expect_equal(observed, 0.61, tolerance = 0.01)
  # Test 'quadratic'
  observed <- kappa_coef(pain_tibble, weights = "quadratic")
  expect_equal(observed, 0.67, tolerance = 0.01)

  # -------------------------------------------------------------------------- #
  # Function works with matrix
  pain_matrix <- as.matrix(pain_ordered)
  observed <- kappa_coef(x = pain_matrix)
  expect_equal(observed, 0.55, tolerance = 0.01)

  # -------------------------------------------------------------------------- #
  # TODO: Function works with matrix with ordered factors
  # pain_matrix <- as.matrix(pain_ordered)
  # pain_matrix <- apply(pain_matrix, 2, factor, levels = ordered_pain_levels,
  #                      ordered = TRUE)
  # # Test 'linear'
  # observed <- kappa_coef(pain_matrix, weights = "linear")
  # expect_equal(observed, 0.61, tolerance = 0.01)
  # # Test 'quadratic'
  # observed <- kappa_coef(pain_matrix, weights = "quadratic")
  # expect_equal(observed, 0.67, tolerance = 0.01)


  # -------------------------------------------------------------------------- #
  # Sim and Wright (2005), Table 3
  spinal_pain <- data.frame(
    t1 = c(rep("Derangement syndrome", 22 + 10 + 2),
           rep("Dysfunctional syndrome", 6 + 27 + 11),
           rep("Postural syndrome", 2 + 5 + 17)),
    t2 = c(rep("Derangement syndrome", 22), rep("Dysfunctional syndrome", 10),
           rep("Postural syndrome", 2),
           rep("Derangement syndrome", 6), rep("Dysfunctional syndrome", 27),
           rep("Postural syndrome", 11),
           rep("Derangement syndrome", 2), rep("Dysfunctional syndrome", 5),
           rep("Postural syndrome", 17)
           )
    )
  observed <- kappa_coef(spinal_pain, weights = "unweighted")
  expect_equal(observed, 0.46, tolerance = 0.01)

  # -------------------------------------------------------------------------- #
  # Check whether missing categories successfully run for factors
  dtf <- pain_raw[sample(nrow(pain_raw)), ]
  dtf$t2[dtf$t2 == "Mild Pain"] <- "No Pain"
  dtf$t1[dtf$t1 == "Severe Pain"] <- "Moderate Pain"

  dtf_ordered <- data.frame(t1 = factor(dtf$t1, levels = c(
    "No Pain", "Mild Pain", "Moderate Pain", "Severe Pain"), ordered = TRUE),
    t2 = factor(dtf$t2, levels = c("No Pain", "Mild Pain", "Moderate Pain",
                                   "Severe Pain"), ordered = TRUE))
  observed <- kappa_coef(dtf, weights = "unweighted")
  expected <- kappa_coef(dtf_ordered, weights = "unweighted")
  expect_equal(observed, expected)

  # Check missing integers makes any difference
  dtf_integer <- sapply(dtf_ordered, as.integer)
  observed <- kappa_coef(dtf_integer, weights = "unweighted")
  expect_equal(observed, expected)

  # -------------------------------------------------------------------------- #
  # Check whether missing categories successfully run for integers
  dtf <- pain_raw[sample(nrow(pain_raw)), ]
  dtf$t2[dtf$t2 == "Mild Pain"] <- "No Pain"
  dtf$t1[dtf$t1 == "Severe Pain"] <- "Moderate Pain"

  dtf_ordered <- data.frame(
    t1 = factor(dtf$t1, levels = ordered_pain_levels, ordered = TRUE),
    t2 = factor(dtf$t2, levels = ordered_pain_levels, ordered = TRUE))
  dtf_integer <- sapply(dtf_ordered, as.integer)

  observed <- kappa_coef(dtf, weights = "unweighted")
  expected <- kappa_coef(dtf_integer, weights = "unweighted")
  expect_equal(observed, expected)

  # Test 'linear'
  observed <- kappa_coef(dtf_integer, weights = "linear")
  expected <- kappa_coef(dtf_ordered, weights = "linear")
  expect_equal(observed, expected)
  # Test 'quadratic'
  observed <- kappa_coef(dtf_integer, weights = "quadratic")
  expected <- kappa_coef(dtf_ordered, weights = "quadratic")
  expect_equal(observed, expected)


  # -------------------------------------------------------------------------- #
  # Rows with Missing values are ignored
  dtf_missing <- pain_ordered
  rows_with_missing <- sample(1:nrow(dtf_missing), 4)
  for (i in rows_with_missing)
    dtf_missing[i, sample(1:2, 1)] <- NA
  dtf_missing_rows <- dtf_missing[-rows_with_missing, ]

  observed <- kappa_coef(dtf_missing, weights = "unweighted")
  expected <- kappa_coef(dtf_missing_rows, weights = "unweighted")
  expect_equal(observed, expected)
  observed <- kappa_coef(dtf_missing, weights = "quadratic")
  expected <- kappa_coef(dtf_missing_rows, weights = "quadratic")
  expect_equal(observed, expected)

  # -------------------------------------------------------------------------- #
  # If there is only one category and all categories are equal, there is
  # perfect aggreement
  dtf <- data.frame(r1 = rep(1, 30), r2 = rep(1, 30))
  observed <- kappa_coef(dtf, weights = "unweighted")
  expect_equal(observed, 1)

  # -------------------------------------------------------------------------- #

})

