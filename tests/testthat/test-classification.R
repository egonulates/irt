

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


  ##########################################################################@###
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
