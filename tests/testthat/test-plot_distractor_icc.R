# library(testthat)


###############################################################################@
############################# plot_distractor_icc  #############################
###############################################################################@
test_that("Test plot_distractor_icc", {
  # Without an item pool or theta
  n_item <- 40 # sample(8:12, 1)
  n_theta <- 10000 # sample(100:200, 1)
  raw_resp <- matrix(sample(LETTERS[1:4], n_item * n_theta, replace = TRUE),
                     nrow = n_theta, ncol = n_item,
                     dimnames = list(paste0("Examinee-", 1:n_theta),
                                     paste0("Item_", 1:n_item)))
  key <- sample(LETTERS[1:4], n_item, replace = TRUE)

  expect_silent(p <- plot_distractor_icc(
    raw_resp, item = sample(sequence(n_item), 1), key = key, bins = 10))
  expect_s3_class(p, 'ggplot'); rm(p)
  # Base R graphics
  expect_silent(plot_distractor_icc(
    raw_resp, item = sample(sequence(n_item), 1), key = key, bins = 10,
    base_r_graph = TRUE))

  expect_message(p <- plot_distractor_icc(
    raw_resp, item = 3, key = key, bins = 10, x_axis_scale = "criterion"))
  expect_s3_class(p, 'ggplot'); rm(p)


  # -------------------------------------------------------------------------- #
  # Supply Response set with raw responses
  n_item <- 40 # sample(8:12, 1)
  n_theta <- 10000 # sample(100:200, 1)
  theta <- rnorm(n_theta)
  ip <- generate_ip(model = "3PL", n = n_item)
  resp_set <- generate_resp_set(ip = ip, theta = theta, prop_missing = .2)
  # Check whether scores and raw responses correspond
  rr <- as.matrix(resp_set, output = "raw_response", ip = ip)
  ss <- as.matrix(resp_set, output = "score", ip = ip)
  for (i in sample(n_item, 2)) {
    expect_equal(mean(rr[, i] == ip$key[i], na.rm = TRUE),
                 mean(ss[, i] == 1, na.rm = TRUE))
  }



  # temp <- data.frame(theta = theta, score = ss[, i], raw_response = rr[, i],
  #                    correct = rr[, i] == ip$key[i])
  # temp <- temp[!is.na(temp$score), ]
  # temp$bin <- cut(temp$theta, breaks = 10)
  # table(temp$bin, temp$score)

  # gd <- get_data_plot_distractor_icc(raw_resp = resp_set, item = 3,
  #                                    criterion = theta, ip = ip)
  # gd <- get_data_plot_distractor_icc(raw_resp = resp_set, item = 3,
  #                                    # x_axis_scale = "percent",
  #                                    x_axis_scale = "number",
  #                                    # x_axis_scale = "criterion",
  #                                    criterion = theta, ip = ip)

  # x_axis_scale = "criterion"
  expect_silent(p <- plot_distractor_icc(
    resp_set, item = sample(sequence(n_item), 1), criterion = theta, ip = ip,
    x_axis_scale = "criterion"))
  expect_s3_class(p, 'ggplot'); rm(p)
  # Base R graphics
  expect_silent(p <- plot_distractor_icc(
    resp_set, item = sample(sequence(n_item), 1), criterion = theta, ip = ip,
    x_axis_scale = "criterion", base_r_graph = TRUE))

  # x_axis_scale = "criterion", Add ICC
  expect_silent(p <- plot_distractor_icc(
    resp_set, item = sample(sequence(n_item), 1), criterion = theta, ip = ip,
    x_axis_scale = "criterion", add_icc = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)
  # Base R graphics
  expect_silent(p <- plot_distractor_icc(
    resp_set, item = sample(sequence(n_item), 1), criterion = theta, ip = ip,
    x_axis_scale = "criterion", add_icc = TRUE, base_r_graph = TRUE))


  # x_axis_scale = "percent"
  expect_silent(p <- plot_distractor_icc(
    resp_set, item = sample(sequence(n_item), 1), criterion = theta, ip = ip,
    x_axis_scale = "percent"))
  expect_s3_class(p, 'ggplot'); rm(p)
  # Base R graphics
  expect_silent(p <- plot_distractor_icc(
    resp_set, item = sample(sequence(n_item), 1), criterion = theta, ip = ip,
    x_axis_scale = "percent", base_r_graph = TRUE))

  # x_axis_scale = "number"
  expect_silent(p <- plot_distractor_icc(
    resp_set, item = sample(sequence(n_item), 1), criterion = theta, ip = ip,
    x_axis_scale = "number"))
  expect_s3_class(p, 'ggplot'); rm(p)
  # Base R graphics
  expect_silent(p <- plot_distractor_icc(
    resp_set, item = sample(sequence(n_item), 1), criterion = theta, ip = ip,
    x_axis_scale = "number", base_r_graph = TRUE))

  # raw_resp = resp_set
  # item = 3
  # bins = 10
  # key = NULL
  # criterion = theta
  # suppress_plot = TRUE
  # x_axis_scale = NULL
  # add_icc = TRUE
  # x_lim = NULL
  # n_dodge = 1
  # title = "Distractor Graph"
  # get_data_plot_distractor_icc <- irt:::get_data_plot_distractor_icc
  # score_raw_resp <- irt:::score_raw_resp
  # .get_x_axis_scale <- irt:::.get_x_axis_scale



  # -------------------------------------------------------------------------- #
  # Plot with ip and theta
  n_item <- 40 # sample(8:12, 1)
  n_theta <- 4000 # sample(100:200, 1)
  theta <- rnorm(n_theta)
  ip <- generate_ip(n = n_item)
  resp <- sim_resp(ip = ip, theta = theta, prop_missing = .1)
  key <- sample(LETTERS[1:4], n_item, replace = TRUE)
  key_matrix <- matrix(rep(key, n_theta), ncol = n_item, byrow = TRUE)
  raw_resp <- ifelse(
    is.na(resp), NA,
    ifelse(resp == 1, key_matrix,
           matrix(sapply(key_matrix,
                         function(x) sample(setdiff(LETTERS[1:4], x), 1)),
                  ncol = n_item, byrow = TRUE)
           ))
  expect_silent(p <- plot_distractor_icc(
    raw_resp, item = sample(sequence(n_item), 1), key = key, bins = 10,
    criterion = theta))
  expect_s3_class(p, 'ggplot'); rm(p)
  # Base R graphics
  expect_silent(plot_distractor_icc(
    raw_resp, item = sample(sequence(n_item), 1), key = key, bins = 10,
    criterion = theta, base_r_graph = TRUE))

  expect_silent(p <- plot_distractor_icc(
    raw_resp, item = sample(sequence(n_item), 1), key = key, bins = 10,
    criterion = theta, x_axis_scale = "percent"))
  expect_s3_class(p, 'ggplot'); rm(p)
  # Base R graphics
  expect_silent(plot_distractor_icc(
    raw_resp, item = sample(sequence(n_item), 1), key = key, bins = 10,
    criterion = theta, x_axis_scale = "percent", base_r_graph = TRUE))


  # -------------------------------------------------------------------------- #
  # 'title' can be NULL to suppress the graph title
  expect_silent(p <- plot_distractor_icc(
    raw_resp, item = sample(sequence(n_item), 1), key = key, bins = 10,
    title = NULL, criterion = theta))
  expect_silent(plot_distractor_icc(
    raw_resp, item = sample(sequence(n_item), 1), key = key, bins = 10,
    title = NULL,
    criterion = theta, x_axis_scale = "percent", base_r_graph = TRUE))
})
