
test_that("kernel_smoothing", {

  ip <- generate_ip(model = "3PL", n = 50)
  true_theta <- rnorm(10000)
  resp <- sim_resp(ip = ip, theta = true_theta, prop_missing = 0.3)

  kern_output <- ks(resp, criterion = true_theta)

  expect_true(all(kern_output$icc <= 1))
  # The column names of icc is the same as item ids
  expect_identical(colnames(kern_output$icc), ip$item_id)

  # -------------------------------------------------------------------------- #
  # The column names of icc is the same as item ids even when ids are all digits
  n_item <- sample(12:24, 1)
  ip <- generate_ip(model = "3PL", n = n_item, item_id = 1:n_item)
  true_theta <- rnorm(10000)
  resp_set <- generate_resp_set(ip = ip, theta = true_theta)
  resp_matrix <- as.matrix(resp_set, ip = ip)

  ks_resp <- ks(resp_matrix, criterion = true_theta)

  expect_true(all(ks_resp$icc <= 1))
  expect_identical(colnames(ks_resp$icc), ip$item_id)

  # A Response_set can be used in addition to matrix
  ks_resp_set <- ks(resp_set, criterion = true_theta)
  expect_identical(ks_resp, ks_resp_set)

})
