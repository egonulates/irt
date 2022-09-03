
###############################################################################@
############################# mean (Item) @#####################################
###############################################################################@

test_that("mean - Item", {

  # -------------------------------------------------------------------------- #
  # Regular mean function with numbers works
  x <- sample(2:100, 3)
  expect_identical(mean(x), sum(x)/3)

  ##  Single theta
  # -------------------------------------------------------------------------- #
  # Check all 4PM  Model
  for (model  in c("Rasch", "1PL", "2PL", "3PL", "4PL", "GPCM", "GRM", "GPCM2",
                   "PCM")) {
    itm <- generate_item(model = model)
    theta <- rnorm(1)
    p <- prob_bare_item_cpp(theta = theta, item = itm, derivative = 0,
                            expected_value = TRUE)
    # p <- prob(ip = itm, theta = theta, expected_value = TRUE)
    expect_identical(mean(x = itm, theta = theta), p)
  }

  # -------------------------------------------------------------------------- #
  # theta argument can be unnamed
  expect_identical(mean(x = itm, theta = theta),
                   mean(x = itm, theta))

  ##  Multiple theta
  # -------------------------------------------------------------------------- #
  # Check all 4PM  Model
  for (model  in c("Rasch", "1PL", "2PL", "3PL", "4PL", "GPCM", "GRM", "GPCM2",
                   "PCM")) {
    itm <- generate_item(model = model)
    theta <- rnorm(sample(2:20, 1))
    p <- sapply(theta, prob_bare_item_cpp, item = itm, derivative = 0,
                expected_value = TRUE)
    # p <- prob(ip = itm, theta = theta, expected_value = TRUE)
    expect_identical(mean(x = itm, theta = theta), p)
  }

  # ---------------------------------------------------------------------------#
  # expected_value - single theta
  n_categories <- sample(3:8, 1)
  item <- generate_item("GRM", n_categories = n_categories)
  theta <- rnorm(1)
  p <- prob(item, theta = theta)
  p_expected <- mean(item, theta = theta)
  expect_type(p_expected, "double")
  expect_identical(length(p_expected), 1L)
  expect_identical(round(p_expected, 10),
                   round(sum(p*(0:(n_categories - 1))), 10))

  # ---------------------------------------------------------------------------#
  # expected_value - multiple thetas
  n_categories <- sample(3:8, 1)
  n_theta <- sample(10:20, 1)
  item <- generate_item("GRM", n_categories = n_categories)
  theta <- rnorm(n_theta)
  p <- prob(item, theta = theta)
  p_expected <- mean(item, theta = theta)
  expect_type(p_expected, "double")
  expect_identical(length(p_expected), n_theta)
  for (i in 1:n_theta)
    expect_identical(round(p_expected[i], 10),
                     round(sum(p[i, ]*(0:(n_categories - 1))), 10))
})



###############################################################################@
############################# mean (ItemPool) @#################################
###############################################################################@

test_that("mean - Itempool", {
  ##  Single theta
  # -------------------------------------------------------------------------- #
  # Check all 4PM  Model
  for (model  in c("Rasch", "1PL", "2PL", "3PL", "4PL", "GPCM", "GRM", "GPCM2",
                   "PCM")) {
    ip <- generate_ip(model = model)
    theta <- rnorm(1)
    # p <- prob(ip = ip, theta = theta, expected_value = TRUE)
    p <- setNames(prob_bare_itempool_cpp(theta = theta, ip = ip, derivative = 0,
                                         expected_value = TRUE)[, 1], ip$item_id)
    expect_identical(mean(x = ip, theta), p)
  }

  ##  Multiple theta
  # -------------------------------------------------------------------------- #
  # Check all 4PM  Model
  for (model  in c("Rasch", "1PL", "2PL", "3PL", "4PL", "GPCM", "GRM", "GPCM2",
                   "PCM")) {
    ip <- generate_ip(model = model)
    theta <- rnorm(sample(4:20, 1))
    # p <- prob(ip = ip, theta = theta, expected_value = TRUE)
    i <- sample(1:length(ip), 1)
    j <- sample(1:length(theta), 1)
    expect_identical(mean(x = ip, theta)[j, i],
                     setNames(mean(x = ip[[i]], theta[j]), ip$item_id[i]))
  }
  # ---------------------------------------------------------------------------#
  # single theta
  ip <- c(generate_ip(n = 2),
          generate_testlet(testlet_id = "t1",
                           item_models = c("3PL", "GRM", "GPCM", "GRM", "2PL"),
                           n_categories = c(2, 3, 6, 7, 2)),
          generate_testlet(n = 3, testlet_id = "t2"))
  theta <- rnorm(1)
  p <- prob(ip, theta = theta)
  p_expected <- mean(ip, theta = theta)
  p_expected <- setNames(unlist(p_expected, use.names = FALSE), ip$resp_id)
  for (i in 1:ip$n$items)
    expect_identical(
      round(p_expected[i], 8),
      setNames(round(sum(0:(ncol(p) - 1) * p[i, ], na.rm = TRUE), 8),
               ip$resp_id[i]))

  # ---------------------------------------------------------------------------#
  # expected_value = TRUE, multiple theta
  ip <- c(generate_ip(n = 2),
          generate_testlet(testlet_id = "t1",
                           item_models = c("3PL", "GRM", "GPCM", "GRM", "2PL"),
                           n_categories = c(2, 3, 6, 7, 2)),
          generate_testlet(n = 3, testlet_id = "t2"))
  n_theta <- sample(11:20, 1)
  theta <- rnorm(n_theta)
  p_expected <- mean(ip, theta = theta)
  p_expected <- do.call(cbind, p_expected)
  i <- sample(1:10, 1) # select random item
  j <- sample(1:n_theta, 1) # select random examinee
  expect_identical(mean(ip$items[[i]], theta = theta[j]),
                   p_expected[j, i], ignore_attr = TRUE)

})


###############################################################################@
############################# mean (Testlet) @##################################
###############################################################################@

test_that("mean - Testlet", {
  ##  Single theta
  # -------------------------------------------------------------------------- #
  t1 <- generate_testlet()
  theta <- rnorm(1)
  expect_identical(mean(t1, theta), mean(t1@item_list, theta))

  # -------------------------------------------------------------------------- #
  ##  Multiple theta
  t1 <- generate_testlet()
  theta <- rnorm(sample(2:10, 1))
  expect_identical(mean(t1, theta), mean(t1@item_list, theta))

})
