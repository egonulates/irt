
# library(rbenchmark, testthat)

############################################################################@###
######################## sim_resp_cpp ##########################################
############################################################################@###
test_that("sim_resp_cpp", {
  # Check whether the proportion of correct response is approximately the same
  theta <- rnorm(1, 0, 0.5)
  item <- item(a = 1, b = rnorm(1, 0, 1))
  n_sim <- 10000
  output <- sapply(1:n_sim, function(x) sim_resp_bare_cpp(theta, item))
  # sum(output) / n_sim; prob(item, theta)
  expect_identical(sum(output) / n_sim, unname(prob(item, theta)[, 2]),
                   tolerance = 0.02)
})



############################################################################@###
######################## info_testlet_bare_cpp  ################################
############################################################################@###
test_that("info_testlet_bare_cpp", {
  # A single item testlet with resp is not NULL
  theta <- rnorm(1, 0, 0.5)
  t1 <- testlet(itempool(b = rnorm(1), item_id = c("t1-i1")), testlet_id = "t1")
  resp <- sim_resp(ip = t1, theta = rnorm(1))
  observed <- info_testlet_bare_cpp(theta = theta, testlet = t1,
                                    observed = FALSE, resp = resp)
  expected <- info(ip = t1@item_list, theta = theta, tif = TRUE)
  expect_true(inherits(expected, "matrix"))
  expect_identical(observed, as.vector(expected))

  # -------------------------------------------------------------------------- #
  # A two item testlet with resp is not NULL
  t1 <- testlet(itempool(b = rnorm(2), item_id = c("t1-i1", "t1-i2")),
                   testlet_id = "t1")
  resp <- sim_resp(ip = t1, theta = rnorm(1))
  observed <- info_testlet_bare_cpp(theta = theta, testlet = t1,
                                    observed = FALSE, resp = resp)
  expected <- info(ip = t1@item_list, theta = theta, tif = TRUE)
  expect_true(inherits(expected, "matrix"))
  expect_identical(observed, as.vector(expected))

  # -------------------------------------------------------------------------- #
  # A three item testlet with resp = NULL
  t1 <- testlet(itempool(b = rnorm(3), item_id = c("t1-i1", "t1-i2", "t1-i3")),
                   testlet_id = "t1")
  observed <- info_testlet_bare_cpp(theta = theta, testlet = t1,
                                    observed = FALSE, resp = NULL)
  expected <- info(ip = t1@item_list, theta = theta, tif = TRUE)
  expect_true(inherits(expected, "matrix"))
  expect_identical(observed, as.vector(expected))

  # -------------------------------------------------------------------------- #
  # Incorrect resp length, function should throw an error
  t1 <- testlet(itempool(b = rnorm(3), item_id = c("ti1", "ti2", "ti3")),
                testlet_id = "t1")
  resp <- 1
  expect_error(info_testlet_bare_cpp(
    theta = rnorm(1, 0, 0.5), testlet = t1, observed = FALSE, resp = resp))

  # -------------------------------------------------------------------------- #
  # All missing responses should return NA
  t1 <- testlet(itempool(b = rnorm(3), item_id = c("t1-i1", "t1-i2", "t1-i3")),
                   testlet_id = "t1")
  resp <- sim_resp(ip = t1, theta = rnorm(1))
  resp <- rep(NA, length(resp))
  observed <- info_testlet_bare_cpp(theta = rnorm(1), testlet = t1,
                                    observed = FALSE, resp = resp)
  expect_true(is.na(observed))

  # -------------------------------------------------------------------------- #
  # Partial missing response string
  t1 <- testlet(itempool(b = rnorm(3), item_id = c("t1-i1", "t1-i2", "t1-i3")),
                   testlet_id = "t1")
  resp <- sim_resp(ip = t1, theta = rnorm(1))
  theta <- rnorm(1)
  selected_items <- sample(1:3, sample(1:2, 1))
  resp[selected_items] <- NA
  observed <- info_testlet_bare_cpp(theta = theta, testlet = t1,
                                    observed = FALSE, resp = resp)
  expected <- info(ip = t1@item_list[-selected_items], theta = theta, tif = TRUE)
  not_expected <- info(ip = t1@item_list, theta = theta, tif = TRUE)
  expect_true(inherits(expected, "matrix"))
  expect_identical(observed, as.vector(expected))
  expect_lt(observed, not_expected)

})

############################################################################@###
######################## Test info_item_bare_cpp function ######################
############################################################################@###
test_that("Test info_item_bare_cpp Function", {
  i1 <- item(b = rnorm(1), item_id = "i1")
  expect_true(is.na(info_item_bare_cpp(theta = 1, item = i1,
                                       observed = FALSE, resp = NA)))
})

############################################################################@###
######################## Test info_itempool_bare_cpp function #################
############################################################################@###
test_that("Test info_itempool_bare_cpp Function", {
  t1 <- testlet(itempool(b = rnorm(2), item_id = c("t1-i1", "t1-i2")),
                   testlet_id = "t1")
  t2 <- testlet(itempool(a = rlnorm(3, 0, .3), b = rnorm(3),
                         item_id = c("t2-i1", "t2-i2", "t2-i3")),
                testlet_id = "t2")
  i1 <- item(b = rnorm(1), item_id = "i1")
  i2 <- item(a = rlnorm(1, 0, .3), b = rnorm(1), c = .2, item_id = "i2")
  i3 <- item(a = rlnorm(1, 0, .3), b = sort(runif(3)), item_id = "i3")
  ip <- c(t1, t2, i1, i2, i3)
  theta <- rnorm(1)
  resp <- sim_resp(ip = ip, theta = theta)[1, ]

  # -------------------------------------------------------------------------- #
  resp1 <- resp
  resp1["i3"] <- NA
  ip1 <- c(t1, t2, i1, i2)
  expected <-  info_itempool_bare_tif_cpp(
    theta = theta, ip = ip1, observed = FALSE, resp = NULL)
  observed <-  info_itempool_bare_tif_cpp(
    theta = theta, ip = ip, observed = FALSE, resp = resp1)
  expect_identical(observed, expected)

  # -------------------------------------------------------------------------- #
  resp1 <- resp
  resp1[c("i3", "t1-i1", "t1-i2")] <- NA
  ip1 <- c(t2, i1, i2)
  expected <-  info_itempool_bare_tif_cpp(
    theta = theta, ip = ip1, observed = FALSE, resp = NULL)
  observed <-  info_itempool_bare_tif_cpp(
    theta = theta, ip = ip, observed = FALSE, resp = resp1)
  expect_identical(observed, expected)

  # -------------------------------------------------------------------------- #
  resp1 <- resp
  resp1[c("i3", "t1-i1")] <- NA
  ip1 <- c(t1@item_list[[2]], t2, i1, i2)
  expected <-  info_itempool_bare_tif_cpp(
    theta = theta, ip = ip1, observed = FALSE, resp = NULL)
  observed <-  info_itempool_bare_tif_cpp(
    theta = theta, ip = ip, observed = FALSE, resp = resp1)
  expect_identical(observed, expected)
})


############################################################################@###
######################## Test info_itempool_cpp function ######################
############################################################################@###
test_that("Test info_itempool_cpp Function", {
  t1 <- testlet(itempool(b = rnorm(2), item_id = c("t1-i1", "t1-i2")),
                   testlet_id = "t1")
  t2 <- testlet(itempool(a = rlnorm(3, 0, .3), b = rnorm(3),
                         item_id = c("t2-i1", "t2-i2", "t2-i3")),
                testlet_id = "t2")
  i1 <- item(b = rnorm(1), item_id = "i1")
  i2 <- item(a = rlnorm(1, 0, .3), b = rnorm(1), c = .2, item_id = "i2")
  i3 <- item(a = rlnorm(1, 0, .3), b = sort(runif(3)), item_id = "i3")
  ip <- c(t1, t2, i1, i2, i3)
  theta <- rnorm(1)
  resp <- sim_resp(ip = ip, theta = theta)[1, ]

  # -------------------------------------------------------------------------- #
  resp1 <- resp
  resp1["i3"] <- NA
  ip1 <- c(t1, t2, i1, i2)
  expected <-  info_itempool_tif_cpp(
    theta = theta, ip = ip1, observed = FALSE, resp = NULL)
  observed <-  info_itempool_tif_cpp(
    theta = theta, ip = ip, observed = FALSE,
    resp = matrix(resp1, nrow = 1))
  expect_identical(observed, expected)

  # -------------------------------------------------------------------------- #
  resp1 <- resp
  resp1[c("i3", "t1-i1", "t1-i2")] <- NA
  ip1 <- c(t2, i1, i2)
  expected <-  info_itempool_tif_cpp(
    theta = theta, ip = ip1, observed = FALSE, resp = NULL)
  observed <-  info_itempool_tif_cpp(
    theta = theta, ip = ip, observed = FALSE,
    resp = matrix(resp1, nrow = 1))
  expect_identical(observed, expected)

  # -------------------------------------------------------------------------- #
  resp1 <- resp
  resp1[c("i3", "t1-i1")] <- NA
  ip1 <- c(t1@item_list[[2]], t2, i1, i2)
  expected <-  info_itempool_tif_cpp(
    theta = theta, ip = ip1, observed = FALSE, resp = NULL)
  observed <-  info_itempool_tif_cpp(
    theta = theta, ip = ip, observed = FALSE,
    resp = matrix(resp1, nrow = 1))
  expect_identical(observed, expected)
})



############################################################################@###
######################## sim_resp_cpp function #################################
############################################################################@###
test_that("Test info_itempool_cpp Function", {
  expect_true(sim_resp_bare_cpp(item = generate_item(), theta = rnorm(1))
              %in% 0:1)
  # # Benchmarking
  # item <- generate_item()
  # theta <- rnorm(1)
  # microbenchmark::microbenchmark(
  #   cpp = sim_resp_bare_cpp(item = item, theta = theta),
  #   R = sim_resp(ip = item, theta = theta),
  # times = 1e4
  # )
})
