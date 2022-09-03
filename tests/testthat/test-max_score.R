
###############################################################################@
############################# max_score (Item) @################################
###############################################################################@

test_that("max_score - Item", {

  # -------------------------------------------------------------------------- #
  # Check all 4PM  Models
  for (model  in c("Rasch", "1PL", "2PL", "3PL", "4PL")) {
    itm <- generate_item(model = model)
    expect_identical(max_score(ip = itm), 1L)
  }

  # -------------------------------------------------------------------------- #
  # Check all Polytomous  Models
  for (model  in c("GPCM", "GRM", "GPCM2", "PCM")) {
    n <- sample(3:10, 1)
    itm <- generate_item(model = model, n_categories = n)
    expect_identical(max_score(ip = itm), n - 1L)
  }
})



###############################################################################@
############################# max_score (ItemPool) @############################
###############################################################################@

test_that("max_score - Itempool", {

  # -------------------------------------------------------------------------- #
  # Check a mixture of models
  n_poly <- sample(5:10, 1)
  n_categories <- c(sample(4:8, n_poly, TRUE), rep(2, 4))
  ip <- generate_ip(
    model = c(sample(c("GPCM", "GRM", "GPCM2", "PCM"), n_poly, TRUE),
              rep("3PL", 4)), n_categories = n_categories)
  resp_set <- sim_resp(ip = ip, theta = rnorm(1), output = "response_set")

  expect_identical(max_score(ip = ip, resp = resp_set),
                   sum(n_categories) - length(ip))

  # -------------------------------------------------------------------------- #
  # Check a mixture of models with missing responses for some items
  n_poly <- sample(5:10, 1)
  n_uni <- sample(5:10, 1)
  n_categories <- c(sample(4:8, n_poly, TRUE), rep(2, n_uni))
  ip <- generate_ip(
    model = c(sample(c("GPCM", "GRM", "GPCM2", "PCM"), n_poly, TRUE),
              rep("3PL", n_uni)), n_categories = n_categories)
  # remove the first item response and check
  resp_set <- sim_resp(ip = ip, theta = rnorm(1), output = "response_set",
                       prop_missing = .4)

  expect_identical(max_score(ip = ip, resp = resp_set),
                   max_score(ip = ip[resp_set[[1]]$item_id]))


  # -------------------------------------------------------------------------- #
  # Check multiple response sets
  n_poly <- sample(5:10, 1)
  n_uni <- sample(5:10, 1)
  n_categories <- c(sample(4:8, n_poly, TRUE), rep(2, n_uni))
  ip <- generate_ip(
    model = c(sample(c("GPCM", "GRM", "GPCM2", "PCM"), n_poly, TRUE),
              rep("3PL", n_uni)), n_categories = n_categories)
  # remove the first item response and check
  resp_set <- sim_resp(ip = ip, theta = rnorm(3), output = "response_set",
                       prop_missing = .4)
  expected <- c(max_score(ip = ip, resp = resp_set[[1]]),
                max_score(ip = ip, resp = resp_set[[2]]),
                max_score(ip = ip, resp = resp_set[[3]]))
  expect_identical(max_score(ip = ip, resp = resp_set), expected)
})
