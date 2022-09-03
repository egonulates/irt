
###############################################################################@
############################# var (Item) @######################################
###############################################################################@

test_that("var - Item", {

  # -------------------------------------------------------------------------- #
  # Regular var function with numbers works
  x <- runif(50, 10, 100)
  expect_identical(var(x), sd(x)^2, tolerance = 1e-8)

  ##  Single theta
  # -------------------------------------------------------------------------- #
  # Check all 4PM  Model
  for (model  in c("Rasch", "1PL", "2PL", "3PL", "4PL")) {
    itm <- generate_item(model = model)
    theta <- rnorm(1)
    p <- unname(prob(ip = itm, theta = theta)[, 2])
    expect_identical(var(itm, theta), p * (1 - p))
  }

  # -------------------------------------------------------------------------- #
  # Check GPCM  Model - Single theta
  ip <- generate_item(model = "GPCM")
  theta <- rnorm(1)
  p <- prob(ip = ip, theta = theta)[1, -1]
  expected <- sum(p * (1:length(p))^2) - mean(ip, theta)
  expect_identical(var(ip, theta), expected)

  ##  Multiple theta
  # -------------------------------------------------------------------------- #
  # Check all 4PM  Model
  for (model  in c("Rasch", "1PL", "2PL", "3PL", "4PL")) {
    itm <- generate_item(model = model)
    theta <- rnorm(sample(2:20, 1))
    p <- unname(prob(ip = itm, theta = theta)[, 2])
    expect_identical(var(x = itm, y = theta), p * (1 - p))
  }

  # -------------------------------------------------------------------------- #
  # Check GPCM  Model - Multiple theta
  for (model  in c("PCM", "GRM", "GPCM", "GPCM2")) {
    ip <- generate_item(model = model)
    theta <- rnorm(sample(2:20, 1))
    p <- prob(ip = ip, theta = theta)[, -1]
    expected <- rowSums(
      p * matrix((1:ip$max_score)^2, nrow = length(theta), ncol = ip$max_score,
                 byrow = TRUE)) - mean(ip, theta)
    expect_identical(var(ip, theta), expected)
  }


  # -------------------------------------------------------------------------- #
  # Check GRM  Model - Multiple theta
  for (model  in c("PCM", "GRM", "GPCM", "GPCM2")) {
    ip <- generate_item(model = model)
    theta <- rnorm(sample(2:20, 1))
    expected <- sapply(theta, var, x = ip)
    expect_identical(var(ip, theta), expected)
  }

})



###############################################################################@
############################# var (ItemPool) @##################################
###############################################################################@

test_that("var - Itempool", {
  ##  Single theta
  # -------------------------------------------------------------------------- #
  # Check all 4PM  Model
  for (model  in c("Rasch", "1PL", "2PL", "3PL", "4PL")) {
    ip <- generate_ip(model = model)
    theta <- rnorm(1)
    p <- prob(ip = ip, theta = theta)
    p <- p[, 2]
    expect_identical(var(x = ip, y = theta), (p * (1 - p)))
  }

  # -------------------------------------------------------------------------- #
  # Check Polytomous Models
  for (model  in c("PCM", "GRM", "GPCM", "GPCM2")) {
    ip <- generate_ip(model = model)
    theta <- rnorm(1)
    expected <- sapply(ip$item_list, var, y = theta)
    expect_identical(var(ip, theta), expected)
  }


  ##  Multiple theta
  # -------------------------------------------------------------------------- #
  # Check all 4PM  Model
  for (model  in c("Rasch", "1PL", "2PL", "3PL", "4PL")) {
    ip <- generate_ip(model = model)
    theta <- rnorm(sample(2:20, 1))
    p <- prob(ip = ip, theta = theta)
    i <- sample(1:length(ip), 1)
    j <- sample(1:length(theta), 1)
    p <- p[[j]][i, 2]
    expect_identical(var(x = ip, y = theta)[j, i],
                     setNames(p * (1 - p), ip$item_id[i]))
  }

  # -------------------------------------------------------------------------- #
  # Check Polytomous Models - Multiple theta
  for (model  in c("PCM", "GRM", "GPCM", "GPCM2")) {
    ip <- generate_ip(model = model)
    theta <- rnorm(sample(2:20, 1))
    i <- sample(1:length(ip), 1)
    j <- sample(1:length(theta), 1)
    expected <- var(ip[[i]], theta[j])
    expect_identical(var(ip, theta)[j, i], expected, ignore_attr = TRUE)
  }
})


###############################################################################@
############################# var (Testlet) @###################################
###############################################################################@

test_that("var - Testlet", {
  # -------------------------------------------------------------------------- #
  ##  Single theta
  t1 <- generate_testlet()
  theta <- rnorm(1)
  expect_identical(var(t1, theta), var(t1@item_list, theta))

  # -------------------------------------------------------------------------- #
  ##  Multiple theta
  t1 <- generate_testlet()
  theta <- rnorm(sample(2:10, 1))
  expect_identical(var(t1, theta), var(t1@item_list, theta))
})
