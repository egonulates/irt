
test_that("ipd_robustz", {

  # -------------------------------------------------------------------------- #
  # Error raised when item pools have different number of items
  n_item <- sample(10:20, 1)
  ip1 <- generate_ip(n = n_item)
  ip2 <- generate_ip(n = n_item + 1)
  expect_error(ipd(ip1 = ip1, ip2 = ip2, method = "robust-z"),
               "Invalid 'anchor_item_ids'")

  # -------------------------------------------------------------------------- #
  # Warning raised when item pools have different item IDs
  n_item <- sample(10:20, 1)
  ip1 <- generate_ip(n = n_item)
  ip2 <- generate_ip(n = n_item)
  ip2$item_id <- sample(ip1$item_id)
  expect_warning(ipd(ip1 = ip1, ip2 = ip2, method = "robust-z"),
                 "The item IDs in the item pools do not align." )

  # -------------------------------------------------------------------------- #
  # Robust-z
  n <- 20
  ip1 <- generate_ip(n = n)
  ipdf1 <- ipdf2 <- as.data.frame(ip1)
  ipdf2$a <- ipdf1$a + runif(n, -0.25, .25)
  ipdf2$b <- ipdf1$b + runif(n, -0.25, .25)
  ipdf2$a[1] <- ipdf1$a[1] + 2
  ipdf2$b[n] <- ipdf1$b[n] - 1
  ip2 <- itempool(ipdf2)
  result <- ipd(ip1 = ip1, ip2 = ip2, method = "robust-z")
  expect_true("Item_1" %in% result$a$unstable)
  expect_true(paste0("Item_", n) %in% result$b$unstable)



  # -------------------------------------------------------------------------- #
  # Robust-z
  # The example from Huynh and Meyer (2010)
  ip1 <- c(itempool(
    a = c(0.729, 0.846, 0.909, 0.818, 0.742, 0.890, 1.741, 0.907, 1.487, 1.228,
          0.672, 1.007, 1.016, 0.776, 0.921, 0.550, 0.624, 0.984, 0.506, 0.594,
          0.687, 0.541, 0.691, 0.843, 0.530, 0.462, 1.007, 0.825, 0.608, 1.177,
          0.900, 0.861, 0.843, 1.404, 0.446, 1.014, 1.632, 0.831, 1.560, 0.798),
    b = c(1.585, 0.635, -0.378, -0.100, -0.195, 0.749, 1.246, 1.016, -0.234,
          0.537, 0.070, 1.985, 1.101, -0.742, 0.463, -0.060, 0.477, 1.084,
          -2.340, 1.068, -0.055, -1.045, 1.859, 0.645, -0.689, -2.583, 1.922,
          0.709, 0.499, 1.973, 0.104, 0.809, 0.640, 0.247, 0.820, 1.837,
          2.129, 1.012, 1.774, 0.095),
    c = c(0.134, 0.304, 0.267, 0.176, 0.215, 0.194, 0.267, 0.159, 0.095,
          0.197, 0.089, 0.272, 0.229, 0.159, 0.162, 0.100, 0.259, 0.167,
          0.000, 0.242, 0.323, 0.000, 0.196, 0.189, 0.000, 0.000, 0.334,
          0.538, 0.125, 0.511, 0.192, 0.353, 0.103, 0.241, 0.245, 0.118,
          0.155, 0.132, 0.215, 0.148),
    model = "3PL"),
    item(a = 0.561, b = c(0.784, -0.113, 1.166), model = "GPCM"),
    item(a = 0.745, b = c(3.687, 2.506, -0.001), model = "GPCM"))

  ip2 <- c(itempool(
    a = c(0.650, 0.782, 0.816, 0.787, 0.611, 0.888, 1.192, 0.589, 1.211,
          0.742, 0.526, 0.690, 0.996, 0.816, 0.781, 0.507, 0.378, 0.976,
          0.473, 0.364, 0.585, 0.566, 0.511, 0.718, 0.354, 1.080, 0.840,
          0.865, 0.528, 0.814, 0.555, 0.701, 0.530, 1.220, 0.344, 0.966,
          1.044, 0.358, 1.192, 0.615),
    b = c(0.676, -0.525, -1.749, -1.092, -1.619, -0.406, -0.132, 0.006,
          -1.352, -0.872, -1.242, 0.873, 0.239, -2.038, -0.487, -1.372,
          -1.492, 0.214, -4.537, 0.220, -0.686, -2.394, 0.747, -0.467,
          -3.629, -5.000, 0.927, 0.305, -0.839, 1.270, -1.618, -0.091,
          -1.228, -1.019, -1.453, 1.090, 1.743, -1.436, 1.024, -1.358),
    c = c(0.110, 0.316, 0.161, 0.149, 0.145, 0.200, 0.243, 0.059, 0.081,
          0.075, 0.028, 0.267, 0.242, 0.189, 0.184, 0.121, 0.000, 0.170,
          0.000, 0.151, 0.383, 0.000, 0.195, 0.177, 0.000, 0.000, 0.352,
          0.647, 0.116, 0.501, 0.000, 0.286, 0.000, 0.248, 0.064, 0.150,
          0.126, 0.000, 0.187, 0.007),
    model = "3PL"),
    item(a = 0.486, b = c(-0.539, -1.489, -0.052), model = "GPCM"),
    item(a = 0.737, b = c(2.599, 1.250, -1.209), model = "GPCM"))
  rz <- ipd(ip1, ip2, method = "robust-z")
  expect_true(all(rz$a$robust_z > -10 & rz$a$robust_z < 10, na.rm = TRUE))
  expect_true(all(rz$b$robust_z > -10 & rz$b$robust_z < 10, na.rm = TRUE))


  # -------------------------------------------------------------------------- #
  # Robust-z example with GPCM2 and 2PL items
  n_item <- 20
  ip1 <- generate_ip(model = sample(c("GPCM2", "2PL"), n_item, T),
                     n_categories = sample(3:5, n_item, T))
  ip2 <- as.data.frame(ip1)
  # Add some noise to item parameters
  ip2$a <- ip2$a + runif(n_item, -0.25, .25)
  ip2$b <- ip2$b + runif(n_item, -0.5, .5)
  ip2$d1 <- ip2$d1 + runif(n_item, -0.25, .25)
  ip2$d2 <- ip2$d2 + runif(n_item, -0.25, .25)
  ip2$d3 <- ip2$d3 + runif(n_item, -0.25, .25)
  ip2$d4 <- ip2$d4 + runif(n_item, -0.25, .25)
  ip2 <- itempool(ip2)

  rz <- ipd(ip1, ip2, method = "robust-z")
  expect_true(all(rz$a$robust_z > -10 & rz$a$robust_z < 10, na.rm = TRUE))
  expect_true(all(rz$d1$robust_z > -10 & rz$d1$robust_z < 10, na.rm = TRUE))

  # -------------------------------------------------------------------------- #
  # Robust-z example with GPCM and 2PL items
  n_item <- 20
  ip1 <- generate_ip(model = sample(c("GPCM", "2PL"), n_item, T),
                     n_categories = sample(3:5, n_item, T))
  ip2 <- as.data.frame(ip1)
  # Add some noise to item parameters
  ip2$a <- ip2$a + runif(n_item, -0.25, .25)
  ip2$b1 <- ip2$b1 + runif(n_item, -0.25, .25)
  ip2$b2 <- ip2$b2 + runif(n_item, -0.25, .25)
  ip2$b3 <- ip2$b3 + runif(n_item, -0.25, .25)
  ip2$b4 <- ip2$b4 + runif(n_item, -0.25, .25)
  ip2 <- itempool(ip2)

  rz <- ipd(ip1, ip2, method = "robust-z")
  expect_true(all(rz$a$robust_z > -10 & rz$a$robust_z < 10, na.rm = TRUE))
  expect_true(all(rz$b1$robust_z > -10 & rz$b1$robust_z < 10, na.rm = TRUE))


  # -------------------------------------------------------------------------- #
  # Robust-z example with GRM and 2PL items
  n_item <- 20
  ip1 <- generate_ip(model = sample(c("GRM", "2PL"), n_item, T),
                     n_categories = sample(3:5, n_item, T))
  ip2 <- as.data.frame(ip1)
  # Add some noise to item parameters
  ip2$a <- ip2$a + runif(n_item, -0.25, .25)
  ip2$b1 <- ip2$b1 + runif(n_item, -0.25, .25)
  ip2$b2 <- ip2$b2 + runif(n_item, -0.25, .25)
  ip2$b3 <- ip2$b3 + runif(n_item, -0.25, .25)
  ip2$b4 <- ip2$b4 + runif(n_item, -0.25, .25)
  ip2 <- itempool(ip2)

  rz <- ipd(ip1, ip2, method = "robust-z")
  expect_true(all(rz$a$robust_z > -10 & rz$a$robust_z < 10, na.rm = TRUE))
  expect_true(all(rz$b1$robust_z > -10 & rz$b1$robust_z < 10, na.rm = TRUE))

})

