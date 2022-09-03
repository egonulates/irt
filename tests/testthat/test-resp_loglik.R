
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% resp_loglik %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###############################################################################@
############################# resp_loglik (Item) @##############################
###############################################################################@

test_that("resp_loglik - Item", {
  ##  Single theta
  ip <- item(a = 1, b = 0.02173983, c = 0, D = 0.7636914)
  expect_identical(
    resp_loglik(resp = 1, ip = ip, theta = 0.4130181),
    -0.5548593,
    ignore_attr = TRUE, tolerance = 1e-5)
  # 3PL
  ip <- item(a = 1.366882, b = -1.10991, c =  0.1462888,
                           D = 0.8379783)
  expect_identical(resp_loglik(resp = 1, ip = ip, theta = -0.2072357),
                   -0.2535339, ignore_attr = TRUE, tolerance = 1e-5)

  # -------------------------------------------------------------------------- #
  theta = -0.228
  a = 1.29
  b = -1.029
  D = 1.702
  ip = item(a = a, b = b, D = D, model = '2PL', item_id = paste0('i', 1:1))
  expected <- -1.91760642
  object <- resp_loglik(ip = ip, resp = 0, theta = theta)
  expect_identical(object, expected, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  theta = -0.62
  a = 1.165
  b = 1.022
  D = 1.702
  ip = item(a = a, b = b, D = D, model = '2PL', item_id = paste0('i', 1:1))
  expected <- -3.29363208
  object <- resp_loglik(ip = ip, resp = 1, theta = theta)
  expect_identical(object, expected, tolerance = 1e-6)

  # Multiple theta
  ip <- item(a = 1.428541, b = 0.6719602, c =  0.05310894,
                           d = 0.8918666, D = 1.570897)
  expect_identical(resp_loglik(resp = c(0, 0, 0, 0, 1, 1, 0), ip = ip,
                            theta = -3:3)[4], -0.2295754,
               ignore_attr = TRUE, tolerance = 1e-5)
  expect_identical(
    resp_loglik(resp = tibble::tibble(item1 = c(0, 0, 0, 0, 1, 1, 0)),
                ip = ip, theta = -3:3)[4], -0.2295754, ignore_attr = TRUE,
    tolerance = 1e-5)

  ## Response Likelihood
  # It is simply exp(resp_loglik)
  # Single theta
  ip <- item(a = 1, b = 0.02173983, c = 0, D = 1.7)
  theta <- 0.4130181
  p <- prob(ip, theta)[, 2]
  resp <- 1
  expect_identical(exp(resp_loglik(resp = resp, ip = ip, theta = theta)), p,
               ignore_attr = TRUE, tolerance = 1e-5)
  # 3PL
  ip <- item(a = 1.366882, b = -1.10991, c =  0.1462888, D = 1)
  theta <- -.53
  resp <- 0
  p <- prob(ip, theta)[, 2]
  expect_identical(exp(resp_loglik(resp = resp, ip = ip, theta = theta)), 1 - p,
                   ignore_attr = TRUE, tolerance = 1e-5)

  # Multiple theta
  ip <- item(a = 1.4, b = 0.6, c =  0.05, d = 0.96, D = 1)
  resp = c(0, 0, 0, 0, 1, 1, 0)
  theta = -3:3
  p <- prob(ip, theta)[, 2]
  expect_identical(exp(resp_loglik(resp = resp, ip = ip, theta = theta)),
               p^resp * (1 - p)^(1 - resp),
               ignore_attr = TRUE, tolerance = 1e-5)
  # item pool
  ip <- itempool(data.frame(
    a = c(1.36, 1.19, 0.96, 1.41, 1.1, 1.44, 2.42),
    b = c(-0.22, 1.56, -1.34, -0.37, -0.16, 0.09, -0.75),
    c = c(0.13, 0.25, 0.25, 0.20, 0.19, 0.05, 0.05),
    D = 1.702))
  resp = c(0, 0, 1, 0, 0, 0, 1)
  theta = -0.55
  p <- prob(ip, theta)
  p <- p[, 2]
  expect_identical(
    exp(resp_loglik(resp = resp, ip = ip, theta = theta)),
    prod(p^resp * (1 - p)^(1 - resp)), ignore_attr = TRUE, tolerance = 1e-5)

  ### Missing Responses ###
  # -------------------------------------------------------------------------- #
  # Missing Responses should return NA
  ip <- item(a = 1.4, b = 0.6, c =  0.05, d = 0.96, D = 1)
  resp = c(NA, 0, 0, 0, 1, NA, 0)
  theta = -3:3
  expect_identical(which(is.na(resp_loglik(resp = resp, ip = ip,
                                           theta = theta))), c(1L, 6L))

  # -------------------------------------------------------------------------- #
  # If resp is all missing data, then resp_loglik should be NA.
  ip <- item(a = 1.4, b = 0.6, c =  0.05, d = 0.96, D = 1)
  theta = -3:3
  resp = rep(NA, length(theta))
  ll <- resp_loglik(ip = ip, resp = resp, theta = theta)
  expect_identical(length(ll), length(theta))
  expect_true(all(is.na(ll)))

  # -------------------------------------------------------------------------- #
  # Missing responses should not change the log likelihood or a response string
  n <- sample(10:20, 1)
  ip <- itempool(a = runif(n, .7, 1.5), b = runif(n, -2, 2))
  ip_list <- ip$item_list
  resp <- sim_resp(ip = ip, theta = rnorm(1, 0, .4))
  # Randomly drop two items and create an item pool
  selected_items <- sample(1:n, 2)
  ip_small <- itempool(ip_list[-selected_items])
  resp_small <- resp[, -selected_items]
  resp_na <- resp
  resp_na[, selected_items] <- NA
  theta <- rnorm(1)
  expect_identical(resp_loglik(ip = ip, resp = resp_na, theta = theta),
               resp_loglik(ip = ip_small, resp = resp_small, theta = theta))

  # -------------------------------------------------------------------------- #
  n <- sample(10:20, 1)
  ip <- itempool(a = runif(n, .7, 1.5), b = runif(n, -2, 2))
  ip_list <- ip$item_list
  resp <- tibble::as_tibble(sim_resp(ip = ip, theta = rnorm(1, 0, .4)))
  # Randomly drop two items and create an item pool
  selected_items <- sample(1:n, 2)
  ip_small <- itempool(ip_list[-selected_items])
  resp_small <- resp[, -selected_items]
  resp_na <- resp
  resp_na[, selected_items] <- NA
  theta <- rnorm(1)
  expect_identical(resp_loglik(ip = ip, resp = resp_na, theta = theta),
               resp_loglik(ip = ip_small, resp = resp_small, theta = theta))


  ### Graded Response Model ###
  # Item
  # -------------------------------------------------------------------------- #
  theta = -0.472
  D = 1.702
  a = 1.08
  b = c(-1.231, 0.805, 1.294)
  ip = item(a = a, b = b, D = D, model = 'GRM')
  expected <- -0.336680359157912
  object <- resp_loglik(ip = ip, resp = 1, theta = theta)
  expect_identical(object, expected, tolerance = 1e-6)

  # Item Pool
  # -------------------------------------------------------------------------- #
  theta = 1.376
  D = 1.702
  a = 1.27
  b = c(-2.663, 1.025, 1.257)
  item1 = item(a = a, b = b, D = D, model = 'GRM')
  a = 1.111
  b = c(0.772, 0.797, 1.259)
  item2 = item(a = a, b = b, D = D, model = 'GRM')
  a = 1.128
  b = c(-1.106, -0.626, -0.225)
  item3 = item(a = a, b = b, D = D, model = 'GRM')
  ip = itempool(c(item1, item2, item3), item_id = paste0('i', 1:3))
  expected <- -3.60888518
  object <- resp_loglik(ip = ip, resp = c(2, 0, 3), theta = theta)
  expect_identical(object, expected, tolerance = 1e-6)


  ### Generalized Partial Credit Model ###
  # Item
  # -------------------------------------------------------------------------- #
  theta = 1.279
  D = 1.702
  a = 1.119
  b = c(-0.39, -0.494, -0.315)
  ip = item(a = a, b = b, D = D, model = 'GPCM')
  expected <- -3.08437963
  object <- resp_loglik(ip = ip, resp = 2, theta = theta)
  expect_identical(object, expected, tolerance = 1e-6)

  # Item Pool
  # -------------------------------------------------------------------------- #
  theta = -0.894
  D = 1.702
  a = 0.93
  b = c(-1.053, -1.833, -1.299)
  item1 = item(a = a, b = b, D = D, model = 'GPCM')
  a = 1.053
  b = c(0.438, 1.254, -0.433)
  item2 = item(a = a, b = b, D = D, model = 'GPCM')
  a = 0.97
  b = c(0.259, -0.12, 0.564)
  item3 = item(a = a, b = b, D = D, model = 'GPCM')
  ip = itempool(c(item1, item2, item3), item_id = paste0('i', 1:3))
  expected <- -7.05057445
  object <- resp_loglik(ip = ip, resp = c(2, 0, 3), theta = theta)
  expect_identical(object, expected, tolerance = 1e-6)


  ### Mixture of Models ###
  # -------------------------------------------------------------------------- #
  # Here the item pool consist of two 2PL, three GRM and two GPCM items.
  # This test checks whether the log likelihood is calculated correctly.
  theta = 1.272
  a = c(0.983, 1.03)
  b = c(-0.581, 1.643)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b), D = D, model = '2PL',
                   item_id = paste0('irt', 1:2))
  expected <- -3.56417023
  resp <- c(0, 0)
  # GRM
  a = 1.027
  b = c(-2.439, -0.21, 0.693)
  item1 = item(a = a, b = b, D = D, model = 'GRM')
  a = 1.024
  b = c(0.241, 0.257, 0.311)
  item2 = item(a = a, b = b, D = D, model = 'GRM')
  a = 0.896
  b = c(-0.225, 0.458, 0.764)
  item3 = item(a = a, b = b, D = D, model = 'GRM')
  ip = c(ip, itempool(c(item1, item2, item3), item_id = paste0('grm', 1:3)))
  resp = c(resp, 3, 2, 1)
  expected <- sum(expected, -6.75156645)
  a = 0.927
  b = c(2.086, 0.107)
  item1 = item(a = a, b = b, D = D, model = 'GPCM')
  a = 1.201
  b = c(-0.349, 1.162)
  item2 = item(a = a, b = b, D = D, model = 'GPCM')
  ip = c(ip, itempool(c(item1, item2), item_id = paste0('gpcm', 1:2)))
  expected <- sum(expected, -1.70721466)
  resp = c(resp, 0, 2)
  object <- resp_loglik(ip = ip, resp = resp, theta = theta)
  expect_identical(object, expected, tolerance = 1e-6)
})

###############################################################################@
############################# resp_loglik (Testlet) %###########################
###############################################################################@

test_that("resp_loglik - Testlet", {
  # Ability Estimation with the testlets
  t1 <- testlet(itempool(b = rnorm(2), item_id = c("t1-i1", "t1-i2")),
                   testlet_id = "t1")
  n_theta <- sample(4:9, 1)
  resp <- sim_resp(ip = t1, theta = rnorm(n_theta))
  theta <- rnorm(n_theta)
  expect_identical(
    resp_loglik(resp = resp, ip = t1, theta = theta),
    resp_loglik(resp = resp, ip = t1@item_list, theta = theta))

  # -------------------------------------------------------------------------- #
  t1 <- testlet(itempool(b = rnorm(1), item_id = c("t1-i1")), testlet_id = "t1")
  n_theta <- sample(4:9, 1)
  resp <- sim_resp(ip = t1, theta = rnorm(n_theta))
  theta <- rnorm(n_theta)
  expect_identical(
    resp_loglik(resp = resp, ip = t1, theta = theta),
    resp_loglik(resp = resp, ip = t1@item_list, theta = theta))

  # -------------------------------------------------------------------------- #
  t1 <- testlet(itempool(b = rnorm(1), item_id = c("t1-i1")), testlet_id = "t1")
  n_theta <- 1
  resp <- as.vector(sim_resp(ip = t1, theta = rnorm(n_theta)))
  theta <- rnorm(n_theta)
  resp_loglik(resp = resp, ip = t1, theta = theta)
  expect_identical(
    resp_loglik(resp = resp, ip = t1, theta = theta),
    resp_loglik(resp = resp, ip = t1@item_list, theta = theta))

})

###############################################################################@
############################# resp_loglik (Itempool) %#########################
###############################################################################@

test_that("resp_loglik - Itempool", {

  ip <- itempool(data.frame(
    a = c(1.36947439378127, 1.19103434309363, 0.966255453648046,
          1.41118730790913, 1.03645211085677, 1.44584470195696,
          1.42817918118089),
    b = c(-0.221226601859114, 1.56170696559919, -1.34820736712213,
          -0.374769023260681, -0.164801504563551, 0.0918581153824936,
          -0.758465659387833),
    c = c(0.132225778349675, 0.252476210868917, 0.251043332414702,
          0.20630893916823, 0.190127589460462, 0.051204833923839,
          0.0552326969336718),
    D = 2.129246))
  expect_identical(
    resp_loglik(resp = c(0, 0, 1, 0, 0, 0, 1), ip = ip, theta = -0.5545929),
    -2.72455, ignore_attr = TRUE, tolerance = 1e-5)

  # -------------------------------------------------------------------------- #
  # resp (numeric), Multiple theta, 1 item (Itempool)
  resp <- c(1,0)
  theta <- c(-1, 1, 2)
  expect_error(resp_loglik(resp = resp, ip = ip, theta = theta),
               regexp = "Invalid arguments.")

  # -------------------------------------------------------------------------- #
  # The length of the ip and the length of the resp should correspond
  ip <- itempool(b = rnorm(10))
  resp <- sample(0:1, 7, replace = TRUE)
  expect_error(resp_loglik(resp = resp, ip = ip, theta = rnorm(1)),
               regexp = "Invalid arguments.")
  resp <- tibble::as_tibble(matrix(resp, nrow = 1,
                           dimnames = list(c(), paste0("i", 1:7))))
  expect_error(resp_loglik(resp = resp, ip = ip, theta = rnorm(1)),
               regexp = "Invalid arguments.")

  # -------------------------------------------------------------------------- #
  # If resp is all missing data, then resp_loglik should be NA.
  expect_true(is.na(resp_loglik(ip = ip, resp = rep(NA, length(ip)),
                                theta = rnorm(1))))
  # If there is one item and multiple thetas and all resp are missing
  # it should return a vector of NAs
  ip <- ip[[1]]
  theta <- c(-1, 1, 2)
  ll <- resp_loglik(ip = ip, resp = rep(NA, length(theta)), theta = theta)
  expect_identical(length(ll), 3L)
  expect_true(all(is.na(ll)))


  # -------------------------------------------------------------------------- #
  # Function works fine with Response and Response_set objects
  ip <- generate_ip(n = 5)
  theta <- rnorm(3)
  resp_set <- generate_resp_set(ip = ip, theta = theta, prop_missing = .2)
  resp_matrix <- as.matrix(resp_set, ip = ip)

  expected <- resp_loglik(ip = ip, resp = resp_matrix, theta = theta)
  observed <- resp_loglik(ip = ip, resp = resp_set, theta = theta)

  # Function works fine with Response_set object
  expect_identical(expected, observed)

  # Function works fine with Response object
  observed <- resp_loglik(ip = ip, resp = resp_set[[1]], theta = theta[1])
  expect_identical(expected[1], observed)

})


###############################################################################@
############################# resp_loglik (REST) @##############################
###############################################################################@
test_that("resp_loglik - REST", {
  # Try character, 1 theta
  expect_error(resp_loglik(resp = 1, ip = "1", theta = 2),
               regexp = "Cannot convert object to an")
  expect_error(resp_loglik(resp = 3, ip = 1, theta = 2),
               regexp = "Insufficient parameters. Please give more information")
})



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% resp_loglik (First Derivative) %%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###############################################################################@
############################# resp_loglik (First Derivative) (Item) @###########
###############################################################################@

test_that("resp_loglik (First Derivative) - Item", {
  ##  Single theta
  ip <- item(a = 1.288916, b = 0.2149486, D = 2.393605)
  expect_identical(resp_loglik(resp = 0, ip = ip, theta = -0.7466429,
                           derivative = 1),
               -0.1510338, ignore_attr = TRUE, tolerance = 1e-5)

  ##  Multiple theta
  # Multiple theta
  ip <- item(a = 0.9580645, b = -0.4588905, c = 0.08207303,
                           d = 0.8559631, D = 1.011083)
  expect_identical(resp_loglik(resp = c(0, 0, 0, 0, 0, 1, 1), ip = ip, theta = -3:3,
                           derivative = 1)[4], -0.3997864,
               ignore_attr = TRUE, tolerance = 1e-5)

  # -------------------------------------------------------------------------- #
  # 2PL - First Derivative
  # Rose, N. (2010), p.4,  Equation 17
  ip <- generate_ip(model = "2PL", D = 1)
  theta <- rnorm(1)
  resp <- generate_resp(ip = ip, theta = theta)[[1]]$score
  expected <- sum(ip$a * (resp - prob(ip = ip, theta = theta)[, 2]))
  observed <- resp_loglik(resp = resp, ip = ip, theta = theta, derivative = 1)
  expect_equal(expected, observed)

  ### Missing Responses ###
  # -------------------------------------------------------------------------- #
  # Missing Responses should return NA
  ip <- item(a = 1.4, b = 0.6, c =  0.05, d = 0.96, D = 1)
  resp = c(NA, 0, 0, 0, 1, NA, 0)
  theta = -3:3
  expect_identical(which(is.na(resp_loglik(resp = resp, ip = ip, theta = theta,
                                       derivative = 1))), c(1L, 6L))

  # -------------------------------------------------------------------------- #
  # Missing responses should not change the log likelihood or a response string
  n <- sample(10:20, 1)
  ip <- itempool(a = runif(n, .7, 1.5), b = runif(n, -2, 2))
  ip_list <- ip$item_list
  resp <- sim_resp(ip = ip, theta = rnorm(1, 0, .4))
  # Randomly drop two items and create an item pool
  selected_items <- sample(1:n, 2)
  ip_small <- itempool(ip_list[-selected_items])
  resp_small <- resp[, -selected_items]
  resp_na <- resp
  resp_na[, selected_items] <- NA
  theta <- rnorm(1)
  expect_identical(resp_loglik(ip = ip, resp = resp_na, theta = theta,
                           derivative = 1),
               resp_loglik(ip = ip_small, resp = resp_small, theta = theta,
                           derivative = 1))

  # -------------------------------------------------------------------------- #
  # All of the elements of response matrix should be numeric
  n <- sample(10:20, 1) # number of examinees
  ip <- item(b = rnorm(1))
  resp <- data.frame(i1 = sim_resp(ip = ip, theta = rnorm(n)))
  resp[1, 1] <- 'a'
  expect_warning(rllfd <- resp_loglik(ip = ip, resp = resp, theta = rnorm(n),
                                      derivative = 1))
  expect_true(is.na(rllfd[1]))

})


###############################################################################@
############################# resp_loglik (First Derivative) (Itempool) @######
###############################################################################@

test_that("resp_loglik (First Derivative) - Itempool", {
  # -------------------------------------------------------------------------- #
  # All of the elements of response matrix should be numeric
  n <- sample(10:20, 1) # number of examinees
  ip <- itempool(b = rnorm(sample(5:10, 1)))
  theta <- rnorm(n)
  resp1 <- resp2 <- data.frame(sim_resp(ip = ip, theta = rnorm(n)))
  resp2[1, 1] <- 'a'
  fd1 <- resp_loglik(ip = ip, resp = resp1, theta = theta, derivative = 1)
  expect_warning(fd2 <- resp_loglik(ip = ip, resp = resp2, theta = theta,
                                    derivative = 1))
  expect_identical(fd1[-1], fd2[-1])
  expect_identical(fd2[1], resp_loglik(ip = ip[-1], resp = resp1[, -1],
                                   theta = theta, derivative = 1)[1])
})


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% resp_loglik (Second Derivative) %%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###############################################################################@
############################# resp_loglik (Second Derivative) (Item) @##########
###############################################################################@


test_that("resp_loglik (Second Derivative) - Item", {
  ##  Single theta
  # 3PL
  ip <- item(a = 0.7356062, b = -0.5102405, c = 0.2897516,
                           D = 2.057638)
  expect_identical(resp_loglik(resp = 1, ip = ip, theta = -0.5288874,
                           derivative = 2),
               -0.1673824, ignore_attr = TRUE, tolerance = 1e-5)

  # -------------------------------------------------------------------------- #
  # All of the elements of response matrix should be numeric, otherwise NAs
  # will be introduced for non-numeric response values.
  n <- sample(10:20, 1) # number of examinees
  ip <- item(b = rnorm(1))
  resp <- data.frame(i1 = sim_resp(ip = ip, theta = rnorm(n)))
  resp[1, 1] <- 'a'
  expect_warning(rllsd <- resp_loglik(ip = ip, resp = resp, theta = rnorm(n),
                                      derivative = 2))
  expect_true(is.na(rllsd[1]))

  ### Missing Responses ###
  # -------------------------------------------------------------------------- #
  # Missing Responses should return NA
  ip <- item(a = 1.4, b = 0.6, c =  0.05, d = 0.96, D = 1)
  resp = c(NA, 0, 0, 0, 1, NA, 0)
  theta = -3:3
  expect_identical(which(is.na(resp_loglik(resp = resp, ip = ip, theta = theta,
                                       derivative = 2))), c(1L, 6L))
})


###############################################################################@
############################# resp_loglik (Second Derivative) (Itempool) @######
###############################################################################@

test_that("resp_loglik (Second Derivative) - Itempool", {
  # -------------------------------------------------------------------------- #
  # All of the elements of response matrix should be numeric
  n <- sample(10:20, 1) # number of examinees
  ip <- itempool(b = rnorm(sample(5:10, 1)))
  theta <- rnorm(n)
  resp1 <- resp2 <- data.frame(sim_resp(ip = ip, theta = rnorm(n)))
  resp2[1, 1] <- 'a'
  sd1 <- resp_loglik(ip = ip, resp = resp1, theta = theta, derivative = 2)
  expect_warning(sd2 <- resp_loglik(ip = ip, resp = resp2, theta = theta,
                                    derivative = 2))
  expect_identical(sd1[-1], sd2[-1])
  expect_identical(sd2[1],
    resp_loglik(ip = ip[-1], resp = resp1[, -1], theta = theta,
                derivative = 2)[1])

  # -------------------------------------------------------------------------- #
  n_examinee <- sample(20:30, 1)
  D = 1.702
  ip = itempool(data.frame(a = c(0.983, 1.03), b = c(-0.581, 1.643)),
                   D = D, model = '2PL', item_id = paste0('irt', 1:2))
  item1 = item(a = 1.027, b = c(-2.439, -0.21, 0.693), D = D, model = 'GPCM')
  item2 = item(a = 1.024, b = c(0.241, 0.257, 0.311), D = D, model = 'GPCM')
  item3 = item(a = 0.896, b = c(-0.225, 0.458, 0.764), D = D, model = 'GPCM')
  ip = c(ip, itempool(c(item1, item2, item3), item_id = paste0('grm', 1:3)))
  item1 = item(a = 0.927, b = c(2.086, 0.107), D = D, model = 'GPCM')
  item2 = item(a = 1.201, b = c(-0.349, 1.162), D = D, model = 'GPCM')
  ip = c(ip, itempool(c(item1, item2), item_id = paste0('gpcm', 1:2)))
  theta <- rnorm(n_examinee)
  resp <- sim_resp(ip = ip, theta = theta)
  # Check regular response matrix
  output <- resp_loglik(ip = ip, resp = resp, theta = theta, derivative = 2)
  expect_identical(length(output), n_examinee)
  # Check with NAs
  resp[sample(1:(prod(dim(resp))), nrow(resp) * 2)] <- NA
  output <- resp_loglik(ip = ip, resp = tibble::as_tibble(resp), theta = theta,
                        derivative = 2)
  expect_identical(length(output), n_examinee)
  # Check tibble
  output <- resp_loglik(ip = ip, resp = tibble::as_tibble(resp), theta = theta,
                        derivative = 2)
  expect_identical(length(output), n_examinee)



  # -------------------------------------------------------------------------- #
  # Missing responses should not change the log likelihood or a response string
  n <- sample(10:20, 1)
  ip <- itempool(a = runif(n, .7, 1.5), b = runif(n, -2, 2))
  ip_list <- ip$item_list
  resp <- sim_resp(ip = ip, theta = rnorm(1, 0, .4))
  # Randomly drop two items and create an item pool
  selected_items <- sample(1:n, 2)
  ip_small <- itempool(ip_list[-selected_items])
  resp_small <- resp[, -selected_items]
  resp_na <- resp
  resp_na[, selected_items] <- NA
  theta <- rnorm(1)
  expect_identical(resp_loglik(ip = ip, resp = resp_na, theta = theta,
                           derivative = 2),
               resp_loglik(ip = ip_small, resp = resp_small, theta = theta,
                           derivative = 2))

  # -------------------------------------------------------------------------- #
  # 2PL - Second Derivative
  # Rose, N. (2010), p.7,  Equation 35
  ip <- generate_ip(model = "2PL", D = 1)
  theta <- rnorm(1)
  resp <- generate_resp(ip = ip, theta = theta)[[1]]$score
  p <- prob(ip = ip, theta = theta)[, 1] # probability of correct responses
  expected <- -1 * sum(ip$a^2 * p * (1 - p))
  observed <- resp_loglik(resp = resp, ip = ip, theta = theta, derivative = 2)
  expect_equal(expected, observed)

})



###############################################################################@
############################# resp_loglik (Response) @##########################
###############################################################################@

test_that("resp_loglik - Response", {

  # Single theta
  ip <- itempool(data.frame(
    a = c(1.36947439378127, 1.19103434309363, 0.966255453648046,
          1.41118730790913, 1.03645211085677, 1.44584470195696,
          1.42817918118089),
    b = c(-0.221226601859114, 1.56170696559919, -1.34820736712213,
          -0.374769023260681, -0.164801504563551, 0.0918581153824936,
          -0.758465659387833),
    c = c(0.132225778349675, 0.252476210868917, 0.251043332414702,
          0.20630893916823, 0.190127589460462, 0.051204833923839,
          0.0552326969336718),
    D = 2.129246))
  resp <- response(score = c(0, 0, 1, 0, 0, 0, 1), item_id = ip$item_id)
  theta <- -0.5545929
  expect_s4_class(resp, "Response")
  expect_identical(
    resp_loglik(resp = resp, theta = theta, ip = ip),
    -2.72455, ignore_attr = TRUE, tolerance = 1e-5)

  # -------------------------------------------------------------------------- #
  # Multiple theta's
  theta <- rnorm(5)
  ip <- generate_ip(n = 8)
  resp <- sim_resp(theta = rnorm(1), ip = ip, prop_missing = .2,
                   output = "response_set")@response_list[[1]]
  expected <- sapply(theta, function(x) resp_loglik(resp = resp, theta = x,
                                                    ip = ip))

  expect_identical(resp_loglik(resp = resp, theta = theta, ip = ip),
               expected, tolerance = 1e-5)

  # -------------------------------------------------------------------------- #
  # Item pool with testlet - No missingness in responses
  ip <- c(generate_testlet(n = sample(2:6, 1), item_id_preamble = "t1"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t2"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t3"),
          generate_ip(n = sample(4:7, 1)))
  ip_standalone <- itempool(irt:::flatten_itempool_cpp(ip))
  true_theta <- rnorm(1)
  resp_testlet <- generate_resp(ip = ip, theta = true_theta, prop_missing = 0)[[1]]
  resp_vector <- resp_testlet$score
  resp_sa <- response(score = resp_vector, item_id = ip_standalone$item_id)

  expect_equal(resp_sa$score, resp_testlet$score)
  expect_equal(resp_sa$item_id, resp_testlet$item_id)
  expect_equal(ip$item_id, ip_standalone$item_id)
  expect_equal(ip$b, ip_standalone$b)

  rll1 <- resp_loglik(ip = ip, resp = resp_testlet, theta = true_theta)
  rll2 <- resp_loglik(ip = ip, resp = resp_vector, theta = true_theta)
  rll3 <- resp_loglik(ip = ip_standalone, resp = resp_sa, theta = true_theta)
  rll4 <- resp_loglik(ip = ip_standalone, resp = resp_vector, theta = true_theta)
  expect_identical(rll1, rll2, tolerance = 1e-8)
  expect_identical(rll1, rll3, tolerance = 1e-8)
  expect_identical(rll1, rll4, tolerance = 1e-8)

  # -------------------------------------------------------------------------- #
  # Item pool with testlet - Missingness in responses
  ip <- c(generate_testlet(n = sample(2:6, 1), item_id_preamble = "t1"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t2"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t3"),
          generate_ip(n = sample(6:12, 1)))
  ip_standalone_full <- itempool(irt:::flatten_itempool_cpp(ip))
  true_theta <- rnorm(1)

  r1 <- generate_resp(ip = ip, theta = true_theta, prop_missing = 0)[[1]]
  r1$examinee_id <- "abc"
  r2 <- generate_resp(ip = ip, theta = true_theta, prop_missing = .3)[[1]]
  temp_resp_set <- c(r1, r2)

  # A Response object with Testlet info
  resp_testlet <- temp_resp_set[[2]]
  # A vector with only responses to non-missing items in the item pool
  resp_vector <- setNames(resp_testlet$score, resp_testlet$item_id)
  # A vector with all responses to the items in the item pool including NAs
  resp_vector_full <- as.matrix(temp_resp_set)[2, ]
  # A Response object without Testlet info
  resp_sa <- response(score = resp_vector_full,
                      item_id = ip_standalone_full$item_id)
  # Item pool with only answered items
  ip_standalone <- ip_standalone_full[resp_sa$item_id]

  expect_equal(unname(resp_sa$score), resp_testlet$score)
  expect_equal(resp_sa$item_id, resp_testlet$item_id)

  rll_1 <- resp_loglik(ip = ip, resp = resp_testlet, theta = true_theta)
  rll_2 <- resp_loglik(ip = ip, resp = resp_vector_full, theta = true_theta)
  rll_3 <- resp_loglik(ip = ip_standalone, resp = resp_vector, theta = true_theta)
  rll_4 <- resp_loglik(ip = ip_standalone, resp = resp_sa, theta = true_theta)
  rll_5 <- resp_loglik(ip = ip_standalone_full, resp = resp_vector_full,
                       theta = true_theta)
  expect_identical(rll_1, rll_2)
  expect_identical(rll_1, rll_3, tolerance = 1e-10)
  expect_identical(rll_1, rll_4, tolerance = 1e-10)
  expect_identical(rll_1, rll_5)


  # ---------------------------------------------------------------------------#
  # The log-likelihood of testlet and standalone items are the same with
  # missingness
  ip <- c(generate_testlet(n = sample(2:6, 1), item_id_preamble = "t1"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t2"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t3"),
          generate_ip(n = sample(6:12, 1)))
  ip_standalone <- itempool(irt:::flatten_itempool_cpp(ip))
  true_theta <- rnorm(20)
  resp <- sim_resp(ip = ip, theta = true_theta, prop_missing = .3)
  i <- sample(nrow(resp), 1)
  resp_vector <- resp[i, ]
  resp_set <- response_set(resp, ip = ip)[[i]]
  resp_set_sa <- response_set(resp, ip = ip_standalone)[[i]]
  resp_vector_short <- resp_vector[!is.na(resp_vector)]
  ip_standalone_short <- ip_standalone[names(resp_vector_short)]
  resp_set_sa_short <- response(score = resp_vector_short,
                                item_id = names(resp_vector_short))

  theta <- rnorm(1)
  observed_resp <- irt:::resp_loglik_response_cpp(
    theta = theta, resp = resp_set, ip = ip)
  observed_resp_sa_short <- irt:::resp_loglik_response_cpp(
    theta = theta, resp = resp_set_sa_short, ip = ip_standalone_short)
  observed_resp_vec <- irt:::resp_loglik_bare_itempool_cpp(
    resp = resp_vector, theta = theta, ip = ip)
  observed_resp_vec_sa <- irt:::resp_loglik_bare_itempool_cpp(
    resp = resp_vector, theta = theta, ip = ip_standalone)
  observed_resp_vec_short <- irt:::resp_loglik_bare_itempool_cpp(
    resp = resp_vector_short, theta = theta, ip = ip_standalone_short)

  expect_equal(observed_resp, observed_resp_sa_short)
  expect_equal(observed_resp, observed_resp_vec)
  expect_equal(observed_resp, observed_resp_vec_sa)
  expect_equal(observed_resp, observed_resp_vec_short)


})


###############################################################################@
############################# resp_loglik (Response_set) @######################
###############################################################################@

test_that("resp_loglik - Response_set", {
  theta <- rnorm(6)
  ip <- generate_ip(n = 8)
  resp <- sim_resp(theta = theta, ip = ip, prop_missing = .6,
                   output = "response_set")
  observed <- resp_loglik(resp = resp, ip = ip, theta = theta)
  i <- sample(1:6, 1)
  expect_identical(observed[i], resp_loglik(ip = ip, resp = resp@response_list[[i]],
                                        theta = theta[i]))

  # # Comparison of speed's for two functions.
  # x <- as.matrix(resp)
  # microbenchmark::microbenchmark(
  #   old = irt:::resp_loglik_itempool_cpp(resp = x, theta = theta, ip = ip),
  #   new = resp_loglik(resp = resp, ip = ip, theta = theta)
  #   )

  # # Result
  # # Unit: microseconds
  # # expr  min    lq    mean median     uq   max neval cld
  # #  old 44.4 61.80  75.585  73.60  86.45 150.2   100   a
  # #  new 63.2 85.95 105.445 107.35 122.10 201.6   100   b
})


###############################################################################@
############################# resp_loglik_testlet_cpp @#########################
###############################################################################@

test_that("resp_loglik_testlet_cpp", {
  theta <- 1.43599
  ip <- itempool(data.frame(
    item_id = c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5"),
    model = c("3PL", "3PL", "3PL", "3PL", "3PL"),
    a = c(1.1305, 1.0788, 1.3404, 0.7606, 0.8043),
    b = c(0.1894, -0.2616, 0.0367, -0.795, -0.3029),
    c = c(0.0532, 0.1842, 0.0766, 0.0581, 0.2393),
    D = c(1, 1, 1, 1, 1)))
  resp <- new("Response", examinee_id = NULL,
              item_id = c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5"),
              testlet_id = NULL, score = c(0L, 1L, 1L, 1L, 1L),
              raw_response = NULL, order = 1:5, response_time = NULL,
              misc = NULL)
  # irt:::resp_loglik_response_cpp(resp = resp, theta = theta, ip = ip)
  expect_equal(resp_loglik_response_cpp(resp = resp, theta = theta, ip = ip),
               -2.253917167)


  # -------------------------------------------------------------------------- #
  theta <- -0.241884
  t1 <- new("Testlet", testlet_id = NULL, item_list = new("Itempool", item_list = list(
    t1Item_1 = new("3PL", a = 1.0907, b = -0.9482, c = 0.1413,
                   D = 1, se_a = NULL, se_b = NULL, se_c = NULL,
                   item_id = "t1Item_1", content = NULL, misc = NULL),
    t1Item_2 = new("3PL", a = 0.8866, b = 0.9205, c = 0.1702, D = 1,
                   se_a = NULL, se_b = NULL, se_c = NULL, item_id = "t1Item_2",
                   content = NULL, misc = NULL),
    t1Item_3 = new("3PL", a = 1.0734, b = 1.0108, c = 0.1162, D = 1,
                   se_a = NULL, se_b = NULL, se_c = NULL, item_id = "t1Item_3",
                   content = NULL, misc = NULL)), misc = NULL), model = "BTM",
    parameters = NULL, se_parameters = NULL, content = NULL, misc = NULL)
  t2 <- new("Testlet", testlet_id = NULL, item_list = new("Itempool", item_list = list(
    t2Item_1 = new("3PL", a = 0.8333, b = 1.0734, c = 0.2881,
                   D = 1, se_a = NULL, se_b = NULL, se_c = NULL,
                   item_id = "t2Item_1", content = NULL, misc = NULL),
    t2Item_2 = new("3PL", a = 1.4286, b = 2.0422, c = 0.2038, D = 1,
                   se_a = NULL, se_b = NULL, se_c = NULL, item_id = "t2Item_2",
                   content = NULL, misc = NULL)),
    misc = NULL), model = "BTM", parameters = NULL, se_parameters = NULL,
    content = NULL, misc = NULL)

  ip1 <- new("Itempool", item_list = list(
    Item_1 = new("3PL", a = 0.8271, b = -0.7105, c = 0.0637, D = 1,
                 se_a = NULL, se_b = NULL, se_c = NULL, item_id = "Item_1",
                 content = NULL, misc = NULL),
    Item_2 = new("3PL", a = 0.9432, b = 0.8275, c = 0.0718, D = 1, se_a = NULL,
                 se_b = NULL, se_c = NULL, item_id = "Item_2", content = NULL,
                 misc = NULL),
    Item_3 = new("3PL", a = 0.8603, b = -0.7635, c = 0.2237, D = 1, se_a = NULL,
                 se_b = NULL, se_c = NULL, item_id = "Item_3", content = NULL,
                 misc = NULL)), misc = NULL)
  ip <- c(t1, ip1, t2)
  resp <- new("Response", examinee_id = NULL,
              item_id = c("t1Item_1", "t1Item_2", "t1Item_3", "Item_1",
                          "Item_2", "Item_3", "t2Item_1", "t2Item_2"),
              testlet_id = c("Testlet_1", "Testlet_1", "Testlet_1", NA, NA, NA,
                             "Testlet_2", "Testlet_2"),
              score = c(1L, 1L, 0L, 0L, 0L, 1L, 1L, 0L), raw_response = NULL,
              order = 1:8, response_time = NULL, misc = NULL)
  expect_equal(resp_loglik_response_cpp(resp = resp, theta = theta, ip = ip),
               -4.36309831367241)

  # -------------------------------------------------------------------------- #

  theta <- rnorm(1)
  ip <- c(generate_testlet(n = 3, item_id_preamble = "t1"),
          generate_ip(n = 9),
          generate_testlet(n = 2, item_id_preamble = "t2"),
          generate_testlet(n = 5, item_id_preamble = "t3"))
  resp <- sim_resp(theta = theta, ip = ip, prop_missing = .1,
                   output = "response_set")[[1]]
  resp2 <- resp
  resp2@testlet_id <- NULL
  ip2 <- itempool(irt:::flatten_itempool_cpp(ip))
  expect_equal(
    resp_loglik_response_cpp(resp = resp, theta = theta, ip = ip),
    resp_loglik_response_cpp(resp = resp2, theta = theta, ip = ip2))

  # microbenchmark::microbenchmark(
  #   old = irt:::resp_loglik_response_cpp(resp = resp2, theta = theta,
  #                                        ip_list = ip2@item_list),
  #   slow = irt:::resp_loglik_response2_cpp(resp = resp, theta = theta, ip = ip),
  #   fast = irt:::resp_loglik_response2_cpp(resp = resp2, theta = theta, ip = ip2),
  #   times = 10000)

  # -------------------------------------------------------------------------- #
})
