
#%%%%%%%%%%%%%%%%%%#############################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% prob %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###############################################################################@
############################# prob (Item) @#####################################
###############################################################################@

test_that("prob - Item", {
  ##  Single theta

  # ---------------------------------------------------------------------------#
  theta = -1.473
  b = 0.19
  D = 1.702
  ip = item(b = b, D = D, model = '1PL')
  expected <- 0.0557019863959564
  object <-  prob(ip = ip, theta = theta)
  expect_true(is.matrix(object))
  expect_identical(colnames(object), paste0(0:1))
  expect_identical(unname(object[, 2]), expected, tolerance = 1e-6)
  expect_identical(unname(object[, 1]), 1 - expected, tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  theta = 0.298
  b = -1.131
  D = 1.702
  ip = item(b = b, D = D, model = '1PL')
  expected <- 0.919246870574227
  object <-  prob(ip = ip, theta = theta)
  expect_true(is.matrix(object))
  expect_identical(colnames(object), paste0(0:1))
  expect_identical(unname(object[, 2]), expected, tolerance = 1e-6)
  expect_identical(unname(object[, 1]), 1 - expected, tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  theta = -0.11
  a = 1.825
  b = 1.728
  D = 1.702
  ip = item(a = a, b = b, D = D, model = '2PL')
  expected <- 0.00330468586568524
  object <-  prob(ip = ip, theta = theta)
  expect_true(is.matrix(object))
  expect_identical(colnames(object), paste0(0:1))
  expect_identical(unname(object[, 2]), expected, tolerance = 1e-6)
  expect_identical(unname(object[, 1]), 1 - expected, tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  theta = 1.797
  a = 1.021
  b = -1.929
  D = 1.702
  ip = item(a = a, b = b, D = D, model = '2PL')
  expected <- 0.998460607268952
  object <-  prob(ip = ip, theta = theta)
  expect_true(is.matrix(object))
  expect_identical(colnames(object), paste0(0:1))
  expect_identical(unname(object[, 2]), expected, tolerance = 1e-6)
  expect_identical(unname(object[, 1]), 1 - expected, tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  theta = -0.12
  a = 1.802
  b = -1.857
  c = 0.279
  D = 1.702
  ip = item(a = a, b = b, c = c, D = D, model = '3PL')
  expected <- 0.996515208564727
  object <- prob(ip = ip, theta = theta)
  expect_true(is.matrix(object))
  expect_identical(colnames(object), paste0(0:1))
  expect_identical(unname(object[, 2]), expected, tolerance = 1e-6)
  expect_identical(unname(object[, 1]), 1 - expected, tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  # 4PL
  theta = -1.881
  a = 0.609
  b = -1.067
  c = 0.173
  d = 0.966
  D = 1.702
  ip = item(a = a, b = b, c = c, d = d, D = D, model = '4PL')
  expected <- 0.411495304540055
  object <- prob(ip = ip, theta = theta)
  expect_true(is.matrix(object))
  expect_identical(colnames(object), paste0(0:1))
  expect_identical(unname(object[, 2]), expected, tolerance = 1e-6)
  expect_identical(unname(object[, 1]), 1 - expected, tolerance = 1e-6)

  ##  Multiple theta
  # ---------------------------------------------------------------------------#
  theta = c(1.216, 1.712, -0.384, 1.87, 1.214)
  a = 1.671
  b = 1.246
  c = 0.068
  d = 0.962
  D = 1.702
  ip = item(a = a, b = b, c = c, d = d, D = D, model = '4PL')
  expected <- c(0.495942258235406, 0.774319045715691, 0.0765871387194444,
                0.832404851239597, 0.494673439580967)
  object <- prob(ip = ip, theta = theta)
  expect_true(is.matrix(object))
  expect_identical(colnames(object), paste0(0:1))
  expect_identical(unname(object[, 2]), expected, tolerance = 1e-6)
  expect_identical(unname(object[, 1]), 1 - expected, tolerance = 1e-6)

  ## Polytomous Items - Graded Response Model
  # ---------------------------------------------------------------------------#
  # Graded Response Model
  theta = 0.516
  a = 0.926
  b = c(-0.836, 0.184, 1.595)
  D = 1.702
  ip = item(a = a, b = b, D = D, model = 'GRM')
  expected <- c(0.106136952861801, 0.265955808662458, 0.473515112558898,
                0.154392125916843)
  object <- prob(ip = ip, theta = theta)
  expect_identical(object,
               matrix(expected, nrow = length(theta),
                      dimnames = list(ip$item_id, paste0(0:length(b)))),
               tolerance = 1e-6)


  # ---------------------------------------------------------------------------#
  # Multiple theta:
  theta = c(0.516, -1.753)
  a = 0.926
  b = c(-0.836, 0.184, 1.595)
  D <- 1.702
  ip = item(a = a, b = b, D = D, model = 'GRM')
  expected <- matrix(c(0.106136952861801, 0.265955808662458, 0.473515112558898,
                       0.154392125916843, 0.809264735403842, 0.145639069218062,
                       0.0400125096711436, 0.00508368570695249), nrow = 2,
                     byrow = TRUE)
  object <- prob(ip = ip, theta = theta)
  expect_identical(object,
               matrix(expected, nrow = length(theta),
                      dimnames = list(NULL, paste0(0:length(b)))),
               tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  # Multiple items
  theta = 0.067
  a = 0.801
  b = c(-1.479, -0.545, 0.018)
  D <- 1.702
  ip1 = item(a = a, b = b, D = D, model = 'GRM')
  expected1 <- c(0.108354063390494, 0.194373945466998, 0.180577749324481,
                 0.516694241818027)
  object <- prob(ip = ip1, theta = theta)
  expect_identical(object,
               matrix(expected1, nrow = length(theta),
                      dimnames = list(ip1$item_id, paste0(0:length(b)))),
               tolerance = 1e-6)

  a = 1.25
  b = c(-0.71, -0.454, -0.197)
  ip2 = item(a = a, b = b, D = D, model = 'GRM')
  expected2 <- c(0.160694799045208, 0.0874693683397382, 0.114999287434029,
                 0.636836545181025)
  object <- prob(ip = ip2, theta = theta)
  expect_identical(object,
               matrix(expected2, nrow = length(theta),
                      dimnames = list(ip2$item_id, paste0(0:length(b)))),
               tolerance = 1e-6)

  a = 1.068
  b = c(-1.156, 0.206, 0.358)
  ip3 = item(a = a, b = b, D = D, model = 'GRM')
  expected3 <- c(0.0976959777464697, 0.465136436208854, 0.0664083762337866,
                 0.37075920981089)
  object <- prob(ip = ip3, theta = theta)
  expect_identical(object,
               matrix(expected3, nrow = length(theta),
                      dimnames = list(ip3$item_id, paste0(0:length(b)))),
               tolerance = 1e-6)
  # Final check:
  ip <- itempool(c(ip1, ip2, ip3))
  object <- prob(ip = ip, theta = theta)
  expected <- matrix(c(expected1, expected2, expected3), nrow = 3, byrow = TRUE,
                     dimnames = list(ip$item_id, paste0(0:length(b))))
  expect_identical(object, expected, tolerance = 1e-6)
  # ---------------------------------------------------------------------------#

  ## Polytomous Items - Generalized Partial Model
  # -------------------------------------------------------------------------- #
  theta = 1.506
  D = 1.702
  a = 0.959
  b = c(-0.733, 0.1, 1.419)
  ip = item(a = a, b = b, D = D, model = 'GPCM')
  expected <- c(0.00115574540552809, 0.0446690831664985, 0.443270466924079,
                0.510904704503894)
  object <- prob(ip = ip, theta = theta)
  expect_identical(object,
               matrix(expected, nrow = length(theta),
                      dimnames = list(ip$item_id, paste0(0:length(b)))),
               tolerance = 1e-6)

  ## Polytomous Items - Reparameterized Generalized Partial Model
  # -------------------------------------------------------------------------- #
  theta <- rnorm(1)
  D <- 1.702
  a <- runif(1, .5, 1.5)
  b <- rnorm(1, 0, .25)
  d <- c(runif(1, -3, -1.75), runif(1, -1.5, 1), runif(1, 1.75, 2.5))
  ip_gpcm2 <- item(a = a, b = b, d = d, D = D, model = 'GPCM2')
  ip_gpcm <- item(a = a, b = b - d, D = D, model = 'GPCM')
  expect_identical(prob(ip = ip_gpcm2, theta = theta),
               prob(ip = ip_gpcm, theta = theta))
  # Derivative of Polytomous Items - Reparameterized Generalized Partial Model
  expect_identical(prob(ip = ip_gpcm2, theta = theta, derivative = 0),
               prob(ip = ip_gpcm, theta = theta, derivative = 0))
  expect_identical(prob(ip = ip_gpcm2, theta = theta, derivative = 1),
               prob(ip = ip_gpcm, theta = theta, derivative = 1))
  expect_identical(prob(ip = ip_gpcm2, theta = theta, derivative = 2),
               prob(ip = ip_gpcm, theta = theta, derivative = 2))

  # -------------------------------------------------------------------------- #
  # A single item as Itempool
  theta = 0.598
  D = 1.702
  a = 1.048
  b = c(1.059, 1.338, 0.885)
  ip = itempool(item(a = a, b = b, D = D, model = 'GPCM'))
  expected <- c(0.614560032837231, 0.270054222262271, 0.0721457026438114,
                0.0432400422566862)
  object <- prob(ip = ip, theta = theta)
  expect_identical(object,
               matrix(expected, nrow = length(theta),
                      dimnames = list(ip$item_id, paste0(0:length(b)))),
               tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  #### TODO: Add more tests like GRM above.
})


###############################################################################@
############################# prob (Itempool) @#################################
###############################################################################@

test_that("prob - Itempool", {

  # ---------------------------------------------------------------------------#
  theta = -1.392
  b = c(-0.529, 1.144, 1.369, 1.619, 1.893, 1.926)
  D = 1.702
  ip = itempool(data.frame(b = b), D = D, model = '1PL',
                    item_id = paste0('i', 1:6))
  expected <- c(0.187121121770166, 0.0131736950374726, 0.00902025044997784,
                0.00591270264426274, 0.00371716465628692, 0.00351485485896469)
  object <- prob(ip = ip, theta = theta)
  expect_identical(object, matrix(c(1 - expected, expected),
                                  nrow = ip$n$items, ncol = 2,
                                  dimnames = list(ip$resp_id, 0:1)),
                   tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  theta = -1.254
  a = c(1.793, 1.788, 1.399, 0.938, 1.123, 1.898)
  b = c(-1.76, -1.124, -0.518, 0.055, 0.093, 1.428)
  c = c(0.061, 0.106, 0.314, 0.118, 0.19, 0.16)
  d = c(0.931, 0.982, 0.957, 0.942, 0.913, 0.953)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b, c, d), D = D, model = '4PL',
                   item_id = paste0('i', 1:6))
  expected <- c(0.777938860052521, 0.458473358386346, 0.408992735094507,
                0.208716988480645, 0.241182784762078, 0.160136931902186)
  object <- prob(ip = ip, theta = theta)
  expect_identical(object, matrix(c(1 - expected, expected),
                                  nrow = ip$n$items, ncol = 2,
                                  dimnames = list(ip$resp_id, 0:1)),
                   tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  theta = 1.245
  a = c(1.487, 1.747, 0.701)
  b = c(-1.168, -0.535, 0.164)
  c = c(0.094, 0.225, 0.126)
  d = c(0.957, 0.956, 0.93)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b, c, d), D = D, model = '4PL',
                   item_id = paste0('i', 1:3))
  expected <- c(0.955082176439385, 0.952342586223101, 0.756419399271527)
  object <- prob(ip = ip, theta = theta)
  expect_identical(object, matrix(c(1 - expected, expected),
                                  nrow = ip$n$items, ncol = 2,
                                  dimnames = list(ip$resp_id, 0:1)),
                   tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  # Multiple theta's and multiple items
  theta1 = 0.736
  a = c(0.798, 0.716, 1.273)
  b = c(-0.413, 0.198, 1.287)
  c = c(0.167, 0.298, 0.108)
  d = c(0.942, 0.978, 0.972)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b, c, d), D = D, model = '4PL',
                   item_id = paste0('i', 1:3))
  expected1 <- c(0.807486914197291, 0.745628156247141, 0.308945733768446)
  expected1 <- matrix(c(1 - expected1, expected1), nrow = ip$n$items, ncol = 2,
                      dimnames = list(ip$resp_id, 0:1))
  object <- prob(ip = ip, theta = theta1)
  expect_identical(object, expected1, tolerance = 1e-6)

  theta2 = -1.235
  expected2 <- c(0.358171389452527, 0.3989898787987, 0.111643938552127)
  expected2 <- matrix(c(1 - expected2, expected2), nrow = ip$n$items, ncol = 2,
                      dimnames = list(ip$resp_id, 0:1))
  object <- prob(ip = ip, theta = theta2)
  expect_identical(object, expected2, tolerance = 1e-6)

  # Final check
  object <- prob(ip, c(theta1, theta2))
  # expected <- matrix(c(expected1, expected2), nrow = 2, byrow = TRUE,
  #                    dimnames = list(names(c(theta1, theta2)), ip$resp_id))
  expect_identical(object, list(expected1, expected2), tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  # A single item as Itempool
  theta = -1.257
  a = 0.896
  b = c(0.124, 0.954, 2.236)
  D <- 1.702
  ip = itempool(item(a = a, b = b, D = D, model = 'GRM'))
  expected <- c(0.89148633042075, 0.0753237961856628, 0.028353764824902,
                0.00483610856868537)
  object <- prob(ip = ip, theta = theta)
  expect_identical(object,
               matrix(expected, nrow = length(theta),
                      dimnames = list(ip$item_id, paste0(0:length(b)))),
               tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  theta = 1.264
  a = c(1.563, 0.726, 1.859, 1.011, 1.905)
  b = c(-1.792, -1.442, 0.425, 0.656, 0.663)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b), D = D, model = '2PL')
  expected <- c(0.999705415681076, 0.965897081219716, 0.934294625159321,
                0.740044330513533, 0.875296995939429)
  object <- prob(ip = ip, theta = theta)
  expect_identical(object, matrix(c(1 - expected, expected),
                                  nrow = ip$n$items, ncol = 2,
                                  dimnames = list(ip$resp_id, 0:1)),
                   tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  theta = -1.775
  a = c(1.554, 1.269, 1.876, 1.535)
  b = c(0.716, 0.72, 0.807, 1.573)
  c = c(0.147, 0.316, 0.27, 0.084)
  d = c(0.94, 0.918, 0.949, 0.931)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b, c, d), D = D, model = '4PL')
  expected <- c(0.148089786919099, 0.318737115324176, 0.270178379196062,
                0.0841346141678334)
  object <- prob(ip = ip, theta = theta)
  expect_identical(object, matrix(c(1 - expected, expected),
                                  nrow = ip$n$items, ncol = 2,
                                  dimnames = list(ip$resp_id, 0:1)),
                   tolerance = 1e-6)


  # ---------------------------------------------------------------------------#
  theta = 1.906
  a = c(1.768, 0.821, 0.721, 1.34, 0.881)
  b = c(-0.911, -0.655, 0.607, 0.613, 0.946)
  c = c(0.07, 0.059, 0.248, 0.319, 0.104)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b, c), D = D, model = '3PL')
  expected <- c(0.999806361016139, 0.974445387042363, 0.873052017464943,
                0.966094633397476, 0.828303694293968)
  object <- prob(ip = ip, theta = theta)
  expect_identical(object, matrix(c(1 - expected, expected),
                                  nrow = ip$n$items, ncol = 2,
                                  dimnames = list(ip$resp_id, 0:1)),
                   tolerance = 1e-6)

  # ---------------------------------------------------------------------------#
  # Polytomous items
  # For one item and one theta it returns the same as prob.Item function:
  item <- item(a = 1, b = c(-1, 0, 1), model = 'GRM', item_id = "xynes")
  ip <- itempool(item)
  expect_identical(prob(item, theta = 0),  prob(ip, theta = 0),
                   ignore_attr = TRUE)

  # For multiple items and one theta it returns a matrix where each line
  # represents an item and columns represents response categories
  item1 <- item(a = 1, b = c(-1, 0, 1), model = 'GRM', item_id = "isn-1")
  item2 <- item(a = 1, b = c(-2, 0, 2), model = 'GRM', item_id = "isn-2")
  theta = 0
  ip <- c(item1, item2)
  p <- prob(ip, theta = theta)
  expect_identical(p[1,], prob(item1, theta)[1, ])
  expect_identical(p[2,], prob(item2, theta)[1, ])

  # ---------------------------------------------------------------------------#
  # Polytomous items with different number of categories
  n_items <- sample(10:20, 1)
  ip <- c(generate_ip(model = rep("GRM", n_items),
                      n_categories = sample(3:6, n_items, T)))
  theta <- rnorm(1)
  p <- prob(ip, theta = theta)
  for (i in 1:n_items) {
    temp_p <- prob(ip = ip[[i]], theta = theta)
    temp_p <- matrix(c(temp_p, rep(NA, ncol(p) - length(temp_p))), nrow = 1,
                     dimnames = list(ip$item_id[i], paste0(0:(ncol(p) - 1))))
    expect_identical(p[i, , drop = FALSE], temp_p)
  }

  # ---------------------------------------------------------------------------#
  # Polytomous items with different number of categories GPCM2
  for (i in 1:100) {
    n_items <- sample(10:20, 1)
    n_categories <- as.integer(c(sample(3:7, n_items - 3, T), rep(2, 3)))
    theta <- rnorm(1)
    ip <- generate_ip(model = c(rep("GPCM", n_items - 3), rep("2PL", 3)),
                      n_categories = n_categories)
    p <- prob(ip, theta = theta)
    expect_identical(ncol(p), max(n_categories))
  }

  ip <- generate_ip(model = c(rep("GPCM2", n_items - 3), rep("2PL", 3)),
                    n_categories = n_categories)
  p <- prob(ip, theta = theta)
  expect_identical(ncol(p), max(n_categories))

  # ---------------------------------------------------------------------------#
  # Polytomous items with different number of categories and dichotomous items
  n_items <- sample(10:20, 1)
  models <- sample(c("3PL", "GRM"), n_items, TRUE)
  ip <- generate_ip(model = models, n_categories = sample(3:6, n_items, T))
  theta <- rnorm(1)
  p <- prob(ip, theta = theta)
  for (i in 1:n_items) {
    temp_p <- prob(ip = ip[[i]], theta = theta)
    if (models[i] == "3PL") {
      expect_identical(p[i, 1], unname(temp_p[1, 1]))
      expect_identical(p[i, 2], unname(temp_p[, 2]))
    } else  if (models[i] == "GRM") {
      temp_p <- c(temp_p, rep(NA, ncol(p) - length(temp_p)))
      names(temp_p) <- 0:(ncol(p) - 1)
      expect_identical(p[i, ], temp_p)
    }
  }

  # ---------------------------------------------------------------------------#
  # Testlets with Polytomous items with different number of categories and
  # dichotomous items
  ip <- c(generate_ip(n = 2),
          generate_testlet(testlet_id = "t1",
                           item_models = c("3PL", "GRM", "GPCM", "GRM", "2PL"),
                           n_categories = c(2, 3, 6, 7, 2)),
          generate_testlet(n = 3, testlet_id = "t2"))
  theta <- rnorm(1)
  p <- prob(ip, theta = theta)
  expect_identical(p[1, 1:2], prob(ip = ip[[1]], theta = theta),
                   ignore_attr = TRUE)
  expect_identical(p[1, 2], prob(ip = ip[[1]], theta = theta)[, 2],
                   ignore_attr = TRUE)
  expect_identical(p[3, 1], 1 - prob(ip = ip[[3]][[1]], theta = theta)[2],
                   ignore_attr = TRUE)
  expect_identical(p[3, 1:2], prob(ip = ip[[3]][[1]], theta = theta),
                   ignore_attr = TRUE)
  expect_identical(p[4, 1:3], prob(ip = ip[[3]][[2]], theta = theta)[1,])
  expect_identical(p[5, 1:6], prob(ip = ip[[3]][[3]], theta = theta)[1,])
  expect_identical(p[6, 1:7], prob(ip = ip[[3]][[4]], theta = theta)[1,])
  expect_identical(p[7, 1], 1 - prob(ip = ip[[3]][[5]], theta = theta)[2],
                   ignore_attr = TRUE)
  expect_identical(p[7, 1:2], prob(ip = ip[[3]][[5]], theta = theta),
                   ignore_attr = TRUE)


})


###############################################################################@
############################# prob (Testlet) @##################################
###############################################################################@

test_that("prob - Testlet", {
  # 3PM items
  testlet <- generate_testlet(n = 4)
  theta <- rnorm(1)
  expect_identical(prob(ip = testlet, theta = theta),
               prob(ip = testlet@item_list, theta = theta))
  expect_identical(prob(ip = testlet, theta = theta, derivative = 1),
               prob(ip = testlet@item_list, theta = theta, derivative = 1))
  expect_identical(prob(ip = testlet, theta = theta, derivative = 2),
               prob(ip = testlet@item_list, theta = theta, derivative = 2))

  # ---------------------------------------------------------------------------#
  # All items are GRM
  testlet <- generate_testlet(n = 4, item_models = "GRM")
  theta <- rnorm(1)
  expect_identical(prob(ip = testlet, theta = theta),
               prob(ip = testlet@item_list, theta = theta))
  expect_identical(prob(ip = testlet, theta = theta, derivative = 1),
               prob(ip = testlet@item_list, theta = theta, derivative = 1))
  # expect_identical(prob(ip = testlet, theta = theta, derivative = 2),
  #              prob(ip = testlet@item_list, theta = theta, derivative = 2))

  # ---------------------------------------------------------------------------#
  # Mixture of items
  testlet <- testlet(generate_ip(model = c("3PL", "2PL", "4PL", "Rasch")))
  theta <- rnorm(1)

  expect_identical(prob(ip = testlet, theta = theta),
               prob(ip = testlet@item_list, theta = theta))
  expect_identical(prob(ip = testlet, theta = theta, derivative = 1),
               prob(ip = testlet@item_list, theta = theta, derivative = 1))

})



###############################################################################@
############################# prob (REST) ######################################
###############################################################################@

test_that("prob - REST", {
  # Try character, 1 theta
  expect_error(prob("1", theta = 2), regexp = "Cannot convert object to an ")


  #
  #  set.seed(345937)
  #  tol <- 1e-5 # tol
  #
  #  # Try numeric, 1 theta
  #  theta <- rnorm(1);     n <- 1;        D = 1
  #  ipdf <- c(a = runif(n, .5, 1.5), b = rnorm(n), c = runif(n, 0,.3))
  #  # irtProb(theta = theta, ip = ipdf, D = D)
  #  expect_identical(prob(ipdf, theta = theta, D = D), 0.8111146,
  #               ignore_attr = TRUE, tolerance = tol)
  #
  #  # Try numeric, multiple theta
  #  theta <- rnorm(4);     n <- 1;        D = 1
  #  ipdf <- c(a = runif(n, .5, 1.5), b = rnorm(n), c = runif(n, 0,.3))
  #  # sum(rowSums(irtProb(theta = theta, ip = ipdf, D = D)))
  #  expect_identical(sum(prob(ipdf, theta = theta, D = D)), 3.230744,
  #               ignore_attr = TRUE, tolerance = tol)
  #
  #
  #  set.seed(3459237)
  #
  #  # Try matrix, 1 theta
  #  theta <- rnorm(1);     n <- 1;        D = 1
  #  ipdf <- matrix(c(a = runif(n, .5, 1.5), b = rnorm(n), c = runif(n, 0,.3)),
  #                 nrow = n)
  #  # irtProb(theta = theta, ip = ipdf, D = D)
  #  expect_identical(prob(ipdf, theta = theta, D = D)[1], 0.2304785,
  #               ignore_attr = TRUE, tolerance = tol)
  #
  #  # Try numeric, multiple theta
  #  theta <- rnorm(4);     n <- 5;        D = 1
  #  names(theta) <- paste0("subject-", 1:4)
  #  ipdf <- matrix(c(a = runif(n, .5, 1.5), b = rnorm(n), c = runif(n, 0,.3)),
  #                 nrow = n)
  #  # sum(rowSums(irtProb(theta = theta, ip = ipdf, D = D)))
  #  expect_identical(sum(prob(ipdf, theta = theta, D = D)), 12.42024,
  #               ignore_attr = TRUE, tolerance = tol)
  #
  #
  #  set.seed(251374)
  #
  #  # Try data.frame, 1 theta
  #  theta <- rnorm(1);     n <- 1;        D = 1
  #  ipdf <- data.frame(a = runif(n, .5, 1.5), b = rnorm(n), c = runif(n, 0,.3))
  #  # irtProb(theta = theta, ip = ipdf, D = D)
  #  expect_identical(prob(ipdf, theta = theta, D = D)[1], 0.5773126,
  #               ignore_attr = TRUE, tolerance = tol)
  #
  #  # Try data.frame, multiple theta
  #  theta <- rnorm(7);     n <- 5;        D = 1
  #  names(theta) <- paste0("subject-", 1:7)
  #  ipdf <- data.frame(a = runif(n, .5, 1.5), b = rnorm(n), c = runif(n, 0,.3))
  #  # sum(rowSums(irtProb(theta = theta, ip = ipdf, D = D)))
  #  expect_identical(sum(prob(ipdf, theta = theta, D = D)), 15.2348,
  #               ignore_attr = TRUE, tolerance = tol)
  #
  #
  #  set.seed(93618)
  #
  #  # Try data.frame, 1 theta
  #  theta <- rnorm(1);     n <- 1;        D = 1.702
  #  ipdf <- data.frame(a = runif(n, .5, 1.5), b = rnorm(n), c = runif(n, 0,.3))
  #  # irtProb(theta = theta, ip = ipdf, D = D)
  #  ipdf <- itempool(ipdf)
  #  expect_identical(prob(ipdf, theta = theta, D = D), 0.9712111,
  #               ignore_attr = TRUE, tolerance = tol)
  #
  #  # Try data.frame, multiple theta
  #  theta <- rnorm(nTheta <- 14);     n <- 5;        D = 1
  #  names(theta) <- paste0("subject-", 1:nTheta)
  #  ipdf <- data.frame(a = runif(n, .5, 1.5), b = rnorm(n), c = runif(n, 0,.3),
  #                     d = runif(n, .85, 1))
  #  # sum(rowSums(irtProb(theta = theta, ip = ipdf, D = D)))
  #  ipdf <- itempool(ipdf)
  #  expect_identical(sum(prob(ipdf, theta = theta, D = D)), 37.03391,
  #               ignore_attr = TRUE, tolerance = tol)

})


#%%%%%%%%%%%%%%%%%%#############################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% cpp %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###############################################################################@
############################# prob_4pm_bare_cpp @###############################
###############################################################################@
test_that("prob_4pm_bare_cpp", {
  # Check basic functionality
  itm <- generate_item("2PL")
  theta <- rnorm(1)
  expect_identical(prob_4pm_bare_cpp(theta = theta, item = itm, derivative = 0,
                                     resp = -9),
                   1/(1+exp(-itm$a * itm$D * (theta - itm$b))))
  # -------------------------------------------------------------------------- #
  # Check 'resp' argument
  itm <- generate_item("3PL")
  theta <- rnorm(1)
  p <- prob_4pm_bare_cpp(theta = theta, item = itm, derivative = 0, resp = -9)
  expect_identical(1 - p, prob_4pm_bare_cpp(theta = theta, item = itm,
                                            derivative = 0, resp = 0))

})


###############################################################################@
############################# prob_gpcm_bare_cpp @##############################
###############################################################################@
test_that("prob_gpcm_bare_cpp", {
  # Check basic functionality
  itm <- generate_item("GPCM", n_categories = 2)
  theta <- rnorm(1)
  p <- prob_gpcm_bare_cpp(theta = theta, item = itm, derivative = 0, resp = -9)
  p1 <- 1 - 1/(1+exp(-itm$a * itm$D * (theta - itm$b)))
  expect_identical(round(p[1], 9), round(p1, 9))
  expect_identical(round(p[2], 9), round(1 - p1, 9))

  # -------------------------------------------------------------------------- #
  # Check 'resp' argument
  itm <- generate_item("GPCM", n_categories = 2)
  theta <- rnorm(1)
  p <- prob_gpcm_bare_cpp(theta = theta, item = itm, derivative = 0, resp = 1)
  p1 <- 1/(1+exp(-itm$a * itm$D * (theta - itm$b)))
  expect_identical(round(p, 10), round(p1, 10))
  p <- prob_gpcm_bare_cpp(theta = theta, item = itm, derivative = 0, resp = 0)
  expect_identical(round(p, 10), round(1 - p1, 10))

})


#%%%%%%%%%%%%%%%%%%#############################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% prob_fd %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###############################################################################@
############################# prob_fd (Item) @##################################
###############################################################################@
test_that("prob_fd - Item", {
  # -------------------------------------------------------------------------- #
  theta = 1.184
  a = 1.851
  b = -1.058
  D = 1.702
  ip = item(a = a, b = b, D = D, model = '2PL',
               item_id = paste0('i', 1:1))
  expected <- 0.00269223774834802
  object <- prob(ip = ip, theta = theta, derivative = 1)
  expect_identical(object[2], expected, tolerance = 1e-6, ignore_attr = TRUE)

  # -------------------------------------------------------------------------- #
  theta = 0.003
  a = 1.511
  b = 0.579
  c = 0.339
  D = 1.702
  ip = item(a = a, b = b, c = c, D = D, model = '3PL',
               item_id = paste0('i', 1:1))
  expected <- 0.256549196945928
  object <- prob(ip = ip, theta = theta, derivative = 1)
  expect_identical(object[2], expected, tolerance = 1e-6, ignore_attr = TRUE)

  # -------------------------------------------------------------------------- #
  theta = -0.876
  a = 0.852
  b = 0.397
  c = 0.096
  d = 0.932
  D = 1.702
  ip = item(a = a, b = b, c = c, d = d, D = D, model = '4PL',
               item_id = paste0('i', 1:1))
  expected <- 0.142753163853866
  object <- prob(ip = ip, theta = theta, derivative = 1)
  expect_identical(object[2], expected, tolerance = 1e-6, ignore_attr = TRUE)

  # -------------------------------------------------------------------------- #
  # Check first derivative for 3PL model - Alternative Calculation
  itm <- generate_item(model = "3PL")
  itm$D <- 1.7
  theta <- rnorm(1)
  p <- prob(ip = itm, theta = theta)
  p <- p[2]
  expect_identical((itm$D*itm$a*(1-p)*(p-itm$c))/(1-itm$c),
                   prob(ip = itm, theta = theta, derivative = 1)[2],
                   tolerance = 1e-7)

  # -------------------------------------------------------------------------- #
  # Check first derivative for GPCM model - Alternative Calculation
  itm <- generate_item(model = "GPCM")
  itm$D <- 1.7
  theta <- rnorm(1)
  p <- prob(ip = itm, theta = theta)
  expect_identical(prob(ip = itm, theta = theta, derivative = 1),
               itm$D * itm$a * p * (0:itm$max_score - mean(itm, theta)))

  ##  Multiple theta
  # -------------------------------------------------------------------------- #
  theta = c(-0.583, 0.398, 1.343)
  a = 0.814
  b = -0.749
  D = 1.702
  ip = item(a = a, b = b, D = D, model = '2PL')
  expected <- c(0.341817255148554, 0.195037837645056, 0.0685900588198937)
  object <- prob(ip = ip, theta = theta, derivative = 1)
  expect_identical(object[, 2], expected, tolerance = 1e-6, ignore_attr = TRUE)


  ### Graded Response Model ###
  # -------------------------------------------------------------------------- #
  theta = 0.682
  D = 1.702
  a = 1.056
  b = c(-0.883, -0.628, 1.297)
  ip = item(a = a, b = b, D = D, model = 'GRM')
  expected <- c(-0.0960286399038229, -0.0463036205353503, -0.193528029828628,
                0.335860290267801)
  object <- prob(ip = ip, theta = theta, derivative = 1)
  expect_identical(object[1, ], expected, tolerance = 1e-6, ignore_attr = TRUE)

  # -------------------------------------------------------------------------- #
  theta = -1.88
  D = 1.702
  a = 0.794
  b = c(-1.108, -0.43, 0.161)
  item1 = item(a = a, b = b, D = D, model = 'GRM')
  a = 0.998
  b = c(-1.177, 0.043, 1.322)
  item2 = item(a = a, b = b, D = D, model = 'GRM')
  ip = itempool(c(item1, item2), item_id = paste0('i', 1:2))
  expected <- matrix(c(
    -0.260343501782074, -0.303125774658898, 0.114038638355838,
    0.243008984736819, 0.070531335195686, 0.0528013786799727,
    0.0757735282305497, 0.00731541124210607), nrow = 2,
    dimnames = list(ip$item_id, paste0(0:length(b))))
  object <- prob(ip = ip, theta = theta, derivative = 1)
  expect_identical(object, expected, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  # Another GRM Example
  itm <- new("GRM", a = 1.1585, b = c(-0.9029, -0.2997, 0.1745), D = 1,
             se_a = NULL, se_b = NULL, item_id = NULL, content = NULL, misc = NULL)
  theta <- 1.443
  observed <- unname(prob(ip = itm, theta = theta, derivative = 1)[1, ])
  expected <- c(-0.0673083, -0.0525824, -0.0562445, 0.1761351)
  expect_equal(observed, expected, tolerance = 1e-6)

  ### Generalized Partial Credit Model ###
  # -------------------------------------------------------------------------- #
  itm <- new("GPCM", a = 0.8484, b = c(-1.5604, -0.9179, -0.5875, -0.2498,
                                       0.5028, 0.8086),
             D = 1, se_a = NULL, se_b = NULL, item_id = NULL, content = NULL,
             misc = NULL)
  theta <- -1.139
  observed <- unname(prob(ip = itm, theta = theta, derivative = 1)[1, ])
  expected <- c(-0.2918071, -0.1650185, 0.0722696, 0.1762049, 0.1444488,
                0.0511681, 0.0127341)
  expect_equal(observed, expected, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  # Another GPCM Example
  itm <- new("GPCM", a = 0.9567,
             b = c(-1.4554, -0.8694, -0.4488, 0.1847, 0.5886), D = 1,
             se_a = NULL, se_b = NULL, item_id = NULL, content = NULL, misc = NULL)
  theta <- 1.145
  observed <- unname(prob(ip = itm, theta = theta, derivative = 1)[1, ])
  expected <- c(-0.0013619, -0.0126137, -0.06071, -0.1597056, -0.1014935,
                0.3358849)
  expect_equal(observed, expected, tolerance = 1e-6)

  ### Partial Credit Model ###
  # -------------------------------------------------------------------------- #
  itm <- new("PCM", b = c(-1.3333, -0.3407, 0.2952), se_b = NULL, item_id = NULL,
             content = NULL, misc = NULL)
  theta <- -1.763
  observed <- unname(prob(ip = itm, theta = theta, derivative = 1)[1, ])
  expected <- c(-0.3067518, 0.1564259, 0.1235834, 0.0267425)
  expect_equal(observed, expected, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  # Another PCM Example
  itm <- new("PCM", b = c(-1.3095, -0.9197, -0.4539, 0.08, 0.7564, 1.4256),
             se_b = NULL, item_id = NULL, content = NULL, misc = NULL)
  theta <- -0.383
  observed <- unname(prob(ip = itm, theta = theta, derivative = 1)[1, ])
  expected <- c(-0.1557388, -0.2401629, -0.1487724, 0.1215373, 0.2535055,
                0.1377699, 0.0318614)
  expect_equal(observed, expected, tolerance = 1e-6)
})


###############################################################################@
############################# prob_fd (Itempool) @#############################
###############################################################################@

test_that("prob_fd - Itempool", {
  # -------------------------------------------------------------------------- #
  theta = setNames(-1.655, "stu-213")
  a = c(1.367, 1.81, 1.734, 1.772)
  b = c(-1.653, -0.996, -0.338, 0.796)
  c = c(0.095, 0.094, 0.073, 0.097)
  d = c(0.927, 0.923, 0.901, 0.988)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b, c, d), D = D, model = '4PL',
                    item_id = paste0('i', 1:4))
  expected <- matrix(c(0.483937252333668, 0.262028867328634, 0.0481259815249311,
                       0.00165358675158043), nrow = 1,
                     dimnames = list(names(theta), ip$item_id))
  expected <- matrix(c(1 - expected, expected), nrow = ip$n$items, ncol = 2,
                      dimnames = list(ip$resp_id, 0:1))
  object <- prob(ip = ip, theta = theta, derivative = 1)
  expect_identical(object, expected, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  theta = -1.942
  a = c(1.94, 0.61, 1.264, 1.219)
  b = c(-0.841, -0.415, 0.433, 1.693)
  c = c(0.204, 0.096, 0.191, 0.348)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b, c), D = D, model = '3PL',
                    item_id = paste0('i', 1:4))
  expected <- c(0.0658024162045501, 0.132452720103545, 0.0103857763507865,
                0.000716874903902979)
  expected <- matrix(c(1 - expected, expected), nrow = ip$n$items, ncol = 2,
                      dimnames = list(ip$resp_id, 0:1))
  object <- prob(ip = ip, theta = theta, derivative = 1)
  expect_identical(object, expected, tolerance = 1e-6)
})


###############################################################################@
############################# prob_fd (REST) @##################################
###############################################################################@

test_that("prob_fd - REST", {
  # Try character, 1 theta
  expect_error(prob("1", theta = 2, derivative = 1),
               regexp = "Cannot convert object to an")

  # -------------------------------------------------------------------------- #
  theta = 0.136
  a = c(1.908, 0.844, 1.398, 1.389)
  b = c(-0.291, 0.302, 0.461, 1.1)
  c = c(0.224, 0.23, 0.216, 0.189)
  D = 1.702
  ipdf = data.frame(a, b, c, D)
  expected <- c(0.403113950366316, 0.272629986752599, 0.403043553464618,
                0.161535475006605)
  ip <- itempool(ipdf)
  expected <- matrix(c(1 - expected, expected), nrow = ip$n$items, ncol = 2,
                      dimnames = list(ip$resp_id, 0:1))
  object <- prob(ip = ipdf, theta = theta, derivative = 1)
  expect_identical(object, expected, tolerance = 1e-6)
})



#%%%%%%%%%%%%%%%%%%#############################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% prob_sd %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

###############################################################################@
############################# prob_sd (Item) @##################################
###############################################################################@

test_that("prob_sd - Item", {
  ##  Single theta
  # -------------------------------------------------------------------------- #
  theta = -1.928
  b = -0.458
  D = 1.702
  ip = item(b = b, D = D, model = '1PL',
               item_id = paste0('i', 1:1))
  expected <- 0.172038459572945
  object <- prob(ip = ip, theta = theta, derivative = 2)
  expect_identical(object[2], expected, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  theta = 0.261
  a = 1.818
  b = 1.978
  D = 1.702
  ip = item(a = a, b = b, D = D, model = '2PL',
               item_id = paste0('i', 1:1))
  expected <- 0.0462631558451308
  object <- prob(ip = ip, theta = theta, derivative = 2)
  expect_identical(object[2], expected, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  theta = -0.437
  a = 0.779
  b = 0.491
  c = 0.133
  D = 1.702
  ip = item(a = a, b = b, c = c, D = D, model = '3PL', item_id = paste0('i', 1:1))
  expected <- 0.146089105331744
  object <- prob(ip = ip, theta = theta, derivative = 2)
  expect_identical(object[2], expected, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  theta = 0.378
  a = 1.05
  b = -1.213
  c = 0.299
  d = 0.904
  D = 1.702
  ip = item(a = a, b = b, c = c, d = d, D = D, model = '4PL',
            item_id = paste0('i', 1:1))
  expected <- -0.0894192189688905
  object <- prob(ip = ip, theta = theta, derivative = 2)
  expect_identical(object[2], expected, tolerance = 1e-6)

  ##  Multiple theta

  # -------------------------------------------------------------------------- #
  theta = c(0.354, 1.911, -0.707)
  a = 1.501
  b = 0.092
  D = 1.702
  ip = item(a = a, b = b, D = D, model = '2PL', item_id = paste0('i', 1:1))
  expected <- c(-0.471703645096405, -0.0602409950855742, 0.511318159477362)
  object <- prob(ip = ip, theta = theta, derivative = 2)
  expect_identical(object[, 2], expected, tolerance = 1e-6)


  ### GRM ###
  # -------------------------------------------------------------------------- #
  # Test Second Derivative for Graded Response Model - Example 1
  itm <- new("GRM", a = 0.8999, b = c(-1.4715, -0.6293, 0.594, 1.0234),
             D = 1, se_a = NULL, se_b = NULL, item_id = NULL, content = NULL,
             misc = NULL)

  theta <- -0.609
  expect_equal(unname(prob(ip = itm, theta = theta, derivative = 2)[1, ]),
               c(0.0646182, -0.0627692, -0.0774531, -0.0014744, 0.0770785),
               tolerance = 1e-6)


  # -------------------------------------------------------------------------- #
  # Test Second Derivative for Graded Response Model - Example 2
  itm <- new("GRM", a = 1.4657,
             b = c(-1.1429, -0.697, -0.2237, 0.1274, 0.7131), D = 1,
             se_a = NULL, se_b = NULL, item_id = NULL, content = NULL, misc = NULL)
  theta <- 0.796
  expect_equal(unname(prob(ip = itm, theta = theta, derivative = 2)[1, ]),
               c(0.0995286, 0.0559414, 0.048217, -0.0100732, -0.1611453,
                 -0.0324686),
               tolerance = 1e-6)

  ### GPCM ###
  # -------------------------------------------------------------------------- #
  # Test Second Derivative for Generalized Partial Credit Model - Example 2
  itm <- new("GPCM", a = 0.6765,
             b = c(-1.3924, -0.907, -0.328, 0.0295, 0.8483, 1.338),
             D = 1, se_a = NULL, se_b = NULL, item_id = NULL, content = NULL,
             misc = NULL)
  theta <- -0.531
  expect_equal(unname(prob(ip = itm, theta = theta, derivative = 2)[1, ]),
               c(0.1693054, -0.0269083, -0.2303271, -0.1707077, 0.0410273,
                 0.1322301, 0.0853803),
               tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  # Another example
  itm <- new("GPCM", a = 1.0297, b = c(-1.1961, -0.4787, 0.7638, 1.1222), D = 1,
             se_a = NULL, se_b = NULL, item_id = NULL, content = NULL, misc = NULL)
  theta <- -1.044
  observed <- unname(prob(ip = itm, theta = theta, derivative = 2)[1, ])
  expected <- c(0.0694643, -0.3093472, 0.0809761, 0.1254165, 0.0334903)
  expect_equal(observed, expected, tolerance = 1e-6)


  ### PCM ###
  # -------------------------------------------------------------------------- #
  # Test Second Derivative for Generalized Partial Credit Model - Example 2
  itm <- new("PCM", b = c(-1.2305, -0.6226, 0.1842, 0.8405, 1.4294),
             se_b = NULL, item_id = NULL, content = NULL, misc = NULL)
  theta <- 0.682
  observed <- unname(prob(ip = itm, theta = theta, derivative = 2)[1, ])
  expected <- c(0.0723567, 0.1969303, 0.0441315, -0.3942974, -0.1762085,
                0.2570875)
  expect_equal(observed, expected, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  # Another example
  itm <- new("PCM", b = c(-1.5745, -0.8832, -0.2557, 0.2209, 0.5299, 1.1564),
             se_b = NULL, item_id = NULL, content = NULL, misc = NULL)
  theta <- -1.517
  observed <- unname(prob(ip = itm, theta = theta, derivative = 2)[1, ])
  expected <- c(0.0325981, -0.336775, 0.023979, 0.1774235, 0.0811331, 0.0194959,
                0.0021455)
  expect_equal(observed, expected, tolerance = 1e-6)

})


###############################################################################@
############################# prob_sd (Itempool) @#############################
###############################################################################@

test_that("prob_sd - Itempool", {
  # -------------------------------------------------------------------------- #
  theta = -0.062
  b = c(-1.916, -0.663, 0.129)
  D = 1.702
  ip = itempool(data.frame(b = b), D = D, model = '1PL',
                    item_id = paste0('i', 1:3))
  expected <- c(-0.104282428836464, -0.265447156448287, 0.113657243364816)
  expected <- matrix(c(1 - expected, expected), nrow = ip$n$items, ncol = 2,
                      dimnames = list(ip$resp_id, 0:1))
  object <- prob(ip = ip, theta = theta, derivative = 2)
  expect_identical(object, expected, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  theta = -0.169
  a = c(0.798, 1.45, 1.254, 1.809)
  b = c(-1.454, -0.578, -0.456, 1.518)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b), D = D, model = '2PL',
                    item_id = paste0('i', 1:4))
  expected <- c(-0.164043869491275, -0.555355095662449, -0.308429942373656,
                0.0514500578350218)
  expected <- matrix(c(1 - expected, expected), nrow = ip$n$items, ncol = 2,
                      dimnames = list(ip$resp_id, 0:1))
  object <- prob(ip = ip, theta = theta, derivative = 2)
  expect_identical(object, expected, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  theta = 1.937
  a = c(1.558, 1.596, 1.562, 1.728, 1.576)
  b = c(-1.641, -0.881, -0.862, 0.629, 0.687)
  c = c(0.298, 0.328, 0.171, 0.348, 0.131)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b, c), D = D, model = '3PL',
                    item_id = paste0('i', 1:5))
  expected <- c(-0.000373889285518031, -0.00234475259915924,
                -0.0034287908697427, -0.110580125095761, -0.190382092420069)
  expected <- matrix(c(1 - expected, expected), nrow = ip$n$items, ncol = 2,
                      dimnames = list(ip$resp_id, 0:1))
  object <- prob(ip = ip, theta = theta, derivative = 2)
  expect_identical(object, expected, tolerance = 1e-6)

  # -------------------------------------------------------------------------- #
  theta = 1.434
  a = c(0.55, 1.669, 1.974)
  b = c(-1.132, 0.872, 1.728)
  c = c(0.216, 0.195, 0.069)
  d = c(0.916, 0.957, 0.986)
  D = 1.702
  ip = itempool(data.frame(a = a, b = b, c, d), D = D, model = '4PL',
                    item_id = paste0('i', 1:3))
  expected <- c(-0.0389423617283958, -0.571146490404175, 0.93590019677188)
  expected <- matrix(c(1 - expected, expected), nrow = ip$n$items, ncol = 2,
                      dimnames = list(ip$resp_id, 0:1))
  object <- prob(ip = ip, theta = theta, derivative = 2)
  expect_identical(object, expected, tolerance = 1e-6)

})


###############################################################################@
############################# prob_sd (REST) @##################################
###############################################################################@


test_that("prob_sd - REST", {
  # Try character, 1 theta
  expect_error(prob("1", theta = 2, derivative = 2),
               regexp = "Cannot convert object to an")

  # -------------------------------------------------------------------------- #
  theta = 1.063
  a = c(0.976, 1.028, 0.675)
  b = c(-1.58, -0.001, 0.697)
  c = c(0.276, 0.233, 0.06)
  D = 1.702
  ipdf = data.frame(a=a, b=b, c=c, D=D)
  ip <- itempool(ipdf)
  expected <- c(-0.0235696200805032, -0.199814498957878, -0.0615062262812214)
  expected <- matrix(c(1 - expected, expected), nrow = ip$n$items, ncol = 2,
                      dimnames = list(ip$resp_id, 0:1))
  object <- prob(ip = ipdf, theta = theta, derivative = 2)
  expect_identical(object, expected, tolerance = 1e-6)
})

