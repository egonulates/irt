# library(testthat)


###############################################################################@
############################# generate_item ####################################
###############################################################################@
test_that("Test generate_item", {
  # Test the default values:
  item <- generate_item()
  expect_s4_class(item, "Item")
  expect_identical(item$model, "3PL")

  # -------------------------------------------------------------------------- #
  # The length of model should be 1 and it cannot be NULL.
  expect_error(generate_item(model = c("3PL", "2PL")),
               "Invalid model argument.")
  expect_error(generate_item(model = NULL), "Invalid model argument.")
  expect_error(generate_item(model = "ABC"), "Invalid model argument.")

  # -------------------------------------------------------------------------- #
  # n_categories cannot be a vector of length more than 1
  expect_error(generate_item(n_categories = 1:2),
               "Invalid number of categories")
  # n_categories cannot be NULL or NA
  expect_error(generate_item(n_categories = NULL, model = "GPCM"),
               "Invalid number of categories")
  expect_error(generate_item(n_categories = NA, model = "GPCM"),
               "Invalid number of categories")

  # -------------------------------------------------------------------------- #
  # if n_categories is a decimal it will be converted to an integer
  item <- generate_item(model = "GRM", n_categories = 4.8)
  expect_identical(item$model, "GRM")
  expect_identical(length(item@b), 3L)

  # -------------------------------------------------------------------------- #
  # Can create a GPCM2 item.
  item <- generate_item(model = "GPCM2", n_categories = 4)
  expect_s4_class(item, "Item")
  expect_identical(item$model, "GPCM2")
  expect_identical(c(class(item)), "GPCM2")


  # -------------------------------------------------------------------------- #
  # Can create a M2PL item.
  item <- generate_item(model = "M2PL")
  expect_s4_class(item, "Item")
  expect_identical(item$model, "M2PL")
  expect_identical(c(class(item)), "M2PL")
  expect_identical(length(item$a), 2L)

  # -------------------------------------------------------------------------- #
  # Can create a M3PL item.
  item <- generate_item(model = "M3PL")
  expect_s4_class(item, "Item")
  expect_identical(item$model, "M3PL")
  expect_identical(c(class(item)), "M3PL")
  expect_identical(length(item$a), 2L)
  expect_true(item$c > 0)

  # -------------------------------------------------------------------------- #
  # When se_parameters = TRUE, the function will generate se_parameters.
  item <- generate_item(model = "3PL", se = TRUE)
  expect_false(is.null(item@se_a))
  expect_true(item@se_a > 0.05)
  expect_true(item@se_a < 0.75)
  expect_true(item@se_b > 0.05)
  expect_true(item@se_b < 0.75)
  expect_true(item@se_c > 0.05)
  expect_true(item@se_c < 0.75)

  # -------------------------------------------------------------------------- #
  # When se_parameters = TRUE, the function will generate se_parameters for
  # polytomous items as well.
  n_categories <- sample(3:7, 1)
  item <- generate_item(model = "GPCM", n_categories = n_categories,
                        se = TRUE)
  expect_false(is.null(item@se_a))
  expect_true(item@se_a > 0.05)
  expect_true(item@se_a < 0.75)
  expect_identical(length(item@b), n_categories - 1L)
  expect_identical(length(item@se_b), n_categories - 1L)
  expect_true(all(item@se_b > 0.05))
  expect_true(all(item@se_b < 0.75))

  # -------------------------------------------------------------------------- #
  # 'se' is a list
  n_categories <- sample(3:7, 1)
  item <- generate_item(model = "GPCM", n_categories = n_categories,
                        se = list(se_a = .444,
                                  se_b = runif(n_categories - 1, .05, .75)))
  expect_false(is.null(item@se_a))
  expect_true(item@se_a > 0.05)
  expect_true(item@se_a < 0.75)
  expect_identical(length(item@b),  n_categories - 1L)
  expect_identical(length(item@se_b), n_categories - 1L)
  expect_true(all(item@se_b > 0.05))
  expect_true(all(item@se_b < 0.75))


  # -------------------------------------------------------------------------- #
  # Set some item parameters to fixed values
  D <- round(runif(1, 1, 2), 4)
  itm <- generate_item(model = "3PL", D = D)
  expect_identical(itm$D, D)

  a <- round(runif(1, 1, 2), 4)
  D <- round(runif(1, 1, 2), 4)
  b <- round(rnorm(1), 4)
  itm <- generate_item(model = "3PL", a = a, D = D, b = b)
  expect_identical(itm$a, a)
  expect_identical(itm$b, b)
  expect_identical(itm$D, D)

  b <- round(runif(5, 1, 2), 4)
  itm <- generate_item(model = "GPCM", b = b)
  expect_identical(itm$b, b)

  # -------------------------------------------------------------------------- #
  # 'misc' field can be added
  item <- generate_item(model = "3PL",
                        misc = list(key = "A", avg_resp_time = 22))
  expect_identical(item$misc$key, "A")
  expect_identical(item$misc$avg_resp_time, 22)

  # -------------------------------------------------------------------------- #
  # 'misc' should be a list
  expect_error(generate_item(misc = 12))

  # -------------------------------------------------------------------------- #
  # For dichotomous items, function automatically adds a key and
  # possible_options.
  for (model in irt:::DICHOTOMOUS_MODELS) {
    item <- generate_item(model = model)
    expect_false(is.null(item@misc$key))
    expect_true(item@misc$key %in% c("A", "B", "C", "D"))
    expect_false(is.null(item@misc$possible_options))
    expect_identical(item@misc$possible_options, c("A", "B", "C", "D"))
  }

  # -------------------------------------------------------------------------- #
  # For dichotomous items, if there is already a 'misc$key' field, it should
  # be intact.
  item <- generate_item(model = "2PL",
                        misc = list(key = "X", avg_resp_time = 22))
  expect_identical(item$misc$key, "X")

})



###############################################################################@
############################# generate_ip ######################################
###############################################################################@
test_that("Test generate_ip", {
  # model = "3PL"
  # n = NULL
  # output = "Itempool"
  # n_categories = 4
  # se_parameters = NULL

  ip <- generate_ip()
  expect_s4_class(ip, "Itempool")
  # Designate the number of items
  ip <- generate_ip(n = 12)
  expect_s4_class(ip, "Itempool")
  expect_identical(length(ip), 12L)
  # Generate item pools for other models
  ip <- generate_ip("Rasch")
  expect_true(all(ip$model == "Rasch"))
  ip <- generate_ip("1PL")
  expect_true(all(ip$model == "1PL"))
  ip <- generate_ip("2PL")
  expect_true(all(ip$model == "2PL"))
  ip <- generate_ip("4PL")
  expect_true(all(ip$model == "4PL"))
  ip <- generate_ip("GPCM")
  expect_true(all(ip$model == "GPCM"))
  ip <- generate_ip("PCM")
  expect_true(all(ip$model == "PCM"))
  ip <- generate_ip("GRM")
  expect_true(all(ip$model == "GRM"))
  ip <- generate_ip("GPCM2")
  expect_true(all(ip$model == "GPCM2"))

  # -------------------------------------------------------------------------- #
  # Function can add se
  ip <- generate_ip(n = 2, model = "2PL",
                    se = list(list(a = .2, b = .3), list(a = .4, b = .5)))
  expect_identical(ip[[1]]@se_a, 0.2)
  expect_identical(ip[[1]]@se_b, 0.3)
  expect_identical(ip[[2]]@se_a, 0.4)
  expect_identical(ip[[2]]@se_b, 0.5)

  # -------------------------------------------------------------------------- #
  # n_categories should be a vector of length 1 or length n, otherwise an error
  # will be raised
  expect_error(generate_ip(n = 4, n_categories = c(5, 6)),
               "Invalid n_categories value.")

  # -------------------------------------------------------------------------- #
  # When n_categories is a vector, the polytomous items will have that number
  # of categories.
  ip <- generate_ip(model = c("3PL", "GRM", "GPCM", "GRM", "2PL"),
                    n_categories = c(2, 3, 6, 7, 2))

  expect_identical(ip[[2]]$model, "GRM")
  expect_identical(ip[[3]]$model, "GPCM")
  expect_identical(ip[[4]]$model, "GRM")
  expect_identical(length(ip[[2]]@b), 2L)
  expect_identical(length(ip[[3]]@b), 5L)
  expect_identical(length(ip[[4]]@b), 6L)
  expect_identical(length(ip[[2]]$parameters$b), 2L)
  expect_identical(length(ip[[3]]$parameters$b), 5L)
  expect_identical(length(ip[[4]]$parameters$b), 6L)

  # -------------------------------------------------------------------------- #
  # When n_categories is a vector of length 1 all of the polytomous items will
  # have the same number of categories
  ip <- generate_ip(model = c("3PL", "GRM", "GPCM", "GRM", "2PL"),
                    n_categories = 7)
  expect_identical(length(ip[[2]]@b), 6L)
  expect_identical(length(ip[[3]]@b), 6L)
  expect_identical(length(ip[[4]]@b), 6L)

  # -------------------------------------------------------------------------- #
  # When se_parameters = TRUE, the function will generate se_parameters.
  ip <- generate_ip(model = c("3PL", "GRM", "GPCM", "GRM", "2PL"),
                    n_categories = c(2, 3, 6, 7, 2),
                    se = TRUE)
  expect_true(ip[[1]]@se_a > 0.05)
  expect_true(ip[[1]]@se_c < 0.75)
  expect_identical(length(ip[[4]]@se_b), 6L)

  # -------------------------------------------------------------------------- #
  # n_categories can specify the item pool size.
  n_items <- sample(30:40, 1)
  ip <- generate_ip(model = "GRM", n_categories = sample(3:6, n_items, T))
  expect_s4_class(ip, "Itempool")
  expect_identical(length(ip), n_items)
})


###############################################################################@
############################# generate_testlet #################################
###############################################################################@
test_that("Test generate_ip", {
  testlet <- generate_testlet()
  expect_s4_class(testlet, "Testlet")
  expect_identical(testlet$model, "BTM")
  # Designate the number of items
  testlet <- generate_testlet(n = 12)
  expect_s4_class(testlet, "Testlet")
  expect_identical(length(testlet), 12L)
  # Generate item pools for other models
  testlet <- generate_testlet(item_models = "Rasch")
  expect_true(all(testlet$item_models == "Rasch"))

  # -------------------------------------------------------------------------- #
  # When n_categories is a vector, the polytomous items will have that number
  # of categories.
  testlet <- generate_testlet(
    item_models = c("3PL", "GRM", "GPCM", "GRM", "2PL"),
    n_categories = c(2, 3, 6, 7, 2))

  expect_identical(testlet[[2]]$model, "GRM")
  expect_identical(testlet[[3]]$model, "GPCM")
  expect_identical(testlet[[4]]$model, "GRM")
  expect_identical(length(testlet[[2]]$parameters$b), 2L)
  expect_identical(length(testlet[[3]]$parameters$b), 5L)
  expect_identical(length(testlet[[4]]$parameters$b), 6L)

  # -------------------------------------------------------------------------- #
  # Item IDs will add the suffix:
  t1 <- generate_testlet(n = 3, item_id_preamble = "t1-")
  expect_identical(t1@item_list[[1]]@item_id, "t1-Item_1")
  expect_identical(t1@item_list[[2]]@item_id, "t1-Item_2")

})


###############################################################################@
############################# generate_resp ####################################
###############################################################################@
test_that("Test generate_ip", {
  n_item <- sample(10:20, 1)
  ip <- generate_ip(model = "3PL", n = n_item)
  resp_list <- generate_resp(ip, theta = rnorm(1))
  expect_true(is.list(resp_list))
  expect_identical(length(resp_list), 1L)
  resp <- resp_list[[1]]
  expect_s4_class(resp, "Response")
  expect_identical(length(resp), n_item)
  expect_true(all(resp@raw_response %in% c("A", "B", "C", "D")))
  expect_true(all(resp@score %in% 0:1))
  expect_true(all(resp@item_id %in% ip$id))
  expect_identical(resp@order, 1:n_item)


  # -------------------------------------------------------------------------- #
  # A list of Response objects
  n_item <- sample(10:20, 1)
  n_theta <- sample(10:20, 1)
  ip <- generate_ip(model = "3PL", n = n_item)
  resp_list <- generate_resp(ip, theta = rnorm(n_theta))
  expect_identical(length(resp_list), n_theta)
  expect_true(all(sapply(resp_list, class) == "Response"))

  # -------------------------------------------------------------------------- #
  # Set the proportion of missing responses:
  n_item <- sample(100:200, 1)
  n_theta <- 1
  ip <- generate_ip(model = "3PL", n = n_item)
  resp_list <- generate_resp(ip, theta = rnorm(5), prop_missing = 0.5)
  resp <- resp_list[[1]]
  expect_true(length(resp) < n_item)

  # -------------------------------------------------------------------------- #
  # Test 'Item' object
  i1 <- generate_item(model = "3PL")
  n_theta <- 1
  resp_list <- generate_resp(i1, theta = rnorm(n_theta))
  resp <- resp_list[[1]]
  expect_s4_class(resp, "Response")
  expect_identical(length(resp), 1L)

  # -------------------------------------------------------------------------- #
  # Test 'Testlet' object
  n_item <- sample(5:15, 1)
  n_theta <- 1
  t1 <- generate_testlet(model = "3PL", n = n_item)
  resp_list <- generate_resp(t1, theta = rnorm(n_theta))
  resp <- resp_list[[1]]
  expect_s4_class(resp, "Response")
  expect_identical(length(resp), n_item)


  # -------------------------------------------------------------------------- #
  # If theta vector has names, they will be assigned as examinee ids
  examinee_ids <- c("Alex", "Maddie", "Dylan")
  theta <- setNames(rnorm(3), examinee_ids)
  ip <- generate_ip(n = 5)
  resp_list <- generate_resp(ip, theta = theta)
  i <- sample(seq_along(examinee_ids), 1)
  expect_identical(resp_list[[i]]$examinee_id, examinee_ids[i])

})


###############################################################################@
############################# generate_resp_set ################################
###############################################################################@
test_that("Test generate_ip", {
  n_item <- sample(10:20, 1)
  n_theta <- sample(10:20, 1)
  ip <- generate_ip(model = "3PL", n = n_item)
  resp_set <- generate_resp_set(ip, theta = rnorm(n_theta))
  expect_s4_class(resp_set, "Response_set")
  expect_identical(length(resp_set), n_theta)

  # -------------------------------------------------------------------------- #
  # Test 'Item' object
  i1 <- generate_item(model = "3PL")
  n_theta <- 1
  resp_set <- generate_resp_set(i1, theta = rnorm(n_theta))
  expect_s4_class(resp_set, "Response_set")
  expect_identical(length(resp_set), 1L)

  # -------------------------------------------------------------------------- #
  # Test 'Testlet' object
  n_item <- sample(5:15, 1)
  n_theta <- sample(10:20, 1)
  t1 <- generate_testlet(model = "3PL", n = n_item)
  resp_set <- generate_resp_set(t1, theta = rnorm(n_theta))
  expect_s4_class(resp_set, "Response_set")
  expect_identical(length(resp_set), n_theta)


  # -------------------------------------------------------------------------- #
  # Check whether raw responses makes sense
  n_item <- sample(2:3, 1)
  n_theta <- sample(10000:20000, 1)
  theta <- rnorm(1)
  ip <- generate_ip(model = "3PL", n = n_item)
  resp_set <- generate_resp_set(ip, theta = rep(theta, n_theta))
  raw_resp <- as.matrix(resp_set, output = "raw_response", ip = ip)
  # Check item's raw responses, compare the proportion of key to the probability
  # of correct response
  for (i in sequence(n_item)) {
    key <- ip$key[i]
    expected <- prob(ip[[i]], theta = theta)[, 2]
    observed <- mean(raw_resp[, i] == key)
    expect_true(abs(observed - expected) < 0.05)
    ## Distractors are expected to be equally spaced
    prop_distractor <- prop.table(table(raw_resp[, i]))
    # remove correct response
    prop_distractor <- prop_distractor[names(prop_distractor) != key]
    expect_true(all(abs(prop_distractor[1] - prop_distractor[-1]) < 0.05))
  }

  # -------------------------------------------------------------------------- #
  # If theta vector has names, they will be assigned as examinee ids
  examinee_ids <- c("Alex", "Maddie", "Dylan")
  theta <- setNames(rnorm(3), examinee_ids)
  ip <- generate_ip(n = 5)

  resp_set <- generate_resp_set(ip, theta = theta)
  expect_identical(resp_set$examinee_id, examinee_ids)

  # -------------------------------------------------------------------------- #
  # Test when some examinee_ids are missing
  examinee_ids <- c(123, 66233)
  theta <- setNames(rnorm(3), examinee_ids)
  ip <- generate_ip(n = 5)

  resp_set <- generate_resp_set(ip, theta = theta)
  expect_identical(resp_set$examinee_id, c(as.character(examinee_ids), NA))

})


