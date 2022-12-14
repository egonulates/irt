

############################################################################@###
################### print.cat_design ###########################################
############################################################################@###

test_that("print.cat_design", {
  # The following function tests cat_design print to the console.
  # cd: cat_design object.
  test_print_output <- function(cd) {
    p <- capture_output(print(cd))
    expect_output(print(p), sprintf("Item Pool Size: %d", length(cd$ip)))
    expect_output(print(p), sprintf("Maximum Test Length: %d", cd$max_test_length))
    expect_output(print(p), sprintf("First Item Rule: '%s'", cd$first_item_rule))
    expect_output(print(p), sprintf("First Item Parameters:"))
    for (i in seq_len(length(cd$first_item_par)))
      expect_output(print(p), paste0(names(cd$first_item_par)[i], ": ",
                                     cd$first_item_par[[i]]))
    expect_output(print(p), sprintf("Next Item Rule: '%s'",
                                    cd$step[[1]]$next_item_rule))
    expect_output(print(p), sprintf("Ability Estimation Rule: '%s'",
                                    cd$step[[1]]$ability_est_rule))
    expect_output(print(p), "Test Termination Rules and Parameters")
    for (i in seq_len(length(cd$termination_rule)))
      expect_output(print(p), paste0(
        names(cd$termination_par)[i], ": ",
        cd$termination_par[[names(cd$termination_par)[i]]] ))
  }
  n_items = 30
  ip <- itempool(data.frame(a = runif(n_items, .5, 1.5), b = rnorm(n_items)))
  cd <- create_cat_design(ip = ip, next_item_rule = "random",
                          termination_rule = "min_item",
                          termination_par = list(min_item = n_items))
  test_print_output(cd)
  cd <- create_cat_design(ip = ip, next_item_rule = "random")
  test_print_output(cd)

  cd <- create_cat_design(
    ip = ip,
    termination_rule = c("min_item", "min_se"),
    termination_par = list(min_item = 10, min_se = .33))
  test_print_output(cd)

  cd <- create_cat_design(
    ip = itempool(b = rnorm(20)),
    termination_rule = c("min_item", "max_item"),
    termination_par = list(min_item = 20, max_item = 20),
    next_item_rule = "fixed",
    next_item_par = lapply(paste0("Item_", 1:20), function(x) list(item_id = x)))
  test_print_output(cd)

  }) # End of test_that

###############################################################################@
############################# c.cat_design #####################################
###############################################################################@
test_that("c.cat_design", {
  ip <- generate_ip(n = 20)
  cd1 <- create_cat_design(ip = ip,
                           termination_rule = c('max_item'),
                           termination_par = list(max_item = 5))
  cd2 <- create_cat_design(ip = ip,
                           termination_rule = c('max_item'),
                           termination_par = list(max_item = 9))

  cd <- c(cd1, cd2)
  expect_true(all(sapply(cd, inherits, "cat_design")))

  # -------------------------------------------------------------------------- #
  # All elements should be a cat_design object
  expect_error(c(cd1, cd2, list(NULL)),
               regexp = "All of the elements should be 'cat_design' class.")

  })
############################################################################@###
################### summary.cat_output #########################################
############################################################################@###
test_that("summary.cat_output", {
  n <- 3 # number of items
  ip <- itempool(data.frame(b = rnorm(n)))

  cd = create_cat_design(ip = ip, next_item_rule = "mfi",
                       termination_rule = "max_item",
                       termination_par = list(max_item = n))
  # --------------------------------- #
  # CAT summary works with single CAT output.
  theta1 <- rnorm(1)
  t1 <- cat_sim(true_ability = theta1, cd = cd)
  co <- summary(t1)
  expect_true(is.data.frame(co))
  expect_equal(co$true_ability, theta1)

  # --------------------------------- #
  # CAT summary works with multiple CAT output.
  theta2 <- rnorm(1)
  t2 <- cat_sim(true_ability = theta2, cd = cd)
  co <- summary(list(t1, t2))
  expect_true(is.data.frame(co))
  expect_equal(co$true_ability, c(theta1, theta2))

  # --------------------------------- #
  # Error raised when inadmissible column name entered.
  expect_error(summary(t1, cols = c("true_theta", "somexyx")))

  # --------------------------------- #
  # QIP works
  qip <- qip_index(t1, summary_func = NULL)
  qip_mean <- qip_index(t1, summary_func = "mean")
  qip_median <- qip_index(t1, summary_func = "median")
  qip_min <- qip_index(t1, summary_func = "min")
  qip_max <- qip_index(t1, summary_func = "max")
  co <- summary(t1, cols = c("mean_qip", "median_qip", "min_qip", "max_qip"))
  expect_equal(co$mean_qip, qip_mean)
  expect_equal(co$median_qip, qip_median)
  expect_equal(co$min_qip, qip_min)
  expect_equal(co$max_qip, qip_max)

  # -------------------------------------------------------------------------- #
  # Test multiple theta
  nsim <- 5L
  examinee_ids <- c("Dave", "Ali", "Joao", "Mirabel", "Giovanni")
  true_theta <- setNames(rnorm(nsim), examinee_ids)
  t1 <- cat_sim(true_ability = true_theta, cd = cd)
  e <- summary(t1)
  expect_true(is.data.frame(e))
  expect_identical(nrow(e), nsim)
  expect_identical(colnames(e),
               c("examinee_id", "true_ability", "est_ability", "se",
                 "test_length"))
  expect_identical(e$true_ability, unname(true_theta))
  expect_true(all(e$test_length == n))
  expect_identical(e$examinee_id, examinee_ids)

  desired_cols <- c("true_ability", "est_ability", "se", "test_length", "bias")
  e <- summary(t1, cols = desired_cols)
  expect_identical(colnames(e), desired_cols)
  expect_identical(e$true_ability, unname(true_theta))
  expect_true(all(e$test_length == n))
  expect_identical(e$bias, e$est_ability - e$true_ability)

  # --------------------------------- #
  # QIP works
  qip <- qip_index(t1, summary_func = NULL)
  qip_mean <- qip_index(t1, summary_func = "mean")
  qip_median <- qip_index(t1, summary_func = "median")
  qip_min <- qip_index(t1, summary_func = "min")
  qip_max <- qip_index(t1, summary_func = "max")
  co <- summary(t1, cols = c("mean_qip", "median_qip", "min_qip", "max_qip"))
  expect_equal(co$mean_qip, unname(qip_mean))
  expect_equal(co$median_qip, unname(qip_median))
  expect_equal(co$min_qip, unname(qip_min))
  expect_equal(co$max_qip, unname(qip_max))

})



############################################################################@###
################### as.data.frame.cat_output ###################################
############################################################################@###

test_that("as.data.frame.cat_output", {
  n <- 30 # number of items
  ip <- itempool(data.frame(b = rnorm(n)))

  cd <- create_cat_design(ip = ip, next_item_rule = "mfi",
                          termination_rule = "max_item",
                          termination_par = list(max_item = 7))
  # CAT summary works with single CAT output.
  co <- cat_sim(true_ability = rnorm(1), cd = cd)
  co_df <- as.data.frame(co)
  i <- sample(1:7, 1)
  expect_identical(co$true_ability[[1]], co_df$true_ability[i])
  expect_identical(co$est_history[[i]]$est_before, co_df$est_before[i])
  expect_identical(co$est_history[[i]]$se_before, co_df$se_before[i])
  expect_identical(co$est_history[[i]]$est_after, co_df$est_after[i])
  expect_identical(co$est_history[[i]]$se_after, co_df$se_after[i])
  expect_identical(co$est_history[[i]]$resp, co_df$resp[i])
  expect_identical(co$est_history[[i]]$item@item_id, co_df$item_id[i])
  expect_identical(co$examinee_id, co_df$examinee_id[i])
})


############################################################################@###
################### print.cat_output ###########################################
############################################################################@###

test_that("print.cat_output", {
  n <- 3 # number of items
  ip <- itempool(data.frame(b = rnorm(n)))

  cd <- create_cat_design(ip = ip, next_item_rule = "mfi",
                          termination_rule = "max_item",
                          termination_par = list(max_item = n))
  # CAT summary works with single CAT output.
  co <- cat_sim(true_ability = rnorm(1), cd = cd)
  expect_output(expected <- print(co))
  expect_true(is.data.frame(expected))
  expect_type(expected$item_id, "character")
  # When silent, it should not produce output
  expect_silent(print(co, silent = TRUE))


  # -------------------------------------------------------------------------- #
  # Run the following for various ways to print cat_output
  # n <- 30 # number of items
  # ip <- itempool(data.frame(b = rnorm(n)))
  #
  # cd <- create_cat_design(ip = ip, next_item_rule = "mfi",
  #                         termination_rule = "max_item",
  #                         termination_par = list(max_item = n))
  # # CAT summary works with single CAT output.
  # co <- cat_sim(true_ability = rnorm(1), cd = cd)
  # print(co)
  # print(co, n = 25)
  # print(co, n = 1)
  # print(co, n = -1)
  # print(co, n = 99)
  # print(co, silent = TRUE)
  # print(co, print_header = FALSE)
  # print(co, base_print = FALSE, print_header = FALSE)
  # print(co, base_print = TRUE, print_header = FALSE)
  # print(co, base_print = TRUE, print_header = TRUE)

})

###############################################################################@
############################# $.cat_output #####################################
###############################################################################@

test_that("$.cat_output", {
  n <- 50 # number of items
  ip <- itempool(data.frame(b = rnorm(n)))
  true_theta <- rnorm(1)
  cd <- create_cat_design(ip = ip, next_item_rule = "mfi",
                          termination_rule = "max_item",
                          termination_par = list(max_item = 10))
  # CAT summary works with single CAT output.
  co <- cat_sim(true_ability = true_theta, cd = cd)
  i <- sample(1:10, 1)
  expect_equal(co$resp[i], co$est_history[[i]]$resp)
  expect_equal(co$ip[[i]], co$est_history[[i]]$item)
  expect_equal(co$item_id[i], co$est_history[[i]]$item@item_id)
  expect_equal(co$est_before[i], co$est_history[[i]]$est_before)
  expect_equal(co$est_after[i], co$est_history[[i]]$est_after)
  expect_equal(co$se_before[i], co$est_history[[i]]$se_before)
  expect_equal(co$se_after[i], co$est_history[[i]]$se_after)
  expect_equal(co$true_theta, true_theta)
  expect_equal(co$test_length, 10)
  expect_equal(co$final_se, co$est_history[[10]]$se_after)
  expect_equal(co$final_est, co$est_history[[10]]$est_after)
  # expect_equal(co$true_ability, true_thetest_beforea)
})

############################################################################@###
################### get_cat_response_data_single ###############################
############################################################################@###
test_that("get_cat_response_data_single", {
  ip <- generate_ip(n = 40)
  cd <- create_cat_design(ip = ip, next_item_rule = 'mfi',
                          termination_rule = 'max_item',
                          termination_par = list(max_item = 10))
  temp_name <- paste0(sample(letters, 3), collapse = "")
  true_theta <- setNames(rnorm(1), temp_name)
  cat_data <- cat_sim(true_ability = true_theta, cd = cd)
  observed <- get_cat_response_data_single(cat_data)
  i <- sample(1:10, 1)
  expect_s4_class(observed, "Response")
  expect_equal(observed@score[i], cat_data$est_history[[i]]$resp)
  expect_equal(observed@item_id[i], cat_data$est_history[[i]]$item$item_id)
  expect_equal(observed@examinee_id, temp_name)
  expect_equal(observed@misc$true_ability, unname(true_theta))
  expect_equal(observed@misc$est_ability,
               tail(cat_data$est_history, 1)[[1]]$est_after)
  expect_equal(observed@misc$se, tail(cat_data$est_history, 1)[[1]]$se_after)
  expect_equal(observed@misc$test_length, length(cat_data$est_history))
  expect_null(observed@testlet_id)

  observed <- get_cat_response_data_single(cat_data)

  # -------------------------------------------------------------------------- #
  # CAT with testlets
  t1 <- testlet(itempool(b = c(-1, 1), item_id = c("t1-i1", "t1-i2"), D = 1.7),
                testlet_id = "t1")
  t2 <- testlet(itempool(b = c(-2, 0, 2), item_id = c("t2-i1", "t2-i2", "t2-i3"),
                          D = 1.7), testlet_id = "t2")
  i1 <- item(b = -1.5, item_id = "i1", D = 1.7)
  i2 <- item(b = 0.25, item_id = "i2", D = 1.7)
  i3 <- item(b = 1.5, item_id = "i3", D = 1.7)
  ip <- c(t1, t2, i1, i2, i3)

  cd <- create_cat_design(
    ip = ip,
    next_item_rule = "mfi",
    testlet_rules = list(next_item_rule = "mfi",
                         termination_rule = "max_item",
                         termination_par = list(max_item = 2)),
    termination_rule = 'max_item',
    termination_par = list('max_item' = 4))

  temp_name <- paste0(sample(letters, 3), collapse = "")
  true_theta <- setNames(rnorm(1), temp_name)
  cat_data <- cat_sim(true_ability = true_theta, cd = cd)

  observed <- get_cat_response_data_single(cat_data)
  i <- sample(1:4, 1)
  expect_s4_class(observed, "Response")
  expect_equal(observed@score[i], cat_data$est_history[[i]]$resp)
  expect_equal(observed@item_id[i], cat_data$est_history[[i]]$item$item_id)
  expect_equal(observed@examinee_id, temp_name)
  expect_equal(observed@misc$true_ability, unname(true_theta))
  expect_equal(observed@misc$est_ability,
               tail(cat_data$est_history, 1)[[1]]$est_after)
  expect_equal(observed@misc$se, tail(cat_data$est_history, 1)[[1]]$se_after)
  expect_equal(observed@misc$test_length, length(cat_data$est_history))
  # Check testlet_id:
  expect_equal(observed@testlet_id[i],
               cat_data$est_history[[i]]$testlet$testlet_id)

  # ---------------------------------- #
  cd <- create_cat_design(
    ip = ip,
    next_item_rule = "mfi",
    testlet_rules = list(next_item_rule = "mfi",
                         termination_rule = "max_item",
                         termination_par = list(max_item = 2)),
    termination_rule = 'max_item',
    termination_par = list('max_item' = 6))

  temp_name <- paste0(sample(letters, 3), collapse = "")
  true_theta <- setNames(rnorm(1), temp_name)
  cat_data <- cat_sim(true_ability = true_theta, cd = cd)

  observed <- get_cat_response_data_single(cat_data)
  i <- sample(1:6, 1)
  expect_s4_class(observed, "Response")
  expect_equal(observed@score[i], cat_data$est_history[[i]]$resp)
  expect_equal(observed@item_id[i], cat_data$est_history[[i]]$item$item_id)
  expect_equal(observed@examinee_id, temp_name)
  expect_equal(observed@misc$true_ability, unname(true_theta))
  expect_equal(observed@misc$est_ability,
               tail(cat_data$est_history, 1)[[1]]$est_after)
  expect_equal(observed@misc$se, tail(cat_data$est_history, 1)[[1]]$se_after)
  expect_equal(observed@misc$test_length, length(cat_data$est_history))
  # Check testlet_id:
  expect_equal(observed@testlet_id[i],
               ifelse(is.null(cat_data$est_history[[i]]$testlet),
                      as.character(NA),
                      cat_data$est_history[[i]]$testlet$testlet_id))

})

############################################################################@###
################### get_cat_response_data ######################################
############################################################################@###
test_that("get_cat_response_data", {
  # Function works with single CAT output.
  n <- 10 # number of items
  ip <- itempool(data.frame(b = rnorm(n)), item_id = paste0("i-", 1:n))

  cd <- create_cat_design(ip = ip, next_item_rule = "random",
                         termination_rule = "max_item",
                         termination_par = list(max_item = n))
  co <- cat_sim(true_ability = rnorm(1), cd = cd)
  observed <- get_cat_response_data(cat_sim_output = co, cd = cd)
  expect_s4_class(observed, "Response_set")
  i <- sample(n, 1)
  expect_identical(co$est_history[[i]]$resp, observed[[1]]@score[i])
  expect_identical(co$est_history[[i]]$item$item_id, observed[[1]]@item_id[i])

  # ---------------------------------- #
  # Function works when "cd" not provided
  resp <- get_cat_response_data(cat_sim_output = co)
  expect_s4_class(observed, "Response_set")
  i <- sample(n, 1)
  expect_identical(co$est_history[[i]]$resp, observed[[1]]@score[i])
  expect_identical(co$est_history[[i]]$item$item_id, observed[[1]]@item_id[i])

  # ---------------------------------- #
  # Attaching CAT summary
  resp <- get_cat_response_data(cat_sim_output = co, output = "matrix",
                                attach_summary = TRUE)
  expect_true(is.data.frame(resp))
  expect_identical(nrow(resp), 1L)
  expect_true(all(c("true_ability", "est_ability", "se", "test_length") %in%
                    colnames(resp)))
  expect_true(all(ip$item_id %in% colnames(resp)))

  # -------------------------------------------------------------------------- #
  n <- 11 # number of items
  ip <- itempool(data.frame(b = rnorm(n)), item_id = paste0("i-", 1:n))

  cd <- create_cat_design(ip = ip, next_item_rule = "random",
                          termination_rule = "max_item",
                          termination_par = list(max_item = 3))
  # Function works with multiple CAT output.
  temp_name <- sapply(1:2, function(x) paste0(sample(letters, 3), collapse = ""))
  true_theta <- setNames(rnorm(2), temp_name)
  co <- cat_sim(true_ability = true_theta , cd = cd)
  t1 <- co[[1]]
  t2 <- co[[2]]
  observed <- get_cat_response_data(cat_sim_output = co, cd = cd)
  i <- sample(3, 1)
  j <- sample(2, 1)
  expect_identical(co[[j]]$est_history[[i]]$resp, observed[[j]]@score[i])
  expect_identical(co[[j]]$est_history[[i]]$item$item_id, observed[[j]]@item_id[i])
  cs <- summary(co)
  expect_identical(cs$examinee_id[j], observed[[j]]@examinee_id)
  expect_identical(temp_name[j], observed[[j]]@examinee_id)
  expect_identical(unname(true_theta[j]), observed[[j]]@misc$true_ability)
  expect_identical(cs$est_ability[j], observed[[j]]@misc$est_ability)
  expect_identical(cs$se[j], observed[[j]]@misc$se)
  expect_identical(cs$test_length[j], observed[[j]]@misc$test_length)

  # ---------------------------------- #
  # matrix
  observed <- get_cat_response_data(cat_sim_output = co, cd = cd,
                                    output_type = "matrix")
  expect_true(is.matrix(observed))
  i <- sample(3, 1)
  j <- sample(2, 1)
  expect_identical(co[[j]]$est_history[[i]]$resp,
                   observed[j, co[[j]]$est_history[[i]]$item@item_id])

  # ---------------------------------- #
  # matrix, remove_na = TRUE
  observed <- get_cat_response_data(cat_sim_output = co, cd = cd,
                                    output_type = "matrix", remove_na = TRUE)
  expect_true(is.matrix(observed))
  i <- sample(3, 1)
  j <- sample(2, 1)
  expect_identical(co[[j]]$est_history[[i]]$resp,
                   observed[j, co[[j]]$est_history[[i]]$item@item_id])

  # ---------------------------------- #
  # When using `get_cat_response_data( , output = "matrix")`, the row names of
  # the matrix should be the same as examinee_id's of the students.
  n <- 40
  ip <- generate_ip(n = n)
  cd <- create_cat_design(ip = ip, next_item_rule = 'mfi',
                          termination_rule = 'max_item',
                          termination_par = list(max_item = 10))
  n_examinee <- sample(10:20, 1)
  examinee_ids <- paste0("Stu-", sample(1:n_examinee))
  true_theta <- setNames(rnorm(n_examinee), examinee_ids)
  cat_data <- cat_sim(true_ability = true_theta, cd = cd)
  output <- get_cat_response_data(cat_sim_output = cat_data, cd = cd,
                                  output_type = "matrix")
  expect_identical(rownames(output), examinee_ids)

  # ---------------------------------- #
  # Function only accepts certain output types:
  expect_error(get_cat_response_data(cat_sim_output = co, output_type = "xyz"))

  # ---------------------------------- #
  # Function works when "cd" not provided
  observed <- get_cat_response_data(cat_sim_output = co)
  expect_s4_class(observed, "Response_set")
  i <- sample(3, 1)
  j <- sample(2, 1)
  expect_identical(co[[j]]$est_history[[i]]$resp, observed[[j]]@score[i])
  expect_identical(co[[j]]$est_history[[i]]$item$item_id, observed[[j]]@item_id[i])

  # -------------------------------------------------------------------------- #

})

############################################################################@###
################### get_cat_administered_items #################################
############################################################################@###
test_that("get_cat_administered_items", {
  # Function works with single CAT output.
  n <- 10 # number of items
  ip <- itempool(data.frame(b = rnorm(n)), item_id = paste0("i-", 1:n))

  cd <- create_cat_design(ip = ip, next_item_rule = "random",
                         termination_rule = "max_item",
                         termination_par = list(max_item = n))
  co <- cat_sim(true_ability = rnorm(1), cd = cd)
  expect_s4_class(get_cat_administered_items(co), "Itempool")

  # -------------------------------------------------------------------------- #
  # Function works with multiple CAT outputs.
  ip <- itempool(data.frame(b = rnorm(n)), item_id = paste0("i-", 1:10))

  cd <- create_cat_design(ip = ip, next_item_rule = "random",
                          termination_rule = "max_item",
                          termination_par = list(max_item = 3))
  n_theta <- sample(2:6, 1)
  # Function works with multiple CAT output.
  co <- cat_sim(true_ability = rnorm(n_theta), cd = cd)
  administered_ip <- get_cat_administered_items(co)
  expect_identical(length(administered_ip), n_theta)
  expect_true(all(sapply(administered_ip, is, "Itempool")))
})

############################################################################@###
################### calculate_exposure_rates ###################################
############################################################################@###
test_that("calculate_exposure_rates", {
  t1 <- testlet(itempool(b = -4:-2, item_id = c("t1-i1", "t1-i2", "t1-i3")),
                   testlet_id = "t1")
  t2 <- testlet(itempool(b = 2:3, item_id = c("t2-i1", "t2-i2")),
                   testlet_id = "t2")
  t3 <- testlet(itempool(b = 0:1, item_id = c("t3-i1", "t3-i2")),
                   testlet_id = "t3")
  ip <- c(t1, t2, t3, itempool(b = rnorm(18)))
  cd <- create_cat_design(ip = ip,
                          next_item_rule = "mepv",
                          termination_rule = "max_item",
                          termination_par = list("max_item" = 10))
  co <- cat_sim(true_ability = rnorm(3), cd = cd)
  observed <- calculate_exposure_rates(co, cd)
  expect_identical(length(observed), length(ip))
  expect_true(all(sapply(observed, function(x) x >= 0 & x <= 1)))

  # Run the same function with item_ids
  observed <- calculate_exposure_rates(co, item_ids = cd$ip$id)
  expect_identical(length(observed), length(ip))
  expect_true(all(sapply(observed, function(x) x >= 0 & x <= 1)))

  # cd <- create_cat_design(
  #   ip = ip,
  #   next_item_rule = "fixed",
  #   next_item_par = lapply(c("t3", "Item_1", "Item_3", "Item_11"),
  #                          function(x) list(item_id = x)),
  #   termination_rule = "max_item", termination_par = list("max_item" = 5))


})


############################################################################@###
################### calculate_overlap_rates ####################################
############################################################################@###
test_that("calculate_overlap_rates", {
  t1 <- testlet(itempool(b = -4:-2, item_id = c("t1-i1", "t1-i2", "t1-i3")),
                testlet_id = "t1")
  t2 <- testlet(itempool(b = 2:3, item_id = c("t2-i1", "t2-i2")),
                testlet_id = "t2")
  ip <- c(t1, t2, itempool(b = rnorm(18)))
  cd <- create_cat_design(ip = ip,
                          next_item_rule = "mepv",
                          termination_rule = "max_item",
                          termination_par = list(max_item = 10))
  co <- cat_sim(true_ability = rnorm(5), cd = cd)
  observed <- calculate_overlap_rates(co, cd)
  expect_identical(length(observed), length(ip))
  expect_true(all(sapply(observed, function(x) x >= 0 & x <= 1)))

  # Run the same function with item_ids
  observed <- calculate_overlap_rates(co, item_ids = cd$ip$id)
  expect_identical(length(observed), length(ip))
  expect_true(all(sapply(observed, function(x) x >= 0 & x <= 1)))
})


###############################################################################@
############################# score_info #######################################
###############################################################################@

test_that("score_info", {

  dtf <- data.frame(
    true_theta = seq(-4, 4, 0.5),
    est_theta = c(
      -2.430531, -2.316062, -2.100415, -1.779566, -2.362738, -2.100788,
      -2.288247, -2.702784, -2.506024, -2.615541, -2.11166, -1.943081,
      -2.36314, -1.990161, -2.422719, -2.497411, -2.683868, -2.348867,
      -2.111577, -2.6934, -2.201781, -1.801934, -2.013848, -2.310344, -1.827356,
      -2.497884, -2.380881, -2.536594, -2.243923, -2.395628, -1.499977,
      -2.023471, -1.924005, -2.486661, -2.16722, -1.998631, -1.592165,
      -2.642884, -1.222019, -2.371198, -1.454231, -1.9767, -2.408944, -1.489217,
      -1.919679, -1.667744, -1.489227, -1.352056, -1.773447, -1.327586,
      -1.499072, -1.262665, -1.620629, -0.699537, -1.395482, -1.171707,
      -1.446412, -1.184794, -0.984963, -1.468609, -0.949585, -0.463254,
      -0.808961, -0.601936, -0.512312, -0.919554, -0.015234, -0.765556,
      -1.011321, -0.672591, -0.87888, -0.232326, -0.461512, -0.867475,
      -1.022559, -0.576963, -0.498658, -0.034344, -0.209091, -0.514374,
      0.18833, 0.178318, -0.22005, 0.116477, 0.090685, -0.081721, 0.226084,
      -0.352253, -0.305916, -0.096985, 0.246753, 0.206033, 0.888314, 0.576819,
      0.27262, 0.861157, 0.743889, 0.43895, 0.727377, 0.389475, 0.940377,
      0.524375, 1.24438, 1.249584, 0.875173, 1.034915, 0.777783, 0.955599,
      0.606263, 1.294469, 1.239519, 1.544825, 1.237777, 1.142372, 1.87516,
      1.87516, 1.183065, 1.87516, 1.87516, 1.083321, 1.87516, 1.87516,
      1.87516, 1.012927, 1.87516, 1.87516, 1.87516, 1.87516, 1.544825,
      1.87516, 1.87516, 1.87516, 1.87516, 1.87516, 1.87516, 1.87516, 1.544825,
      1.87516, 1.544825, 1.87516, 1.87516, 1.87516, 1.87516, 1.87516, 1.87516,
      1.87516, 1.87516, 1.87516, 1.87516, 1.87516, 1.87516, 1.87516, 1.87516,
      1.87516, 1.87516, 1.87516, 1.87516, 1.87516, 1.87516, 1.87516, 1.87516,
      1.87516, 1.87516, 1.87516, 1.87516, 1.87516, 1.87516, 1.87516, 1.87516,
      1.87516
    )
  )
  expected <- structure(
    list(
      theta = c(-3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3),
      sinfo = c(
        0.00112453519073912, 1.80492287617084e-05, 2.44509275462053e-05,
        0.000738726088817481, 0.000673989214543598, 0.000787140491573192,
        0.00171628861698954, 0.00321806764673126, 0.00542962868356037,
        0.00680336952349951, 0.00167266293395082, 0.000410812580209677,
        0.00027723251115547)),
    class = "data.frame", row.names = c(NA, -13L))

  observed <- score_info(dtf$true_theta, dtf$est_theta)
  observed <- observed[-c(1, nrow(observed)), ]
  expect_equal(observed$score_info, expected$sinfo, tolerance = 1e-8)
})






###############################################################################@
############################# qip_index ########################################
###############################################################################@

test_that("qip_index", {

  # -------------------------------------------------------------------------- #
  # Replicate QIP calculations for Table 1 of Gonulates (2019)
  test_data <- data.frame(
    est_before = c(0, 1.418, 2.312, 1.839, 2.198, 1.703, 1.834, 1.931, 2.001,
                   2.047, 2.086, 2.118, 2.145, 2.168, 2.185),
    b = c(0.001, 1.339, 2.226, 1.632, 1.511, 1.069, 1.047, 0.947, 0.755,
          0.707, 0.63, 0.585, 0.5, 0.348, 0.248),
    resp = c(1L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L),
    est_after = c(1.418, 2.312, 1.839, 2.198, 1.703, 1.834, 1.931, 2.001,
                  2.047, 2.086, 2.118, 2.145, 2.168, 2.185, 1.865),
    info = c(0.722, 0.719, 0.719, 0.701, 0.523, 0.547, 0.476, 0.384, 0.277,
             0.244, 0.207, 0.185, 0.157, 0.12, 0.1),
    info_max = c(0.722, 0.722, 0.722, 0.722, 0.722, 0.722, 0.722, 0.722,
                 0.722, 0.722, 0.722, 0.722, 0.722, 0.722, 0.722),
    qip_k = c(1, 0.995, 0.995, 0.97, 0.724, 0.758, 0.659, 0.532, 0.383,
              0.337, 0.286, 0.256, 0.217, 0.166, 0.138),
    qip = c(1, 0.998, 0.997, 0.99, 0.937, 0.907, 0.871, 0.829, 0.779,
            0.735, 0.694, 0.658, 0.624, 0.591, 0.561))

  ip <- itempool(b = test_data$b, D = 1.7)
  n_item <- length(ip)
  cd <- create_cat_design(ip = ip,
                          next_item_rule = "fixed",
                          termination_rule = 'max_item',
                          termination_par = list(max_item = n_item))

  cat_data <- cat_sim(true_ability = 0, cd = cd)
  for (i in 1:n_item) {
    cat_data$est_history[[i]]$est_before <- test_data$est_before[i]
    cat_data$est_history[[i]]$resp <- test_data$resp[i]
    cat_data$est_history[[i]]$est_after <- test_data$est_after[i]
  }

  qip_k <- qip_index(cat_sim_output = cat_data)
  expect_equal(unname(qip_k[1, ]), test_data$qip_k, tolerance = 1e-3)
  qip <- qip_index(cat_sim_output = cat_data, summary_func = "mean")
  expect_equal(qip, tail(test_data$qip, 1), tolerance = 1e-3)

  # -------------------------------------------------------------------------- #
  # QIP Works for 1PL
  n <- 10 # number of items
  ip <- itempool(data.frame(b = rnorm(n)), item_id = paste0("i-", 1:n))

  cd <- create_cat_design(ip = ip, next_item_rule = "random",
                         termination_rule = "max_item",
                         termination_par = list(max_item = n))
  co <- cat_sim(true_ability = rnorm(sample(5:20, 1)), cd = cd)
  qip_mean <- qip_index(co, summary_func = "mean")
  qip_median <- qip_index(co, summary_func = "median")
  qip_min <- qip_index(co, summary_func = "min")
  qip_max <- qip_index(co, summary_func = "max")
  qip_q55 <- qip_index(co, summary_func = "quantile", prob = .55)

  i <- sample(1:length(co), 1)
  qip <- qip_index(co[[i]], summary_func = NULL)[1,]
  expect_equal(unname(qip_mean[i]), mean(qip))
  expect_equal(unname(qip_median[i]), median(qip))
  expect_equal(unname(qip_min[i]), min(qip))
  expect_equal(unname(qip_max[i]), max(qip))
  expect_equal(unname(qip_q55[i]), unname(quantile(qip, prob = .55)))

})

