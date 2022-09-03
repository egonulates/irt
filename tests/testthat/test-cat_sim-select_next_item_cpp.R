# library(rbenchmark, testthat)


#' This function creates an cat_output object in order to ease the writing
#' of est_history that will be used throughout this test module. Ideally
#' one does not need to
create_est_history <- function(num_of_steps, ip = NULL,
                               empty_last_step = TRUE, cat_design = NULL,
                               true_ability = rnorm(1)) {
  cd <- cat_design
  if (is.null(cat_design) && !is.null(ip)) {
    cd <- create_cat_design(
      ip = ip, termination_rule = 'max_item',
      termination_par = list('max_item' = min(length(ip), num_of_steps)))
  }
  co <- cat_sim(true_ability = true_ability, cd = cd)
  # Only get the portion of the estimate history that is declared by number
  # of steps
  co$est_history <- co$est_history[1:num_of_steps]
  if (empty_last_step) {
    i <- length(co$est_history)
    co$est_history[[i]]$resp <- NA
    co$est_history[[i]]$est_after <- NA
    co$est_history[[i]]$se_after <- NA
    co$est_history[[i]]["testlet"] <- list(NULL)
    co$est_history[[i]]["item"] <- list(NULL)
  }
  return(co)
}

print_est_history <- function(est_history, silent = FALSE) {
  eh <- list(est_history = est_history)
  class(eh) <- "cat_output"
  return(irt:::.print.cat_output(eh, silent = silent))
}


############################################################################@###
################### select_next_item_cpp #######################################
############################################################################@###

test_that("select_next_item_cpp", {
  # -------------------------------------------------------------------------- #
  # If an item from a testlet is administered, then regardless of the plan,
  # the next item will be the item from that testlet:
  t1 <- testlet(itempool(b = -4:-2, item_id = c("t1-i1", "t1-i2", "t1-i3")))
  t2 <- testlet(itempool(b = 2:3, item_id = c("t2-i1", "t2-i2")))
  ip <- c(t1, t2, itempool(b = -1:0, item_id = paste0("i", 1:2)))
  t1 <- ip[[1]]
  t2 <- ip[[2]]
  cd <- create_cat_design(ip = ip, next_item_rule = 'random',
                         termination_rule = 'max_item',
                         termination_par = list('max_item' = 4))
  est_history <- list(
    list(est_before = 0.2, se_before = 0.3, resp = 0,
         item = ip[[4]], testlet = NULL, est_after = 0.3, se_after = .2),
    # As a second item administer the first item in the testlet
    list(est_before = 0.2, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = 0.3, se_after = .2),
    list(est_before = 0.2, se_before = 0.3, resp = NA, item = NULL,
         testlet = NULL, est_after = 0.3, se_after = .2)
    )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_identical(selected_item[["item"]], t1$item_list[[2]])
  expect_identical(selected_item[["testlet"]], t1)

})

############################################################################@###
################### Item Selection - Infinite ##################################
############################################################################@###

test_that("Item Selection - Infinite", {
  cd <- create_cat_design(next_item_rule = 'random',
                          termination_rule = 'max_item',
                          termination_par = list('max_item' = 5))
  initial_theta <- round(rnorm(1, 0, .4), 2)
  est_history <- list(
    list(est_before = initial_theta, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
    )

  # Select first item
  est_history <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  est_history_step <- tail(est_history, 1)[[1]]
  selected_item <- est_history_step$item
  expect_s4_class(selected_item, "1PL")
  expect_equal(selected_item$b, initial_theta)
  expect_null(est_history_step$testlet)

  # Select second item
  initial_theta <- round(rnorm(1, 0, .4), 2)
  est_history <- list(
    list(est_before = 0.2, se_before = 0.3, resp = 0,
         item = selected_item, testlet = NULL, est_after = initial_theta,
         se_after = .2),
    list(est_before = initial_theta, se_before = 0.2, resp = NA,
         item = NULL, testlet = NULL, est_after = 0.3, se_after = .2)
    )
  est_history <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  est_history_step <- tail(est_history, 1)[[1]]
  selected_item <- est_history_step$item
  expect_s4_class(selected_item, "1PL")
  expect_equal(selected_item$b, initial_theta)
  expect_null(est_history_step$testlet)
})


############################################################################@###
################### Item Selection - Random ####################################
############################################################################@###

test_that("Item Selection - Random", {
  n_ip <- 5
  ip <- itempool(a = runif(n_ip, .5, 1.5), b = rnorm(n_ip))
  cd <- create_cat_design(ip = ip, next_item_rule = 'random',
                         termination_rule = 'max_item',
                         termination_par = list('max_item' = n_ip))
  est_history <- list(
    list(est_before = 0.2, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
    )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_true(selected_item$item$item_id %in% ip$item_id)

  est_history <- list(
    list(est_before = 0.2, se_before = 0.3, resp = 0,
         item = ip[[1]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0.2, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = 0.3, se_after = .2)
    )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_true(selected_item$item$item_id %in% ip[-1]$item_id)

  est_history <- list(
    list(est_before = 0.2, se_before = 0.3, resp = 0,
         item = ip[[1]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0.2, se_before = 0.3, resp = 0,
         item = ip[[2]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0.2, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = 0.3, se_after = .2)
    )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_true(selected_item$item$item_id %in% ip[-c(1:2)]$item_id)

  est_history <- list(
    list(est_before = 0.2, se_before = 0.3, resp = 0,
         item = ip[[1]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0.2, se_before = 0.3, resp = 0,
         item = ip[[2]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0.2, se_before = 0.3, resp = 0,
         item = ip[[3]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0.2, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = 0.3, se_after = .2)
    )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_true(selected_item$item$item_id %in% ip[-c(1:3)]$item_id)

  # -------------------------------------------------------------------------- #
  # Random Test Item Selection with Simpson-Hetter Exposure control
  # When there is an item with very low Simpson hetter value, that item will
  # not be administered:
  n_items <- 2000
  test_length <- 3
  ip <- generate_ip(n = n_items, misc = lapply(
    c(rep(1, test_length), rep(0, n_items - test_length)),
    function(x) list(sympson_hetter_k = x)))
  cd <- create_cat_design(ip = ip,
                          next_item_rule = "random",
                          exposure_control_rule = "sympson-hetter",
                          termination_rule = 'max_item',
                          termination_par = list('max_item' = test_length))
  # Select a testlet as first item
  est_history <- list(
    list(est_before = -4, se_before = 0.3, resp = 1,
         item = ip[[1]], testlet = NULL, est_after = 0, se_after = 1),
    list(est_before = 0, se_before = 0.3, resp = 1,
         item = ip[[2]], testlet = NULL, est_after = 0, se_after = 1),
    list(est_before = 0, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  for (i in 1:5) {
    selected_item <- irt:::select_next_item_cpp(
      cd = cd, est_history = est_history, additional_args = list())
    expect_true("set_aside_item_list" %in% names(selected_item$additional_args))
    # Even though there are hundreds of items, due to exposure control only
    # the one eligible item selected.
    expect_identical(selected_item$est_history[[3]]$item, ip[[3]])
  }

  # -------------------------------------------------------------------------- #
  # Random item selection with Testlets. There is randomization between
  # elements whereas within testlet items are selected via MFI.
  t1 <- testlet(itempool(b = c(-1, 1), item_id = c("t1-i1", "t1-i2"), D = 1.702),
                testlet_id = "t1")
  t2 <- testlet(itempool(b = c(-2, 0, 2), item_id = c("t2-i1", "t2-i2", "t2-i3"),
                          D = 1.702), testlet_id = "t2")
  i1 <- item(b = -1.5, item_id = "i1", D = 1.702)
  i2 <- item(b = 0.25, item_id = "i2", D = 1.702)
  i3 <- item(b = 1.5, item_id = "i3", D = 1.702)
  ip <- c(t1, t2, i1, i2, i3)

  cd <- create_cat_design(
    ip = ip,
    next_item_rule = "random",
    testlet_rules = list(next_item_rule = "mfi",
                         termination_rule = "max_item",
                         termination_par = list(max_item = 2)),
    termination_rule = 'max_item',
    termination_par = list('max_item' = 4))

  # Select a second item as testlet
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = 1.5, se_after = .6),
    list(est_before = 1.5, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_identical(eh_last_step$item, t2[[3]])
  expect_identical(eh_last_step$testlet, t2)



})


############################################################################@###
################### Item Selection - MEPV ######################################
############################################################################@###

test_that("Item Selection - Minimum Expected Posterior Variance - MEPV", {
  # -------------------------------------------------------------------------- #
  ### Minimum Expected Posterior Variance ###
  # Item
  n_ip <- 5
  ip <- itempool(b = -2:2)
  cd <- create_cat_design(ip = ip,
                          next_item_rule = "mepv",
                          next_item_par = list(var_calc_method = "eap"),
                          termination_rule = 'min_item',
                          termination_par = list('min_item' = n_ip))
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = NA, testlet = NULL,
         item = NULL, est_after = NA, se_after = NA)
  )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_identical(selected_item$item$item_id, ip[[3]]$item_id)

  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0,
         item = ip[[3]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 1.2, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_identical(selected_item$item$item_id, ip[[2]]$item_id)
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0,
         item = ip[[3]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0, se_before = 0.3, resp = 1,
         item = ip[[1]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0, se_before = 0.3, resp = 0,
         item = ip[[5]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = -0.5, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_identical(selected_item$item$item_id, ip[[2]]$item_id)
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0,
         item = ip[[3]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0, se_before = 0.3, resp = 1,
         item = ip[[1]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0, se_before = 0.3, resp = 0,
         item = ip[[5]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0, se_before = 0.3, resp = 0,
         item = ip[[2]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = -0.5, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_identical(selected_item$item$item_id, ip[[4]]$item_id)

  # -------------------------------------------------------------------------- #
  # MEPV does not change
  n_ip <- 5
  ip <- itempool(a = rlnorm(n_ip, 0, .3), b = rnorm(n_ip),
                     c = runif(n_ip, 0, .3))
  cd <- create_cat_design(ip = ip,
                          next_item_rule = "mepv",
                          next_item_par = list(var_calc_method = "owen"),
                          termination_rule = 'min_item',
                          termination_par = list('min_item' = n_ip))
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = NA, item = NULL,
         testlet = NULL, est_after = NA, se_after = NA)
  )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  selected_item_id <- selected_item$item$item_id
  for (i in 1:10) {
    selected_item <- irt:::select_next_item_cpp(
      cd = cd, est_history = est_history, list())$est_history
    selected_item <- selected_item[[length(selected_item)]]
    expect_identical(selected_item$item$item_id, selected_item_id)
  }

  # -------------------------------------------------------------------------- #
  ### Minimum Expected Posterior Variance ###
  # Item
  n_ip <- 5
  ip <- itempool(a = rlnorm(n_ip, 0, .3), b = rnorm(n_ip),
                     c = runif(n_ip, 0, .3))
  initial_ability_est <- rnorm(1)
  cd <- create_cat_design(ip = ip,
                          first_item_rule = "fixed_theta",
                          first_item_par = list(theta = initial_ability_est),
                          next_item_rule = "mepv",
                          next_item_par = list(var_calc_method = "owen"),
                          ability_est_rule = "owen",
                          ability_est_par = list(prior_mean = 0, prior_var = 4),
                          termination_rule = c("max_item"),
                          termination_par = list(max_item = n_ip))
  est_history <- list(
    list(est_before = initial_ability_est, se_before = NA, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  epv <- rep(0, n_ip)
  for (i in seq_len(n_ip)) {
    item <- ip[[i]]
    for (resp in 0:1) {
      P <- prob(ip = item, theta = initial_ability_est)[, 2]
      if (resp == 0) P <- 1 - P
      est <- irt:::est_ability_owen_cpp(ip = itempool(item), resp = resp,
                                        m0 = 0, v0 = 1)
      epv[i] = epv[i] + P * est$se^2
    }
  }
  expected <- ip$item_id[which.min(epv)]
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  observed <- selected_item[[length(selected_item)]]$item$item_id
  expect_identical(expected, observed)

  # -------------------------------------------------------------------------- #
  ### Testlet ###
  t1 <- testlet(itempool(b = -3:-2, item_id = c("t1-i1", "t1-i2"), D = 1.702),
                testlet_id = "t1")
  t2 <- testlet(itempool(b = 2:4, item_id = c("t2-i1", "t2-i2", "t2-i3"),
                          D = 1.702), testlet_id = "t2")
  i1 <- item(b = -1, item_id = "i1", D = 1.702)
  i2 <- item(b = 0, item_id = "i2", D = 1.702)
  i3 <- item(b = 1, item_id = "i3", D = 1.702)
  ip <- c(t1, t2, i1, i2, i3)
  expect_identical(length(ip), 5L)
  vcm <- "eap"
  for (vcm in c("eap", "owen")) {
    cd <- create_cat_design(ip = ip,
                            next_item_rule = "mepv",
                            next_item_par = list(var_calc_method = vcm),
                            termination_rule = 'min_item',
                            termination_par = list('min_item' = 4))
    # Select a testlet as first item
    est_history <- list(
      list(est_before = -4, se_before = 0.3, resp = NA,
           item = NULL, testlet = NULL, est_after = NA, se_after = NA)
    )
    selected_item <- irt:::select_next_item_cpp(
      cd = cd, est_history = est_history, list())$est_history
    selected_item <- selected_item[[length(selected_item)]]
    expect_identical(selected_item$item, t1@item_list[[1]])
    expect_identical(selected_item$testlet, t1)

    # Select an item as first item
    est_history <- list(
      list(est_before = 0, se_before = 0.3, resp = NA,
           item = NULL, testlet = NULL, est_after = NA, se_after = NA)
    )
    selected_item <- irt:::select_next_item_cpp(
      cd = cd, est_history = est_history, list())$est_history
    selected_item <- selected_item[[length(selected_item)]]
    expect_identical(selected_item$item, i2)
    expect_null(selected_item$testlet)

    # Select a 3-item testlet as first item
    est_history <- list(
      list(est_before = 4, se_before = 0.3, resp = NA,
           item = NULL, testlet = NULL, est_after = NA, se_after = NA)
    )
    selected_item <- irt:::select_next_item_cpp(
      cd = cd, est_history = est_history, list())$est_history
    selected_item <- selected_item[[length(selected_item)]]
    expect_identical(selected_item$item, t2@item_list[[1]])
    expect_identical(selected_item$testlet, t2)

    # Select a second item as testlet
    est_history <- list(
      list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
           testlet = t1, est_after = -1, se_after = .6),
      list(est_before = -1, se_before = 0.6, resp = NA,
           item = NULL, testlet = NULL, est_after = NA, se_after = NA)
    )
    selected_item <- irt:::select_next_item_cpp(
      cd = cd, est_history = est_history, list())$est_history
    selected_item <- selected_item[[length(selected_item)]]
    expect_identical(selected_item$item, t1@item_list[[2]])
    expect_identical(selected_item$testlet, t1)

    # Select a third item as testlet
    est_history <- list(
      list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[1]],
           testlet = t2, est_after = -1, se_after = .6),
      list(est_before = 0, se_before = 0.3, resp = 1, item = t2$item_list[[2]],
           testlet = t2, est_after = -1, se_after = .6),
      list(est_before = -1, se_before = 0.6, resp = NA,
           item = NULL, testlet = NULL, est_after = NA, se_after = NA)
    )
    selected_item <- irt:::select_next_item_cpp(
      cd = cd, est_history = est_history, list())$est_history
    selected_item <- selected_item[[length(selected_item)]]
    expect_identical(selected_item$item, t2@item_list[[3]])
    expect_identical(selected_item$testlet, t2)

    # Select a new item after an administration of a testlet
    est_history <- list(
      list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[1]],
           testlet = t2, est_after = -1, se_after = .6),
      list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
           testlet = t2, est_after = -1, se_after = .6),
      list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[3]],
           testlet = t2, est_after = -1, se_after = .6),
      list(est_before = -1, se_before = 0.6, resp = NA,
           item = NULL, testlet = NULL, est_after = NA, se_after = NA)
    )
    selected_item <- irt:::select_next_item_cpp(
      cd = cd, est_history = est_history, list())$est_history
    selected_item <- selected_item[[length(selected_item)]]
    if (vcm == "owen") {
      # # Here is how mepv calculated for two items
      # owen1 <- irt:::est_ability_owen_cpp(ip = t2@item_list,
      #                                     resp = c(0, 0, 0),
      #                                     m0 = 0, v0 = 1)
      # # i1
      # resp_lik(ip = i1, resp = 1, theta = -1) * irt:::est_ability_owen_cpp(
      #   ip = i1, resp = 1, m0 = owen1$est, v0 = owen1$se^2)$se^2 +
      # resp_lik(ip = i1, resp = 0, theta = -1) * irt:::est_ability_owen_cpp(
      #   ip = i1, resp = 0, m0 = owen1$est, v0 = owen1$se^2)$se^2
      # # i2
      # resp_lik(ip = i2, resp = 1, theta = -1) * irt:::est_ability_owen_cpp(
      #   ip = i2, resp = 1, m0 = owen1$est, v0 = owen1$se^2)$se^2 +
      # resp_lik(ip = i2, resp = 0, theta = -1) * irt:::est_ability_owen_cpp(
      #   ip = i2, resp = 0, m0 = owen1$est, v0 = owen1$se^2)$se^2
      expect_identical(selected_item$item, t1@item_list[[1]])
      expect_identical(selected_item$testlet, t1)
    } else {
      expect_identical(selected_item$item, i1)
      expect_null(selected_item$testlet)
    }

    # Select a new item after an administration of a testlet
    est_history <- list(
      list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[1]],
           testlet = t2, est_after = -1, se_after = .6),
      list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
           testlet = t2, est_after = -1, se_after = .6),
      list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[3]],
           testlet = t2, est_after = -1, se_after = .6),
      list(est_before = -2.5, se_before = 0.6, resp = NA,
           item = NULL, testlet = NULL, est_after = NA, se_after = NA)
    )
    selected_item <- irt:::select_next_item_cpp(
      cd = cd, est_history = est_history, list())$est_history
    selected_item <- selected_item[[length(selected_item)]]
    expect_identical(selected_item$item, t1@item_list[[1]])
    expect_identical(selected_item$testlet, t1)
  }

  # -------------------------------------------------------------------------- #
  ### Sympson-Hetter ###
  t1 <- testlet(itempool(b = -3:-2, item_id = c("t1-i1", "t1-i2"), D = 1.702),
                testlet_id = "t1", misc = list(sympson_hetter_k = 0))
  t2 <- testlet(itempool(a = c(0.2, 0.2), b = 4:5,
                         item_id = c("t2-i1", "t2-i2"), D = 1.702),
                testlet_id = "t2", misc = list(sympson_hetter_k = 1))
  i1 <- item(b = -1, D = 1.702, item_id = "i1",
             misc = list(sympson_hetter_k = 1))
  i2 <- item(b = 0, D = 1.702, item_id = "i2",
             misc = list(sympson_hetter_k = 1))
  i3 <- item(b = 1, D = 1.702, item_id = "i3",
             misc = list(sympson_hetter_k = 1))
  ip <- c(t1, t2, i1, i2, i3)
  cd <- create_cat_design(ip = ip,
                          next_item_rule = "mepv",
                          next_item_par = list(var_calc_method = "eap"),
                          exposure_control_rule = "sympson-hetter",
                          termination_rule = 'max_item',
                          termination_par = list('max_item' = 4))
  # Select a testlet as first item
  est_history <- list(
    list(est_before = -4, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())
  # selected_item <- irt:::select_next_item_cpp(
  #   cd = cd, est_history = est_history, additional_args = list())
  expect_true("set_aside_item_list" %in% names(selected_item$additional_args))
  expect_identical(selected_item$additional_args$set_aside_item_list[[1]], t1)
  selected_item <- selected_item$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_identical(selected_item$item, i1)
  expect_null(selected_item$testlet)

  # skip("")
  # # ------------------------------------------------------------------------ #
  # # Performance Check of MEPV for large item pool
  # {
  #   cat("-------------------------\nStart time: ", strftime(Sys.time()), "\n")
  #   print(Sys.time())
  #   set.seed(123)
  #   n_items <- 500
  #   n_testlet <- 20
  #   ip <- itempool(c(sapply(paste0("Testlet_", 1:n_testlet),
  #                            function(x) generate_testlet(
  #                              testlet_id = x, n = 2)),
  #                     generate_ip(n = n_items, output = "list")))
  #   cd <- create_cat_design(ip = ip,
  #                           next_item_rule = "mepv",
  #                           next_item_par = list(var_calc_method = "owen"),
  #                           termination_rule = 'min_item',
  #                           termination_par = list('min_item' = 60))
  #   est_history <- create_est_history(num_of_steps = 50, ip = ip,
  #                                     cat_design = cd)$est_history
  #   # print_est_history(est_history)
  #   print(microbenchmark::microbenchmark(
  #     snic = irt:::select_next_item_cpp(
  #       cd = cd, est_history = est_history, additional_args = list()),
  #     bare = irt:::select_next_item_mepv_cpp(
  #       cd = cd, est_history = est_history, additional_args = list()),
  #     times = 100
  #   ))
  #   cat("---------------------------\nEnd time: ", strftime(Sys.time()), "\n")
  # }
  # # --------------------------- #
  # # Start time:  2020-07-06 15:55:36
  # # [1] "2020-07-06 15:55:36 EDT"
  # # Unit: milliseconds
  # # expr     min      lq     mean   median      uq      max neval
  # # snic 69.2849 70.2792 75.98840 71.19615 72.0817 120.5621    10
  # # bare 68.7588 69.9863 71.06902 70.62645 71.1626  77.8612    10
  # # --------------------------- #
  # # End time:  2020-07-06 15:55:42
  #
  # # --------------------------- #
  # # Start time:  2020-07-06 16:14:42
  # # [1] "2020-07-06 16:14:42 EDT"
  # # Unit: milliseconds
  # #  expr     min       lq     mean   median       uq       max neval
  # #  snic 58.6722 60.98655 63.81935 62.43820 64.10895   90.8495   100
  # #  bare 58.6808 61.84180 78.02609 63.00905 66.07295 1395.0916   100
  # # --------------------------- #
  # # End time:  2020-07-06 16:15:00
  #
  #
  #   cd_eap <- create_cat_design(ip = ip,
  #                           next_item_rule = "mepv",
  #                           next_item_par = list(var_calc_method = "eap"),
  #                           termination_rule = 'min_item',
  #                           termination_par = list('min_item' = 60))
  #   cd_owen <- create_cat_design(ip = ip,
  #                           next_item_rule = "mepv",
  #                           next_item_par = list(var_calc_method = "owen"),
  #                           termination_rule = 'min_item',
  #                           termination_par = list('min_item' = 60))
  #   est_history_eap <- create_est_history(num_of_steps = 50, ip = ip,
  #                                     cat_design = cd_eap)$est_history
  #   est_history_eap <- create_est_history(num_of_steps = 50, ip = ip,
  #                                     cat_design = cd_owen)$est_history
  #
  #   # print_est_history(est_history)
  #   print(microbenchmark::microbenchmark(
  #     eap = irt:::select_next_item_cpp(
  #       cd = cd_eap, est_history = est_history, additional_args = list()),
  #     owen = irt:::select_next_item_cpp(
  #       cd = cd_owen, est_history = est_history, additional_args = list()),
  #     times = 10
  #   ))
  #
  #
  #
  #
  #
  # # First performance check without any improvement.
  # # --------------------------- #
  # # Start time: "2020-07-06 13:49:11 EDT"
  # # Unit: seconds
  # # expr     min       lq     mean   median       uq      max neval
  # # snic 3.07022 3.074536 3.114801 3.105701 3.156388 3.190121    10
  # # bare 3.09067 3.104146 3.126132 3.127472 3.140221 3.173316    10
  # # End time: "2020-07-06 13:51:48 EDT"
  # # --------------------------- #
  #
  #
  # selected_item <- irt:::select_next_item_cpp(
  #   cd = cd, est_history = est_history, additional_args = list())
  # print_est_history(selected_item$est_history)
})


############################################################################@###
################### Item Selection - MFI ####################################
############################################################################@###

test_that("Item Selection - Maximum Fisher Information", {

  n_ip <- 5
  ip <- itempool(b = -2:2)
  cd <- create_cat_design(ip = ip,
                          next_item_rule = 'mfi',
                          termination_rule = 'min_item',
                          termination_par = list('min_item' = n_ip))
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = ip[[3]],
         testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 1.2, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = 0.3, se_after = .2)
    )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_identical(selected_item$item$item_id, ip[[4]]$item_id)
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0,
         item = ip[[3]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0, se_before = 0.3, resp = 0,
         item = ip[[1]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0, se_before = 0.3, resp = 0,
         item = ip[[5]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = -0.5, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = 0.3, se_after = .2)
    )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_identical(selected_item$item$item_id, ip[[2]]$item_id)

  # -------------------------------------------------------------------------- #
  # MFI with testlets should select the most informative item or testlet
  ip <- c(item(a = 1, b = -2),
          item(a = 1, b = -3),
          testlet(itempool(item(a = 0.5, b = 0),
                           item(a = 0.75, b = -1),
                           item_id = paste0("t1-i", 1:2)), testlet_id = "t1"),
          testlet(itempool(item(a = 0.25, b = 0),
                           item(a = 0.50, b = -1),
                           item(a = 0.25, b = 1),
                           item_id = paste0("t2-i", 1:3)),
				  testlet_id = "t2"),
          item(a = 1, b = 0)
          )
  cd <- create_cat_design(ip = ip,
                          next_item_rule = 'mfi',
                          termination_rule = 'min_item',
                          termination_par = list('min_item' = 3))
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = ip[[3]][[1]],
         testlet = ip[[3]], est_after = 0.3, se_after = .2),
    list(est_before = 1, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
    )

  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, additional_args = list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  # Second item of the test let is selected
  expect_identical(ip[[3]][[2]], selected_item$item)
  expect_identical(ip[[3]], selected_item$testlet)

  theta <- 1
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = ip[[2]],
         testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = theta, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
    )

  output <- irt:::select_next_item_fisher_max_info_cpp(
    cd = cd, est_history = est_history, additional_args = NULL)
  output <- output$est_history
  output <- output[[length(output)]]
  expect_identical(output$item, ip[[5]])
  expect_null(output$testlet)

  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, additional_args = list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_identical(selected_item$item, ip[[5]])
  expect_null(selected_item$testlet)


  theta <- -1
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = ip[[2]],
         testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = theta, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
    )
  item_info <- info(ip[-2], theta = theta)[1,]
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, additional_args = list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_identical(selected_item$testlet, ip[[3]])

  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # MFI with testlets and within testlet item selection.
  t1 <- testlet(itempool(b = c(-1, 1), item_id = c("t1-i1", "t1-i2"), D = 1.702),
                testlet_id = "t1")
  t2 <- testlet(itempool(b = c(-2, 0, 2), item_id = c("t2-i1", "t2-i2", "t2-i3"),
                          D = 1.702), testlet_id = "t2")
  i1 <- item(b = -1.5, item_id = "i1", D = 1.702)
  i2 <- item(b = 0.25, item_id = "i2", D = 1.702)
  i3 <- item(b = 1.5, item_id = "i3", D = 1.702)
  ip <- c(t1, t2, i1, i2, i3)

  cd <- create_cat_design(
    ip = ip,
    next_item_rule = "mfi",
    testlet_rules = list(next_item_rule = "mfi",
                         termination_rule = "max_item",
                         termination_par = list(max_item = 2)),
    termination_rule = 'max_item',
    termination_par = list('max_item' = 6))

  # --------------------------------- #
  # The most appropriate item from within the same testlet is selected after
  # the administration of the first item from the testlet.
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = -.5, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  # Even though there are better items in the item pool, the best item within
  # the testlet t2 is administered
  expect_identical(eh_last_step$item, t2[[1]])
  expect_identical(eh_last_step$testlet, t2)

  # --------------------------------- #
  # Testlet should terminate
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[3]],
         testlet = t2, est_after = -1.95, se_after = .6),
    list(est_before = -1.95, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  # Even though testlet t2's first item is the most appropriate one to
  # administer, only two items can be administered from a testlet. Hence,
  # The most appropriate next item is selected.
  expect_identical(eh_last_step$item, i1)
  expect_null(eh_last_step$testlet)

  # --------------------------------- #
  # The next testlet item is selected after the administration of the first
  # testlet.
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[3]],
         testlet = t2, est_after = 1, se_after = .6),
    list(est_before = 1, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_identical(eh_last_step$item, t1[[2]])
  expect_identical(eh_last_step$testlet, t1)


  # --------------------------------- #
  # The next testlet item is selected after the administration of the first
  # testlet.
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[2]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = -0.25, se_after = .6),
    list(est_before = -0.25, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_identical(eh_last_step$item, t2[[2]])
  expect_identical(eh_last_step$testlet, t2)

  # --------------------------------- #
  # The next standalone item is selected after the administration of the first
  # testlet.
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[2]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = 0.26, se_after = .6),
    list(est_before = 0.26, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_identical(eh_last_step$item, t2[[2]])
  expect_identical(eh_last_step$testlet, t2)

  # --------------------------------- #
  # A standalone item is selected after the administration of both testlets
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[2]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = 0.26, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[2]],
         testlet = t2, est_after = 0.26, se_after = .6),
    list(est_before = 0.26, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_identical(eh_last_step$item, i2)
  expect_null(eh_last_step$testlet)


  # --------------------------------- #
  # A testlet is selected after the administration of a standalone item
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[2]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = -0.25, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = i3,
         testlet = NULL, est_after = 1.6, se_after = .6),
    list(est_before = 1.6, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_identical(eh_last_step$testlet, t2)
  expect_identical(eh_last_step$item, t2[[3]])

  # --------------------------------- #
  # A testlet is selected after the administration of a standalone item
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[2]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = -0.25, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = i1,
         testlet = NULL, est_after = -1.6, se_after = .6),
    list(est_before = -1.6, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_identical(eh_last_step$testlet, t2)
  expect_identical(eh_last_step$item, t2[[1]])


  # --------------------------------- #
  # An item within the testlet is administered after the administration of the
  # first item from the same testlet (even though that testlet item is not the
  # most appropriate item to administer)
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[2]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = -0.25, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = i1,
         testlet = NULL, est_after = -1.6, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = 1.55, se_after = .6),
    list(est_before = 1.55, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_identical(eh_last_step$testlet, t2)
  expect_identical(eh_last_step$item, t2[[3]])


  # --------------------------------- #
  # A standalone item is selected after the administration of testlet
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[2]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = -0.25, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[1]],
         testlet = t2, est_after = -0.25, se_after = .6),
    list(est_before = 2, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_null(eh_last_step$testlet)
  expect_identical(eh_last_step$item, i3)

  # --------------------------------- #
  # A standalone item is selected after the administration of testlet
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[2]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = -0.25, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[1]],
         testlet = t2, est_after = -0.25, se_after = .6),
    list(est_before = 0, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_null(eh_last_step$testlet)
  expect_identical(eh_last_step$item, i2)


  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #

  # MFI selects most informative testlet or item
  t1 <- testlet(itempool(b = c(-3, -2), item_id = c("t1-i1", "t1-i2"), D = 1.702),
                testlet_id = "t1")
  t2 <- testlet(itempool(b = c(2, 3, 4), item_id = c("t2-i1", "t2-i2", "t2-i3"),
                          D = 1.702), testlet_id = "t2")
  i1 <- item(b = -1.5, item_id = "i1", D = 1.702)
  i2 <- item(b = 0.25, item_id = "i2", D = 1.702)
  i3 <- item(b = 1.5, item_id = "i3", D = 1.702)
  ip <- c(t1, t2, i1, i2, i3)

  cd <- create_cat_design(
    ip = ip,
    next_item_rule = "mfi",
    testlet_rules = list(next_item_rule = "mfi",
                         termination_rule = "max_item",
                         termination_par = list(max_item = 2)),
    termination_rule = 'max_item',
    termination_par = list('max_item' = 6))

  # --------------------------------- #
  # Most informative item is a standalone item
  last_theta <- 0
  # info(ip, theta = last_theta)
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[2]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = last_theta, se_after = .6),
    list(est_before = last_theta, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_null(eh_last_step$testlet)
  expect_identical(eh_last_step$item, i2)

  # --------------------------------- #
  # Most informative item is a testlet item
  last_theta <- 1.4
  # info(ip, theta = last_theta)
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[2]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = last_theta, se_after = .6),
    list(est_before = last_theta, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_identical(eh_last_step$testlet, t2)
  expect_identical(eh_last_step$item, t2[[1]])


  # -------------------------------------------------------------------------- #

})

############################################################################@###
################### Item Selection - Fixed #####################################
############################################################################@###

test_that("Item Selection - Fixed", {
  # Fixed test
  n_ip <- 5L
  ip <- itempool(a = runif(n_ip, .5, 1.5), b = rnorm(n_ip))
  cd <- create_cat_design(
    ip = ip,
    next_item_rule = 'fixed',
    next_item_par = list(list(item_id = "Item_2"), list(item_id = "Item_5"),
                         list(item_id = "Item_1"), list(item_id = "Item_3"),
                         list(item_id = "Item_4")),
    termination_rule = 'max_item',
    termination_par = list('max_item' = n_ip),
    exposure_control_rule = 'randomesque',
    exposure_control_par = list(num_items = 1))

  est_history <- list(
    list(est_before = 0.2, se_before = 0.3, resp = 1,
         item = ip[[4]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0.2, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = 0.3, se_after = .2)
    )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_identical(selected_item$item, ip[[5]])

  est_history <- list(
    list(est_before = 0.2, se_before = 0.3, resp = 1,
         item = ip[[4]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0.2, se_before = 0.3, resp = 0,
         item = ip[[5]], testlet = NULL, est_after = 0.3, se_after = .2),
    list(est_before = 0.2, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = 0.3, se_after = .2)
    )
  selected_item <- irt:::select_next_item_cpp(
    cd = cd, est_history = est_history, list())$est_history
  selected_item <- selected_item[[length(selected_item)]]
  expect_identical(selected_item$item, ip[[1]])






  # -------------------------------------------------------------------------- #
  #### TODO: UNCOMMENT THE FOLLOWING AND FIX THE ISSUE
  # # ------------------------------------------------------------------------ #
  # # Fixed where ip consist of standalone items and testlets.
  # temp_list <- list(ids = paste0("Testlet_", 1:8), n = 1:8)
  # ip <- itempool(sample(c(generate_ip(n = 10, output = "list"),
  #                         sapply(1:length(temp_list$ids), function(i)
  #                           generate_testlet(testlet_id = temp_list$ids[i],
  #                                            n = temp_list$n[i])))))
  #
  # cd <- create_cat_design(
  #   ip = ip,
  #   termination_rule = c('min_item', 'max_item'),
  #   termination_par = list(min_item = 8, max_item = 8),
  #   next_item_rule = 'fixed',
  #   next_item_par = list(item_id = c("Testlet_8", "Item_1")))
  #
  # est_history <- list(
  #   list(est_before = 0.2, se_before = 0.3, resp = NA,
  #        item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  #   )
  # selected_item <- irt:::select_next_item_cpp(
  #   cd = cd, est_history = est_history, list())$est_history
  # selected_item <- selected_item[[length(selected_item)]]
  # expect_identical(selected_item$testlet, ip["Testlet_8"][[1]])
  # expect_identical(selected_item$item, ip["Testlet_8"][[1]]$item_list[[1]])
  #
  # est_history <- list(
  #   list(est_before = 0.2, se_before = 0.3, resp = 1,
  #        item = ip["Testlet_8"][[1]]$item_list[[1]],
  #        testlet = ip["Testlet_8"][[1]], est_after = 0.3, se_after = .2),
  #   list(est_before = 0.2, se_before = 0.3, resp = NA,
  #        item = NULL, testlet = NULL, est_after = 0.3, se_after = .2)
  #   )
  # selected_item <- irt:::select_next_item_cpp(
  #   cd = cd, est_history = est_history, list())$est_history
  # selected_item <- selected_item[[length(selected_item)]]
  # expect_identical(selected_item$testlet, ip["Testlet_8"][[1]])
  # expect_identical(selected_item$item, ip["Testlet_8"][[1]]$item_list[[2]])

})

############################################################################@###
################### get_unadministered_testlet_items_cpp #######################
############################################################################@###

test_that("get_unadministered_testlet_items_cpp", {
  t1 <- testlet(itempool(b = -3:-2, item_id = c("t1-i1", "t1-i2"), D = 1.702),
                testlet_id = "t1")
  t2 <- testlet(itempool(b = 2:4, item_id = c("t2-i1", "t2-i2", "t2-i3"),
                          D = 1.702), testlet_id = "t2")
  i1 <- item(b = -1, item_id = "i1", D = 1.702)
  i2 <- item(b = 0, item_id = "i2", D = 1.702)
  i3 <- item(b = 1, item_id = "i3", D = 1.702)
  ip <- c(t1, t2, i1, i2, i3)


  cd <- create_cat_design(
    ip = ip,
    next_item_rule = "mfi",
    testlet_rules = list(next_item_rule = "none",
                         termination_rule = "max_item",
                         termination_par = list(max_item = 999)),
    termination_rule = 'max_item',
    termination_par = list('max_item' = 4))

  # get_unadministered_testlet_items_cpp <- irt:::get_unadministered_testlet_items_cpp

  # -------------------------------------------------------------------------- #
  est_history <- list(
    list(est_before = -4, se_before = 0.3, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  observed <- get_unadministered_testlet_items_cpp(testlet = t1,
                                                   est_history = est_history)

  expect_equal(observed, t1@item_list@item_list)
  expect_identical(names(observed), t1$item_id)

  # -------------------------------------------------------------------------- #
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = -1, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  observed <- get_unadministered_testlet_items_cpp(testlet = t1,
                                                   est_history = est_history)
  expect_equal(observed, t1@item_list@item_list[2])
  expect_identical(names(observed), t1@item_list@item_list[[2]]$item_id)

  observed <- get_unadministered_testlet_items_cpp(testlet = t2,
                                                   est_history = est_history)
  expect_equal(observed, t2@item_list@item_list)
  expect_equal(names(observed), t2$item_id)

  # -------------------------------------------------------------------------- #
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[1]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[3]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = -1, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  observed <- get_unadministered_testlet_items_cpp(testlet = t2,
                                                   est_history = est_history)
  expect_equal(observed, list())

  observed <- get_unadministered_testlet_items_cpp(testlet = t1,
                                                   est_history = est_history)
  expect_equal(observed, t1@item_list@item_list)
  expect_identical(names(observed), names(t1@item_list@item_list))

})


############################################################################@###
################### terminate_testlet_cat_cpp ##################################
############################################################################@###
test_that("terminate_testlet_cat_cpp", {

  t1 <- testlet(itempool(b = -3:-2, item_id = c("t1-i1", "t1-i2"), D = 1.702),
                testlet_id = "t1")
  t2 <- testlet(itempool(b = 2:4, item_id = c("t2-i1", "t2-i2", "t2-i3"),
                          D = 1.702), testlet_id = "t2")
  i1 <- item(b = -1, item_id = "i1", D = 1.702)
  i2 <- item(b = 0, item_id = "i2", D = 1.702)
  i3 <- item(b = 1, item_id = "i3", D = 1.702)
  ip <- c(t1, t2, i1, i2, i3)


  cd <- create_cat_design(
    ip = ip,
    next_item_rule = "mfi",
    testlet_rules = list(next_item_rule = "none",
                         termination_rule = "max_item",
                         termination_par = list(max_item = 2)),
    termination_rule = 'max_item',
    termination_par = list('max_item' = 2))

  # terminate_testlet_cat_cpp <- irt:::terminate_testlet_cat_cpp

  # -------------------------------------------------------------------------- #
  # Only the first item from the testlet administered, second item can be
  # administered.
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = -1, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  expect_false(terminate_testlet_cat_cpp(testlet = t1, cd = cd,
                                         est_history = est_history,
                                         additional_args =  list()))
  expect_false(terminate_testlet_cat_cpp(testlet = t2, cd = cd,
                                         est_history = est_history,
                                         additional_args =  list()))

  # -------------------------------------------------------------------------- #
  # Both items from the testlet has been administered, no more items should be
  # administered
  est_history <- list(
    list(est_before = 0, se_before = 0.5, resp = 0, item = t2$item_list[[1]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0.5, se_before = 0.4, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = -1, se_before = 0.2, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  # Since all of the items in the testlet administered this should return FALSE
  expect_true(terminate_testlet_cat_cpp(testlet = t2, cd = cd,
                                        est_history = est_history,
                                        additional_args =  list()))
  # t1 can be administered
  expect_false(terminate_testlet_cat_cpp(testlet = t1, cd = cd,
                                         est_history = est_history,
                                         additional_args =  list()))


  # -------------------------------------------------------------------------- #
  # Both items from the testlet t1 has been administered, no more items should
  # be administered
  est_history <- list(
    list(est_before = 0, se_before = 0.5, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = 0.5, se_before = 0.4, resp = 0, item = t1$item_list[[2]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = 0.5, se_before = 0.4, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = -1, se_before = 0.2, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  # Since all of the items in the testlet administered this should return FALSE
  expect_true(terminate_testlet_cat_cpp(testlet = t1, cd = cd,
                                        est_history = est_history,
                                        additional_args =  list()))
  expect_false(terminate_testlet_cat_cpp(testlet = t2, cd = cd,
                                         est_history = est_history,
                                         additional_args =  list()))

})

############################################################################@###
################### select_next_testlet_item_none_cpp ##########################
############################################################################@###

test_that("select_next_testlet_item_none_cpp", {
  t1 <- testlet(itempool(b = -3:-2, item_id = c("t1-i1", "t1-i2"), D = 1.702),
                testlet_id = "t1")
  t2 <- testlet(itempool(b = 2:4, item_id = c("t2-i1", "t2-i2", "t2-i3"),
                          D = 1.702), testlet_id = "t2")
  i1 <- item(b = -1, item_id = "i1", D = 1.702)
  i2 <- item(b = 0, item_id = "i2", D = 1.702)
  i3 <- item(b = 1, item_id = "i3", D = 1.702)
  ip <- c(t1, t2, i1, i2, i3)

  cd <- create_cat_design(
    ip = ip,
    next_item_rule = "mfi",
    testlet_rules = list(next_item_rule = "none",
                         termination_rule = "max_item",
                         termination_par = list(max_item = 999)),
    termination_rule = 'max_item',
    termination_par = list('max_item' = 4))

  # select_next_testlet_item_none_cpp <- irt:::select_next_testlet_item_none_cpp
  # process_testlet_cat_cpp <- irt:::process_testlet_cat_cpp

  # Select a second item as testlet
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = -1, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  selected_item <- tail(select_next_testlet_item_none_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_identical(selected_item$item, t1@item_list[[2]])
  expect_identical(selected_item$testlet, t1)
  # Check 'process_testlet_cat_cpp' function
  selected_item <- tail(process_testlet_cat_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_identical(selected_item$item, t1@item_list[[2]])
  expect_identical(selected_item$testlet, t1)

  # -------------------------------------------------------------------------- #
  # Select a new item after an administration of a testlet
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[1]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[3]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = -1, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  selected_item <- select_next_testlet_item_none_cpp(
    cd = cd, est_history = est_history, list())
  expect_equal(selected_item, list())

  selected_item <- process_testlet_cat_cpp(
    cd = cd, est_history = est_history, list())
  expect_equal(selected_item, list())

  # -------------------------------------------------------------------------- #
  # Select a new testlet item when the testlet is selected at the last step of
  # est_history
  est_history <- list(
    list(est_before = -1, se_before = 0.6, resp = NA,
         item = NULL, testlet = t1, est_after = NA, se_after = NA)
  )
  selected_item <- tail(select_next_testlet_item_none_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_identical(selected_item$item, t1@item_list[[1]])
  expect_identical(selected_item$testlet, t1)

  # -------------------------------------------------------------------------- #
  # Select a new testlet item when the testlet is selected at the last step of
  # est_history
  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[1]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[3]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = -1, se_before = 0.6, resp = NA,
         item = NULL, testlet = t1, est_after = NA, se_after = NA)
  )
  selected_item <- tail(select_next_testlet_item_none_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]
  expect_identical(selected_item$item, t1@item_list[[1]])
  expect_identical(selected_item$testlet, t1)

})


############################################################################@###
################### select_next_testlet_item_mfi_cpp ###########################
############################################################################@###

test_that("select_next_testlet_item_mfi_cpp", {

  t1 <- testlet(itempool(b = c(-1, 1), item_id = c("t1-i1", "t1-i2"), D = 1.702),
                testlet_id = "t1")
  t2 <- testlet(itempool(b = c(-2, 0, 2), item_id = c("t2-i1", "t2-i2", "t2-i3"),
                          D = 1.702), testlet_id = "t2")
  i1 <- item(b = -1.5, item_id = "i1", D = 1.702)
  i2 <- item(b = 0.25, item_id = "i2", D = 1.702)
  i3 <- item(b = 1.5, item_id = "i3", D = 1.702)
  ip <- c(t1, t2, i1, i2, i3)

  cd <- create_cat_design(
    ip = ip,
    next_item_rule = "mfi",
    testlet_rules = list(next_item_rule = "mfi",
                         termination_rule = "max_item",
                         termination_par = list(max_item = 999)),
    termination_rule = 'max_item',
    termination_par = list('max_item' = 2))

  # -------------------------------------------------------------------------- #
  # Best item within a testlet is selected
  est_history <- list(
    list(est_before = 0, se_before = 0.2, resp = 0, item = t1$item_list[[1]],
         testlet = t1, est_after = -1, se_after = .6),
    list(est_before = -2, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_testlet_item_mfi_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]

  expect_equal(eh_last_step$testlet, t1)
  expect_equal(eh_last_step$item, t1$item_list[[2]])

  # -------------------------------------------------------------------------- #
  # Best item within a testlet is selected -- Important check!!
  est_history <- list(
    list(est_before = 0, se_before = 0.2, resp = 0, item = t2$item_list[[1]],
         testlet = t2, est_after = 1.5, se_after = .6),
    list(est_before = 1.5, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- tail(irt:::select_next_testlet_item_mfi_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]

  expect_equal(eh_last_step$testlet, t2)
  expect_equal(eh_last_step$item, t2$item_list[[3]])

  # -------------------------------------------------------------------------- #
  # Best item within a testlet is selected -- Important check!!
  est_history <- list(
    list(est_before = -2, se_before = 0.2, resp = 0, item = t2$item_list[[3]],
         testlet = t2, est_after = 0.25, se_after = .6),
    list(est_before = 0.25, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  eh_last_step <- tail(irt:::select_next_testlet_item_mfi_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]

  expect_equal(eh_last_step$testlet, t2)
  expect_equal(eh_last_step$item, t2$item_list[[2]])

  # -------------------------------------------------------------------------- #
  # Best item within a testlet is selected -- Important check!!
  est_history <- list(
    list(est_before = 2, se_before = 0.2, resp = 0, item = t2$item_list[[3]],
         testlet = t2, est_after = -2.25, se_after = .6),
    list(est_before = -2.25, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )
  eh_last_step <- tail(irt:::select_next_testlet_item_mfi_cpp(
    cd = cd, est_history = est_history, list())$est_history, 1)[[1]]

  expect_equal(eh_last_step$testlet, t2)
  expect_equal(eh_last_step$item, t2$item_list[[1]])

  # -------------------------------------------------------------------------- #
  # No item left in the testlet
  cd <- create_cat_design(
    ip = ip,
    next_item_rule = "mfi",
    testlet_rules = list(next_item_rule = "mfi",
                         termination_rule = "max_item",
                         termination_par = list(max_item = 5)),
    termination_rule = 'max_item',
    termination_par = list('max_item' = 4))

  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[1]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[3]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = -1, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- irt:::select_next_testlet_item_mfi_cpp(
    cd = cd, est_history = est_history, list())

  expect_equal(eh_last_step, list())

  # -------------------------------------------------------------------------- #
  # Termination criteria satisfied
  cd <- create_cat_design(
    ip = ip,
    next_item_rule = "mfi",
    testlet_rules = list(next_item_rule = "mfi",
                         termination_rule = "max_item",
                         termination_par = list(max_item = 2)),
    termination_rule = 'max_item',
    termination_par = list('max_item' = 4))

  est_history <- list(
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[1]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = 0, se_before = 0.3, resp = 0, item = t2$item_list[[2]],
         testlet = t2, est_after = -1, se_after = .6),
    list(est_before = -1, se_before = 0.6, resp = NA,
         item = NULL, testlet = NULL, est_after = NA, se_after = NA)
  )

  eh_last_step <- irt:::select_next_testlet_item_mfi_cpp(
    cd = cd, est_history = est_history, list())

  expect_equal(eh_last_step, list())

})

