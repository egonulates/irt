

# library(testthat)


###############################################################################@
############################# check_validity_response_set_cpp() ################
###############################################################################@

test_that("Test 'check_validity_response_set_cpp()' function", {

  ip <- c(generate_testlet(item_id_preamble = "t1"),
          generate_ip(n = 5, model = c("2PL", "3PL", "GPCM", "PCM", "GRM")),
          generate_testlet(item_id_preamble = "t2"))
  rs <- sim_resp(theta = rnorm(3), ip = ip, prop_missing = 0.3,
                 output = "response_set")

  # ---------------------------------------------------------------------------#

  ip <- generate_ip(n = 5, item_id = paste0("i", 1:5))
  x_long <- data.frame(examinee_id = c("stu1", "stu1", "stu1", "stu2", "stu2"),
                       item_id = c("i1", "i2", "i4", "i1", "i2"),
                       scr = c(0, 1, 0, 1, 0),
                       rwscore = c("A", "D", "B", "C", "D"),
                       resptime = c(33, 55, 22, 66, 31),
                       # These will be passed to misc
                       item_type = c("MC", "MC", "MS", "SA", "MC"),
                       word_count = c(123, 442, 552, 342, 666),
                       # This will be ignored
                       lexile_level = c(1, 4, 3, 2, 1)
                       )

  rs <- response_set(x = x_long,
                     data_format = "long",
                     examinee_id_var = "examinee_id",
                     item_id_var = "item_id",
                     score_var = "scr",
                     raw_response_var = "rwscore",
                     response_time_var = "resptime",
                     misc_var = c("item_type", "lexile_level")
                     )

  # ---------------------------------------------------------------------------#
  # Check whether function complains when there is  NA.
  rs_test <- rs
  rs_test@response_list[[1]]@score[1] <- NA
  expect_error(irt:::check_validity_response_set_cpp(resp = rs_test, ip = ip),
               "Invalid 'score'. 'score' vector should not contain NA.")

  # ---------------------------------------------------------------------------#
  # Check whether function complains when 'score' is NULL.
  rs_test <- rs
  rs_test@response_list[[1]]@score <- NULL
  expect_error(irt:::check_validity_response_set_cpp(resp = rs_test, ip = ip),
               "Invalid 'score'. 'score' cannot be NULL.")

  # ---------------------------------------------------------------------------#
  # resp should be response_set object
  expect_error(irt:::check_validity_response_set_cpp(resp = rs@response_list[[1]],
                                                 ip = ip),
               "Invalid 'resp_set'. 'resp_set' should be a Response_set object")

  # ---------------------------------------------------------------------------#
  # ip should be an Itempool object
  expect_error(irt:::check_validity_response_set_cpp(resp = rs,
                                                 ip = ip@item_list[[1]]),
               "Invalid 'ip'. 'ip' should be an Itempool object.")

  # ---------------------------------------------------------------------------#
  # All item_ids should be valid.
  rs_test <- rs
  rs_test@response_list[[1]]@item_id[1] <- "xyz"
  expect_error(irt:::check_validity_response_set_cpp(resp = rs_test, ip = ip),
               paste0("Invalid 'ip'. All of the items in the response ",
                      "data should be in the item pool, ip."))

  # ---------------------------------------------------------------------------#
  # Check whether function complains when 'item_id' is NULL.
  rs_test <- rs
  rs_test@response_list[[1]]@item_id <- rs_test@response_list[[1]]@item_id[1:2]
  expect_error(irt:::check_validity_response_set_cpp(resp = rs_test, ip = ip),
               "Incompatible 'item_id' and 'score'.")

  # ---------------------------------------------------------------------------#
  # # The length of item_id and scores should be the same
  # rs_test <- rs
  # rs_test@response_list[[1]]@item_id <- NULL
  # expect_error(irt:::check_validity_response_set_cpp(resp = rs_test, ip = ip),
  #              "Invalid 'item_id'. 'item_id' cannot be NULL.")

  # ---------------------------------------------------------------------------#
  # Check
  for (i in 1:5) {
    theta <- rnorm(6)
    ip <- generate_ip(n = 8)
    resp <- sim_resp(theta = theta, ip = ip, prop_missing = .8,
	                 output = "response_set")
    expect_true(irt:::check_validity_response_set_cpp(resp = resp, ip = ip))
  }
})


###############################################################################@
############################# get_examinee_id_response_set_cpp() ###############
###############################################################################@

test_that("Test 'get_examinee_id_response_set_cpp()' function", {
  n_item <- sample(11:31, 1)
  n_theta <- sample(7:21, 1)
  ip <- generate_ip(n = n_item)
  theta <- rnorm(n_theta)
  resp_set <- sim_resp(ip = ip, theta = theta, prop_missing = .2,
                       output = "response_set")
  expect_identical(irt:::get_examinee_id_response_set_cpp(resp_set),
               paste0("S", 1:n_theta))

  # ---------------------------------------------------------------------------#
  # Benchmark two methods of examinee_id extraction
  # microbenchmark::microbenchmark(
  #   A = unlist(sapply(resp_set@response_list, slot, "examinee_id")),
  #   B = irt:::get_examinee_id_response_set_cpp(resp_set), times = 1e4
  #   )

  # ---------------------------------------------------------------------------#
  # Set one of the examinee_id's of responses to NULL
  i <- sample(1:n_theta, 1)
  resp_set[[i]]@examinee_id <- NULL
  examinee_ids <- irt:::get_examinee_id_response_set_cpp(resp_set)
  expect_true(is.na(examinee_ids[i]))

  # ---------------------------------------------------------------------------#
  # Set all of the examinee_ids to NULL, result of the function should be NULL
  for (i in 1:n_theta) resp_set[[i]]@examinee_id <- NULL
  expect_null(irt:::get_examinee_id_response_set_cpp(resp_set))
})



###############################################################################@
############################# max_score_response_set_cpp #######################
###############################################################################@

test_that("Test 'get_examinee_id_response_set_cpp()' function", {
  n_categories <- c(sample(4:8, 3), rep(2, 4))
  ip <- generate_ip(model = c(rep("GPCM", 3), rep("3PL", 4)),
                    n_categories = n_categories)
  resp_set <- sim_resp(ip = ip, theta = rnorm(1), output = "response_set")

  expect_identical(irt:::max_score_response_set_cpp(resp_set = resp_set, ip = ip),
                   sum(n_categories) - length(ip))

})
