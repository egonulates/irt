# library(testthat)

skip_tests <- TRUE

###############################################################################@
############################# ws_extract_data ##################################
###############################################################################@
test_that("ws_extract_data", {
  skip_on_cran()
  skip_if(skip_tests)

  target_dir <- "c://temp/est_winsteps"

  # -------------------------------------------------------------------------- #
  # 'item_ids' should be valid if 'seq' column is not in the anchor_info
  item_ids <- sample(paste0("i", 1:10))
  anchor_info <- data.frame(item_id = sample(item_ids, 4),
                            b = round(rnorm(4), 2))
  expect_error(ws_create_anchor_file(anchor_info = anchor_info,
                                     target_dir = target_dir,
                                     analysis_name = "anchor_file_test",
                                     item_ids = NULL),
               regexp = "Invalid 'item_ids'"
               )

  # -------------------------------------------------------------------------- #
  # 'anchor_info' should contain either 'seq' or 'item_id' or both.
  item_ids <- sample(paste0("i", 1:10))
  anchor_info <- data.frame(abc = sample(item_ids, 4),
                            b = round(rnorm(4), 2))
  expect_error(ws_create_anchor_file(
    anchor_info = anchor_info,
    target_dir = target_dir,
    analysis_name = "anchor_file_test",
    item_ids = NULL),
    regexp = "Invalid \\'anchor_file\\'")

})





###############################################################################@
############################# ws_extract_data ##################################
###############################################################################@
test_that("ws_extract_data", {
  skip_on_cran()
  skip_if(skip_tests)

  # -------------------------------------------------------------------------- #
  n_examinee <- 10
  resp_data <- as.data.frame(sim_resp(ip = generate_ip(n = 8),
                                      theta = rnorm(n_examinee),
                                      prop_missing = .3))
  resp_data[, 1] <- 1
  x <- cbind(ex_id = rownames(resp_data),
             gender = sample(c("Male", "Female"), n_examinee, TRUE),
             grade = sample(6:8, n_examinee, TRUE),
             resp_data)
  observed <- ws_extract_data(x, examinee_id_var = "ex_id",
                              additional_vars = c("gender", "grade"))
  expect_equal(observed$examinee_id, x$ex_id)
  expect_equal(observed$resp, resp_data)
  expect_equal(observed$misc, x[, c("gender", "grade")])
  # Make sure function output has the following list elements
  expect_true(all(c("examinee_id", "resp", "misc") %in% names(observed)))


  # -------------------------------------------------------------------------- #
  # Column numbers as items value
  observed <- ws_extract_data(x, examinee_id_var = "ex_id",
                              items = c(2, 5, 6, 9))
  expect_equal(observed$examinee_id, x$ex_id)
  expect_equal(observed$resp, x[, c(2, 5, 6, 9)])
  expect_null(observed$misc)


  # -------------------------------------------------------------------------- #
  # Column numbers as item ids
  observed <- ws_extract_data(x, examinee_id_var = "ex_id",
                              items = c("Item_1", "Item_9", "Item_5"))
  expect_equal(observed$examinee_id, x$ex_id)
  x[, match(c("Item_1", "Item_8", "Item_5"), colnames(x))]
  expect_equal(observed$resp, x[c("Item_1", "Item_8", "Item_5")])
  expect_null(observed$misc)

})


############################################################################@###
############################# ws_create_data_file ##########################@###
############################################################################@###


test_that("ws_create_data_file", {
  skip_on_cran()
  skip_if(skip_tests)

  # -------------------------------------------------------------------------- #
  n_examinee <- 10
  resp_data <- as.data.frame(sim_resp(ip = generate_ip(n = 8),
                                      theta = rnorm(n_examinee),
                                      prop_missing = .3))
  resp_data[, 1] <- 1
  x <- cbind(ex_id = rownames(resp_data),
             gender = sample(c("Male", "Female"), n_examinee, TRUE),
             grade = sample(6:8, n_examinee, TRUE),
             resp_data)
  observed <- ws_create_data_file(x, target_dir = "c://temp",
                                  data_fn = "data_temp_delete.txt",
                                  examinee_id_var = "ex_id",
                                  additional_vars = c("gender", "grade"))
  expect_equal(observed$examinee_id, x$ex_id)
  expect_equal(observed$resp, resp_data)
  expect_equal(observed$misc, x[, c("gender", "grade")])
  # Make sure function output has the following list elements
  expect_true(all(c("examinee_id", "resp", "misc") %in% names(observed)))

  # -------------------------------------------------------------------------- #
})



############################################################################@###
############################# est_winsteps #################################@###
############################################################################@###

test_that("est_winsteps", {
  skip_on_cran()
  skip_if(skip_tests)

  # -------------------------------------------------------------------------- #
  n_examinee <- 10
  resp_data <- as.data.frame(sim_resp(ip = generate_ip(n = 8),
                                      theta = rnorm(n_examinee),
                                      prop_missing = .3))
  resp_data[, 1] <- 1
  x <- cbind(ex_id = rownames(resp_data),
             gender = sample(c("Male", "Female"), n_examinee, TRUE),
             grade = sample(6:8, n_examinee, TRUE),
             resp_data)

  target_dir <- "c://temp/est_winsteps"
  analysis_name <- "winsteps_analysis"
  items <- NULL
  examinee_id_var <- "ex_id"
  additional_vars <- c("gender", "grade")
  anchor_info <- NULL
  overwrite <- TRUE
  winsteps_exe_folder <- file.path("C:/Winsteps")

  output <- est_winsteps(x = x, target_dir = target_dir,
                         analysis_name = analysis_name, items = items,
                         examinee_id_var = examinee_id_var,
                         additional_vars = additional_vars,
                         anchor_info = anchor_info,
                         overwrite = overwrite,
                         winsteps_exe_folder = winsteps_exe_folder
                         )



  # -------------------------------------------------------------------------- #
  # Check whether anchor items work
  n_examinee <- 200
  n_item <- 16
  resp_data <- as.data.frame(sim_resp(ip = generate_ip(n = n_item),
                                      theta = rnorm(n_examinee),
                                      prop_missing = .3))
  x <- cbind(ex_id = rownames(resp_data),
             gender = sample(c("Male", "Female"), n_examinee, TRUE),
             grade = sample(6:8, n_examinee, TRUE),
             resp_data)

  target_dir <- "c://temp/est_winsteps"
  analysis_name <- "winsteps_analysis_anchor"
  items <- colnames(resp_data)
  examinee_id_var <- "ex_id"
  additional_vars <- c("gender", "grade")
  overwrite <- TRUE
  winsteps_exe_folder <- file.path("C:/Winsteps")

  anchor_info <- data.frame(item_id = items[sample(1:n_item, 3)],
                            b = c(-1, 0, 1))

  output <- irt:::est_winsteps(x = x,
                               target_dir = target_dir,
                               analysis_name = analysis_name, items = items,
                               examinee_id_var = examinee_id_var,
                               additional_vars = additional_vars,
                               anchor_info = anchor_info,
                               overwrite = overwrite,
                               winsteps_exe_folder = winsteps_exe_folder
                               )
  expect_equal(unname(output$ip$b[items == anchor_info$item_id[1]]), -1)
  expect_equal(unname(output$ip$b[items == anchor_info$item_id[2]]), 0)
  expect_equal(unname(output$ip$b[items == anchor_info$item_id[3]]), 1)


})

