# library(testthat)

skip_tests <- FALSE

###############################################################################@
############################# ws_create_anchor_file ############################
###############################################################################@
test_that("ws_create_anchor_file", {
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
                              items = c("Item_1", "Item_8", "Item_5"))
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
  rownames(resp_data) <- paste0("S", sample(
    c(1:10, 20:30, 100:110, 2200:2210, 33300:33310, 444440:444450),
    nrow(resp_data), replace = FALSE))
  x <- cbind(ex_id = rownames(resp_data),
             gender = sample(c("Male", "Female"), n_examinee, TRUE),
             grade = sample(6:8, n_examinee, TRUE),
             resp_data)
  observed <- ws_create_data_file(x,
                                  target_dir = "c://temp//est_winsteps",
                                  data_fn = "data_temp_delete.txt",
                                  examinee_id_var = "ex_id",
                                  additional_vars = c("gender", "grade"))
  expect_equal(unname(observed$person_vars["examinee_id"]),
               max(nchar(x$ex_id)))
  expect_equal(observed$item_ids, colnames(resp_data))
  expect_false(observed$polytomous)
  # expect_equal(observed$examinee_id, x$ex_id)
  # expect_equal(observed$resp, resp_data)
  # expect_equal(observed$misc, x[, c("gender", "grade")])
  # # Make sure function output has the following list elements
  # expect_true(all(c("examinee_id", "resp", "misc") %in% names(observed)))


  # -------------------------------------------------------------------------- #
  # Master's Partial Credit Model
  n_examinee <- 400
  ip <- generate_ip(model = "PCM", n = 20, n_categories = sample(3:4, 20, T))
  resp_data <- as.data.frame(sim_resp(ip = ip,
                                      theta = rnorm(n_examinee),
                                      prop_missing = .3))
  x <- cbind(ex_id = rownames(resp_data),
             gender = sample(c("Male", "Female"), n_examinee, TRUE),
             grade = sample(6:8, n_examinee, TRUE),
             resp_data)
  observed <- ws_create_data_file(x,
                                  target_dir = "c://temp//est_winsteps",
                                  data_fn = "data_temp_delete.txt",
                                  examinee_id_var = "ex_id",
                                  additional_vars = c("gender", "grade"))
  expect_false(is.null(observed$polytomous))
  expect_true(observed$polytomous)


})


############################################################################@###
############################# ws_create_control_file #######################@###
############################################################################@###

test_that("ws_create_control_file", {
  skip_on_cran()
  skip_if(skip_tests)

  # -------------------------------------------------------------------------- #
  target_dir <- "c://temp/est_winsteps"
  analysis_name <- "winsteps_analysis_ttfn"
  data_fn <- paste0(analysis_name, "_data.txt")
  item_fn <- paste0(analysis_name, "_item_pars.txt")
  person_fn <- paste0(analysis_name, "_person_pars.txt")
  control_fn <- paste0("test_control_file.txt")
  output_fn <- paste0(analysis_name, "_output_file.txt")
  batch_fn <- paste0(analysis_name, "_batch_file.bat")
  title <- "winsteps_analysis_tffn"
  item_ids <- paste0("itmn", 1:5)
  custom_args <-  c("TOTALSCORE=YES ; ",
                    "UDECIMALS=4 ",
                    "PTBISERIAL=YES ; ",
                    "PVALUE=YES ; ")

  output <- ws_create_control_file(
    data_fn = data_fn,
    item_fn = item_fn,
    person_fn = person_fn,
    anchor_fn = NULL,
    target_dir = target_dir,
    control_fn = control_fn,
    title = title,
    item_ids = item_ids,
    polytomous = FALSE,
    custom_args = custom_args,
    item_start_col = 1,
    num_of_items = 0,
    person_start_col = 0,
    person_text_len = 0,
    person_vars = NULL,
    examinee_id_width = 0,
    max_resp_width = 0,
    resp_codes = c("0", "1")
    )

  output_fn <- file.path(target_dir, control_fn)
  expect_true(file.exists(output_fn))
  output <- readLines(output_fn)
  expect_true(any(grepl("itmn4", output)))
  expect_true(any(grepl(title, output)))
  expect_true(any(grepl("TITLE", output)))
  expect_true(any(grepl(data_fn, output)))
  expect_true(any(grepl(custom_args[1], output)))
  expect_true(any(grepl(custom_args[2], output)))
  expect_true(any(grepl(custom_args[3], output)))
  expect_true(any(grepl(custom_args[4], output)))

  expect_false(any(grepl("models", output, ignore.case = FALSE)))
  expect_false(any(grepl("isgroups", output, ignore.case = FALSE)))

  # -------------------------------------------------------------------------- #
  # Polytomous items
  is_polytomous <- TRUE
  output <- ws_create_control_file(
    data_fn = data_fn,
    item_fn = item_fn,
    person_fn = person_fn,
    anchor_fn = NULL,
    target_dir = target_dir,
    control_fn = control_fn,
    title = title,
    item_ids = item_ids,
    polytomous = is_polytomous,
    custom_args = custom_args,
    item_start_col = 1,
    num_of_items = 0,
    person_start_col = 0,
    person_text_len = 0,
    person_vars = NULL,
    examinee_id_width = 0,
    max_resp_width = 0,
    resp_codes = c("0", "1")
    )

  output_fn <- file.path(target_dir, control_fn)
  expect_true(file.exists(output_fn))
  output <- readLines(output_fn)
  expect_true(any(grepl("models", output, ignore.case = TRUE)))
  expect_true(any(grepl("isgroups", output, ignore.case = TRUE)))

  # -------------------------------------------------------------------------- #
  # Acceptable and unacceptable 'ISGROUPS=', 'GROUPS=', 'MODELS=' values
  custom_args <- c(
    "abc", " DIGits = 12; models = 1", " DIGits = 12; MODELS = 1",
    " models = xx", "models=R", " models   =   3",  "models  =R",
    " MODELS = xx", "MODeLS=R", " moDELS   =   3",  "Models  =R",
    " groups= 0", "groups=1", " groups   =   3",  "groups  =0",
    " GROUPS = 3", "GrOups=0", " GRoUPS   =   0",  "Groups  =0",
    " isgroups= 0", "isgroups=1", " isgroups   =   3",  "isgroups  =0",
    " ISGROUPS = 3", "isGroups=0", " IsGRoUPS   =   0",  "isGroups  =0",
    "isgroups=33")
  temp <- data.frame(
    custom_args,
    models = grepl("^\\s*models\\s*=", custom_args, ignore.case = TRUE),
    groups = grepl("^\\s*groups\\s*=", custom_args, ignore.case = TRUE),
    isgroups = grepl("^\\s*isgroups\\s*=", custom_args, ignore.case = TRUE)
             )

  # -------------------------------------------------------------------------- #

})



############################################################################@###
############################# ws_read_isfile ###############################@###
############################################################################@###

test_that("ws_read_isfile", {
  skip_on_cran()
  skip_if(skip_tests)

  # -------------------------------------------------------------------------- #
  # GRM
  ip_df <- data.frame(
    item_id = c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5", "Item_6",
                "Item_7", "Item_8", "Item_9", "Item_10", "Item_11", "Item_12",
                "Item_13", "Item_14", "Item_15", "Item_16", "Item_17",
                "Item_18", "Item_19", "Item_20"),
    model = "PCM",
    b1 = c(-0.2784, -0.7555, -0.6188, 0.0101, -0.5801, 0.0011,
           -0.8275, -0.3771, -0.3028, -0.2172, -0.5394, -1.149, -0.2376,
           -1.2081, -0.0443, -1.0072, 0.3155, 0.0494, -1.0462, -0.8416),
    b2 = c(0.5055, 0.5433, 0.3677, 0.7889, 0.0291, 0.4941, -0.4814,
           0.0103, 0.1414, 0.5303, 0.1295, -0.1308, 0.9833, 0.2237,
           1.4929, 0.497, 0.888, 0.9017, 0.331, -0.0421),
    b3 = c(0.9097, NA, NA, NA, 0.989, 1.1455, NA, 1.1626, 1.9915, NA, NA, NA,
           NA, 0.6789, NA, NA, NA, NA, NA, 2.1166))


  output <- c("; ITEM STRUCTURE FILE (not for anchoring: use SFILE=) FOR Winsteps Analysis May 29 10:34 2024",
";ENTRY STAT  MAX  CAT BOT+.25  CAT STRU MEASURE   ERROR CAT-0.5  AT CAT  50%PRB  CAT STRU MEASURE   ERROR CAT-0.5  AT CAT  50%PRB  CAT STRU MEASURE   ERROR CAT-0.5 TOP-.25  50%PRB",
"     1    1    3    0   -1.87    1    1    -.53     .16   -1.12    -.29    -.83    2    2     .39     .16     .31     .90     .33    3    3    1.01     .20    1.69    2.40    1.37",
"     2    1    2    0   -2.28    1    1   -1.06     .15   -1.40    -.22   -1.21    2    2     .61     .15     .95    1.83     .76    0    0     .00     .00     .00     .00     .00",
"     3    1    2    0   -2.05    1    1    -.74     .15   -1.26    -.26    -.99    2    2     .22     .15     .74    1.54     .48    0    0     .00     .00     .00     .00     .00",
"     4    1    2    0   -1.41    1    1    -.03     .15    -.67     .25    -.37    2    2     .52     .17    1.17    1.91     .86    0    0     .00     .00     .00     .00     .00",
"     5    1    3    0   -2.10    1    1    -.79     .16   -1.33    -.47   -1.05    2    2     .25     .16     .16     .77     .19    3    3     .95     .19    1.59    2.32    1.28",
"     6    1    3    0   -1.54    1    1    -.13     .16    -.84    -.08    -.51    2    2     .42     .17     .47    1.03     .46    3    3    1.15     .21    1.81    2.52    1.49",
"     7    1    2    0   -2.58    1    1   -1.19     .17   -1.83    -.91   -1.53    2    2    -.64     .14     .01     .75    -.29    0    0     .00     .00     .00     .00     .00",
"     8    1    3    0   -1.99    1    1    -.56     .15   -1.28    -.47    -.95    2    2    -.13     .15     .16     .86     .07    3    3    1.46     .20    1.85    2.69    1.63",
"     9    1    3    0   -1.86    1    1    -.44     .15   -1.15    -.34    -.82    2    2     .01     .16     .31    1.01     .21    3    3    1.62     .21    2.01    2.86    1.80",
"    10    1    2    0   -2.04    1    1    -.72     .15   -1.25    -.26    -.99    2    2     .20     .15     .73    1.52     .47    0    0     .00     .00     .00     .00     .00",
"    11    1    2    0   -2.09    1    1    -.76     .15   -1.30    -.32   -1.03    2    2     .11     .15     .66    1.44     .39    0    0     .00     .00     .00     .00     .00",
"    12    1    2    0   -2.82    1    1   -1.59     .17   -1.95    -.80   -1.75    2    2     .00     .14     .35    1.23     .16    0    0     .00     .00     .00     .00     .00",
"    13    1    2    0   -1.53    1    1    -.24     .14    -.72     .32    -.47    2    2     .87     .17    1.35    2.16    1.10    0    0     .00     .00     .00     .00     .00",
"    14    1    3    0   -2.83    1    1   -1.63     .18   -1.94    -.87   -1.76    2    2     .26     .15    -.11     .56     .01    3    3     .67     .17    1.39    2.09    1.06",
"    15    1    2    0   -1.38    1    1    -.10     .14    -.56     .49    -.32    2    2    1.08     .18    1.54    2.36    1.30    0    0     .00     .00     .00     .00     .00",
"    16    1    2    0   -2.46    1    1   -1.23     .16   -1.60    -.46   -1.40    2    2     .31     .15     .68    1.55     .48    0    0     .00     .00     .00     .00     .00",
"    17    1    2    0   -1.15    1    1     .26     .14    -.42     .48    -.11    2    2     .69     .17    1.38    2.10    1.06    0    0     .00     .00     .00     .00     .00",
"    18    1    2    0   -1.56    1    1    -.28     .14    -.74     .32    -.50    2    2     .91     .17    1.37    2.19    1.13    0    0     .00     .00     .00     .00     .00",
"    19    1    2    0   -2.26    1    1    -.99     .15   -1.43    -.37   -1.20    2    2     .24     .15     .69    1.52     .46    0    0     .00     .00     .00     .00     .00",
"    20    1    3    0   -2.32    1    1    -.94     .16   -1.57    -.67   -1.27    2    2    -.31     .14     .15    1.22    -.04    3    3    2.56     .25    2.70    3.70    2.61"
)




  # -------------------------------------------------------------------------- #

})


############################################################################@###
############################# est_winsteps #################################@###
############################################################################@###

test_that("est_winsteps", {
  skip_on_cran()
  skip_if(skip_tests)

  # -------------------------------------------------------------------------- #
  # Dichotomous items
  n_examinee <- 200
  resp_data <- as.data.frame(sim_resp(ip = generate_ip(n = 8),
                                      theta = rnorm(n_examinee),
                                      prop_missing = .3))
  resp_data[, 1] <- 1
  x <- cbind(ex_id = rownames(resp_data),
             gender = sample(c("Male", "Female"), n_examinee, TRUE),
             grade = sample(6:8, n_examinee, TRUE),
             resp_data)

  target_dir <- "c://temp/est_winsteps/dichotomous"
  analysis_name <- "winsteps_analysis"
  items <- NULL
  examinee_id_var <- "ex_id"
  additional_vars <- c("gender", "grade")
  custom_args = c("TOTALSCORE=YES ; Include extreme responses in reported scores",
                  "UDECIMALS=4 ; Number of decimal places reported",
                  "PTBISERIAL=YES ; Raw score point-biserial excluding extremes",
                  "PVALUE=YES ; report proportion-correct-values")
  anchor_info <- NULL
  overwrite <- TRUE
  verbose <- TRUE
  read_person_pars <- TRUE
  winsteps_exe_folder <- file.path("C:/Winsteps")

  output <- est_winsteps(x = x,
                         target_dir = target_dir,
                         analysis_name = analysis_name,
                         items = items,
                         examinee_id_var = examinee_id_var,
                         additional_vars = additional_vars,
                         anchor_info = anchor_info,
                         overwrite = overwrite,
                         winsteps_exe_folder = winsteps_exe_folder
                         )

  expect_type(output, "list")
  expect_s4_class(output$ip, "Itempool")
  expect_false(is.null(output$raw_person_pars))
  expect_false(is.null(output$raw_item_pars))

  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_item_pars.txt"))))
  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_person_pars.txt"))))
  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_control_file.txt"))))
  expect_false(file.exists(
    file.path(target_dir, paste0(analysis_name, "_item_structure.txt"))))

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

  target_dir <- "c://temp/est_winsteps/anchor"
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

  # -------------------------------------------------------------------------- #
  # Master's Partial Credit Model
  n_examinee <- 1400
  ip <- generate_ip(model = "PCM", n = 20, n_categories = sample(3:4, 20, T))
  resp_data <- as.data.frame(sim_resp(ip = ip,
                                      theta = rnorm(n_examinee),
                                      prop_missing = .3))
  x <- cbind(ex_id = rownames(resp_data),
             gender = sample(c("Male", "Female"), n_examinee, TRUE),
             grade = sample(6:8, n_examinee, TRUE),
             resp_data)

  target_dir <- "c://temp/est_winsteps/grm"
  analysis_name <- "winsteps_analysis_grm"
  items <- colnames(resp_data)
  examinee_id_var <- "ex_id"
  additional_vars <- c("gender", "grade")
  overwrite <- TRUE
  overwrite <- TRUE
  verbose <- TRUE
  custom_args = c("TOTALSCORE=YES ; Include extreme responses in reported scores",
                  "UDECIMALS=4 ; Number of decimal places reported",
                  # "CSV=YES",
                  # "SFILE = abc.csv",
                  "PTBISERIAL=YES ; Raw score point-biserial excluding extremes",
                  "PVALUE=YES ; report proportion-correct-values")
  read_person_pars <- TRUE
  winsteps_exe_folder <- file.path("C:/Winsteps")
  anchor_info <- NULL
  # custom_args <- c("ISFILE = winsteps_analysis_grm_isf.isf")

  output <- irt:::est_winsteps(x = x,
                               target_dir = target_dir,
                               analysis_name = analysis_name, items = items,
                               examinee_id_var = examinee_id_var,
                               additional_vars = additional_vars,
                               anchor_info = anchor_info,
                               custom_args = custom_args,
                               overwrite = overwrite,
                               winsteps_exe_folder = winsteps_exe_folder
                               )

  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_item_pars.csv"))))
  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_person_pars.csv"))))
  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_control_file.txt"))))
  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_item_structure.csv"))))


  # 'ws_read_isfile()' function works as expected
  output_isfile <- ws_read_isfile_csv(
    target_dir = target_dir,
    isfile_fn = paste0(analysis_name, "_item_structure.csv"),
    item_pars = output$raw_item_pars,
    polytomous_model = "PCM")

  expect_equal(unname(which(is.na(ip$b3))),
               unname(which(is.na(output_isfile$b3))))
  expect_equal(colnames(output_isfile), c("ENTRY", "MAX", "b1", "b2", "b3",
                                          "model"))

  # PCM
  output_isfile <- ws_read_isfile_csv(
    target_dir = target_dir,
    isfile_fn = paste0(analysis_name, "_item_structure.csv"),
    item_pars = output$raw_item_pars,
    polytomous_model = "GPCM2")

  expect_equal(unname(which(is.na(ip$b3))),
               unname(which(is.na(output_isfile$d3))))
  expect_equal(colnames(output_isfile), c("ENTRY", "MAX", "d1", "d2", "d3",
                                          "b", "D", "a", "model"))



  # -------------------------------------------------------------------------- #
  # Mixture of Rasch and Partial Credit Model items
  n_examinee <- 1600
  ip <- c(generate_ip(model = "PCM", n = 10, n_categories = sample(3:4, 10, T),
                      item_id = paste0("PCM", 1:10)),
          generate_ip(model = "Rasch", n = 5, item_id = paste0("Rasch", 1:5)))
  resp_data <- as.data.frame(sim_resp(ip = ip,
                                      theta = rnorm(n_examinee),
                                      prop_missing = .3))
  x <- cbind(ex_id = rownames(resp_data),
             gender = sample(c("Male", "Female"), n_examinee, TRUE),
             grade = sample(6:8, n_examinee, TRUE),
             resp_data)

  target_dir <- "c://temp/est_winsteps/mixed"
  analysis_name <- "winsteps_analysis_mixed"
  items <- colnames(resp_data)
  examinee_id_var <- "ex_id"
  additional_vars <- c("gender", "grade")
  overwrite <- TRUE
  winsteps_exe_folder <- file.path("C:/Winsteps")
  anchor_info <- NULL

  custom_args = c("TOTALSCORE=YES ; Include extreme responses in reported scores",
                  "UDECIMALS=4 ; Number of decimal places reported",
                  "PTBISERIAL=YES ; Raw score point-biserial excluding extremes",
                  "PVALUE=YES ; report proportion-correct-values",
                  # "CSV=YES",
                  # "SFILE = abc.csv",
                  "SCOREFILE = scr_file.csv",
                  "IDFILE = *",
                  "1",
                  "2",
                  "3",
                  "*",
                  "SCOREFILE = scr_file_2.csv",
                  # "IDFILE = *",
                  # "1",
                  # "2",
                  # "3",
                  # "4",
                  # "5",
                  # "6",
                  # "7",
                  # "*",
                  # "SCFILE = scr_file_3.csv",
                  ""
                  )


  output <- irt:::est_winsteps(x = x,
                               target_dir = target_dir,
                               analysis_name = analysis_name, items = items,
                               examinee_id_var = examinee_id_var,
                               additional_vars = additional_vars,
                               anchor_info = anchor_info,
                               custom_args = custom_args,
                               overwrite = overwrite,
                               winsteps_exe_folder = winsteps_exe_folder
                               )

  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_item_pars.csv"))))
  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_person_pars.csv"))))
  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_control_file.txt"))))
  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_item_structure.csv"))))

  expect_s3_class(object = output$raw_isfile, "data.frame")
  expect_s3_class(object = output$raw_sfile, "data.frame")
  expect_s4_class(object = output$ip, "Itempool")

  # 'ws_read_isfile()' function works as expected
  output_isfile <- ws_read_isfile_csv(
    target_dir = target_dir,
    isfile_fn = paste0(analysis_name, "_item_structure.csv"),
    item_pars = output$raw_item_pars,
    polytomous_model = "GPCM2")

  expect_equal(unname(which(is.na(ip$b2))),
               unname(which(is.na(output_isfile$d2))))
  expect_equal(unname(which(is.na(ip$b3))),
               unname(which(is.na(output_isfile$d3))))
  expect_equal(colnames(output_isfile), c("ENTRY", "MAX", "d1", "d2", "d3",
                                          "b", "D", "a", "model"))

  # -------------------------------------------------------------------------- #
  # Mixture of Rasch and Partial Credit Model items with Perfect dichotomous
  # response
  n_examinee <- 1600
  ip <- c(generate_ip(model = "PCM", n = 8, n_categories = sample(3:4, 8, T),
                      item_id = paste0("PCM", 1:8)),
          generate_ip(model = "Rasch", n = 7, item_id = paste0("Rasch", 1:7)))
  resp_data <- as.data.frame(sim_resp(ip = ip,
                                      theta = rnorm(n_examinee),
                                      prop_missing = .3))
  x <- cbind(ex_id = rownames(resp_data),
             gender = sample(c("Male", "Female"), n_examinee, TRUE),
             grade = sample(6:8, n_examinee, TRUE),
             resp_data)
  x[, "Rasch1"] <- 1

  target_dir <- "c://temp/est_winsteps/mixed"
  analysis_name <- "winsteps_analysis_mixed"
  items <- colnames(resp_data)
  examinee_id_var <- "ex_id"
  additional_vars <- c("gender", "grade")
  overwrite <- TRUE
  winsteps_exe_folder <- file.path("C:/Winsteps")
  anchor_info <- NULL


  output <- irt:::est_winsteps(x = x,
                               target_dir = target_dir,
                               analysis_name = analysis_name, items = items,
                               examinee_id_var = examinee_id_var,
                               additional_vars = additional_vars,
                               anchor_info = anchor_info,
                               overwrite = overwrite,
                               winsteps_exe_folder = winsteps_exe_folder
                               )

  expect_equal(unname(output$ip$STATUS[output$ip$item_id == "Rasch1"]), -2)

  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_item_pars.csv"))))
  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_person_pars.csv"))))
  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_control_file.txt"))))
  expect_true(file.exists(
    file.path(target_dir, paste0(analysis_name, "_item_structure.csv"))))

  # 'ws_read_isfile()' function works as expected
  output_isfile <- ws_read_isfile_csv(
    target_dir = target_dir,
    isfile_fn = paste0(analysis_name, "_item_structure.csv"),
    item_pars = output$raw_item_pars,
    polytomous_model = "GPCM2")

  expect_equal(unname(which(is.na(ip$b2))),
               unname(which(is.na(output_isfile$d2))))
  expect_equal(unname(which(is.na(ip$b3))),
               unname(which(is.na(output_isfile$d3))))
  expect_equal(colnames(output_isfile), c("ENTRY", "MAX", "d1", "d2", "d3",
                                          "b", "D", "a", "model"))

})

