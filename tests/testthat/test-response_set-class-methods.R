

# library(testthat)

###############################################################################@
############################# response_set() ###################################
###############################################################################@

test_that("response_set", {

  # x = resp
  # data_format = "wide"
  # ip =  NULL
  # examinee_id_var = NULL
  # testlet_id_var = NULL
  # item_id_var = NULL
  # score_var = NULL
  # raw_response_var = NULL
  # order_var = NULL
  # response_time_var = NULL
  # misc_var = NULL
  # misc = NULL
  # misc_unique_var = NULL
  # fill_na_score = NULL

  ################# Test overall arguments #################################@###
  x_wide <- matrix(sample(0:1, 35, T), nrow = 7, ncol = 5)
  ip_x <- generate_ip(n = 7)
  x_long <- data.frame(examinee_id = c("stu1", "stu1", "stu2", "stu2"),
                  item_id = c("i1", "i2", "i1", "i2"),
                  scr = c(0, 1, 1, 0))

  # 'data_format' argument should be valid
  expect_error(response_set(x_wide, data_format = "abc"),
               "Invalid 'data_format' argument. The 'data_format' should be ")

  # 'ip' argument should be valid
  expect_error(response_set(x_wide, ip = "abc"),
               "Invalid 'ip' argument. 'ip' should be an Itempool object.")

  # 'x' argument should be valid
  expect_error(response_set(x = 12),
               "Invalid 'x' argument. 'x' should be a matrix, data.frame or")

  # A list that is not composed of "Response" elements should raise an error
  expect_error(
    response_set(x = list(a = 1, b = 2)),
    "Invalid 'x' argument. 'x' should be a list of 'Response' objects.")

  # # Invalid variable name arguments
  # expect_error(response_set(x_wide, score_var = 12, item_id_var = "item_ids"),
  #              paste0("Invalid \"", "score_var",
  #                     "\" argument. The value should be one"))
  # expect_error(response_set(x_wide, score_var = c("a", "b"),
  #                           item_id_var = "item_ids"),
  #              paste0("Invalid \"", "score_var",
  #                     "\" argument. The value should be one"))

  # expect_error(response_set(x = x_long,
  #                           score_var = "scr", item_id_var = "item_id",
  #                           examinee_id_var = "examinee_id",
  #                           order_var = "order"),
  #              paste0("Invalid \"", "order_var",
  #                     "\" argument. The value should be one"))

  # ---------------------------------------------------------------------------#
  # A NA response should raise an error.
  resp <- NA
  expect_error(response_set(x = as.data.frame(resp), ip = generate_ip()))

  # ---------------------------------------------------------------------------#
  # An all NA response string should raise an error.
  n_items <- 5
  n_examinee <- 4
  ip <- itempool(a = runif(n_items, .7, 1.5), b = runif(n_items, -2, 2))
  resp <- sim_resp(ip = ip, theta = rnorm(n_examinee, 0, .4))
  resp[1, ] <- 0
  resp[2, ] <- NA
  resp[3, sample(1:n_items, 2)] <- NA
  expect_error(response_set(x = resp, ip = ip))




  ################# Test "list" format #####################################@###


  ################# Test "response" format #################################@###
  ip <- generate_ip(n = 5)
  resp <- response(score = sample(0:1, 3, T), item_id = sample(ip$item_id, 3))
  expect_s4_class(response_set(resp, ip = ip), "Response_set")

  # ################# Test "response" format ###############################@###
  # # 'fill_missing_items = 0' fills the missing items with 0 values
  # ip <- generate_ip(n = 5)

  # ---------------------------------------------------------------------------#
  # If there are testlet items in the responses and there are testlets in the
  # itempool, then the testlet objects will be added to the response set
  # object via `response_set()` function.
  ip <- c(generate_testlet(n = 3, item_id_preamble = "t1"),
          generate_ip(n = 5),
          generate_testlet(n = 4, item_id_preamble = "t2"))
  resp <- sim_resp(ip = ip, theta = rnorm(5), output = "matrix")
  resp_set <- response_set(resp, data_format = "wide", ip = ip)
  r <- resp_set[[sample(1:length(resp_set), 1)]]
  expect_true(!is.null(r@testlet_id))
  expect_true(!is.null(r$testlet_id))

  # ---------------------------------------------------------------------------#
  # For 'long' format, if user did not supply testlet_id and if there are
  # testlet_id's in the itempool, then `response_set()` function will add
  # testlet_id's automatically.
  ip <- c(generate_testlet(n = 3, item_id_preamble = "t1"),
          generate_ip(n = 5),
          generate_testlet(n = 4, item_id_preamble = "t2"))
  resp <- sim_resp(ip = ip, theta = rnorm(5), output = "response_set")
  resp_long <- data.frame()
  for (i in 1:length(resp))
    resp_long <- rbind(resp_long, data.frame(resp[[i]]))
  resp_long$score <- as.integer(resp_long$score)
  resp_long$order <- as.integer(resp_long$order)
  # remove testlet column
  resp_long_wo_testlet <- resp_long
  resp_long_wo_testlet$testlet_id <- NULL

  resp_set <- response_set(x = resp_long_wo_testlet,
                           data_format = "long", ip = ip,
                           examinee_id_var = "examinee_id",
                           item_id_var = "item_id",
                           score_var = "score", order_var = "order")
  r <- resp_set[[sample(1:length(resp_set), 1)]]
  expect_true(!is.null(r@testlet_id))
  expect_true(!is.null(r$testlet_id))


  # ---------------------------------------------------------------------------#
  # Convert a matrix or data.frame of raw responses to a response set.
  n_items <- sample(10:15, 1)
  n_theta <- sample(20:30, 1)
  raw_resp_wide <- matrix(
    sample(LETTERS[1:5], n_items * n_theta, replace = TRUE), ncol = n_items,
    dimnames = list(sprintf("Stu-%02d", sequence(n_theta)),
                    sprintf("Itm-%02d", sequence(n_items))))

  resp_set <- response_set(raw_resp_wide)
  expect_s4_class(resp_set, "Response_set")
  i <- sample(sequence(n_theta), 1)
  j <- sample(sequence(n_items), 1)
  expect_identical(resp_set[[i]]$raw_response[j], raw_resp_wide[i, j])


  # Convert all raw response long format to Response_set
  raw_resp_long <- cbind(examinee_id = rownames(raw_resp_wide),
                         as.data.frame(raw_resp_wide))
  raw_resp_long <- reshape(raw_resp_long,
                           direction = "long",
                           varying = list(colnames(raw_resp_wide)),
                           v.names = "raw_response",
                           idvar = "examinee_id",
                           timevar = "item_id",
                           times = colnames(raw_resp_wide),
                           new.row.names = sequence(n_theta * n_items))
  resp_set <- response_set(
    x = raw_resp_long,
    data_format = "long",
    examinee_id_var = "examinee_id",
    item_id_var = "item_id",
    raw_response_var = "raw_response")
  expect_s4_class(resp_set, "Response_set")
  i <- sample(sequence(n_theta), 1)
  j <- sample(sequence(n_items), 1)
  expect_identical(resp_set[[i]]$raw_response[j], raw_resp_wide[i, j])

    # data_format = "wide"
    # ip =  NULL
    # examinee_id_var = NULL
    # testlet_id_var = NULL
    # item_id_var = NULL
    # score_var = NULL
    # raw_response_var = NULL
    # order_var = NULL
    # response_time_var = NULL
    # misc_var = NULL
    # misc_unique_var = NULL
    # misc = NULL
    # fill_na_score = NULL


  # ---------------------------------------------------------------------------#
  # convert a matrix with testlets into response set
  ip <- c(generate_testlet(n = sample(2:6, 1), item_id_preamble = "t1"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t2"),
          generate_ip(n = sample(6:12, 1)),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t3")
          )
  ip_standalone <- itempool(irt:::flatten_itempool_cpp(ip))
  true_theta <- rnorm(20)
  resp <- sim_resp(ip = ip, theta = true_theta, prop_missing = .3)
  resp_set <- response_set(resp, ip = ip)
  i <- sample(nrow(resp), 1)
  r <- resp_set[[i]]
  expect_equal(length(r@item_id), length(r@testlet_id))
  expect_equal(length(r@score), length(r@testlet_id))

})



###############################################################################@
############################# .create_response_set_from_list ###################
###############################################################################@

test_that(".create_response_set_from_list", {

  t1 <- testlet(generate_ip(n = 2, item_id = c("i1", "i5")), testlet_id = "t1")
  t2 <- testlet(generate_ip(n = 2, item_id = c("i2", "i4")), testlet_id = "t2")
  i3 <- generate_item(item_id = "i3")
  i6 <- generate_item(item_id = "i6")
  ip  <- c(t2, i6, t1, i3)
  x_long <- data.frame(exm_id = c("stu1", "stu1", "stu1", "stu1",
                                  "stu2", "stu2", "stu2", "stu2", "stu2",
                                  "stu3", "stu3", "stu3", "stu3"),
                       itm_id = c("i5", "i2", "i3", "i4",
                                  "i2", "i5", "i3", "i4", "i1",
                                  "i2", "i1", "i4", "i3"),
                       # t1: i1, i5  ;  t2: i2, i4
                       tstlt_id = c("t1", "t2", NA, "t2",
                                    "t2", "t1", NA, "t2", "t1",
                                    "t2",  "t1", "t2", NA),
                       scr = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0))

  # ---------------------------------------------------------------------------#
  # No itempool + Testlet
  rs <- response_set(x = x_long,
                     data_format = "long",
                     examinee_id_var = "exm_id",
                     item_id_var = "itm_id",
                     testlet_id_var = "tstlt_id",
                     score_var = "scr")
  expect_true(validObject(rs))
  expect_equal(names(rs@response_list), rs$examinee_id)
  rs_list <- rs@response_list
  expect_identical(.create_response_set_from_list(x = rs_list),
                   response_set(x = rs_list))
  rs_new <- response_set(x = rs_list)
  expect_true(validObject(rs_new))
  expect_identical(rs, rs_new)
  expect_identical(rs_new@item_id, c("i5", "i2", "i3", "i4", "i1"))
  expect_identical(rs_new@testlet_id, c("t1", "t2", NA, "t2", "t1"))

  # ---------------------------------------------------------------------------#
  # with item pool + Testlet
  rs_new <- response_set(x = rs_list, ip = ip)
  expect_true(validObject(rs_new))
  temp <- as.data.frame(ip)[, c("item_id", "testlet_id" )]
  temp <- temp[temp$item_id %in% x_long$itm_id, ]
  expect_identical(rs_new@item_id, temp$item_id)
  expect_identical(rs_new@testlet_id, temp$testlet_id)

  # ---------------------------------------------------------------------------#
  # No itempool + Testlet
  rs <- response_set(x = x_long[, colnames(x_long) != "tstlt_id"],
                     data_format = "long",
                     examinee_id_var = "exm_id",
                     item_id_var = "itm_id",
                     score_var = "scr")
  rs_list <- rs@response_list
  rs_new <- response_set(x = rs_list)
  expect_true(validObject(rs_new))
  expect_equal(names(rs_new@response_list), rs_new$examinee_id)
  expect_identical(rs, rs_new)
  expect_identical(rs_new@item_id, c("i5", "i2", "i3", "i4", "i1"))
  expect_null(rs_new@testlet_id)

  # ---------------------------------------------------------------------------#
  # itempool + No Testlet in the response_set list
  # Even though there are no testlet info In the original response list, when
  # the response_list is provided with the item pool with testlet info,
  # it will automatically add the testlets to the new Reponse_set.
  rs <- response_set(x = x_long[, colnames(x_long) != "tstlt_id"],
                     data_format = "long",
                     examinee_id_var = "exm_id",
                     item_id_var = "itm_id",
                     score_var = "scr")
  expect_true(validObject(rs))
  expect_null(rs@testlet_id)
  rs_list <- rs@response_list
  rs_new <- response_set(x = rs_list, ip = ip)
  expect_true(validObject(rs_new))
  temp <- as.data.frame(ip)[, c("item_id", "testlet_id" )]
  temp <- temp[temp$item_id %in% x_long$itm_id, ]
  expect_identical(rs_new@item_id, temp$item_id)
  expect_identical(rs_new@testlet_id, temp$testlet_id)

  # ---------------------------------------------------------------------------#
  # Function can attach an Itempool object:
  rs_list <- rs@response_list
  ip <- generate_ip(n = length(unique(x_long$itm_id)),
                    item_id = unique(x_long$itm_id))
  rs <- response_set(x = rs_list, ip = ip)
  expect_s4_class(rs, "Response_set")
  expect_true(validObject(rs))

  # ---------------------------------------------------------------------------#
  # All of the item_id's should be in the ip, i.e. Itempool
  ip <- generate_ip(n = length(unique(x_long$itm_id)) - 1,
                    item_id = unique(x_long$itm_id)[-1])
  expect_error(response_set(x = rs_list, ip = ip),
               "Invalid 'item_id'. All of the 'item_id's should be in 'ip'.")

  # ---------------------------------------------------------------------------#
  # When x = "list" of Response objects and none of the Response objects have
  # examinee_id slot, examinee_id's will be populated.
  ip <- generate_ip(n = 5)
  item_ids <- sample(ip$item_id, 3)
  resp <- response(score = sample(0:1, 3, T), item_id = item_ids)
  resp_list <- lapply(1:5, function(x) resp)
  rs <- response_set(resp_list, ip = ip)
  expect_s4_class(rs, "Response_set")
  expect_true(validObject(rs))
  i <- sample(1:5, 1)
  expect_identical(rs@response_list[[i]]@examinee_id, paste0("S", i))
  # @item_id follows the item pool's order
  expected_item_ids <- item_ids[match(ip$item_id, item_ids)]
  expected_item_ids <- expected_item_ids[!is.na(expected_item_ids)]
  expect_identical(rs@item_id, expected_item_ids)
  expect_null(rs@testlet_id)
  expect_equal(names(rs@response_list), rs$examinee_id)

  # ---------------------------------------------------------------------------#
  # When x = "list" of Response objects and only some of the Response objects
  # have non-NULL examinee_id slot, examinee_id's will be populated.
  ip <- generate_ip(n = 5)
  resp <- response(score = sample(0:1, 3, T), item_id = sample(ip$item_id, 3))
  resp_list <- lapply(1:5, function(x) resp)
  resp_list[[2]]@examinee_id <- "Ex-222"
  rs <- response_set(resp_list, ip = ip)
  expect_true(validObject(rs))
  expect_identical(rs@response_list[[3]]@examinee_id, "S2")
  expect_equal(names(rs@response_list), rs$examinee_id)

  # ---------------------------------------------------------------------------#
  # When x = "list" of Response objects and all of the Response objects
  # have non-NULL examinee_id slot, examinee_id's will be assigned properly.
  ip <- generate_ip(n = 5)
  resp <- response(score = sample(0:1, 3, T), item_id = sample(ip$item_id, 3))
  resp_list <- lapply(1:5, function(x) {
    resp@examinee_id = paste0("Ex-", x);
    resp})
  rs <- response_set(resp_list, ip = ip)
  expect_true(validObject(rs))
  expect_equal(names(rs@response_list), rs$examinee_id)
  i <- sample(1:5, 1)
  expect_identical(rs@response_list[[i]]@examinee_id, paste0("Ex-", i))
})

###############################################################################@
############################# .create_response_set_from_wide_format ############
###############################################################################@

test_that("create_response_set_from_wide_format", {

  ################# Test "matrix" format ###################################@###
  ip <- generate_ip(n = 10)
  theta <- rnorm(14)
  resp <- sim_resp(theta = theta, ip = ip, output = "response_set")
  resp_matrix <- as.matrix(resp)
  new_resp <- response_set(resp_matrix)
  expect_true(validObject(new_resp))
  for (i in sample(1:14, 3))
    for (j in sample(1:10, 3))
      expect_identical(new_resp@response_list[[i]]@score[j],
                       # setNames(resp_matrix[i, j], paste0("Item_", j)))
                       resp_matrix[i, j])
  expect_identical(new_resp@item_id, colnames(resp_matrix))
  expect_null(new_resp@testlet_id)
  expect_null(new_resp@misc)
  expect_equal(names(new_resp@response_list), new_resp$examinee_id)

  # ---------------------------------------------------------------------------#
  # Warn when user tries to use a x = matrix with data_format = "long"
  expect_error(response_set(resp_matrix, data_format = "long"),
               paste0("Invalid 'data_format'. Please use 'data_format = ",
                      "\"wide\"' when 'x' argument is a 'matrix'."))

  # ---------------------------------------------------------------------------#
  # 'fill_na_score' works as expected when 'data_format' = "wide"
  ip <- generate_ip(n = 5)
  resp <- sim_resp(theta = rnorm(3), ip = ip, output = "matrix")
  resp[1, "Item_1"] <- NA
  resp[2, "Item_2"] <- NA
  resp[3, "Item_3"] <- NA

  rs <- response_set(x = resp, data_format = "wide", fill_na_score = NULL)
  expect_true(validObject(rs))
  expect_identical(rs@item_id, colnames(resp))
  expect_null(rs@testlet_id)
  expect_null(rs@misc)
  expect_equal(names(rs@response_list), rs$examinee_id)

  expect_false("Item_1" %in% rs[[1]]$item_id)
  expect_false("Item_2" %in% rs[[2]]$item_id)
  expect_false("Item_3" %in% rs[[3]]$item_id)

  # Without item pool
  rs <- response_set(x = resp, data_format = "wide", fill_na_score = 0)
  expect_true(validObject(rs))
  expect_true("Item_1" %in% rs[[1]]$item_id)
  expect_true("Item_2" %in% rs[[2]]$item_id)
  expect_true("Item_3" %in% rs[[3]]$item_id)
  expect_identical(rs[[1]]$score[rs[[1]]$item_id == "Item_1"], 0)
  expect_identical(rs[[2]]$score[rs[[2]]$item_id == "Item_2"], 0)
  expect_identical(rs[[3]]$score[rs[[3]]$item_id == "Item_3"], 0)
  expect_equal(names(rs@response_list), rs$examinee_id)

  expect_false("Item_6" %in% rs[[1]]$item_id)

  expect_identical(rs@item_id, colnames(resp))
  expect_null(rs@testlet_id)
  expect_null(rs@misc)

  # With item pool
  ip <- c(ip, generate_item(), generate_item())
  rs <- response_set(x = resp, data_format = "wide", ip = ip,
                     fill_na_score = 0)
  expect_true(validObject(rs))
  expect_true("Item_1" %in% rs[[1]]$item_id)
  expect_true("Item_2" %in% rs[[2]]$item_id)
  expect_true("Item_3" %in% rs[[3]]$item_id)
  expect_identical(rs[[1]]$score[rs[[1]]$item_id == "Item_1"], 0)
  expect_identical(rs[[2]]$score[rs[[2]]$item_id == "Item_2"], 0)
  expect_identical(rs[[3]]$score[rs[[3]]$item_id == "Item_3"], 0)
  expect_equal(names(rs@response_list), rs$examinee_id)

  expect_true("Item_6" %in% rs[[1]]$item_id)
  expect_identical(rs[[1]]$score[rs[[1]]$item_id == "Item_6"], 0)

  expect_identical(rs@item_id, ip$item_id)
  expect_null(rs@testlet_id)
  expect_null(rs@misc)

  # ---------------------------------------------------------------------------#
  # The order of 'item_id' slot is correct
  ip <- generate_ip(n = 10)
  resp <- sim_resp(theta = rnorm(3), ip = ip, output = "matrix")
  expected_item_ids <- sample(colnames(resp))
  resp <- resp[, expected_item_ids]
  rs <- response_set(x = resp, data_format = "wide")
  expect_true(validObject(rs))
  expect_identical(rs@item_id, expected_item_ids)
  expect_equal(names(rs@response_list), rs$examinee_id)

  # ---------------------------------------------------------------------------#
  # The order of 'item_id' slot is correct when 'ip' present
  ip <- generate_ip(n = 10)
  resp <- sim_resp(theta = rnorm(3), ip = ip, output = "matrix")
  expected_item_ids <- sample(colnames(resp))
  resp <- resp[, expected_item_ids]
  rs <- response_set(x = resp, data_format = "wide", ip = ip)
  expect_true(validObject(rs))
  expect_identical(rs@item_id, expected_item_ids)
  expect_equal(names(rs@response_list), rs$examinee_id)

  # ---------------------------------------------------------------------------#
  # If there are testlets in the item pool, that information will be added to
  # the 'testlet_id' slot.
  ip <- c(generate_item(),
          do.call("c", lapply(paste0("t", 1:4), function(x)
            generate_testlet(testlet_id = x))),
          generate_item())
  resp <- sim_resp(theta = rnorm(3), ip = ip, output = "matrix")
  rs <- response_set(x = resp, data_format = "wide", ip = ip)
  expect_true(validObject(rs))
  expect_identical(rs@item_id, colnames(resp))
  expect_identical(rs@testlet_id, ip$testlet_id)
  expect_equal(names(rs@response_list), rs$examinee_id)

  # ---------------------------------------------------------------------------#
  # If there are testlets in the item pool and testlet items are scattered
  # in the response matrix, that information will be added to the 'testlet_id'
  # slot.
  ip <- c(generate_item(),
          do.call("c", lapply(paste0("t", 1:3), function(x)
            generate_testlet(testlet_id = x))),
          generate_item())
  resp <- sim_resp(theta = rnorm(3), ip = ip, output = "matrix")
  expected_item_ids <- sample(colnames(resp))
  resp <- resp[, expected_item_ids]
  rs <- response_set(x = resp, data_format = "wide", ip = ip)
  expect_true(validObject(rs))
  expected_testlet_ids <- ip$testlet_id[match(colnames(resp), ip$item_id)]
  expect_identical(rs@item_id, colnames(resp))
  expect_identical(rs@testlet_id, expected_testlet_ids)
  expect_equal(names(rs@response_list), rs$examinee_id)

  # ---------------------------------------------------------------------------#
  # If there are testlets in the item pool and testlet items are scattered
  # in the response matrix, that information will be added to the 'testlet_id'
  # slot even if some items in item pool is not in response set.
  ip <- c(generate_item(),
          do.call("c", lapply(paste0("t", 1:3), function(x)
            generate_testlet(testlet_id = x))),
          generate_ip(n = 3))
  resp <- sim_resp(theta = rnorm(3), ip = ip, output = "matrix")
  # Remove one item
  resp <- resp[, -(ncol(resp) - 1)]

  expected_item_ids <- sample(colnames(resp))
  resp <- resp[, expected_item_ids]
  rs <- response_set(x = resp, data_format = "wide", ip = ip)
  expect_true(validObject(rs))
  expected_testlet_ids <- ip$testlet_id[match(colnames(resp), ip$item_id)]
  expect_identical(rs@item_id, colnames(resp))
  expect_identical(rs@testlet_id, expected_testlet_ids)
  expect_equal(names(rs@response_list), rs$examinee_id)


  ################# Test "wide" format #####################################@###
  x_wide <- matrix(sample(0:1, 35, TRUE), nrow = 7, ncol = 5)
  rs <- response_set(x_wide)
  expect_s4_class(rs, "Response_set")
  expect_equal(names(rs@response_list), rs$examinee_id)


  # ---------------------------------------------------------------------------#
  # Column names of x and ip ID's should match
  ip <- generate_ip(n = 10)
  resp_matrix <- matrix(sample(0:1, 140, T), nrow = 14,
                        dimnames = list(NULL, paste0("wi_", 1:10)))
  expect_error(response_set(resp_matrix, ip = ip),
               paste0("Invalid 'ip'. All of the items in the response ",
                      "data should be in the item pool, ip."))

  # ---------------------------------------------------------------------------#
  # Raise error when there are no responses (i.e., all missing) in a row.
  ip <- generate_ip()
  n_examinee <- sample(5:20, 1)
  examinee_ids <- sample(paste0("Ex-", 1:n_examinee))
  resp_matrix <- as.matrix(generate_resp_set(
    ip = ip, theta = setNames(rnorm(n_examinee), examinee_ids),
    prop_missing = .2))

  i <- sample(1:n_examinee, 1)
  resp_matrix[i, ] <- NA
  expect_error(response_set(x = resp_matrix, data_format = "wide"),
               paste0("There are no valid responses in row ", i))


  # ---------------------------------------------------------------------------#
  # When data_format = "wide" the examinee_id's cannot be duplicated.


  # ---------------------------------------------------------------------------#
})


###############################################################################@
############################# .create_response_set_from_long_format ############
###############################################################################@

test_that(".create_response_set_from_long_format", {

  ################# Test "long" format #####################################@###
  x_long <- data.frame(exm_id = c("stu1", "stu1", "stu1", "stu2", "stu2"),
                       itm_id = c("i1", "i2", "i4", "i1", "i2"),
                       scr = c(0, 1, 0, 1, 0),
                       rwscore = c("A", "D", "B", "C", "D"),
                       resptime = c(33, 55, 22, 66, 31),
                       # These will be passed to misc
                       item_type = c("MC", "MC", "MS", "SA", "MC"),
                       word_count = c(123, 442, 552, 342, 666),
                       # This will be ignored
                       lexile_level = c(1, 4, 3, 2, 1),
                       ability = c(1.1, 1.1, 1.1, -.2, -.2),
                       grade = c("7", "7", "7", "11", "11")
                       )


  # x = x_long
  # data_format = "long"
  # ip = NULL
  # examinee_id_var = "exm_id"
  # testlet_id_var = NULL
  # item_id_var = "itm_id"
  # score_var = "scr"
  # raw_response_var = "rwscore"
  # order_var = NULL
  # response_time_var = "resptime"
  # misc = NULL
  # misc_var = c("item_type", "lexile_level")
  # misc_unique_var = c("ability", "grade")
  # fill_na_score = NULL

  expect_s4_class(response_set(x = x_long,
                               data_format = "long",
                               examinee_id_var = "exm_id",
                               item_id_var = "itm_id",
                               score_var = "scr"), "Response_set")

  rs <- response_set(x = x_long,
                     data_format = "long",
                     examinee_id_var = "exm_id",
                     item_id_var = "itm_id",
                     score_var = "scr",
                     raw_response_var = "rwscore",
                     response_time_var = "resptime",
                     misc_var = c("item_type", "lexile_level"),
                     misc_unique_var = c("ability", "grade")
                     )
  expect_true(validObject(rs))
  expect_identical(length(rs@response_list), 2L)
  r1 <- rs@response_list[[1]]
  expect_s4_class(r1, "Response")
  expect_identical(r1@examinee_id, "stu1")
  expect_identical(r1@item_id[2], "i2")
  expect_identical(r1@score[3], 0)
  expect_identical(r1@raw_response[3], "B")
  expect_identical(r1@response_time[2], 55)
  expect_identical(r1@misc$item_type[3], "MS")
  expect_identical(r1@misc$lexile_level[2], 4)
  expect_identical(r1@misc$grade, "7")
  expect_identical(r1@misc$ability, 1.1)
  expect_equal(names(rs@response_list), rs$examinee_id)

  expect_identical(rs@item_id, c("i1", "i2", "i4"))
  expect_null(rs@testlet_id)


  # Expect error when incorrect columns provided
  expect_error(response_set(x = x_long,
                            data_format = "long",
                            examinee_id_var = "exm_id",
                            item_id_var = "itm_id",
                            score_var = "scr",
                            misc_var = "XYZ"),
               regexp = "Invalid 'misc_var'. Following ")

  # ---------------------------------------------------------------------------#
  # The examinee order in Response_set should have the same order as the
  # examinee order in the original data set.
  ip <- generate_ip(n = 4, item_id = paste0("i", 1:4))
  x_long2 <- data.frame(examinee_id = c("stu2", "stu2", "stu2", "stu1", "stu1"),
                        item_id = c("i1", "i2", "i4", "i1", "i2"),
                        scr = c(0, 1, 0, 1, 0))

  rs <- response_set(x = x_long2,
                     data_format = "long",
                     examinee_id_var = "examinee_id",
                     item_id_var = "item_id",
                     score_var = "scr")
  expect_true(validObject(rs))
  expect_identical(rs[[1]]$examinee_id, "stu2")
  expect_identical(rs[[2]]$examinee_id, "stu1")
  expect_true("i4" %in% rs[[1]]$item_id)
  expect_false("i4" %in% rs[[2]]$item_id)
  expect_identical(rs@item_id, c("i1", "i2", "i4"))
  expect_null(rs@testlet_id)
  expect_equal(names(rs@response_list), rs$examinee_id)

  # The same rule applies when examinee_id is integer.
  x_long2 <- data.frame(examinee_id = c(3L, 3L, 3L, 2L, 2L),
                        item_id = c("i1", "i2", "i4", "i1", "i2"),
                        scr = c(0, 1, 0, 1, 0))

  rs <- response_set(x = x_long2,
                     data_format = "long",
                     examinee_id_var = "examinee_id",
                     item_id_var = "item_id",
                     score_var = "scr")
  expect_true(validObject(rs))
  expect_identical(rs[[1]]$examinee_id, 3L)
  expect_identical(rs[[2]]$examinee_id, 2L)
  expect_equal(names(rs@response_list), as.character(rs$examinee_id))


  # ---------------------------------------------------------------------------#
  # 'long' format with Testlet
  x_long <- data.frame(exm_id = c("stu1", "stu1", "stu1", "stu1",
                                  "stu2", "stu2", "stu2", "stu2", "stu2",
                                  "stu3", "stu3", "stu3", "stu3"),
                       itm_id = c("i5", "i2", "i3", "i4",
                                  "i2", "i5", "i3", "i4", "i1",
                                  "i2", "i1", "i4", "i3"),
                       # t1: i1, i5  ;  t2: i2, i4
                       tstlt_id = c("t1", "t2", NA, "t2",
                                    "t2", "t1", NA, "t2", "t1",
                                    "t2",  "t1", "t2", NA),
                       scr = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0))

  rs <- response_set(x = x_long,
                     data_format = "long",
                     examinee_id_var = "exm_id",
                     item_id_var = "itm_id",
                     testlet_id_var = "tstlt_id",
                     score_var = "scr")
  expect_true(validObject(rs))
  expect_identical(rs@item_id, c("i5", "i2", "i3", "i4", "i1"))
  expect_identical(rs@testlet_id, c("t1", "t2", NA, "t2", "t1"))
  expect_equal(names(rs@response_list), rs$examinee_id)


  # ---------------------------------------------------------------------------#
  # 'fill_na_score' works as expected when 'data_format' = "long"
  t1 <- testlet(generate_ip(n = 2, item_id = c("i1", "i4")), testlet_id = "t1")
  ip <- c(t1, generate_item(item_id = "i2"), generate_item(item_id = "i3"))
  # ip <- generate_ip(n = 4, item_id = paste0("i", 1:4))
  x_long2 <- data.frame(examinee_id = c("stu1", "stu1", "stu1", "stu2", "stu2"),
                        itm_id = c("i1", "i2", "i4", "i1", "i2"),
                        tstlt_id = c("t1", NA, "t1", "t1", NA),
                        rwscore = c("A", "D", "B", "C", "D"),
                        scr = c(0, 1, 0, 1, 0))

  rs <- response_set(x = x_long2,
                     data_format = "long",
                     examinee_id_var = "examinee_id",
                     item_id_var = "itm_id",
                     testlet_id_var = "tstlt_id",
                     score_var = "scr",
                     raw_response_var = "rwscore",
                     fill_na_score = NULL)
  expect_true(validObject(rs))
  expect_false("i4" %in% rs[[2]]$item_id)
  expect_false("i3" %in% rs[[2]]$item_id)
  expect_identical(rs@item_id, c("i1", "i2", "i4"))
  expect_identical(rs@testlet_id, c("t1", NA, "t1"))
  expect_equal(names(rs@response_list), rs$examinee_id)

  # Without item pool
  rs <- response_set(
    x = x_long2,
    data_format = "long",
    examinee_id_var = "examinee_id",
    item_id_var = "itm_id",
    testlet_id_var = "tstlt_id",
    score_var = "scr",
    raw_response_var = "rwscore",
    fill_na_score = 0
    )
  expect_true(validObject(rs))
  expect_true("i4" %in% rs[[2]]$item_id)
  expect_identical(rs[[2]]$score[rs[[2]]$item_id == "i4"], 0)
  expect_false("i3" %in% rs[[2]]$item_id)
  expect_identical(rs@item_id, c("i1", "i2", "i4"))
  expect_identical(rs@testlet_id, c("t1", NA, "t1"))
  expect_equal(names(rs@response_list), rs$examinee_id)

  # With item pool - without testlet
  rs <- response_set(
    x = x_long2,
    ip = ip,
    data_format = "long",
    examinee_id_var = "examinee_id",
    item_id_var = "itm_id",
    score_var = "scr",
    raw_response_var = "rwscore",
    fill_na_score = 0
    )
  expect_true(validObject(rs))
  expect_true("i4" %in% rs[[2]]$item_id)
  expect_identical(rs[[2]]$score[rs[[2]]$item_id == "i4"], 0)
  expect_true("i3" %in% rs[[1]]$item_id)
  expect_true("i3" %in% rs[[2]]$item_id)
  expect_identical(rs[[2]]$score[rs[[1]]$item_id == "i3"], 0)
  expect_identical(rs@item_id, ip$item_id)
  expect_identical(rs@testlet_id, unname(ip$testlet_id))
  expect_equal(names(rs@response_list), rs$examinee_id)


  # With item pool and Testlet
  rs <- response_set(
    x = x_long2,
    ip = ip,
    data_format = "long",
    examinee_id_var = "examinee_id",
    item_id_var = "itm_id",
    testlet_id_var = "tstlt_id",
    score_var = "scr",
    raw_response_var = "rwscore",
    fill_na_score = 0)
  expect_true(validObject(rs))
  expect_true("i4" %in% rs[[2]]$item_id)
  expect_identical(rs[[2]]$score[rs[[2]]$item_id == "i4"], 0)
  expect_true("i3" %in% rs[[1]]$item_id)
  expect_true("i3" %in% rs[[2]]$item_id)
  expect_identical(rs[[2]]$score[rs[[1]]$item_id == "i3"], 0)
  expect_identical(rs@item_id, ip$item_id)
  expect_identical(rs@testlet_id, unname(ip$testlet_id))
  expect_equal(names(rs@response_list), rs$examinee_id)

  ################# Test "tibble" format ###################################@###
  ip <- itempool(b = rnorm(4))
  resp_tibble <- tibble::as_tibble(sim_resp(ip = ip, theta = rnorm(10)))
  rs <- response_set(resp_tibble)
  expect_s4_class(rs, "Response_set")
  expect_true(validObject(rs))
  expect_equal(names(rs@response_list), rs$examinee_id)


})

###############################################################################@
############################# name_examinees ###################################
###############################################################################@

test_that("Test 'name_examinees' function", {
  r1 <- response(sample(0:1, sample(5:10, 1), T))
  r2 <- response(sample(0:1, sample(5:10, 1), T))
  r3 <- response(sample(0:1, sample(5:10, 1), T))
  r4 <- response(sample(0:1, sample(5:10, 1), T), examinee_id = "e4")

  expect_null(r1$examinee_id)
  expect_null(r2$examinee_id)
  expect_null(r3$examinee_id)
  expect_identical(r4$examinee_id, "e4")

  response_list <- list(r1, r2, r3)
  expect_null(unlist(sapply(response_list, slot, "examinee_id")))
  l1_named <- name_examinees(response_list)
  expect_identical(unname(unlist(sapply(l1_named, slot, "examinee_id"))),
                   paste0("S", 1:3))
  expect_identical(names(l1_named), c("S1", "S2", "S3"))

  response_list <- list(r1, r4, r2)
  expect_identical(unlist(sapply(response_list, slot, "examinee_id")), "e4")
  l2_named <- name_examinees(response_list)
  expect_identical(unname(unlist(sapply(l2_named, slot, "examinee_id"))),
                   c("S1", "e4", "S2"))
  expect_identical(names(l2_named), c("S1", "e4", "S2"))


  r1_named <- name_examinees(list(r1))
  expect_identical(unname(unlist(sapply(r1_named, slot, "examinee_id"))), "S1")
  expect_identical(names(r1_named), c("S1"))


  r4_named <- name_examinees(list(r4))
  expect_identical(unname(unlist(sapply(r4_named, slot, "examinee_id"))), "e4")
  expect_identical(names(r4_named), c("e4"))

  # ---------------------------------------------------------------------------#
  r1 <- response(sample(0:1, sample(5:10, 1), T), examinee_id = "S1")
  r2 <- response(sample(0:1, sample(5:10, 1), T), examinee_id = "S2")
  r3 <- response(sample(0:1, sample(5:10, 1), T))

  response_list <- list(r1, r2, r3)
  l5_named <- name_examinees(response_list)
  expect_identical(unname(unlist(sapply(l5_named, slot, "examinee_id"))),
                   c("S1", "S2", "S3"))
  expect_identical(names(l5_named), c("S1", "S2", "S3"))

  # ---------------------------------------------------------------------------#
  # Duplicated
  r1 <- response(sample(0:1, sample(5:10, 1), T), examinee_id = "S1")
  r2 <- response(sample(0:1, sample(5:10, 1), T), examinee_id = "S1")
  r3 <- response(sample(0:1, sample(5:10, 1), T))
  expect_error(name_examinees(list(r1, r2, r3)),
               "Invalid response list. There are duplicated examinee ID's.")

})



###############################################################################@
############################# as.data.frame (Response_set) #####################
###############################################################################@

test_that("as.data.frame (Response_set)", {
  x_long <- data.frame(exm_id = c("stu1", "stu1", "stu1", "stu2", "stu2"),
                       itm_id = c("i1", "i2", "i4", "i1", "i2"),
                       scr = c(0, 1, 0, 1, 0),
                       rwscore = c("A", "D", "B", "C", "D"),
                       resptime = c(33, 55, 22, 66, 31),
                       # These will be passed to misc
                       item_type = c("MC", "MC", "MS", "SA", "MC"),
                       word_count = c(123, 442, 552, 342, 666),
                       # This will be ignored
                       lexile_level = c(1, 4, 3, 2, 1),
                       ability = c(1.1, 1.1, 1.1, -.2, -.2),
                       grade = c("7", "7", "7", "11", "11")
                       )
  rs <- response_set(x = x_long,
                     data_format = "long",
                     examinee_id_var = "exm_id",
                     item_id_var = "itm_id",
                     score_var = "scr",
                     raw_response_var = "rwscore",
                     response_time_var = "resptime",
                     misc_var = c("item_type", "lexile_level"),
                     misc_unique_var = c("ability", "grade")
                     )

  rs_df <- as.data.frame(rs)
  expect_identical(rs_df$examinee_id, x_long$exm_id)
  expect_identical(rs_df$item_id, x_long$itm_id)
  expect_identical(rs_df$score, x_long$scr)
  expect_identical(rs_df$raw_response, x_long$rwscore)
  expect_identical(rs_df$response_time, x_long$resptime)
  expect_identical(rs_df$item_type, x_long$item_type)
  expect_identical(rs_df$lexile_level, x_long$lexile_level)
  expect_identical(rs_df$ability, x_long$ability)
  expect_identical(rs_df$grade, x_long$grade)

})


###############################################################################@
############################# as.matrix.Response_set() #########################
###############################################################################@

test_that("Test 'as.matrix.Response_set()' function", {

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
  x <- as.matrix(rs)
  expect_true(is(x, "matrix"))
  expect_identical(x[1, 3], 0)
  expect_true(is.na(x[2, 3]))
  expect_identical(colnames(x), c("i1", "i2", "i4"))
  expect_identical(rownames(x), c("stu1", "stu2"))

  x <- as.matrix(rs, output = "raw_response")
  expect_identical(x[1, 3], "B")
  expect_true(is.na(x[2, 3]))
  expect_identical(colnames(x), c("i1", "i2", "i4"))
  expect_identical(rownames(x), c("stu1", "stu2"))

  x <- as.matrix(rs, output = "response_time")
  expect_identical(x[1, 3], 22)
  expect_true(is.na(x[2, 3]))
  expect_identical(colnames(x), c("i1", "i2", "i4"))
  expect_identical(rownames(x), c("stu1", "stu2"))

  x <- as.matrix(rs, output = "item_id")
  expect_identical(x[1, 3], "i4")
  expect_true(is.na(x[2, 3]))
  expect_identical(colnames(x), c("i1", "i2", "i4"))
  expect_identical(rownames(x), c("stu1", "stu2"))

  x <- as.matrix(rs, output = "item_type")
  expect_identical(x[1, 3], "MS")
  expect_true(is.na(x[2, 3]))
  expect_identical(colnames(x), c("i1", "i2", "i4"))
  expect_identical(rownames(x), c("stu1", "stu2"))

  x <- as.matrix(rs, output = "lexile_level")
  expect_identical(x[1, 3], 3)
  expect_true(is.na(x[2, 3]))
  expect_identical(colnames(x), c("i1", "i2", "i4"))
  expect_identical(rownames(x), c("stu1", "stu2"))

  # Fail to extract non-existent slot
  expect_error(as.matrix(rs, output = "abc"), "Unable to extract \"abc\"")

  # ---------------------------------------------------------------------------#
  # If one of the Response elements has one slot but another doesn't, the one
  # that doesn't have the slot will have all NA's on that row.
  r1 <- rs@response_list[[1]]
  r2 <- rs@response_list[[2]]
  # Remove raw scores of first Response object
  r1@raw_response <- NULL
  r1@misc$lexile_level <- NULL
  rs2 <- response_set(list(r1, r2))

  x <- as.matrix(rs2, output = "raw_response")
  expect_true(is(x, "matrix"))
  expect_true(all(is.na(x[1,])))
  expect_identical(x[2, 2], "D")
  expect_true(is.na(x[2, 3]))
  expect_identical(colnames(x), c("i1", "i2", "i4"))
  expect_identical(rownames(x), c("stu1", "stu2"))

  x <- as.matrix(rs2, output = "lexile_level")
  expect_true(all(is.na(x[1,])))
  expect_identical(x[2, 2], 1)
  expect_true(is.na(x[2, 3]))
  expect_identical(colnames(x), c("i1", "i2", "i4"))
  expect_identical(rownames(x), c("stu1", "stu2"))


  # ---------------------------------------------------------------------------#
  # Order of the items in the matrix follows the @item_id slot.
  t1 <- testlet(generate_ip(n = 2, item_id = c("i1", "i4")), testlet_id = "t1")
  ip <- c(t1, generate_item(item_id = "i2"), generate_item(item_id = "i3"))
  # ip <- generate_ip(n = 4, item_id = paste0("i", 1:4))
  x_long2 <- data.frame(examinee_id = c("stu1", "stu1", "stu1", "stu2", "stu2"),
                        itm_id = c("i4", "i2", "i1", "i1", "i2"),
                        tstlt_id = c("t1", NA, "t1", "t1", NA),
                        rwscore = c("A", "D", "B", "C", "D"),
                        scr = c(0, 1, 0, 1, 0))

  rs <- response_set(x = x_long2,
                     # ip = ip,
                     data_format = "long",
                     examinee_id_var = "examinee_id",
                     item_id_var = "itm_id",
                     testlet_id_var = "tstlt_id",
                     score_var = "scr",
                     raw_response_var = "rwscore")
  item_ids <- c("i4", "i2", "i1")
  expect_identical(rs@item_id, item_ids)
  expect_identical(rs@testlet_id, c("t1", NA, "t1"))
  rm <- as.matrix(rs)
  expect_identical(colnames(rm), item_ids)
  rm <- as.matrix(rs, output = "raw_response")
  expect_identical(colnames(rm), item_ids)

  # Add ip:
  rs <- response_set(x = x_long2,
                     ip = ip,
                     data_format = "long",
                     examinee_id_var = "examinee_id",
                     item_id_var = "itm_id",
                     testlet_id_var = "tstlt_id",
                     score_var = "scr",
                     raw_response_var = "rwscore")
  item_ids <- c("i1", "i4", "i2")
  expect_identical(rs@item_id, item_ids)
  expect_identical(rs@testlet_id, c("t1", "t1", NA))
  rm <- as.matrix(rs)
  expect_identical(colnames(rm), item_ids)
  rm <- as.matrix(rs, output = "raw_response")
  expect_identical(colnames(rm), item_ids)



  # ---------------------------------------------------------------------------#
  # Another example
  ip <- generate_ip(n = 10)
  theta <- rnorm(14)
  resp <- sim_resp(theta = theta, ip = ip, output = "response_set")
  rm <- as.matrix(resp)
  expect_true(is(rm, "matrix"))
  expect_identical(nrow(rm), 14L)
  expect_identical(ncol(rm), 10L)
  expect_identical(colnames(rm), ip$item_id)
  for (i in sample(1:14, 3))
    for (j in sample(1:10, 3))
      expect_identical(resp@response_list[[i]]@score[j], rm[i, j])

  # ---------------------------------------------------------------------------#
  # as.matrix will return NULL if there are no scores
  r1 <- response(raw_response = c("M", "X", "P"), item_id = paste0("i", 1:3))
  r2 <- response(raw_response = c("B", "A", "C"), item_id = paste0("i", 2:4))
  resp <- response_set(list(r1, r2))
  expect_true(all(is.na(as.matrix(resp))))
  raw_matrix <- as.matrix(resp, output = "raw_response")
  expect_identical(raw_matrix[1, 2], "X")
  expect_true(is.na(raw_matrix[2, 1]))
  expect_identical(raw_matrix[2, 3], "A")


  # ---------------------------------------------------------------------------#
  # as.matrix will successfully run for Response_sets with testlets
  t1 <- testlet(itempool(b = rnorm(2), item_id = c("t1-i1", "t1-i2")),
                   testlet_id = "t1")
  t2 <- testlet(itempool(a = rlnorm(3, 0, .3), b = rnorm(3),
                                item_id = c("t2-i1", "t2-i2", "t2-i3")),
								testlet_id = "t2")
  i1 <- item(b = rnorm(1), item_id = "i1")
  i2 <- item(a = rlnorm(1, 0, .3), b = rnorm(1), c = .2, item_id = "i2")
  i3 <- item(a = rlnorm(1, 0, .3), b = sort(runif(3)), item_id = "i3")
  ip <- c(t1, t2, i1, i2, i3)
  n_examinee <- 4
  resp <- sim_resp(ip = ip, theta = rnorm(n_examinee))
  resp_set <- response_set(resp, ip = ip)
  resp_matrix <- as.matrix(resp_set)
  expect_true(is(resp_matrix, "matrix"))
  expect_identical(ncol(resp_matrix), 8L)
  expect_identical(colnames(resp_matrix), ip$resp_id)

  # ---------------------------------------------------------------------------#
  # print Response_set with only one response
  ip <- generate_ip(n = 10)
  resp <- sim_resp(ip = ip, theta = rnorm(1), prop_missing = .3,
                   output = "response_set")
  resp_matrix <- as.matrix(resp, output = "score")
  expect_null(rownames(resp_matrix))

  resp$response_list[[1]]@examinee_id <- "ABC"
  resp_matrix <- as.matrix(resp, output = "score")
  expect_identical(rownames(resp_matrix), "ABC")

  # ---------------------------------------------------------------------------#
  # When one of the Response within the response set has examinee_id as NULL
  # the response set object correctly converted to a matrix.
  n_item <- sample(11:31, 1)
  n_theta <- sample(7:21, 1)
  ip <- generate_ip(n = n_item)
  theta <- rnorm(n_theta)
  resp_set <- sim_resp(ip = ip, theta = theta, prop_missing = .2,
                       output = "response_set")
  # Set one of the examinee_id's of responses to NULL
  i <- sample(2:n_theta, 1)
  resp_set[[i]]@examinee_id <- NULL
  resp_matrix <- as.matrix(resp_set)
  expect_true(is(resp_matrix, "matrix"))
  expect_identical(nrow(resp_matrix), n_theta)

  # ---------------------------------------------------------------------------#
  # The column names of the output of 'as.matrix' will always be equal to
  # the item_id names.
  n_item <- sample(11:31, 1)
  n_theta <- sample(700:2100, 1)
  ip <- generate_ip(n = n_item)
  theta <- rnorm(n_theta)
  resp_set <- generate_resp_set(ip = ip, theta = theta, prop_missing = .3)
  for (col in c("score", "raw_response", "item_id", "order")) {
    observed <- as.matrix(resp_set, output = col)
    expect_true(all(colnames(observed) %in% ip$item_id))
    # when ip provided, the column names order exactly matches item order of ip
    observed <- as.matrix(resp_set, ip = ip, output = col)
    expect_identical(colnames(observed), ip$item_id)
  }
  # Check when column names are integer values
  ip <- generate_ip(n = n_item, item_id = 1:n_item)
  expect_identical(ip$item_id, as.character(1:n_item))
  resp_set <- sim_resp(ip = ip, theta = theta, prop_missing = .3,
                       output = "response_set")
  for (col in c("score", "raw_response", "item_id", "order")) {
    observed <- as.matrix(resp_set, output = col)
    expect_true(all(colnames(observed) %in% ip$item_id))
    # when ip provided, the column names order exactly matches item order of ip
    observed <- as.matrix(resp_set, ip = ip, output = col)
    expect_identical(colnames(observed), ip$item_id)
  }


  # ---------------------------------------------------------------------------#
  # If 'ip' is provided, then the items that do not have any responses
  # in the response set will be populates as NA
  ip <- generate_ip(n = 15)
  theta <- rnorm(1)
  resp_set <- generate_resp_set(ip = ip, theta = theta, prop_missing = .5)
  item_ids <- resp_set@item_id
  # ip was not provided
  rm <- as.matrix(resp_set)
  expect_identical(colnames(rm), item_ids)
  rm <- as.matrix(resp_set, output = "raw_response")
  expect_identical(colnames(rm), item_ids)
  # ip was provided
  rm <- as.matrix(resp_set, ip = ip)
  expect_identical(colnames(rm), ip$item_id)
  rm <- as.matrix(resp_set, output = "raw_response")
  expect_identical(colnames(rm), item_ids)


  # ---------------------------------------------------------------------------#
  # Generate a response set from scratch and check whether
  n_item <- sample(15:31, 1)
  n_theta <- sample(400:700, 1)
  ip <- generate_ip(n = n_item)
  theta <- rnorm(n_theta)
  resp_set <- generate_resp_set(ip = ip, theta = theta, prop_missing = .3)
  resp_score <- data.frame(as.matrix(resp_set, output = "score", ip = ip),
                           check.names = FALSE)
  resp_raw <- data.frame(as.matrix(resp_set, output = "raw_response"),
                           check.names = FALSE)
  resp_score_long <- reshape(resp_score, direction = "long",
                             times = colnames(resp_score),
                             ids = rownames(resp_score),
                             v.names = "score",
                             timevar = "item_id",
                             idvar = "examinee_id",
                             varying = list(colnames(resp_score)))
  resp_raw_long <- reshape(resp_raw, direction = "long",
                           times = colnames(resp_raw),
                           ids = rownames(resp_raw),
                           v.names = "raw_response",
                           timevar = "item_id",
                           idvar = "examinee_id",
                           varying = list(colnames(resp_raw)))
  resp_long <- merge(x = resp_score_long, y = resp_raw_long,
                     by = c("examinee_id", "item_id"))
  resp_set_new <- response_set(x = resp_long, data_format = "long",
                               ip = ip, examinee_id_var = "examinee_id",
                               item_id_var = "item_id", score_var = "score",
                               raw_response_var = "raw_response")
  resp_score_new <- as.matrix(resp_set_new, output = "score", ip = ip)
  resp_raw_new <- as.matrix(resp_set_new, output = "raw_response", ip = ip)
  expect_identical(colnames(resp_score_new), ip$item_id)
  expect_identical(colnames(resp_raw_new), ip$item_id)

  # Check for a few students whether response matrices are corresponding
  for (examinee_id in sample(resp_set_new$examinee_id, 3)) {
    old_response <- resp_set[[which(resp_set$examinee_id == examinee_id)]]
    new_response <- resp_set_new[[which(
      resp_set_new$examinee_id == examinee_id)]]
    for (item_id in sample(old_response$item_id, 2)) {
      temp_score <- old_response$score[old_response$item_id == item_id]
      temp_raw_resp <- old_response$raw_response[
        old_response$item_id == item_id]
      # Check score
      expect_identical(temp_score,
                       new_response$score[new_response$item_id == item_id])
      expect_identical(temp_score,
                       resp_score[rownames(resp_score) == examinee_id,
                                  colnames(resp_score) == item_id])
      expect_identical(temp_score,
                       resp_score_new[rownames(resp_score_new) == examinee_id,
                                      colnames(resp_score_new) == item_id])
      # Check raw_response
      expect_identical(
        temp_raw_resp,
        new_response$raw_response[new_response$item_id == item_id])
      expect_identical(temp_raw_resp,
                       resp_raw[rownames(resp_raw) == examinee_id,
                                colnames(resp_raw) == item_id])
      expect_identical(temp_raw_resp,
                       resp_raw_new[rownames(resp_raw_new) == examinee_id,
                                    colnames(resp_raw_new) == item_id])
    }
  }
})

###############################################################################@
############################# as.list.Response_set() ###########################
###############################################################################@

test_that("as.list.Response_set()", {
  n_item <- sample(15:31, 1)
  n_theta <- sample(40:70, 1)
  ip <- generate_ip(n = n_item)
  theta <- rnorm(n_theta)
  resp_set <- generate_resp_set(ip = ip, theta = theta, prop_missing = .3)
  observed <- as.list(resp_set)
  i <- sample(1:n_theta, 1)
  expect_identical(observed[[i]], resp_set[[i]])
  expect_identical(observed[[i]], resp_set@response_list[[i]])
  # lapply method can be applied to resp_set
  expect_type(lapply(
    resp_set, function(x) resp_lik(resp = x, ip = ip, theta = 1)), "list")
})

###############################################################################@
############################# print.Response_set() #############################
###############################################################################@
test_that("Test 'print.Response_set' function", {

  # ---------------------------------------------------------------------------#
  # print a response set of 5 items
  ip <- generate_ip(n = 12)
  n_theta <- sample(5:19, 1)
  resp <- sim_resp(ip = ip, theta = rnorm(n_theta), output = "response_set")
  expect_output(print(resp), paste0("A 'Response_set' of ", n_theta,
                                    " responses."))
  expect_output(print(resp), "Item_1 Item_2 Item_3 Item_4")
  expect_true(any(grepl(paste0("with ", n_theta - 4, " more responses"),
                        capture.output(print(resp, n = 4)))))
  expect_false(any(grepl("S5", capture.output(print(resp, n = 4)))))


  # ---------------------------------------------------------------------------#
  # print when there is only raw responses
  r1 <- response(raw_response = c("M", "X", "P"), item_id = paste0("i", 1:3))
  r2 <- response(raw_response = c("B", "A", "C"), item_id = paste0("i", 2:4))
  resp <- response_set(list(r1, r2))
  expect_output(print(resp), "A 'Response_set' of 2 responses.")
  expect_output(print(resp, base_print = TRUE), "NA  \"B\" \"A\" \"C\"")
  expect_output(print(resp), "[ ]+B[ ]+A[ ]+C")


  # ---------------------------------------------------------------------------#
  # print Response_set with testlets
  t1 <- testlet(itempool(b = rnorm(2), item_id = c("t1-i1", "t1-i2")),
                   testlet_id = "t1")
  t2 <- testlet(itempool(a = rlnorm(3, 0, .3), b = rnorm(3),
                                item_id = c("t2-i1", "t2-i2", "t2-i3")),
								testlet_id = "t2")
  i1 <- item(b = rnorm(1), item_id = "i1")
  i2 <- item(a = rlnorm(1, 0, .3), b = rnorm(1), c = .2, item_id = "i2")
  i3 <- item(a = rlnorm(1, 0, .3), b = sort(runif(3)), item_id = "i3")
  ip <- c(t1, t2, i1, i2, i3)
  n_examinee <- 4
  resp <- sim_resp(ip = ip, theta = rnorm(n_examinee))
  resp_set <- response_set(resp, ip = ip)
  expect_output(print(resp_set, base_print = TRUE),
                "t1-i1 t1-i2 t2-i1 t2-i2 t2-i3 i1 i2 i3")
  expect_output(print(resp_set),
                "`t1-i1` `t1-i2` `t2-i1` `t2-i2` `t2-i3`[ ]+i1[ ]+i2[ ]+i3")


  # ---------------------------------------------------------------------------#
  # print Response_set with only one response
  ip <- generate_ip(n = 10)
  resp <- sim_resp(ip = ip, theta = rnorm(1), prop_missing = .3,
                   output = "response_set")
  expect_false(any(grepl("NULL", capture.output(print(resp)))))


  # ---------------------------------------------------------------------------#
  # When one of the Response within the response set has examinee_id as NULL
  # the response set object correctly printed.
  n_item <- sample(11:31, 1)
  n_theta <- sample(7:20, 1)
  ip <- generate_ip(n = n_item)
  theta <- rnorm(n_theta)
  resp_set <- sim_resp(ip = ip, theta = theta, prop_missing = .2,
                       output = "response_set")
  # Set one of the examinee_id's of responses to NULL
  i <- sample(4:n_theta, 1)
  resp_set[[i]]@examinee_id <- NULL
  expect_output(print(resp_set, base_print = TRUE), "S3")
  expect_output(print(resp_set, base_print = TRUE), "<NA>")
  expect_output(print(resp_set), "S3")


  # ---------------------------------------------------------------------------#
  # Only first 10 items will be printed.
  n_item <- sample(30:50, 1)
  n_theta <- sample(50:70, 1)
  ip <- generate_ip(n = n_item)
  theta <- rnorm(n_theta)
  resp_set <- sim_resp(ip = ip, theta = theta, prop_missing = .2,
                       output = "response_set")
  x <- capture.output(print(resp_set, base_print = TRUE))
  resp_set_matrix <- as.matrix(resp_set)

  # 11th column/item_id is not in the title
  expect_false(any(grepl(colnames(resp_set_matrix)[11], x[1:5])))
  expect_output(print(resp_set, base_print = TRUE),
                paste0("... with ", n_theta - 10, " more responses, and ",
                       n_item - 10, " more items:"))

  # ---------------------------------------------------------------------------#
  # print(..., n = XXX) should be a valid number

  # TODO


})


###############################################################################@
############################# $ method #########################################
###############################################################################@

test_that("Test '$' method for Response_set", {
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
                     misc_var = c("item_type", "lexile_level"),
                     misc = list(form = "T1"),
                     ip = ip
                     )

  expect_identical(rs$response_list, rs@response_list)
  expect_identical(rs$misc, rs@misc)
  expect_identical(rs$examinee_id, unique(x_long$examinee_id))
  # Invalid name should return NULL
  expect_null(rs$abc)
  # Unknown field should first check misc field of Response_set
  expect_identical(rs$form, "T1")
  # If unknown '$name' could not be found in 'misc' check fields of individual
  # responses.
  expect_identical(rs$lexile_level[[2]], c(2, 1))

  # Assign a single value to misc field of each response
  rs$accessibility_code <- c("FB-2", "DA-15")
  expect_identical(rs$response_list[[2]]$accessibility_code, "DA-15")
  expect_identical(rs$accessibility_code, setNames(c("FB-2", "DA-15"),
                                                   c("stu1", "stu2")))
  expect_identical(rs$item_id, rs@item_id)
  expect_identical(rs$testlet_id, rs@testlet_id)
})



###############################################################################@
############################# $<- method (Response_set) ########################
###############################################################################@


test_that("Test '$' method for Response_set", {
  # Add ip
  n_item <- 5
  ip <- generate_ip(n = n_item, item_id = paste0("i", 1:n_item))
  resp <- sim_resp(ip = ip, theta = rnorm(7), output = "response_set")
  ip2 <- generate_ip(n = n_item + 2, item_id = paste0("i", 1:(n_item + 2)))

  # Add misc
  temp <- 1:n_item
  expect_error(resp$misc <- temp, "Invalid value. 'misc' should be a list.")
  resp$misc <- list(a = temp)
  expect_identical(resp$misc$a, temp)

  # Add response_list
  expect_identical(length(resp$response_list), 7L)
  rl <- resp@response_list
  rl[[1]] <- NULL
  resp$response_list <- rl
  expect_identical(length(resp$response_list), 6L)

  # Add element to 'misc'
  expect_identical(names(resp$misc), "a")
  resp$form <- "Form1"
  expect_identical(names(resp$misc), c("a", "form"))
  expect_identical(resp$misc$form, "Form1")

  # ---------------------------------------------------------------------------#
  # If unknown '$name' is tried to assigned to Response_set item and this field
  # has the same length of the length of responses in the response set, then
  # each element will be assigned to the 'misc' fields of individual response
  # objects within the response set with the same 'name'.
  n_item <- 5
  n_theta <- 7
  ip <- generate_ip(n = n_item, item_id = paste0("i", 1:n_item))
  resp <- sim_resp(ip = ip, theta = rnorm(n_theta), output = "response_set")
  i <- sample(1:n_item, 1)
  expect_null(resp$response_list[[i]]@misc)
  lex_lev <- sample(1:4, n_theta, TRUE)
  resp$lexile_level <- lex_lev
  expect_null(resp$misc$lexile_level)
  i <- sample(1:n_theta, 1)
  expect_identical(resp$response_list[[i]]$misc$lexile_level, lex_lev[i])
  # Add another field to each Response
  est_theta <- rnorm(n_theta)
  resp$est_theta <- est_theta
  i <- sample(1:n_theta, 1)
  expect_identical(resp$response_list[[i]]$misc$est_theta, est_theta[i])
  # Add another field to Response_set
  expect_null(resp$misc)
  resp$segments <- c("S124", "Seg-44")
  expect_identical(names(resp$misc), "segments")
  expect_identical(resp$misc$segments, c("S124", "Seg-44"))


  # ---------------------------------------------------------------------------#
  n_item <- 5
  n_theta <- 7
  ip <- generate_ip(n = n_item, item_id = paste0("i", 1:n_item))
  resp_set <- sim_resp(ip = ip, theta = rnorm(n_theta), output = "response_set")
  expect_identical(resp_set$examinee_id, paste0("S", 1:n_theta))
  new_examinee_ids <- paste0("Subj--", 1:n_theta)
  resp_set$examinee_id <- new_examinee_ids
  expect_identical(resp_set$examinee_id, new_examinee_ids)
  expect_error(resp_set$examinee_id <- new_examinee_ids[-1], "Invalid value")

})


###############################################################################@
############################# length (Response_set) ############################
###############################################################################@


test_that("Test length (Response_set)", {
  # Add ip
  ip <- generate_ip(n = 5, item_id = paste0("i", 1:5))
  n <- sample(10:40, 1)
  resp <- sim_resp(ip = ip, theta = rnorm(n), output = "response_set")
  expect_identical(length(resp), n)
})


###############################################################################@
############################# [ (Response_set) #################################
###############################################################################@

test_that("Test `[` (Response_set)", {
  # Add ip
  ip <- generate_ip(n = 5, item_id = paste0("i", 1:5))
  resp_set <- sim_resp(ip = ip, theta = rnorm(sample(10:40, 1)),
                       output = "response_set")

  # Single element extraction should be a response set
  r1 <- resp_set[2]
  r2 <- response_set(list(S2 = resp_set[[2]]))
  expect_s4_class(r1, "Response_set")
  expect_identical(r1, r2)

  # Extract elements via a character vector
  r1 <- resp_set[c("S1", "S7")]
  r2 <- response_set(list(S1 = resp_set[[1]], S7 = resp_set[[7]]))
  expect_s4_class(r1, "Response_set")
  expect_identical(r1, r2)

  # Subset via a numeric vector
  r1 <- resp_set[c(1, 3)] # Order is important
  expect_identical(length(r1), 2L)
  expect_identical(r1[[1]], resp_set[[1]])
  expect_identical(r1[[2]], resp_set[[3]])

  # Order is important
  r2 <- resp_set[c(3, 1)]
  expect_identical(r2[[1]], resp_set[[3]])
  expect_identical(r2[[2]], resp_set[[1]])

  # Get all but one response
  r1 <- resp_set[-2]
  expect_identical(length(r1), length(resp_set) - 1L)
  expect_identical(r1[[2]], resp_set[[3]])

  # ---------------------------------------------------------------------------#
  # Check subsetting when ALL examinee_id's are null
  ip <- generate_ip(n = 5, item_id = paste0("i", 1:5))
  resp_set <- sim_resp(ip = ip, theta = rnorm(3), output = "response_set")
  for (i in 1:length(resp_set))
    resp_set@response_list[[i]]@examinee_id <- NULL
  names(resp_set@response_list) <- NULL

  # Single element extraction should be a response set
  r1 <- resp_set[2]
  r2 <- response_set(resp_set[[2]])
  r2[[1]]$examinee_id <- NULL
  expect_s4_class(r1, "Response_set")
  # TODO: response_set function may unnecessarily adding names. Check this later
  # expect_identical(r1, r2)

  # Cannot subset via named vector if all examinee_id's are NULL
  expect_error(resp_set[c("S1", "S2")])

  # Subset via a numeric vector
  r1 <- resp_set[c(1, 3)] # Order is important
  expect_identical(length(r1), 2L)
  expect_identical(r1[[1]], resp_set[[1]])
  expect_identical(r1[[2]], resp_set[[3]])

  # Order is important
  r2 <- resp_set[c(3, 1)]
  expect_identical(r2[[1]], resp_set[[3]])
  expect_identical(r2[[2]], resp_set[[1]])

  # Get all but one response
  r1 <- resp_set[-2]
  expect_identical(length(r1), length(resp_set) - 1L)
  expect_identical(r1[[2]], resp_set[[3]])

  # use logical vector to subset
  r1 <- resp_set[c(TRUE, FALSE, TRUE)]
  expect_identical(length(r1), 2L)
  expect_identical(r1[[2]], resp_set[[3]])


  # ---------------------------------------------------------------------------#
  # Check subsetting when SOME examinee_id's are null
  ip <- generate_ip(n = 5, item_id = paste0("i", 1:5))
  resp_set <- sim_resp(ip = ip, theta = rnorm(5), output = "response_set")
  resp_set@response_list[[2]]@examinee_id <- NULL
  resp_set@response_list[[4]]@examinee_id <- NULL

  # Single element extraction should be a response set
  r1 <- resp_set[2]
  expect_s4_class(r1, "Response_set")
  expect_identical(r1[[1]], resp_set[[2]])

  # Extract elements via a character vector
  r1 <- resp_set[c("S1", "S3")]
  r2 <- response_set(list(S1 = resp_set[[1]], S3 = resp_set[[3]]))
  expect_s4_class(r1, "Response_set")
  expect_identical(r1, r2)

  # Subset via a numeric vector
  r1 <- resp_set[c(1, 4)] # Order is important
  expect_identical(length(r1), 2L)
  expect_identical(r1[[1]], resp_set[[1]])
  expect_identical(r1[[2]], resp_set[[4]])

  # Order is important
  r2 <- resp_set[c(4, 1)]
  expect_identical(r2[[1]], resp_set[[4]])
  expect_identical(r2[[2]], resp_set[[1]])

  # Get all but one response
  r1 <- resp_set[-2]
  expect_identical(length(r1), length(resp_set) - 1L)
  expect_identical(r1[[2]], resp_set[[3]])

  # use logical vector to subset
  r1 <- resp_set[c(TRUE, FALSE, TRUE, FALSE, FALSE)]
  expect_identical(length(r1), 2L)
  expect_identical(r1[[2]], resp_set[[3]])

  # Recycle, i.e. get all elements
  expect_identical(resp_set[TRUE], resp_set)


})



###############################################################################@
############################# [[ (Response_set) ################################
###############################################################################@

test_that("Test `[[` (Response_set)", {
  # Add ip
  ip <- generate_ip(n = 5, item_id = paste0("i", 1:5))
  n_theta <- sample(10:40, 1)
  resp <- sim_resp(ip = ip, theta = rnorm(n_theta), output = "response_set")
  i <- sample(1:n_theta, 1)
  expect_identical(resp[[i]], resp$response_list[[i]])
})



###############################################################################@
############################# [[<- (Response_set) ##############################
###############################################################################@

test_that("Test `[[<-` (Response_set)", {
  # Add ip
  ip <- generate_ip(n = 20, item_id = paste0("i", 1:20))
  n_theta <- sample(10:40, 1)
  resp <- sim_resp(ip = ip, theta = rnorm(n_theta), output = "response_set")
  r1 <- sim_resp(ip = ip, theta = rnorm(1), output = "response_set")[[1]]
  i <- sample(1:n_theta, 1)
  resp[[i]] <- r1
  expect_identical(resp[[i]], r1)
})


###############################################################################@
############################# concatenation of 'Response' objects ##############
###############################################################################@

test_that("concatenation of 'Response' objects", {
  r1 <- response(sample(0:1, sample(5:10, 1), T))
  r2 <- response(sample(0:1, sample(5:10, 1), T))
  r3 <- response(sample(0:1, sample(5:10, 1), T), examinee_id = "e3")
  r4 <- response(sample(0:1, sample(5:10, 1), T), examinee_id = "e4")

  rs1 <- response_set(list(r1, r2))
  rs2 <- response_set(list(r3, r4))

  # concatenate two Response objects
  rsn1 <- c(r1, r2)
  expect_s4_class(rsn1, "Response_set")
  expect_true(validObject(rsn1))
  expect_identical(length(rsn1), 2L)
  expect_identical(rsn1$response_list[[2]]$score, r2$score)
  expect_identical(rsn1$response_list[[2]]$examinee_id, "S2")

  # concatenate two Response_list objects
  rsn2 <- c(rs1, rs2)
  expect_s4_class(rsn2, "Response_set")
  expect_true(validObject(rsn2))
  expect_identical(length(rsn2), 4L)

  # concatenate a Response_list and a Response object
  rsn3 <- c(rs1, r3)
  expect_s4_class(rsn3, "Response_set")
  expect_true(validObject(rsn3))
  expect_identical(length(rsn3), 3L)

  rsn4 <- c(rs1, r2)
  expect_s4_class(rsn4, "Response_set")
  expect_true(validObject(rsn4))
  expect_identical(length(rsn4), 3L)
})


###############################################################################@
############################# convert_to_resp_set ##############################
###############################################################################@


test_that("convert_to_resp_set()", {
  n_item <- sample(4:9, 1)
  n_theta <- sample(5:10, 1)
  ip <- generate_ip(n = n_item)
  resp_set <- generate_resp_set(ip = ip, theta = rnorm(n_theta),
                                prop_missing = .2)
  resp_matrix <- as.matrix(resp_set, ip = ip)


  # ---------------------------------------------------------------------------#
  # Response object
  x <- response(1:2)
  expect_s4_class(x, "Response")
  expect_s4_class(convert_to_resp_set(x), "Response_set")

  # ---------------------------------------------------------------------------#
  # Response_set object
  x <- resp_set
  expect_s4_class(convert_to_resp_set(x), "Response_set")

  # ---------------------------------------------------------------------------#
  # Matrix object
  x <- resp_matrix
  expect_s4_class(convert_to_resp_set(x), "Response_set")


  # ---------------------------------------------------------------------------#
  # data.frame object
  x <- as.data.frame(resp_matrix)
  expect_s4_class(convert_to_resp_set(x), "Response_set")

  # ---------------------------------------------------------------------------#
  # List of Response objects
  x <- list(resp_set[[1]], resp_set[[2]])
  expect_s4_class(convert_to_resp_set(x), "Response_set")

  # ---------------------------------------------------------------------------#
  # A vector of numbers
  x <- sample(0:1, length(ip), T)
  expect_s4_class(convert_to_resp_set(x), "Response_set")

  # ---------------------------------------------------------------------------#
  # A response_set with all raw responses
  x <- cbind(t = "x", as.data.frame(resp_matrix))
  observed <- convert_to_resp_set(x, enforce_data_type = "raw_response")
  expect_s4_class(observed, "Response_set")
  expect_true(all(is.na(observed$score)))

  expect_error(convert_to_resp_set(x, enforce_data_type = "score"))


  # ---------------------------------------------------------------------------#
  # bad matrix object
  x <- matrix(as.character(resp_matrix), ncol = n_item)
  observed <- convert_to_resp_set(x, enforce_data_type = "raw_response")
  expect_s4_class(observed, "Response_set")
  expect_true(all(is.na(observed$score)))

  expect_error(convert_to_resp_set(x, enforce_data_type = "score"))

  ################# Errors #################################################@###

  expect_error(convert_to_resp_set(as.numeric(NA)))
  expect_error(convert_to_resp_set(rep(NA, length(ip)), ip = ip))


})
