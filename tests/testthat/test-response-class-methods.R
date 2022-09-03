

# library(testthat)

###############################################################################@
############################# response() #######################################
###############################################################################@

test_that("Test 'response()' function", {

  # score = NULL
  # examinee_id = NULL
  # item_id = NULL
  # raw_response = NULL
  # testlet_id = NULL
  # order = NULL
  # response_time = NULL
  # misc = NULL

  expect_s4_class(
    object = response(examinee_id = "Stu12",
                      item_id = c("Item1", "Item2", "Item3", "Item4"),
                      score = c(0, 1, 1, 1),
                      order = c(1L, 2L, 3L, 4L),
                      misc = list(item_role = c("F", "O", "O", "O"))
                 ),
    class = "Response")

  # ---------------------------------------------------------------------------#
  # A minimal Response object:
  expect_s4_class(
    object = response(c(0, 1, 1, 0)),
    class = "Response")

  expect_s4_class(
    object = response(raw_response = c("C", "A", "B", "A")),
    class = "Response")

  # ---------------------------------------------------------------------------#
  # # This is a very interesting error due to order of arguments:
  # response("Response",
  #          examinee_id = "Stu12",
  #          item_id = c("Item1", "Item2", "Item3", "Item4"),
  #          score = c(0, 1, 1, 1),
  #          order = c(1L, 2L, 3L, 4L),
  #          misc = list(item_role = c("F", "O", "O", "O"))
  #          )

  # ---------------------------------------------------------------------------#
  # # All slots are NULL in minimal response object
  r <- response(c(0, 1, 1, 0))
  expect_null(r@examinee_id)
  # Default item_ids assigned
  expect_identical(r@item_id, paste0("Item_", 1:4))
  expect_null(r@testlet_id)
  expect_null(r@raw_response)
  expect_null(r@order)
  expect_null(r@response_time)
  expect_null(r@misc)

  # ---------------------------------------------------------------------------#
  # Error will be raised if a matrix entered

  expect_error(response(matrix(0, ncol = 10, nrow = 3)),
               "Invalid 'score'. Score should be a valid atomic vector")

  # ---------------------------------------------------------------------------#
  # Missing (NA) responses will be removed from the responses
  n_items <- 10
  ip <- itempool(a = rlnorm(n_items), b = rnorm(n_items))
  resp_matrix <- sim_resp(ip = ip, theta = rnorm(1, 0, .2))[1, ]
  missing_indices <- rep(F, n_items)
  missing_indices[sample(1:n_items, 3)] <- TRUE
  resp_matrix[missing_indices] <- NA

  expect_s4_class(response(resp_matrix, item_id = ip$item_id), "Response")

  # ---------------------------------------------------------------------------#
  # An empty score value is not acceptable
  expect_error(response(score = numeric(0), item_id = c("I1")),
               "Invalid 'score'. Score should be a valid atomic vector.")
})


###############################################################################@
############################# print.Response() #################################
###############################################################################@

test_that("Test 'print.Response()' function", {
  r1 <- response(examinee_id = "Stu12",
                 item_id = c("Item1", "Item2", "Item3", "Item4"),
                 score = c(0, 1, 1, 1),
                 order = c(1L, 2L, 3L, 4L),
                 misc = list(item_role = c("F", "O", "O", "O"),
                             # This should not be printed
                             c("trukdnf12", "y", "z9", "t"),
                             abcdef_col = c("12", "123"),
                             cdef952 = matrix(0, nrow = 4),
                             direct123 = list("a", "b", "c", "d"),
                             theta = -2.22,
                             test_date = as.Date("2022-12-12")
                             )
                  )

  expect_output(print(r1), regexp = "Examinee ID: \"Stu12\"")
  expect_output(print(r1), regexp = "item_role")
  expect_output(print(r1), regexp = "score")
  expect_output(print(r1), regexp = "order")
  text <- capture_output(print(r1))
  expect_true(grepl(pattern = "abcdef_col", x = text))
  expect_true(grepl(pattern = "cdef952", x = text))
  expect_true(grepl(pattern = "direct123", x = text))
  expect_true(grepl(pattern = "trukdnf12", x = text))

  # ---------------------------------------------------------------------------#
  # print only a selected number of rows
  text <- capture_output(print(r1, n = 2))
  expect_true(grepl(pattern = "Item2", x = text))
  expect_false(grepl(pattern = "Item3", x = text))


  # ---------------------------------------------------------------------------#
  # print without 'misc' slot
  r2 <- response(examinee_id = "Stu12",
                item_id = c("Item1", "Item2", "Item3", "Item4"),
                score = c(0, 1, 1, 1),
                order = c(1L, 2L, 3L, 4L)
                 )
  expect_output(print(r2), regexp = "Examinee ID: \"Stu12\"")
  expect_output(print(r2), regexp = "score")
  expect_output(print(r2), regexp = "order")


  # ---------------------------------------------------------------------------#
  # print all rows using Inf
  n_items <- sample(30:40, 1)
  r3 <- response(sample(0:1, n_items, T))
  expect_false(grepl(pattern = "Item_12", x = capture_output(print(r3))))
  expect_output(print(r3, n = 12), "Item_12")
  expect_false(grepl(pattern = "Item_13", x = capture_output(print(r3))))
  expect_output(print(r3, n = Inf), paste0("Item_", n_items))


  # ---------------------------------------------------------------------------#
  # When the number of rows to print is larger than the actual number of
  # responses, only the maximum number of responses should be printed
  n_items <- sample(30:40, 1)
  r4 <- response(sample(0:1, n_items, T))
  output <- capture.output(print(r4, n = 50))
  expect_false(any(grepl("NA", output)))
  expect_false(any(grepl("more scores", output)))
  expect_true(any(grepl(paste0("Item_", n_items), output)))
  expect_true(any(grepl(paste0(" with ", n_items, " scores\\."), output)))

  # ---------------------------------------------------------------------------#
  # If there is only one item in the Response object, the title should read
  # singlular instead of plural
  n_items <- 1
  r5 <- response(sample(0:1, n_items, T))
  output <- capture.output(print(r5))
  expect_true(any(grepl(" with 1 score\\.", output)))
  expect_false(any(grepl(" with 1 scores\\.", output)))

})



###############################################################################@
############################# show.Response() ##################################
###############################################################################@

test_that("Test 'show.Response()' function", {
  r1 <- response(examinee_id = "Stu12",
                item_id = c("Item1", "Item2", "Item3", "Item4"),
                score = c(0, 1, 1, 1),
                order = c(1L, 2L, 3L, 4L),
                misc = list(item_role = c("F", "O", "O", "O"),
                            # This should not be printed
                            c("trukdnf12", "y", "z9", "t"),
                            abcdef_col = c("12", "123"),
                            cdef952 = matrix(0, nrow = 4),
                            direct123 = list("a", "b", "c", "d")
                            )
                 )

  expect_output(show(r1), regexp = "Examinee ID: \"Stu12\"")
  expect_output(show(r1), regexp = "item_role")
  expect_output(show(r1), regexp = "score")
  expect_output(show(r1), regexp = "order")
  text <- capture_output(show(r1))
  expect_true(grepl(pattern = "abcdef_col", x = text))
  expect_true(grepl(pattern = "cdef952", x = text))
  expect_true(grepl(pattern = "direct123", x = text))
  expect_true(grepl(pattern = "trukdnf12", x = text))
})



###############################################################################@
############################# as.data.frame.Response() #########################
###############################################################################@

test_that("Test 'as.data.frame.Response()' function", {
  r1 <- response(examinee_id = "Stu12",
                item_id = c("Item1", "Item2", "Item3", "Item4"),
                score = c(0, 1, 1, 1),
                order = c(1L, 2L, 3L, 4L),
                misc = list(item_role = c("F", "O", "O", "O"),
                            # This should not be printed
                            c("trukdnf12", "y", "z9", "t"),
                            abcdef_col = c("12", "123"),
                            cdef952 = matrix(0, nrow = 4),
                            direct123 = list("a", "b", "c", "d")
                            )
                 )
  r1df <- as.data.frame(r1)
  expect_s3_class(r1df, "data.frame")
  expect_true(all(c("score", "order", "item_id", "item_role") %in%
                    colnames(r1df)))
  expect_false(any(c("direct123", "misc") %in% colnames(r1df)))

  # ---------------------------------------------------------------------------#
  # Documentation example:
  resp <- response(examinee_id = "Stu12",
                   item_id = c("Item1", "Item2", "Item3", "Item4"),
                   score = c(0, 1, 1, 1),
                   raw_response = c("B", "A", "D", "Right Angle"),
                   order = c(1L, 2L, 3L, 4L),
                   misc = list(item_role = c("F", "O", "O", "O"),
                               lexile_level = c(1, 4, 3, 1),
                               item_type = c("MC", "MC", "MS", "SA"),
                               test_date = as.Date("2021-11-21"),
                               Form = "Test Form 001",
                               theta = 2.2))
  rdf <- as.data.frame(resp)
  expect_s3_class(rdf, "data.frame")
  expect_true(all(c("examinee_id", "item_id", "score", "raw_response", "order",
                    "item_role", "lexile_level", "item_type", "test_date",
                    "Form", "theta") %in%  colnames(rdf)))
  # test 'attach_unique_misc = FALSE'
  rdf <- as.data.frame(resp, attach_unique_misc = FALSE)
  expect_true(all(c("examinee_id", "item_id", "score", "raw_response", "order",
                    "item_role", "lexile_level", "item_type") %in%
                    colnames(rdf)))
  expect_false(any(c("test_date", "Form", "theta") %in%  colnames(rdf)))

  # ---------------------------------------------------------------------------#

  resp <- new("Response",
              examinee_id = "stu1",
              item_id = c("i1", "i2", "i4"),
              testlet_id = NULL,
              score = c(0, 1, 0),
              raw_response = c("A", "D", "B"),
              order = NULL,
              response_time = c(33, 55, 22),
              misc = list(item_type = c("MC", "MC", "MS"),
                          lexile_level = c(1, 4, 3),
                          ability = 1.1,
                          grade = "7"))
  rdf <- as.data.frame(resp)
  n <- length(resp)
  expect_identical(rdf$examinee_id, rep(resp@examinee_id, n))
  expect_identical(rdf$item_id, resp@item_id)
  expect_identical(rdf$score, resp@score)
  expect_identical(rdf$raw_response, resp@raw_response)
  expect_identical(rdf$response_time, resp@response_time)
  expect_identical(rdf$item_type, resp@misc$item_type)
  expect_identical(rdf$lexile_level, resp@misc$lexile_level)
  expect_identical(rdf$ability, rep(resp@misc$ability, n))
  expect_identical(rdf$grade, rep(resp@misc$grade, n))
})



###############################################################################@
############################# $ method #########################################
###############################################################################@

test_that("Test '$' method", {
  r1 <- response(examinee_id = "Stu12",
                 item_id = c("Item1", "Item2", "Item3", "Item4"),
                 score = c(0, 1, 1, 1),
                 order = c(1L, 2L, 3L, 4L),
                 response_time = c(19, 22, 77, 31),
                 misc = list(item_role = c("F", "O", "O", "O"),
                             # This should not be printed
                             c("trukdnf12", "y", "z9", "t"),
                             abcdef_col = c("12", "123"),
                             cdef952 = matrix(0, nrow = 4),
                             direct123 = list("a", "b", "c", "d")
                             )
                 )
  expect_identical(r1$examinee_id, "Stu12")

  expect_identical(r1$item_id, c("Item1", "Item2", "Item3", "Item4"))
  expect_identical(r1$score, c(0, 1, 1, 1))
  expect_identical(r1$order, c(1L, 2L, 3L, 4L))
  expect_identical(r1$response_time, c(19, 22, 77, 31))
  expect_identical(r1$misc, list(item_role = c("F", "O", "O", "O"),
                                  c("trukdnf12", "y", "z9", "t"),
                                  abcdef_col = c("12", "123"),
                                  cdef952 = matrix(0, nrow = 4),
                                  direct123 = list("a", "b", "c", "d")))
  expect_identical(r1$misc$item_role, c("F", "O", "O", "O"))
  expect_identical(r1$misc$direct123, list("a", "b", "c", "d"))

  # ---------------------------------------------------------------------------#
  # $testlet_id works
  ip <- c(generate_testlet(n = 3, item_id_preamble = "t1"),
          generate_ip(n = 5),
          generate_testlet(n = 4, item_id_preamble = "t2"))
  resp <- sim_resp(ip = ip, theta = rnorm(1), output = "response_set")[[1]]
  expect_true(!is.null(resp$testlet_id))
  expect_identical(resp$testlet_id, resp@testlet_id)
})


###############################################################################@
############################# $<- method #######################################
###############################################################################@

test_that("Test '$<-' method", {
  r1 <- response(examinee_id = "Stu12",
                 item_id = c("Item1", "Item2", "Item3", "Item4"),
                 score = c(0, 1, 1, 1),
                 order = c(1L, 2L, 3L, 4L),
                 response_time = c(19, 22, 77, 31),
                 misc = list(item_role = c("F", "O", "O", "O"),
                             # This should not be printed
                             c("trukdnf12", "y", "z9", "t"),
                             abcdef_col = c("12", "123"),
                             cdef952 = matrix(0, nrow = 4),
                             direct123 = list("a", "b", "c", "d")
                             )
                 )
  r1$examinee_id <- "ksdfh"
  expect_identical(r1$examinee_id, "ksdfh")
  r1$item_id <- c("I1", "I2", "I3", "I4")
  expect_identical(r1$item_id, c("I1", "I2", "I3", "I4"))
  r1$score <- c(3, 1, 0, 1)
  expect_identical(r1$score, c(3, 1, 0, 1))
  r1$order <- c(5L, 2L, 1L, 8L)
  expect_identical(r1$order, c(5L, 2L, 1L, 8L))
  r1$response_time <- c(191, 292, 67, 34)
  expect_identical(r1$response_time, c(191, 292, 67, 34))
  r1$misc <- list(a = 1:6, b = letters[1:3])
  expect_identical(r1$misc, list(a = 1:6, b = letters[1:3]))
  r1$form <- "Form12"
  expect_identical(r1$misc$form, "Form12")
  r1$ft <- c(T, T, F, T)
  expect_identical(r1$ft, r1$misc$ft)
})



###############################################################################@
############################# length (Response) ################################
###############################################################################@

test_that("Test length (Response)", {
  n_items <- sample(10:40, 1)
  r1 <- response(sample(0:1, n_items, T))
  expect_identical(length(r1), n_items)

})


###############################################################################@
############################# convert_to_response ##############################
###############################################################################@

test_that("convert_to_response", {
  ip <- generate_ip(n = 8, item_id = paste0("i", 1:8))

  # ---------------------------------------------------------------------------#
  # If the resp is a Response_set and it's length is 1, then return the first
  # element
  resp <- generate_resp(ip = ip, theta = rnorm(1))
  resp <- resp[[1]]
  expect_identical(convert_to_response(resp), resp)

  # ---------------------------------------------------------------------------#
  # If the resp is a Response_set and it's length is 1, then return the first
  # element
  resp_set <- generate_resp_set(ip = ip, theta = rnorm(1))
  expect_identical(convert_to_response(resp_set), resp_set[[1]])

  # ---------------------------------------------------------------------------#
  # A Response set with length larger than 1 will raise an error
  resp_set <- generate_resp_set(ip = ip, theta = rnorm(10))
  expect_error(convert_to_response(resp_set),
               regexp = "Please select one Response object by")

  # ---------------------------------------------------------------------------#
  # A response string without an itempool
  resp <- generate_resp(ip = ip, theta = rnorm(1))
  resp <- resp[[1]]
  observed <- convert_to_response(resp$score)
  expect_identical(resp$score, observed$score)

  # ---------------------------------------------------------------------------#
  # A response string with an itempool
  resp <- generate_resp(ip = ip, theta = rnorm(1))
  resp <- resp[[1]]
  observed <- convert_to_response(resp$score, ip = ip)
  expect_identical(resp$score, observed$score)
  expect_identical(resp$item_id, observed$item_id)

  # ---------------------------------------------------------------------------#
  # An incorrect item pool will raise an error
  resp <- generate_resp(ip = ip, theta = rnorm(1))
  resp <- resp[[1]]
  expect_error(convert_to_response(resp$score, ip = ip[1:4]))

})






