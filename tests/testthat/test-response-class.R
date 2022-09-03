
# library(testthat)

test_that("Test Response-class", {
  expect_s4_class(
    object = new("Response",
                 examinee_id = "Stu12",
                 item_id = c("Item1", "Item2", "Item3", "Item4"),
                 score = c(0, 1, 1, 1),
                 order = c(1L, 2L, 3L, 4L),
                 misc = list(item_role = c("F", "O", "O", "O"))
                 ),
    class = "Response")

  # A minimal Response object:
  expect_s4_class(
    object = new("Response", item_id = "I1", score = 1),
    class = "Response")

  # # All slots are NULL in minimal response object
  r <- new("Response", item_id = "I1", score = 1)
  expect_null(r@examinee_id)
  expect_identical(r@item_id, "I1")
  expect_null(r@testlet_id)
  expect_identical(r@score, 1)
  expect_null(r@raw_response)
  expect_null(r@order)
  expect_null(r@response_time)
  expect_null(r@misc)
})



test_that("Test setValidity of Response-class", {

  ###################### 'examinee_id' #####################################@###
  # examinee_id's length should be 1
  expect_error(new("Response", examinee_id = c("a", "b")))
  # Incorrect type
  expect_error(new("Response", examinee_id = 12))


  ###################### 'item_id' #########################################@###
  # Duplicated item_id's are not allowed
  expect_error(new("Response", item_id = c("i1", "i2", "i1")))

  ###################### 'score' ###########################################@###
  # 'score' and 'item_id' should have the same length.
  expect_error(new("Response", item_id = c("i1", "i2", "i3"), score = c(0, 1)),
               "'item_id' and 'score' should have the same lengths.")

  # -------------------------------------------------------------------------- #
  # There cannot be any NA in the score slot.
  expect_error(new("Response", item_id = c("i1", "i2", "i3"),
                   score = c(0, 1, NA)),
               "Invalid 'score' vector. Score values cannot be missing.")


  ###################### 'raw_response' ####################################@###
  expect_error(new("Response", item_id = c("i1", "i2", "i3"),
                   raw_response = c("A", "B", "B", "D")),
               "'item_id' and 'raw_response' should have the same lengths.")

  ###################### 'order' ###########################################@###
  expect_error(new("Response", item_id = c("i1", "i2", "i3"),
                   score = 1:3,
                   order = 1L:6L),
               )

  # 2021-05-09: I abandon the restriction of ordered item order variables.
  # There is a possibility that a multi-part item can be administered.
  expect_s4_class(new("Response", item_id = c("i1", "i2", "i3"),
                      score = 1:3, order = c(1L, 4L, 4L)), "Response")
             # "Invalid 'order' vector. The values of 'order' vector cannot be")
  # expect_error(new("Response", item_id = c("i1", "i2", "i3"),
  #                  score = 1:3,
  #                  order = c(1L, 3L, 4L)),
  #              "Invalid 'order' vector. The values of")

  ###################### 'response_time' ###################################@###
  expect_error(new("Response", item_id = c("i1", "i2", "i3"),
                   score = 1:3,
                   response_time = c(22, 55)),
               "Invalid 'response_time' vector. The length of")

  expect_error(new("Response", item_id = c("i1", "i2", "i3"),
                   score = 1:3,
                   response_time = c(22, 12, -3)),
               "Invalid 'response_time' vector. The values of")
})
