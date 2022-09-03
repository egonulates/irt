
# library(testthat)

###############################################################################@
############################# Response_set #####################################
###############################################################################@

test_that("Test Response_set-class", {
  r1 <- new("Response", examinee_id = "Stu1", item_id = c("i1", "i3"),
            score = c(0, 1))
  r2 <- new("Response", examinee_id = "Stu2", item_id = c("i5", "i2"),
            score = c(1, 1))

  expect_s4_class(
    object = new("Response_set",
                 response_list = list(r1, r2),
                 item_id = c("i1", "i3", "i5", "i2"),
                 misc = list(admin_date = "12-23-2021")),
    class = "Response_set")

  # A minimal Response object:
  expect_error(object = new("Response_set", response_list = list()),
               "Invalid 'response_list'. All of the elements of")
})


###############################################################################@
############################# setValidity Response_set #########################
###############################################################################@


test_that("Test setValidity of Response-class", {

  # ###################### Check 'response_list' ###########################@###
  # All of the examinee ID's should be unique.
  ip <- generate_ip(n = 5, item_id = paste0("i", 1:5))
  r1 <- new("Response", examinee_id = "S1", item_id = c("i1", "i3"),
            score = c(0, 1))
  r2 <- new("Response", examinee_id = "S2", item_id = c("i5", "i2", "i1"),
            score = c(1, 1, 0))
  r3 <- new("Response", examinee_id = "S2", item_id = c("i5", "i2", "i1", "i3"),
            score = c(0, 1, 1, 0))
  # Response with duplicated examinee_id
  r_duplicated_eid <- new("Response", examinee_id = "S1",
                          item_id = c("i5", "i2"), score = c(1, 1))
  # Response with examinee_id NULL
  r_null_eid <- new("Response", examinee_id = NULL, item_id = c("i2", "i1"),
                    score = c(1, 1))

  ### TESTS ###

  # -------------------------------------------------------------------------- #
  # Duplicated examinee ID's is not allowed
  expect_error(new("Response_set",
                   response_list = list(r1, r2, r_duplicated_eid),
                   item_id = c("i1", "i3", "i5", "i2")
                   ),
               "Invalid 'response_list'. The 'examinee_id's of")
  # One examinee ID can be NULL
  expect_s4_class(new("Response_set",
                      response_list = list(r1, r2, r_null_eid),
                      item_id = c("i1", "i3", "i5", "i2")),
                  "Response_set")

  # # -------------------------------------------------------------------------- #
  # # All of the item ID's should match the item ID's of the item pool
  # # Create a response for which item ID do not match the item ids of the
  # # item pool
  # r_item_id <- new("Response", examinee_id = NULL, item_id = c("i2", "XYX"),
  #                   score = c(1, 1))
  # expect_error(new("Response_set", response_list = list(r1, r2, r_item_id)),
  #              "Invalid 'response_list'. All of the items ID's")

  # -------------------------------------------------------------------------- #
  # In response list, all of the elements should be Response object.
  expect_error(new("Response_set", response_list = list(r1, r2, b =  1:3)),
               "Invalid 'response_list'. All of the elements of 'response_list")

})
