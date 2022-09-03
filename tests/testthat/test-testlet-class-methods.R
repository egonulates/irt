
# library(testthat)

###############################################################################@
################################ testlet #######################################
###############################################################################@

# Test concatenation function "c" of "Itempool" class
test_that("testlet", {
  i1 <- item(b = rnorm(1))
  i2 <- item(b = rnorm(1))
  i3 <- item(b = rnorm(1), item_id = "mi-item-1")
  i4 <- item(b = rnorm(1), item_id = "myitem4")
  # A sequence of items
  expect_s4_class(t1 <- testlet(i1, i2, i3, i4), 'Testlet')
  expect_identical(t1@model, 'BTM')
  expect_s4_class(t1 <- testlet(i1, i2, i3, i4, testlet_id = "my_teslet1"), 'Testlet')
  expect_identical(t1@testlet_id, 'my_teslet1')
  # The order of items inthe item_list should follow: i1, i2, i3, i4
  expect_identical(t1@item_list$item_id, c("Item_1", "Item_2", "mi-item-1", "myitem4"))
  # A list of items
  expect_s4_class(t1 <- testlet(list(i1, i2, i3, i4)), 'Testlet')
  expect_identical(t1@item_list[[2]]@item_id, 'Item_2')
  expect_s4_class(t1 <- testlet(list(i1, i2, i3, i4), testlet_id = 'mt1'), 'Testlet')
  expect_identical(t1@testlet_id, 'mt1')
  # an Itempool
  ip <- itempool(list(i1, i2, i3, i4))
  expect_s4_class(t1 <- testlet(ip), 'Testlet')
  expect_s4_class(t1 <- testlet(ip, testlet_id = 'mt1'), 'Testlet')
  expect_identical(t1@testlet_id, 'mt1')
  # a data frame
  ip_dtf <- data.frame(a = runif(10, .5, 1.5), b = rnorm(10))
  expect_s4_class(t1 <- testlet(ip_dtf, testlet_id = 'mt1'), 'Testlet')
  expect_identical(t1@testlet_id, 'mt1')

  # Testlet with 'misc' field.
  t1 <- testlet(itempool(b = rnorm(2), item_id = paste0("t1-i", 1:2),
                         misc = list(list(sympson_hetter_k = .8, form = "b3"),
                                     list(sympson_hetter_k = .9))),
                   testlet_id = "t1")
  expect_s4_class(t1, "Testlet")

  # -------------------------------------------------------------------------- #
  # Convert a complicated data frame into a testlet
  t_dtf <- structure(list(item_id = c("t1_Item_1", "t1_Item_2"),
                          model = c("GPCM", "3PL"),
                          a = c(0.6882, 1.0781),
                          b = c(NA, -0.0519),
                          c = c(NA, 0.1116),
                          b1 = c(0.1523, NA),
                          b2 = c(0.5282, NA),
                          b3 = c(1.3779, NA),
                          D = c(1, 1),
                          key = c(NA, "B"),
                          possible_options = list(NA, c("A", "B", "C", "D"))),
             row.names = 1:2, class = "data.frame")
  t1 <- testlet(t_dtf)
  expect_s4_class(t1, "Testlet")
  expect_true(is.data.frame(as.data.frame(t1)))

})

###############################################################################@
############################# $ method (Testlet) ###############################
###############################################################################@

test_that("$ method (Testlet)", {
  i1 <- item(a = 1.2, b = 0.2, c = .1, content = "Geo", item_id = "an1")
  i2 <- item(a = 1.38, b = -2.1, c = .2, content = "Geo", item_id = "an2")
  i3 <- item(a = 1.38, b = -1.1, content = "Geo", item_id = "an3")
  t1 <- testlet(c(i1, i2, i3), testlet_id = "testlet--1", content = "Alg",
                misc = list(skill = "Number Sense"))
  expect_identical(t1$testlet_id, "testlet--1")
  expect_identical(t1$content, "Alg")
  expect_null(t1$parameters)
  expect_type(t1$item_list, "list")
  expect_true(all(sapply(t1$item_list, is.Item)))
  expect_identical(t1$item_id, c(i1$item_id, i2$item_id, i3$item_id))
  expect_identical(t1$item_models, setNames(c("3PL", "3PL", "2PL"), t1$item_id))
  expect_identical(t1$misc, list(skill = "Number Sense"))
  expect_identical(t1$skill, "Number Sense")

  # $max_score
  t1 <- testlet(generate_ip(model = c("2PL", "GRM", "3PL", "GRM"),
                            n_categories = c(2, 3, 2, 6)),
                testlet_id = "testlet--1", content = "Alg")
  expect_identical(t1$max_score, 9)

})

###############################################################################@
############################# $<- method (Testlet) #############################
###############################################################################@

test_that("$<- method (Testlet)", {
  i1 <- item(a = 1.2, b = 0.2, c = .1)
  i2 <- item(a = 1.38, b = -2.1, c = .2)
  i3 <- item(a = 1.38, b = -1.1)
  t1 <- testlet(c(i1, i2, i3), testlet_id = "t1")
  # testlet_id
  expect_identical(t1@testlet_id, "t1")
  t1$testlet_id <- "t2"
  expect_identical(t1@testlet_id, "t2")
  t1$testlet_id <- "t55"
  expect_identical(t1@testlet_id, "t55")
  # item_list
  expect_identical(t1$item_list[[1]]$a, 1.2)
  t1$item_list <- convert_model(itempool(t1$item_list), "Rasch")
  expect_null(t1$item_list[[1]]$a)
  # Content
  expect_null(t1@content)
  t1$content <- "Complex Numbers"
  expect_identical(t1@content, "Complex Numbers")
  # misc
  expect_null(t1@misc)
  t1$skill <- "Arithmetic"
  expect_identical(t1@misc$skill, "Arithmetic")
  t1$form_no <- c("Form-1", "Form-22")
  expect_identical(t1@misc$form_no, c("Form-1", "Form-22"))
  expect_identical(length(t1@misc), 2L)

  t1$form_no <- NULL
  expect_equal(t1@misc, list(skill = "Arithmetic"))
  # Assigning NULL can make the whole misc field empty if there is only one
  # misc field
  t1$skill <- NULL
  expect_null(t1@misc)

})

