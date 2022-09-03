# library(testthat)

###############################################################################@
############################# item  ############################################
###############################################################################@
test_that("Test item", {
  # args = list()
  # model = NULL
  # item_id = NULL
  # parameters = NULL
  # se = NULL
  # content = NULL
  # misc = NULL
  # -------------------------------------------------------------------------- #
  ### model + parameters  ###
  # D parameter may be missing.
  expect_s4_class(
    item(model = "2PL", parameters = list(a = 1.9, b = 1)), "Item")
  expect_s4_class(
    item(model = "2PL", parameters = list(a = 1.9, b = 1)), "2PL")
  expect_s4_class(
    item(model = "2PL", parameters = list(a = 1.9, b = 1, D = 1.7)), "2PL")

  ### model + without parameters ###
  expect_s4_class(item(model = "2PL", a = 1.9, b = 1, D = 1.7), "2PL")
  expect_s4_class(item(model = "2PL", a = 1.9, b = 1), "2PL")

  # Rasch model
  item <- item(b = 1,  model = "Rasch")
  expect_s4_class(item, "Item")
  expect_s4_class(item, "Rasch")
  expect_null(item$D)
  expect_null(item$a)

  # Create a GPCM model
  item <- item(parameters = list(a = 1.9, b = c(-1, 2), D = 1), model = "GPCM")
  expect_s4_class(item, "Item")
  expect_s4_class(item, "GPCM")
  expect_identical(item$model, "GPCM")
  # GPCM without D parameter
  item <- item(parameters = list(a = 1.9, b = c(-1, 2)), model = "GPCM")
  expect_s4_class(item, "Item")
  expect_s4_class(item, "GPCM")
  expect_identical(item$model, "GPCM")

  # Create a GPCM2 model
  item <- item(parameters = list(a = 1.9, b = 0.2, d = c(-1, 2), D = 1),
               model = "GPCM2")
  expect_s4_class(item, "Item")
  expect_s4_class(item, "GPCM2")
  expect_identical(item$model, "GPCM2")

  # Redundant parameters will be put in misc
  item <- item(a = 1.9, b = 1, c = .2,  model = "2PL")
  expect_s4_class(item, "Item")
  expect_s4_class(item, "2PL")
  expect_identical(item@misc$c, .2)

  # Names of the previous parameters should be removed
  item <- item(a = 1.9, b = c(a = -1, b = 1.2, c = 2.2), model = "GPCM")
  expect_null(names(item$b))

  # Additional parameters successfully added
  item <- item(a = 1.9, b = c(-1.1, 0.8, 1.6),  model = "GRM", item_id = "abc",
               content = "Geometry", misc = list(sympson_hetter_k = 1),
               se = list(a = .8, b = c(.2, .3, .4)))
  expect_s4_class(item, "Item")
  expect_s4_class(item, "GRM")
  expect_identical(item@item_id, "abc")
  expect_identical(item@content, "Geometry")
  expect_identical(item@misc, list(sympson_hetter_k = 1))
  expect_identical(item@se_a, .8)
  expect_identical(item@se_b, c(.2, .3, .4))

  # Additional parameters will be passed to misc field:
  item <- item(a = 1.9, b = 1, c = .2,  model = "2PL", key = "D", level = 11)
  expect_identical(item@misc$key, "D")
  expect_identical(item@misc$level, 11)

  # Additional parameters will be added to misc field:
  item <- item(a = 1.9, b = 1, c = .2,  model = "2PL",
               misc = list(test_id = "T123"),
               key = "D", level = 11)
  expect_identical(item@misc$key, "D")
  expect_identical(item@misc$level, 11)
  expect_identical(item@misc$test_id, "T123")


  ### No model but with parameters ###
  # Create a Rasch model
  item <- item(parameters = list(b = 1))
  expect_s4_class(item, "Item")
  expect_identical(item$model, "Rasch")
  expect_identical(item$b, 1)
  expect_null(item$D)

  # Create a 1PL model
  item <- item(parameters = list(b = 1, D = 1.7))
  expect_s4_class(item, "Item")
  expect_identical(item$model, "1PL")
  expect_identical(item$b, 1)
  expect_identical(item$D, 1.7)

  # Create a 2PL model
  item <- item(parameters = list(a = 1.9, b = 1))
  expect_s4_class(item, "Item")
  expect_identical(item$model, "2PL")
  expect_identical(item$a, 1.9)
  expect_identical(item$b, 1)

  # Create a 3PL model
  item <- item(parameters = list(a = 1.9, b = 1, c = .2))
  expect_s4_class(item, "Item")
  expect_identical(item$model, "3PL")
  expect_identical(item$a, 1.9)
  expect_identical(item$b, 1)
  expect_identical(item$c, .2)

  # Create a 4PL model
  item <- item(parameters = list(a = 1.9, b = 1, c = .2, d = .99))
  expect_s4_class(item, "Item")
  expect_identical(item$model, "4PL")
  expect_identical(item$a, 1.9)
  expect_identical(item$b, 1)
  expect_identical(item$c, .2)
  expect_identical(item$d, .99)

  # -------------------------------------------------------------------------- #
  # Create a GRM model
  item <- item(parameters = list(a = 1.9, b = c(-1, 2)),
               item_id = "i1", content = "Algebra")
  expect_s4_class(item, "Item")
  expect_s4_class(item, "GRM")
  expect_identical(item$model, "GRM")
  expect_identical(item@a, 1.9)
  expect_identical(item@b, c(-1, 2))
  expect_null(names(item@b))
  expect_identical(item@item_id, "i1")
  expect_identical(item@content, "Algebra")

  # -------------------------------------------------------------------------- #
  # Create a GPCM2 model
  item <- item(model = "GPCM2", a = 1.2, b = -.345, d = c(-1, 2))
  expect_s4_class(item, "Item")
  expect_s4_class(item, "GPCM2")
  expect_null(names(item@b))
  expect_identical(item@d, c(-1, 2))


  # -------------------------------------------------------------------------- #
  # Create a PCM model
  item <- item(parameters = list(b = c(x = -1, y = 2)))
  expect_s4_class(item, "Item")
  expect_s4_class(item, "PCM")
  expect_identical(item$model, "PCM")
  expect_null(names(item@b))
  expect_identical(item@b, c(-1, 2))

  # -------------------------------------------------------------------------- #
  # Additional arguments that are not parameters will be added as a misc field
  item <- item(parameters = list(a = 1.9, b = 1, c = .2), key = "B",
               test_id = "TestForm1", misc = list(level = "A1"))
  expect_s4_class(item, "Item")
  expect_identical(item$model, "3PL")
  expect_identical(item$a, 1.9)
  expect_identical(item$b, 1)
  expect_identical(item$c, .2)

  expect_identical(item@misc$key, "B")
  expect_identical(item@misc$test_id, "TestForm1")
  expect_identical(item@misc$level, "A1")

  # -------------------------------------------------------------------------- #
  ### No model or parameters ###
  # Create a 2PM model
  item <- item(a = 1.9, b = 1, c = .2)
  expect_s4_class(item, "Item")
  expect_s4_class(item, "3PL")
  expect_identical(item$model, "3PL")
  expect_identical(item@a, 1.9)
  expect_identical(item@b, 1)
  expect_identical(item@c, .2)

  # -------------------------------------------------------------------------- #
  # Unconventional vectors with only b parameters
  item <- item(b = list(1))
  expect_s4_class(item, "Item")
  expect_s4_class(item, "Rasch")
  expect_identical(item$model, "Rasch")
  item <- item(b = matrix(1))
  expect_s4_class(item, "Item")
  expect_s4_class(item, "Rasch")
  expect_identical(item$model, "Rasch")

  # -------------------------------------------------------------------------- #
  # Unnecessary parameters ("c") will be put in misc field
  item <- item(b = 1, c = .2)
  expect_s4_class(item, "Item")
  expect_s4_class(item, "Rasch")
  expect_identical(item$model, "Rasch")
  expect_identical(item@b, 1)
  expect_error(item@c, "no slot of name")
  expect_identical(item@misc$c, .2)

  # -------------------------------------------------------------------------- #
  # Create a GRM model
  item <- item(a = 1.9, b = c(-1, 2), item_id = "i1", content = "Algebra")
  expect_s4_class(item, "Item")
  expect_s4_class(item, "GRM")
  expect_identical(item$model, "GRM")
  expect_identical(item$a, 1.9)
  expect_identical(item$b, c(-1, 2))
  expect_identical(item$item_id, "i1")
  expect_identical(item$content, "Algebra")

  # -------------------------------------------------------------------------- #
  # Create a GRM model with individual b parameters
  item <- item(a = 1, b1 = -.473, b2 = 0.752, b3 = 1.266, D = 1, model  = "GRM")
  expect_identical(item$b, c(-.473, 0.752, 1.266))

  # -------------------------------------------------------------------------- #
  # Create a GRM model with shuffled individual b parameters
  item <- item(a = 1, b2 = 0.752, b1 = -.473, b3 = 1.266, D = 1, model  = "GRM")
  expect_identical(item$b, c(-.473, 0.752, 1.266))

  item <- item(b2 = 0.0062, b3 = 1.6774, a = 0.9485, b1 = -0.926, model = "GRM")
  expect_identical(item$b, c(-0.926, 0.0062, 1.6774))
  expect_identical(item$a, 0.9485)

  # -------------------------------------------------------------------------- #
  # Create a PCM model
  item <- item(b = c(-1, 2))
  expect_s4_class(item, "Item")
  expect_s4_class(item, "PCM")
  expect_identical(item$model, "PCM")
  expect_identical(item$b, c(-1, 2))

  # -------------------------------------------------------------------------- #
  # Create a M3PL model
  item <- item(a = c(1.9, 1.1), d = 1, c = .2)
  expect_s4_class(item, "Item")
  expect_s4_class(item, "M3PL")
  expect_identical(item$model, "M3PL")

  # -------------------------------------------------------------------------- #
  # Create a M2PL model
  item <- item(a = c(1.9, 1.1), d = 1)
  expect_s4_class(item, "Item")
  expect_s4_class(item, "M2PL")
  expect_identical(item$model, "M2PL")

  ### Other Cases ###
  # -------------------------------------------------------------------------- #
  # A row of a data frame can be used for creating items:
  dtf <- data.frame(a = c(1, 2), b1 = c(-2, 1), b2 = c(.2, .9))
  expect_s4_class(item(a = dtf$a[1], b = dtf[1, -1], model = "GRM"), "Item")
  expect_s4_class(item(a = dtf$a[1], b = dtf[1, -1], model = "GRM"), "GRM")
  expect_s4_class(item(a = dtf$a[1], b = dtf[1, -1]), "Item")
  expect_s4_class(item(a = dtf$a[1], b = dtf[1, -1]), "GRM")

  # -------------------------------------------------------------------------- #
  # Can process an Item class object within item() function
  item <- item(a = 1.9, b = c(-1.1, 0.8, 1.6),  model = "GRM")
  expect_s4_class(item(item), "Item")
  expect_s4_class(item(item), "GRM")
  expect_null(item$item_id)
  expect_null(item$content)
  expect_null(item$misc)
  item <- item(item, item_id = "abc",
               content = "Geometry", misc = list(sympson_hetter_k = 1),
               se = list(a = .864, b = c(.228, .306, .417)))
  expect_identical(item$item_id, "abc")
  expect_identical(item$content, "Geometry")
  expect_identical(item$se_a, .864)
  expect_identical(item$se_b, c(.228, .306, .417))

  # -------------------------------------------------------------------------- #
  # item() function should accept "id" "ID", "iD"
  item_id <- "xyz123"
  # item <- item(a = 1.9, b = 1, id = item_id)
  # expect_identical(item$item_id, item_id)
  # item <- item(a = 1.9, b = 1, Id = item_id)
  # expect_identical(item$item_id, item_id)
  # item <- item(a = 1.9, b = 1, iD = item_id)
  # expect_identical(item$item_id, item_id)
  # item <- item(a = 1.9, b = 1, ID = item_id)
  # expect_identical(item$item_id, item_id)
  item <- item(a = 1.9, b = 1, Item_id = item_id)
  expect_identical(item$item_id, item_id)
  item <- item(a = 1.9, b = 1, ITEM_ID = item_id)
  expect_identical(item$item_id, item_id)
  item <- item(a = 1.9, b = 1, item_id = item_id)
  expect_identical(item$item_id, item_id)
  item <- item(a = 1.9, b = 1, Item_ID = item_id)
  expect_identical(item$item_id, item_id)
  item <- item(a = 1.9, b = 1, Item_Id = item_id)
  expect_identical(item$item_id, item_id)

  # -------------------------------------------------------------------------- #
  # If, for example, for "GRM" model the parameters are given as b1, b2, b3,
  # then, it should be converted to b = c(b1, b2, b3).
  item <- item(a = 1, b1 = -1.35, b2 = -.08, b3 = 2.15, D = 1.7,
               content = "Alg", model = "GRM")
  expect_s4_class(item, "GRM")
  expect_identical(item@b, c(-1.35, -.08, 2.15))
  expect_identical(item@b[1], -1.35)
  expect_identical(item@b[3], 2.15)
  expect_identical(item@content, "Alg")


  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  ########################### Errors ##########################################@
  ## model ##
  # Model should be valid
  expect_error(item(model = "abc"), "Invalid 'model' specification.")

  # Model without parameters arguments but incomplete parameters
  expect_error(item(model = "2PL", a = 1, D = 1), "Incomplete parameters.")
  expect_error(item(model = "2PL", a = 1), "Incomplete parameters.")

  ## parameters ##
  expect_error(item(model = "2PL", parameters = list()),
               "Invalid item parameters")
  expect_error(item(model = "2PL", parameters = list(b = 1)),
               "Invalid item parameters")

  # Ambiguous parameters should raise error:
  expect_error(item(a = c(1, 2), b = c(-1, .2)),
               "Invalid parameters. For \"GRM\" model the size of the \"a\"")
  expect_error(item(a = c(1, 2, 3), b = c(-1, .2, 2)), "Invalid parameters.")
  expect_error(item(a = c(1, 2), b = c(-1, .2, 2)), "Invalid parameters.")

  # -------------------------------------------------------------------------- #
  # When there is no argument, an error should be raised
  expect_error(object = item(), "Insufficient parameters.")

  # -------------------------------------------------------------------------- #
  # Issue error if model is incorrectly specified
  expect_error(object = item(x = c(1,2,.2), model = "2PL"),
               "Incomplete parameters. Make sure to provide")
  expect_error(object = item(x = c(1,2,.2), model = "WeirdoModel"),
               "Invalid 'model' specification.")

  # -------------------------------------------------------------------------- #
  # Issue error if parameters are NULL or NA
  expect_error(object = item(NULL), "Insufficient parameters.")
  expect_error(object = item(NA), "Insufficient parameters.")

  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  ### Test numeric part ####################################################@###
  # 'item()' returns an "Item" class when an "Item" class entered.
  item <- item(new("2PL", a = 2, b = -.22, D = 1.702))
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "2PL")

  item <- item(a = 2, b = -.22, c = .24, d = .99)
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "4PL")

  item <- item(a = 1, b = 1.2)
  expect_s4_class(object = item, class = "Item")

  item <- item(b = c(2.11), item_id = "Item 12")
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "Rasch")

  item1 <- item(a = 2, b = -.22, c = .24, d = .99)
  expect_true(item1@b == -.22)

  # -------------------------------------------------------------------------- #
  # 1PL
  item <- item(b = 1.2, D = 1)
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "1PL")
  expect_identical(item@D, 1)

  item <- item(b = 1.2)
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "Rasch")

  item <- item(b = 1.2, D = 1)
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "1PL")

  item <- item(b = c(-1, 2, 1))
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "PCM")

  # -------------------------------------------------------------------------- #
  # 2PL
  item <- item(a = 1, b = 1.2, D = 1)
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "2PL")
  expect_identical(item@D, 1)

  item <- item(a = 1, b = 1.2)
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "2PL")

  # TODO: The following gives error, solve this.
  item <- item(a = 1.38, b = -.81, D = 1.2, model = "2PL")
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "2PL")
  expect_identical(item@a, 1.38)
  expect_identical(item@b, -0.81)
  expect_identical(item@D, 1.2)

  # -------------------------------------------------------------------------- #
  # 3PL
  item <- item(a = 1, b = 2, c = .2, D = 1.7)
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "3PL")

  item <- item(a = 1, b = 2, c = .2)
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "3PL")
  expect_identical(item@D, 1)

  item <- item(a = 1, b = 0.02173983, c = 0, D = 0.7636914, model = "3PL")
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "3PL")
  expect_identical(item@D, 0.7636914)

  # -------------------------------------------------------------------------- #
  # 4PL
  item <- item(a = 1, b = 2, c = .2, d = .99, D = 1.1)
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "4PL")
  expect_identical(item@b, 2)
  expect_identical(item@d, .99)
  expect_identical(item@D, 1.1)

  # Default value of D
  item <- item(a = 1, b = 1.2)
  expect_identical(item$D, 1)

  # Test the IDs are "Item-xx"
  ip <- item(b = rnorm(10))
  expect_null(ip@item_id)

  # -------------------------------------------------------------------------- #
  # Create Items for Graded Response Model
  # The default method is GRM when no model presented and there are multiple
  # b parameter values.
  item <- item(parameters = list(a = 1.2, b = c(-2.3, -1.2, 0.4, 1.3), D = 1))
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "GRM")

  item <- item(parameters = list(a = 1.2, b = c(-2.3, -1.2, 0.4, 1.3), D = 1),
               model = "GRM")
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "GRM")

  # ignore D
  item <- item(parameters = list(a = 1.2, b = c(-2.3, -1.2, 0.4, 1.3)))
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "GRM")
  expect_identical(item@D, 1)

  # -------------------------------------------------------------------------- #
  # Create Items for Generalized Partial Credit Model
  item <- item(parameters = list(a = 1.2, b = c(-2.3, -1.2, 0.4, 1.3),
                                 D = 1.702),
                  model = "GPCM")
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "GPCM")

  # -------------------------------------------------------------------------- #
  item <- item(b = 2.11, c = .22, a = 1, item_id = "Item 11",
               content = "Math-12",
               se = list(a = .237, b = .854, c = .466))
  expect_s4_class(object = item, class = "Item")
  expect_s4_class(object = item, class = "3PL")
  expect_true(all(item@item_id == "Item 11", item$model == "3PL",
                  item@b == 2.11, item1@se_c == .1,
                  item@content == "Math-12"))
  expect_identical(item$se_a, .237)
  expect_identical(item$se_b, .854)
  expect_identical(item$se_c, .466)

  # -------------------------------------------------------------------------- #
  expect_error(item(b = 1, model = "abc"),
               "Invalid 'model' specification. Model name should be one of ")

  # -------------------------------------------------------------------------- #
  # Another thing to pass
  item <- item(a = 1, b = c(-1, 0, 1), D = 1)
  expect_identical(item$model, 'GRM')

  # -------------------------------------------------------------------------- #
  # Correctly specify an incorrectly ordered parameter vector
  item <- item(b = 2.11, c = .22, a = 1)
  expect_true(item@b == 2.11)


  # -------------------------------------------------------------------------- #
  # Check whether the names of parameters removed
  dtf <- data.frame(a = c(1, 2), xxb1 = c(-2, 1), xxb2 = c(.2, .9))
  i = 1
  item <- item(a = dtf$a[i], b = unlist(dtf[i, paste0('xxb', 1:2)]))
  expect_null(names(item@a))
  expect_null(names(item@b))

  # -------------------------------------------------------------------------- #
  # Can add standard errors using 'se' argument
  item <- item(a = 1, b = 2, c = .2, D = 1.7,
               se = list(a = .53, b = .84, c = .293))
  expect_identical(item@se_a, .53)
  expect_identical(item@se_b, .84)
  expect_identical(item@se_c, .293)

  # -------------------------------------------------------------------------- #
  # Can add standard errors using 'se_a', "se_b" etc arguments
  item <- item(a = 1, b = 2, c = .2, D = 1.7, se_a = .235, se_c = .758)
  expect_identical(item@se_a, .235)
  expect_null(item@se_b)
  expect_identical(item@se_c, .758)

  # -------------------------------------------------------------------------- #
  # D parameter can be passed to item() function
  itm <- item(a = 1, b = 2)
  expect_true(is.null(itm@misc))
  expect_identical(itm@D, 1)
  itm <- item(itm, D = 2.456)
  expect_identical(itm@D, 2.456)
  # Make sure redundant misc field is not added
  expect_true(is.null(itm@misc))
})


###############################################################################@
############################# is.Item  #########################################
###############################################################################@
# Test is.Item
test_that("Test is.Item", {
  expect_true(object = is.Item(new("2PL", a = 2, b = -.22, D = 1.702)))
  expect_true(object = is.Item(new("M2PL", a = c(1,2), d = -.22, D = 1)))
})

###############################################################################@
############################# print.Item  ######################################
###############################################################################@
# Test print.Item and show.Item
test_that("Test show.Item", {
  # IRT-2PM
  item <- new("2PL",b = 2.7, a = 1.3, D = 1)
  expect_output(print(item), regexp = "2PL")
  expect_output(print(item), regexp = "A '2PL' item.")
  expect_output(print(item),
                regexp = "Two-Parameter Logistic Model")
  # IRT-3PM
  item <- new("3PL", b = 2, c = .12, a = 1.2, D = 1)
  expect_output(print(item), regexp = "a = 1.2")
  expect_output(print(item), regexp = "b = 2")
  expect_output(print(item), regexp = "c = 0.12")
  expect_output(print(item),
                regexp = "Three-Parameter Logistic Model")
  # IRT-3PM - Print Content
  item <- new("3PL", content = "C-1.2", item_id = "MyItem122",
              b = 2, c = .123, a = 1.2, D = 1.7)
  expect_output(print(item), regexp = "ID:")
  expect_output(print(item), regexp = "Content:")
  expect_output(print(item), regexp = " C-1.2", fixed = TRUE)
  expect_output(print(item),
                regexp = "Three-Parameter Logistic Model")
  # MIRT-3PM
  item <- new("M3PL", content = "C-12",d = 2, c = .12, a = c(1,1.2), D = 1.7)
  expect_output(print(item), regexp = "a = 1;  1.2")
  # GRM
  item <- item(a = 1.2, b = c(-2, 1, 2), model = "GRM")
  expect_output(print(item), regexp = "GRM")
  expect_output(print(item), regexp = "b = -2;  1;  2")
  expect_output(print(item), regexp = "Graded Response Model")

  # misc field printed properly:
  item <- item(a = 1.2, b = -0.94, item_id = "item1", content = "Earth Science",
               misc = list(key = "C",  operational = TRUE, type = "MC",
                           enemies = c("i2", "i3"), ep = c(1.2, -2.11, 3.1),
                           np = list(x = 1, y = 2)))
  expect_output(print(item), regexp = "key: \"C\"")
  expect_output(print(item), regexp = "operational: TRUE")
  expect_output(print(item), regexp = "type: \"MC\"")
  expect_output(print(item), regexp = "enemies: \"i2\", \"i3\"")
})


###############################################################################@
############################# '$' method #######################################
###############################################################################@
# Test '$' method
test_that("Test $ method ", {
  misc <- list(key = "B", strand = c("S12.03", "S42.12"))
  item <- item(a = 1, b = .2, c = .3, item_id = 'myid', content = 'Algebra',
               misc = misc)
  expect_identical(item$a, 1)
  expect_identical(item$b, .2)
  expect_identical(item$c, .3)
  expect_identical(item$D, 1)
  expect_null(item$d) # Non-exsitent parameter
  expect_identical(item$item_id, 'myid')
  expect_identical(item$parameters, list(a = 1, b = .2, c = .3, D = 1))
  expect_identical(item$content, 'Algebra')
  expect_identical(item$model, '3PL')
  expect_identical(item$max_score, 1L)

  expect_identical(item$misc, misc)
  expect_identical(item$misc$key, misc$key)
  expect_identical(item$key, misc$key)
  expect_true(is.null(item$xyz))

  # -------------------------------------------------------------------------- #
  # se parameters
  item@se_a <- .234
  item@se_b <- .567
  expect_identical(item$se, list(se_a = .234, se_b = .567, se_c = NULL))

  # -------------------------------------------------------------------------- #
  # 'max_score'
  item <- generate_item(model = "GRM", n_categories = 5)
  expect_identical(item$max_score, 4L)

})

###############################################################################@
############################# '$<-' method #####################################
###############################################################################@
# Test '$<-' method
test_that("Test $<- method ", {
  item <- item(a = 1, b = .2, c = .3, item_id = 'myid', content = 'Algebra')
  expect_identical(item$a, 1)
  expect_identical(item$b, .2)
  expect_identical(item$c, .3)
  expect_identical(item$D, 1)
  # expect_identical(item$item_id, 'myid')
  expect_identical(item$item_id, 'myid')
  expect_identical(item$content, 'Algebra')
  expect_identical(item$model, '3PL')
  item$a <- 2
  item$b <- -1
  item$se_b <- .873
  item$c <- .41
  item$D <- 1.1
  item$d <- .99   # Non-exsitent parameter
  item$item_id <- "New_ID"
  item$content <- "Geometry"
  expect_identical(item$a, 2)
  expect_identical(item$b, -1)
  expect_identical(item$se_b, .873)
  expect_null(item$se_a)
  expect_null(item$se_c)
  expect_identical(item$c, .41)
  expect_identical(item$D, 1.1)
  expect_null(item$xyz)  # Non-exsitent parameter
  expect_identical(item$item_id, 'New_ID')
  expect_identical(item$content, 'Geometry')

  item$a = 1.52 # equal sign works as if `<-`
  expect_identical(item$a, 1.52)

  # expect_identical(item$model, '3PL')

  # -------------------------------------------------------------------------- #
  # Assigning a negative standard error should raise an error
  expect_error(item$se_c <- -2,
               paste0("Invalid 'se_c' values. Standard error values of item ",
                      "parameters cannot be smaller than 0."))
  expect_null(item$se_c)

  # -------------------------------------------------------------------------- #
  # One cannot change the item's model or parameters
  item <- item(a = 1, b = .2, c = .3, item_id = 'myid', content = 'Algebra')
  expect_identical(item$model, '3PL')
  item$model <- "Rasch"
  expect_identical(item$model, "3PL")


  # -------------------------------------------------------------------------- #
  # if the name is not a slot name, it will be added as a misc field.
  i1 <- generate_item()
  i1$foo <- "BAR"
  expect_identical(i1@misc$foo, "BAR")

  # -------------------------------------------------------------------------- #
  # Assigning NULL to misc value removes that field
  i1 <- generate_item()
  i1$foo <- "BAR"
  expect_identical(i1@misc$foo, "BAR")
  i1$foo <- NULL
  expect_true(is.null(i1@misc$foo))

  # But removing an essential slot will raise an error:
  expect_error(i1$a <- NULL)

  })


###############################################################################@
############################# as.data.frame.Item ###############################
###############################################################################@
# Test '$<-' method
test_that("Test as.data.frame.Item", {

  # -------------------------------------------------------------------------- #
  # 4PL item
  item <- generate_item(model = "4PL")
  dtf <- as.data.frame(item)
  expect_identical(dtf$model, "4PL")
  expect_identical(item@a, dtf$a)
  expect_identical(item@b, dtf$b)
  expect_identical(item@c, dtf$c)
  expect_identical(item@d, dtf$d)
  expect_null(dtf$se_a)
  expect_null(dtf$item_id)
  expect_null(dtf$content)

  # -------------------------------------------------------------------------- #
  # 3PL item without se parameters
  item <- generate_item(model = "3PL", se = TRUE)
  dtf <- as.data.frame(item)
  expect_identical(item@se_a, dtf$se_a)
  expect_identical(item@se_b, dtf$se_b)
  expect_identical(item@se_c, dtf$se_c)
  dtf <- as.data.frame(item, include_se = FALSE)
  expect_null(dtf$se_a)
  expect_null(dtf$se_b)
  expect_null(dtf$se_c)

  # -------------------------------------------------------------------------- #
  # Rasch item
  item <- generate_item(model = "Rasch", item_id = "i1", content = "Algebra",
                        misc = list(type = "MC", op = TRUE, c("i1", "i2")))
  item@se_b <- runif(1)
  dtf <- as.data.frame(item)
  expect_identical(dtf$model, "Rasch")
  expect_identical(item@b, dtf$b)
  expect_identical(item@se_b, dtf$se_b)
  expect_identical(item@item_id, dtf$item_id)
  expect_identical(item@content, dtf$content)
  expect_identical(item@misc$type, dtf$type)
  expect_identical(item@misc$op, dtf$op)

  # -------------------------------------------------------------------------- #
  # GRM item
  item <- generate_item(model = "GRM", item_id = "i1", content = "Algebra",
                        n_categories = 5,
                        misc = list(type = "MC", op = TRUE))
  item@se_a <- runif(1)
  item@se_b <- runif(length(item@b))
  dtf <- as.data.frame(item)
  expect_identical(dtf$model, "GRM")
  expect_identical(item@b[1], dtf$b1)
  expect_identical(item@b[4], dtf$b4)
  expect_identical(item@se_b[2], dtf$se_b2)
  expect_identical(item@se_b[3], dtf$se_b3)
  expect_identical(item@item_id, dtf$item_id)
  expect_identical(item@content, dtf$content)
  expect_identical(item@misc$type, dtf$type)
  expect_identical(item@misc$op, dtf$op)


  # -------------------------------------------------------------------------- #
  # GRM item without se parameters
  item <- generate_item(model = "GRM", se = TRUE)
  dtf <- as.data.frame(item)
  expect_identical(item@se_b[1], dtf$se_b1)
  expect_identical(item@se_a, dtf$se_a)
  dtf <- as.data.frame(item, include_se = FALSE)
  expect_null(dtf$se_b1)
  expect_null(dtf$se_a)

  # -------------------------------------------------------------------------- #
  # GPCM2 item
  item <- generate_item(model = "GPCM2", item_id = "i1", content = "Algebra",
                        n_categories = 5,
                        misc = list(type = "MC", op = TRUE))
  item@se_a <- runif(1)
  item@se_b <- runif(1)
  item@se_d <- runif(length(item@d))
  dtf <- as.data.frame(item)
  expect_identical(dtf$model, "GPCM2")
  expect_identical(item@a, dtf$a)
  expect_identical(item@b, dtf$b)
  expect_identical(item@d[1], dtf$d1)
  expect_identical(item@d[4], dtf$d4)
  expect_identical(item@se_d[2], dtf$se_d2)
  expect_identical(item@se_d[3], dtf$se_d3)
  expect_identical(item@item_id, dtf$item_id)
  expect_identical(item@content, dtf$content)
  expect_identical(item@misc$type, dtf$type)
  expect_identical(item@misc$op, dtf$op)

  # -------------------------------------------------------------------------- #
  # M3PL item
  item <- item(model = "M3PL", a = c(1.2, 1.8, 2.3), d = 1, c = .2)
  item@se_a <- runif(3)
  item@se_d <- runif(1)
  dtf <- as.data.frame(item)
  expect_identical(dtf$model, "M3PL")
  expect_identical(item@d, dtf$d)
  expect_identical(item@a[1], dtf$a1)
  expect_identical(item@a[3], dtf$a3)
  expect_identical(item@se_a[2], dtf$se_a2)
  expect_identical(item@se_a[3], dtf$se_a3)

})


###############################################################################@
############################# get_slot_names_item ##############################
###############################################################################@

test_that("get_slot_names_item", {

  # -------------------------------------------------------------------------- #
  # GPCM item
  itm <- generate_item(model = "GPCM", n_categories = 3)
  expect_true(setequal(c("a", "b", "D"),
                       get_slot_names_item(itm, type = "pars")))
  expect_true(setequal(c("a", "b1", "b2", "D"),
                       get_slot_names_item(itm, type = "pars_df")))
  expect_true(setequal(c("se_a", "se_b"),
                       get_slot_names_item(itm, type = "se")))
  # since there are no se_parameters set
  expect_null(get_slot_names_item(itm, type = "se_df"))
  expect_true(setequal(slotNames(itm),
                       get_slot_names_item(itm, type = "all")))
  expect_true(setequal(c(get_slot_names_item(itm, "pars"),
                         get_slot_names_item(itm, "se")),
                       get_slot_names_item(itm, c("pars", "se"))))

  # -------------------------------------------------------------------------- #
  # 4PL item
  itm <- generate_item(model = "4PL")
  expect_true(setequal(c("a", "b", "c", "d", "D"),
                       get_slot_names_item(itm, type = "pars")))
  expect_true(setequal(c("se_a", "se_b", "se_c", "se_d"),
                       get_slot_names_item(itm, type = "se")))
  expect_true(setequal(slotNames(itm),
                       get_slot_names_item(itm, type = "all")))
  expect_true(setequal(c(get_slot_names_item(itm, "pars"),
                         get_slot_names_item(itm, "se")),
                       get_slot_names_item(itm, c("pars", "se"))))
})
