# library(testthat)
# Test setValidity of "Item" class

test_that("Test setValidity of Item class", {
  # irt items
  expect_s4_class(new("2PL", a = 2, b = -.22, D = 1.702), class = "Item")
  expect_s4_class(new("2PL", a = 2, b = -.22, D = 1.702), class = "2PL")
  expect_s4_class(new("3PL", a = 2, b = -.22, c = 0, D = 1.702), class = "3PL")
  expect_s4_class(new("3PL", a = 2, b = -.22, c = 0, D = 1.702, item_id = "MyItem12",
                      content = "2"), class = "Item")
  expect_s4_class(new("3PL", a = 2, b = -.22, c = 0, D = 1.702, item_id = "MyItem12",
                      content = "2"), class = "3PL")
  expect_s4_class(new("1PL", b = 2, D = 1.702, item_id = "MyItem1", content = "1.2"),
                  class = "1PL")
  expect_s4_class(new("1PL", b = 2, D = 1.702, se_b = .12, item_id = "MyItem12",
                      content = "1.2"), class = "1PL")
  expect_s4_class(new("3PL", b = 2, c = .12, a = 1.2, D = 1.702, se_c = .12,
                      se_a = .15, se_b = .17, item_id = "MyItem12", content = "1.2"),
                  class = "3PL")

  # Test the default values of "Item"
  item <- new("Item")
  expect_s4_class(item, "Item")
  expect_null(item@content)
  expect_null(item@item_id)
  expect_null(item@misc)


  # -------------------------------------------------------------------------- #
  # Defaults for models:
  item <- new("Rasch")
  expect_identical(item@b, 0)
  expect_null(item@item_id)
  expect_null(item@content)

  item <- new("1PL")
  expect_identical(item@b, 0)
  expect_identical(item@D, 1)
  expect_null(item@item_id)
  expect_null(item@content)

  item <- new("2PL")
  expect_identical(item@a, 1)
  expect_identical(item@b, 0)
  expect_identical(item@D, 1)
  expect_null(item@item_id)
  expect_null(item@content)

  item <- new("3PL")
  expect_identical(item@a, 1)
  expect_identical(item@b, 0)
  expect_identical(item@c, 0)
  expect_identical(item@D, 1)
  expect_null(item@item_id)
  expect_null(item@content)

  item <- new("4PL")
  expect_identical(item@a, 1)
  expect_identical(item@b, 0)
  expect_identical(item@c, 0)
  expect_identical(item@d, 1)
  expect_identical(item@D, 1)
  expect_null(item@item_id)
  expect_null(item@content)

  # TODO: add more default tests for GPCM, GRM, M2PL, etc.

  # -------------------------------------------------------------------------- #
  # default b parameter
  item <- new("3PL", 2.11, c = .22, a = 1, D = 1.702)
  expect_identical(item@b, 2.11)

  # -------------------------------------------------------------------------- #
  # mirt items
  # expect_s4_class(new("M1PL", d = 1.2, D = 1), class = "Item")
  expect_s4_class(new("M2PL", d = 1.2, a = c(1, 1.2), D = 1), class = "M2PL")
  expect_s4_class(new("M3PL", d = 1.2, a = c(1, 1.2), c = .1, D = 1), "M3PL")

  # -------------------------------------------------------------------------- #
  # Generalized Partial Credit items
  expect_s4_class(new("GPCM", b = c(-3, -.5, 1.5), a = 1, D = 1),
                  class = "GPCM")

  # -------------------------------------------------------------------------- #
  # Reparametrized Generalized Partial Credit items
  expect_s4_class(new("GPCM2", d = c(-3, -.5, 1.5), b = 0.4, a = 1, D = 1),
                  class = "GPCM2")
  # An error will be raised if user switches b with d (b should have length 1)
  expect_error(new("GPCM2", b = c(-3, -.5, 1.5), d = 0.4, a = 1, D = 1),
               "Invalid parameters. For \"GPCM2\" model the size of the \"b\"")


  ########################### ERROR CASES #####################################@
  # -------------------------------------------------------------------------- #
  ###### item_id ######@
  # item_id should have length 1 or NULL
  expect_error(new("2PL", a = 2, b = -.22, D = 1.702, item_id = c("Item1", "Item2")),
               regexp = "Invalid 'item_id'.")

  # -------------------------------------------------------------------------- #
  ###### parameters ######@
  # Item parameters cannot have NA in their values
  expect_error(
    new("3PL", a = 2, b = -.22, c = NA),
    # regexp = "Invalid parameter. Item parameters cannot be NULL or NA.")
    "assignment of an object of class (.*)logical(.*) is not valid for @(.*)c")
  expect_error(
    new("3PL", a = 2, b = -.22, c = as.numeric(NA)),
    paste0("Invalid parameters. The values of \"c\" parameters should be ",
           "between 0 and 1."))
  # Item parameter values cannot be non-numeric
  expect_error(
    new("1PL", b = "ABC", D = 1.1),
    # regexp = "Invalid parameters. All parameters should be numeric.")
    "assignment of an object of class (.*)character(.*) is not valid for @(.*)b")

  # -------------------------------------------------------------------------- #
  # c Parameter cannot be larger than 1 or smaller than 0.
  expect_error(new("3PL", a = 1, b = 2, c = 3, D = 1.702),
               regexp = "Invalid parameters. The values of \"c\" parameters")
  expect_error(new("3PL", a = 1, b = 2, c = -0.1, D = 1.702),
               regexp = "Invalid parameters. The values of \"c\" parameters")
  expect_error(new("M3PL", d = 1.2, a = c(1, 1.2), c = 1.1, D = 1),
               regexp = "Invalid parameters. The values of \"c\" parameters")

  # -------------------------------------------------------------------------- #
  # d Parameter cannot be larger than 1 or smaller than 0. Also cannot be
  # smaller than c parameter.
  expect_error(new("4PL", a = 1, b = 2, d = 1.1, D = 1.702),
               regexp = "Invalid parameters. The values of \"d\" parameters")
  expect_error(new("4PL", a = 1, b = 2, c = .2, d = -0.3, D = 1.702),
               regexp = "Invalid parameters. The values of \"d\" parameters")

  # -------------------------------------------------------------------------- #
  # Problematic naming of parameters
  # TODO: add a more sensible error message for the following
  expect_error(new("2PL", c = .22, a = 1, D = 1.702),
               "assignment of an object of class")
  expect_error(new("2PL", b = .22, b = 1, D = 1.702))
  expect_error(new("M2PL", e = 1.2, a = c(1, 1.2), D = 1),
               regexp = "invalid name for slot of class")
  expect_error(object = new("M3PL", a = c(1, 1.2) , b = 2.11, c = .22, D = 1),
               regexp = "invalid name for slot of class")

  # -------------------------------------------------------------------------- #
  # Length of parameters should be 1.
  expect_error(new("3PL", a = c(1, 1.2) , b = 2.11, c = .22, D = 1.702),
               "Invalid parameters. For \"3PL\" model the size of the \"a\"")
  expect_error(new("3PL", a = 1 , b = 2.11, c = c(.22, 1.2), D = 1.702),
               "Invalid parameters. For \"3PL\" model the size of the \"c\"")
  expect_error(new("M3PL", a = c(1, 1.2) , d = 2.11, c = c(.22, 1.2),
                   D = 1.702),
               "Invalid parameters. For \"M3PL\" model the size of the \"c\"")


  # -------------------------------------------------------------------------- #
  # Length of each parameter element should correspond to the size argument of
  # PMODELS.
  expect_error(new("GRM", a = c(1, 2.2) , b = c(-1, 0, 2.11), D = 1.702),
               "Invalid parameters. For \"GRM\" model the size of the \"a\"")
  expect_error(new("GPCM", a = c(0.2, 1, 2.2) , b = c(-1, 0, 2.11, 3),
                   D = 1.702),
               "Invalid parameters. For \"GPCM\" model the size of the \"a\"")


  # -------------------------------------------------------------------------- #
  ###### se_parameters ######@

  # se_parameters should be numeric
  expect_s4_class(new("4PL", a = 2, b = -.22, c = 0.2, d = .99, D = 1.702,
                      se_a = Inf, se_b = 0, se_c = .2, se_d = .99), "4PL")

  # NA is an acceptable se_parameter
  expect_s4_class(new("2PL", a = 2, b = -.22, D = 1.702, se_a = Inf,
                      se_b = as.numeric(NA)), "2PL")

  # Even logical NA is acceptable
  expect_s4_class(new("GRM", a = 2, b = c(-1, 0, 1), D = 1.702, se_a = Inf,
                      se_b = rep(as.numeric(NA), 3)), "GRM")

  # se_paramters for polytomous items
  expect_s4_class(new("GPCM", a = 2, b = c(-.22, 1.2, 2.3), D = 1.702,
                      se_a = 1, se_b = c(.13, .2, .3)), "GPCM")
  # The order of se_parameters can be different
  expect_s4_class(new("GPCM", a = 2, b = c(-.22, 1.2, 2.3), D = 1.702,
                      se_b = c(.13, .2, .3), se_a = .3), "GPCM")

  # se_parameters should be larger than 0
  expect_error(new("2PL", a = 2, b = -.22, D = 1.702, se_a = .14, se_b = -.1),
               paste0("Invalid 'se_b' values. Standard error values of ",
                      "item parameters cannot be smaller than 0."))

  # se_parameters should match item parameters
  expect_error(new("2PL", a = 2, b = -.22, D = 1.702, se_a = .14, se_b = .13,
                   se_c = .24))

  # se_parameters elements cannot be empty, and should have
  expect_error(new("2PL", a = 2, b = -.22, D = 1.702, se_a = Inf,
                   se_b = numeric(0)),
               regexp = "Invalid 'se_b'. The length of 'se_b' should be equal")

  # Item parameter values cannot be non-numeric
  expect_error(new("1PL", b = 1, D = 1, se_b = "ABC"))

  # For polytomous items, the standard errors should have the same number of
  # elements as the number of parameters
  expect_error(new("GPCM", a = 2, b = c(-.22, 1.2, 2.3), D = 1.702, se_a = 1,
                   se_b = .13),
               regexp = "Invalid 'se_b'. The length of 'se_b' should be equal")
})



