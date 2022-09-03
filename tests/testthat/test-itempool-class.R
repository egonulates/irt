# library(testthat)

# Test setValidity of "Itempool" class
test_that("Test setValidity of Item class", {
  # Each element of the "Itempool" should be "Item" class
  item1 <- item(a = 1.12, b = -2.1, c = 0.28)
  item2 <- item(a = 2, b = 3.2, c = 0.21)
  item3 <- c(a = 1.2, b = 2.8, c = 0.12) # This is not 'Item' class
  item4 <- item(a = 1.12, b = -1.23, c = .2, item_id = "I-21")
  item5 <- item(a = 0.84, b = 2.23, c = .25, item_id = "I-22")
  item6 <- item(b = 1, item_id = 'i6')
  item7 <- item(b = 2, item_id = 'i7')
  # Create a new Itempool
  expect_s4_class(
    object = new("Itempool", item_list = list(`I-21` = item4, `I-22` = item5)),
    class = "Itempool")
  expect_s4_class(object = new("Itempool", item_list = list(`I-21` = item4)),
            class = "Itempool")
  # expect_s4_class(
  #   object = new("Itempool", item_list = list(item1, item4),
  #                misc = list(calibration_date = as.Date("2020-01-17"))),
  #   class = "Itempool")


  # -------------------------------------------------------------------------- #
  ### Errors ###
  # ID's should be unique
  expect_error(new("Itempool", item_list = list(i1 = item1, i2 = item2)),
               regexp = "Invalid ID's.")
  expect_error(
    new("Itempool", item_list = list(item4, item5)),
   "The names of the 'item_list' elements should be the same as the ID's of ")
  # Incorrect Class
  expect_error(new("Itempool", item_list = item4))
  expect_error(new("Itempool", item_list = list()),
               regexp = "Invalid elements.")

  # All of the elements of Itempool class should be 'Item' class.
  expect_error(new("Itempool", item_list = list(item1, item2, item3)),
               regexp = "Invalid 'Itempool' elements.")


  # Elements can be Testlet and Item classes.
  testlet1 <- new('Testlet', testlet_id = 't1',
                 item_list = itempool(i6 = item6, i7 = item7))
  expect_s4_class(
    new("Itempool",
        item_list = list(`I-21` = item4, `I-22` = item5, t1 = testlet1)),
    class = "Itempool")

  testlet1 <- new('Testlet', testlet_id = 't5',
                  item_list = itempool(item5, item6))
  # Duplicated item in testlet and standalone items
  expect_error(object = new(
    "Itempool",
    item_list = list(`I-21` = item4, `I-22` = item5, t5 = testlet1)),
    regexp = paste0("Invalid ID's. Each Item object in the item pool ",
                    "should have a unique 'item_id'."))
})
