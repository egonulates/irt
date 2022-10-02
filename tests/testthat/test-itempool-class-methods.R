# library(testthat)

###############################################################################@
############################# itempool #########################################
###############################################################################@

test_that("Test itempool", {
  ################# Test item_pool part ####################################@###
  ### Itempool object
  ip <- itempool(list(item(b = 1, item_id = "K1"),
                      item(a = 1, b = 2, item_id = "K2")))
  expect_s4_class(itempool(ip), "Itempool")
  ip1 <- itempool(ip, item_id = c('i1', 'i2'))
  irt:::get_slot_itempool_cpp(ip = ip1, slotName = "id")
  irt:::get_ids_itempool_cpp(ip = ip1)
  expect_identical(ip1$id, c('i1', 'i2'))
  expect_identical(ip1$item_id, c('i1', 'i2'))

  ip1 <- itempool(ip, item_id = c('a1', 'a2'), content = c("A", "G"))
  expect_identical(ip1$item_id, c('a1', 'a2'))
  expect_identical(ip1$content, setNames(c('A', 'G'), ip1$item_id))
  expect_identical(names(ip1$content)[2], "a2")
  # When there is no named argument return the Itempool, ignore the arguments
  ip1 <- itempool(ip, 3, "b", c(1, 2,3))
  expect_identical(ip, ip1)
  # Change the D parameter
  ip <- itempool(item_list = list(item(a = .9, b = -1.7, item_id = "K1"),
                                  item(a = 1, b = 2, item_id = "K2")))
  expect_identical(ip$D, setNames(c(1, 1), ip$item_id))
  ip1 <- itempool(ip, D = 1.2)
  expect_identical(ip1$D, setNames(c(1.2, 1.2), ip1$item_id))
  ip1 <- itempool(ip, D = c(3.4, 5.5))
  expect_identical(ip1$D, setNames(c(3.4, 5.5), ip1$item_id))

  # -------------------------------------------------------------------------- #
  ## item
  ip <- item(a = 1, b = 2)
  # Item do not have an ID automatically
  expect_null(ip$item_id)
  ip1 <- itempool(ip)
  expect_s4_class(ip1, "Itempool")
  # Default item_id is 'Item_1, itempool() function adds an ID
  expect_identical(ip1$item_id, 'Item_1')

  # -------------------------------------------------------------------------- #
  # Note: 2020/02/03: following feature deprecated
  ip1 <- itempool(ip, item_id = 'i1', content = 'algebra')
  expect_s4_class(ip1, "Itempool")
  expect_identical(ip1$item_id, 'i1')
  expect_identical(ip1$content, setNames('algebra', ip1$item_id))
  # The following would be great if it works.
  expect_identical(ip$D, 1)
  ip1 <- itempool(ip, D = 3.2)
  expect_identical(ip1$D, setNames(3.2, ip1$item_id))

  # -------------------------------------------------------------------------- #
  ip <- itempool(b = c(-1, 0.2, 1.1), model = "Rasch")
  expect_identical(ip[[1]]$model, "Rasch")
  expect_s4_class(ip[[1]], "Rasch")

  # -------------------------------------------------------------------------- #
  ## numeric or integer
  ip = itempool(a =  1, b = .2, D = 1.1, content = 'dd')
  expect_s4_class(ip, "Itempool")
  expect_identical(length(ip), 1L)
  expect_identical(ip$content, setNames('dd', ip$item_id))
  # Another example with multiple items
  ip <- itempool(a = c(1, 2), b = c(-1, .2), content = "Algebra", D = 3)
  expect_s4_class(ip, "Itempool")
  expect_identical(length(ip), 2L)
  expect_identical(ip$content, setNames(c("Algebra", "Algebra"), ip$item_id))
  expect_identical(names(ip$content), ip$item_id)
  expect_identical(ip$D, setNames(c(3, 3), ip$item_id))

  # -------------------------------------------------------------------------- #
  ## List
  ip_list <- generate_ip(n = 3, output = "list")
  expect_s4_class(itempool(ip_list), "Itempool")
  expect_identical(length(itempool(ip_list)), 3L)
  # Error:
  expect_error(itempool(list(b = c(1,2,3))),
               regexp = paste0("Invalid elements. All elements of the list ",
                               "should be an 'Item' or 'Testlet' object."))

  # An item without an item_id will be automatically assigned an item_id.
  item_no_id <- generate_item()
  item_no_id@item_id <- NULL
  expect_true(validObject(item_no_id))
  ip_list <- list(generate_item(item_id = "abc1"), item_no_id)
  ip <- itempool(ip_list)
  expect_s4_class(ip, "Itempool")
  expect_identical(ip[[2]]$item_id, "Item_1")


  # -------------------------------------------------------------------------- #
  # A list of items in which some of items don't have 'item_id's, the function
  # should automatically assign names
  ip_list <- vector('list', 3)
  ip_list[[1]] <- item(b = 1)
  ip_list[[2]] <- item(a = .99, b = 1)
  ip_list[[3]] <- item(a = .99, b = 1, c = .2, item_id = 'my_item1')
  expect_true(is.null(ip_list[[1]]$item_id))
  expect_true(is.null(ip_list[[2]]$item_id))
  expect_false(is.null(ip_list[[3]]$item_id))
  expect_type(ip_list, "list")
  ip <- itempool(ip_list)
  expect_s4_class(ip, "Itempool")
  expect_identical(ip, itempool(as.data.frame(ip)))
  expect_false(any(is.null(ip$item_id)))
  expect_identical(ip$item_id[3], ip_list[[3]]$item_id)

  # -------------------------------------------------------------------------- #
  ## data.frame or matrix
  n <- 10
  ip <- itempool(data.frame(a = runif(n, .5, 1.5), b = rnorm(n)))
  expect_s4_class(ip, "Itempool")
  expect_identical(ip, itempool(as.data.frame(ip)))
  expect_identical(ip$D, setNames(rep(1, n), ip$item_id))
  expect_identical(ip$model, setNames(rep('2PL', n), ip$item_id))

  # -------------------------------------------------------------------------- #
  n <- 10
  ip <- itempool(data.frame(a = runif(n, .5, 1.5), b = rnorm(n), D = 1))
  expect_s4_class(ip, "Itempool")
  expect_identical(ip, itempool(as.data.frame(ip)))
  expect_identical(ip$D, setNames(rep(1, n), ip$item_id))
  expect_identical(ip$model, setNames(rep('2PL', n), ip$item_id))

  # -------------------------------------------------------------------------- #
  ## Matrix
  n <- 10
  ip_matrix <- matrix(runif(3*n), ncol = 3,
                      dimnames = list(NULL, c("a", "b", "c")))
  ip <- itempool(ip_matrix)
  expect_s4_class(ip, "Itempool")
  expect_identical(ip, itempool(as.data.frame(ip)))
  expect_identical(ip$model, setNames(rep("3PL", n), ip$item_id))


  # Test the IDs are "Item_xx"
  ip <- itempool(b = rnorm(10))
  expect_true(all(ip$item_id == paste0("Item_", 1:10)))
  ip <- itempool(a = runif(10, 1, 2), b = rnorm(10))
  expect_true(all(ip$item_id == paste0("Item_", 1:10)))

  #############@###
  expect_identical(itempool(new(
    "Itempool",
    item_list = list(K1 = item(b = 1, item_id = "K1"),
                     K2 = item(a = 1, b = 2, item_id = "K2"))),
    item_id = c("E1", "E2"))[[1]]@item_id, "E1")
  expect_identical(itempool(new(
    "Itempool",
    item_list = list(K1 = item(b = 1, item_id = "K1"),
                     K2 = item(a = 1, b = 2, item_id = "K2"))),
    content = c("E1", "E2"))[[1]]@content, "E1")

  # -------------------------------------------------------------------------- #
  ## Testlet
  t1 <- testlet(itempool(b = -3:-2, item_id = c("t1-i1", "t1-i2")),
                testlet_id = "t1")
  t2 <- testlet(itempool(b = 2:4, item_id = c("t2-i1", "t2-i2", "t2-i3")),
                   testlet_id = "t2")
  i1 <- item(b = -1, item_id = "i1")
  i2 <- item(b = 0, item_id = "i2")
  expect_s4_class(itempool(i1, t1), "Itempool")
  expect_s4_class(itempool(t1, i1), "Itempool")
  expect_s4_class(itempool(t1, i1, t2), "Itempool")
  expect_s4_class(itempool(i1, t1, t2), "Itempool")
  expect_s4_class(itempool(t1), "Itempool")
  expect_s4_class(itempool(t1, t2), "Itempool")

  # -------------------------------------------------------------------------- #
  # mirt
  ip <- itempool(
    item_list = list(
      new("M2PL", item_id = "Item 1", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 2", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 3", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 4", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 5", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7)
    ))
  expect_s4_class(itempool(ip), "Itempool")
  expect_identical(ip, itempool(as.data.frame(ip)))
  expect_s4_class(itempool(ip@item_list), "Itempool")

  # -------------------------------------------------------------------------- #
  # misc fields correctly specified.
  ip <- itempool(b = rnorm(2), item_id = paste0("t1-i", 1:2),
                 misc = list(list(sympson_hetter_k = .8, form = "b3"),
                             list(sympson_hetter_k = .9)))
  expect_identical(ip[[1]]@misc$sympson_hetter_k, 0.8)
  expect_identical(ip[[1]]@misc$form, "b3")
  expect_identical(ip[[2]]@misc$sympson_hetter_k, 0.9)

  # Add the same misc value to all of the items
  ip <- itempool(b = rnorm(2), item_id = paste0("t1-i", 1:2),
                 misc = list(sympson_hetter_k = .8))
  expect_identical(ip[[1]]@misc$sympson_hetter_k, 0.8)
  expect_identical(ip[[2]]@misc$sympson_hetter_k, 0.8)

  # Add the same misc value to all of the items
  ip <- itempool(b = rnorm(2), item_id = paste0("t1-i", 1:2),
                 misc = list(list(sympson_hetter_k = .8, form = "Form1")))
  expect_identical(ip[[1]]@misc$sympson_hetter_k, 0.8)
  expect_identical(ip[[2]]@misc$sympson_hetter_k, 0.8)

  ################# Test item part #########################################@###
  # Put name if there is no name.
  expect_s4_class(itempool(item(b = 1)), "Itempool")
  expect_s4_class(itempool(item(a = 1, b = 1.2, item_id = "It-12")), "Itempool")
  expect_s4_class(itempool(item(a = 1, b = 1.2, item_id = "It-12",
                                content = "A24")),
            "Itempool")
  expect_s4_class(itempool(item(a = 1, b = 1.2, c = .23)), "Itempool")

  expect_s4_class(itempool(item(a = 1, b = 1.2, c = .23, item_id = "jeb1")),
                  "Itempool")

  expect_identical(itempool(item(a = 1, b = 1.2, c = .23,
                                 item_id = "jeb1"))$item_id, "jeb1")
  expect_identical(itempool(item(a = 1, b = 1.2, c = .23),
                             content = "A1")$content, setNames("A1", "Item_1"))

  ################# Test numeric part ######################################@###
  expect_error(itempool(1, model = "1PL"))
  expect_error(itempool(c(1, 1.2), item_id = "It-12"),
               "Itempool object cannot be created for '' model.")
  expect_error(itempool(c(1, 1.2,.2,.93), item_id = "It-12", content = "A24"),
               "Itempool object cannot be created for '' model.")

  # When only b parameter entered without a model, it is 1PL
  ip <- itempool(b = rnorm(5))
  expect_identical(ip[[1]]$model, "1PL")

  ip <- itempool(a = rnorm(3), b = rnorm(3))
  expect_identical(ip[[1]]$model, "2PL")

  ip <- itempool(a = rlnorm(3), b = rnorm(3), c = runif(3, 0, .3))
  expect_identical(ip[[1]]$model, "3PL")

  ip <- itempool(a = rlnorm(3), b = rnorm(3), c = runif(3, 0, .3),
                 d = runif(3, .8, 1))
  expect_identical(ip[[1]]$model, "4PL")

  ################# Test matrix part #######################################@###
  ip <- matrix(c(a = .12, b = 2.21), nrow = 1,
               dimnames = list(NULL, c("a", "b")), byrow = FALSE)
  expect_s4_class(itempool(ip), "Itempool")
  expect_s4_class(itempool(ip, item_id = "Kan12", content = "kn1"), "Itempool")
  ip <- matrix(c(1,-2,.2,1, 1.3, 1.2, .35, .93), nrow = 2, byrow = TRUE,
               dimnames = list(NULL, c("a", "b", "c", "d")))
  expect_s4_class(itempool(ip), "Itempool")
  expect_identical(itempool(ip, content = c("c1", "c2"))[[1]]@content, "c1")

  ip <- matrix(1:10)
  colnames(ip) <- "b"
  expect_identical(itempool(ip, content = rep("Alg1", 10))[[4]]@content, "Alg1")
  ################# Test data.frame part ###################################@###

  n <- sample(5:20,1)
  ip <- data.frame(a = runif(n, .5, 1.5), b = rnorm(n), c = runif(n, 0,.3))
  expect_s4_class(itempool(ip), "Itempool")

  # -------------------------------------------------------------------------- #
  # Data frame for GPCM items with varying number of categories
  ipdf <- structure(list(a = c(1.0244, 1.1956, 0.665, 1.0409, 1.2382),
                         b1 = c(-1.0764, -0.9672, -0.7706, -0.6755, -1.6088),
                         b2 = c(0.2585,  0.6053, -0.2641, -0.3546, -0.0542),
                         b3 = c(NA, NA, 0.3584, 0.3088, NA),
                         b4 = c(NA, NA, NA, 1.4189, NA)),
                    class = "data.frame", row.names = c(NA, -5L))
  ip <- itempool(ipdf, model = "GPCM", D = 1)
  expect_s4_class(ip, "Itempool")
  expect_s4_class(itempool(ip), "Itempool")
  expect_identical(ip[[1]]$b, c(ipdf$b1[1], ipdf$b2[1]))
  expect_identical(length(ip[[3]]$b), 3L)



  # -------------------------------------------------------------------------- #
  # Expect an error when both data.frame has a model column and model is
  # also an argument for 'itempool()' function.
  ipdf <- data.frame(a = c(1.0244, 1.1956, 0.665, 1.0409, 1.2382),
                     b1 = c(-1.0764, -0.9672, -0.7706, -0.6755, -1.6088),
                     b2 = c(0.2585,  0.6053, -0.2641, -0.3546, -0.0542),
                     model = "GRM")
  expect_error(itempool(ipdf, model = "GPCM"))

  # -------------------------------------------------------------------------- #
  # Data frame for GPCM items with reshuffled columns
  ipdf <- structure(list(b1 = c(-0.9261, -0.0526, -0.9284, -1.3163, -1.3292),
                         b2 = c(0.0062, 0.4253, 0.7774, -0.3523, -0.0219),
                         b3 = c(1.6774, 0.8239, 2.2602, 0.5002, 0.6511),
                         a = c(0.9485, 1.7504, 1.4159, 0.728, 1.4252)),
                    class = "data.frame", row.names = c(NA,  -5L))
  ipdf <- ipdf[, sample(colnames(ipdf))]
  ip <- itempool(ipdf, model = "GPCM", D = 1)
  expect_s4_class(ip, "Itempool")
  i <- sample(1:nrow(ipdf), 1)
  expect_identical(ip[[i]]$b, c(ipdf$b1[i], ipdf$b2[i], ipdf$b3[i]))
  i <- sample(1:nrow(ipdf), 1)
  expect_identical(ip[[i]]$a, ipdf$a[i])

  # -------------------------------------------------------------------------- #

  n <- sample(10:20, 1)
  ipdf <- data.frame(a = rlnorm(n, 0, .3), b1 = rnorm(n, -1))
  ipdf$b2 <- ipdf$b1 + runif(n, 0.1)
  ipdf$b3 <- ipdf$b2 + runif(n, 0.1)
  ipdf$D <- 1.7
  ipdf$content = rep(c("G", "M"), len = n)
  rownames(ipdf) <- paste0("item--", 1:n)

  ip <- itempool(ipdf, model = "GRM")
  expect_s4_class(ip, "Itempool")
  expect_s4_class(itempool(ip), "Itempool")
  expect_true(all(sapply(ip$item_list, function(x) x$model == "GRM")))
  i <- sample(1:n, 1)
  expect_identical(ip$item_list[[i]]$a, ipdf[i, "a"])
  i <- sample(1:n, 1)
  expect_identical(ip$item_list[[i]]$b[2], ipdf[i, "b2"])
  expect_null(names(ip$b))
  expect_identical(names(ip$a), ip$item_id)
  expect_identical(names(ip$D), ip$item_id)
  expect_identical(names(ip$content), ip$item_id)
  expect_identical(ip$D[i], setNames(1.7, ip$item_id[i]))
  expect_identical(ip$item_list[[3]]$content, "G")
  expect_identical(ip$item_list[[4]]$content, "M")
  i <- sample(1:n, 1)
  # The following feature (data.frame row names as item ids) discontinued on
  # 2020-09-06
  # expect_identical(ip$item_id[i], paste0("item--", i))

  # Check whether item_id is updated when item_id is explicitly put in the
  # data.frame
  ipdf$item_id = paste0("ii", 1:n)
  ip <- itempool(ipdf, model = "GRM")
  i <- sample(1:n, 1)
  expect_identical(ip$item_id[i], paste0("ii", i))

  # -------------------------------------------------------------------------- #
  # Both testlet_id, Id, ID, iD should be acceptable as data frame column name,
  # but if multiple of them are present, "ID" or "Id" or "iD" should be ignored.
  n <- sample(4:7, 1)
  ids <- sample(letters, n)
  ip <- itempool(data.frame(item_id = ids, b = rnorm(n)))
  expect_identical(ip$item_id, ids)
  expect_identical(ip$id, ids)
  ip <- itempool(data.frame(ITEM_ID = ids, b = rnorm(n)))
  expect_identical(ip$item_id, ids)
  ip <- itempool(data.frame(Item_Id = ids, b = rnorm(n)))
  expect_identical(ip$item_id, ids)
  ip <- itempool(data.frame(Item_ID = ids, b = rnorm(n)))
  expect_identical(ip$item_id, ids)


  # ip <- itempool(data.frame(b = rnorm(n)), Item_id = ids)
  # expect_identical(ip$item_id, ids)
  ip <- itempool(data.frame(b = rnorm(n)), item_id = ids)
  expect_identical(ip$item_id, ids)

  # -------------------------------------------------------------------------- #
  # The following data frame or tibble cannot be created when item_id's are
  # duplicated. A more informative error should be issued.
  n <- sample(4:7, 1)
  ipdf <- data.frame(item_id = sample(letters[1:3], n, TRUE),
                       a = rlnorm(n, 0, .3), b = rnorm(n),
                       content = sample(c("Geo", "Alg"), n, TRUE))
  expect_error(itempool(ipdf),
               "Invalid item IDs. Item ID cannot be duplicated.")
  ipdf <- data.frame(Item_ID = sample(letters[1:3], n, TRUE),
                       a = rlnorm(n, 0, .3), b = rnorm(n),
                       content = sample(c("Geo", "Alg"), n, TRUE))
  expect_error(itempool(ipdf),
               "Invalid item IDs. Item ID cannot be duplicated.")
  # ID or testlet_id as argument
  expect_error(itempool(data.frame(b = rnorm(n)),
                         item_id = sample(letters[1:3], n, TRUE)),
               "Item ID cannot be duplicated.")
  expect_error(itempool(data.frame(b = rnorm(n)),
                         item_id = sample(letters[1:3], n, TRUE)),
               "Item ID cannot be duplicated.")

  ################# First element Numeric ##################################@###
  ip <- itempool(a = 1:2, b = 2:3, model = "2PL")
  ip <- itempool(a = 1:2, b = 2:3, c = c(.2, .3), model = "2PL")

  ################# se_parameters ##########################################@###
  # Test a simple list for se_parameters
  item_list <- generate_ip(n = 2, model = "2PL", output = "list")
  ip <- itempool(item_list,
                 se = list(list(a = .2, b = .3),
                           list(a = .4, b = .5)))
  expect_identical(ip[[1]]@se_a, .2)
  expect_identical(ip[[1]]@se_b, .3)
  expect_identical(ip[[2]]@se_a, .4)
  expect_identical(ip[[2]]@se_b, .5)


  # -------------------------------------------------------------------------- #
  item_list <- generate_ip(n = 2, model = "2PL", output = "list")
  ip <- itempool(item_list,
                 se = list(list(se_a = .2, se_b = .3),
                           list(se_a = .4, se_b = .5)))
  expect_identical(ip[[1]]@se_a, .2)
  expect_identical(ip[[1]]@se_b, .3)
  expect_identical(ip[[2]]@se_a, .4)
  expect_identical(ip[[2]]@se_b, .5)

  # -------------------------------------------------------------------------- #
  # Entering se_parameters as data.frame
  ipdf <- as.data.frame(generate_ip(se = TRUE))
  ip <- itempool(ipdf[, c("a", "b", "c", "item_id")],
                 se = ipdf[, c("se_a", "se_b", "se_c")])
  i <- sample(1:nrow(ipdf), 1)
  expect_identical(ip[[i]]@se_a, ipdf$se_a[i])
  expect_identical(ip[[i]]@se_b, ipdf$se_b[i])
  expect_identical(ip[[i]]@se_c, ipdf$se_c[i])


  # -------------------------------------------------------------------------- #
  # A data frame of mixture of testlets and items can be converted to an
  # itempool
  ip <- c(
    generate_testlet(item_models = c("GPCM", "3PL"), item_id_preamble = "t1_"),
    generate_testlet(item_models = c("2PL", "GRM"), item_id_preamble = "t2_"),
    generate_ip(model = c("2PL", "3PL", "GPCM")),
    generate_testlet(item_models = c("3PL", "3PL"), item_id_preamble = "t3_"))
  ipdf <- as.data.frame(ip)
  ip_new <- itempool(ipdf)
  expect_identical(ip, ip_new)

  # -------------------------------------------------------------------------- #
  # When a list column presented, if a value in list column is NA, then there
  # shouldn't be a misc column for that element.
  ipdf <- structure(list(item_id = c("t1_Item_1", "t1_Item_2"),
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
  ip <- itempool(ipdf)
  expect_true(is.data.frame(as.data.frame(ip)))

  # -------------------------------------------------------------------------- #
  # item_id column can be added successfully when a tibble is provided (bugfix)
  ipdf <- tibble::tibble(item_id = c("12-3", "12-4", "12-5", "12345-6"),
                         a = runif(4, .5, 2), b = rnorm(4))
  expect_s4_class(itempool(ipdf), "Itempool")

})





###############################################################################@
############################# concatenation of 'Item' objects ##################
###############################################################################@

# Test concatenation function "c" of "Itempool" class
test_that("Test concatenation function 'c' of 'Itempool' class", {
  # Each element of the "Itempool" should be "Item" class
  item1 <- item(a = 1.12, b = -2.1, c = 0.28)
  item2 <- item(a = 2, b = 3.2, c = 0.21)
  item3 <- c(a = 1.2, b = 2.8, c = 0.12) # This is not 'Item' class
  item4 <- item(a = 1.12, b = -1.23, c = .2, item_id = "I-21")
  item5 <- item(a = 0.84, b = 2.23, c = .25, item_id = "I-22")
  # Create a new Itempool
  expect_s4_class(object = c(item4, item5), class = "Itempool")
  # Creat a new Itempool object even the 'Item's don't have names
  expect_s4_class(object = c(item1, item2), class = "Itempool")
  expect_s4_class(object = c(item1, item2, item4), class = "Itempool")

  # -------------------------------------------------------------------------- #
  # mirt items
  item4 <- item(a = 1.12, b = -1.23, c = .2, item_id = "I-1")
  item5 <- item(a = 0.84, b = 2.23, c = .25, item_id = "I-2")
  item6 <- new("M2PL", a = c(2.1, 1.23, 1.3), d = -1.2, D = 1.7, item_id = "MI-1")
  item7 <- new("M2PL", a = c(1.1, 1.23), d = -1.2, D = 1.7, item_id = "MI-2")
  item8 <- new("M3PL", item_id = "MI-3", a = c(2.1, 1.23, 1.3), d = -1.2, c = .2,
               D = 1.7)
  item9 <- new("M3PL", content = "Algebra", a = c(2.1, 1.23, 1.3), d = -1.2,
               c = .2, D = 1.7)

  ip <- itempool(list(item4 ,item6, item7, item8))
  expect_s4_class(object = ip, class = "Itempool")
  expect_s4_class(itempool(ip), "Itempool")
  expect_s4_class(object = c(item4 ,item6, item7, item8), class = "Itempool")
  expect_s4_class(object = c(item4 ,item6, item7, item8, item9),
                  class = "Itempool")

  # -------------------------------------------------------------------------- #
  # Concatenate Testlet and Item objects
  i1 <- item(b = rnorm(1))
  i2 <- item(b = rnorm(1))
  i3 <- item(b = rnorm(1))
  i4 <- item(b = rnorm(1))
  i5 <- item(b = rnorm(1), item_id = 'ii')
  i6 <- item(b = rnorm(1), item_id = 'ii')
  i7 <- item(b = rnorm(1), item_id = 'i7')
  t1 <- testlet(i3, i4)
  t2 <- testlet(i6, i7)
  ip <- c(i1, i2, t1)
  expect_s4_class(ip, 'Itempool')
  expect_s4_class(itempool(ip), "Itempool")
  expect_identical(length(ip), 3L)
  # Combine two items and two testlets
  ip <- c(i1, i2, t1, t2)
  expect_s4_class(ip, 'Itempool')
  expect_s4_class(itempool(ip), "Itempool")
  expect_identical(length(ip), 4L)

  t3 <- testlet(i3, i5)
  expect_error(c(i6, t3), "Invalid 'item_id's.")


  # -------------------------------------------------------------------------- #
  # Concatenate Testlet and Itempool objects
  t1 <- generate_testlet(n = 3, item_id_preamble = "t1")
  ip1 <- generate_ip(n = 5)
  t2 <- generate_testlet(n = 4, item_id_preamble = "t2")
  expect_s4_class(c(t1, ip1, t2), "Itempool")

  # -------------------------------------------------------------------------- #
  ### Errors ###
  # Give error when one of the item is not 'Item' class
  item3 <- c(a = 1.2, b = 2.8, c = 0.12) # This is not 'Item' class
  item4 <- item(a = 1.12, b = -1.23, c = .2, item_id = "I-21")
  item6 <- new("M2PL", a = c(2.1, 1, 1.3), d = -1.2, D = 1.7, item_id = "MI-1")
  expect_error(c(item4, item3),
               regexp = paste0("All of the elements should be either 'Item', ",
                               "'Itempool' or 'Testlet' class."))
  expect_error(c(item4, item3, item6),
               regexp = paste0("All of the elements should be either 'Item', ",
                               "'Itempool' or 'Testlet' class."))
})

###############################################################################@
################### Subsetting 'Itempool' objects with "[" #####################
###############################################################################@
test_that("Test concatenation with [", {
  ip <- itempool(data.frame(a = runif(8, .5, 1.5), b = rnorm(8),
                               c = runif(8, 0,.3)), item_id = paste0("Item_",1:8),
                    content = c(rep("Algebra", 3), rep("Geometry", 2),
                                rep("Arithmetic", 3)))
  ip_new <- ip[1]
  expect_s4_class(ip_new, "Itempool")
  expect_identical(length(ip_new), 1L)
  expect_identical(ip_new[[1]], ip[[1]])

  ip_new <- ip[c(1:3)]
  expect_s4_class(ip_new, "Itempool")
  expect_identical(length(ip_new), 3L)
  expect_identical(ip_new[[2]], ip[[2]])

  ip_new <- ip[c(T, F, T, T, F, F, F, T)]
  expect_s4_class(ip_new, "Itempool")
  expect_identical(length(ip_new), 4L)
  expect_identical(ip_new[[2]], ip[[3]])

  ip_new <- ip[-2]
  expect_s4_class(ip_new, "Itempool")
  expect_identical(length(ip_new), length(ip) - 1L)
  expect_identical(ip_new[[2]], ip[[3]])

  # If the logical vector is shorter than the vector being subsetted,
  # it will be recycled to be the same length.
  ip_new <- ip[c(T, F)]
  expect_s4_class(ip_new, "Itempool")
  expect_identical(length(ip_new), 4L)
  expect_identical(ip_new[[2]], ip[[3]])

  ip_new <- ip[c("Item_1", "Item_2")]

  # -------------------------------------------------------------------------- #
  # The order of numbers matter
  ip_new <- ip[c(6, 3, 7, 1)]
  expect_identical(ip_new[[1]], ip[[6]])
  expect_identical(ip_new[[2]], ip[[3]])
  expect_identical(ip_new[[3]], ip[[7]])
  expect_identical(ip_new[[4]], ip[[1]])

  # -------------------------------------------------------------------------- #
  # Missing indieces
  ip_new <- ip[c(7, NA, 2)]
  expect_s4_class(ip_new, "Itempool")
  expect_identical(length(ip_new), 2L)
  expect_identical(ip_new[[1]], ip[[7]])

  ip_new <- ip[c(NA, F, T, T, F, F, F, NA)]
  expect_s4_class(ip_new, "Itempool")
  expect_identical(length(ip_new), 2L)
  expect_identical(ip_new[[1]], ip[[3]])

  # -------------------------------------------------------------------------- #
  # Subsetting by IDs
  ip_new <- ip[c("Item_6", "Item_3", "Item_4")]
  expect_s4_class(ip_new, "Itempool")
  expect_identical(length(ip_new), 3L)
  expect_identical(ip_new[[1]], ip[[6]])
  expect_identical(ip_new[[2]], ip[[3]])
  expect_identical(ip_new[[3]], ip[[4]])

  # -------------------------------------------------------------------------- #
  # Errors
  # Cannot subset using an invalid item_id.
  expect_error(ip[c("abc", "Item_3", "Item_4")], "Failed to subset")

  # -------------------------------------------------------------------------- #
  # misc is also transferred to the new Itempool object
  ip <- itempool(b = rnorm(5))
  ip$misc <- list(form_id = "F12")

  # -------------------------------------------------------------------------- #
  # mirt
  ip <- itempool(list(
      new("M2PL", item_id = "Item 1", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 2", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 3", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 4", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 5", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7)
    ))
  # The following tests are crucial. For example, in ability estimation using
  # maximum likelihood, the NA items are removed for standard error calculation
  # using ip[!is.na(resp)].
  expect_s4_class(ip[1], "Itempool")
  expect_s4_class(ip[c(1:3)], "Itempool")
  expect_s4_class(ip[c(T, F, T, T, F)], "Itempool")


  # -------------------------------------------------------------------------- #
  # When subsetting an item pool, if the result is an empty item pool, return
  # NULL
  ip <- generate_ip(model = "2PL")
  expect_error(ip[ip$model == "GPC"],
               "The selection did not match any Item/Testlet object")
})

###############################################################################@
################### Subsetting 'Itempool' objects with "[[" ####################
###############################################################################@

test_that("Test concatenation with [[", {
  # Each element of the "Itempool" should be "Item" class
  ip <- itempool(data.frame(a = runif(8, .5, 1.5), b = rnorm(8),
                               c = runif(8, 0,.3)), item_id = paste0("Item_",1:8),
                    content = c(rep("Algebra", 3), rep("Geometry", 2),
                                rep("Arithmetic", 3)))

  expect_s4_class(ip[[1]], "Item")

  # -------------------------------------------------------------------------- #
  ip <- itempool(list(
      new("M2PL", item_id = "Item 1", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 2", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 3", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 4", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 5", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7)
    ))
  expect_s4_class(ip[[1]], "Item")

  # -------------------------------------------------------------------------- #
  # When out of bounds, a custom error will be shown
  ip <- generate_ip(n = 10)
  expect_error(ip[[11]], paste0("Subscript out of bounds. Please use an index ",
                                "between 1 and 10."))
})

###############################################################################@
############################# [[<-  method (Itempool) ##########################
###############################################################################@
test_that("Test Setting 'Itempool' objects with '[[<-'", {
  ip <- itempool(b = rnorm(5))
  item <- item(b = 1, a = 1.5)
  expect_identical(ip[[1]]$model, "1PL")
  ip[[1]] <- item
  expect_identical(ip[[1]]$model, "2PL")
  # An invalid assignment
  expect_error(ip[[1]] <- 12, "Invalid assignment.")
})

###############################################################################@
############################# $<-  method (Itempool) ###########################
###############################################################################@
test_that("Test $<- method (Itempool)", {
  # Setting 'misc'
  ip <- itempool(b = rnorm(5))
  expect_null(ip$misc)
  expect_null(ip@misc)
  ip$misc <- list(form_id = "Form-123")
  expect_identical(ip@misc$form_id, "Form-123")


  # -------------------------------------------------------------------------- #
  # Setting 'id' and "item_id"
  ip <- c(generate_ip(n = 3, item_id = paste0("i", 1:3)),
          generate_testlet(n  = 2, item_id_preamble = "t1"),
          generate_item(item_id = "abc"))
  new_ids <- c("itm11", "itm21", "itm31", "tstlt44", "uu12")
  ip$id <- new_ids
  expect_identical(ip@item_list[[1]]@item_id, new_ids[1])
  expect_identical(ip@item_list[[2]]@item_id, new_ids[2])
  expect_identical(ip@item_list[[3]]@item_id, new_ids[3])
  expect_identical(ip@item_list[[4]]@testlet_id, new_ids[4])
  expect_identical(ip@item_list[[5]]@item_id, new_ids[5])
  expect_identical(names(ip@item_list)[1], new_ids[1])
  expect_identical(names(ip@item_list)[2], new_ids[2])
  expect_identical(names(ip@item_list)[5], new_ids[5])


  new_item_ids <- c("zz11", "xx21", "yy31", "tx1", "tx2", "duk12")
  ip$item_id <- new_item_ids
  expect_identical(ip@item_list[[1]]@item_id, new_item_ids[1])
  expect_identical(ip@item_list[[2]]@item_id, new_item_ids[2])
  expect_identical(ip@item_list[[3]]@item_id, new_item_ids[3])
  expect_identical(ip@item_list[[4]]@item_list[[1]]@item_id, new_item_ids[4])
  expect_identical(ip@item_list[[4]]@item_list[[2]]@item_id, new_item_ids[5])
  expect_identical(ip@item_list[[5]]@item_id, new_item_ids[6])

  expect_identical(names(ip@item_list)[1], new_item_ids[1])
  expect_identical(names(ip@item_list)[2], new_item_ids[2])
  expect_identical(names(ip@item_list[[4]]@item_list@item_list)[1],
                   new_item_ids[4])
  expect_identical(names(ip@item_list[[4]]@item_list@item_list)[2],
                   new_item_ids[5])
  expect_identical(names(ip@item_list)[5], new_item_ids[6])

  # -------------------------------------------------------------------------- #
  # Setting 'item_id'
  ip <- itempool(b = rnorm(2))
  expect_identical(ip$item_id[1], "Item_1")
  expect_identical(ip$item_id[2], "Item_2")
  ip$id <- c("a", "b")
  expect_identical(ip$item_id[1], "a")
  expect_identical(ip$item_id[2], "b")
  expect_identical(ip@item_list[[1]]@item_id, "a")
  expect_identical(ip@item_list[[2]]@item_id, "b")

  # -------------------------------------------------------------------------- #
  # Setting 'item_id', another example
  ip <- generate_ip(model = rep("3PL", 3))
  ip$item_id <- paste0("xyz-", ip$item_id)
  expect_identical(ip@item_list[[1]]@item_id, "xyz-Item_1")
  expect_identical(ip@item_list[[2]]@item_id, "xyz-Item_2")

  # -------------------------------------------------------------------------- #
  # Errors in setting 'item_id'
  ip <- itempool(b = rnorm(2))
  # item_id length and ip length should be the same
  expect_error(ip$id <- c("a", "b", "c"),
               "'id' should be a character vector with length equal to 2")
  expect_error(ip$id <- c(2, 1),
               "'id' should be a character vector with length equal to 2")
  expect_error(ip$id <- c("a", "a"),
               "'id' should not have any duplicated values")

  # -------------------------------------------------------------------------- #
  # Setting 'content'
  ip <- itempool(b = rnorm(2))
  expect_null(ip$content[1])
  expect_null(ip$content[2])
  ip$content <- c("a", "b")
  expect_identical(ip$content[1], setNames("a", ip$item_id[1]))
  expect_identical(ip$content[2], setNames("b", ip$item_id[2]))

  # -------------------------------------------------------------------------- #
  # One can set content to NULL
  ip <- itempool(b = rnorm(2), content = c("Algebra", "Geometry"))
  expect_identical(ip$content, setNames(c("Algebra", "Geometry"), ip$item_id))
  ip$content <- NULL
  expect_null(ip$content)

  # -------------------------------------------------------------------------- #
  # Errors in setting 'content'
  ip <- itempool(b = rnorm(2))
  # item_id length and ip length should be the same
  expect_error(ip$content <- c("a", "b", "c"),
               "'content' should be a character vector with length equal to 2")
  expect_error(ip$content <- c(2, 1),
               "'content' should be a character vector with length equal to 2")

  # -------------------------------------------------------------------------- #
  # A single value can be assigned to all content
  ip <- itempool(b = rnorm(4))
  ip$content <- "Algebra"
  expect_identical(ip$content, setNames(rep("Algebra", 4), ip$item_id))

  # -------------------------------------------------------------------------- #
  # Setting 'item_list'
  ip <- itempool(b = rnorm(2))
  item_list <- list(item(a = 1, b = 2), item(a = 2, b = 3, c = .2),
                    item(b = 1, model = 'Rasch'),
                    testlet(itempool(b = rnorm(2))))
  names(item_list) <- sapply(item_list, function(i) i$item_id)
  ip$item_list <- item_list
  expect_identical(length(ip), 4L)
  expect_identical(ip[[3]]$model, 'Rasch')

  # -------------------------------------------------------------------------- #
  # Parameter values can be assigned to item pools through $<-
  ip <- itempool(b = rnorm(3))
  new_b <- rnorm(3)
  new_D <- rnorm(3, 6)
  new_id <- c("slki1", "i6345", "dkwry")
  new_content <- c("si1", "i6", "wry")
  ip$D <- new_D
  ip$b <- new_b
  ip$id <- new_id
  ip$content <- new_content
  expect_identical(ip$D, setNames(new_D, ip$item_id))
  expect_identical(ip$b, setNames(new_b, ip$item_id))
  expect_identical(ip$id, new_id)
  expect_identical(ip$content, setNames(new_content, ip$item_id))

  # -------------------------------------------------------------------------- #
  # A single value can be assigned as a single parameters to all elements
  ip <- itempool(b = rnorm(3))
  ip$D <- 1.9
  ip$b <- 1
  ip$se_b <- c(.91, .92, .93)
  expect_identical(ip$D, setNames(rep(1.9, 3), ip$item_id))
  expect_identical(ip$b, setNames(rep(1, 3), ip$item_id))
  expect_identical(ip[[2]]@se_b, .92)

  # -------------------------------------------------------------------------- #
  # Errors in setting 'item_list'
  ip <- itempool(b = rnorm(2))
  # item_list should be a list of testlets or length and ip length should be
  # the same
  expect_error(ip$item_list <- c("a", "b", "c"),
               "'item_list' should be a list of 'Item' or 'Testlet' objects.")
  expect_error(ip$item_list <- itempool(b = rnorm(3)),
               "'item_list' should be a list of 'Item' or 'Testlet' objects.")

  # # -------------------------------------------------------------------------- #
  # # Parameter values cannot be assigned to item pools with mixed models
  # ip <- itempool(item(a = 1, b = 2), item(b = 1, model = "Rasch"))
  # expect_error(ip$b <- 1, "is not a valid name.")
  # expect_error(ip$D <- 1, "is not a valid name.")


  # -------------------------------------------------------------------------- #
  # Add misc field to all items - A vector misc field
  n_items <- sample(5:10, 1)
  ip <- generate_ip(n = n_items)
  resp_time <- sample(100:200, n_items)
  ip$resp_time <- resp_time
  i <- sample(1:n_items, 1)
  expect_identical(ip[[i]]@misc$resp_time, resp_time[i])

  # -------------------------------------------------------------------------- #
  # Add misc field to all items - A list misc field
  n_items <- 3
  ip <- generate_ip(n = n_items)
  test_specs <- list(list(accom = TRUE, level = 1),
                     list(accom = TRUE, level = 3),
                     list(accom = FALSE, level = 2))
  ip$test_specs <- test_specs
  i <- sample(1:n_items, 1)
  expect_identical(ip[[i]]@misc$test_specs, test_specs[[i]])

  # -------------------------------------------------------------------------- #
  # Modify an existing misc field
  n_items <- sample(5:10, 1)
  ip <- generate_ip(n = n_items)
  expect_false(any(ip$key %in% c("X", "Y", "Z")))
  new_keys <- sample(c("X", "Y", "Z"), n_items, replace = TRUE)
  ip$key <- new_keys
  i <- sample(1:n_items, 1)
  expect_identical(ip[[i]]@misc$key, new_keys[[i]])

  # -------------------------------------------------------------------------- #
  # If the length of value is not equal to the number of elements,
  # than add it as a misc field to the item pool
  n_items <- sample(5:10, 1)
  ip <- generate_ip(n = n_items)
  ip$test_id <- "Fall2022-MTH"
  expect_identical(ip@misc$test_id, "Fall2022-MTH")

  # -------------------------------------------------------------------------- #
  # Setting a 'misc' field to NULL deletes that field
  ip <- generate_ip(model = "GRM", n = 5)
  ip$form <- LETTERS[1:5]
  expect_equal(ip[[1]]@misc$form, "A")
  ip$form <- NULL
  expect_null(ip[[1]]@misc$form)

  # -------------------------------------------------------------------------- #
  # Setting a 'misc' field to NULL deletes that field even if some of the items
  # do not have that misc field
  ip <- generate_ip(model = "GRM", n = 5)
  ip$form <- LETTERS[1:5]
  ip@item_list[[2]]@misc$form <- NULL
  expect_true(is.na(ip$form[2]))
  ip$form <- NULL
  expect_null(ip[[1]]@misc$form)

  # -------------------------------------------------------------------------- #
  # Setting a 'misc' field to NULL deletes that field from overall Itempool misc
  # but it does not effect item's misc field.
  ip <- generate_ip(model = "GRM", n = 5)
  ip$form_name <- LETTERS[1:5]
  ip$form_name <- "F1"
  expect_equal(ip@misc$form_name, "F1")
  expect_equal(ip[[1]]@misc$form_name, "A")
  ip$form_name <- NULL
  expect_null(ip@misc$form_name)
  expect_equal(ip[[1]]@misc$form_name, "A")

})



###############################################################################@
############################# as.list (Itempool) ###############################
###############################################################################@

test_that("Test as.list( Itempool)", {
  # Each element of the "Itempool" should be "Item" class
  ip <- itempool(a = runif(4, .5, 1.5), b = rnorm(4), c = runif(4, 0,.3),
                 item_id = paste0("Item_",1:4), content = rep("Algebra", 4))
  expect_type(as.list(ip), "list")

  # -------------------------------------------------------------------------- #
  # mirt
  ip <- itempool(list(
      new("M2PL", item_id = "Item 1", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 2", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 3", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 4", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 5", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7)
    ))
  expect_type(as.list(ip), "list")
})


###############################################################################@
############################# as.data.frame (Itempool) #########################
###############################################################################@

test_that("Test as.data.frame (Itempool)", {
  # Each element of the "Itempool" should be "Item" class
  ip <- generate_ip(model = "3PL", n = 4, content = rep("Algebra", 4))
  ipdf <- as.data.frame(ip)
  expect_identical(ipdf$model, rep("3PL", 4))
  expect_true(is.data.frame(ipdf))
  expect_true(all(c("item_id", "model", "a", "b", "c", "D", "content") %in%
                    colnames(ipdf)))
  expect_identical(rownames(ipdf), paste0(1:ip$n$items))
  expect_identical(ipdf$item_id, ip$item_id)
  expect_identical(setNames(ipdf$a, ip$item_id), ip$a)
  expect_identical(setNames(ipdf$b, ip$item_id), ip$b)
  expect_identical(setNames(ipdf$c, ip$item_id), ip$c)
  expect_identical(setNames(ipdf$D, ip$item_id), ip$D)
  expect_identical(setNames(ipdf$model, ip$item_id), ip$model)
  expect_identical(setNames(ipdf$content, ip$item_id), ip$content)
  expect_type(setNames(ipdf$content, ip$item_id), 'character')

  # -------------------------------------------------------------------------- #
  # Single item:
  ip <- itempool(data.frame(a = 7.4, b = rnorm(1), c = .3, D = 1.7),
                     item_id = paste0("Item_",1), content = rep("Algebra", 1))
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))

  # -------------------------------------------------------------------------- #
  # mirt
  ip <- itempool(list(
      new("M2PL", item_id = "Item 1", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 2", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 3", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 4", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 5", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7)
    ))
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))
  expect_identical(ipdf$item_id, ip$item_id)
  expect_identical(rownames(ipdf), paste0(1:ip$n$items))
  expect_identical(setNames(ipdf$model, ip$item_id), ip$model)
  expect_identical(setNames(ipdf$content, ip$item_id), ip$content)

  # -------------------------------------------------------------------------- #
  # A mixture of Dichotomous items without a testlet.
  i3 <- itempool(a = rlnorm(2, 0, .3), b = rnorm(2), c = runif(2, 0, .3),
                     item_id = paste0("i3-", 1:2))
  i4 <- itempool(a = rlnorm(2, 0, .3), b = rnorm(2), item_id = paste0("i4", 1:2))
  i5 <- itempool(b = rnorm(3), item_id = paste0("i5-", 1:3))

  ip <- c(i3, i4, i5)
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))
  expect_true(all(c("item_id", "model", "a", "b", "c", "D") %in% colnames(ipdf)))
  expect_identical(ipdf$item_id, ip$resp_id)
  expect_identical(rownames(ipdf), paste0(1:ip$n$items))
  expect_identical(setNames(ipdf$model, ip$item_id), ip$item_model)

  # -------------------------------------------------------------------------- #
  # A mixture of Dichotomous items without a testlet.
  ip <- generate_ip(model = c("2PL", "1PL", "4PL", "4PL", "1PL", "3PL",
                              "Rasch"))
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))
  expect_true("model" %in% colnames(as.data.frame(ip)))
  expect_true(all(c("item_id", "model", "a", "b", "c", "d", "D") %in%
                    colnames(ipdf)))
  expect_true(is.character(ipdf$item_id))
  expect_identical(ipdf$item_id, ip$resp_id)
  expect_identical(ipdf$item_id, ip$item_id)
  expect_identical(rownames(ipdf), paste0(1:ip$n$items))
  expect_identical(setNames(ipdf$model, ip$item_id), ip$item_model)

  # -------------------------------------------------------------------------- #
  # A mixture of testlets and items but all of the standalone items and the
  # testlet items has the same model, All D's are equal
  n_t1 <- sample(3:9, 1)
  n_t2 <- sample(3:9, 1)
  n_t3 <- 1
  n_i1 <- sample(3:9, 1)
  n_i2 <- sample(3:9, 1)
  t1 <- testlet(itempool(
    a = rlnorm(n_t1, 0, .3), b = rnorm(n_t1), c = runif(n_t1, 0, .3),
    d = runif(n_t1, .95, 1), item_id = paste0("t1-i", 1:n_t1)), testlet_id = "t1")
  t2 <- testlet(itempool(
    a = rlnorm(n_t2, 0, .3), b = rnorm(n_t2), c = runif(n_t2, 0, .3),
    d = runif(n_t2, .95, 1), item_id = paste0("t2-i", 1:n_t2)), testlet_id = "t2")
  t3 <- testlet(itempool(
    a = rlnorm(n_t3, 0, .3), b = rnorm(n_t3), c = runif(n_t3, 0, .3),
    d = runif(n_t3, .95, 1), item_id = paste0("t3-i", 1:n_t3)), testlet_id = "t3")
  i1 <- generate_ip(n = n_i1, model = "4PL", item_id = paste0("i1-", 1:n_i1))
  i2 <- generate_ip(n = n_i2, model = "4PL", item_id = paste0("i2-", 1:n_i2))
  ip <- c(t1, i1, t2, i2, t3)
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))
  # Testlet column is correctly specified
  expect_true(all(t1$testlet_id == ipdf[1:n_t1, "testlet_id"]))
  expect_true(is.na(ipdf[n_t1 + 1, "testlet_id"]))
  # Check parameters
  expect_identical(t2[[2]]$a, ipdf[n_t1 + n_i1 + 2, "a"])
  expect_identical(i1[[3]]$b, ipdf[n_t1 + 3, "b"])
  # Check model
  expect_true(all(ipdf$model == "4PL"))

  # -------------------------------------------------------------------------- #
  # A mixture of testlets and items but all of the standalone items and the
  # testlet items has the same model, D's are different
  n_t1 <- sample(3:6, 1)
  n_t2 <- sample(3:6, 1)
  n_t3 <- 1
  n_i1 <- sample(3:9, 1)
  n_i2 <- sample(3:9, 1)
  weird_D <- 92.8174
  t1 <- testlet(itempool(
    a = rlnorm(n_t1, 0, .3), b = rnorm(n_t1), c = runif(n_t1, 0, .3),
    d = runif(n_t1, .95, 1), item_id = paste0("t1-i", 1:n_t1), D = 1),
    testlet_id = "t1")
  t2 <- testlet(itempool(
    a = rlnorm(n_t2, 0, .3), b = rnorm(n_t2), c = runif(n_t2, 0, .3),
    d = runif(n_t2, .95, 1), item_id = paste0("t2-i", 1:n_t2), D = weird_D),
    testlet_id = "t2")
  t3 <- testlet(itempool(
    a = rlnorm(n_t3, 0, .3), b = rnorm(n_t3), c = runif(n_t3, 0, .3),
    d = runif(n_t3, .95, 1), item_id = paste0("t3-i", 1:n_t3)),
    testlet_id = "t3")
  i1 <- generate_ip(n = n_i1, model = "4PL", item_id = paste0("i1-", 1:n_i1))
  i2 <- generate_ip(n = n_i2, model = "4PL", item_id = paste0("i2-", 1:n_i2))
  ip <- c(t1, c(i1, i2), t2, t3)
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))
  expect_identical(ipdf$D[n_t1 + n_i1 + n_i2 + 1], weird_D)
  expect_identical(ipdf$item_id, ip$resp_id)
  expect_identical(rownames(ipdf), paste0(1:ip$n$items))
  expect_identical(setNames(ipdf$model, ip$resp_id), ip$item_model)

  # -------------------------------------------------------------------------- #
  # PCM with different number of thresholds
  ip <- itempool(list(
    new("PCM", item_id = "PCM-1", content = "Stress", b = c(-0.837, 0.529)),
    new("PCM", item_id = "PCM-2", content = "Stress", b = c(-0.837, 0.529, 1.2))
    ))
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))
  expect_identical(colnames(ipdf),
                   c("item_id", "model", "b1", "b2", "b3", "content"))
  expect_identical(ipdf$item_id, ip$resp_id)
  expect_identical(rownames(ipdf), paste0(1:ip$n$items))
  expect_identical(setNames(ipdf$model, ip$item_id), ip$item_model)

  # -------------------------------------------------------------------------- #
  # GPCM with different number of thresholds
  ip <- itempool(
    list(new("GRM", item_id = "GRM-1", content = "Stress",
             a = 0.888, b = c(-0.837, -0.529, -0.382, 1.649), D = 1.702),
         new("GRM", item_id = "GRM-2", content = "Depression",
             a = 1.081, b = c(-0.692, -0.157,  0.567), D = 1.702)
            ))
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))
  expect_identical(colnames(ipdf), c("item_id", "model", "a", "b1", "b2", "b3",
                                 "b4", "D", "content"))
  expect_identical(ipdf$item_id, ip$resp_id)
  expect_identical(rownames(ipdf), paste0(1:ip$n$items))
  expect_identical(setNames(ipdf$model, ip$item_id), ip$item_model)

  # -------------------------------------------------------------------------- #
  # mirt with different number of dimensions
  ip <- itempool(list(
    new("M2PL", item_id = "Item 1", content = "Algebra",
        a = c(runif(2,1,2)), d = rnorm(1), D = 1.7),
    new("M2PL", item_id = "Item 2", content = "Geometry",
        a = c(runif(3,1,2)), d = rnorm(1), D = 1.7),
    new("M2PL", item_id = "Item 3", content = "Geometry",
        a = c(runif(4,1,2)), d = rnorm(1), D = 1.7)
    ))
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))
  expect_identical(colnames(ipdf), c("item_id", "model", "a1", "a2", "a3", "a4",
                                 "d", "D", "content"))
  expect_identical(ipdf$item_id, ip$resp_id)
  expect_identical(rownames(ipdf), paste0(1:ip$n$items))
  expect_identical(setNames(ipdf$model, ip$item_id), ip$item_model)

  # -------------------------------------------------------------------------- #
  # GRM item with only two categories and a 3PL item
  # Check whether the "Item_1" with only 2 categories converted to the data
  # frame nicely. It the threshold parameter is not labeled as "b1" but as "b"
  ip <- c(
    generate_item(model = "GRM", n_categories = 2),
    generate_item(model = "GRM", n_categories = 3),
    generate_item(model = "3PL", n = 4)
    )
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))

  # -------------------------------------------------------------------------- #
  # A mixture of Polytomous items without a testlet.
  ip <- itempool(list(
    new("GRM", item_id = "GRM-1", content = "Stress",
        a = 0.888, b = c(-0.837, -0.529, -0.382, 1.649), D = 1.702),
    new("GRM", item_id = "GRM-2", content = "Stress",
        a = 1.41, b = c(-0.837, 0.529), D = 1.702),
    new("GRM", item_id = "GRM-3", content = "Depression",
        a = 1.081, b = c(-0.692, -0.157,  0.567), D = 1.702),
    new("GPCM", item_id = "GPCM-1", content = "Stress",
        a = 0.888, b = c(-0.837, -0.529, 0.382, 1.649), D = 1.702),
    new("GPCM", item_id = "GPCM-2", content = "Stress",
        a = 1.41, b = c(-0.837, 0.529), D = 1.702),
    new("GPCM", item_id = "GPCM-3", content = "Depression",
        a = 1.081, b = c(-0.692, -0.157,  0.567), D = 1.702),
    new("PCM", item_id = "PCM-1", content = "Stress", b = c(-0.837, 0.529)),
    new("PCM", item_id = "PCM-2", content = "Stress", b = c(-0.837, 0.529, 1.2))))
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))
  expect_true(all(c("item_id", "model", "a", "b1", "b2", "b3", "b4", "D",
                    "content") %in% colnames(ipdf)))
  expect_identical(ipdf$item_id, ip$resp_id)
  expect_identical(setNames(ipdf$content, ip$resp_id), ip$item_content)
  expect_identical(rownames(ipdf), paste0(1:ip$n$items))
  expect_identical(setNames(ipdf$model, ip$resp_id), ip$item_model)

  # -------------------------------------------------------------------------- #
  # Polytomous items with a testlet.
  t1 <- testlet(itempool(list(
    new("GRM", item_id = "t1GRM-1", content = "Stress",
        a = 0.888, b = c(-0.837, -0.529, -0.382, 1.649), D = 1.702),
    new("GRM", item_id = "t1GRM-2", a = 1.41, b = c(-0.837, 0.529), D = 1.702))),
    testlet_id = "t1")
  i2 <- itempool(list(
    new("GRM", item_id = "GRM-1", content = "Stress",
        a = 0.888, b = c(-0.837, -0.529, -0.382, 1.649), D = 1.702),
    new("GRM", item_id = "GRM-2", content = "Depression",
        a = 1.41, b = c(-0.837, 0.529), D = 1.702)))

  ip <- c(t1, i2)
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))
  expect_true(all(c("item_id", "testlet_id", "model", "a", "b1", "b2", "b3",
                    "b4", "D", "content") %in% colnames(ipdf)))
  expect_identical(setNames(ipdf$content, ip$resp_id), ip$item_content)
  expect_identical(ipdf$item_id, ip$resp_id)
  expect_identical(rownames(ipdf), paste0(1:ip$n$items))
  expect_identical(setNames(ipdf$model, ip$resp_id), ip$item_model)

  # -------------------------------------------------------------------------- #
  # A mixture of Dichotmous and Polytomous items without a testlet.
  i1 <- itempool(a = rlnorm(4, 0, .3), b = rnorm(4), c = runif(4, 0, .3),
                     d = runif(4, .95, 1), item_id = paste0("i1-", 1:4))
  i2 <- itempool(list(
    new("GRM", item_id = "GRM-1", content = "Stress",
        a = 0.888, b = c(-0.837, -0.529, -0.382, 1.649), D = 1.702),
    new("GRM", item_id = "GRM-2", content = "Depression",
        a = 1.081, b = c(-0.692, -0.157,  0.567), D = 1.702)))
  i3 <- itempool(a = rlnorm(2, 0, .3), b = rnorm(2), c = runif(2, 0, .3),
                 item_id = paste0("i3-", 1:2))
  i4 <- itempool(a = rlnorm(2, 0, .3), b = rnorm(2), item_id = paste0("i4-", 1:2))
  i5 <- itempool(b = rnorm(3), item_id = paste0("i5-", 1:3))

  ip <- c(i1, i2, i3, i4, i5)
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))
  expect_identical(setNames(ipdf$content, ip$item_id), ip$item_content)
  expect_identical(setNames(ipdf$content, ip$resp_id), ip$item_content)
  expect_identical(ipdf$item_id, ip$resp_id)
  expect_identical(rownames(ipdf), paste0(1:ip$n$items))
  expect_identical(setNames(ipdf$model, ip$resp_id), ip$item_model)
  expect_identical(setNames(ipdf$model, ip$item_id), ip$item_model)
  # The order of columns should the following way:
  expect_identical(colnames(ipdf),
                   c("item_id", "model", "a", "b", "c", "d", "b1", "b2",
                     "b3", "b4", "D", "content"))
  # cat("c(\"", paste0(colnames(ipdf), collaspse = "\", \""), "\")", sep = "")

  # -------------------------------------------------------------------------- #
  # A mixture of items: Dichotomous and Polytomous items scattered among
  # testlets
  t1 <- generate_testlet(n = 3, item_models = "3PL", testlet_id = "t1")
  i1 <- generate_ip(model = "3PL", n = 4, item_id = paste("i1-", 1:4))
  t2 <- generate_testlet(item_models = c("GRM", "3PL", "3PL", "3PL", "GRM"),
                         testlet_id = "t2", item_id_preamble = "t2-")
  i2 <- c(generate_item(model = "GRM", n_categories = 3, item_id = "grm-1",
                        content = "Stress"),
          generate_item(model = "GRM", n_categories = 4, item_id = "grm-2",
                        content = "Depression"))
  t3 <- generate_testlet(item_models = c("GRM", "2PL", "3PL", "Rasch", "GPCM"),
                         testlet_id = "t3", item_id_preamble = "t3-")
  ip <- c(t1, i1, t2, i2, t3)
  ipdf <- as.data.frame(ip)
  expect_true(is.data.frame(ipdf))
  expect_identical(ipdf$item_id, ip$resp_id)
  expect_identical(rownames(ipdf), paste0(1:ip$n$items))
  expect_identical(setNames(ipdf$model, ip$resp_id), ip$item_model)

  # -------------------------------------------------------------------------- #
  # If a misc field has one printable value, it should be added as a column.
  ip <- c(generate_item(misc = list(form = "Form-1", key = "A")),
          generate_item(misc = list(form = "Form-1", key = "C", level = "C-")),
          # Add an item without any misc field
          generate_item(),
          # add an item with a misc field length more than 1
          generate_item(misc = list(form = "Form-1", key = "C",
                                    enemies = c("i1", "i2"))),
          generate_item(misc = list(form = "Form-1", key = "B", level = 12))
          )
  ipdf <- as.data.frame(ip)
  # a misc field with length more than 1 is not included in the data frame
  expect_true("enemies" %in% colnames(ipdf))
  expect_true(all(c("form", "key", "level") %in% colnames(ipdf)))
  expect_identical(ipdf$form[5], ip[[5]]$misc$form)
  expect_identical(ipdf$level[2], ip[[2]]$misc$level)
  # numeric misc field converted to character
  expect_identical(ipdf$level[5], as.character(ip[[5]]$misc$level))
  expect_true(is.na(ipdf$form[3]))

  # -------------------------------------------------------------------------- #
  # misc field when item pool has testlets
  i1 <- item(b = 1, item_id = "I-1", content = "bec",
             misc = list(sympson_hetter_k = 0.1,
                         form = "A1"))
  t1 <- testlet(itempool(b = rnorm(2), item_id = paste0("t1-i", 1:2),
                         misc = list(list(sympson_hetter_k = .2, form = "b3"),
                                     list(sympson_hetter_k = .3))),
                   testlet_id = "t1")
  ip <- c(i1, t1)
  ipdf <- as.data.frame(ip)
  expect_identical(ipdf$form[1], i1@misc$form)
  expect_identical(ipdf$sympson_hetter_k[1], i1@misc$sympson_hetter_k)
  expect_identical(ipdf$form[2], t1[[1]]@misc$form)
  expect_identical(ipdf$sympson_hetter_k[2], t1[[1]]@misc$sympson_hetter_k)
  expect_identical(ipdf$form[3], as.character(NA))
  expect_identical(ipdf$sympson_hetter_k[3], t1[[2]]@misc$sympson_hetter_k)

  # -------------------------------------------------------------------------- #
  # "2PL" item pool with standard errors
  ip <- generate_ip(n = 10, model = sample(c("2PL", "GRM"), 10, T), se = TRUE)
  ipdf <- as.data.frame(ip)
  expect_true("se_a" %in% colnames(ipdf))
  expect_true("se_b" %in% colnames(ipdf))
  expect_true("se_b1" %in% colnames(ipdf))


  # -------------------------------------------------------------------------- #
  # In the code, I hard coded the correct order of the item parameters that
  # should be appear in data frame. If more parameters/models will be added
  # to PMODELS, the code will become a bug. The following lines check for that
  # hard coded part. An error will be raised if the code needs to be updated.

  # All of the possible parameters names
  par_names <- Reduce(union,
                      lapply(unique(PMODELS), function(i) names(i$parameters)))

  # The parameters whose length are longer than 1. For example, "b" parameter
  # can be vectorized, so in data frame it can appear as b1, b2, ....
  vectorized_pars <- unname(lapply(lapply(PMODELS, function(m)
    sapply(m$parameters, `[[`, "size")), function(n) n[n > 1]))
  vectorized_pars <- unlist(unique(
    vectorized_pars[sapply(vectorized_pars, length) > 0]))

  # I assumed that all of the item parameters are the following:
  for (i in 1:length(vectorized_pars)) {
    par_names <- c(par_names, paste0(names(vectorized_pars)[i],
                                     1:vectorized_pars[i]))
  }

  # Hardcoded code piece in the `as.data.frame.Itempool()` function
  all_possible_par_names <- c("a", paste0("a", 1:100), "b", "c", "d",
                              paste0("b", 1:100), paste0("d", 1:100), "D")

  expect_true(all(par_names %in% all_possible_par_names))

  # # All of the SE parameters
  # Reduce(union, unlist(lapply(PMODELS,
  #                             function(m) sapply(m$parameters, `[[`, "se"))))

  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # - se                                                                     - #
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #

  # All same dichotomous models where all items have se
  ip <- generate_ip(model = "2PL", se = TRUE)
  ipdf <- as.data.frame(ip)
  expect_true("se_a" %in% colnames(ipdf))
  expect_true("se_b" %in% colnames(ipdf))
  i <- sample(1:length(ip), 1)
  expect_identical(ipdf$se_a[i], ip[[i]]@se_a)
  expect_identical(ipdf$se_b[i], ip[[i]]@se_b)
  # When 'include_se = FALSE' se are not included in data frame
  ipdf <- as.data.frame(ip, include_se = FALSE)
  expect_false("se_a" %in% colnames(ipdf))
  expect_false("se_b" %in% colnames(ipdf))

  # -------------------------------------------------------------------------- #
  # All same polytomous models with same number of categories where all items
  # have se
  ip <- generate_ip(model = "GPCM", se = TRUE)
  ipdf <- as.data.frame(ip)
  expect_true("se_a" %in% colnames(ipdf))
  expect_true("se_b2" %in% colnames(ipdf))
  i <- sample(1:length(ip), 1)
  expect_identical(ipdf$se_a[i], ip[[i]]@se_a)
  expect_identical(ipdf$se_b1[i], ip[[i]]@se_b[1])

  # -------------------------------------------------------------------------- #
  # All same polytomous models but with different number of categories where
  # all items have se
  n_items <- sample(5:10, 1)
  n_categories <- sample(3:7, n_items, replace = TRUE)
  ip <- generate_ip(model = "GPCM", n = n_items, n_categories = n_categories,
                    se = TRUE)
  ipdf <- as.data.frame(ip)
  expect_true("se_a" %in% colnames(ipdf))
  expect_true("se_b1" %in% colnames(ipdf))
  i <- sample(1:length(ip), 1)
  expect_identical(ipdf$se_a[i], ip[[i]]@se_a)
  expect_identical(ipdf$se_b1[i], ip[[i]]@se_b[1])

  # -------------------------------------------------------------------------- #
  # Mixture of polytomous and dichotomous items with different number of
  # categories where all items have se
  n_items <- sample(9:19, 1)
  n_categories <- sample(3:7, n_items, replace = TRUE)
  models <- c("2PL", "3PL", "GPCM", "GPCM2",
              sample(c("2PL", "3PL", "GPCM", "GPCM2"), n_items - 4,
                     replace = TRUE))
  ip <- generate_ip(model = models, n_categories = n_categories, se = TRUE)
  ipdf <- as.data.frame(ip)
  expect_true("se_a" %in% colnames(ipdf))
  expect_true("se_b" %in% colnames(ipdf))
  i <- sample(1:length(ip), 1)
  expect_identical(ipdf$se_a[i],  ip[[i]]@se_a)
  expect_identical(ipdf$se_b[2],  ip[[2]]@se_b)
  expect_identical(ipdf$se_b2[3], ip[[3]]@se_b[2])
  expect_identical(ipdf$se_b[4],  ip[[4]]@se_b)
  expect_identical(ipdf$se_d2[4], ip[[4]]@se_d[2])

  # -------------------------------------------------------------------------- #
  # An item pool without any se should not return any se
  n_items <- sample(9:19, 1)
  n_categories <- sample(3:7, n_items, replace = TRUE)
  models <- c("2PL", "3PL", "GPCM", "GPCM2",
              sample(c("2PL", "3PL", "GPCM", "GPCM2"), n_items - 4,
                     replace = TRUE))
  ip <- generate_ip(model = models, n_categories = n_categories, se = NULL)
  ipdf <- as.data.frame(ip)
  expect_false("a_se" %in% colnames(ipdf))
  expect_false("b_se" %in% colnames(ipdf))

  # -------------------------------------------------------------------------- #
  # Misc fields that list objects should be added as a list to the data frame
  ip <- generate_ip(n = 3, model = "GRM")
  temp_order <- c(3, 5, 6)
  temp_enemies <- list(c("i1", "i2"), c("i9", "i6"), c("i3", "i8"))
  ip$order <- temp_order
  ip$enemies <- temp_enemies
  expect_identical(ip[[1]]@misc$enemies, temp_enemies[[1]])
  ipdf <- as.data.frame(ip)
  expect_true("enemies" %in% colnames(ipdf))
  expect_identical(ipdf$enemies[[1]], temp_enemies[[1]])

  # -------------------------------------------------------------------------- #
  # Convert another complicated item pool to data.frame
  ip <- new(
    "Itempool",
    item_list = list(
      t1_Item_1 = new("GPCM",  a = 0.6882, b = c(0.1523, 0.5282, 1.3779),
                      D = 1, se_a = NULL, se_b = NULL, item_id = "t1_Item_1",
                      content = NULL, misc = list(possible_options = list(NA))),
      t1_Item_2 = new("3PL", a = 1.0781, b = -0.0519, c = 0.1116, D = 1,
                      se_a = NULL, se_b = NULL, se_c = NULL, item_id = "t1_Item_2",
                      content = NULL,
                      misc = list(key = "B",
                                  possible_options = list(c("A", "B", "C", "D"))
                                  )
                      )),
    misc = NULL)
  expect_true(is.data.frame(as.data.frame(ip)))

})


###############################################################################@
############################# is.Itempool ######################################
###############################################################################@

test_that("Test is.Itempool", {
  # Each element of the "Itempool" should be "Item" class
  item1 <- item(a = 1.12, b = -2.1, c = 0.28)
  item2 <- item(a = 2, b = 3.2, c = 0.21)
  expect_true(is.Itempool(c(item1, item2)))

  # -------------------------------------------------------------------------- #
  # mirt
  ip <- itempool(list(
      new("M2PL", item_id = "Item 1", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 2", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 3", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 4", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 5", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7)
    ))
  expect_true(is.Itempool(ip))
})


###############################################################################@
############################# length (Itempool) ################################
###############################################################################@

test_that("Test length (Itempool)", {
  n <- sample(10:100,1)
  ip <- itempool(a = runif(n, .5, 1.5), b = rnorm(n),
                 c = runif(n, 0,.3), item_id = paste0("Item_",1:n))
  expect_true(length(ip) == n)

  # -------------------------------------------------------------------------- #
  # mirt
  ip <- itempool(list(
      new("M2PL", item_id = "Item 1", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 2", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 3", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 4", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 5", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7)
    ))
  expect_identical(length(ip), 5L)
})


###############################################################################@
################### $ (Itempool) ###############################################
###############################################################################@

test_that("Test $ operator", {
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # Overall Tests
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #

  # Each element of the "Itempool" should be "Item" class
  n <- sample(10:20, 1)
  ip <- itempool(a = runif(n, .5, 1.5), b = rnorm(n), c = runif(n, 0, .3),
                 item_id = paste0("Item_",1:n), content = rep("Algebra", n))
  ip_list <- ip$item_list
  i <- sample(1:n, 1)
  expect_identical(ip$model[i], setNames("3PL", ip$item_id[i]))
  expect_identical(names(ip$model)[i], ip$item_id[i]) # vectors should be named
  expect_identical(ip$content[i], setNames("Algebra", ip$item_id[i]))
  expect_identical(names(ip$content)[i], ip$item_id[i]) # vectors should be named
  expect_identical(ip$item_id[i], paste0("Item_", i))
  expect_true(inherits(ip$parameters, "matrix"))
  expect_identical(unname(ip$parameters[3, 1]), ip[[3]]@a)
  expect_identical(unname(ip$parameters[3, "a"]), ip[[3]]@a)
  # Extract single parameter
  expect_identical(ip$a[i], setNames(ip_list[[i]]@a, ip$item_id[i]))
  expect_identical(ip$b[i], setNames(ip_list[[i]]@b, ip$item_id[i]))
  expect_identical(ip$D[i], setNames(ip_list[[i]]@D, ip$item_id[i]))
  expect_identical(names(ip$a)[i], ip$item_id[i]) # vectors should be named
  expect_identical(names(ip$b)[i], ip$item_id[i]) # vectors should be named
  expect_identical(names(ip$D)[i], ip$item_id[i]) # vectors should be named


  # Benchmarking
  # n <- sample(1000:2000, 1)
  # ip <- generate_ip(n = n, item_id = paste0("Item_",1:n),
  #                   content = rep("Algebra", n))
  #
  # library(Rcpp)
  # cppFunction('
  # Rcpp::Nullable<Rcpp::StringVector> gsic(Rcpp::S4 ip, std::string slotName)
  # {
  #   Rcpp::S4 tempS4;
  #   List item_list = as<List>(ip.slot("item_list"));
  #   int num_items = item_list.size();
  #   Rcpp::StringVector output(num_items);
  #   int count_empty_str = 0;
  #   for (int i = 0; i < num_items; i++)
  #   {
  #     tempS4 = as<Rcpp::S4> (item_list(i));
  #     if (Rf_isNull(tempS4.slot(slotName))) {
  #       output[i] = StringVector::get_na();
  #       count_empty_str++;
  #     } else output[i] = as<std::string>(tempS4.slot(slotName));
  #   }
  #   if (count_empty_str == num_items)
  #   {
  #     return R_NilValue;
  #   }
  #   return(output);
  # }
  # ')
  #
  # microbenchmark::microbenchmark(
  #   cpp = gsic(ip, "model"),
  #   R = sapply(ip@item_list, slot, "model"),
  #   times = 1e2
  #   )


  # -------------------------------------------------------------------------- #
  # Test with various item types
  ip <- itempool(list(
    item(b = 1, D = 1, item_id = "I-1", content = "bec"),
    item(a = 1, b = 2, item_id = "I-2", content = "cab"),
    item(a = 1, b = 2, c = .12, item_id = "I-3", content = "dac")))
  expect_identical(ip$model[1], setNames("1PL", ip$item_id[1]))
  expect_identical(ip$content[2], setNames("cab", ip$item_id[2]))
  expect_identical(ip$item_id[3], "I-3")

  expect_true(is(ip$parameters, "matrix"))
  expect_identical(unname(ip$parameters[3, "c"]), 0.12)

  # -------------------------------------------------------------------------- #
  # Test 1PL
  ip <- itempool(data.frame(b = rnorm(10)))
  expect_identical(ip$model[1], setNames("Rasch", ip$item_id[1]))
  expect_true(is.null(ip$content))
  expect_identical(ip$item_id[3], "Item_3")
  expect_true(inherits(ip$parameters, "matrix"))
  expect_type(ip$parameters[,"b"], "double")

  # -------------------------------------------------------------------------- #
  # For GPCM with different number of categories, "$" still can return parameter
  ip <- itempool(lapply(2:5, function(x)
    generate_item(model = "GPCM", n_categories = x)))
  a <- sapply(ip$item_list, function(x) x$a)
  expect_identical(a, ip$a)
  expect_identical(names(ip$a), ip$item_id)

  # -------------------------------------------------------------------------- #
  # One should not extract an unknown parameter
  ip <- itempool(b = rnorm(4))
  expect_null(ip$kkk)
  expect_null(ip$a)

  # -------------------------------------------------------------------------- #
  # mirt
  ip <- itempool(list(
      new("M2PL", item_id = "Item 1", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 2", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 3", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 4", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 5", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7)
    ))
  expect_identical(ip$model[1], setNames("M2PL", ip$item_id[1]))
  expect_identical(ip$item_id[3], "Item 3")
  expect_true(inherits(ip$parameters, "matrix"))
  expect_identical(unname(ip$parameters[3, 2]), ip@item_list[[3]]@a[2])
  expect_identical(ip$parameters[,'d'][4],
                   setNames(ip@item_list[[4]]@d, ip$item_id[4]))
  expect_identical(ip$content[4], setNames("Algebra", ip$item_id[4]))
  expect_identical(which(ip$content %in% c("Algebra"))[2], 4L)
  expect_identical(which(ip$content %in% c("Algebra", "Arithmetic"))[3], 5L)
  expect_identical(which(ip$content %in% c("Algebra")),
                    which(ip$content %in% c("Algebra", "Reading")))



  # -------------------------------------------------------------------------- #
  # Graded Response Model (GRM)
  ip <- itempool(
    list(
      new("GRM", item_id = "Item 1", content = "Stress",
          a = 0.888, b = c(-0.837, -0.529, -0.382,  1.649), D = 1.702),
      new("GRM", item_id = "Item 2", content = "Depression",
          a = 1.081, b = c(-0.692, -0.157,  0.567,  0.646), D = 1.702)))
  expect_identical(ip$model[1], setNames("GRM", ip$item_id[1]))
  expect_identical(names(ip$model)[1], "Item 1")
  expect_identical(ip$item_id[2], "Item 2")
  expect_true(inherits(ip$parameters, "matrix"))


  # -------------------------------------------------------------------------- #
  # Graded Response Model (GRM) with se
  ip <- generate_ip(model = sample(c("GRM", "3PL", "GPCM2"), 15, T), se = TRUE)
  expect_true(all(c("d1", "b2", "a", "D") %in% colnames(ip$parameters)))
  expect_false(any(c("model", "item_id", "se_b1") %in% colnames(ip$parameters)))
  expect_true(all(c("se_d1", "se_b2", "se_a") %in% colnames(ip$se)))
  expect_false(any(c("model", "item_id", "D", "a", "b1") %in% colnames(ip$se)))

  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # $content
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  ip <- itempool(
    data.frame(a = runif(8, .5, 1.5), b = rnorm(8), c = runif(8, 0,.3)),
    item_id = paste0("MyItem_",1:8),
    content = c(rep("Algebra", 3), rep("Geometry", 2), rep("Arithmetic", 3)))
  expect_identical(ip$content[4], setNames("Geometry", ip$item_id[4]))
  expect_identical(ip$item_id[3], "MyItem_3")
  expect_identical(which(ip$content == "Algebra")[2], setNames(2L, ip$item_id[2]))
  expect_identical(which(ip$content %in% c("Algebra", "Arithmetic"))[4], 6L)
  expect_identical(which(ip$content %in% "Algebra"),
                    which(ip$content %in% c("Algebra", "Reading")))

  # -------------------------------------------------------------------------- #
  # Test content where some content is missing
  ip <- itempool(item(b = 0, content = "A"),
                 item(b = 1, item_id = "i2"),
                 item(b = 2, content = "C"))
  expect_true(is.na(ip$content[2]))

  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # $se
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #

  # All same dichotomous models where all items have se
  ip <- generate_ip(model = "2PL", se = TRUE)
  se_pars <- ip$se
  i <- sample(1:length(ip), 1)
  expect_identical(unname(se_pars[i, "se_a"]), ip[[i]]@se_a)
  i <- sample(1:length(ip), 1)
  expect_identical(unname(se_pars[i, "se_b"]), ip[[i]]@se_b)

  # -------------------------------------------------------------------------- #
  # All same dichotomous models where all items have se except one
  # is NULL
  ip <- generate_ip(model = "2PL", se = TRUE)
  ip[[1]]@se_a <- NULL
  se_pars <- ip$se
  i <- sample(2:length(ip), 1)
  expect_identical(unname(se_pars[i, "se_a"]), ip[[i]]@se_a)
  i <- sample(2:length(ip), 1)
  expect_identical(unname(se_pars[i, "se_b"]), ip[[i]]@se_b)
  expect_true(is.na(se_pars[1, "se_a"]))
  expect_identical(unname(se_pars[1, "se_b"]), ip[[1]]@se_b)

  # -------------------------------------------------------------------------- #
  # All same polytomous models with same number of categories where all items
  # have se
  ip <- generate_ip(model = "GPCM", se = TRUE)
  se_pars <- data.frame(ip$se)
  i <- sample(1:length(ip), 1)
  expect_identical(se_pars$se_a[i], ip[[i]]@se_a)
  i <- sample(1:length(ip), 1)
  expect_identical(se_pars$se_b1[i], ip[[i]]@se_b[1])


  # -------------------------------------------------------------------------- #
  # All same polytomous models with same number of categories where all items
  # have se except one is NULL
  ip <- generate_ip(model = "GPCM", se = TRUE)
  ip[[1]]@se_a <- NULL
  se_pars <- data.frame(ip$se)
  i <- sample(2:length(ip), 1)
  expect_identical(se_pars$se_a[i], ip[[i]]@se_a)
  i <- sample(2:length(ip), 1)
  expect_identical(se_pars$se_b1[i], ip[[i]]@se_b[1])
  expect_true(is.na(se_pars$se_a[1]))
  expect_false(is.na(se_pars$se_b1[1]))

  # -------------------------------------------------------------------------- #
  # All same polytomous models but with different number of categories where
  # all items have se
  n_items <- sample(5:10, 1)
  n_categories <- sample(3:7, n_items, replace = TRUE)
  ip <- generate_ip(model = "GPCM", n = n_items, n_categories = n_categories,
                    se = TRUE)
  se_pars <- data.frame(ip$se)
  i <- sample(1:length(ip), 1)
  expect_identical(se_pars$se_a[i], ip[[i]]@se_a)
  i <- sample(1:length(ip), 1)
  expect_identical(se_pars$se_b1[i], ip[[i]]@se_b[1])

  # -------------------------------------------------------------------------- #
  # All same polytomous models but with different number of categories where
  # all items have se except one is NULL
  n_items <- sample(5:10, 1)
  n_categories <- sample(3:7, n_items, replace = TRUE)
  ip <- generate_ip(model = "GPCM", n = n_items, n_categories = n_categories,
                    se = TRUE)
  ip[[1]]@se_a <- NULL
  se_pars <- data.frame(ip$se)
  i <- sample(2:length(ip), 1)
  expect_identical(se_pars$se_a[i], ip[[i]]@se_a)
  i <- sample(2:length(ip), 1)
  expect_identical(se_pars$se_b1[i], ip[[i]]@se_b[1])
  expect_true(is.na(se_pars$se_a[1]))
  expect_false(is.na(se_pars$se_b1[1]))

  # -------------------------------------------------------------------------- #
  # Mixture of polytomous and dichotomous items with different number of
  # categories where all items have se
  n_items <- sample(9:19, 1)
  n_categories <- sample(3:7, n_items, replace = TRUE)
  models <- c("2PL", "3PL", "GPCM", "GPCM2",
              sample(c("2PL", "3PL", "GPCM", "GPCM2"), n_items - 4,
                     replace = TRUE))
  ip <- generate_ip(model = models, n_categories = n_categories,
                    se = TRUE)
  se_pars <- data.frame(ip$se)
  expect_identical(se_pars$se_a[1], ip[[1]]@se_a)
  expect_identical(se_pars$se_b[1], ip[[1]]@se_b)
  expect_identical(se_pars$se_c[2], ip[[2]]@se_c)
  expect_identical(se_pars$se_a[3], ip[[3]]@se_a)
  expect_identical(se_pars$se_b[4], ip[[4]]@se_b)
  expect_identical(se_pars$se_d2[4], ip[[4]]@se_d[2])

  # -------------------------------------------------------------------------- #
  # Mixture of polytomous and dichotomous items with different number of
  # categories where all items have se except one is NULL
  n_items <- sample(9:19, 1)
  n_categories <- sample(3:7, n_items, replace = TRUE)
  models <- c("2PL", "3PL", "GPCM", "GPCM2",
              sample(c("2PL", "3PL", "GPCM", "GPCM2"), n_items - 4,
                     replace = TRUE))
  ip <- generate_ip(model = models, n_categories = n_categories, se = TRUE)
  ip[[length(ip)]]@se_a <- NULL
  se_pars <- data.frame(ip$se)

  expect_identical(se_pars$se_a[1], ip[[1]]@se_a)
  expect_identical(se_pars$se_b[1], ip[[1]]@se_b)
  expect_identical(se_pars$se_c[2], ip[[2]]@se_c)
  expect_identical(se_pars$se_a[3], ip[[3]]@se_a)
  expect_identical(se_pars$se_b[4], ip[[4]]@se_b)
  expect_identical(se_pars$se_d2[4], ip[[4]]@se_d[2])
  expect_true(all(is.na(unlist(se_pars[length(ip), "se_a"]))))

  # -------------------------------------------------------------------------- #
  # Mixture of polytomous and dichotomous items with different number of
  # categories where all items have se except one is NULL and other
  # item has one parameter SE which is NA
  n_items <- sample(9:19, 1)
  n_categories <- sample(3:7, n_items, replace = TRUE)
  models <- c("2PL", "3PL", "GPCM", "GPCM2",
              sample(c("2PL", "3PL", "GPCM", "GPCM2"), n_items - 4,
                     replace = TRUE))
  ip <- generate_ip(model = models, n_categories = n_categories,
                    se = TRUE)
  ip[[length(ip)]]@se_a <- NULL
  ip[[length(ip) - 1]]@se_a <- as.numeric(NA)
  se_pars <- data.frame(ip$se)
  expect_identical(se_pars$se_a[1], ip[[1]]@se_a)
  expect_identical(se_pars$se_b[1], ip[[1]]@se_b)
  expect_identical(se_pars$se_c[2], ip[[2]]@se_c)
  expect_identical(se_pars$se_a[3], ip[[3]]@se_a)
  expect_identical(se_pars$se_b[4], ip[[4]]@se_b)
  expect_identical(se_pars$se_d2[4], ip[[4]]@se_d[2])
  expect_true(all(is.na(unlist(se_pars[length(ip), "se_a"]))))
  expect_true(is.na(se_pars$se_a[length(ip) - 1]))

  # -------------------------------------------------------------------------- #
  # An item pool without any se should return NULL
  n_items <- sample(9:19, 1)
  n_categories <- sample(3:7, n_items, replace = TRUE)
  models <- c("2PL", "3PL", "GPCM", "GPCM2",
              sample(c("2PL", "3PL", "GPCM", "GPCM2"), n_items - 4,
                     replace = TRUE))
  ip <- generate_ip(model = models, n_categories = n_categories, se = NULL)
  expect_null(ip$se)


  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # $a, $b, $c, $D
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #

  # The parameter order should not be important
  # This test might be redundant.
  a1 <- rlnorm(1, 0, .3)
  b1 <- rnorm(1)
  c1 <- runif(1, 0, .3)
  D1 <- 1.7
  a2 <- rlnorm(1, 0, .3)
  b2 <- rnorm(1)
  c2 <- runif(1, 0, .3)
  D2 <- 1

  i1 <- new("3PL", b = b1, a = a1, D = D1, c = c1, misc = list(form = "F1"))
  i2 <- new("3PL", c = c2, D = D2, a = a2, b = b2, misc = list(form = "F2"))
  ip <- c(i1, i2)
  expect_identical(i1@a, a1)
  expect_identical(i2@a, a2)
  expect_identical(i1@b, b1)
  expect_identical(i2@b, b2)
  expect_identical(i1@c, c1)
  expect_identical(i2@c, c2)
  expect_identical(i1@D, D1)
  expect_identical(i2@D, D2)

  expect_identical(ip$a, setNames(c(a1, a2), ip$item_id))
  expect_identical(ip$b, setNames(c(b1, b2), ip$item_id))
  expect_identical(ip$c, setNames(c(c1, c2), ip$item_id))
  expect_identical(ip$D, setNames(c(D1, D2), ip$item_id))


  a1 <- rlnorm(1, 0, .3)
  b1 <- rnorm(1)
  c1 <- runif(1, 0, .3)
  D1 <- 1.7
  i1 <- item(b = b1, a = a1, D = D1, c = c1)
  expect_identical(i1@a, a1)
  expect_identical(i1@b, b1)
  expect_identical(i1@c, c1)
  expect_identical(i1@D, D1)

  i1 <- item(parameters = sample(list(b = b1, a = a1, D = D1, c = c1)))
  expect_identical(i1@a, a1)
  expect_identical(i1@b, b1)
  expect_identical(i1@c, c1)
  expect_identical(i1@D, D1)

  # -------------------------------------------------------------------------- #
  # Pulling parameters when there are multiple models:
  ip <- generate_ip(model = c("2PL", "GPCM", "GPCM2"))
  expect_false(is.null(ip$a))
  expect_null(ip$c)
  expect_identical(ip$a[1], setNames(ip[[1]]@a, ip$item_id[1]))
  expect_identical(ip$a[2], setNames(ip[[2]]@a, ip$item_id[2]))
  expect_identical(ip$a[3], setNames(ip[[3]]@a, ip$item_id[3]))
  expect_identical(ip$b[1], setNames(ip[[1]]@b, ip$item_id[1]))
  expect_true(is.na(ip$b[2]))
  expect_identical(ip$b[3], setNames(ip[[3]]$b, ip$item_id[3]))
  expect_true(is.na(ip$d1[1]))
  expect_true(is.na(ip$d1[2]))
  expect_identical(ip$d1[3], setNames(ip[[3]]$d[1], ip$item_id[3]))


  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # $resp_ip, $item_content, $item_model, $resp_item_list
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  i1 <- item(b = 1, item_id = "i1", content = "A")
  i2 <- item(a = 1, b = 1, item_id = "i2", content = "B")
  i3 <- item(a = 1, b = 1, c = 1, item_id = "i3", content = "C")
  i4 <- item(a = 1, b = c(1, 2), item_id = "i4", content = "D", model = "GRM")
  i5 <- item(a = 1, b = c(1, 2), item_id = "i5", content = "E", model = "GPCM")
  t1 <- testlet(i2, i5, testlet_id = "testlet1", content = "TZM")
  t2 <- generate_testlet(n = 3, item_id_preamble = "t2-", testlet_id = "t2")
  ip <- c(i1, i3, t1, i4, t2)
  # There is a warning when printing this item pool:
  expect_output(show(ip), "Itempool")
  expect_identical(ip$id, c("i1", "i3", "testlet1", "i4", "t2"))
  expect_identical(ip$item_id, c("i1", "i3", "i2", "i5", "i4",
                             paste0("t2-Item_", 1:3)))
  expect_identical(ip$resp_id, c("i1", "i3", "i2", "i5", "i4",
                             paste0("t2-Item_", 1:3)))
  expect_identical(ip$content, setNames(c("A", "C", "TZM", "D", NA), ip$id))
  expect_identical(ip$item_content,
                   setNames(c("A", "C", "B", "E", "D", NA, NA, NA), ip$resp_id))
  expect_identical(names(ip$item_content), c("i1", "i3", "i2", "i5", "i4",
                                         paste0("t2-Item_", 1:3)))
  expect_identical(ip$model, setNames(c("Rasch", "3PL", "BTM", "GRM", "BTM"),
                                      ip$id))
  expect_identical(ip$item_model, setNames(c(
    "Rasch", "3PL", "2PL", "GPCM", "GRM", "3PL", "3PL", "3PL"), ip$resp_id))

  # $resp_item_list: A list of standalone items
  expect_true("testlet1" %in% ip$id)
  sa_ip_list <- ip$resp_item_list
  expect_false("testlet1" %in% names(sa_ip_list))
  expect_false("i2" %in% ip$id)
  expect_true("i2" %in% ip$item_id)
  expect_true("i2" %in% names(sa_ip_list))
  expect_identical(i4, sa_ip_list[[5]])
  expect_identical(t2[[2]], sa_ip_list[[7]])

  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # $n
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  expect_type(ip$n, "list")
  expect_identical(ip$n$testlets, 2L)
  expect_identical(ip$n$elements, 5L)
  expect_identical(ip$n$items, 8L)


  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # $items
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  ip <- generate_ip(n = 7)
  expect_type(ip$items, "list")
  expect_identical(length(ip$items), 7L)
  expect_identical(names(ip$items), ip$item_id)

  # With testlets
  ip <- c(generate_ip(n = 2), generate_testlet(n = 4))
  ip_list <- ip$items
  expect_type(ip_list, "list")
  expect_identical(length(ip_list), 6L)
  expect_identical(ip_list[[4]], ip[[3]]@item_list[[2]])
  expect_identical(names(ip_list), ip$resp_id)

  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # $max_Score
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #

  ip <- c(generate_ip(model = c("GPCM", "GRM", "3PL"),
                      n_categories = c(7, 5, 2)),
          generate_testlet(n = 4, item_models = c("GRM", "2PL", "GRM", "GPCM"),
                           n_categories = 3))
  expect_identical(ip$max_score, 18)

  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # $item_misc
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  i1 <- item(b = 1, item_id = "I-1", content = "bec",
             misc = list(sympson_hetter_k = 1,
                         form = "A1"))
  t1 <- testlet(itempool(b = rnorm(2), item_id = paste0("t1-i", 1:2),
                         misc = list(list(sympson_hetter_k = .8, form = "B1"),
                                     list(sympson_hetter_k = .9))),
                   testlet_id = "t1")
  t2 <- generate_testlet(n = 4, item_id = "t2")
  t2[[2]]@misc <- list(form = "B2")
  t3 <- generate_testlet(n = 3, item_id = "t3")
  i2 <- generate_item(misc = list(form = "A1"))
  i3 <- generate_item()
  ip <- c(i1, t1, i2, t2, i3, t3)
  misc <- ip$item_misc
  expect_identical(i1$misc, misc[[1]])
  expect_identical(t1@item_list$misc[[1]], misc[[2]])
  expect_identical(t1@item_list$misc[[2]], misc[[3]])
  expect_identical(t2@item_list$misc[[2]], misc[[6]])
  expect_identical(t2@item_list$misc[[3]], misc[[7]])
  expect_identical(i2$misc, misc[[4]])
  expect_identical(i3$misc, misc[[9]])
  expect_identical(t3@item_list$misc[[3]], misc[[ip$n$items]])

  # -------------------------------------------------------------------------- #
  # $item_misc returns NULL when no items or testlet has misc field
  ip <- c(generate_ip(model = "GPCM"), generate_testlet(item_models = "GPCM"))
  expect_null(ip$item_misc)

  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # $misc
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #

  # misc field should return a list
  ip <- c(generate_item(model = "GRM", misc = list(form = "F1")),
          generate_item(model = "GPCM", misc = list(form = "F2")))
  expect_type(ip$misc, "list")
  expect_identical(length(ip$misc), 2L)
  expect_identical(names(ip$misc), ip$item_id)
  expect_true(all(sapply(ip$misc, names) == "form"))

  # -------------------------------------------------------------------------- #
  # misc: when no misc field function should return NULL
  ip <- generate_ip(model = "GRM")
  expect_null(ip$misc)

  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # $item_max_score
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #

  ip <- generate_ip(n = 7)
  ip_max_score <- ip$item_max_score
  expect_identical(ip_max_score, setNames(rep(1, 7), ip$item_id))
  expect_identical(names(ip_max_score), ip$item_id)

  # With testlets
  ip <- c(generate_ip(n = 2),
          generate_testlet(n = 4, item_models = c("GRM", "2PL", "GRM"),
                           n_categories = 3))
  ip_max_score <- ip$item_max_score
  expect_identical(ip_max_score, setNames(c(1, 1, 2, 1, 2, 2), ip$resp_id))
  expect_identical(names(ip_max_score), ip$resp_id)

  # With testlets and polytomous items
  ip <- c(generate_ip(n = 2),
          generate_testlet(
            testlet_id = "t1",
            item_models = c("3PL", "GRM", "GPCM", "GRM", "2PL"),
            n_categories = c(2, 3, 6, 7, 2), item_id_preamble = "t1-"),
          generate_testlet(n = 3, testlet_id = "t2", item_id_preamble = "t2-"))
  ip_max_score <- ip$item_max_score
  expect_identical(names(ip_max_score), ip$resp_id)
  expect_identical(ip_max_score, setNames(c(1, 1, 1, 2, 5, 6, 1, 1, 1, 1),
                                          ip$resp_id))
})

###############################################################################@
############################# print.Itempool ###################################
###############################################################################@

test_that("Test print.Itempool", {
  # Each element of the "Itempool" should be "Item" class
  ip <- generate_ip(n = 4, item_id = paste0("Item_", 1:4),
                    content = rep("Algebra", 4))
  expect_output(print(ip), "An object of class 'Itempool'")
  expect_true(grepl("An object of class 'Itempool'", capture_output(print(ip))))
  expect_output(print(ip), "Model of items: (.*)3PL(.*)D = ")
  expect_output(print(ip), "D = 1")


  # -------------------------------------------------------------------------- #
  ip <- itempool(data.frame(a = runif(4, .5, 1.5), b = 1:4, D = 1:4),
                    item_id = paste0("Item_",1:4),
                    content = rep("Algebra", 4))
  expect_output(print(ip), "Model of items: ")
  expect_output(print(ip), "a[ ]+b[ ]+D")
  expect_output(print(ip), "Content = Algebra")

  # -------------------------------------------------------------------------- #
  # Test with various item types
  ip <- itempool(list(item(b = 1, item_id = "I-1", content = "bec"),
                      item(a = 1, b = 2, item_id = "I-2", content = "cab"),
                      item(a = 1, b = 2, c = .12, item_id = "I-3",
                           content = "dac")))
  expect_output(print(ip), "An object of class 'Itempool'")

  ip <- itempool(data.frame(b = rnorm(10)))
  expect_output(print(ip), "An object of class 'Itempool'")

  # -------------------------------------------------------------------------- #
  # Test Itempool with a misc field printed correctly
  i1 <- item(b = sort(rnorm(3)), item_id = "I-1", content = "bec",
             misc = list(sympson_hetter_k = 1, level = "A", word_count = 85))
  t1 <- testlet(itempool(
    b = rnorm(2), item_id = paste0("t1-i", 1:2),
    misc = list(list(sympson_hetter_k = .8, word_count = 85, level = "A"),
                list(sympson_hetter_k = .9, level = "B"))),
    testlet_id = "t1")
  ip <- c(i1, t1)
  expect_output(print(ip), "An object of class 'Itempool'")
  expect_output(print(ip, width = 120), "symps") # "sympson_hetter_k"

  # -------------------------------------------------------------------------- #
  # mirt
  ip <- itempool(list(
      new("M2PL", item_id = "Item 1", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 2", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 3", content = "Geometry", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 4", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7),
      new("M2PL", item_id = "Item 5", content = "Algebra", a = c(runif(3,1,2)),
          d = rnorm(1), D = 1.7)
    ))
  expect_output(print(ip), "[ ]+d[ ]+content")
  expect_output(print(ip), "D = 1.7")

  # -------------------------------------------------------------------------- #
  # GRM
  ip <- itempool(list(
    new("GRM", item_id = "Item 1", content = "Stress", a = 0.888,
        b = c(-0.837, -0.529, -0.382, 1.649), D = 1.702),
    new("GRM", item_id = "Item 2", content = "Depression", a = 1.081,
        b = c(-0.692, -0.157,  0.567, 0.646), D = 1.702)))
  expect_output(print(ip), "item_id[ ]+a[ ]+b1[ ]+b2[ ]+b3[ ]+b4[ ]+content")
  expect_output(print(ip), "Model of items: ")
  expect_output(print(ip), "D = 1.702")

  # -------------------------------------------------------------------------- #
  # An item pool consist of Testlet can be printed nicely.
  t1 <- testlet(itempool(b = rnorm(5), item_id = paste0("t1-i", 1:5)),
                   testlet_id = "t1")
  t2 <- testlet(itempool(b = rnorm(4), item_id = paste0("t2-i", 1:4)),
                   testlet_id = "t2")
  ip <- c(t1, t2)
  expect_output(object = print(t2), regexp = "t2-i2")
  expect_output(object = print(ip), regexp = "t2-i2")

  # -------------------------------------------------------------------------- #
  # If argument n is presented only that number of items printed to the console
  ip <- generate_ip(n = 50)
  expect_output(object = print(ip), regexp = "# ... with 40 more items")
  expect_output(object = print(ip, n = 45), regexp = "# ... with 5 more items")
  expect_output(object = print(ip), regexp = "Total number of items = 50")

  # -------------------------------------------------------------------------- #
  # If n is between 11 and 20 all of the items are printed
  ip <- generate_ip(n = sample(11:19, 1))
  text <- capture.output(print(ip))
  expect_false(any(grepl("with (.*) more items", text)))
  expect_false(any(grepl("Total number of items ", text)))

  # If n is between 1 and 10 all of the items are printed
  ip <- generate_ip(n = sample(1:10, 1))
  text <- capture.output(print(ip))
  expect_false(any(grepl("with (.*) more items", text)))
  expect_false(any(grepl("Total number of items ", text)))

  # -------------------------------------------------------------------------- #
  # For PCM, there shouldn't be a "D = " in the printed output
  ip <- itempool(list(
    new("PCM", item_id = "PCM-1", content = "Stress", b = c(-0.837, 0.529)),
    new("PCM", item_id = "PCM-2", content = "Stress", b = c(-0.837, 0.529, 1.2))))
  expect_output(print(ip), "PCM-1")
  expect_output(print(ip), "b1")
  expect_false(any(grepl(pattern = "D = ", capture.output(ip))))

  # -------------------------------------------------------------------------- #
  # A mixture of testlets and items but all of the standalone items and the
  # testlet items has the same model, All D's are equal
  n_t1 <- sample(5:9, 1)
  n_t2 <- sample(4:7, 1)
  n_t3 <- 1
  t1 <- testlet(itempool(
    a = rlnorm(n_t1, 0, .3), b = rnorm(n_t1), c = runif(n_t1, 0, .3),
    d = runif(n_t1, .95, 1), item_id = paste0("t1-i", 1:n_t1)), testlet_id = "t1")
  t2 <- testlet(itempool(
    a = rlnorm(n_t2, 0, .3), b = rnorm(n_t2), c = runif(n_t2, 0, .3),
    d = runif(n_t2, .95, 1), item_id = paste0("t2-i", 1:n_t2)), testlet_id = "t2")
  t3 <- testlet(itempool(
    a = rlnorm(n_t3, 0, .3), b = rnorm(n_t3), c = runif(n_t3, 0, .3),
    d = runif(n_t3, .95, 1), item_id = paste0("t3-i", 1:n_t3)), testlet_id = "t3")
  i1 <- itempool(a = rlnorm(4, 0, .3), b = rnorm(4), c = runif(4, 0, .3),
                     d = runif(4, .95, 1), item_id = paste0("i1-", 1:4))
  i2 <- itempool(a = rlnorm(2, 0, .3), b = rnorm(2), c = runif(2, 0, .3),
                     d = runif(2, .95, 1), item_id = paste0("i2-", 1:2))
  ip <- c(t1, i1, t2, i2, t3)
  expect_output(print(ip), "t1-i1")
  expect_output(print(ip), "D = 1")

  # If item pools with testlets has more than 20 items, additional info will
  # be printed
  n_items <- ip$n$items
  i3 <- generate_ip(n = 15, model = "4PL", testlet_id = paste0("i3-", 1:15))
  ip <- c(ip, i3)
  expect_output(print(ip), paste0("with ", n_items + 15 - 10," more items"))
  expect_output(print(ip), paste0("Number of Testlets =  ", ip$n$testlets))
  expect_output(print(ip), paste0("Number of items within Testlets = ",
                                  ip$n$items - (ip$n$elements - ip$n$testlets)))
  expect_output(print(ip), paste0("Number of standalone items = ",
                                  ip$n$elements - ip$n$testlets))
  expect_output(print(ip), paste0("Total number of items = ", ip$n$items))

  # Change the "n = " argument for the print() function
  expect_output(print(ip, n = 13), paste0("with ", n_items + 15 - 13,
                                          " more items"))
  expect_output(print(ip, n = 13), paste0("Number of Testlets =  ",
                                          ip$n$testlets))
  expect_output(print(ip, n = 13),
                paste0("Number of items within Testlets = ",
                       ip$n$items - (ip$n$elements - ip$n$testlets)))
  expect_output(print(ip, n = 13), paste0("Number of standalone items = ",
                                          ip$n$elements - ip$n$testlets))
  expect_output(print(ip, n = 13),
                paste0("Total number of items = ", ip$n$items))

  # -------------------------------------------------------------------------- #
  # A mixture of testlets and items but all of the standalone items and the
  # testlet items has the same model, D's are different
  n_t1 <- sample(3:6, 1)
  n_t2 <- sample(3:6, 1)
  n_t3 <- 1
  weird_D <- 9.81
  t1 <- testlet(itempool(
    a = rlnorm(n_t1, 0, .3), b = rnorm(n_t1), c = runif(n_t1, 0, .3),
    d = runif(n_t1, .95, 1), item_id = paste0("t1-i", 1:n_t1), D = 1),
    testlet_id = "t1")
  t2 <- testlet(itempool(
    a = rlnorm(n_t2, 0, .3), b = rnorm(n_t2), c = runif(n_t2, 0, .3),
    d = runif(n_t2, .95, 1), item_id = paste0("t2-i", 1:n_t2), D = weird_D),
    testlet_id = "t2")
  t3 <- testlet(itempool(
    a = rlnorm(n_t3, 0, .3), b = rnorm(n_t3), c = runif(n_t3, 0, .3),
    d = runif(n_t3, .95, 1), item_id = paste0("t3-i", 1:n_t3)), testlet_id = "t3")
  i1 <- itempool(a = rlnorm(4, 0, .3), b = rnorm(4), c = runif(4, 0, .3),
                     d = runif(4, .95, 1), item_id = paste0("i1-", 1:4), D = 1)
  i2 <- itempool(a = rlnorm(2, 0, .3), b = rnorm(2), c = runif(2, 0, .3),
                     d = runif(2, .95, 1), item_id = paste0("i2-", 1:2))
  ip <- c(t1, c(i1, i2), t2, t3)
  expect_output(print(ip), "t1-i1")
  expect_false(any(grepl(pattern = "D = ", capture.output(ip))))
  expect_output(print(ip, n = 15), paste(weird_D))

  # -------------------------------------------------------------------------- #
  # A mixture of Dichotomous items without a testlet.
  i1 <- itempool(a = rlnorm(4, 0, .3), b = rnorm(4), c = runif(4, 0, .3),
                     d = runif(4, .95, 1), item_id = paste0("i1-", 1:4))
  i3 <- itempool(a = rlnorm(2, 0, .3), b = rnorm(2), c = runif(2, 0, .3),
                     item_id = paste0("i3-", 1:2))
  i4 <- itempool(a = rlnorm(2, 0, .3), b = rnorm(2),
                     item_id = paste0("i4-", 1:2))
  i5 <- itempool(b = rnorm(3), item_id = paste0("i5-", 1:3))

  ip <- c(i1, i3, i4, i5)
  expect_output(print(ip), "i1-1")

  # -------------------------------------------------------------------------- #
  # A mixture of Dichotmous and Polytomous items without a testlet.
  i1 <- itempool(a = rlnorm(4, 0, .3), b = rnorm(4), c = runif(4, 0, .3),
                     d = runif(4, .95, 1), item_id = paste0("i1-", 1:4))
  i2 <- itempool(list(
    new("GRM", item_id = "GRM-1", content = "Stress", a = 0.888,
        b = c(-0.837, -0.529, -0.382, 1.649), D = 1.702),
    new("GRM", item_id = "GRM-2", content = "Depression",
        a = 1.081, b = c(-0.692, -0.157,  0.567), D = 1.702)))
  i3 <- itempool(a = rlnorm(2, 0, .3), b = rnorm(2), c = runif(2, 0, .3),
                     item_id = paste0("i3-", 1:2))
  i4 <- itempool(a = rlnorm(2, 0, .3), b = rnorm(2),
                     item_id = paste0("i4-", 1:2))
  i5 <- itempool(b = rnorm(3), item_id = paste0("i5-", 1:3))
  i6 <- itempool(list(
    new("GPCM", item_id = "GPCM-1", content = "Stress",
        a = 0.888, b = c(-0.837, -0.529, -0.382, 1.649), D = 1.702),
    new("GPCM", item_id = "GPCM-2", content = "Depression",
        a = 1.081, b = c(-0.692, -0.157,  0.567), D = 1.702)))

  ip <- c(i1, i2, i3, i4, i5, i6)
  expect_output(print(ip), "i1-1")
  expect_output(print(ip), "GRM")

  # -------------------------------------------------------------------------- #
  # A mixture of items and testlets.
  t1 <- testlet(itempool(b = rnorm(5), item_id = paste0("t1-i", 1:5)),
                testlet_id = "t1")
  t2 <- testlet(itempool(a = 1:4, b = rnorm(4),
                         item_id = paste0("t2-i", 1:4)), testlet_id = "t2")
  i1 <- itempool(a = rlnorm(4, 0, .3), b = rnorm(4), c = runif(4, 0, .3),
                     d = runif(4, .95, 1))
  i2 <- itempool(list(
    new("GRM", item_id = "GRM-1", content = "Stress",
        a = 0.888, b = c(-0.837, -0.529, -0.382, 1.649), D = 1.702),
    new("GRM", item_id = "GRM-2", content = "Depression",
        a = 1.081, b = c(-0.692, -0.157,  0.567), D = 1.702)))

  ip <- c(t1, i1, t2, i2)

  expect_output(print(ip), "item_id testlet_id model")
  expect_output(print(ip), "t2-i1")
  expect_output(print(ip), "2PL")
  expect_output(print(ip), "content")

  # -------------------------------------------------------------------------- #
  # Mixture of testlet and items with 'misc' field. If 'misc' is common it
  # should be shown when printing.
  t1 <- testlet(itempool(b = -3:-2, item_id = c("t1-i1", "t1-i2"), D = 1.702),
                testlet_id = "t1", misc = list(sympson_hetter_k = 0))
  t2 <- testlet(itempool(a = c(0.2, 0.2), b = 4:5, item_id = c("t2-i1", "t2-i2"),
                         D = 1.702), testlet_id = "t2",
                misc = list(sympson_hetter_k = 1))
  i1 <- item(b = -1, D = 1.702, item_id = "i1", misc = list(sympson_hetter_k = .2))
  i2 <- item(b = 0, D = 1.702, item_id = "i2", misc = list(sympson_hetter_k = .8))
  i3 <- item(b = 1, D = 1.702, item_id = "i3", misc = list(sympson_hetter_k = .9))
  ip <- c(t1, t2, i1, i2, i3)
  expect_output(print(ip), "b sympson_hetter_k")

  # -------------------------------------------------------------------------- #
  # 2020-04-18: When printing an item from a testlet, instead of printing the
  # item parameters, a question mark is printed like: "testlet1 3        ?".
  i1 <- item(b = 1, item_id = "i1", content = "A")
  i2 <- item(a = 1, b = 1, item_id = "i2", content = "B")
  i3 <- item(a = 1, b = 1, c = 1, item_id = "i3", content = "C")
  i4 <- item(a = 1, b = c(1, 2), item_id = "i4", content = "D", model = "GRM")
  i5 <- item(a = 1, b = c(1, 2), item_id = "i5", content = "E", model = "GPCM")
  t1 <- testlet(i2, i5, item_id = "testlet1", content = "TZM")
  ip <- c(i1, i3, t1, i4)
  expect_false(any(grepl(pattern = "\\?", capture.output(ip))))
  # Also, when printing an element, there is an error.
  expect_output(print(ip[4]))

  # -------------------------------------------------------------------------- #
  # 2020-08-10: If 'misc' or 'content' field is common among items it should
  # be printed in the header:
  ip <- generate_ip(n = 6, content = rep("Algebra", 6),
                    misc = lapply(1:6, function(x)
                      list(Form = "A1", sh = round(runif(1), 1),
                           test = 1)))
  ip[[6]]$misc$test <- NA
  expect_output(print(ip), "Content = Algebra")
  expect_output(print(ip), "Form = A1")
  expect_output(print(ip), "sh[ ]+test")

  # -------------------------------------------------------------------------- #
  # Check how an itempool with one testlet printed
  # Create an item pool with only one testlet
  i2 <- item(a = 1, b = 1.2345, item_id = "biii2", content = "B")
  i5 <- item(a = 1, b = c(1, 2), item_id = "biii5", content = "E", model = "GPCM")
  t1 <- testlet(i2, i5, testlet_id = "testlet1", content = "TZM")
  ip <- itempool(t1)
  expect_output(print(ip, base_print = TRUE), "1.2345")
  expect_output(print(ip), "GPCM")
  expect_output(print(ip), "content")
  expect_output(print(ip), "1.23")
  expect_output(print(ip), "testlet1")
  expect_output(print(ip), regexp = "biii2")

  # -------------------------------------------------------------------------- #
  # The item parameters will always be shown regardless of whether they are all
  # the same
  # 3PL model
  ip <- generate_ip(model = "3PL")
  ip$a <- 1.245
  ip$b <- 0.887
  ip$c <- 0.362
  expect_false(any(grepl(pattern = "a = ", capture.output(ip))))
  expect_false(any(grepl(pattern = "b = ", capture.output(ip))))
  expect_false(any(grepl(pattern = "c = ", capture.output(ip))))
  expect_identical(sum(grepl(pattern = paste0(ip$a[1]),
                             capture.output(print(ip, base_print = TRUE)))),
                   ip$n$items)
  expect_identical(sum(grepl(pattern = paste0(ip$b[1]),
                             capture.output(print(ip, base_print = TRUE)))),
                   ip$n$items)
  expect_identical(sum(grepl(pattern = paste0(ip$c[1]),
                             capture.output(print(ip, base_print = TRUE)))),
                   ip$n$items)
  # Since printed via pillar
  expect_identical(sum(grepl(pattern = paste0(round(ip$a[1], 2)), capture.output(ip))),
                   ip$n$items)
  expect_identical(sum(grepl(pattern = paste0(ip$b[1]), capture.output(ip))),
                   ip$n$items)
  expect_identical(sum(grepl(pattern = paste0(ip$c[1]), capture.output(ip))),
                   ip$n$items)

  # GPCM
  ip2 <- generate_ip(model = "GRM", n_categories = sample(3:5, 6, T),
                     item_id = paste0("p_item_", 1:6))
  ip2$a <- 1.245

  expect_false(any(grepl(pattern = "a = ", capture.output(ip2))))
  expect_identical(sum(grepl(pattern = paste0(round(ip2$a[1], 2)),
                             capture.output(ip2))), ip2$n$items)
  expect_identical(sum(grepl(pattern = paste0(ip2$a[1]),
                             capture.output(print(ip2, base_print = TRUE)))),
                   ip2$n$items)

  # Mixture of item parameters
  ip3 <- c(ip, ip2)

})


###############################################################################@
############################# convert_model ####################################
###############################################################################@

test_that("Test convert_model", {
  # Test for item
  item <- item(a = 1, b = 1.2, c = 0.2, D = rnorm(1, 2, .2))
  expect_null(item$d)
  item_new <- convert_model(item, target_model = "4PL")
  expect_identical(item_new$d, 1)
  expect_identical(item_new$a, item$a)
  expect_identical(item_new$b, item$b)
  expect_identical(item_new$c, item$c)
  expect_identical(item_new$D, item$D)
  # Convert to Rasch
  item_new <- convert_model(item, "Rasch")
  expect_null(item_new$c)
  expect_null(item_new$a)
  expect_null(item_new$D)
  expect_identical(item_new$b, item$b)

  # -------------------------------------------------------------------------- #
  # Convert Rasch to others
  item <- item(b = 1.2, model = "Rasch")
  expect_null(item$d)

  # -------------------------------------------------------------------------- #
  # One cannot convert GRM to unidimensional IRT models.
  item <- item(a = 1, b = c(-1, 1.2), D = rnorm(1, 2, .2))
  expect_error(convert_model(ip = item, target_model = "2PL"))

  # -------------------------------------------------------------------------- #
  # Convert GPCM to GPCM2
  item <- generate_item(model = "GPCM2")
  item_new <- convert_model(ip = item, target_model = "GPCM")
  expect_identical(item_new$b, item$b - item$d)
  expect_identical(item_new$model, "GPCM")
  theta <- rnorm(1)
  expect_identical(prob(ip = item, theta = theta),
                   prob(ip = item_new, theta = theta))


  # -------------------------------------------------------------------------- #
  # Test Errors
  item <- item(a = 1, b = 2, c = .2)
  expect_error(convert_model(12, target_model = "3PL"),
               regexp = "Invalid object type.")

  expect_error(convert_model(item, target_model = "XYZ"),
               regexp = "Invalid target_model.")

  # -------------------------------------------------------------------------- #
  # Reduce item parameters
  old_item <- item(a = 1.2, b = .14, c = .3, item_id = "abdy3c", content = "d03kj")
  item <- convert_model(old_item, target_model = "2PL")
  expect_s4_class(item, "2PL")
  expect_identical(item@a, old_item@a)
  expect_identical(item@b, old_item@b)
  expect_identical(item@item_id, old_item@item_id)
  expect_identical(item@content, old_item@content)

  item <- convert_model(old_item, target_model = "1PL")
  expect_s4_class(item, "1PL")
  expect_identical(item@b, old_item@b)
  expect_identical(item@item_id, old_item@item_id)
  expect_identical(item@content, old_item@content)

  # -------------------------------------------------------------------------- #
  # Increase item parameters
  old_item <- generate_item(model = "2PL", item_id = "abdy3c", content = "d03kj")
  item <- convert_model(old_item, target_model = "4PL")
  expect_s4_class(item, "4PL")
  expect_identical(item@a, old_item@a)
  expect_identical(item@b, old_item@b)
  expect_identical(item@c, 0)
  expect_identical(item@d, 1)
  expect_identical(item@item_id, old_item@item_id)
  expect_identical(item@content, old_item@content)

  # -------------------------------------------------------------------------- #
  # Test Item Pool
  n <- 10 # number of items
  ip <- itempool(data.frame(a = runif(n, .5, 1.5), b = rnorm(n),
                            c = runif(n, 0,.3)), item_id = paste0("Item_",1:n),
                 content = sample(c("Algebra", "Arithmetic", "Geometry"),
                                  n, replace = TRUE))
  new_ip <- convert_model(ip, "2PL")
  expect_identical(ip$a, new_ip$a)
  expect_identical(ip$b, new_ip$b)
  expect_identical(ip$item_id, new_ip$item_id)
  expect_identical(ip$content, new_ip$content)

  # -------------------------------------------------------------------------- #
  # Test Testlet
  t1 <- testlet(itempool(a = c(.5, 1.5, 2), b = rnorm(3), c = c(.1, .2, .3)))
  expect_identical(t1[[1]]$model, "3PL")
  expect_identical(t1[[1]]$c, .1)
  expect_null(t1[[1]]$d)
  t1_new <- convert_model(t1, "2PL")
  expect_identical(t1_new[[1]]$model, "2PL")
  expect_null(t1_new[[1]]$c)
  t1_new <- convert_model(t1, "4PL")
  expect_identical(t1_new[[1]]$model, "4PL")
  expect_identical(t1_new[[1]]$d, 1)
})


