# library(testthat)


###############################################################################@
############################# plot_info  #######################################
###############################################################################@
test_that("Test plot_info", {
  # Single item
  ip <- item(a = 1, b = 0.81, D = 1)
  expect_silent(p <- plot_info(ip, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  # Base R
  expect_silent(p <- plot_info(ip, base_r_graph = TRUE, suppress_plot = TRUE))
  expect_silent(p <- plot_info(ip, base_r_graph = TRUE, focus_item = "Item_1",
                               suppress_plot = TRUE))

  # Test theta_range
  expect_silent(p <- plot_info(ip, suppress_plot = TRUE, theta_range = -3:3))
  expect_silent(p <- plot_info(ip, base_r_graph = TRUE, suppress_plot = TRUE,
                               theta_range = -3:3))

  # -------------------------------------------------------------------------- #
  # 3PL
  n <- sample(10:20,1)
  ip <- generate_ip(n = n)
  expect_silent(p <- plot_info(ip, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  # Base R
  expect_silent(p <- plot_info(ip, base_r_graph = TRUE, suppress_plot = TRUE))

  ### tif  = TRUE
  expect_silent(p <- plot_info(ip, tif = TRUE, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  # Base R
  expect_silent(p <- plot_info(ip, tif = TRUE, base_r_graph = TRUE,
                               suppress_plot = TRUE))

  ### Focus item
  expect_silent(p <- plot_info(ip, focus_item = "Item_3", suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  # Base R
  expect_silent(p <- plot_info(ip, base_r_graph = TRUE, focus_item = "Item_3",
                               suppress_plot = TRUE))


  # theta_range = c(-5, 5); tif = FALSE; focus_item = "Item_2"; title = ""
  # get_data_plot_info <- irt:::get_data_plot_info
  # flatten_itempool_cpp <- irt:::flatten_itempool_cpp

  # -------------------------------------------------------------------------- #
  # Mixed format items
  item1 <- item(a = runif(1, .5, 2), b = sort(runif(3, -2, 2)), model = 'GRM')
  item2 <- item(a = runif(1, .5, 2), b = sort(runif(2, -2, 2)), model = 'GRM')
  item3 <- item(a = runif(1, .5, 2), b = runif(1, -2, 2))
  ip <- c(item1, item2, item3)
  expect_silent(p <- plot_info(ip, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  expect_silent(p <- plot_info(ip, tif = TRUE, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  # Base R
  expect_silent(p <- plot_info(ip, base_r_graph = TRUE, focus_item = "Item_3",
                               suppress_plot = TRUE))

  # -------------------------------------------------------------------------- #
  # when tif =  TRUE, focus_item cannot be set.
  ip <- generate_ip()
  expect_message(plot_info(ip = ip, tif = TRUE, focus_item = "Item_1",
                           suppress_plot = TRUE),
                 regexp =  "When 'tif = TRUE', focus_item cannot be plotted.")

  # -------------------------------------------------------------------------- #
  # focus_item with an item pool of size 1
  ip <- generate_ip(n = 1)
  expect_silent(p <- plot_info(ip = ip, focus_item = "Item_1",
                               suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  # -------------------------------------------------------------------------- #
  # Multiple focus_item
  ip <- generate_ip(n = 10)
  expect_silent(p <- plot_info(ip, focus_item = c(2, 8), suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  expect_silent(p <- plot_info(ip, focus_item = c("Item_5", "Item_6"),
                               suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  # -------------------------------------------------------------------------- #
  # Additional arguments
  expect_silent(p <- plot_info(ip, focus_item = 7, alpha = .7, color = "gray",
                               suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  expect_silent(p <- plot_info(ip, focus_item = "Item_3", color = "green",
                               base_r_graph = TRUE))

  # -------------------------------------------------------------------------- #
  # 'title' can be NULL to suppress the graph title
  expect_silent(p <- plot_info(ip, focus_item = "Item_3", suppress_plot = TRUE,
                               title = NULL))
  expect_silent(p <- plot_info(ip, base_r_graph = TRUE, focus_item = "Item_3",
                               suppress_plot = TRUE, title = NULL))


  # -------------------------------------------------------------------------- #
  # Info with testlet
  ip <- c(testlet(itempool(b = c(-1, 1), item_id = c("t1-i1", "t1-i2"),
                           D = 1.702), testlet_id = "t1"),
          testlet(itempool(b = c(-2, 0, 2),
                           item_id = c("t2-i1", "t2-i2", "t2-i3"),
                           D = 1.702), testlet_id = "t2"),
          item(b = -1.5, item_id = "i1", D = 1.702),
          item(b = 0.25, item_id = "i2", D = 1.702),
          item(b = 1.5, item_id = "i3", D = 1.702))

  expect_silent(p <- plot_info(ip, suppress_plot = TRUE))
  expect_silent(p <- plot_info(ip, separate_testlet = FALSE, suppress_plot = TRUE))

})

