# library(testthat)

###############################################################################@
############################# plot.Itempool - icc ##############################
###############################################################################@
test_that("plot.Itempool - icc", {

  # 3PL
  n <- sample(10:50,1)
  ip <- itempool(a = runif(n, .5, 2), b = rnorm(n), c = runif(n, 0, .3), D = 1)
  expect_silent(p <- plot(ip, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  # Focus item
  expect_silent(p <- plot(ip, type = "icc", focus_item = "Item_2",
                          suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  # Base R
  expect_silent(p <- plot(ip, type = "icc", base_r_graph = TRUE,
                          suppress_plot = TRUE))
  # Base R - Focus Item
  expect_silent(p <- plot(ip, type = "icc", focus_item = "Item_2",
                          base_r_graph = TRUE, suppress_plot = TRUE))

  # A vector for theta range
  expect_silent(p <- plot(ip, type = "icc", base_r_graph = FALSE,
                          theta_range = -3:3, suppress_plot = TRUE))
  expect_silent(p <- plot(ip, type = "icc", base_r_graph = TRUE,
                          theta_range = -3:3, suppress_plot = TRUE))

  # -------------------------------------------------------------------------- #
  # GRM
  n <- sample(10:50,1)
  ip <- generate_ip(model = "GRM", n_categories = sample(3:6, n, TRUE))
  expect_message(p <- plot(ip, type = "icc", suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  # Focus Item
  expect_message(p <- plot(ip, type = "icc", focus_item = "Item_2",
                           suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  # Base R
  expect_message(p <- plot(ip, type = "icc", base_r_graph = TRUE,
                           suppress_plot = TRUE))
  # Base R - Focus Item
  expect_message(p <- plot(ip, type = "icc", base_r_graph = TRUE,
                           focus_item = "Item_2", suppress_plot = TRUE))

  # -------------------------------------------------------------------------- #
  # Mixed GRM and 3PL
  n <- sample(10:30, 1)
  ip <- generate_ip(model = sample(c("GRM", "3PL", "GRM", "3PL"), n, TRUE))

  expect_message(p <- plot(ip, type = "icc", suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  # Focus Item
  expect_message(p <- plot(ip, type = "icc", focus_item = "Item_2",
                           suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  # Base R
  expect_message(p <- plot(ip, type = "icc", base_r_graph = TRUE,
                           suppress_plot = TRUE))
  # Base R - Focus Item
  expect_message(p <- plot(ip, type = "icc", base_r_graph = TRUE,
                           focus_item = "Item_2", suppress_plot = TRUE))

  # -------------------------------------------------------------------------- #
  # Itempools with testlets
  ip <- c(generate_testlet(n = 2, testlet_id = "t1"),
        generate_testlet(n = 3, testlet_id = "t2"),
        generate_testlet(n = 1, testlet_id = "t3"))
  expect_silent(p <- plot(ip, type = "icc", suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  # Base R
  expect_silent(p <- plot(ip, type = "icc", base_r_graph = TRUE,
                          suppress_plot = TRUE))

  # -------------------------------------------------------------------------- #
  # title can be NULL to suppress graph title
  n <- sample(10:50,1)
  ip <- itempool(a = runif(n, .5, 2), b = rnorm(n), c = runif(n, 0, .3), D = 1)
  expect_silent(p <- plot(ip, type = "icc", suppress_plot = TRUE, title = NULL))
  expect_silent(p <- plot(ip, type = "icc", suppress_plot = TRUE, title = NULL,
                          base_r_graph = TRUE))

  # x = ip; theta_range = c(-4,4); title = "";
  # suppress_plot = FALSE; legend_title = NULL
  # focus_item = NULL; args <- list()
  # PMODELS <- irt:::PMODELS
  # get_data_plot_itempool_icc <- irt:::get_data_plot_itempool_icc
  # UNIDIM_DICHO_MODELS <- irt:::UNIDIM_DICHO_MODELS
  # UNIDIM_POLY_MODELS <- irt:::UNIDIM_POLY_MODELS

})


###############################################################################@
############################# plot.Itempool - tcc ##############################
###############################################################################@

test_that("plot.Itempool - tcc", {

  # 3PL
  n <- sample(10:50,1)
  ip <- itempool(a = runif(n, .5, 2), b = rnorm(n), c = runif(n, 0, .3), D = 1)

  expect_silent(p <- plot(ip, type = "tcc", suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  # Base R
  expect_silent(p <- plot(ip, type = "tcc", base_r_graph = TRUE,
                          suppress_plot = TRUE))
  expect_silent(p <- plot(ip, type = "tcc", base_r_graph = TRUE))

  # -------------------------------------------------------------------------- #
  # GRM
  n <- sample(10:50,1)
  ip <- generate_ip(model = "GRM", n_categories = sample(3:6, n, TRUE))
  expect_silent(p <- plot(ip, type = "tcc", suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  expect_silent(p <- plot(ip, type = "tcc", suppress_plot = TRUE,
                          tcc_prop_corr = TRUE))
  expect_s3_class(p, 'ggplot')

  # Base R
  expect_silent(p <- plot(ip, type = "tcc", base_r_graph = TRUE,
                          suppress_plot = TRUE))


  # -------------------------------------------------------------------------- #
  # Mixed GRM and 3PL
  n <- sample(10:30, 1)
  ip <- generate_ip(model = sample(c("GRM", "3PL", "GRM", "3PL"), n, TRUE))
  expect_silent(p <- plot(ip, type = "tcc", suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  expect_silent(p <- plot(ip, type = "tcc", suppress_plot = TRUE,
                          tcc_prop_corr = TRUE))
  expect_s3_class(p, 'ggplot')
  # Base R
  expect_silent(p <- plot(ip, type = "tcc", focus_item = "Item_2",
                          base_r_graph = TRUE, suppress_plot = TRUE))

  # -------------------------------------------------------------------------- #
  # Itempools with testlets
  ip <- c(generate_testlet(n = 2, testlet_id = "t1"),
        generate_testlet(n = 3, testlet_id = "t2"),
        generate_testlet(n = 1, testlet_id = "t3"))
  expect_silent(p <- plot(ip, type = "tcc", suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  # Base R
  expect_silent(p <- plot(ip, type = "tcc", base_r_graph = TRUE,
                          suppress_plot = TRUE))

  # -------------------------------------------------------------------------- #
  # title can be NULL to suppress graph title
  n <- sample(10:50,1)
  ip <- itempool(a = runif(n, .5, 2), b = rnorm(n), c = runif(n, 0, .3), D = 1)
  expect_silent(p <- plot(ip, type = "tcc", suppress_plot = TRUE, title = NULL))
  expect_silent(p <- plot(ip, type = "tcc", suppress_plot = TRUE, title = NULL,
                          base_r_graph = TRUE))


  # x = ip; theta_range = c(-4,4); title = ""; suppress_plot = FALSE;
  # focus_item = NULL; args <- list()
  # PMODELS <- irt:::PMODELS
  # get_data_plot_itempool_tcc <- irt:::get_data_plot_itempool_tcc
  # UNIDIM_DICHO_MODELS <- irt:::UNIDIM_DICHO_MODELS
  # UNIDIM_POLY_MODELS <- irt:::UNIDIM_POLY_MODELS
})


###############################################################################@
############################# plot.Itempool - pars #############################
###############################################################################@

test_that("plot.Itempool - pars", {

  # 3PL
  n <- sample(10:50, 1)
  ip <- generate_ip(model = "3PL", n = n)
  expect_silent(p <- plot(ip, type = "pars", suppress_plot = TRUE))
  expect_silent(p <- plot(ip, type = "pars", focus_item = "Item_2",
                          suppress_plot = TRUE))
  # Base R
  expect_silent(p <- plot(ip, type = "pars", focus_item = "Item_2",
                          base_r_graph = TRUE, suppress_plot = TRUE))

  # x = ip; theta_range = c(-4,4); title = ""; suppress_plot = FALSE;
  # focus_item = NULL; args <- list()

  # -------------------------------------------------------------------------- #
  # GRM
  n <- sample(10:50,1)
  ip <- generate_ip(model = "GRM", n_categories = sample(3:6, n, TRUE))
  expect_silent(p <- plot(ip, type = "pars", suppress_plot = TRUE))
  expect_silent(p <- plot(ip, type = "pars", focus_item = "Item_1",
                          suppress_plot = TRUE))
  # Base R
  expect_silent(p <- plot(ip, type = "pars", focus_item = "Item_2",
                          base_r_graph = TRUE, suppress_plot = TRUE))


  # -------------------------------------------------------------------------- #
  # Mixed GRM and 3PL
  n <- sample(300:900, 1)
  ip <- generate_ip(model = sample(c("GRM", "3PL", "GRM", "3PL"), n, TRUE))
  expect_silent(p <- plot(ip, type = "pars", suppress_plot = TRUE))
  expect_silent(p <- plot(ip, type = "pars", focus_item = "Item_1",
                          suppress_plot = TRUE))
  expect_silent(p <- plot(ip, type = "pars", focus_item = "Item_2",
                          dotsize = .4, suppress_plot = TRUE))
  # Base R
  expect_silent(p <- plot(ip, type = "pars", focus_item = "Item_2",
                          base_r_graph = TRUE, suppress_plot = TRUE))

  # -------------------------------------------------------------------------- #
  # Itempools with testlets
  ip <- c(generate_testlet(n = 2, testlet_id = "t1"),
        generate_testlet(n = 3, testlet_id = "t2"),
        generate_testlet(n = 1, testlet_id = "t3"))
  expect_silent(p <- plot(ip, type = "pars", suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  # Base R
  expect_silent(p <- plot(ip, type = "pars", base_r_graph = TRUE,
                          suppress_plot = TRUE))

  # -------------------------------------------------------------------------- #
  # title can be NULL to suppress graph title
  n <- sample(10:50,1)
  ip <- itempool(a = runif(n, .5, 2), b = rnorm(n), c = runif(n, 0, .3), D = 1)
  expect_silent(p <- plot(ip, type = "pars", suppress_plot = TRUE,
                          title = NULL))
  expect_silent(p <- plot(ip, type = "pars", suppress_plot = TRUE, title = NULL,
                          base_r_graph = TRUE))

  # x = ip; theta_range = c(-4,4); title = ""; suppress_plot = FALSE;
  # focus_item = NULL; args <- list()
  # PMODELS <- irt:::PMODELS
  # get_data_plot_itempool_pars <- irt:::get_data_plot_itempool_pars

})

###############################################################################@
############################# plot.Itempool - hist #############################
###############################################################################@

test_that("plot.Itempool - hist", {

  # 3PL
  n <- sample(10:50, 1)
  ip <- generate_ip(model = "3PL", n = n)
  expect_silent(p <- plot(ip, type = "hist", suppress_plot = TRUE))
  expect_silent(p <- plot(ip, type = "hist", focus_item = "Item_2",
                          suppress_plot = TRUE))
  # Base R
  expect_silent(p <- plot(ip, type = "hist", focus_item = "Item_2",
                          base_r_graph = TRUE, suppress_plot = TRUE))

  # x = ip; theta_range = c(-4,4); title = ""; suppress_plot = FALSE;
  # focus_item = NULL; args <- list()

  # -------------------------------------------------------------------------- #
  # GRM
  n <- sample(10:50,1)
  ip <- generate_ip(model = "GRM", n_categories = sample(3:6, n, TRUE))
  expect_silent(p <- plot(ip, type = "hist", suppress_plot = TRUE))
  expect_silent(p <- plot(ip, type = "hist", focus_item = "Item_1",
                          suppress_plot = TRUE))
  # Base R
  expect_silent(p <- plot(ip, type = "hist", focus_item = "Item_2",
                          base_r_graph = TRUE, suppress_plot = TRUE))


  # -------------------------------------------------------------------------- #
  # Mixed GRM and 3PL
  n <- sample(300:900, 1)
  ip <- generate_ip(model = sample(c("GRM", "3PL", "GRM", "3PL"), n, TRUE))
  expect_silent(p <- plot(ip, type = "hist", suppress_plot = TRUE))
  expect_silent(p <- plot(ip, type = "hist", focus_item = "Item_1",
                          suppress_plot = TRUE))
  expect_silent(p <- plot(ip, type = "hist", focus_item = "Item_2",
                          dotsize = .4, suppress_plot = TRUE))
  # Base R
  expect_silent(p <- plot(ip, type = "hist", focus_item = "Item_2",
                          base_r_graph = TRUE, suppress_plot = TRUE))

  # -------------------------------------------------------------------------- #
  # Itempools with testlets
  ip <- c(generate_testlet(n = 2, testlet_id = "t1"),
        generate_testlet(n = 3, testlet_id = "t2"),
        generate_testlet(n = 1, testlet_id = "t3"))
  expect_silent(p <- plot(ip, type = "hist", suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  # Base R
  expect_silent(p <- plot(ip, type = "hist", base_r_graph = TRUE,
                          suppress_plot = TRUE))

  # -------------------------------------------------------------------------- #
  # 'title' can be NULL to suppress the graph title
  n <- sample(10:50,1)
  ip <- itempool(a = runif(n, .5, 2), b = rnorm(n), c = runif(n, 0, .3), D = 1)
  expect_silent(p <- plot(ip, type = "hist", suppress_plot = TRUE,
                          title = NULL))
  expect_silent(p <- plot(ip, type = "hist", suppress_plot = TRUE, title = NULL,
                          base_r_graph = TRUE))

  # x = ip; theta_range = c(-4,4); title = ""; suppress_plot = FALSE;
  # focus_item = NULL; args <- list()
  # PMODELS <- irt:::PMODELS
  # get_data_plot_itempool_hist <- irt:::get_data_plot_itempool_hist



})
