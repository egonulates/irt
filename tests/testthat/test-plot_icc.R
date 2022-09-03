
###############################################################################@
############################# plot_empirical_icc  ##############################
###############################################################################@

test_that("plot_empirical_icc", {
  ip <- generate_ip(model = c("3PL", "GRM"), n = 20)
  true_theta <- rnorm(2000)
  resp <- sim_resp(ip = ip, theta = true_theta)

  expect_silent(p <- plot_empirical_icc(resp, "Item_3", ip = ip,
                                        theta = true_theta,
                                        suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)


  expect_silent(p <- plot_empirical_icc(resp, 3, ip = ip, theta = true_theta,
                                        suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # Base R graphics
  expect_silent(p <- plot_empirical_icc(resp, 3, ip = ip, theta = true_theta,
                                        base_r_graph = TRUE,
                                        suppress_plot = TRUE))

  # Change the number of bins
  expect_silent(p <- plot_empirical_icc(resp, 3, ip = ip, theta = true_theta,
                                        bins = 5, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  expect_silent(p <- plot_empirical_icc(resp, 3, ip = ip, theta = true_theta,
                                        bins = 15, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # Fixed bin width
  expect_silent(p <- plot_empirical_icc(resp, 3, ip = ip, theta = true_theta,
                                        binwidth = .2, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  expect_silent(p <- plot_empirical_icc(resp, 3, ip = ip, theta = true_theta,
                                        binwidth = .5, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # Plot GRM item's ICC
  expect_silent(p <- plot_empirical_icc(resp, "Item_4", ip = ip,
                                        theta = true_theta,
                                        suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  expect_silent(p <- plot_empirical_icc(resp, "Item_4", ip = ip,
                                        theta = true_theta, binwidth = .2,
                                        suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # -------------------------------------------------------------------------- #
  # 'title' can be NULL to suppress the graph title
  expect_silent(p <- plot_empirical_icc(resp, "Item_3", ip = ip,
                                        theta = true_theta, title = NULL,
                                        suppress_plot = TRUE))
  expect_silent(p <- plot_empirical_icc(resp, "Item_3", ip = ip,
                                        theta = true_theta, title = NULL,
                                        suppress_plot = TRUE,
                                        base_r_graph = TRUE))

  # -------------------------------------------------------------------------- #
  # Response_set as 'resp' argument
  ip <- generate_ip(model = c("3PL", "GRM"), n = 20)
  true_theta <- rnorm(2000)
  resp_set <- generate_resp_set(ip = ip, theta = true_theta, prop_missing = .2)

  expect_silent(p <- plot_empirical_icc(resp_set, "Item_3", ip = ip,
                                        theta = true_theta,
                                        suppress_plot = TRUE))

  # -------------------------------------------------------------------------- #
  # Item pool with testlets
  ip <- c(generate_testlet(n = 4, item_id_preamble = "t1"),
          generate_testlet(n = 2, item_id_preamble = "t2"),
          generate_testlet(n = 3, item_id_preamble = "t3"),
          generate_ip(n = 5))
  true_theta <- rnorm(2000)
  resp_set <- generate_resp_set(ip = ip, theta = true_theta, prop_missing = .2)

  expect_silent(p <- plot_empirical_icc(resp = resp_set, item = "Item_3",
                                        ip = ip, bins = 10, theta = true_theta,
                                        suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)
  expect_silent(p <- plot_empirical_icc(resp = resp_set, item = "t2Item_1",
                                        ip = ip, bins = 10, theta = true_theta,
                                        suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)
  expect_silent(p <- plot_empirical_icc(resp = resp_set, item = 5, ip = ip,
                                        bins = 10, theta = true_theta,
                                        suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)
  expect_silent(p <- plot_empirical_icc(resp = resp_set, item = 12, ip = ip,
                                        bins = 10, theta = true_theta,
                                        suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)


  resp <- as.matrix(resp_set)
  expect_silent(p <- plot_empirical_icc(resp = resp, item = "Item_3",
                                        ip = ip, bins = 10, theta = true_theta,
                                        suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)
  expect_silent(p <- plot_empirical_icc(resp = resp, item = "t2Item_1",
                                        ip = ip, bins = 10, theta = true_theta,
                                        suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)
  expect_silent(p <- plot_empirical_icc(resp = resp, item = 5, ip = ip,
                                        bins = 10, theta = true_theta,
                                        suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)
  expect_silent(p <- plot_empirical_icc(resp = resp, item = 12, ip = ip,
                                        bins = 10, theta = true_theta,
                                        suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)


  # get_data_plot_empirical_icc <- irt:::get_data_plot_empirical_icc
  # UNIDIM_DICHO_MODELS <- irt:::UNIDIM_DICHO_MODELS
  # UNIDIM_POLY_MODELS <- irt:::UNIDIM_POLY_MODELS
  # convert_to_resp_set <- irt:::convert_to_resp_set
  # bins = 10; binwidth = NULL; title = ""
  # resp <- resp_set;
  # item = "Item_1";
  # theta = true_theta;


})


test_that("plot_empirical_icc - Secondary test", {
  ip <- generate_ip(model = "3PL", n = 20)
  theta <- rnorm(100)
  resp <- sim_resp(ip = ip, theta = theta, prop_missing = .1)
  # item = 1
  # bins = 10
  # type = "eicc"
  # theta = NULL
  # title = "my title"
  # suppress_plot = TRUE
  # x_axis_scale = "theta"

  # -------------------------------------------------------------------------- #
  # The default
  expect_silent(p <- plot_empirical_icc(resp = resp, ip = ip, item = 1,
                                        suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # -------------------------------------------------------------------------- #
  # Change bin number
  expect_silent(p <- plot_empirical_icc(resp = resp, ip = ip, item = 1,
                                        bins = 3, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # # -------------------------------------------------------------------------- #
  # # Change x axis scale
  # plot_empirical_icc(
  #   resp = resp, ip = ip, item = 1, x_axis_scale = "number",
  #   suppress_plot = TRUE)
  # expect_silent(p <- plot_empirical_icc(
  #   resp = resp, ip = ip, item = 1, x_axis_scale = "number",
  #   suppress_plot = TRUE))
  # expect_s3_class(p, 'ggplot'); rm(p)
  # expect_silent(p <- plot_empirical_icc(
  #   resp = resp, item = 1, x_axis_scale = "theta", ip = ip,
  #   suppress_plot = TRUE))
  # expect_s3_class(p, 'ggplot'); rm(p)
  # expect_silent(p <- plot_empirical_icc(
  #   resp = resp, item = 1, x_axis_scale = "percent", suppress_plot = TRUE))
  # expect_s3_class(p, 'ggplot'); rm(p)

  # -------------------------------------------------------------------------- #
  # item can be item ID
  expect_silent(p <- plot_empirical_icc(resp = resp, ip = ip, item = "Item_3",
                                       suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # # -------------------------------------------------------------------------- #
  # # type = "oep"
  # expect_silent(p <- plot_empirical_icc(
  #   resp = resp, item = 2, type = "oep", ip = ip, x_axis_scale = "theta",
  #   suppress_plot = TRUE))
  # expect_s3_class(p, 'ggplot'); rm(p)
  #
  # # -------------------------------------------------------------------------- #
  # # When type = "oep", only x_axis_scale = "theta" is allowed.
  # expect_warning(p <- plot_empirical_icc(
  #   resp = resp, item = 2, type = "oep", ip = ip, x_axis_scale = "percent",
  #   suppress_plot = TRUE))
  # expect_s3_class(p, 'ggplot'); rm(p)

  # # -------------------------------------------------------------------------- #
  # # Expect error when graph type requires Itempool
  # expect_error(p <- plot_empirical_icc(resp = resp, item = "Item_3",
  #                                     type = "oep", suppress_plot = TRUE))
  # expect_error(p <- plot_empirical_icc(resp = resp, item = "Item_3",
  #                                     type = "oept", suppress_plot = TRUE))
  # # Length of theta and number of rows of resp should be equal
  # expect_error(p<- plot_empirical_icc(
  #   resp = resp, item = 1, ip = ip, type = "oept", theta = 1:2,
  #   suppress_plot = TRUE))
  # # When x_axis_scale is "theta", either theta or an item pool should be
  # # provided.
  # expect_error(p<- plot_empirical_icc(
  #   resp = resp, item = 1, x_axis_scale = "theta", suppress_plot = TRUE))

  # expect_silent(p <- plot_empirical_icc(resp = resp, item = "Item_3", ip = ip,
  #                                     type = "oept", suppress_plot = TRUE))
  # expect_s3_class(p, 'ggplot')

  # # -------------------------------------------------------------------------- #
  # # Check multiline x-axis labels
  # expect_silent(p <- plot_empirical_icc(
  #   resp = resp, item = 1, x_axis_scale = "percent", n_dodge = 2,
  #   suppress_plot = TRUE))
  # expect_s3_class(p, 'ggplot'); rm(p)
  #
  # # -------------------------------------------------------------------------- #
  # # Polytomous items
  # ip <- generate_ip(model = "GRM", n = 20)
  # theta <- rnorm(1000)
  # resp <- sim_resp(ip = ip, theta = theta, prop_missing = .1)
  #
  # expect_silent(p <- plot_empirical_icc(resp = resp, item = 1,
  #                                      suppress_plot = TRUE))
  # expect_s3_class(p, 'ggplot'); rm(p)
  #
  # # type = "oep"
  # expect_silent(p <- plot_empirical_icc(
  #   resp = resp, item = 2, type = "oep", ip = ip, x_axis_scale = "theta",
  #   suppress_plot = TRUE))
  # expect_s3_class(p, 'ggplot'); rm(p)

})

###############################################################################@
############################# plot_empirical_icc2  ##############################
###############################################################################@

test_that("plot_empirical_icc2", {
  ip <- generate_ip(model = c("3PL", "GRM"), n = 20)
  true_theta <- rnorm(2000)
  resp <- sim_resp(ip = ip, theta = true_theta)

  # item <- 3
  # bins = 10
  # binwidth = NULL
  # binwidth = .25
  # ip = NULL
  # x_axis_scale = NULL
  # theta = NULL

  # Provide item ID
  expect_silent(p <- plot_empirical_icc2(resp = resp, item = "Item_5",
                                         suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # Provide item number
  expect_silent(p <- plot_empirical_icc2(resp, item = 3, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # Change x-axis scale
  expect_silent(p <- plot_empirical_icc2(resp, item = 3,
                                         x_axis_scale = "number",
                                         suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # Change number of bins and x-axis scale
  expect_silent(p <- plot_empirical_icc2(resp, item = 3, bins = 11,
                                         x_axis_scale = "theta",
                                         suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # Use bin width
  expect_silent(p <- plot_empirical_icc2(resp, item = 3, binwidth = 2,
                                         suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # Use theta scores instead of raw scores
  expect_silent(p <- plot_empirical_icc2(resp, item = 3, binwidth = .2,
                                         ip = ip, theta = true_theta,
                                         suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # A GRM item
  expect_silent(p <- plot_empirical_icc2(resp, item = 4, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  expect_silent(p <- plot_empirical_icc2(resp, item = 4,
                                         x_axis_scale = "percent",
                                         suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  expect_silent(p <- plot_empirical_icc2(resp, item = 4,
                                         x_axis_scale = "number",
                                         suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  expect_silent(p <- plot_empirical_icc2(resp, item = 4, binwidth = 4,
                                         suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # Use raw score and custom binwidth
  expect_silent(p <- plot_empirical_icc2(resp, item = 4,
                                         x_axis_scale = "percent",
                                         binwidth = 4, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # Use theta score
  expect_silent(p <- plot_empirical_icc2(resp, item = 4, binwidth = .2, ip = ip,
                                         theta = true_theta,
                                         suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # Add arguments for 'geom_line'
  expect_silent(p <- plot_empirical_icc2(resp, item = 4, binwidth = .2, ip = ip,
                                         theta = true_theta, size = 1,
                                         alpha = .25, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

  # -------------------------------------------------------------------------- #
  # 'title' can be NULL to suppress the graph title
  expect_silent(p <- plot_empirical_icc2(resp, item = 3, title = NULL,
                                         suppress_plot = TRUE))

  # -------------------------------------------------------------------------- #
  # Item pool with testlets
  ip <- c(generate_testlet(n = 4, item_id_preamble = "t1"),
          generate_testlet(n = 2, item_id_preamble = "t2"),
          generate_testlet(n = 3, item_id_preamble = "t3"),
          generate_ip(n = 5))
  true_theta <- rnorm(2000)
  resp <- sim_resp(ip = ip, theta = true_theta, prop_missing = .3)
  resp_set <- response_set(resp, ip = ip)
  expect_silent(p <- plot_empirical_icc2(resp, item = 7, binwidth = .2,
                                         ip = ip, theta = true_theta,
                                         suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)
  expect_silent(p <- plot_empirical_icc2(resp = resp, item = 13, title = NULL,
                                         ip = ip, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)
  expect_silent(p <- plot_empirical_icc2(resp, item = 13, title = NULL,
                                         theta = true_theta,
                                         ip = ip, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)
  expect_silent(p <- plot_empirical_icc2(resp, item = "t3Item_1", binwidth = .2,
                                         ip = ip, theta = true_theta,
                                         suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)
  expect_silent(p <- plot_empirical_icc2(resp, item = "Item_4", binwidth = .2,
                                         ip = ip, theta = true_theta,
                                         suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)


  # Testlets and Response_set object
  expect_silent(p <- plot_empirical_icc2(resp_set, item = 7, binwidth = .2,
                                         ip = ip, theta = true_theta,
                                         suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot'); rm(p)

})


###############################################################################@
############################# plot_empirical_icc  ##############################
###############################################################################@
# test_that("Test plot_empirical_icc", {
#   ip <- generate_ip(model = "3PL", n = 20)
#   theta <- rnorm(100)
#   resp <- sim_resp(ip = ip, theta = theta, prop_missing = .1)
#   # item = 1
#   # bins = 10
#   # type = "eicc"
#   # theta = NULL
#   # title = "my title"
#   # suppress_plot = TRUE
#   # x_axis_scale = "theta"
#
#   # -------------------------------------------------------------------------- #
#   # The default
#   expect_silent(p <- plot_empirical_icc(resp = resp, item = 1,
#                                        suppress_plot = TRUE))
#   expect_s3_class(p, 'ggplot'); rm(p)
#
#   # -------------------------------------------------------------------------- #
#   # Change bin number
#   expect_silent(p <- plot_empirical_icc(resp = resp, type = "eicc", item = 1,
#                                        bins = 3, suppress_plot = TRUE))
#   expect_s3_class(p, 'ggplot'); rm(p)
#
#   # -------------------------------------------------------------------------- #
#   # Change x axis scale
#   expect_silent(p <- plot_empirical_icc(
#     resp = resp, item = 1, x_axis_scale = "number", suppress_plot = TRUE))
#   expect_s3_class(p, 'ggplot'); rm(p)
#   expect_silent(p <- plot_empirical_icc(
#     resp = resp, item = 1, x_axis_scale = "theta", ip = ip,
#     suppress_plot = TRUE))
#   expect_s3_class(p, 'ggplot'); rm(p)
#   expect_silent(p <- plot_empirical_icc(
#     resp = resp, item = 1, x_axis_scale = "percent", suppress_plot = TRUE))
#   expect_s3_class(p, 'ggplot'); rm(p)
#
#   # -------------------------------------------------------------------------- #
#   # item can be item ID
#   expect_silent(p <- plot_empirical_icc(resp = resp, item = "Item_3",
#                                        suppress_plot = TRUE))
#   expect_s3_class(p, 'ggplot'); rm(p)
#
#   # -------------------------------------------------------------------------- #
#   # type = "oep"
#   expect_silent(p <- plot_empirical_icc(
#     resp = resp, item = 2, type = "oep", ip = ip, x_axis_scale = "theta",
#     suppress_plot = TRUE))
#   expect_s3_class(p, 'ggplot'); rm(p)
#
#   # -------------------------------------------------------------------------- #
#   # When type = "oep", only x_axis_scale = "theta" is allowed.
#   expect_warning(p <- plot_empirical_icc(
#     resp = resp, item = 2, type = "oep", ip = ip, x_axis_scale = "percent",
#     suppress_plot = TRUE))
#   expect_s3_class(p, 'ggplot'); rm(p)
#
#   # -------------------------------------------------------------------------- #
#   # Expect error when graph type requires Itempool
#   expect_error(p <- plot_empirical_icc(resp = resp, item = "Item_3",
#                                       type = "oep", suppress_plot = TRUE))
#   expect_error(p <- plot_empirical_icc(resp = resp, item = "Item_3",
#                                       type = "oept", suppress_plot = TRUE))
#   # Length of theta and number of rows of resp should be equal
#   expect_error(p<- plot_empirical_icc(
#     resp = resp, item = 1, ip = ip, type = "oept", theta = 1:2,
#     suppress_plot = TRUE))
#   # When x_axis_scale is "theta", either theta or an item pool should be
#   # provided.
#   expect_error(p<- plot_empirical_icc(
#     resp = resp, item = 1, x_axis_scale = "theta", suppress_plot = TRUE))
#
#   # expect_silent(p <- plot_empirical_icc(resp = resp, item = "Item_3", ip = ip,
#   #                                     type = "oept", suppress_plot = TRUE))
#   # expect_s3_class(p, 'ggplot')
#
#   # -------------------------------------------------------------------------- #
#   # Check multiline x-axis labels
#   expect_silent(p <- plot_empirical_icc(
#     resp = resp, item = 1, x_axis_scale = "percent", n_dodge = 2,
#     suppress_plot = TRUE))
#   expect_s3_class(p, 'ggplot'); rm(p)
#
#   # -------------------------------------------------------------------------- #
#   # Polytomous items
#   ip <- generate_ip(model = "GRM", n = 20)
#   theta <- rnorm(1000)
#   resp <- sim_resp(ip = ip, theta = theta, prop_missing = .1)
#
#   expect_silent(p <- plot_empirical_icc(resp = resp, item = 1,
#                                        suppress_plot = TRUE))
#   expect_s3_class(p, 'ggplot'); rm(p)
#
#   # type = "oep"
#   expect_silent(p <- plot_empirical_icc(
#     resp = resp, item = 2, type = "oep", ip = ip, x_axis_scale = "theta",
#     suppress_plot = TRUE))
#   expect_s3_class(p, 'ggplot'); rm(p)
#
# })
