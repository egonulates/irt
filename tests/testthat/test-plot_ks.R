# library(testthat)

###############################################################################@
############################# plot.ks_output ###################################
###############################################################################@
test_that("plot.ks_output", {
  # 3PL
  n <- sample(10:50,1)
  ip <- itempool(a = runif(n, .5, 2), b = rnorm(n), c = runif(n, 0, .3), D = 1)
  resp_set <- generate_resp_set(ip = ip, theta = rnorm(1000))
  ks_output <- ks(resp_set)

  p <- plot(ks_output, 3, suppress_plot = TRUE)
  expect_s3_class(p, 'ggplot')

  # Add ICC curve
  p <- plot(ks_output, 3, ip = ip, suppress_plot = TRUE)
  expect_s3_class(p, 'ggplot')


  # Remove error bands
  p <- plot(ks_output, 3, ip = ip, ci = NULL, suppress_plot = TRUE)
  expect_s3_class(p, 'ggplot')

  ### Base R versions ###
  expect_silent(plot(ks_output, 3, base_r_graph = TRUE))

  # Add ICC curve
  expect_silent(plot(ks_output, 3, ip = ip, base_r_graph = TRUE))

  # Remove error bands
  expect_silent(plot(ks_output, 3, ip = ip, ci = NULL, base_r_graph = TRUE))


  # Integer is acceptable as item_no
  p <- plot(ks_output, item_no = 3L, ip = ip, suppress_plot = TRUE)
  expect_s3_class(p, 'ggplot')

  # -------------------------------------------------------------------------- #
  # 'title' can be NULL to suppress the graph title
  expect_silent(plot(ks_output, 3, ip = ip, title = NULL))
  expect_silent(plot(ks_output, 3, ip = ip, title = NULL, base_r_graph = TRUE))
})
