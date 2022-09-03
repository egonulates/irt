# library(testthat)

###############################################################################@
############################# plot.Item  #######################################
###############################################################################@

test_that("Test plot.Item", {
  expect_silent(p <- plot(x = item(b = 0.3, D = 1), suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  ip <- item(a = 1.2, b = 0.3, c = .2)
  expect_silent(p <- plot(ip, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  expect_silent(p <- plot(item(a = 1.2, b = 0.3, c = .2, d = .89, D = 1),
                          suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  # Base graph
  expect_silent(plot(generate_item(), base_r_graph = TRUE))
  # theta_range
  expect_silent(plot(generate_item(), base_r_graph = TRUE, theta_range = -3:3))
  expect_silent(plot(generate_item(), base_r_graph = FALSE, theta_range = -3:3))

  ### Plot Generalized Partial Credit Model ###
  itm_gpcm <- generate_item(model = "GPCM")
  expect_silent(p <- plot(itm_gpcm, suppress_plot = TRUE))
  expect_silent(p <- plot(itm_gpcm, suppress_plot = TRUE,
                          legend_title = "Response Categories"))
  expect_s3_class(p, 'ggplot')
  # Test base graphs with different category names (ideally visually)
  expect_silent(p <- plot(itm_gpcm, base_r_graph = TRUE))
  expect_silent(p <- plot(itm_gpcm, base_r_graph = TRUE, legend_title = ""))
  expect_silent(p <- plot(itm_gpcm, base_r_graph = TRUE,
                          legend_title = "Response Categories"))
  expect_silent(p <- plot(
    itm_gpcm, base_r_graph = TRUE,
    category_names = c("Strongly Disagree", "Disagree",  "Agree",
                       "Strongly Agree")))
  expect_silent(p <- plot(
    itm_gpcm, base_r_graph = TRUE,
    category_names = c("Strongly Strongly Disagree", "Disagree",  "Agree",
                       "Strongly Agree")))

  ### Plot Reparametrized Generalized Partial Credit Model ###
  itm_gpcm <- generate_item(model = "GPCM2")
  expect_silent(p <- plot(itm_gpcm, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  ### Plot Partial Credit Model ###
  itm_pcm <- generate_item(model = "PCM")
  expect_silent(p <- plot(itm_gpcm, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  ### Plot Graded Response Model ###
  ip <- item(a = 0.902, b = c(-1.411, 0.385, 1.79))
  expect_silent(p <- plot(ip, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  # De Ayala (2009), p. 221, Fig. 8.9
  ip <- item(a = 1.5, b = c(-1, 1), D = 1)
  expect_silent(p <- plot(ip, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  p <- plot(ip, suppress_plot = TRUE) + ggplot2::geom_vline(xintercept = 0)
  expect_s3_class(p, 'ggplot')
  # De Ayala (2009), p. 222, Fig. 8.10
  ip <- item(a = 1.5, b = c(-1, 1.4, 2), D = 1)
  expect_silent(p <- plot(ip, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  # Check category names
  expect_silent(p <- plot(ip, category_names = c("Strongly Disagree", "Disagree",
                                                 "Agree","Strongly Agree"),
                          suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  # Suppress category names legend:
  expect_silent(p <- plot(ip, category_names = NULL, suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  # Plot 2PM
  ip <- item(a = 0.8, b = 1, model = "GRM")
  expect_silent(p <- plot(ip, category_names = c("Incorrect", "Correct"),
                          legend_title = "Response", suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  # -------------------------------------------------------------------------- #
  # 'title' can be NULL to suppress the graph title
  n <- sample(10:50,1)
  ip <- item(a = 0.8, b = 1, model = "2PL")
  expect_silent(p <- plot(ip, suppress_plot = TRUE, title = NULL))
  expect_silent(p <- plot(ip, suppress_plot = TRUE, title = NULL,
                          base_r_graph = TRUE))

})


