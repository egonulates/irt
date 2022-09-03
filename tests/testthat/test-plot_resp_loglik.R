# library(testthat)


###############################################################################@
############################# plot_resp_loglik  ################################
###############################################################################@
test_that("Test plot_resp_loglik", {
  ip <- generate_ip(model = "3PL", n = sample(10:50,1))
  resp_set <- generate_resp_set(ip = ip, theta = rnorm(10))

  expect_silent(p <- plot_resp_loglik(ip, resp_set[[1]], suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  expect_silent(p <- plot_resp_loglik(ip, resp_set[[1]], suppress_plot = TRUE,
                                      base_r_graph = TRUE))

  # Custom theta_range
  expect_silent(p <- plot_resp_loglik(ip, resp_set[[2]], suppress_plot = TRUE,
                                      base_r_graph = TRUE, theta_range = -3:3))

  # Likelihood = TRUE
  expect_silent(p <- plot_resp_loglik(ip, resp_set[[1]], likelihood = TRUE,
                                      suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')
  expect_silent(p <- plot_resp_loglik(ip, resp_set[[1]], likelihood = TRUE,
                                      suppress_plot = TRUE,
                                      base_r_graph = TRUE))
  # Resp is score vector
  expect_silent(p <- plot_resp_loglik(ip, resp = resp_set[[1]]$score,
                                      suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')

  # 4PM model
  ip <- generate_ip(model = "4PL", n = sample(8:10,1))
  resp_set <- generate_resp_set(ip = ip, theta = rnorm(10), prop_missing = .2)

  expect_silent(p <- plot_resp_loglik(ip, resp = resp_set[[1]],
                                      suppress_plot = TRUE))
  expect_s3_class(p, 'ggplot')


  # -------------------------------------------------------------------------- #
  # 'title' can be NULL to suppress the graph title
  expect_silent(p <- plot_resp_loglik(ip, resp_set[[1]], title = NULL,
                                      suppress_plot = TRUE))
  expect_silent(p <- plot_resp_loglik(ip, resp_set[[1]], title = NULL,
                                      suppress_plot = TRUE,
                                      base_r_graph = TRUE))

  # resp <- resp_set[[1]]
  # theta_range = c(-5,5)
  # title = ""
  # likelihood = FALSE
  # show_estimate = TRUE
  # suppress_plot = FALSE
  # text_size = 12
})



