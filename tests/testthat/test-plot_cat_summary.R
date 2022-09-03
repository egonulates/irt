# library(testthat)


###############################################################################@
############################# plot.cat_output  #################################
###############################################################################@
test_that("Test plot.cat_output", {
  cd <- create_cat_design(ip = generate_ip(n = 100))
  co <- cat_sim(true_ability = rnorm(1), cd = cd)

  expect_silent(p <- plot(co))
  expect_s3_class(p, 'ggplot')
  # Suppress item difficulties
  expect_silent(p <- plot(co, plot_b = FALSE))
  # Suppress Standard Error Band
  expect_silent(p <- plot(co, se_band = FALSE))
  # Add final theta estimate line
  expect_silent(p <- plot(co, horizontal_line = "final_theta"))
  expect_silent(p <- plot(co, horizontal_line = "true_theta"))
  # Change Title
  expect_silent(p <- plot(co, title = "CAT Progress for Examinee ABC"))

  # -------------------------------------------------------------------------- #
  # All incorrect response
  cd <- create_cat_design(ip = itempool(a = rep(2, 40), b = runif(40, 3, 5)))
  co <- cat_sim(true_ability = -1e16, cd = cd)
  expect_silent(p <- plot(co))
  expect_s3_class(p, 'ggplot')

  # -------------------------------------------------------------------------- #
  # All correct response
  cd <- create_cat_design(ip = itempool(a = rep(2, 40), b = runif(40, -5, -3)))
  co <- cat_sim(true_ability = 1e16, cd = cd)
  expect_silent(p <- plot(co))
  expect_s3_class(p, 'ggplot')


  # -------------------------------------------------------------------------- #
  # Very long test
  cd <- create_cat_design(ip = generate_ip(n = 100),
                          termination_rule = "max_item",
                          termination_par = list(max_item = 99))
  co <- cat_sim(true_ability = rnorm(1), cd = cd)
  expect_silent(p <- plot(co))
  expect_s3_class(p, 'ggplot')


  # -------------------------------------------------------------------------- #
  # The numbers on the x-axis are integers
  cd <- create_cat_design(ip = generate_ip(n = 100),
                          termination_rule = "max_item",
                          termination_par = list(max_item = 79))
  co <- cat_sim(true_ability = rnorm(1), cd = cd)
  plot(co)
  expect_silent(p <- plot(co))
  expect_s3_class(p, 'ggplot')

})

