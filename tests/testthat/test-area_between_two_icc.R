
# library(rbenchmark, testthat)

find_intersection <- function(ip) {
  theta <- seq(-4, 4, .001)
  p1 = prob(ip[[1]], theta = theta)[, 2]
  p2 = prob(ip[[2]], theta = theta)[, 2]
  pos <- head(cumsum(rle(p1 - p2 >= 0)$lengths), -1)
  if (length(pos) > 0) {
    expected <- (theta[pos] + theta[sapply(pos + 1, function(x)
    min(x, length(theta)))]) / 2
  } else expected <- theta[pos]
  return(expected)
}

############################################################################@###
######################## find_icc_intersect_cpp ################################
############################################################################@###
test_that("find_icc_intersect_cpp", {
  # Only Rasch, 1PL, 2PL, 3PL is allowed
  ip <- generate_ip(model = c("GRM", "3PL"))
  expect_error(find_icc_intersect_cpp(item_1 = ip[[1]], item_2 = ip[[2]]),
               "Invalid model.")
  check_find_icc <- function(ip) {
    expected <- find_intersection(ip)
    observed <- find_icc_intersect_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
                                       theta_range = c(-4, 4))
    expect_equal(expected, observed, tolerance = 0.015)
    # plot(ip)
    # plot(ip, theta_range = observed + c(-1, 1) * 0.0009,
    #      suppress_plot = TRUE) +
    #   ggplot2::ylim(min(p1[pos], p2[pos]), p1[pos + 1])
  }

  # -------------------------------------------------------------------------- #
  # Check find_intersection() function for 2PL-2PL
  ip <- generate_ip(model = c("2PL", "2PL"))
  b1 <- ip[[1]]$b
  b2 <- ip[[2]]$b
  a1 <- ip[[1]]$a
  a2 <- ip[[2]]$a
  D <- ip[[1]]$D
  t1 <- round(runif(1, -5, -3), 2)
  t2 <- round(runif(1, 3, 5), 2)

  # Special case for 2PL
  expected <- (a1 * b1 - a2 * b2) / (a1 - a2)
  observed <- find_icc_intersect_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
                                     theta_range = c(t1, t2))
  if (expected > t1 && expected < t2) {
    expect_equal(observed, expected, tolerance = 0.001)
  } else expect_true(length(observed) == 0) # no intersection


  # -------------------------------------------------------------------------- #
  # Check various model combinations
  for (models in c(combn(c("Rasch", "2PL", "3PL", "4PL"), 2, simplify = FALSE),
                   list(c("2PL", "2PL")), list(c("3PL", "3PL")),
                   list(c("4PL", "4PL")))) {
    for (i in 1:10) check_find_icc(generate_ip(model = models))
  }

})

#
# ############################################################################@###
# ######################## area_between_icc_exact_cpp ############################
# ############################################################################@###
# test_that("area_between_icc_exact_cpp", {
#
#   # -------------------------------------------------------------------------- #
#   # Only Rasch, 1PL, 2PL, 3PL is allowed
#   ip <- generate_ip(model = c("GRM", "3PL"))
#   expect_error(area_between_icc_exact_cpp(item_1 = ip[[1]], item_2 = ip[[2]]),
#                "Invalid model.")
#
#
#   # -------------------------------------------------------------------------- #
#   # The scaling constants "D" should be the same
#   ip <- generate_ip(model = c("3PL", "3PL"))
#   ip[[1]]$D <- 1.7
#   expect_error(area_between_icc_exact_cpp(item_1 = ip[[1]], item_2 = ip[[2]]),
#                "Scaling constants of the items should be equal to each other.")
#
#   # -------------------------------------------------------------------------- #
#   # Check Rasch-Rasch
#   ip <- generate_ip(model = c("Rasch", "Rasch"))
#   expect_equal(area_between_icc_exact_cpp(item_1 = ip[[1]], item_2 = ip[[2]]),
#                ip[[2]]$b - ip[[1]]$b)
#   expect_equal(area_between_icc_exact_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                           signed_area = FALSE),
#                abs(ip[[2]]$b - ip[[1]]$b))
#
#   # -------------------------------------------------------------------------- #
#   # Check 1PL-Rasch
#   ip <- generate_ip(model = c("1PL", "Rasch"))
#   expect_equal(area_between_icc_exact_cpp(item_1 = ip[[1]], item_2 = ip[[2]]),
#                ip[[2]]$b - ip[[1]]$b)
#   expect_equal(area_between_icc_exact_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                           signed_area = FALSE),
#                abs(ip[[2]]$b - ip[[1]]$b), tolerance = 0.0001)
#
#   # -------------------------------------------------------------------------- #
#   # Check 1PL-1PL
#   ip <- generate_ip(model = c("1PL", "1PL"))
#   expect_equal(area_between_icc_exact_cpp(item_1 = ip[[1]], item_2 = ip[[2]]),
#                ip[[2]]$b - ip[[1]]$b)
#   expect_equal(area_between_icc_exact_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                           signed_area = FALSE),
#                abs(ip[[2]]$b - ip[[1]]$b), tolerance = 0.0001)
#
#   # -------------------------------------------------------------------------- #
#   # Check 2PL-2PL
#   ip <- generate_ip(model = c("2PL", "2PL"))
#   expect_equal(area_between_icc_exact_cpp(item_1 = ip[[1]], item_2 = ip[[2]]),
#                ip[[2]]$b - ip[[1]]$b)
#
#   # -------------------------------------------------------------------------- #
#   # 3PL - 3PL - The area between non-equal 'c' parameters is infinity.
#   ip <- generate_ip(model = c("3PL", "3PL"))
#   expect_equal(area_between_icc_exact_cpp(item_1 = ip[[1]], item_2 = ip[[2]]),
#                Inf)
#
#   # -------------------------------------------------------------------------- #
#   # Check 3PL-3PL -- Raju (1990) - Table 2 - Item 1
#   ip <- c(item(a = 0.5, b = -2.67, c = 0.2),
#           item(a = 0.61, b = -1.67, c = 0.2))
#   expect_equal(round(area_between_icc_exact_cpp(item_1 = ip[[1]],
#                                                 item_2 = ip[[2]]), 2),
#                0.8, tolerance = 0.1)
#   expect_equal(round(area_between_icc_exact_cpp(
#     item_1 = ip[[1]], item_2 = ip[[2]], signed_area = FALSE), 2),
#     0.8, tolerance = 0.1)
#
#   # Check 3PL-3PL -- Raju (1990) - Table 2 - Item 3
#   ip <- c(item(a = 0.91, b = -1.59, c = 0.2, D = 1.7),
#           item(a = 0.76, b = -1.67, c = 0.2, D = 1.7))
#   expect_equal(round(area_between_icc_exact_cpp(
#     item_1 = ip[[1]], item_2 = ip[[2]]), 2), -0.06, tolerance = 0.1)
#   expect_equal(round(area_between_icc_exact_cpp(
#     item_1 = ip[[1]], item_2 = ip[[2]], signed_area = FALSE), 2),
#     0.15, tolerance = 0.01)
#
#   # Check 3PL-3PL -- Raju (1990) - Table 20 - Item 3
#   ip <- c(item(a = 1.17, b = -0.2, c = 0.2, D = 1.7),
#           item(a = 0.87, b = -0.44, c = 0.2, D = 1.7))
#   expect_equal(round(area_between_icc_exact_cpp(
#     item_1 = ip[[1]], item_2 = ip[[2]]), 2), -0.19, tolerance = 0.1)
#   expect_equal(round(area_between_icc_exact_cpp(
#     item_1 = ip[[1]], item_2 = ip[[2]], signed_area = FALSE), 2),
#     0.25, tolerance = 0.01)
#
#   # -------------------------------------------------------------------------- #
#
# })
#
#
#
# ############################################################################@###
# ######################## area_between_icc_closed_cpp ###########################
# ############################################################################@###
# test_that("area_between_icc_closed_cpp", {
#
#   # -------------------------------------------------------------------------- #
#   # Only Rasch, 1PL, 2PL, 3PL is allowed
#   ip <- generate_ip(model = c("GPCM", "3PL"))
#   expect_error(area_between_icc_closed_cpp(item_1 = ip[[1]], item_2 = ip[[2]]),
#                "Invalid model.")
#
#   # -------------------------------------------------------------------------- #
#   # Check Rasch-Rasch
#   ip <- generate_ip(model = c("Rasch", "Rasch"))
#   t1 <- runif(1, -5, -3)
#   t2 <- runif(1, 3, 5)
#   b1 <- ip[[1]]$b
#   b2 <- ip[[2]]$b
#   expected <- log( ((1 + exp(t2 - b1)) * (1 + exp(t1 - b2))) /
#                    ((1 + exp(t1 - b1)) * (1 + exp(t2 - b2))) )
#   expect_equal(area_between_icc_closed_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                            theta_range = c(t1, t2)), expected)
#   expect_equal(area_between_icc_closed_cpp(
#     item_1 = ip[[1]], item_2 = ip[[2]], theta_range = c(t1, t2),
#     signed_area = FALSE), abs(expected))
#
#   # -------------------------------------------------------------------------- #
#   # Check 1PL-Rasch
#   ip <- generate_ip(model = c("1PL", "Rasch"))
#   t1 <- runif(1, -5, -3)
#   t2 <- runif(1, 3, 5)
#   b1 <- ip[[1]]$b
#   b2 <- ip[[2]]$b
#   expected <- log( ((1 + exp(t2 - b1)) * (1 + exp(t1 - b2))) /
#                    ((1 + exp(t1 - b1)) * (1 + exp(t2 - b2))) )
#   expect_equal(area_between_icc_closed_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                            theta_range = c(t1, t2)), expected)
#   expect_equal(area_between_icc_closed_cpp(
#     item_1 = ip[[1]], item_2 = ip[[2]], theta_range = c(t1, t2),
#     signed_area = FALSE), abs(expected))
#
#   # -------------------------------------------------------------------------- #
#   # Different D scaling constants should be fine
#   D <- 1.702
#   ip <- generate_ip(model = c("3PL", "3PL"), D = D)
#   expected_signed <- area_between_icc_closed_cpp(item_1 = ip[[1]],
#                                                  item_2 = ip[[2]])
#   expected_unsigned <- area_between_icc_closed_cpp(item_1 = ip[[1]],
#                                                    item_2 = ip[[2]],
#                                                    signed_area = FALSE)
#   ip[[1]]$a <- ip[[1]]$a * D
#   ip[[1]]$D <- 1
#   expect_equal(area_between_icc_closed_cpp(item_1 = ip[[1]], item_2 = ip[[2]]),
#                expected_signed)
#   expect_equal(area_between_icc_closed_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                            signed_area = FALSE),
#                expected_unsigned)
#
#
#   # -------------------------------------------------------------------------- #
#   # Check 2PL-2PL
#   ip <- generate_ip(model = c("2PL", "2PL"), D = 1.702)
#   t1 <- round(runif(1, -5, -3), 2)
#   t2 <- round(runif(1, 3, 5), 2)
#   b1 <- ip[[1]]$b
#   b2 <- ip[[2]]$b
#   a1 <- ip[[1]]$a
#   a2 <- ip[[2]]$a
#   D <- ip[[1]]$D
#   # plot(ip)
#
#   # Signed area
#   expected <- log( ((1 + exp(D * a1 * (t2 - b1)))^(1/(D*a1)) *
#                       (1 + exp(D * a2 * (t1 - b2)))^(1/(D*a2))) /
#                    ((1 + exp(D * a1 * (t1 - b1)))^(1/(D*a1)) *
#                       (1 + exp(D * a2 * (t2 - b2)))^(1/(D*a2))) )
#   expect_equal(area_between_icc_closed_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                            theta_range = c(t1, t2)), expected)
#
#   # Unsigned area
#   theta_intersect <- (a1 * b1 - a2 * b2) / (a1 - a2)
#   if (theta_intersect < t1 || theta_intersect > t2) {
#     expected <- abs(area_between_icc_closed_cpp(
#       item_1 = ip[[1]], item_2 = ip[[2]], theta_range = c(t1, t2)))
#   } else {
#     area1 <- area_between_icc_closed_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                          theta_range = c(t1, theta_intersect))
#     area2 <- area_between_icc_closed_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                          theta_range = c(theta_intersect, t2))
#     expected <- abs(area1) + abs(area2)
#   }
#
#   expect_equal(area_between_icc_closed_cpp(
#     item_1 = ip[[1]], item_2 = ip[[2]], theta_range = c(t1, t2),
#     signed_area = FALSE), expected)
#
#   # -------------------------------------------------------------------------- #
#   # Check 3PL-3PL
#   ip <- generate_ip(model = c("3PL", "3PL"), D = 1.702)
#   t1 <- round(runif(1, -5, -3), 2)
#   t2 <- round(runif(1, 3, 5), 2)
#   b1 <- ip[[1]]$b
#   b2 <- ip[[2]]$b
#   a1 <- ip[[1]]$a
#   a2 <- ip[[2]]$a
#   c1 <- ip[[1]]$c
#   c2 <- ip[[2]]$c
#   D <- ip[[1]]$D
#   # plot(ip)
#   intersections <- c(t1, find_intersection(ip), t2)
#
#   find_area <- function(item1, item2, t1, t2) {
#     a1 <- item1$a
#     b1 <- item1$b
#     c1 <- item1$c
#     D1 <- item1$D
#     a2 <- item2$a
#     b2 <- item2$b
#     c2 <- item2$c
#     D2 <- item2$D
#     (c1 - c2) * (t2 - t1) +
#       log( ((1 + exp(D1 * a1 * (t2 - b1)))^((1 - c1)/(D1*a1)) *
#                       (1 + exp(D2 * a2 * (t1 - b2)))^((1 - c2)/(D2*a2))) /
#                    ((1 + exp(D1 * a1 * (t1 - b1)))^((1 - c1)/(D1*a1)) *
#                       (1 + exp(D2 * a2 * (t2 - b2)))^((1 - c2)/(D2*a2))) )
#   }
#
#   for (i in seq_len(length(intersections) - 1)) {
#     expect_equal(
#       find_area(ip[[1]], ip[[2]], intersections[i], intersections[i + 1]),
#       area_between_icc_closed_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                   theta_range = c(intersections[i],
#                                                   intersections[i + 1])))
#   }
#
#   # Signed area
#   expected_signed <- 0
#   for (i in seq_len(length(intersections) - 1)) {
#     expected_signed <- expected_signed +
#       find_area(ip[[1]], ip[[2]], intersections[i], intersections[i + 1])
#   }
#   expect_equal(area_between_icc_closed_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                            theta_range = c(t1, t2)),
#                expected_signed)
#   expect_equal(area_between_icc_closed_cpp(item_1 = ip[[2]], item_2 = ip[[1]],
#                                            theta_range = c(t1, t2)),
#                -expected_signed)
#
#   # Unsigned area
#   expected_unsigned <- 0
#   for (i in seq_len(length(intersections) - 1)) {
#     temp <- find_area(ip[[1]], ip[[2]], intersections[i], intersections[i + 1])
#     expected_unsigned <- expected_unsigned + abs(temp)
#   }
#   expect_equal(area_between_icc_closed_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                            signed_area = FALSE,
#                                            theta_range = c(t1, t2)),
#                expected_unsigned)
#   expect_equal(area_between_icc_closed_cpp(item_1 = ip[[2]], item_2 = ip[[1]],
#                                            signed_area = FALSE,
#                                            theta_range = c(t1, t2)),
#                expected_unsigned)
# })
#
#
# ############################################################################@###
# ######################## area_between_icc ######################################
# ############################################################################@###
#
# test_that("area_between_icc", {
#
#   # -------------------------------------------------------------------------- #
#   # Check argument
#   ip <- generate_ip(model = c("3PL", "3PL"))
#   expect_error(area_between_icc(item1 = ip[[1]], item2 = ip[[2]], type = "XYZ"))
#
#   # -------------------------------------------------------------------------- #
#   # Closed
#   ip <- generate_ip(model = c("3PL", "3PL"))
#   t1 <- round(runif(1, -5, -3), 2)
#   t2 <- round(runif(1, 3, 5), 2)
#   # Signed area
#   observed <- area_between_icc(item1 = ip[[1]], item2 = ip[[2]],
#                                type = "closed", theta_range = c(t1, t2),
#                                signed_area = TRUE)
#   expected <- area_between_icc_closed_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                           signed_area = TRUE,
#                                           theta_range = c(t1, t2))
#   expect_equal(observed[1, 2], expected)
#   # Unsigned area
#   observed <- area_between_icc(item1 = ip[[1]], item2 = ip[[2]],
#                                type = "closed", theta_range = c(t1, t2),
#                                signed_area = FALSE)
#   expected <- area_between_icc_closed_cpp(item_1 = ip[[1]], item_2 = ip[[2]],
#                                           signed_area = FALSE,
#                                           theta_range = c(t1, t2))
#   expect_equal(observed[1, 2], expected)
#
#   # -------------------------------------------------------------------------- #
#   # Exact
#   # Check 3PL-3PL -- Raju (1990) - Table 20 - Item 15
#   ip <- c(item(a = 0.73, b = 0.44, c = 0.2, D = 1.7),
#           item(a = 0.98, b = 0.25, c = 0.2, D = 1.7))
#   expected <- area_between_icc(item1 = ip[[1]], item2 = ip[[2]],
#                                signed_area = TRUE)
#   expect_equal(round(expected[1, 2], 2), -0.15, tolerance = 0.1)
#
#   # Unsigned area
#   expected <-  area_between_icc(item1 = ip[[1]], item2 = ip[[2]],
#                                 signed_area = FALSE)
#   expect_equal(round(expected[1, 2], 2), 0.26, tolerance = 0.01)
# })
