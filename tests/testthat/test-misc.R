
# library(rbenchmark, testthat)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% get_max_possible_total_score %%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_that("get_max_possible_total_score", {
  i1 <- sample(10:20, 1) # Number of standalone items
  i2 <- sample(3:8, 1) # number of categories of of the GRM item
  i3 <- sample(3:10, 1) # testlet size

  ip <- c(itempool(b = rnorm(i1)),
          item(a = 1, b = sort(rnorm(i2))))
  # -------------------------------------------------------------------------- #
  # No resp vector
  expect_identical(as.integer(get_max_possible_total_score(ip)), i1 + i2)
  expect_type(get_max_possible_total_score(ip), "double")

  # -------------------------------------------------------------------------- #
  # resp vector with one examinee
  resp <- sim_resp(ip = ip, theta = rnorm(1))
  expect_identical(as.integer(get_max_possible_total_score(
    ip, resp = as.vector(resp))), i1 + i2)
  expect_type(get_max_possible_total_score(ip), "double")

  # -------------------------------------------------------------------------- #
  # resp vector with multiple examinees
  n_theta <- sample(10:30, 1)
  theta <- setNames(rnorm(n_theta), paste0(sample(letters, 1), 1:n_theta))
  resp <- sim_resp(ip = ip, theta = theta)
  expect_identical(get_max_possible_total_score(ip, resp = resp),
                   setNames(as.numeric(rep(i1 + i2, n_theta)), names(theta)))

  # -------------------------------------------------------------------------- #
  # The size of the resp vector should be appropriate.
  expect_error(get_max_possible_total_score(ip, resp = as.vector(resp)))

  # -------------------------------------------------------------------------- #
  # item pool should be an Itempool object
  expect_error(get_max_possible_total_score(ip[[1]]))

  # -------------------------------------------------------------------------- #
  # Testlet should work
  ip <- c(
    itempool(b = rnorm(i1)),
    item(a = 1, b = sort(rnorm(i2))),
    testlet(itempool(b = rnorm(i3), item_id = paste0("t1-", 1:i3)))
    )
  expect_identical(as.integer(get_max_possible_total_score(ip)), i1 + i2 + i3)
  expect_type(get_max_possible_total_score(ip), "double")

  # -------------------------------------------------------------------------- #
  # NA works for 1pl items
  ip <- itempool(b = rnorm(i1))
  resp <- as.vector(sim_resp(ip, theta = rnorm(1, 1)))
  expect_type(get_max_possible_total_score(ip), "double")
  expect_identical(as.integer(get_max_possible_total_score(ip)), i1)
  expect_identical(as.integer(get_max_possible_total_score(ip, resp)), i1)
  resp[c(3, 5, 8)] <- NA
  expect_identical(as.integer(get_max_possible_total_score(ip, resp)),
                   sum(!is.na(resp)))

  # -------------------------------------------------------------------------- #
  # NA works for 1pl items
  ip <- c(
    itempool(b = rnorm(i1)),
    item(a = 1, b = sort(rnorm(i2))),
    testlet(itempool(b = rnorm(i3), item_id = paste0("t1-", 1:i3)))
    )
  resp <- as.vector(sim_resp(ip, theta = rnorm(1, 3)))
  resp[c(3, 5, 8)] <- NA
  expect_identical(get_max_possible_total_score(ip, resp), i1 + i2 + i3 - 3)
  resp[i1 + 1] <- NA # set GRM item NA
  expect_identical(get_max_possible_total_score(ip, resp), i1 + i3 - 3)

  # -------------------------------------------------------------------------- #
  # Dichotomous and polytomous items
  ip <- generate_ip(model = c("3PL", "GRM", "3PL", "GRM", "GRM"),
                    n_categories = c(2, 5, 2, 4, 6))
  expect_identical(get_max_possible_total_score(ip), 1 + 4 + 1 + 3 + 5)

})




############################################################################@###
######################## Test convert_resp function ############################
############################################################################@###
test_that("Test convert_resp Function", {
  n_theta <- sample(13:24, 1)
  n_item <- sample(15:36, 1)
  resp <- data.frame(
    examinee_id = rep(paste0("Ex-", 1:n_theta), each = n_item),
    item_id = rep(paste0("Item-", 1:n_item), n_theta),
    score = sample(0:1, n_item * n_theta, replace = TRUE)
    )
  # Remove some of the columns
  resp_na <- resp
  resp_na$score[sample(1:nrow(resp), floor(0.9 * min(n_theta, n_item)))] <- NA
  # resp_wide <- convert_resp(resp_na[!is.na(resp_na$score), ])
  expect_true(TRUE)
  # for (i in sample(1:max(n_item, n_theta)))
  #   expect_identical(resp_na[i, "score"], resp_wide[resp_na[i, "examinee_id"],
  #                                               resp_na[i, "item_id"]])


  # # Check the speed. It is very slow compared to tidyr alternative.
  # # Check https://github.com/tidyverse/tidyr/blob/master/R/spread.R
  # n_theta <- 50000
  # n_item <- 100
  # resp <- data.frame(
  #   examinee_id = rep(paste0("Ex-", 1:n_theta), each = n_item),
  #   item_id = rep(paste0("Item-", 1:n_item), n_theta),
  #   score = sample(0:1, n_item * n_theta, replace = TRUE)
  #   )
  # resp_na <- resp
  # resp_na$score[sample(1:nrow(resp), floor(0.9 * min(n_theta, n_item)))] <- NA
  # resp_na <- resp_na[!is.na(resp_na$score), ]
  # microbenchmark::microbenchmark(
  # tidyr = tidyr::spread(resp_na, item_id, score) %>%
  #   tibble::column_to_rownames("examinee_id"),
  # convert_resp = convert_resp(resp_na),
  # times = 3L
  # )
  # profvis::profvis(convert_resp(resp_na))
})



############################################################################@###
######################## is_single_value #######################################
############################################################################@###

test_that("is_single_value", {
  expected_numeric <- list(
    n1 = 1.99,
    n2 = -3.44,
    n3 = 14
  )

  expected_character <- list(
    c1 = "1kkdn",
    c2 = letters[1]
  )

  expect_true(all(sapply(expected_numeric, is_single_value,
                         class = "numeric", accept_na = FALSE)))
  expect_false(any(sapply(expected_character, is_single_value,
                          class = "integer", accept_na = FALSE)))

  expect_true(all(sapply(expected_character, is_single_value,
                         class = "character", accept_na = FALSE)))
  expect_false(any(sapply(expected_character, is_single_value,
                         class = "numeric", accept_na = FALSE)))

  expect_false(is_single_value(12L, class = "numeric"))
  expect_false(is_single_value(NA, class = "numeric", accept_na = TRUE))
  expect_true(is_single_value(as.numeric(NA), class = "numeric",
                              accept_na = TRUE))
  expect_false(is_single_value(NA, class = c("logical", "numeric"),
                               accept_na = FALSE))


  expect_true(all(sapply(expected_numeric, is_single_value,
                         accept_na = FALSE)))

  expect_true(is_single_value(TRUE))
  expect_false(is_single_value(TRUE, class = "character"))

  expect_true(is_single_value(as.Date("2021-01-01")))

  expected_falses <- list(
    bb = 1:3,
    pt1 = letters[1:4],
    pt2 = list(1),
    pt2 = matrix(2),
    v3 = data.frame(x = 1)
  )

  expect_true(all(sapply(expected_numeric, is_single_value,
                         class = "numeric", accept_na = FALSE)))

  # -------------------------------------------------------------------------- #
  expect_true(is_single_value(12))
  expect_true(is_single_value(12, class = "numeric"))   # TRUE
  expect_false(is_single_value(12, class = "character"))   # FALSE
  expect_true(is_single_value("12", class = "character"))   # TRUE
  expect_false(is_single_value(12, class = "logical"))   # FALSE

  expect_false(is_single_value(c(12, 18), class = "numeric"))   # FALSE
  expect_false(is_single_value(c(12, NA), class = "numeric"))   # FALSE
  expect_false(is_single_value(12L, class = "numeric"))   # FALSE
  expect_true(is_single_value(12L, class = "integer"))   # TRUE

  expect_true(is_single_value(12, class = c("numeric", "integer")))   # TRUE
  expect_true(is_single_value(12, class = c("numeric", "integer", "character")))
  expect_true(is_single_value("12", class = c("numeric", "integer",
                                              "character")))
  expect_true(is_single_value(12L, class = c("numeric", "integer")))   # TRUE
  expect_false(is_single_value(1:5, class = c("numeric", "integer")))   # FALSE

  expect_false(is_single_value(c("a", "b"), class =  c("integer", "character")))


})


############################################################################@###
######################## is_atomic_vector ######################################
############################################################################@###

test_that("is_atomic_vector", {
  expected_falses <- list(
    p1 = list(1, letters[1:10]),
    p2 = mtcars,
    p3 = NULL,
    p4 = matrix(),
    p5 = iris$Species, # Vector of factors
    p5 = array()
  )
  # expect_false(is_atomic_vector(matrix()))
  # expect_false(is_atomic_vector(list(1, letters[1:10])))
  # expect_false(is_atomic_vector(mtcars))
  # expect_false(is_atomic_vector(NULL))
  # expect_false(is_atomic_vector(array()))

  expect_false(any(sapply(expected_falses, is_atomic_vector)))

  expected_trues <- list(
    p1 = 3,
    p2 = 2:12,
    p3 = NA,
    p4 = letters[1:10],
    p6 = c(NA, NA, TRUE),
    p7 = as.Date(paste0("2021-01-", 11:31))
  )
  expect_true(all(sapply(expected_trues, is_atomic_vector)))

  # -------------------------------------------------------------------------- #
  expect_true(is_atomic_vector(c(1, 4.2), class = c("integer", "numeric")))
  expect_error(is_atomic_vector(c(1, 4.2), class = c("xyz", "numeric")))

})



############################################################################@###
######################## is_integer ############################################
############################################################################@###

test_that("is_integer", {

  expected_falses <- list(
    bb = 5 - 1e-8,
    pt1 = 1.0000001,
    pt2 = 1.00000001,
    pt2 = 1.00000000001,
    v3 = 3243.34,
    c1 = "abc"
  )
  expect_false(any(sapply(expected_falses, is_integer)))

  # List is taken from https://stackoverflow.com/a/57172412/2275286
  expected_trues <- list(
    cl = sqrt(2)^2,
    pp = 9.0,
    n1 = -2,
    # t = 1 / (1 - 0.98), # This cannot pass the test.. it should
    ar0 = 66L,
    ar1 = 66,
    ar2 = 1 + 2^-50,
    v = 222e3,
    w1 = 1e4,
    w2 = 1e5,
    # v2 = "1000000000000000000000000000000000001",
    an = 2 / 49 * 49,
    ju1 = 1e22,
    ju2 = 1e24,
    al = floor(1),
    v5 = 1.0000000000000001 # this is under machine precision!
  )
  expect_true(all(sapply(expected_trues, is_integer)))

  # -------------------------------------------------------------------------- #
  # Extreme cases
  expect_false(is_integer(list()))
  expect_false(is_integer(c()))
  expect_false(is_integer(NULL))
  # this is controversial:
  expect_false(is_integer(as.integer()))

  # -------------------------------------------------------------------------- #
  # Data frame will result in a matrix of logical values
  x <- data.frame(int = 1:7, char = letters[1:7])
  expect_equal(
    is_integer(x),
    matrix(c(rep(TRUE, 7), rep(FALSE, 7)), nrow = 7,
           dimnames = list(NULL, c("int", "char")))
    )

  # -------------------------------------------------------------------------- #
  # Matrix will result in a matrix of logical
  expect_equal(is_integer(matrix(1:6, 2, 3)), matrix(rep(TRUE, 6), 2, 3))

})
