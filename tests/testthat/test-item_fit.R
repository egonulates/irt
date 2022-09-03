
test_that("item_fit - Itempool", {
  ip <- generate_ip(model = "2PL")
  theta <- rnorm(100)
  resp <- sim_resp(ip = ip, theta = theta)

  # expect_true(inherits(item_fit(ip = ip, resp = resp, theta = theta), "matrix"))

  # -------------------------------------------------------------------------- #
  # --------------- Q1 ------------------------------------------------------- #
  # -------------------------------------------------------------------------- #

  # Test Yen's Q1
  ip <- generate_ip(model = "2PL", n = 10)
  theta <- rnorm(1000)
  resp <- sim_resp(ip = ip, theta = theta, output = "response_set")
  q1 <- item_fit(ip = ip, resp = resp, theta = theta, type = "Q1")
  expect_true(all(q1$p_value <= 1))
  expect_true(all(q1$df == 10 - 2))

  # -------------------------------------------------------------------------- #
  # Change the number of groups
  ip <- generate_ip(model = sample(c("Rasch", "3PL", "4PL", "2PL"), 20, TRUE))
  theta <- rnorm(1000)
  resp <- sim_resp(ip = ip, theta = theta, output = "response_set")
  n_groups <- sample(20:30, 1)
  q1 <- item_fit(ip = ip, resp = resp, theta = theta, type = "Q1",
                 n_groups = n_groups)
  expect_true(all(q1$p_value <= 1))
  num_of_pars <- sapply(ip$model, switch, `4PL` = 4, `3PL` = 3, Rasch = 1,
                        `2PL` = 2)
  expect_true(all(q1$df == (n_groups - num_of_pars)))

  # -------------------------------------------------------------------------- #
  # Check when there are missing data
  ip <- generate_ip(model = "2PL", n = 10)
  theta <- rnorm(1000)
  resp <- sim_resp(ip = ip, theta = theta, prop_missing = .3,
                   output = "response_set")
  q1 <- item_fit(ip = ip, resp = resp, theta = theta, type = "Q1")
  expect_true(all(q1$p_value <= 1))
  # Check whether designating individual items will result in the same number
  q1_4 <- item_fit(ip = ip, resp = resp, theta = theta, type = "Q1",
                   item_id = "Item_4")
  expect_equal(q1_4$Q1, q1$Q1[q1$item_id == "Item_4"])

  q1_59 <- item_fit(ip = ip, resp = resp, theta = theta, type = "Q1",
                    item_id = c("Item_5", "Item_9"))
  expect_equal(q1_59$Q1, q1$Q1[q1$item_id %in% c("Item_5", "Item_9")])

  # Reverse the item item_id order
  q1_95 <- item_fit(ip = ip, resp = resp, theta = theta, type = "Q1",
                    item_id = c("Item_9", "Item_5"))
  expect_equal(q1_95$Q1, q1$Q1[match(c("Item_9", "Item_5"), q1$item_id)])

  # -------------------------------------------------------------------------- #
  # Second check with missing data
  ip <- generate_ip(model = "2PL", n = 10)
  theta <- rnorm(1000)
  resp <- sim_resp(ip = ip, theta = theta, prop_missing = .3,
                   output = "matrix")
  # remove all rows for which first element is missing
  resp_missing <- resp[!is.na(resp[, 1]), ]
  q1_all <- item_fit(ip = ip, resp = resp, theta = theta, type = "Q1",
                     item_id = NULL)
  q1 <- item_fit(ip = ip, resp = resp_missing, theta = theta, type = "Q1",
                 item_id = "Item_1")

  # -------------------------------------------------------------------------- #
  # Q1 won't work with polytomous items
  ip_poly <- generate_ip(model = "GRM")
  theta <- rnorm(100)
  resp <- sim_resp(ip = ip_poly, theta = theta)
  expect_error(item_fit(ip = ip_poly, resp = resp, theta = theta, type = "Q1"))


  # # ------------------------------------------------------------------------ #
  # # Q1 will eliminate theta values that are NA.
  # ip <- generate_ip(model = "2PL", n = 10)
  # theta_all <- rnorm(1000)
  # resp_set_all <- generate_resp_set(
  #   ip = ip, theta = theta, prop_missing = .3)
  # theta_all[1] <- NA
  # theta <- theta_all[-1]
  # resp_set <- resp_set_all[-1]
  #
  # expected <- item_fit(ip = ip, resp = resp_set, theta = theta, type = "Q1")
  # observed <- item_fit(ip = ip, resp = resp_set_all, theta = theta_all,
  #                      type = "Q1")
  # expect_identical(expected, observed)




  # -------------------------------------------------------------------------- #
  # --------------- G2 ------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # type = "G2"
  # item_id = NULL
  # n_groups = NULL
  # source("C:/Users/EGonulates/Dropbox/irt/R/misc.R")
  # UNIDIM_DICHO_MODELS = irt:::UNIDIM_DICHO_MODELS
  # UNIDIM_POLY_MODELS = irt:::UNIDIM_POLY_MODELS

  ip <- generate_ip(model = sample(c("GRM", "3PL"), 10, TRUE),
                    n_categories = sample(3:5, 10, TRUE))
  theta <- rnorm(1000)
  resp <- sim_resp(ip = ip, theta = theta, prop_missing = .2)
  q1 <- item_fit(ip = ip, resp = resp, theta = theta, type = "G2")

  # -------------------------------------------------------------------------- #
  # Check G2 for some dichotomous items.
  ip <- new("Itempool", item_list = list(
            Item_1 = new(
              "2PL", a = 1.051, b = -0.418, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_1", content = NULL, misc = list(key = "B")
            ),
            Item_2 = new(
              "2PL", a = 0.7682, b = -0.2721, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_2", content = NULL, misc = list(key = "B")
            ),
            Item_3 = new(
              "2PL", a = 0.9021, b = -0.2116, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_3", content = NULL, misc = list(key = "A")
            ),
            Item_4 = new(
              "2PL", a = 0.6313, b = 0.2425, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_4", content = NULL, misc = list(key = "D")
            ),
            Item_5 = new(
              "2PL", a = 1.0508, b = -0.0725, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_5", content = NULL, misc = list(key = "C")
            ),
            Item_6 = new(
              "2PL", a = 0.9248, b = 0.3227, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_6", content = NULL, misc = list(key = "D")
            ),
            Item_7 = new(
              "2PL", a = 0.7116, b = 0.6916, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_7", content = NULL, misc = list(key = "D")
            ),
            Item_8 = new(
              "2PL", a = 0.9361, b = 1.0015, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_8", content = NULL, misc = list(key = "A")
            ),
            Item_9 = new(
              "2PL", a = 1.3194, b = 0.0735, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_9", content = NULL, misc = list(key = "D")
            ),
            Item_10 = new(
              "2PL", a = 0.6909, b = -1.8462, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_10", content = NULL, misc = list(key = "A")
            ),
            Item_11 = new(
              "2PL", a = 1.1087, b = -0.032, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_11", content = NULL, misc = list(key = "D")
            ),
            Item_12 = new(
              "2PL", a = 0.8828, b = -0.1818, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_12", content = NULL, misc = list(key = "C")
            ),
            Item_13 = new(
              "2PL", a = 1.1485, b = 0.7319, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_13", content = NULL, misc = list(key = "B")
            ),
            Item_14 = new(
              "2PL", a = 0.9496, b = -0.1067, D = 1, se_a = NULL, se_b = NULL,
              item_id = "Item_14", content = NULL, misc = list(key = "A")
            )
          ),
          misc = NULL
        )

  run_with_seed(455, {
    theta <- rnorm(1000)
    resp <- sim_resp(ip = ip, theta = theta, prop_missing = .2)
  })

  fit <- item_fit(ip = ip, resp = resp, theta = theta, type = "G2")
  expect_equal(fit$G2,
               c(5.374936, 6.097998, 11.581648, 5.234175, 17.79125, 2.45495,
                 12.68328, 9.198217, 9.60408, 17.105162, 23.465291, 5.467533,
                 9.918014, 6.321263),
               tolerance = 1e-6)


  # -------------------------------------------------------------------------- #
  # Check G2 for some polytomous items.
  ip <- new(
    "Itempool", item_list = list(
      Item_1 = new(
        "GPCM", a = 1.4278, b = c(-2.0908, -1.6089, -0.786, 0.8881),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_1", content = NULL,
        misc = NULL
      ),
      Item_2 = new(
        "GPCM", a = 0.986, b = c(-1.4155, 0.479, 1.1432),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_2", content = NULL,
        misc = NULL
      ),
      Item_3 = new(
        "GPCM", a = 0.759, b = c(-1.5782, -0.7862),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_3", content = NULL,
        misc = NULL
      ),
      Item_4 = new(
        "GPCM", a = 0.6119, b = c(-0.8913, -0.2102, 0.2706),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_4", content = NULL,
        misc = NULL
      ),
      Item_5 = new(
        "GPCM", a = 0.4667, b = c(-0.9122, -0.4907, 0.1223, 0.7481),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_5", content = NULL,
        misc = NULL
      ),
      Item_6 = new(
        "GPCM", a = 1.4921, b = c(-1.2259, -0.5096, -0.1937, 0.1084),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_6", content = NULL,
        misc = NULL
      ),
      Item_7 = new(
        "GPCM", a = 0.9372, b = c(-1.1481, -0.3764, 0.1241, 1.1742),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_7", content = NULL,
        misc = NULL
      ),
      Item_8 = new(
        "GPCM", a = 0.6592, b = c(0.5744, 0.9792),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_8", content = NULL,
        misc = NULL
      ),
      Item_9 = new(
        "GPCM", a = 0.9205, b = c(-0.218, 0.0956, 0.4805, 1.2106),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_9", content = NULL,
        misc = NULL
      ),
      Item_10 = new(
        "GPCM", a = 1.238, b = c(-0.321, 0.5795, 1.6335),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_10", content = NULL,
        misc = NULL
      ),
      Item_11 = new(
        "GPCM", a = 0.8476, b = c(-0.6155, -0.2682, 0.8473),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_11", content = NULL,
        misc = NULL
      ),
      Item_12 = new(
        "GPCM", a = 1.1982, b = c(-1.0571, -0.4545, 0.0052),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_12", content = NULL,
        misc = NULL
      ),
      Item_13 = new(
        "GPCM", a = 2.7269, b = c(-0.845, -0.3339, 0.1629),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_13", content = NULL,
        misc = NULL
      ),
      Item_14 = new(
        "GPCM", a = 0.9321, b = c(-0.8051, -0.3629, 0.9211, 1.7103),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_14", content = NULL,
        misc = NULL
      ),
      Item_15 = new(
        "GPCM", a = 0.6109, b = c(-0.6427, 0.1886, 0.6676, 1.0998),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_15", content = NULL,
        misc = NULL
      ),
      Item_16 = new(
        "GPCM", a = 0.8415, b = c(-1.0866, -0.2141, 0.334),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_16", content = NULL,
        misc = NULL
      ),
      Item_17 = new(
        "GPCM", a = 0.9544, b = c(-0.3261, 0.5474),
        D = 1, se_a = NULL, se_b = NULL, item_id = "Item_17", content = NULL,
        misc = NULL
      )
    ),
    misc = NULL
  )

  run_with_seed(461, {
    theta <- rnorm(8000)
    resp <- sim_resp(ip = ip, theta = theta, prop_missing = .2)
  })
  fit <- item_fit(ip = ip, resp = resp, theta = theta, type = "G2")
  expect_equal(fit$G2,
               c(39.718845, 31.872205, 15.142727, 19.165699, 37.19971,
                 53.463734, 52.500698, 19.06835, 96.375508, 35.974711,
                 37.877789, 40.537522, 41.732632, 39.201129, 48.582821,
                 33.614737, 22.640951),
               tolerance = 1e-6)

})

