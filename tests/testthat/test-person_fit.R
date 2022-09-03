
test_that("person_fit - Itempool", {
  ip <- generate_ip(model = "2PL")
  theta <- rnorm(1)
  resp <- sim_resp(ip = ip, theta = theta)
  expect_type(person_fit(ip = ip, resp = resp, theta = theta), "double")
})


test_that("person_fit - Testlet", {
  t1 <- testlet(generate_ip(n = 3))
  theta <- rnorm(1)
  resp <- sim_resp(ip = t1, theta = theta)
  expect_type(person_fit(ip = t1, resp = resp, theta = theta), "double")

})


# This method is only for one theta.
temp_lz_dich <- function(ip, resp, theta){
    # If the Model is one of the following: "1PM" "2PM" "3PM" "4PM"
    if (all(ip$model %in% irt:::UNIDIM_DICHO_MODELS)) {
      stopifnot(length(theta) == 1)
      # Calculate l_0:
      l0 <- sum(resp_loglik(ip = ip, resp = resp, theta = theta),
                na.rm = TRUE)
      # Expected value of L_0
      p <- prob(ip = ip, theta = theta)
      q <- p[, 1]
      p <- p[, 2]
      el0 <- sum(p * log(p) + q * log(q), na.rm = TRUE)
      # Variance of L_0
      vl0 <- sum(p * q * log(p/q)^2, na.rm = TRUE)
      return((l0 - el0) / sqrt(vl0))
    } else
      stop("Currently this method is only available for dichotomous IRT ",
           "models.")
}



###############################################################################@
############################# person_fit (Response_set) @#######################
###############################################################################@

test_that("person_fit (Response_set)", {
  n_item <- sample(11:31, 1)
  n_theta <- sample(7:21, 1)
  ip <- generate_ip(n = n_item)
  theta <- rnorm(n_theta)
  resp_set <- sim_resp(ip = ip, theta = theta, prop_missing = .2,
                       output = "response_set")
  # i <- sample(1:n_theta, 1)
  # r1 <- resp_set[[i]]
  pf <- person_fit(resp = resp_set, theta = theta, ip = ip, type = "lz")
  expect_identical(length(pf), n_theta)
  expect_identical(names(pf), resp_set$examinee_id)

  # -------------------------------------------------------------------------- #
  # When examinee_id is integer the function works as it should
  r1 <- resp_set[[1]]
  r1@examinee_id <- 123L
  pf <- person_fit(resp = r1, theta = 0.1, ip = ip)
  pf <- person_fit(resp = r1, theta = setNames(0.1, "abc"), ip = ip)
  expect_identical(names(pf), "123")

})

###############################################################################@
############################# lz_response_set_cpp @#############################
###############################################################################@

test_that("lz_response_set_cpp()", {

  # -------------------------------------------------------------------------- #
  # Test only 3PL items
  ipdf <- structure(list(
    a = c(
      0.4674, 1.3744, 0.6807, 0.8943, 0.9022, 0.8401, 0.7397,
      1.59, 0.9259, 1.4537, 0.8536, 1.289, 0.759, 0.54, 1.3468
    ),
    b = c(
      -1.7639, -1.9585, -0.1521, 0.5134, 0.7686, -1.0284,
      -0.0713, -1.114, 1.4822, 1.1565, 0.6938, 0.1096, -0.7108,
      -0.4435, -0.411
    ),
    c = c(
      0.0461, 0.0062, 0.0541, 0.2483, 0.1469, 0.2886, 0.2259,
      0.0186, 0.0346, 0.1189, 0.0105, 0.2858, 0.1944, 0.1919,
      0.0985
    )),
    class = "data.frame", row.names = c(NA, -15L)
    )
  theta <- c(0.447, -1.221, 0.625, -2.618, -1.068, -0.159, -0.821, 0.363)
  resp_matrix <- structure(
    c(1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 1L,
      1L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 0L,
      0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
      1L, 0L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L,
      1L, 1L, 1L, 0L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 0L,
      0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 0L,
      0L, 1L, 0L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L,
      1L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 1L, 1L, 1L),
    .Dim = c(8L, 15L),
    .Dimnames = list(c("Examinee-1", "Examinee-2", "Examinee-3", "Examinee-4",
                       "Examinee-5", "Examinee-6", "Examinee-7", "Examinee-8"),
                     c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5",
                       "Item_6",  "Item_7", "Item_8", "Item_9", "Item_10",
                       "Item_11", "Item_12", "Item_13", "Item_14", "Item_15")))
  expected <- c(-0.3157, -0.4461, -0.4819, 0.6219, 0.9358, 0.5455, -0.8908,
                0.4967)


  ip <- itempool(ipdf, D = 1)
  resp <- response_set(resp_matrix, ip = ip)
  observed <- person_fit(resp, ip = ip, theta = theta)
  expect_identical(observed, expected, tolerance = 1e-3, ignore_attr = TRUE)

  i <- sample(1:length(resp), 1)
  expected <- temp_lz_dich(ip = ip, resp = resp[[i]], theta = theta[i])
  expect_identical(observed[i], expected, tolerance = 1e-4, ignore_attr = TRUE)

  # -------------------------------------------------------------------------- #
  # Only 3PL items
  ip_matrix <- structure(
    c(1.119, 0.9265, 1.902, 0.7841, 1.5802, 1.0171, 0.8221,
      1.3942, 1.237, 1.3435, 0.8869, 0.7607, 1.3243, 0.8663, 0.6661,
      0.904, -1.6983, -1.8438, 2.0367, -1.2217, -1.0677, -1.8623, 2.2303,
      -0.275, -0.5281, -0.5502, 0.1153, -0.3603, -1.7341, 0.3788, 0.1739,
      0.0491, 0.0548, 0.2031, 0.1803, 0.2321, 0.1287, 0.1328, 0.1092,
      0.0973, 0.2475, 0.0799, 0.0988, 0.2245, 0.2378),
    .Dim = c(15L, 3L),
    .Dimnames = list(c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5",
                       "Item_6", "Item_7", "Item_8", "Item_9", "Item_10",
                       "Item_11", "Item_12", "Item_13", "Item_14", "Item_15"),
                     c("a", "b", "c")))
  resp_matrix <- structure(
    c(1L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L,
      1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L,
      1L, 1L, 1L, 0L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 1L,
      1L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 0L, 1L, 0L, 1L, 1L, 1L, 1L,
      1L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 1L, 1L, 0L, 0L, 1L, 0L,
      1L, 1L, 0L, 1L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 0L, 1L, 1L,
      1L, 0L, 1L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 1L, 1L, 0L, 1L, 0L, 0L, 1L, 1L,
      0L, 1L, 0L, 1L, 1L, 1L),
    .Dim = c(10L, 15L),
    .Dimnames = list(c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9",
                       "S10"),
                     c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5",
                       "Item_6", "Item_7", "Item_8", "Item_9", "Item_10",
                       "Item_11", "Item_12", "Item_13", "Item_14", "Item_15")))
  theta <- c(-0.554, 0.524, -1.21, -0.315, 0.852, -0.709, 0.541, -0.163,
             1.548, -1.302)

  ip <- itempool(ip_matrix, model = "3PL", D = 1)
  resp <- response_set(resp_matrix, ip = ip)
  observed <- person_fit(resp = resp, theta = theta, ip = ip)

  expected <- c(-0.4621, -0.4949, -0.4429, 1.0483, -1.7151, 0.3985, 0.7867,
                0.6302, -0.3231, -0.3932)

  expect_identical(observed, expected, tolerance = 1e-4, ignore_attr = TRUE)

  i <- sample(1:length(resp), 1)
  expected <- temp_lz_dich(ip = ip, resp = resp[[i]], theta = theta[i])
  expect_identical(observed[i], expected, tolerance = 1e-4, ignore_attr = TRUE)


  # -------------------------------------------------------------------------- #
  # Test only GPCM items
  ipdf <- structure(list(a = c(0.9485, 1.7504, 1.4159, 0.728, 1.4252),
                         b1 = c(-0.9261, -0.0526, -0.9284, -1.3163, -1.3292),
                         b2 = c(0.0062, 0.4253, 0.7774, -0.3523, -0.0219),
                         b3 = c(1.6774, 0.8239, 2.2602, 0.5002, 0.6511)
                         ),
                    class = "data.frame", row.names = c(NA,  -5L))

  resp_matrix <- structure(
    c(0L, 1L, 0L, 0L, 2L, 0L, 1L, 1L, 1L, 0L, 3L, 3L, 0L, 2L, 0L),
    .Dim = c(3L, 5L),
    .Dimnames = list(c("S1", "S2", "S3"),
                     c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5")))

  theta <- c(-1.792, 0.304, -0.318)

  # setup ip and resp
  ip <- itempool(ipdf, D = 1, model = "GPCM")
  resp <- response_set(resp_matrix, ip = ip)

  expected <- c(0.2006049,  0.9999808, -0.6475260)
  observed <- person_fit(resp, ip = ip, theta = theta)
  expect_identical(observed, expected, tolerance = 1e-6, ignore_attr = TRUE)

  # -------------------------------------------------------------------------- #
  # Test only GPCM items
  ipdf <- structure(list(
    item_id = c("Item_1", "Item_2", "Item_3", "Item_4",
           "Item_5", "Item_6", "Item_7", "Item_8", "Item_9", "Item_10",
           "Item_11", "Item_12", "Item_13", "Item_14", "Item_15", "Item_16",
           "Item_17", "Item_18", "Item_19", "Item_20"),
    model = c("GPCM", "GPCM", "GPCM", "GPCM", "GPCM", "GPCM", "GPCM", "GPCM",
              "GPCM", "GPCM", "GPCM", "GPCM", "GPCM", "GPCM", "GPCM", "GPCM",
              "GPCM", "GPCM", "GPCM", "GPCM"),
    a = c(0.951, 1.0003, 0.8223, 1.5294, 0.7299, 1.1409, 0.7512, 1.0778,
          1.1736, 0.6207, 0.8238, 1.3304, 1.1993, 1.0658, 0.6998, 1.0936,
          0.6924, 0.8607, 0.9087, 0.747),
    b1 = c(-1.0497, -0.316, -0.6616, -2.0132, -0.1359, -1.42, -0.2045,
           -0.8014, -1.3198, -0.8311, -1.1778, -0.7591, -1.7761, -1.6126,
           -0.7914, -0.1781, -0.5525, -1.1118, -0.6876, -1.2257),
    b2 = c(-0.4901, 0.1982, -0.1916, -0.47, 0.2008, -0.7761, 0.2256,
           -0.097, 0.2948, 0.0661, -0.3442, -0.0807, -0.1468, -0.1452,
           0.0889,  0.4287, -0.0542, -0.3149, 0.151, -0.3314),
    b3 = c(0.0374, 1.0318, 0.2175, 1.1988, 0.8552, 0.766, 1.5736, 0.3385,
           0.8282, 0.7, 0.0974, 1.6949, 0.3264, 1.0384, 0.7518, 1.2706,
           0.5189, 1.0474, 0.5169, 0.7345),
    D = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
    class = "data.frame", row.names = c(NA, -20L))

  resp_matrix <- structure(
    c(3L, 3L, 0L, 3L, 2L, 2L, 0L, 2L, 0L, 1L, 3L, 2L, 1L, 3L, 1L, 0L, 0L,
      0L, 1L, 0L, 2L, 3L, 0L, 2L, 0L, 1L, 0L, 1L, 1L, 0L, 2L, 1L, 3L, 2L,
      1L, 2L, 0L, 1L, 2L, 0L, 1L, 3L, 0L, 3L, 0L, 2L, 0L, 0L, 2L, 0L, 2L,
      3L, 2L, 2L, 2L, 2L, 0L, 2L, 1L, 1L, 2L, 3L, 0L, 3L, 1L, 3L, 0L, 1L,
      1L, 0L, 3L, 3L, 1L, 3L, 0L, 3L, 1L, 0L, 1L, 1L, 2L, 3L, 2L, 2L, 1L,
      0L, 0L, 0L, 1L, 1L, 2L, 3L, 0L, 3L, 2L, 1L, 0L, 1L, 2L, 2L, 0L, 2L,
      0L, 3L, 1L, 2L, 0L, 1L, 0L, 0L, 0L, 3L, 0L, 2L, 1L, 1L, 0L, 0L, 2L,
      0L, 2L, 3L, 1L, 2L, 1L, 2L, 1L, 1L, 3L, 1L, 2L, 3L, 1L, 3L, 2L, 2L,
      0L, 1L, 1L, 0L, 1L, 3L, 0L, 2L, 0L, 3L, 2L, 0L, 1L, 0L, 3L, 3L, 0L,
      0L, 0L, 2L, 0L, 0L, 2L, 0L, 3L, 1L, 2L, 2L, 1L, 0L, 0L, 1L, 0L, 0L,
      2L, 3L, 1L, 2L, 0L, 2L, 0L, 1L, 3L, 1L, 1L, 3L, 0L, 1L, 0L, 2L, 0L,
      1L, 1L, 1L, 3L, 3L, 1L, 2L, 2L, 2L, 1L, 1L, 0L, 1L),
    .Dim = c(10L, 20L),
    .Dimnames = list(c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9",
                       "S10"),
                     c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5",
                       "Item_6", "Item_7", "Item_8", "Item_9", "Item_10",
                       "Item_11", "Item_12", "Item_13", "Item_14", "Item_15",
                       "Item_16", "Item_17", "Item_18", "Item_19", "Item_20")))

  theta <- c(0.144, 2.156, -0.651, 1.319, -0.923, 0.011, -1.701, -0.511,
             -0.545, -1.331)

  ip <- itempool(ipdf)
  resp <- response_set(resp_matrix, ip = ip)

  expected <- c(-0.5208906, -1.178065, 0.1441855, -0.862519, 1.127305,
                0.023183, 1.222306, 2.074301, -1.126762, 1.117917)
  observed <- person_fit(resp = resp, ip = ip, theta = theta)

  expect_identical(expected, observed, tolerance = 1e-6, ignore_attr = TRUE)


  # -------------------------------------------------------------------------- #
  # Test only PCM items
  ipdf <- structure(list(
    item_id = c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5", "Item_6",
           "Item_7", "Item_8"),
    model = c("PCM", "PCM", "PCM", "PCM", "PCM", "PCM", "PCM", "PCM"),
    b1 = c(-1.1279, -1.1637, -0.8247, 0.1733, -0.8417, -1.1277, 0.0099,
           -0.3035),
    b2 = c(-0.5228, 0.1498, 0.2688, 0.7608, -0.2445, -0.5669, 1.1301, 0.4956),
    b3 = c(0.91, 0.7041, 0.853, 2.8923, 1.2979, 1.0334, 2.2426, 1.0022)),
    class = "data.frame", row.names = c(NA, -8L))
  resp_matrix <- structure(
    c(2L, 0L, 3L, 2L, 0L, 2L, 3L, 2L, 1L, 1L, 3L, 1L, 2L, 2L, 1L, 2L, 3L, 1L,
      2L, 0L, 2L, 0L, 3L, 0L, 0L, 1L, 3L, 2L, 0L, 2L, 1L, 1L, 2L, 0L, 0L, 1L,
      1L, 3L, 0L, 2L, 3L, 2L, 3L, 1L, 2L, 2L, 2L, 1L, 2L, 0L, 3L, 2L, 2L, 0L,
      0L, 2L, 2L, 2L, 2L, 2L, 2L, 0L, 2L, 1L, 0L, 1L, 2L, 1L, 0L, 1L, 2L, 1L,
      3L, 2L, 0L, 2L, 3L, 3L, 1L, 0L),
    .Dim = c(10L, 8L),
    .Dimnames = list(c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9",
                       "S10"),
                     c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5",
                       "Item_6", "Item_7", "Item_8")))
  theta <- c(0.718, -0.4, 1.824, -0.872, -0.748, -0.033, 1.348, 1.13, 0.112,
             -0.813)

  ip <- itempool(ipdf)
  resp <- response_set(resp_matrix, ip = ip)

  expected <- c(0.806365, 0.44845, 0.648081, -1.218025, 0.782512, 1.382906,
                1.102423, -1.675802, 0.786487, -1.614959)
  observed <- person_fit(resp = resp, ip = ip, theta = theta)
  expect_identical(expected, observed, tolerance = 1e-6, ignore_attr = TRUE)

  # -------------------------------------------------------------------------- #
  # Test only GRM items
  ipdf <- structure(list(
    item_id = c("Item_1", "Item_2", "Item_3", "Item_4",
           "Item_5", "Item_6", "Item_7", "Item_8", "Item_9", "Item_10",
           "Item_11", "Item_12", "Item_13"),
    model = c("GRM", "GRM", "GRM", "GRM", "GRM", "GRM", "GRM", "GRM", "GRM",
              "GRM", "GRM", "GRM", "GRM"),
    a = c(1.0143, 1.0165, 0.919, 2.0675, 0.8411, 0.6635, 0.9454, 1.2201,
          1.3618, 0.6215, 1.2404, 0.8362, 0.9711),
    b1 = c(-1.8596, -2.1676, -0.973, -1.1662, -1.1197, -1.2024, -2.0193,
           -1.4599, -0.0322, -2.6521, -0.1579, -0.9597, -0.8969),
    b2 = c(-0.9396, -0.8096, -0.2769, -0.1685, -0.7051, -0.8435, -0.9611,
           -0.5855, 0.2823, -0.4774, 0.2839, -0.4511, -0.4198),
    b3 = c(0.4373, -0.0616, 0.5534, 0.2108, 0.361, 0.384, 0.0065, 0.0577,
           0.8541, -0.033, 0.5949, -0.0612, 0.0691),
    b4 = c(0.9454, 0.6335, 1.696, 0.5338, 0.84, 1.8057, 0.5994, 0.6579,
           1.2681, 0.4999, 1.2898, 1.6624, 0.8937),
    D = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
    class = "data.frame", row.names = c(NA, -13L))
  resp_matrix <- structure(
    c(2L, 1L, 3L, 2L, 4L, 4L, 1L, 1L, 2L, 2L, 4L, 1L, 4L, 0L, 4L, 2L, 3L, 3L,
      4L, 3L, 1L, 0L, 1L, 1L, 4L, 0L, 0L, 0L, 4L, 2L, 0L, 1L, 4L, 0L, 4L, 1L,
      1L, 2L, 4L, 1L, 4L, 0L, 0L, 1L, 1L, 2L, 0L, 2L, 4L, 0L, 2L, 3L, 4L, 0L,
      4L, 3L, 2L, 0L, 4L, 1L, 0L, 1L, 4L, 0L, 4L, 4L, 1L, 2L, 2L, 4L, 2L, 0L,
      4L, 0L, 4L, 4L, 1L, 3L, 3L, 1L, 0L, 0L, 2L, 0L, 4L, 3L, 0L, 4L, 2L, 1L,
      2L, 4L, 4L, 0L, 4L, 1L, 0L, 0L, 4L, 1L, 4L, 0L, 0L, 0L, 4L, 0L, 1L, 3L,
      4L, 2L, 0L, 0L, 4L, 1L, 4L, 3L, 4L, 4L, 2L, 1L, 4L, 2L, 4L, 3L, 4L, 4L,
      1L, 0L, 4L, 2L),
    .Dim = c(10L, 13L),
    .Dimnames = list(c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9",
                       "S10"),
                     c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5",
                       "Item_6", "Item_7", "Item_8", "Item_9", "Item_10",
                       "Item_11", "Item_12", "Item_13")))
  theta <- c(-0.12, -1.697, 0.576, -1.213, 2.834, 0.359, -1.441, -0.285,
             1.755, -0.236)

  ip <- itempool(ipdf)
  resp <- response_set(resp_matrix, ip = ip)

  expected <- c(-0.603581, -0.017428, 1.487841, -0.036934, 0.311089, 0.922378,
                -1.526852, -1.050703, -0.720228, -1.960701)

  observed <- person_fit(resp = resp, ip = ip, theta = theta)
  expect_identical(expected, observed, tolerance = 1e-6, ignore_attr = TRUE)

  # -------------------------------------------------------------------------- #
  # A mixture of GPCM and 3PL parameters
  item_pars <- structure(list(
    item_id = c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5", "Item_6"),
    model = c("3PL", "3PL", "3PL", "GPCM", "GPCM", "GPCM"),
    a = c(1.4872, 0.9237, 0.8819, 0.9474, 1.0171, 0.704),
    b1 = c(NA, NA, NA, -0.3453, -0.083, -0.1216),
    b2 = c(NA, NA, NA, 0.4991, 0.6567, 0.3755),
    b3 = c(NA, NA, NA, NA, 1.1107, 0.681),
    b4 = c(NA, NA, NA, NA, NA, 1.1292),
    D = c(1.7, 1.7,  1.7, 1.7, 1.7, 1.7),
    b = c(1.2386, -0.1884, -0.3594, NA,  NA, NA),
    c = c(0.1999, 0.2254, 0.1036, NA, NA, NA)),
    class = "data.frame", row.names = c(NA,  -6L))
  resp <- structure(
    c(1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, NA, 1L, 0L, 0L, NA,
      2L, 2L, 0L, 0L, 0L, 2L, 3L, NA, 2L, 2L, NA, 4L),
    .Dim = 5:6,
    .Dimnames = list(c("S1", "S2", "S3", "S4", "S5"),
                     c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5",
                       "Item_6")))
  theta <- -2:2

  # setup arguments
  ip <- itempool(item_pars)
  resp_set <- response_set(resp, ip = ip)

  expected <- c(-1.387642, -0.981382, 0.062959, 0.799129, -0.480936)
  observed <- person_fit(resp = resp_set, ip = ip, theta = theta)

  expect_identical(expected, observed, tolerance = 1e-4, ignore_attr = TRUE)
  expect_identical(names(observed), rownames(resp))


  # -------------------------------------------------------------------------- #
  # person_fit function can accept a matrix as "resp" and return the correct
  # lz values



  # -------------------------------------------------------------------------- #
  # person_fit function can accept a "Response" object as "resp" and return
  # the correct lz values



  # -------------------------------------------------------------------------- #
  # person_fit function can return correct names for lz when some resp have
  # examinee_id's some don't
  resp_matrix <- structure(
    c(1L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, NA, 1L, 0L, 0L, NA),
    .Dim = c(3, 6),
    .Dimnames = list(c("S1", "S2", "S3"),
                     c("Item_1", "Item_2", "Item_3", "Item_4", "Item_5",
                       "Item_6")))
  theta <- rnorm(3)
  ip <- generate_ip(n = 6)
  resp <- response_set(resp_matrix)
  # Set one of the examinee_id's to NULL
  resp@response_list[[1]]@examinee_id <- NULL

  lz <- person_fit(resp = resp, ip = ip, theta = theta)
  expect_identical(names(lz), c(NA, "S2", "S3"))

  # -------------------------------------------------------------------------- #
  # person_fit function can return correct names for lz when none of the resp
  # have examinee_id's
  resp@response_list[[1]]@examinee_id <- NULL
  resp@response_list[[2]]@examinee_id <- NULL
  resp@response_list[[3]]@examinee_id <- NULL
  theta <- rnorm(3)
  ip <- generate_ip(n = 6)
  lz <- person_fit(resp = resp, ip = ip, theta = theta)
  expect_null(names(lz))
})



###############################################################################@
############################# cusum_single #####################################
###############################################################################@

test_that("cusum_single", {

  # Example from Table 1 (p.4) of Yu and Cheng (2020):
  dt <- data.frame(
    item = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
             20),
    a = c(0.976, 0.973, 0.871, 0.768, 0.94, 1.109, 1.063, 0.888, 0.648, 0.733,
          0.8, 0.823, 0.611, 0.965, 1.052, 0.937, 0.894, 0.72, 0.686, 0.608),
    b = c(-0.693, 0.6, -0.607, -0.637, -1.095, -0.202, -0.679, 0.058, -0.822,
          -0.768, -0.737, -1.158, -0.294, -0.856, -0.833, -0.613, -0.151,
          -0.614, -0.07, -0.806),
    c = c( 0.371, 0.224, 0.159, 0.377, 0.159, 0.146, 0.181, 0.251, 0.179, 0.214,
           0.312, 0.224, 0.246, 0.225, 0.155, 0.166, 0.456, 0.327, 0.112,
           0.169),
    u = c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1),
    p = c(0.163, 0.419, 0.741, 0.8, 0.865, 0.63, 0.798, 0.592, 0.752, 0.77,
          0.804, 0.138, 0.669, 0.835, 0.83, 0.756, 0.747, 0.227, 0.442, 0.737),
    T = c(-0.008, 0.029, 0.013, 0.01, 0.007, 0.019, 0.01, 0.02, 0.012, 0.012,
          0.01, -0.007, 0.017, 0.008, 0.009, 0.012, 0.013, -0.011, -0.022,
          0.013),
    Cp = c(0, 0.029, 0.042, 0.052, 0.059, 0.077, 0.087, 0.108, 0.12, 0.132,
           0.141, 0.135, 0.151, 0.159, 0.168, 0.18, 0.193, 0.181, 0.159, 0.173),
    Cn = c(-0.008, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.007, 0, 0, 0, 0, 0, -0.011,
           -0.033, -0.02)
    )

  ip <- itempool(dt[, c("a", "b", "c")], D = 1.7)
  resp <- dt$u
  resp_set <- response(resp)
  theta <- -0.06

  observed <- cusum_single(ip = ip, resp = resp, method = "T5", theta = theta)
  expect_equal(round(observed$Cp, 3), dt$Cp, tolerance = 1e-2)
  expect_equal(round(observed$Cn, 3), dt$Cn, tolerance = 1e-3)

  observed <- cusum_single(ip = ip, resp = resp_set, method = "T5", theta = theta)
  expect_equal(round(observed$Cp, 3), dt$Cp, tolerance = 1e-2)
  expect_equal(round(observed$Cn, 3), dt$Cn, tolerance = 1e-3)

  # -------------------------------------------------------------------------- #

})
