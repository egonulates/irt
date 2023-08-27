
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% resp_lik %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


###############################################################################@
############################# resp_lik_bare_item_cpp ###########################
###############################################################################@

test_that("resp_lik_bare_item_cpp", {
  ip <- generate_item("3PL")
  theta <- rnorm(1)
  p <- unname(prob(ip = ip, theta = theta)[, 2])
  rlik <- resp_lik_bare_item_cpp(resp = 1, theta = theta, item = ip)
  expect_identical(round(p, 10), round(rlik, 10))
  rlik <- resp_lik_bare_item_cpp(resp = 0, theta = theta, item = ip)
  expect_equal(1 - p, rlik, tolerance = 1e-10)
})


###############################################################################@
############################# resp_lik (Item) @#################################
###############################################################################@

test_that("resp_lik - Item", {
  ##  Single theta
  ip <- item(a = rlnorm(1, 0, .3), b = rnorm(1), c = 0)
  theta <- rnorm(1)
  expect_identical(resp_lik(resp = 1, ip = ip, theta = theta),
                   unname(prob(ip = ip, theta = theta)[, 2]))
  expect_identical(resp_lik(resp = 0, ip = ip, theta = theta),
                   1 - unname(prob(ip = ip, theta = theta)[, 2]))
  expect_identical(resp_lik(resp = 0, ip = ip, theta = theta),
                   unname(prob(ip = ip, theta = theta)[, 1]))

  # ---------------------------------------------------------------------------#
  ##  GRM
  ip <- item(a = rlnorm(1, 0, .3), b = sort(rnorm(sample(2:7, 1))),
             model = "GRM")
  theta <- rnorm(1)
  resp <- sample(0L:max_score(ip), 1)
  expect_identical(resp_lik(ip = ip, resp = resp, theta = theta),
                    prob(ip = ip, theta = theta)[resp + 1])

  # ---------------------------------------------------------------------------#
  ##  GPCM
  ip <- item(a = rlnorm(1, 0, .3), b = sort(rnorm(sample(2:7, 1))),
             model = "GPCM")
  theta <- rnorm(1)
  resp <- sample(0L:max_score(ip), 1)
  expect_equal(resp_lik(ip = ip, resp = resp, theta = theta),
               prob(ip = ip, theta = theta)[resp + 1], tolerance = 1e-8)


  # ---------------------------------------------------------------------------#
  ##  GPCM2
  max_scr <- sample(2L:7L, 1)
  ip <- generate_item(model = "GPCM2", n_categories = max_scr + 1)
  theta <- rnorm(1)
  resp <- sample(0L:max_score(ip), 1)
  expect_equal(resp_lik(ip = ip, resp = resp, theta = theta),
               prob(ip = ip, theta = theta)[resp + 1], tolerance = 1e-8)

})

###############################################################################@
############################# resp_lik (Itempool) @#############################
###############################################################################@

test_that("resp_lik - Itempool", {
  ip <- generate_ip(n = 5)
  theta <- rnorm(3)
  resp_set <- generate_resp_set(ip = ip, theta = theta, prop_missing = .2)
  resp_matrix <- as.matrix(resp_set, ip = ip)

  expected <- resp_lik(ip = ip, resp = resp_matrix, theta = theta)
  observed <- resp_lik(ip = ip, resp = resp_set, theta = theta)

  # Function works fine with Response_set object
  expect_identical(expected, observed)

  # Function works fine with Response object
  observed <- resp_lik(ip = ip, resp = resp_set[[1]], theta = theta[1])
  expect_identical(expected[1], observed)
})

###############################################################################@
############################# resp_lik (Testlet) @##############################
###############################################################################@

test_that("resp_lik - Testlet", {
  ip <- generate_ip(model = sample(c("3PL", "GRM"), 8, T))
  testlet <- testlet(ip)

  theta <- rnorm(1)
  resp <- sim_resp(ip = ip, theta = theta)

  expect_identical(irt:::resp_lik_bare_itempool_cpp(resp = resp, theta = theta,
                                                ip = ip),
               irt:::resp_lik_bare_testlet_cpp(resp = resp, theta = theta,
                                               testlet = testlet))
})


###############################################################################@
############################# resp_lik_response_cpp @###########################
###############################################################################@


test_that("resp_lik_response_cpp", {
  ip <- generate_ip(model = sample(c("3PL", "GRM"), 8, T))
  theta <- rnorm(1)
  resp <- generate_resp(ip = ip, theta = theta)[[1]]
  p <- prob(ip = ip, theta = theta)
  expected <- prod(sapply(1:length(resp),
                          function(i) p[i, (resp$score + 1)[i]]))
  observed <- irt:::resp_lik_response_cpp(theta = theta, resp = resp, ip = ip)
  expect_identical(expected, observed, tolerance = 1e-8)

  # ---------------------------------------------------------------------------#
  # The likelihood of testlet and standalone items are the same
  ip <- c(generate_testlet(n = sample(2:6, 1), item_id_preamble = "t1"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t2"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t3"),
          generate_ip(n = sample(6:12, 1)))
  ip_standalone <- itempool(irt:::flatten_itempool_cpp(ip))
  theta <- rnorm(1)
  resp <- generate_resp(ip = ip, theta = theta, prop_missing = 0)[[1]]
  resp_vector <- resp$score

  observed_resp <- irt:::resp_lik_response_cpp(theta = theta, resp = resp, ip = ip)
  observed_resp_vec <- irt:::resp_lik_bare_itempool_cpp(
    resp = resp_vector, theta = theta, ip = ip)
  observed_resp_vec_sa <- irt:::resp_lik_bare_itempool_cpp(
    resp = resp_vector, theta = theta, ip = ip_standalone)
  expect_identical(observed_resp, observed_resp_vec, tolerance = 1e-10)
  expect_identical(observed_resp, observed_resp_vec_sa, tolerance = 1e-10)
  expect_identical(observed_resp_vec_sa, observed_resp_vec)


  # ---------------------------------------------------------------------------#
  # The likelihood of testlet and standalone items are the same
  ip <- c(generate_testlet(n = sample(2:6, 1), item_id_preamble = "t1"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t2"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t3"),
          generate_ip(n = sample(6:12, 1)))
  ip_standalone <- itempool(irt:::flatten_itempool_cpp(ip))
  theta <- rnorm(1)
  resp_set <- generate_resp_set(ip = ip, theta = theta, prop_missing = .3)
  resp_vector <- as.matrix(resp_set, ip = ip)[1, ]
  resp <- resp_set[[1]]

  observed_resp <- irt:::resp_lik_response_cpp(theta = theta, resp = resp, ip = ip)
  observed_resp_vec <- irt:::resp_lik_bare_itempool_cpp(
    resp = resp_vector, theta = theta, ip = ip)
  observed_resp_vec_sa <- irt:::resp_lik_bare_itempool_cpp(
    resp = resp_vector, theta = theta, ip = ip_standalone)
  expect_identical(observed_resp, observed_resp_vec, tolerance = 1e-10)
  expect_identical(observed_resp, observed_resp_vec_sa, tolerance = 1e-10)
  expect_identical(observed_resp_vec_sa, observed_resp_vec)


  # ---------------------------------------------------------------------------#
  # The likelihood of testlet and standalone items are the same
  ip <- c(generate_testlet(n = sample(2:6, 1), item_id_preamble = "t1"),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t2"),
          generate_ip(n = sample(6:12, 1)),
          generate_testlet(n = sample(2:6, 1), item_id_preamble = "t3")
          )
  ip_standalone <- itempool(irt:::flatten_itempool_cpp(ip))
  true_theta <- rnorm(20)
  resp <- sim_resp(ip = ip, theta = true_theta, prop_missing = .3)
  i <- sample(nrow(resp), 1)
  resp_vector <- resp[i, ]
  resp_set <- response_set(resp, ip = ip)[[i]]
  resp_set_sa <- response_set(resp, ip = ip_standalone)[[i]]
  resp_vector_short <- resp_vector[!is.na(resp_vector)]
  ip_standalone_short <- ip_standalone[names(resp_vector_short)]
  resp_set_sa_short <- response(score = resp_vector_short,
                                item_id = names(resp_vector_short))

  theta <- rnorm(1)

  temp <- prob(ip = ip_standalone_short, theta = theta)
  expected <- 1
  for (i in (1:length(ip_standalone_short))) {
    expected <- expected * temp[i, resp_vector_short[i] + 1]
  }

  observed_resp <- irt:::resp_lik_response_cpp(theta = theta, resp = resp_set,
                                               ip = ip)
  observed_resp_sa_short <- irt:::resp_lik_response_cpp(
    theta = theta, resp = resp_set_sa_short, ip = ip_standalone_short)
  observed_resp_vec <- irt:::resp_lik_bare_itempool_cpp(resp = resp_vector,
                                                        theta = theta, ip = ip)
  observed_resp_vec_sa <- irt:::resp_lik_bare_itempool_cpp(
    resp = resp_vector, theta = theta, ip = ip_standalone)
  observed_resp_vec_short <- irt:::resp_lik_bare_itempool_cpp(
    resp = resp_vector_short, theta = theta, ip = ip_standalone_short)

  expect_equal(expected, observed_resp)
  expect_equal(expected, observed_resp_sa_short)
  expect_equal(expected, observed_resp_vec)
  expect_equal(expected, observed_resp_vec_sa)
  expect_equal(expected, observed_resp_vec_short)

  # microbenchmark::microbenchmark(
  #   new = irt:::resp_lik_response_cpp(theta = theta, resp = resp_set, ip = ip),
  #   old = irt:::resp_lik_response_cpp_DEPRECATED(theta = theta, resp = resp_set, ip = ip),
  #   short = irt:::resp_lik_response_cpp(theta = theta, resp = resp_set_sa_short,
  #                                       ip = ip_standalone_short),
  #   short_dep = irt:::resp_lik_response_cpp_DEPRECATED(
  #     theta = theta, resp = resp_set_sa_short, ip = ip_standalone_short),
  #   sa = irt:::resp_lik_bare_itempool_cpp(
  #     resp = resp_vector, theta = theta, ip = ip_standalone),
  #   sa_short = irt:::resp_lik_bare_itempool_cpp(
  #     resp = resp_vector_short, theta = theta, ip = ip_standalone_short)
  #   )

})


###############################################################################@
############################# resp_lik_response_set_cpp @#######################
###############################################################################@

test_that("resp_lik_response_set_cpp", {
  ip <- generate_ip(model = sample(c("3PL", "GRM"), 8, T))
  n <- 3
  resp <- sim_resp(theta = rnorm(n), ip = ip, prop_missing = .2,
                   output = "response_set")

  theta <- rnorm(n)

  i <- sample(1:n, 1)
  r1 <- resp@response_list[[i]]
  expect_identical(resp_lik(ip = ip[r1@item_id], resp = r1@score, theta = theta[i]),
               irt:::resp_lik_response_set_cpp(resp, theta, ip)[i])

  # ---------------------------------------------------------------------------#

  i <- sample(1:n, 1)
  r1 <- resp@response_list[[i]]
  expect_identical(resp_lik(ip = ip[r1@item_id], resp = r1@score, theta = theta[i]),
               irt:::resp_lik_response_set_cpp(resp, theta, ip = ip)[i])
})
