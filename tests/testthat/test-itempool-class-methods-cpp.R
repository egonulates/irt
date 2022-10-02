# library(testthat)



###############################################################################@
################### get_item_ids_itempool_cpp ##################################
###############################################################################@

test_that("get_item_ids_itempool_cpp", {

  # -------------------------------------------------------------------------- #
  # Item pool with testlets
  ip <- itempool(sample(c(
    lapply(paste0("t", 1:10), function(x) generate_testlet(testlet_id = x)),
    lapply(paste0("i", 1:10), function(x) generate_item(item_id = x)))))

  ids <- as.list(get_slot_itempool_cpp(ip = ip, slotName = "id"))
  for (j in which(sapply(ip, is, "Testlet"))) ids[[j]] <- ip[[j]]@item_list$id
  expected <- unlist(ids)

  observed <- get_item_ids_itempool_cpp(ip)
  expect_identical(observed, expected)

  expected <- unname(sapply(irt:::flatten_itempool_cpp(ip), slot, "item_id"))
  expect_identical(observed, expected)

  # -------------------------------------------------------------------------- #
  # Regular item pool
  ip <- generate_ip()
  expect_identical(get_item_ids_itempool_cpp(ip), ip$item_id)

  # -------------------------------------------------------------------------- #
  # Another item pool with testlets
  ip <- c(generate_testlet(n = 4, item_id_preamble = "t1"),
          generate_ip(n = 2, item_id = paste0("itm-", 1:2)),
          generate_testlet(n = 2, item_id_preamble = "t2"),
          generate_ip(n = 2, item_id = paste0("m-", 1:2)),
          generate_testlet(n = 3, item_id_preamble = "t3"),
          generate_ip(n = 5, item_id = paste0("ii-", 1:5)))
  expect_identical(get_item_ids_itempool_cpp(ip), ip$item_id)

})




###############################################################################@
############################# flatten_itempool_cpp #############################
###############################################################################@


test_that("Test generate_ip", {

  ip <- c(generate_testlet(item_id_preamble = "t1"),
          generate_ip(n = 5, model = c("2PL", "3PL", "GPCM", "PCM", "GRM")),
          generate_testlet(item_id_preamble = "t2"))

  ip_list <- irt:::flatten_itempool_cpp(ip)

  # Make sure the list elements are named.
  expect_true(all(ip$resp_id %in% names(ip_list)))
})
