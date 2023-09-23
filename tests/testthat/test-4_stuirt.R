# library(testthat)

#' Check to see whether the IRTPRO is installed in this machine. If no, skip
#' the tests on this file.
#'
#' @noRd
skip_if_irtpro_exe_not_found <- function() {
  irtpro_exe_paths <- c(file.path("C:/Program Files/IRTPRO 6.0"))
  if (!any(dir.exists(irtpro_exe_paths)))
    skip("IRTPRO is not installed on this compter.")
}


# Set this TRUE to skip all of the tests in this file.
skip_tests <- TRUE

target_dir <- "C:/Temp/testthat-stuirt"


############################################################################@###
############################# equate_stuirt ################################@###
############################################################################@###

test_that("equate_stuirt", {
  skip_on_cran()
  skip_if(skip_tests)
  # -------------------------------------------------------------------------- #
  # Mixed models STUIRT equating
  n_item <- 30
  models <- sample(c("3PL", "GPCM2"), n_item, TRUE)
  new_ip <- generate_ip(model = models, D = 1.702)
  old_ip_df <- data.frame(new_ip)
  old_ip_df$a <- old_ip_df$a + round(runif(n_item, min = -.2, max = .2), 2)
  old_ip_df$b <- old_ip_df$b + round(runif(n_item, min = -.2, max = .2), 2)
  old_ip_df$d1 <- old_ip_df$d1 + round(runif(n_item, min = -.2, max = .2), 2)
  old_ip_df$d2 <- old_ip_df$d2 + round(runif(n_item, min = -.2, max = .2), 2)
  old_ip_df$d3 <- old_ip_df$d3 + round(runif(n_item, min = -.2, max = .2), 2)
  ref_ip <- itempool(old_ip_df)

  # Add standard errors
  for (i in 1:n_item) {
    new_ip[[i]]$se_a <- round(runif(1, .5, 1), 6)
    new_ip[[i]]$se_b <- round(runif(1, .5, 1), 6)
    if (new_ip[[i]]$model == "GPCM2") {
      new_ip[[i]]$se_d <- round(runif(length(new_ip[[i]]$d), .5, 1), 6)
    } else if (new_ip[[i]]$model == "3PL") {
    new_ip[[i]]$se_c <- round(runif(1, .15, 4), 6)
    }
  }

  stuirt_exe_path = "C:/STUIRT/STUIRT.exe"

  target_dir <- "C:/Temp/testthat-stuirt"



  # is_atomic_vector = irt:::is_atomic_vector
  # is_single_value = irt:::is_single_value
  # output_path <- file.path(target_dir, "stuirt_analysis.out")
  result <- equate_stuirt(new_ip = new_ip, ref_ip = ref_ip,
                          target_dir = target_dir,
                          stuirt_exe_path = stuirt_exe_path,
                          )
  expect_s4_class(result$output$equated_ip, "Itempool")
  expect_identical(result$input$method, "stocking-lord")

  ### Check equating has done as expected ###
  equated_ip <- result$output$equated_ip
  A <- result$output$stocking_lord[1] # Slope
  B <- result$output$stocking_lord[2] # Intercept

  # a parameters
  i <- sample(1:n_item, 1)
  expect_identical(equated_ip[[i]]$a, new_ip[[i]]$a / A)
  # b parameters
  i <- sample(1:n_item, 1)
  expect_identical(equated_ip[[i]]$b, A * new_ip[[i]]$b + B)

  # d parameter of GPCM2
  i <- sample(which(new_ip$model == "GPCM2"), 1)
  expect_identical(equated_ip[[i]]$d, A * new_ip[[i]]$d)

  # Standard errors of a parameters
  i <- sample(1:n_item, 1)
  expect_identical(equated_ip[[i]]$se_a, new_ip[[i]]$se_a / A)

  # SE of b parameters
  i <- sample(1:n_item, 1)
  expect_identical(equated_ip[[i]]$se_b, A * new_ip[[i]]$se_b)

  # SE of d parameter of GPCM2
  i <- sample(which(new_ip$model == "GPCM2"), 1)
  expect_identical(equated_ip[[i]]$se_d, A * new_ip[[i]]$se_d)


  # -------------------------------------------------------------------------- #
  # When only some of the items are deemed common
  common_item_ids <- sample(new_ip$item_id, 5)

  result <- equate_stuirt(new_ip = new_ip, ref_ip = ref_ip,
                          target_dir = target_dir,
                          common_item_ids = common_item_ids,
                          stuirt_exe_path = stuirt_exe_path,
                          starting_values = c(0.99, 0.29),
                          new_dist =
                          )

  # -------------------------------------------------------------------------- #
  # Another example with specific values
  test_fn <- test_path("data_for_tests", "STUIRT", "stuirt_eq_1.RDS")
  if (file.exists(test_fn)) {
    test_data <- readRDS(test_fn)

    analysis_name <- paste0("stuirt_analysis_", paste0(letters[sample(1:24, 4)],
                                                       collapse = ""))
    result <- equate_stuirt(new_ip = test_data$new_ip,
                            ref_ip = test_data$ref_ip,
                            target_dir = target_dir,
                            analysis_name = analysis_name,
                            stuirt_exe_path = stuirt_exe_path,
                            add_options = TRUE,
                            starting_values = test_data$starting_values,
                            new_dist = list(type = "PN", n = 41, pars = c(0, 1, 4)),
                            ref_dist = list(type = "PN", n = 41, pars = c(0, 1, 4)),
                            fs = c("DO", "DO"),
                            sy = c("BI", "BI"),
                            ko = "SL"
                            )
    result$output
    test_data$expected
  }


  # -------------------------------------------------------------------------- #
  # Warning raised when the item IDs do not match
  n_item <- 30
  models <- sample(c("3PL", "GPCM2"), n_item, TRUE)
  new_ip <- generate_ip(model = models, D = 1.702)
  old_ip_df <- data.frame(new_ip)
  old_ip_df$a <- old_ip_df$a + round(runif(n_item, min = -.2, max = .2), 2)
  old_ip_df$b <- old_ip_df$b + round(runif(n_item, min = -.2, max = .2), 2)
  old_ip_df$d1 <- old_ip_df$d1 + round(runif(n_item, min = -.2, max = .2), 2)
  old_ip_df$d2 <- old_ip_df$d2 + round(runif(n_item, min = -.2, max = .2), 2)
  old_ip_df$d3 <- old_ip_df$d3 + round(runif(n_item, min = -.2, max = .2), 2)
  ref_ip <- itempool(old_ip_df)
  ref_ip$item_id <- paste0("ii", 1:n_item)

  stuirt_exe_path = "C:/STUIRT/STUIRT.exe"

  target_dir <- "C:/Temp/testthat-stuirt"
  expect_warning(equate_stuirt(new_ip = new_ip, ref_ip = ref_ip,
                               target_dir = target_dir,
                               stuirt_exe_path = stuirt_exe_path),
                 regexp = "All items are used as common items"
    )
  # -------------------------------------------------------------------------- #
  # Warning raised when models of new and old IP are different

  # When all items are common items
  ref_ip <- generate_ip(model = "2PL", n = n_item, D = 1.702)
  expect_error(equate_stuirt(new_ip = new_ip, ref_ip = ref_ip,
                             target_dir = target_dir,
                             stuirt_exe_path = stuirt_exe_path),
               regexp = "The psychometric models of"
    )


  # when there are common items
  expect_error(equate_stuirt(new_ip = new_ip, ref_ip = ref_ip,
                             target_dir = target_dir,
                             common_item_ids = ref_ip$item_id[1:5],
                             stuirt_exe_path = stuirt_exe_path),
               regexp = "The psychometric models of"
    )



})

