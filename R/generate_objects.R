

###############################################################################@
############################# generate_item ####################################
###############################################################################@
#' Generate a random Item object
#'
#' @param model The model of the Item object.
#' @param n_categories For polytomous models, the number of categories for an
#'   'item' object.
#' @param se The values of parameter standard errors, i.e. a list
#'   object with elements named as parameter names (excluding \code{"D"}
#'   parameter).
#'
#'   If the value is \code{TRUE}, this function will generate standard error
#'   values from a uniform distribution between 0.05 and 0.75 for each parameter.
#' @param ... Additional parameters passed to \code{item()} function.
#'
#' @return An \code{\link{Item-class}} object
#'
#' @importFrom stats rnorm rlnorm runif
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' # By default, a '3PL' model Item generated
#' generate_item()
#' # Generate item pools for other models
#' generate_item("Rasch")
#' generate_item("1PL")
#' generate_item("2PL")
#' generate_item("4PL")
#' # Polytomous items
#' generate_item("GRM")
#' generate_item("GPCM")
#' generate_item("PCM")
#' generate_item("GPCM2")
#' # Different number of categories
#' generate_item("GRM", n_categories = 2)
#' generate_item("GPCM", n_categories = 5)
#'
#' # Generate standard errors for item parameters
#' generate_item(se = TRUE)
#'
generate_item <- function(model = "3PL", n_categories = 4,
                          se = NULL, ...) {
  args <- list(...)
  misc <- args$misc
  args$misc <- NULL
  ##### Argument Checks #@###
  if (!is.null(misc) && !is.list(misc))
    stop("'misc' argument should be a list object.")
  if (!is.numeric(n_categories) || length(n_categories) != 1 ||
      n_categories < 2) {
    stop("Invalid number of categories, 'n_categories'. Provide an integer ",
         "larger than 1.")
  } else n_categories <- as.integer(n_categories)

  if (is.null(model) || length(model) != 1 || !is.character(model) ||
      !model %in% names(PMODELS))
    stop("Invalid model argument.")

  pars <- list()
  if (model %in% UNIDIM_DICHO_MODELS) {
    pars$b <- round(rnorm(1), 4)
    if (model %in% c("1PL", "2PL", "3PL", "4PL")) {
      pars$D <- DEFAULT_D_SCALING
      if (model %in% c("2PL", "3PL", "4PL")) {
        pars$a <- round(rlnorm(1, 0, 0.3), 4)
        if (model %in% c("3PL", "4PL")) {
          pars$c <- round(runif(1, 0, 0.3), 4)
          if (model == "4PL") pars$d <- round(runif(1, 0.90, 0.99), 4)
        }
      }
    }
  } else if (model %in% UNIDIM_POLY_MODELS) {
    # Create threshold parameters
    while (TRUE) {
      pars$b <- sort(round(rnorm(n_categories - 1, sd = .75), 4))
      temp_diff <- pars$b[-1] - pars$b[-(n_categories - 1)]
      if (n_categories == 2 || min(temp_diff) > 0.3)
        break
    }
    # For GPCM2, switch b to d and create a new b location parameter
    if (model == "GPCM2") {
      pars$d <- pars$b
      pars$b <- rnorm(1, 0, 0.33)
    }

    if (model %in% c("GRM", "GPCM", "GPCM2")) {
      pars$a <- round(rlnorm(1, 0, 0.3), 4)
      pars$D <- DEFAULT_D_SCALING
    }
  } else if (model %in% c("M2PL", "M3PL")) {
    n_dimensions <- 2
    pars$d <- round(rnorm(1), 3)
    pars$a <- round(rlnorm(n_dimensions, 0, 0.3), 2)
    pars$D <- DEFAULT_D_SCALING_MIRT
    if (model == "M3PL") pars$c <- round(runif(1, 0, 0.3), 4)
  } else {
    warning("This model has not been implemented yet.")
    return(NULL)
  }

  # Check if any of the parameters supplemented in `...`, update those
  # parameters
  pars[names(pars)[names(pars) %in% names(args)]] <-
    args[names(pars)[names(pars) %in% names(args)]]

  # For dichotomous items, add a "key" between "A", "B", "C", "D"
  if (model %in% DICHOTOMOUS_MODELS) {
    possible_options <- c("A", "B", "C", "D")
    key <- sample(possible_options, 1)
    # scoring_function <- function(item, response, ...) {
    #   raw_response <- response@raw_response[response@item_id == item@item_id]
    #   ifelse(raw_response == item@misc$key, 1L, 0L)
    # }

    if (is.null(misc)) {
      misc <- list(key = key, possible_options = possible_options)
    } else {
      if (is.null(misc$key))
        misc <- c(misc, key = key)
      if (is.null(misc$possible_options))
        misc <- c(misc, possible_options = possible_options)
    }
  }

  # Create se if se value is `TRUE`
  if (!is.null(se) && is.logical(se) &&
      length(se) == 1 && se) {
    # se <- names(PMODELS[[model]]$parameters)[
    #       sapply(PMODELS[[model]]$parameters, `[[`, "se")];
    se_par_names <- unlist(sapply(PMODELS[[model]]$parameters, `[[`, "se"))
    se <- stats::setNames(vector("list", length(se_par_names)), se_par_names)
    for (i in 1:length(se))
      se[[i]] <- round(runif(length(pars[[names(se_par_names)[i]]]), min = 0.05,
                             max = 0.75), 3)
  }
  return(do.call("item", c(list(parameters = pars, model = model, se = se,
                                misc = misc), args)))
  # return(item(parameters = pars, model = model, se = se, misc = misc, ...))
}





###############################################################################@
############################# generate_ip ######################################
###############################################################################@
#' Generate a random \code{Itempool} object
#'
#' @param model The model of the item pool
#' @param n The number of items in the item pool.
#' @param output The type of object returned. The default value is
#'   \code{"Itempool"}.
#'   \describe{
#'     \item{\strong{\code{"Itempool"}}}{Return an
#'       \code{\link{Itempool-class}} object.
#'       }
#'     \item{\strong{\code{"Item"}}}{If \code{n = 1} return an
#'       \code{\link{Item-class}} object. If \code{n > 1}, returns a list of
#'       \code{\link{Item-class}} object.
#'       }
#'     \item{\strong{\code{"list"}}}{Return a list of item
#'       \code{\link{Item-class}} objects.
#'       }
#'     }
#' @param n_categories For polytomous items, designate the number of categories
#'   each item should have. It can be a single integer value larger than 1. In
#'   this case all of the polytomous items will have this number of categories.
#'   It can be a vector of length \code{n} designating the categories of each
#'   item. For dichotomous items, the values in \code{n_categories} will be
#'   ignored.
#' @param se The values of parameter standard errors for each item,
#'   i.e. a list
#'   object with elements named as parameter names (excluding \code{"D"}
#'   parameter).
#'
#'   If the value is \code{TRUE}, this function will generate standard error
#'   values from a uniform distribution between 0.05 and 0.75 for each
#'   parameter of each item.
#' @param ... Additional parameters passed to \code{itempool()} function.
#'
#' @return An \code{\link{Itempool-class}} object
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @examples
#' # By default, a '3PL' model item pool generated
#' generate_ip()
#' # Designate the number of items
#' generate_ip(n = 12)
#' # Generate item pools for other models
#' generate_ip(model = "Rasch")
#' generate_ip(model = "1PL")
#' generate_ip(model = "2PL")
#' generate_ip(model = "4PL")
#' generate_ip(model = "GRM") # Graded Response Model
#' generate_ip(model = "GPCM") # Generalized Partial Credit Model
#' generate_ip(model = "PCM") # Partial Credit Model
#' generate_ip(model = "GPCM2") # Reparametrized GPCM
#' # Mixture of models
#' generate_ip(model = c("4PL", "Rasch"))
#' generate_ip(model = sample(c("4PL", "GPCM"), 12, TRUE))
#' generate_ip(model = c("2PL", "GRM", "Rasch"), n = 11)
#'
#' # Generate parameters standard errors for each item
#' generate_ip(se_paramters = TRUE)
#'
#' # Generate an item pool consist of testlets and standalone items
#' temp_list <- list(ids = paste0("testlet-", 1:7), n = c(2, 3, 4, 2, 3, 4, 2))
#' ip <- itempool(sample(c(
#'   generate_ip(n = 10, output = "list"),
#'   sapply(1:length(temp_list$ids), function(i)
#'     generate_testlet(testlet_id = temp_list$ids[i],
#'                      n = temp_list$item_models[i],
#'                      item_id_preamble = paste0("t", i, "-"))))))
#
generate_ip <- function(model = "3PL", n = NULL, output = "Itempool",
                        n_categories = 4, se = NULL, ...) {
  if (is.null(n)) {
    if (length(model) > 1) {
      n <- length(model)
    } else if (length(n_categories) > 1) {
      n <- length(n_categories)
    } else n <-  sample(10:20, 1)
  }
  model <- rep(model, length.out = n)

  if (is.null(n_categories) || !length(n_categories) %in% c(1, n))
    stop("Invalid n_categories value.")
  n_categories <- rep(n_categories, length.out = n)

  item_list <- lapply(1:n, function(i) generate_item(
    model = model[i], n_categories = n_categories[i],
    # The following returns TRUE if se and NULL otherwise
    se = switch(is.list(se) + 1, se, NULL))
    )
  # If se is TRUE, no need to add se to itempool()
  # function because the se are already set in item_list.
  if (!is.null(se) && is.logical(se) &&
      length(se) == 1 && se) {
    ip <- itempool(item_list, ...)
  } else {
    ip <- itempool(item_list, se = se, ...)
  }

  if (output == "Itempool") {
    return(ip)
  } else if (output == "list" || (output == "Item" && n > 1)) {
    return(ip@item_list)
  } else if (output == "Item") {
    return(ip@item_list[[1]])
  } else stop("Invalid 'output' value.")
}

###############################################################################@
############################# generate_testlet #################################
###############################################################################@
#' Generate a random Testlet object
#'
#' @param model The model of the Testlet
#' @param n The number of items in the Testlet.
#' @param item_models A single model name or a vector of model names with the
#'   size of n that represents the models of items in the Testlet object.
#' @param item_id_preamble The preamble for the item ids within the Testlet.
#' @param n_categories For polytomous items, designate the number of categories
#'   each item should have. It can be a single integer value larger than 1. In
#'   this case all of the polytomous items of the testlet will have this number
#'   of categories. It can be a vector of length \code{n} designating the
#'   categories of each item. For dichotomous items, the values in
#'   \code{n_categories} will be ignored.
#' @param ... Additional parameters passed to \code{testlet()} function.
#'
#' @return A \code{\link{Testlet-class}} object
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @examples
#' # By default, a Testlet object with '3PL' model items generated
#' generate_testlet()
#' # Designate the number of items in the testlet
#' generate_testlet(n = 12)
#' # Set the ID of the testlet
#' generate_testlet(testlet_ = "my-testlet")
#' # Designate the ID of testlet and preamble for item ids
#' generate_testlet(testlet_id = "my-testlet", item_id_preamble = "mt-")
#' # Generate item pools for other models
#' generate_testlet(item_model = "Rasch")
#' generate_testlet(item_model = "1PL")
#' generate_testlet(item_model = "2PL")
#' generate_testlet(item_model = "4PL")
#' generate_testlet(item_model = "GRM") # Graded Response Model
#' generate_testlet(item_model = "GPCM") # Generalized Partial Credit Model
#' generate_testlet(item_model = "PCM") # Partial Credit Model
#' generate_testlet(item_model = "GPCM2") # Reparametrized GPCM
#' # Mixture of models
#' generate_testlet(item_models = c("4PL", "Rasch"))
#' generate_testlet(model = c("2PL", "GRM", "Rasch"), n = 11)
#'
#' # Generating multiple testlet objects with custom ids
#' sapply(paste0("testlet-", 1:4), function(x) generate_testlet(testlet_id = x))
#'
#' # Generate testlet with dichotomous and polytomous with different number of
#' # categories.
#' generate_testlet(
#'   item_models = c("3PL", "GRM", "GPCM", "GRM", "2PL"),
#'   n_categories = c(2, 3, 6, 7, 2))
#'
#' # # Generating multiple testlet objects with custom ids and item models and
#' # # put them in an item pool:
#' # temp_list <- list(ids = paste0("testlet-", 1:3),
#' #                   item_models = c("Rasch", "2PL", "GPCM"))
#' # itempool(sapply(1:length(temp_list$item_id), function(i)
#' #   generate_testlet(item_id = temp_list$item_id[i],
#' #   item_models = temp_list$item_models[i])))
#'
generate_testlet <- function(model = "BTM", n = NULL,
                             item_models = "3PL", item_id_preamble = NULL,
                             n_categories = 4, ...) {
  args <- list(...)
  if (is.null(n)) {
    n <- sample(2:5, 1)
    if (length(item_models) > 1) n <- length(item_models)
  }
  if (is.null(item_models) || !is.character(item_models)) {
    item_models <- rep("3PL", length.out = n)
  } else if (length(item_models) == 1 || length(item_models) != n) {
    item_models <- rep(item_models, length.out = n)
  }
  ip <- generate_ip(model = item_models, n_categories = n_categories)
  # item_list <- lapply(item_models, generate_item)
  # ip <- itempool(item_list)
  if (is.null(item_id_preamble)) {
    if ("item_id" %in% names(args)) {
      ip$item_id <- paste0(args$item_id, "-", ip$item_id)
    } else if (!is.null(args$testlet_id)) {
      ip$item_id <- paste0(args$testlet_id, "-", ip$item_id)
    } else
      ip$item_id <- paste0("t-", ip$item_id)
  } else {
      ip$item_id <- paste0(item_id_preamble, ip$item_id)
  }
  return(testlet(ip, ...))
}



###############################################################################@
############################# generate_resp ####################################
###############################################################################@
#' Generate random item responses (Response object)
#'
#' @description
#' \code{generate_resp} Generate dichotomous (0 or 1) or polytomous responses
#' for given ability and item parameter(s). This function returns a
#' \code{\link{Response-class}} object.
#'
#' @param ip An \code{\link{Item-class}}, \code{\link{Itempool-class}},
#'   \code{\link{Testlet-class}} object containing the item parameters.
#' @param theta An object containing the subject ability parameters.
#' @param prop_missing Proportion of responses that should be missing. Default
#'   value is \code{0}.
#'
#' @return Returns a list of \code{\link{Response-class}} objects with equal
#' length to the length of theta.
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#' @include itempool-class-methods.R
#' @include response-class.R
#'
#' @author Emre Gonulates
#'
#' @export
#'
#'
#' @examples
#' ip <- generate_ip(model = "3PL", n = 15)
#' generate_resp(ip, theta = rnorm(1))
#'
#' # A list of Response objects
#' generate_resp(ip, theta = rnorm(5))
#'
#' # Set the proportion of missing responses:
#' generate_resp(ip, theta = rnorm(5), prop_missing = 0.3)
#'
generate_resp <- function(ip, theta, prop_missing = 0) {
  if (!is(ip, "Itempool")) ip <- itempool(ip)
  result <- sim_resp_response_set_cpp(theta = theta, ip = ip,
                                      prop_missing = prop_missing,
                                      examinee_id = as.character(names(theta)))
  result@response_list
}


###############################################################################@
############################# generate_resp_set ################################
###############################################################################@
#' Generate a random item responses (Response_set object)
#'
#' @description
#' \code{generate_resp_set} Generate dichotomous (0 or 1) or polytomous
#' responses for given ability and item parameter(s). This function returns a
#' \code{\link{Response_set-class}} object,
#'
#' @param ip An \code{\link{Item-class}}, \code{\link{Itempool-class}},
#'   \code{\link{Testlet-class}} object containing the item parameters.
#' @param theta An object containing the subject ability parameters.
#' @param prop_missing Proportion of responses that should be missing. Default
#'   value is \code{0}.
#'
#' @return Returns a \code{\link{Response_set-class}} object.
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#' @include itempool-class-methods.R
#' @include response-class.R
#'
#' @author Emre Gonulates
#'
#' @export
#'
#'
#' @examples
#' ip <- generate_ip(model = "3PL", n = 15)
#' generate_resp_set(ip, theta = rnorm(5))
#'
#' # Set the proportion of missing responses:
#' generate_resp_set(ip, theta = rnorm(7), prop_missing = 0.3)
#'
generate_resp_set <- function(ip, theta, prop_missing = 0) {
  if (!is(ip, "Itempool")) ip <- itempool(ip)
  sim_resp_response_set_cpp(theta = theta, ip = ip,
                            prop_missing = prop_missing,
                            examinee_id = as.character(names(theta)))
}







