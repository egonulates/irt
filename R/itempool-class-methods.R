
###############################################################################@
############################# itempool #########################################
###############################################################################@
#' Create an \code{Itempool} object
#'
#' @description This method creates a new \code{\link{Itempool-class}} object.
#'
#' @param ... The object that is desired to be converted to an 'Itempool'
#'          object. Also additional arguments related to the Itempool.
#'
#' @return An \code{\link{Itempool-class}} object.
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#'
#' @export
#'
#' @importFrom methods validObject new slot<-
#'
#' @author Emre Gonulates
#'
#' @examples
#' # Create an item pool with two 2PL items
#' itempool(a = c(1, 1.4), b = c(-2, 1))
#' itempool(a = c(1, 1.4), b = c(-2, 1), model = "2PL")
#' # Set D parameter
#' itempool(a = c(1, 1.4), b = c(-2, 1), D = 1.7)
#' # Set item IDs
#' itempool(a = c(1, 1.4), b = c(-2, 1), item_id = c("i1", "i2"))
#' # Set content
#' itempool(a = c(1, 1.4), b = c(-2, 1), content = c("Algebra", "Geometry"))
#'
#' # Create 3PL items from a data frame:
#' ipdf <- data.frame(a = c(.9, .79, 1.26),
#'                    b = c(-1, .43, -2.3),
#'                    c = c(.2, .38, .25))
#' itempool(ipdf)
#'
#' # Create GRM (Graded Response Model) items from a data frame
#' ipdf <- data.frame(a = rlnorm(10, 0, .3), b1 = rnorm(10), b2 = rnorm(10))
#' itempool(ipdf, model = "GRM")
#'
#' # Create a Rasch model item pool
#' itempool(b = c(-1, 0.2, 1.1), model = "Rasch")
#'
#' # Add 'misc' field:
#' ip <- itempool(b = rnorm(2), item_id = paste0("t1-i", 1:2),
#'                misc = list(list(sympson_hetter_k = .8),
#'                            list(sympson_hetter_k = .9)))
#' ip[[1]]  # First item of the item pool
#'
itempool <- function(...){
  # This function will create new "Itempool" objects.
  args <- list(...)
  # There are three possibilities to create an Itempool object:
  # (1) Give an argument like itempool(x = ....).
  # (2) Give an argument like itempool(<Object without Name>,....).
  # (3) Give arguments like item parameter names like
  #     itempool(a = runif(10, .5, 1.5), b = rnorm(2), ....).
  # If it looks like the third option, I'll create an 'x' object out of them.


  ## ------------------------  Functions ------------------------------------ ##
  # This function converts adds se given as a data frame or list
  # to a list of items (not testlets yet)
  #
  # @param item_list a list of items/testlets  (for now just items)
  # @param args The arguments vector supplied to the main function
  #
  # @return A list of items/testlets where the \code{se} slot
  #   updated based on the entry
  add_se <- function(item_list, args) {
    if (is.null(args$se)) return(item_list)
    se <- args$se
    # Get se_parameter name of each item and define acceptable names for
    # parameters
    se_par_names <- lapply(sapply(
      item_list, function(x) class(x)),
      function(s) {
        unlist(sapply(PMODELS[[s]]$parameters, `[[`, "se"))
        # p <- PMODELS[[s]]$parameters
        # se_names <- unlist(sapply(p, `[[`, "se"));
        # return(c(se_names, paste0(names(se_names), "_se")))
        })
    # Convert matrix to data frame
    if (inherits(se, "matrix"))
      se <- as.data.frame(se)

    if (inherits(se, "data.frame")) {
      if (nrow(se) !=  length(item_list))
        stop("Invalid 'se. The number of rows of the ",
             "'se' should be ", length(item_list), ".")
      # Convert data frame rows to list elements
      se <- lapply(seq_len(nrow(se)),
           function(i) lapply(se, "[", i))
      # sanitize the names of the se parameters
      se <- lapply(se, function(x) {
        names(x) <- gsub("_se$|^se_", "", names(x));
        x
        })
    }
    # se_parameter should follow a certain structure
    if (!inherits(se, "list"))
      stop("Invalid 'se' provided to 'itempool()' function. ",
           "Please provide a list or data.frame for 'se'",
           call. = FALSE)
    item_list <- lapply(1:length(item_list), FUN = function(i) {
      for (s in 1:length(se_par_names[[i]])) {
        if (se_par_names[[i]][s] %in% names(se[[i]])) {
          slot(item_list[[i]], se_par_names[[i]][s]) <-
            se[[i]][[se_par_names[[i]][s]]]
        } else if (names(se_par_names[[i]])[s] %in% names(se[[i]])) {
          slot(item_list[[i]], se_par_names[[i]][s]) <-
            se[[i]][[names(se_par_names[[i]])[s]]]
        }
      }
      return(item_list[[i]])})
    return(item_list)
  }

  ## -------------------------------------------------------------------------##
  # This function adds item ID's given as a data frame or list
  # to a list of items (not testlets yet)
  #
  # @param item_list a list of items/testlets  (for now just items)
  # @param args The arguments vector supplied to the main function
  #
  # @return A list of items/testlets where the \code{item_id} slot
  #   updated based on the entry
  add_item_id <- function(item_list, args) {
    # ID can be supplied in two ways. The first, it can be supplied as an
    # argument to "args". Or it can be a column if the first element of the
    # function is a data.frame/matrix

    item_id <- NULL
    # Check whether 'item_id' exists in the arguments
    if ("item_id" %in% tolower(names(args))) {
      for (id_name in c("item_id", "Item_ID", "ITEM_ID", "Item_Id", "item_ID"))
        if (id_name %in% names(args)) {
          item_id <- args[[id_name]]
          break
        }
    } else if (length(args) > 0 &&
               inherits(args[[1]], c('matrix', 'data.frame')) &&
               "item_id" %in% tolower(colnames(x))) {
      for (id_name in c("item_id", "Item_ID", "ITEM_ID", "Item_Id", "item_ID"))
        if (id_name %in% colnames(args[[1]])) {
          item_id <- args[[1]][, id_name, drop = TRUE]
          break
        }
      if (is.null(item_id) && !is.null(rownames(args[[1]])))
        item_id <- rownames(args[[1]])
    }
    if (is.null(item_id)) {
      return(name_items(item_list))
    }
    item_id <- as.character(item_id)
    # Item ID cannot be duplicated
    if (any(duplicated(item_id)))
      stop("Invalid item IDs. Item ID cannot be duplicated.", call. = FALSE)
    item_num <- sum(sapply(item_list, length))
    if (length(item_id) != item_num)
    # if (length(item_id) != length(item_list))
      stop(paste0("Invalid item IDs. Item ID length provided should be ",
                  item_num, "."), call. = FALSE)
    # Assign item ids
    counter <- 1
    for (i in sequence(length(item_list))) { # element can be item or testlet
      if (inherits(item_list[[i]], "Item")) {
        item_list[[i]]@item_id <- item_id[counter]
        names(item_list)[i] <- item_id[counter]
        counter <- counter + 1
      } else if (is(item_list[[i]], "Testlet")) {
        for (j in sequence(length(item_list[[i]]@item_list@item_list))) {
          item_list[[i]]@item_list@item_list[[j]]@item_id <- item_id[counter]
          names(item_list[[i]]@item_list@item_list)[j] <- item_id[counter]
          counter <- counter + 1
        }
      } else stop("Invalid element in the item list.")
    }
    # item_list <- lapply(1:length(item_list), FUN = function(i) {
    #   item_list[[i]]@item_id <- item_id[i];
    #   return(item_list[[i]])
    #   })

    return(item_list)
  }

  ## -------------------------------------------------------------------------##
  # This function adds item content's given to a list of items(not testlets yet)
  #
  # @param item_list a list of items/testlets  (for now just items)
  # @param args The arguments vector supplied to the main function
  #
  # @return A list of items/testlets where the \code{content} slot
  #   updated based on the entry
  add_content <- function(item_list, args) {
    # content can be supplied in two ways. The first, it can be supplied as an
    # argument to "args". Or it can be a column if the first element of the
    # function is a data.frame/matrix

    content <- NULL
    # Check whether 'content' exists in the arguments
    if ("content" %in% tolower(names(args))) {
      content <- args[[names(args)["content" == tolower(names(args))][1]]]
    } else if (length(args) > 0 &&
               inherits(args[[1]], c('matrix', 'data.frame')) &&
               "content" %in% tolower(colnames(x))) {
      content <- args[[1]][, colnames(args[[1]])[
        "content" == tolower(colnames(args[[1]]))][1]]
    }
    if (is.null(content)) return(item_list)
    content <- as.character(content)
    # If the length of content is not equal to item_list, recycle content:
    content <- rep(content, length.out = length(item_list))

    # Assign item/testlet content
    item_list <- lapply(1:length(item_list), FUN = function(i) {
      item_list[[i]]@content <- content[i];
      return(item_list[[i]])})
    return(item_list)
  }

  ## -------------------------------------------------------------------------##
  # This function adds item 'misc' slot given to a list of items
  # (not testlets yet)
  #
  # @param item_list a list of items/testlets  (for now just items)
  # @param args The arguments vector supplied to the main function
  #
  # @return A list of items/testlets where the \code{misc} slot
  #   updated based on the entry
  add_misc <- function(item_list, args) {
    misc <- NULL
    # Check whether 'misc' exists in the arguments
    if ("misc" %in% tolower(names(args))) {
      misc <- args[[names(args)["misc" == tolower(names(args))][1]]]
    }
    if (is.null(misc)) return(item_list)

    # Make sure 'misc' is a list and all elements of the 'misc' are also list.
    if (!is.list(misc))
      stop("'misc' should be a list. If 'misc' values of items are ",
           "different than each other, then 'misc' should be a list of lists",
           "where each list element represents the 'misc' field of an item. ",
           "For example, if a 'time_limit' misc field is desired to be ",
           "assigned to each element of an item pool with two items, 'misc' ",
           "field should look like this: ",
           "'misc = list(list(time_limit = 120), list(time_limit = 180))'. ",
           "Whereas, if all items should have the same 'misc' field, the ",
           "following can be written: 'misc = list(test_form_id = \"F1\")'.",
           call. = TRUE)

    # If the length of misc is not equal to item_list, recycle content:
    if (!is.list(misc) || !all(sapply(misc, is.list)))
      misc <- list(misc)

    misc <- rep(misc, length.out = length(item_list))

    if (!is.list(misc) || !all(sapply(misc, is.list)) ||
        length(misc) !=  length(item_list))
      stop("Cannot add the misc field. Please provide a valid 'misc' value.")

    # Assign item/testlet content
    item_list <- lapply(1:length(item_list), FUN = function(i) {
      item_list[[i]]@misc <- misc[[i]];
      return(item_list[[i]])})
    return(item_list)
  }

  ## -------------------------------------------------------------------------##
  # This function adds D parameter to the items
  #
  # @param item_list a list of items/testlets  (for now just items)
  # @param args The arguments vector supplied to the main function
  #
  # @return A list of items/testlets where the appropriate "D" parameter added
  #   to the items
  add_D <- function(item_list, args) {
    D <- args$D
    if (is.null(D)) return(item_list)

    # If the length of D is not equal to item_list, recycle content:
    D <- rep(as.numeric(D), length.out = length(item_list))

    # Assign item/testlet content
    item_list <- lapply(1:length(item_list), FUN = function(i) {
      if ("D" %in% names(PMODELS[[c(class(item_list[[i]]))]]$parameters))
        item_list[[i]]@D <- D[i];
      return(item_list[[i]])})
    return(item_list)
  }

  # Check whether the 'model' in function arguments is indeed a valid 'model'
  # name.
  check_model_argument <- function(model) {
    if (is.null(model) || !is.character(model) || length(model) != 1 ||
        (!model %in% names(PMODELS))) return(FALSE)
    return(TRUE)
  }



  ### --------------------- Start Main Function ---------------------------- ###

  # Scenario 1, first argument is an Itempool object and arguments are
  # relevant things to be added, like item_id, or D
  x <- args[[1]]
  if (inherits(x, 'Itempool')) {
    args[[1]] <- NULL
    # Add item ids
    x@item_list <- add_item_id(item_list = x@item_list, args = args)
    # Add content
    x@item_list <- add_content(item_list = x@item_list, args = args)
    # Add se
    x@item_list <- add_se(item_list = x@item_list, args = args)
    # Add misc
    x@item_list <- add_misc(item_list = x@item_list, args = args)
    # Add D
    x@item_list <- add_D(item_list = x@item_list, args = args)

    names(x@item_list) <- sapply(x@item_list, function(i)
      ifelse(is(i, "Testlet"), i@testlet_id, i@item_id))

    if (!all(sapply(x@item_list, FUN = "validObject")))
      stop("Invalid item. At least one 'Item' object is not valid." )
    if (!validObject(x))
      stop("Invalid 'Itempool'. At least one 'Item' object is not valid." )
    return(x)
  } else if (inherits(x, c("Item", "Testlet"))) {
    item_list_indicator <- sapply(args, function(m)
      is(m, "Item") | is(m, "Testlet"))
    item_list <- args[item_list_indicator]
    item_list <- name_items(item_list)
    if (length(item_list) == 1) {
      if (length(args) > 1)  x <- do.call("item", args)
      x <- name_items(list(x))
      names(x) <- sapply(x, function(i) ifelse(inherits(i, "Item"), i@item_id,
                                               i@testlet_id))
      # if (is.null(x@item_id))  x@item_id <- paste0("Item-", 1)
      result <- new("Itempool", item_list = x)
    } else if (length(item_list) > 1) {
      args[item_list_indicator] <- NULL
      args <- c(list(item_list), args)
      result <- do.call("itempool", args)
    }
    validObject(result)
    return(result)
  } else if (inherits(x, c("integer", "numeric"))) {
    # Check whether 'model' argument is in the arguments.
    # This part is only create items from Rasch, 1PL, 2PL, 3PL, 4PL models.
    #   -> (Yes): Check whether all of the item parameters for that model are
    #        in the argument:
    #        -> (Yes) Check:
    #             (1) whether the length of each element is the same and
    #             (2) whether each parameter set is numeric
    #             If there are parameters that are not
    #             Create itempool object.
    #        -> (No) Raise Error.
    #   -> (No): Check whether it is one of the Rasch, 1PL, 2PL, 3PL, 4PL models
    #        -> (Yes) Check
    #             (1) whether the length of each element is the same and
    #             (2) whether each parameter set is numeric
    #             Create itempool object.
    #        -> (No) Raise Error.

    # First check if model is in the arguments, if not, check whether the
    # arguments for Rasch, 1PL, 2PL, 3PL, 4PL models are complete. If they
    # are complete assign a model name, otherwise
    if (!"model" %in% names(args)) {
      for (m in c("4PL", "3PL", "2PL", "1PL")) {
        par_names <- names(unlist(sapply(PMODELS[[m]]$parameters, `[[`, "se")))
        # par_names <- names(PMODELS[[m]]$parameters)[
        #   sapply(PMODELS[[m]]$parameters, `[[`, "se")];
        if (all(par_names %in% names(args))) {
          args$model <- m
          break
        }
      }
    }
    if ("model" %in% names(args) && check_model_argument(args$model) &&
        args$model %in% UNIDIM_DICHO_MODELS) {
      par_names <- names(unlist(sapply(PMODELS[[args$model]]$parameters,
                                       `[[`, "se")))
      # par_names <- names(PMODELS[[args$model]]$parameters)[
      #   sapply(PMODELS[[args$model]]$parameters, `[[`, "se")];
      if (all(par_names %in% names(args))) {
        # Check whether all of the parameter has the same length
        if (length(unique(sapply(args[par_names], length))) == 1 &&
            all(sapply(args[par_names], is.numeric))) {
          # Create a list of items using only the item parameters.
          n_items <- length(args[[par_names[1]]])
          result <- vector("list", n_items)
          pars <- do.call("cbind.data.frame", args[par_names])
          pars <- lapply(seq_len(nrow(pars)), function(i) lapply(pars, "[", i))
          result <- lapply(pars, function(i)
            do.call("item", args = c(model = args$model, i)))
          # Remove all parameter arguments from the arg
          args[par_names] <- NULL
          # Add results to the argument
          args <- c(list(name_items(result)), args)
          # Re-run the itempool() function with new arguments
          result <- do.call("itempool", args)
        } else {
          stop("All parameters supplied should be numeric and should have the ",
               "same length.", call. = FALSE)
        }
      } else {
        stop(paste0("Incomplete parameters. For '", args$model, "' model ",
                    "following parameters should be provided: ",
                    paste0("'", par_names, "'", collapse = ", "), "."))
      }
    } else
      stop(paste0("Itempool object cannot be created for '", args$model,
                  "' model. Please either choose a different model or ",
                  "use a different method to enter item parameter values.",
                  "see '?itempool'."), call. = FALSE)
    validObject(result)
    return(result)
  } else if (inherits(x, 'list')) {
    if (all(sapply(x, FUN = function(m) is(m, "Item") | is(m, "Testlet")))) {
      # Add item_id
      x <- add_item_id(item_list = x, args = args)
      # Add content
      x <- add_content(item_list = x, args = args)
      # Add se
      x <- add_se(item_list = x, args = args)
      # Add misc
      x <- add_misc(item_list = x, args = args)
      # Add D
      x <- add_D(item_list = x, args = args)

      names(x) <- sapply(x, function(i) ifelse(inherits(i, "Item"), i@item_id,
                                               i@testlet_id))
      result <- new("Itempool", item_list = x)
      validObject(result)
      return(result)
    } else stop("Invalid elements. All elements of the list should be an ",
                "'Item' or 'Testlet' object.")
  } else if (inherits(x, c('matrix', 'data.frame'))) {
    if (inherits(x, "matrix")) x <- as.data.frame(x)
    if ("model" %in% names(args)) {
      x <- cbind(x, model = args$model)
      args$model <- NULL
    }
    # For each element of x_list, this function checks each element of x_list
    # for two conditions, either an element is not NA, or if the element is a
    # list, it should not be a list with one NA element (like list(NA)).
    # 'm' is an element of x_list, which can composed of single values or
    # list values (list values are misc fields)
    # m <- x_list[[1]]
    # m <- x_list[[2]]
    #
    # Remove NA arguments in x. For example, if the data set looks like this:
    #        a      b1      b2     b3     b4 model
    # 1 1.0244 -1.0764  0.2585     NA     NA  GPCM
    # 2 0.6650 -0.7706 -0.2641 0.3584     NA  GPCM
    # 3 1.0409 -0.6755 -0.3546 0.3088 1.4189  GPCM
    check_acceptable_elements <- function(m) {
      m <- m[sapply(m , function(i)
        (!is.list(i) && !is.na(i)) || !(length(i) == 1 && all(is.na(i[[1]])))
        )]
      # unlist single element lists
      lapply(m, function(i) if (is.list(i) && length(i) == 1) unlist(i) else i)
    }
    if (any(c("testlet", "testlet_id") %in% colnames(x))) {
      testlet_col_name <- colnames(x)[which(colnames(x) %in%
                                              c("testlet", "testlet_id"))[1]]
      testlet_ids <- unique(x[[testlet_col_name]])
      testlet_ids <- testlet_ids[!is.na(testlet_ids)]
      x_list <- list()
      for (i in 1:nrow(x)) {
        testlet_id <- x[[testlet_col_name]][i]
        if (is.na(testlet_id)) { # item is NOT within a testlet
          temp <- lapply(x[i, , drop = FALSE], "[")
          temp <- check_acceptable_elements(temp)
          x_list <- c(x_list, do.call("item", temp))
        } else {# item is within a testlet
          # Check whether the item is already included in a testlet before. If
          # yes, go to the next items
          if (testlet_id %in% testlet_ids) {
            temp <- x[!is.na(x[[testlet_col_name]]) &
                        x[[testlet_col_name]] == testlet_id,
                      colnames(x) != testlet_col_name, drop = FALSE]
            x_list <- c(x_list, testlet(temp, testlet_id = testlet_id))
            testlet_ids <- setdiff(testlet_ids, testlet_id)
          } else {# item has already included in a previous testlet
            next
          }
        }
      }
    } else {
      x_list <- lapply(seq_len(nrow(x)), function(i) lapply(x, "[", i))
      x_list <- lapply(x_list, check_acceptable_elements)
      # convert x to a list of items:
      x_list <- lapply(x_list, function(i) do.call("item", i))
    }
    x_list <- add_item_id(x_list, args)
    args[[1]] <- x_list
    return(do.call("itempool", args))
  }
  stop("Cannot coerce this object to an 'Itempool' object.")
}

###############################################################################@
############################# name_items #######################################
###############################################################################@
#' Give Item class elements a unique item_id.
#' @description This function gives unique item_id's to elements of Item
#'   class. If there is no item_id's specified for an \code{\link{Item-class}}
#'   object, a default item_id will be given to that object. If some elements
#'   have item_id's already, uniqueness of the names will be checked. If they
#'   are not unique, an error will be issued. If some are unique and some are
#'   empty, a default item_id will be given to the empty ones.
#' @param item_list A list consist of \code{\link{Item-class}} or
#'   \code{\link{Testlet-class}} class objects.
#'
#' @return A list with \code{\link{Item-class}} object, which are all named.
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#'
#' @author Emre Gonulates
#'
#' @keywords internal
#'
#' @noRd
#'
#' @examples
#' item1 <- item(a = 1.12, b = -2.1, c = 0.28)
#' item2 <- item(a = 2, b = 3.2, c = 0.21)
#' item4 <- item(a = 1.12, b = -1.23, c = .2, item_id = "I-21")
#' item5 <- item(a = 0.84, b = 2.23, c = .25, item_id = "I-22")
#'
#' item_list <- list(item1, item2)
#' name_items(item_list)
#'
#' item_list <- list(item1, item4, item2)
#' name_items(item_list)
#'
#' item_list <- list(item4, item5)
#' name_items(item_list)
#'
#' # Following code will give an error
#' # item_list <- list(item4, item4)
#' # name_items(item_list)
name_items <- function(item_list)
{
  # Stop if all elements of the list is not \code{\link{Item-class}} objects
  stopifnot(all(sapply(item_list,
                       FUN = function(x) is(x, "Item") | is(x, "Testlet"))))
  # Get item item_id's of only non-testlet items
  itemIDs <- unlist(sapply(
    item_list,
    FUN = function(itemObject) if (is(itemObject, "Item")) itemObject@item_id))
  # Check whether there is a testlet in the item_list:
  has_testlet <- any(sapply(item_list, is, "Testlet"))
  # If there is a testlet get item_id's of items within testlet.
  if (has_testlet)
    for (t in which(sapply(item_list, is, "Testlet")))
      itemIDs <- c(itemIDs, unlist(sapply(
        item_list[[t]]@item_list@item_list,
        FUN = function(itemObject) itemObject@item_id)))

  if (any(duplicated(itemIDs))) stop(paste0(
    "\nInvalid 'item_id's. There are duplicated item_id's. Correct them ",
    "before proceeding. Duplicated item_id's are: \n\"",
    paste0(itemIDs[duplicated(itemIDs)], collapse = "\", \""), "\""),
    call. = FALSE)

  counter <- 1
  testlet_counter <- 1
  for (i in seq_len(length(item_list))) {
    if (is(item_list[[i]], "Item")) {
      while (is.null(item_list[[i]]@item_id)) {
        temp_id <- paste0("Item_", counter)
        if (!temp_id %in% itemIDs) {
          item_list[[i]]@item_id <- temp_id
          itemIDs <- c(itemIDs, temp_id)
        }
        counter <- counter + 1
      }
    } else if (is(item_list[[i]], "Testlet")) {
      # Give an testlet_id to testlet
      while (is.null(item_list[[i]]@testlet_id)) {
        temp_id <- paste0("Testlet_", testlet_counter)
        if (!temp_id %in% itemIDs) {
          item_list[[i]]@testlet_id <- temp_id
          itemIDs <- c(itemIDs, temp_id)
        }
        testlet_counter <- testlet_counter + 1
      }

      for (j in seq_len(length(item_list[[i]]))) {
        while (is.null(item_list[[i]]@item_list[[j]]@item_id)) {
          temp_id <- paste0("Item_", counter)
          if (!temp_id %in% itemIDs) {
            item_list[[i]]@item_list[[j]]@item_id <- temp_id
            itemIDs <- c(itemIDs, temp_id)
          }
          counter <- counter + 1
        }
      }
    }
  }
  return(item_list)
}



###############################################################################@
############################# concatenation of 'Item' objects ##################
###############################################################################@
.concatenate.Item.Itempool.obj <- function(x, ...) {
  args = list(x, ...)
  if (!all(sapply(args, inherits, c("Item", "Itempool", "Testlet"))))
    stop("All of the elements should be either 'Item', 'Itempool' or ",
         "'Testlet' class.", .call = FALSE)

  item_list = list()
  element_no = 0 # This designates the index of element at the final output ip
  for (i in 1:length(args)) {
    if (inherits(args[[i]], "Item")) {
      element_no <- element_no + 1
      item_list[[element_no]] = args[[i]]
    } else if (is(args[[i]], "Itempool")) {
      for (j in 1:length(args[[i]])) {
        element_no <- element_no + 1
        item_list[[element_no]] <- args[[i]]@item_list[[j]]
      }
    } else if (is(args[[i]], "Testlet")) {
      element_no <- element_no + 1
      item_list[[element_no]] = args[[i]]
    }
  }
  # Name items in case they are missing.
  item_list <- name_items(item_list)
  names(item_list) <- sapply(item_list, function(i) i$id)
  return(methods::new(Class = "Itempool", item_list = item_list))
}

#' Concatenate \code{Item}, \code{Itempool} or \code{Testlet} objects and
#' return an Itempool object.
#'
#' If the elements do not have ID fields, function will assign default names.
#'
#' @param x A list consist of \code{\link{Item-class}} objects.
#' @param ... Additional arguments
#'
#' @return An \code{\link{Itempool-class}} object.
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#'
#' @rdname c
#'
#' @method c Item
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' item1 <- item(a = 1.12, b = -2.1, c = 0.28)
#' item2 <- item(a = 2, b = 3.2, c = 0.21)
#'
#' # Concatenate items
#' c(item1, item2)
#'
#' ip <- itempool(a = c(1, 1.2), b = c(1, 2), c = c(.2, .4))
#' # Concatenate items and an Itempool object
#' c(item1, ip)
#' c(item1, item2, ip)
#' c(ip, item1, item2)
setMethod("c", signature(x = "Item"), .concatenate.Item.Itempool.obj)

#' @rdname c
#' @method c Itempool
setMethod("c", signature(x = "Itempool"), .concatenate.Item.Itempool.obj)

#' @rdname c
#' @method c Testlet
setMethod("c", signature(x = "Testlet"), .concatenate.Item.Itempool.obj)


###############################################################################@
############################# [ (Itempool) #####################################
###############################################################################@
#' Subset \code{Itempool} objects
#'
#' @param x An \code{\link{Itempool-class}} object from which to extract
#'   element(s) or in which to replace element(s).
#' @param i indices specifying elements to extract or replace.
#' @param j This will not be used in \code{\link{Itempool-class}} objects.
#' @param ... Parameters to be passed to the function.
#' @param drop (From R manual:) For matrices and arrays. If TRUE the result is
#' coerced to the lowest possible dimension (see the examples). This only works
#' for extracting elements, not for the replacement. See drop for further
#' details.
#'
#' @return An \code{\link{Itempool-class}} object with elements from
#'   \code{\link{Item-class}}.
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#'
#' @export
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
#' @examples
#' ip <- itempool(a = c(1.12, 2.1, 1.28), b = c(2, 3.2, 0.21),
#'                 item_id = c("i1", "i2", "i3"))
#'
#' ip[1]
#' # Create an Itempool using the first and third element:
#' ip[c(1, 3)] # Order is important
#' ip[c(3, 1)]
#' ip[-2]
#' ip[c(TRUE, FALSE, TRUE)]
#' ip[c("i2", "i1")]
#' # Recycle, i.e. get all elements
#' ip[TRUE]
setMethod("[", c(x = "Itempool", i = "ANY", j = "missing", drop = "ANY"),
          function(x, i, j, ..., drop = TRUE)
          {
            # If the length of i is the same as Itempool object and all of
            # its elements are character and none duplicated and all of them
            # are ID's of elements (not Item ID's within the testlets)
            if (is.character(i)) {
              if (!any(duplicated(i)) && all(i %in% x$id)) {
                x@item_list <- x@item_list[i]
              } else
                stop(paste0("Failed to subset using the given vector. Please ",
                            "use valid Item or testlet ID's. There ",
                            "are either duplicated ID's or non-existent ID's ",
                            "in the subsetting vector provided."))
            } else if (is.logical(i) || is.numeric(i)) {
              # If there are NA values in the index then (1) if indices are
              # numeric, remove NAs. (2) if indices are logical, convert NAs to
              # FALSE
              if (any(is.na(i))) {
                if (is.numeric(i)) i <- i[!is.na(i)] else i[is.na(i)] <- FALSE
              }
              x@item_list <- x@item_list[i]
            }
            tryCatch({
              validObject(x)
              },
              error = function(e) {
                if (grepl("Item pool cannot be empty.", e$message))
                  stop("The selection did not match any Item/Testlet object.",
                       call. = FALSE)
                else stop(e$message, call. = FALSE)
              })
            return(x)
          })


###############################################################################@
############################# [[ (Itempool) ####################################
###############################################################################@
#' Subset \code{Itempool} objects
#'
#' @param x An \code{\link{Itempool-class}} object from which to extract
#'   element(s) or in which to replace element(s).
#' @param i indices specifying elements to extract or replace.
#' @param j This will not be used in \code{\link{Itempool-class}} objects.
#' @param ... Additional parameters to be passed to the function.
#'
#' @return An \code{\link{Item-class}} or \code{\link{Testlet-class}} object.
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#'
#' @export
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
#' @examples
#' item1 <- item(a = 1.12, b = -2.1, c = 0.28)
#' item2 <- item(a = 2, b = 3.2, c = 0.21)
#'
#' ip1 <- c(item1, item2)
#' ip1[[1]]
setMethod("[[", c("Itempool", "numeric", "missing"),
          function(x, i, j, ...)
          {
            result <- tryCatch({
              x@item_list[[i]]
              },
              error = function(e) {
                if (grepl("subscript out of bounds", e$message))
                  stop(paste0(
                    "Subscript out of bounds. Please use an index between ",
                    "1 and ", length(x), "."), call. = FALSE)
                NULL
              }
              )
            return(result)
          })


###############################################################################@
############################# [[<- (Itempool) ##################################
###############################################################################@
#' Set the elements of an \code{Itempool} objects.
#'
#' @param x \code{\link{Itempool-class}} object.
#' @param i indices specifying elements to extract or replace.
#' @param j This will not be used in \code{\link{Itempool-class}} objects.
#' @param value An \code{\link{Item-class}} or \code{\link{Testlet-class}}
#'   object.
#' @param ... Additional parameters to be passed to the function.
#'
#' @return An updated \code{\link{Itempool-class}} object.
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#'
#' @export
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
#' @examples
#' item1 <- item(a = 1.12, b = -2.1, c = 0.28)
#' item2 <- item(a = 2, b = 3.2, c = 0.21)
#' ip <- c(item1, item2)
#' item3 <- item(a = 1, b = -.2, c = 0.4)
#' ip[[2]] <- item3
setMethod("[[<-", signature = c("Itempool", "numeric", "missing"),
          function(x, i, j, value)
          {
            # Make sure the 'value' is either Item or Testlet
            if (!is(value, "Testlet") && !is(value, "Item"))
              stop(paste0("Invalid assignment. All elements of the list ",
                          "should be an 'Item' or 'Testlet' object."))
            x@item_list[[i]] <- value
            x <- itempool(x@item_list)
            validObject(x)
            return(x)
          })


###############################################################################@
############################# $<- (Itempool) ###################################
###############################################################################@
#' Set values to parameters or components of 'Itempool' class.
#'
#' @param x \code{\link{Itempool-class}} object.
#' @param name Name of the parameter or component. Currently only \code{misc},
#'          \code{item_id}, \code{id}, \code{content}, \code{item_list} are
#'          available.
#' @param value The new value that will be assigned.
#'   \describe{
#'     \item{\strong{\code{'item_id'}}}{For \code{item_id}, the value should be
#'     a list of strings that has the same length as the number of items in the
#'     \code{\link{Itempool-class}} object, i.e. \code{ip$n$items}. There should
#'     not be any duplicated ID's. If there are \code{\link{Testlet-class}}
#'     objects in the item pool, the items within the testlet elements will be
#'     updated.
#'     }
#'     \item{\strong{\code{'id'}}}{For \code{id}, the value should be a list of
#'     strings that has the same length as the length of the
#'     \code{\link{Itempool-class}} object. There should not be any duplicated
#'     ID's. If there are only \code{\link{Item-class}} objects, then item ID's
#'     will be updated. If there are \code{\link{Testlet-class}} objects in the
#'     item pool, then only the testlet IDs will be updated. Items within the
#'     Testlet can be updated using \code{..$item_id}.
#'     }
#'     \item{\strong{\code{'content'}}}{For \code{content}, the value should be
#'     either \code{NULL} or a list of strings that has the same length as the
#'     length of the \code{\link{Itempool-class}} object.
#'     }
#'     \item{\strong{\code{'item_list'}}}{For \code{item_list}, the value should
#'     be a list of \code{\link{Item-class}} or \code{\link{Testlet-class}}
#'     objects.
#'     }
#'     \item{\strong{\code{'misc'}}}{For \code{misc}, the value should be a
#'     list.
#'     }
#'   }
#'
#' @return This operation will return an \code{\link{Itempool-class}} object.
#'
#' @importFrom methods slot<-
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#'
#' @examples
#' ip <- generate_ip(model = "3PL", n = 5)
#' ip$a
#' # Set new values for the a parameters
#' ip$a <- 2
#' # Set new values for the b parameters
#' ip$b <- -2:2
#' # Set new ids
#' ip$item_id <- paste0("my-item-", 5:9)
#'
#' # Set new item content
#' ip$content <- c("Geometry", "Algebra", "Algebra", "Geometry", "Geometry")
#'
#' # Add misc field to all items:
#' ip$difficulty <- c("Easy", "Easy", "Hard", "Hard", "Hard")
#' ip$difficulty
#'
#' # Add an overall misc field to itempool:
#' ip$form_name <- "Frm8"
#'
#' # Remove the misc field from all items
#' ip$difficulty <- NULL
#' ip$difficulty
#'
setMethod(
  "$<-", "Itempool",
  function(x, name, value) {
    switch(
      name,
      'id' = {
        # Check whether 'value' is character vector and the length of
        # the 'value' is equal to the length of the Itempool
        if (!is.character(value) || length(value) != length(x))
           stop(paste0("'id' should be a character vector with length ",
                       "equal to ", length(x), "."))
         # There shouldn't be a duplicated value.
         if (any(duplicated(value)))
           stop(paste0("'id' should not have any duplicated values. ",
                       "Check:\n", paste0("'", value[duplicated(value)],
                                          "'", collapse = ", ") ))
         for (i in seq_len(length(value))) {
    		   if (is(x[[i]], "Item")) {
                 x[[i]]@item_id <- value[i]
    		   } else if (is(x[[i]], "Testlet")) {
    		     x[[i]]@testlet_id <- value[i]
    		   } else stop("Invalid value. ")
         }
      },
      'item_id' = {
        # Check whether 'value' is character vector and the length of
        # the 'value' is equal to the length of the Itempool
        n_items <- x$n$items
        if (!is.character(value) || length(value) != n_items)
           stop(paste0("'item_id' should be a character vector with length ",
                       "equal to ", length(n_items), "."))
         # There shouldn't be a duplicated value.
         if (any(duplicated(value)))
           stop(paste0("'item_id' should not have any duplicated values. ",
                       "Check:\n", paste0("'", value[duplicated(value)],
                                          "'", collapse = ", ") ))
         item_counter <- 0
         for (i in seq_len(length(x@item_list))) {
           if (inherits(x@item_list[[i]], "Item")) {
             item_counter <- item_counter + 1
             x@item_list[[i]]@item_id <- value[item_counter]
             names(x@item_list)[i] <- value[item_counter]
           } else if (is(x@item_list[[i]], "Testlet")) {
             for (j in sequence(length(x@item_list[[i]]@item_list))) {
               item_counter <- item_counter + 1
               x@item_list[[i]]@item_list[[j]]@item_id <- value[item_counter]
               names(x@item_list[[i]]@item_list@item_list)[j] <-
                 value[item_counter]
             }
           } else stop("Invalid object type.")
         }
      },
      'testlet_id' = {
        stop("This has not been implemented yet.")
      },
      'content' = {
         # Check whether 'value' is either NULL or character vector and
         # the length of the 'value' is equal to the length of the Itempool
         if (!is.null(value) &&
             (!is.character(value) || !length(value) %in% c(1, length(x))
              ))
           stop(paste0("'content' should be a character vector with length",
                       " equal to ", length(x), "."))
         if (is.null(value)) {
           for (i in seq_len(length(x))) x[[i]]@content <- NULL
         } else if (length(value) == 1) {
           for (i in seq_len(length(x))) x[[i]]@content <- value
         } else
           for (i in seq_len(length(x))) x[[i]]@content <- value[i]
         },
      'item_list' = {
         if (!is(value, "Itempool") &&
             all(sapply(value, function(i) is.Item(i) ||
                        is(i, "Testlet")))) {
           x@item_list <- value
         } else
           stop(paste0("'item_list' should be a list of 'Item' or ",
                       "'Testlet' objects."))
         },
      'misc' = {x@misc <- value},
      ### ELSE ###
      {
        model <- x$model
        valid_slot_name <- FALSE
        # Check whether an item parameter values of the items desired
        # to be updated (assuming all item models are the same.).
        if (all(model == model[1])) {
          par_names <- names(PMODELS[[model[1]]]$parameters)
          se_par_names <- unlist(sapply(PMODELS[[model[1]]]$parameters,
                                        `[[`, "se"))
          if (name %in% c(par_names, se_par_names)) {
            valid_slot_name <- TRUE
            value <- rep(value, length.out = length(x))
            for (i in seq_len(length(x))) slot(x[[i]], name) <- value[i]
          }
        }
        # If 'name' is not a parameter name, then add it as a 'misc' field.
        if (!valid_slot_name) {

          # Function that adds misc field to an Item/Itempool/Testlet
          # object
          add_misc_field <- function(obj, val) {
            if (is.null(obj@misc)) obj@misc <- list()
            obj@misc[[name]] <- val
            return(obj)
          }
          # Add 'name' as misc value
          ip_size <- x$n
          if (length(value) == ip_size$elements) { # Assign value to
                                                   # testlets/items
            for (i in seq_len(length(x)))
              x[[i]] <- add_misc_field(x[[i]], value[[i]])
            # Assign value to standalone items and and items within testlets
          } else if (length(value) == ip_size$items) {
            counter <- 0
            for (i in seq_len(length(x))) {
              if (is(x[[i]], "Item")) {
                counter <- counter + 1
                x[[i]] <- add_misc_field(x[[i]], value[[counter]])
              } else if (is(x[[i]], "Testlet")) {
                for (j in length(x[[i]])) {
                  counter <- counter + 1
                  x[[i]]@item_list@item_list[[j]] <- add_misc_field(
                    x[[i]]@item_list@item_list[[j]], value[[counter]])
                }
              }
            }
          } else {

            # If the value is NULL, it means a misc field will need to be removed
            # This will only remove an existing misc field.
            if (is.null(value)) {
              if (name %in% names(x@misc)) {
                x <- add_misc_field(x, value)
              } else  {
                x@item_list <- lapply(x@item_list, function(itm) {
                  itm@misc[[name]] <- NULL;
                  if (length(itm@misc) == 0) itm@misc <- NULL;
                  itm})
              }
            } else {
              x <- add_misc_field(x, value)
            }
          }
        }
      }
    )
    validObject(x)
    return(x)
  }
  )



###############################################################################@
############################# $<- (Testlet) ####################################
###############################################################################@

#' Set values to parameters or components of \code{\link{Testlet-class}} object
#'
#' @param x An \code{\link{Testlet-class}} object.
#' @param name Name of the parameter or component.
#' @param value The new value that will be assigned.
#'
#' @return This operation will not return anything.
#'
#' @export
#'
#' @importFrom methods new slot<-
#'
#' @author Emre Gonulates
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#'
#' @examples
#' tlt <- generate_testlet()
#' tlt$testlet_id <- "New-Testlet-ID-111"
#' tlt$content <- "Algebra"
#' # Set all misc fields like this
#' tlt$misc <- list(passage_text = "This is a reading passage.",
#'                  passage_lexile = 450)
#'
#' tlt$passage_text
#' # Add a misc field
#' tlt$passage_language <- "En-US"
#'
#' # Remove a misc field
#' tlt$passage_language <- NULL
#'
setMethod(
  "$<-", "Testlet",
  function(x, name, value) {
    switch(name,
           'id' = {x@testlet_id <- value},
		       'testlet_id' = {x@testlet_id <- value},
           'content' = {x@content <- value},
           'misc' = {x@misc <- value},
           'item_list' = {if (is(value, "Itempool")) x@item_list <- value},
           # The default is checking item
           if (name %in% slotNames(x)) {
             warning("This parameter cannot be assigned by `$<-` operator.")
             # slot(x, name) <- value
           } else { # add/remove misc field
             if (name %in% names(x@parameters)) {
               x@parameters[[name]] <- value
             } else {
               if (is.null(x@misc)) x@misc <- list()
               x@misc[[name]] <- value
               if (length(x@misc) == 0) x@misc <- NULL;
             }
           }
           )
    if (validObject(x)) return(x)
  }
  )

###############################################################################@
############################# as.list (Itempool) ###############################
###############################################################################@
#' This function converts Itempool objects to a list object
#'
#' @param x an \code{\link{Itempool-class}} to be coerced to a list object
#' @param ... Additional parameters to be passed to the function.
#'
#' @return A list object with elements from 'Item' class.
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' item1 <- item(a = 1.12, b = -2.1, c = 0.28)
#' item2 <- item(a = 2, b = 3.2, c = 0.21)
#'
#' ip1 <- c(item1, item2)
#' as.list(ip1)
#'
as.list.Itempool <- function(x, ...) return(x@item_list)


###############################################################################@
############################# as.data.frame (Itempool) #########################
###############################################################################@
#' Convert an \code{\link{Itempool-class}} object into a \code{data.frame}.
#'
#' @description This function converts \code{\link{Itempool-class}} objects to a
#'   \code{data.frame} object.
#'
#' @param x An \code{\link{Itempool-class}} object
#' @param row.names \code{NULL} or a character vector giving the row names for
#'   the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting
#'   column names
#' @param ... additional arguments
#' @param include_se If \code{TRUE}, and items have \code{se},
#'   those will be included in the data frame.
#'
#' @return A data frame of items within each row. If all items cannot be
#'   coerced to a \code{data.frame}, an list of items will be returned and a
#'   warning will be raised.
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname as.data.frame
#'
#' @examples
#' ip1 <- generate_ip()
#' as.data.frame(ip1)
#'
#' ip2 <- generate_ip(n = 10, model = "GRM",
#'                    content = sample(c("G", "A"), 10, TRUE),
#'                    item_id = paste0("grm-i-", 1:10))
#' as.data.frame(ip2)
#'
#' t1 <- generate_testlet(n = 3, item_id_preamble = "t1")
#' t2 <- generate_testlet(n = 2, item_id_preamble = "t2")
#' ip3 <- c(ip1, t1, t2)
#' as.data.frame(ip3)
#'
#' ip4 <- c(ip2, ip3)
#' as.data.frame(ip4)
#'
#'
#' item1 <- item(a = 1.12, b = -2.1, c = 0.28)
#' item2 <- item(a = 2, b = 3.2, c = 0.21)
#'
#' ip1 <- c(item1, item2)
#' as.data.frame(ip1)
as.data.frame.Itempool <- function(x, row.names = NULL, optional = FALSE, ...,
                                   include_se = TRUE)
{
  args <- list(...)

  result <- lapply(x, as.data.frame, include_se = include_se)

  col_names <- lapply(result, names)
  col_names <- Reduce(union, col_names[order(sapply(col_names, length),
                                             decreasing = TRUE)])

  # All of the possible parameters in the correct order that should appear in df
  all_possible_par_names <- c("a", paste0("a", 1:100), "b", "c", "d",
                              paste0("b", 1:100), paste0("d", 1:100), "D")

  temp <- c("item_id", "model", intersect(all_possible_par_names, col_names))
  col_names <- c(temp, setdiff(col_names, temp))


  if ("testlet_id" %in% col_names)
    col_names <- c("item_id", "testlet_id", "model",
                   setdiff(col_names, c("item_id", "testlet_id", "model")))

  result <- lapply(
    result, function(r) {
      temp <- setdiff(col_names, names(r))
      temp <- setNames(rep(list(rep(NA, nrow(r))), length(temp)), temp)
      if (length(temp) > 0) do.call(
        "data.frame", lapply(c(r, temp), function(i)
          if (is.list(i)) I(i) else i)) else r
    })
  return(do.call(rbind, c(result, make.row.names = FALSE))[, col_names])
}


###############################################################################@
############################# as.data.frame (Testlet) ##########################
###############################################################################@
#' Convert an \code{\link{Testlet-class}} object into a \code{data.frame}.
#'
#' @description  This function converts \code{\link{Testlet-class}} objects to a
#'   \code{data.frame} object. If testlet has an ID, an additional column
#'   will be created for the testlet ID.
#'
#' @param x An \code{\link{Testlet-class}} object
#' @param row.names \code{NULL} or a character vector giving the row name for
#'   the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting
#'   column names
#' @param ... additional arguments
#' @param include_se If \code{TRUE}, and items have \code{se_parameters},
#'   those will be included in the data frame.
#'
#' @return A data frame representation of the item.
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname as.data.frame
#'
#' @examples
#' testlet1 <- generate_testlet()
#' as.data.frame(testlet1)
#' testlet2 <- generate_testlet(testlet_id = "T1")
#' as.data.frame(testlet2)
as.data.frame.Testlet <- function(x, row.names = NULL, optional = FALSE, ...,
                                  include_se = TRUE)
{
  output <- as.data.frame(x@item_list, include_se = include_se)
  if (!is.null(x@testlet_id))
    output <- cbind(testlet_id = x@testlet_id, output)
  return(output)
}

###############################################################################@
############################# is.ItemPool ######################################
###############################################################################@
#' Check whether an object is an  \code{\link{Itempool-class}} object
#'
#' @param x an object that is checked for being a member of 'Itempool' class
#'
#' @export
#'
#' @rdname is
#'
#' @author Emre Gonulates
#'
is.Itempool <- function(x){is(x,"Itempool")}


###############################################################################@
############################# length (Itempool) ################################
###############################################################################@
#' Find the length of an \code{\link{Itempool-class}} object
#'
#' @param x an \code{\link{Itempool-class}} object
#'
#' @export
#'
#' @rdname length
#'
#' @author Emre Gonulates
#'
setMethod(f = "length", signature = "Itempool",
          definition = function(x) length(x@item_list))



###############################################################################@
############################# $ method #########################################
###############################################################################@
#' Get slots of the an \code{\link{Itempool-class}} object.
#'
#' @param x An \code{\link{Itempool-class}} object.
#' @param name Name of the parameter.
#'          Available values:
#'          \describe{
#'            \item{\strong{\code{'id'}}}{Extract \code{id}'s of all items
#'              and testlets.
#'              This will not extract the \code{item_id}'s of items within the
#'              testlet.}
#'            \item{\strong{\code{'content'}}}{Extract \code{content}'s of
#'              all items and testlets.
#'              This will not extract the \code{content}'s of items within the
#'              testlet.}
#'            \item{\strong{\code{'model'}}}{Extract \code{model}'s of
#'              all items and testlets.
#'              This will not extract the \code{model}'s of items within the
#'              testlet. Use \code{$item_model} to extract models of standalone
#'              items.}
#'            \item{\strong{\code{'misc'}}}{Extract \code{misc} parameters of
#'              all items and testlets.
#'              This will not extract the \code{misc} parameters of items
#'              within the testlet.}
#'            \item{\strong{\code{'item_list'}}}{Extract individual elements of
#'              item pool. If there are testlets in the item pool, a testlet
#'              will be an item of the resulting list. If individual items
#'              within the testlet is desired to be elements of the list, then
#'              use \code{$items}.}
#'            \item{\strong{\code{'items'}}}{Extract individual items
#'              within the item pool. If there are testlets in the item pool
#'              individual elements of the testlet will be extracted. Resulting
#'              list will only consist of \code{\link{Item-class}} objects.
#'            }
#'            \item{\strong{\code{'parameters'}}}{Extract \code{parameters}'s of
#'              all items and testlets.
#'              This will not extract the \code{parameters}'s of items within
#'              the testlet.}
#'            \item{\strong{\code{'se'}}}{Extract
#'              \code{se}'s of all items and testlets.
#'              This will not extract the \code{se}'s of items
#'              within the testlet.}
#'            \item{\strong{\code{'n'}}}{Return a list with three objects:
#'              \code{elements} the number of standalone items and testlets.
#'              \code{testlets} the number of Testlet objects.
#'              \code{items} the sum of the number of items within testlets and
#'              standalone items.
#'              }
#'            \item{\strong{\code{'max_score'}}}{Returns the maximum possible
#'              raw score of the item pool.
#'              }
#'            \item{\strong{\code{'item_id'} or \code{'resp_id'}}}{Extract
#'              \code{item_id}'s of all standalone items and items within the
#'              testlets. It will not return \code{testlet_id}'s. This is
#'              mainly to get the \code{item_id}'s of items which has a
#'              response.
#'              }
#'            \item{\strong{\code{'testlet_id'}}}{Extract \code{testlet_id}'s
#'              of all items within the testlets. If the item is a standalone
#'              item, then a \code{NA} vector will be returned for it's testlet
#'              ID value.
#'              }
#'            \item{\strong{\code{'item_content'}}}{Extract
#'              \code{content}'s of all standalone items and items within the
#'              testlets. It will not return testlet \code{content}'s. This
#'              is mainly to get the \code{content}'s of items which has a
#'              response.
#'              }
#'            \item{\strong{\code{'item_model'}}}{Extract
#'              \code{model}'s of all standalone items and items within the
#'              testlets. It will not return testlet \code{model}'s. This is
#'              mainly to get the \code{model}'s of items which has a response.
#'              }
#'            \item{\strong{\code{'item_misc'}}}{Extract
#'              \code{misc} fields of all standalone items and items within
#'              the testlets. It will not return testlet \code{misc} fields.
#'              }
#'            \item{\strong{\code{'resp_item_list'}}}{Combine items that are
#'              not in a testlet and items within a testlet and return a list
#'              object. This list does not contain any \code{Testlet} objects.
#'              All of the elements are \code{Item} objects. If there are no
#'              testlets in the item pool, then this argument will be the
#'              same as \code{$item_list}.
#'              }
#'            \item{\strong{\code{'item_max_score'}}}{Extract the maximum score
#'              each standalone item can get.
#'              }
#'          }
#'
#' @return See the 'name' argument above for possible return values.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @include item-class.R
#'
#' @examples
#' ip <- generate_ip(n = 7, model = "3PL", content = c("Geometry", "Algebra"))
#'
#' ip$a
#' ip$b
#' ip$D
#' ip$model
#' ip$id
#' ip$content
#'
setMethod(
  "$", "Itempool",
  function(x, name) {
    switch(
      name,
      'id' = return(get_slot_itempool_cpp(ip = x, slotName = "id")),
      'content' = {
        result <- get_slot_itempool_cpp(ip = x, slotName = "content")
        if (!is.null(result))
          result <- stats::setNames(
            result, get_slot_itempool_cpp(ip = x, slotName = "id"))
        return(result)
        },
      'model' = return(sapply(x, function(s) ifelse(inherits(s, "Testlet"),
                                                   s@model, c(class(s))))),
      'misc' = {
        result <- lapply(x@item_list, function(i) i@misc)
        if (all(sapply(result, is.null))) result <- NULL
        return(result)
        },
      'item_list' = return(x@item_list),
      'items' = return(flatten_itempool_cpp(x)),
      'parameters' = {
        result <- as.data.frame(x)
        models <- result$model
        par_names <- lapply(PMODELS[models], function(s) names(s$parameters))
        cols <- grepl(paste0("^", Reduce(union, par_names), "[0-9]*$",
                             collapse = "|"), colnames(result))
        ids <- result$item_id
        result <- as.matrix(result[, cols, drop = FALSE])
        rownames(result) <- ids
        return(result)
        },
      'se' = {
        result <- as.data.frame(x)
        models <- result$model
        se_par_names <- lapply(PMODELS[models], function(s)
          unlist(sapply(s$parameters, `[[`, "se")))
        cols <- grepl(paste0("^", Reduce(union, se_par_names), "[0-9]*$",
                             collapse = "|"), colnames(result))
        if (any(cols)) {
          ids <- result$id
          result <- as.matrix(result[, cols, drop = FALSE])
          rownames(result) <- ids
        } else result <- NULL
         return(result)
        },
      'max_score' = return(sum(x$item_max_score)),
  	  'item_id' = {
  	      return(get_item_ids_itempool_cpp(x))
          # ids <- as.list(get_slot_itempool_cpp(ip = x, slotName = "id"))
          # for (j in which(sapply(x, is, "Testlet")))
          #   ids[[j]] <- x[[j]]@item_list$id
          # return(unlist(ids))
          },
      'resp_id' = return(x$item_id),
	    'testlet_id' = return(get_testlet_ids_itempool_cpp(x)),
      'n' = {
        ip_size <- get_itempool_size(x)
        return(list(elements = unname(ip_size["elements"]),
                    testlets = unname(ip_size["testlets"]),
                    items = unname(ip_size["items"])))
        },
      'item_content' = {
        content <- as.list(get_slot_itempool_cpp(ip = x, slotName = "content"))
        # Add content of the testlets. Here, if items of the testlet do not
        # have any content assigned, then they will be represented as NAs.
        for (j in which(sapply(x, is, "Testlet"))) {
          temp_content <- x[[j]]@item_list$content
          if (is.null(temp_content)) {
            content[[j]] <- rep(NA, length(x[[j]]@item_list))
          } else content[[j]] <- temp_content
        }
        return(stats::setNames(unlist(content), x$resp_id))
        },
      'item_model' = {
        models <- lapply(x, class)
        # models <- as.list(get_slot_itempool_cpp(ip = x, slotName = "model"))
        for (j in which(sapply(x, is, "Testlet")))
          models[[j]] <- x[[j]]@item_list$model
        return(stats::setNames(unlist(models), x$resp_id))
        },
      'item_misc' = {
        misc <- list()
        for (i in seq_along(x@item_list)) {
          if (is(x@item_list[[i]], "Testlet")) {
            temp_misc <- x@item_list[[i]]@item_list$misc
            if (is.null(temp_misc))
              temp_misc <- stats::setNames(
                vector("list", length(x@item_list[[i]]@item_list)),
                x@item_list[[i]]@item_list$item_id)
          } else {
            temp_misc <- stats::setNames(list(x@item_list[[i]]$misc),
                                         x@item_list[[i]]@item_id)
          }
          misc <- c(misc, temp_misc)
        }
        if (all(sapply(misc, is.null))) return(NULL)
        return(misc)
        },
      'resp_item_list' = {
        return(flatten_itempool_cpp(x))
        },
      'item_max_score' = {
        return(get_max_possible_score_itempool_cpp(x))
        },
      # The default is checking whether individual parameters and extracting
      # them if all of the models are equal.
      {
        result <- as.data.frame(x)
        if (name %in% colnames(result)) {
          return(stats::setNames(result[, name], x$resp_id))
        } else return(NULL)
      }
    )
  }
  )



###############################################################################@
############################# .print.Itempool ##################################
###############################################################################@
# Prints an \code{\link{Itempool-class}} object
#
# @keywords internal
#
# @param x An \code{\link{Itempool-class}} object to be printed.
# @param ... Additional arguments.
# @param n maximum number of items to print. Default is \code{NULL}, where
#   all items are printed if the number of items are smaller than 20, otherwise
#   only first 10 items are printed.
# @param print_header Whether to print the object class in the first line.
# @param base_print Whether to print the \code{\link{Itempool-class}} object
#   using the base printing capabilities. If FALSE, the function will look at
#   'tibble' package and tries to print the \code{\link{Itempool-class}} using
#   that function.
#
# @author Emre Gonulates
# @noRd
# @examples
# (ip <- generate_ip(model = "3PL", n = 20))
# (ip <- c(generate_ip(model = "3PL", n = 20),
#          generate_testlet(testlet_id = "t1"),
#          generate_testlet(testlet_id = "t2")))
.print.Itempool <- function(x, ..., n = NULL, print_header = TRUE,
                            base_print = FALSE) {
  # Make sure the printed object is valid
  tryCatch(validObject(x),
           error = function(e)
             stop(paste0("This is not a valid 'ItemPool' object:\n", e$message),
                  call. = FALSE))
  header_text <- c()
  if (print_header)
    header_text <- c(header_text, "An object of class 'Itempool'.\n")
    # cat("An object of class 'Itempool'.\n")

  result <- as.data.frame(x)

  # Get all possible item parameter names
  par_names <- unique(as.vector(unlist(sapply(
    PMODELS[unique(result$model)], function(i) names(i$parameters)))))

  # Check whether all item models are the same
  if (length(unique(result$model)) == 1) {
    header_text <- c(header_text,
                     paste0("Model of items: ", result$model[1], "\n"))
    # cat("Model of items: ", result$model[1], "\n", sep = "")
    # cat("Model of items: ", format_text(result$model[1], bold = TRUE), "\n",
    #     sep = "")
    # cat("Model of items: \033[1;m", result$model[1] , "\033[0;m\n",sep = "")
    result$model <- NULL
  }

  # Check if all D parameters are the same, if yes remove them.
  if (!is.null(result$D) && length(unique(result$D)) == 1) {
    header_text <- c(header_text, paste0("D = ", result$D[1],"\n"))
    # cat(paste0("D = ", result$D[1],"\n"))
    result$D <- NULL
  }

  # Check if all content are the same and, if yes remove them.
  if (!is.null(result$content) && length(unique(result$content)) == 1) {
    header_text <- c(header_text, paste0("Content = ", result$content[1],"\n"))
    # cat(paste0("Content = ", result$content[1],"\n"))
    result$content <- NULL
  }

  # Print common fields and remove them from the data frame shown.
  if (nrow(result) > 1) {
    remove_cols <- c()
    # col_names will hold the list of column names to be checked after removing
    # important column names and parameter names
    col_names <- colnames(result)
    for (i in par_names) {
      col_names <- col_names[!grepl(paste0("^", i, "[0-9]?$"), col_names)]
    }
    col_names <- setdiff(col_names, c("item_id", "testlet_id"))

    for (col_name in col_names) {
      if (length(unique(result[, col_name])) == 1) {
        header_text <- c(header_text,
                         paste0(col_name, " = ", result[1, col_name], "\n"))
        # cat(paste0(col_name, " = ", result[1, col_name], "\n"))
        remove_cols <- c(remove_cols, col_name)
      }
    }
    result[, remove_cols] <- NULL
  }

  # cat("\n")
  has_testlet <- any(sapply(x@item_list, is, "Testlet"))
  print_ip_size <- function(x) {
    if (has_testlet) {
      n_testlets <- x$n$testlets
      n_standalone_items <- x$n$elements - x$n$testlets
      n_all_items <- x$n$items
      n_digits <- nchar(as.character(n_all_items))

      text_after_df <- sprintf(paste0("%32s = %", n_digits,"d\n"),
                               "Number of Testlets", n_testlets)
      text_after_df <- paste0(
        text_after_df,
        sprintf(paste0("%32s = %", n_digits,"d\n"),
                "Number of items within Testlets",
                n_all_items - n_standalone_items))
      text_after_df <- paste0(
        text_after_df,
        sprintf(paste0("%32s = %", n_digits,"d\n"),
                "Number of standalone items",
                n_standalone_items))
      text_after_df <- paste0(
        text_after_df,
        sprintf(paste0("%32s = %", n_digits,"d\n"),
                "Total number of items", n_all_items))
    } else {
      text_after_df <- paste0("Total number of items = ", length(x), "\n")
    }
    return(text_after_df)
  }

  ## Print first n rows ##
  # if number of items are between 1-20, print all items. If number of items
  # are larger than 20, print first 10 items.
  n_all_items <- x$n$items
  if (is.null(n)) n <- ifelse(n_all_items <= 20, n_all_items, 10)

  print_tibble <- !base_print &&
    requireNamespace("pillar", quietly = TRUE) &&
    requireNamespace("tibble", quietly = TRUE)
  ### Print Header ###
  header_text <- c(header_text, "\n")
  if (print_tibble) {
    cat(pillar::style_subtle(header_text), sep = "")
  } else {
    cat(header_text, sep = "")
  }

  ### Print Item Parameters ###
  if (n < 1) {
    result <- ""
  } else if (n < x$n$items) {
    result <- result[1:n, ]
  }

  # First try tibble/pillar, if not use base R to print item parameters
  if (print_tibble) {
    setup_tbl <- pillar::tbl_format_setup(tibble::tibble(result),
                                          width = NULL,
                                          n = n,
                                          max_extra_cols = NULL,
                                          max_footer_lines = NULL)
    print(setup_tbl$body)
    if (setup_tbl$extra_cols_total > 0) {
      footer_extra_col_text <- paste0(
        setup_tbl$extra_cols_total, " more variable",
        ifelse(setup_tbl$extra_cols_total > 1, "s", ""), ": '",
        paste0(names(setup_tbl$extra_cols), collapse = "', '"), "'")
    } else  footer_extra_col_text <- ""

    # Base R print:
  } else {
    if (has_testlet) result$testlet_id[is.na(result$testlet_id)] <- ""
    print(result)
  }

  ### Print Footer ###

  if (n < 1) {
    text_after_df <- print_ip_size(x)
  } else if (n < x$n$items) {
    if (print_tibble && setup_tbl$extra_cols_total > 0) {
      text_after_df <- paste0(
        "# ... with ", x$n$items - n, " more items, and ",
        footer_extra_col_text, "\n", print_ip_size(x))
    } else
      text_after_df <- paste0(
        paste0("# ... with ", x$n$items - n, " more items\n"), print_ip_size(x))
  } else {
    if (print_tibble && setup_tbl$extra_cols_total > 0) {
      text_after_df <- paste0("# ... with ", footer_extra_col_text, "\n")
    } else {
      text_after_df <- paste0()
    }
  }

  if (print_tibble) {
    cat(pillar::style_subtle(text_after_df))
  } else {
    cat(text_after_df)
  }
}


###############################################################################@
############################# print.Itempool ###################################
###############################################################################@
#' Show an \code{\link{Itempool-class}} object
#'
#' @param x An \code{\link{Itempool-class}} object that will be showed.
#' @param ... Additional arguments. For example, an argument \code{n = 14},
#'   will print 14 items to the console.
#'
#' @export
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
#'
#' @examples
#' ip <- generate_ip(n = 42)
#' print(ip)
#' print(ip, n = 3)
#' print(ip, n = 12)
#' print(ip, n = Inf)
#'
setMethod("print", "Itempool", function(x, ...)  {
  args <- list(...)
  .print.Itempool(x = x,
                  n = switch("n" %in% names(args), args$n, NULL),
                  base_print = ifelse("base_print" %in% names(args),
                                      args$base_print, FALSE)
                  )
  })


###############################################################################@
############################# show.Itempool ####################################
###############################################################################@
#' Show an \code{\link{Itempool-class}} object
#'
#' @param object An \code{\link{Itempool-class}} object that will be showed.
#'
#' @export
#'
#' @rdname show
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
setMethod("show", "Itempool", function(object) {.print.Itempool(object)})


###############################################################################@
############################# as.Itempool ######################################
###############################################################################@
#' Coerce a given object to \code{\link{Itempool-class}} object
#'
#'
#' @description This function is a wrapper for \code{\link{itempool}} function.
#' It is recommended to use that function.
#'
#' @param ... The object that is desired to be converted to an  'Itempool'
#'          object. Also additional arguments related to the Itempool.
#'
#' @return An \code{\link{Itempool-class}} object.
#'
#' @include item-class.R
#' @include item-class-methods.R
#' @include itempool-class.R
#'
#'
#' @seealso \code{\link{itempool}}
#'
#' @export
#' @importFrom methods validObject
#'
#' @author Emre Gonulates
"as.Itempool" <- function(...)
{
  args <- list(...)
  return(do.call("itempool", args))
}


###############################################################################@
############################# convert_model (generic)  #########################
###############################################################################@
#' Convert model parameters from one model to another
#'
#' @name convert_model
#' @description
#' This is especially handy for converting IRT models with less parameters
#' (such as 1 parameter logistic model) to higher dimensional models such as
#' three parameter logistic model.
#'
#' @param ip An \code{\link{Item-class}} or \code{\link{Itempool-class}} object
#' @param target_model The target model that the conversion will be made.
#'
#' @include item-class.R
#'
#' @rdname convert_model
#'
#' @author Emre Gonulates
#'
#' @return An 'Item' object with new model parameters will be returned.
setGeneric("convert_model",
           def = function(ip, target_model = "3PL")
           {standardGeneric("convert_model")},
           useAsDefault =
             function(ip, target_model = "3PL")
             stop(paste0("Invalid object type. Only 'Item', 'Testlet' or ",
                          "'Itempool' types objects can be converted. "))
)


###############################################################################@
############################# convert_model (Item) #############################
###############################################################################@
#' Convert model parameters from one model to another.
#'
#' @rdname convert_model
#'
#' @export
#'
#' @importFrom methods new slot<-
#'
#' @author Emre Gonulates
#'
setMethod(
  f = "convert_model", signature = c(ip = "Item"),
  function(ip, target_model = "3PL") {
    available_target_models <- c(UNIDIM_DICHO_MODELS, "GPCM")
    if (!target_model %in% available_target_models)
      stop(paste0("Invalid target_model. Target model should be either: ",
                  paste0("'", available_target_models, "'", collapse = ", "),
                  "."))
    current_model <- ip$model
    # Both target and item belongs to unidimensional irt models
    if (target_model %in% UNIDIM_DICHO_MODELS &&
        current_model %in% UNIDIM_DICHO_MODELS) {
      # Create a dummy Item
      output <- new(target_model)
      for (p in intersect(slotNames(output), slotNames(ip)))
        slot(output, p) <- slot(ip, p)
    } else if (current_model == "GPCM2" && target_model == "GPCM") {
      output <- new("GPCM", a = ip$a, b = ip$b - ip$d, D = ip$D,
                    item_id = ip@item_id, content = ip@content, misc = ip@misc)
    } else
      stop("The Item cannot be converted to the target model.")
    validObject(output)
    return(output)
  })

###############################################################################@
############################# convert_model (Itempool) #########################
###############################################################################@
#' Convert model parameters from one model to another.
#'
#' @rdname convert_model
#'
#' @keywords internal
#'
#' @export
#'
#' @importFrom methods new
#'
#' @author Emre Gonulates
#'
setMethod(
  f = "convert_model", signature = c(ip = "Itempool"),
  function(ip, target_model = "3PL")
  {
    return(new(
      Class = "Itempool", item_list = lapply(
        ip@item_list, FUN = function(y) convert_model(
          ip = y, target_model = target_model))))
  })


###############################################################################@
############################# convert_model (Testlet) ##########################
###############################################################################@
#' Convert model parameters from one model to another.
#'
#' @rdname convert_model
#'
#' @keywords internal
#'
#' @export
#'
#' @author Emre Gonulates
#'
setMethod(
  f = "convert_model", signature = c(ip = "Testlet"),
  function(ip, target_model = "3PL")
  {
    ip@item_list <- convert_model(ip = ip@item_list,
                                  target_model = target_model)
    return(ip)
  })


###############################################################################@
############################# add_misc (generic)  ##############################
###############################################################################@
#' Add or change a named value to 'misc' slot of an \code{\link{Item-class}},
#' \code{\link{Itempool-class}} or \code{\link{Testlet-class}} object.
#'
#' @name add_misc
#'
#' @param ip An \code{\link{Item-class}}, \code{\link{Testlet-class}} or
#'   \code{\link{Itempool-class}} object.
#' @param value A list where each element should be named. Elements within the
#'   list will be added to 'misc' slot.
#'
#' @return An object with added 'misc' slot.
#'
#' @include item-class.R
#'
#' @rdname add_misc
#'
#' @author Emre Gonulates
#'
#' @export
#'
#' @examples
#' item <- item(b = 1)
#' add_misc(item, list(sympson_hetter_k = .75))
#'
setGeneric("add_misc",
           def = function(ip, value)
           {standardGeneric("add_misc")},
           useAsDefault =
             function(ip, value)
             stop(paste0("Invalid object type. Only 'Item', 'Testlet' type or ",
                          "'Itempool' type objects can be used. "))
)


# The following is a generic function of 'add_misc' since it is basically the
# same for 'Item', 'Itempool' or 'Testlet'
.add_misc <- function(ip, value) {
  if (!is(value, "list") || is.null(names(value)) || any(names(value) == ""))
    stop("The 'value' argument should be a named list element.")
  if (is.null(ip@misc)) ip@misc = list()
  for (i in seq_along(length(value)))
    ip@misc[[names(value[i])]] <- value[[i]]
  return(ip)
}

###############################################################################@
############################# add_misc (Item) ##################################
###############################################################################@
#' @rdname add_misc
#'
#' @export
#'
setMethod(
  f = "add_misc", signature = c(ip = "Item"),
  function(ip, value) {
    return(.add_misc(ip, value))
  })

###############################################################################@
############################# add_misc (Testlet) ###############################
###############################################################################@
#' @rdname add_misc
#'
#' @export
#'
setMethod(
  f = "add_misc", signature = c(ip = "Testlet"),
  function(ip, value) {
    return(.add_misc(ip, value))
  })


###############################################################################@
############################# add_misc (Itempool) ##############################
###############################################################################@
#' @rdname add_misc
#'
#' @export
#'
setMethod(
  f = "add_misc", signature = c(ip = "Itempool"),
  function(ip, value) {
    return(.add_misc(ip, value))
  })

