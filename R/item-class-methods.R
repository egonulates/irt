
###############################################################################@
############################# item #############################################
###############################################################################@
#' Create an \code{Item} object
#'
#' @description This function is used for creating \code{\link{Item-class}}
#'   objects.
#'
#' @param ... The item parameter arguments.
#' @param model The model that item \code{parameters} represents. Currently
#'   model can be:
#'   1PL, 2PL, 3PL, 4PL, M1PL, M2PL and M3PL, GRM, PCM or GPCM.
#'   Ideally, a model should be specified for the construction of an
#'   \code{\link{Item-class}} object.
#' @param item_id Item ID. Default value is \code{NULL}.
#' @param parameters A list containing numeric vectors that represent item
#'   parameters. Depending on the model these can change.
#' @param se A list object containing standard error of item parameters.
#' @param content Content information for item.
#' @param misc This slot is a list where one can put any information about
#'  the item. For example, one can enter the ID's of the enemies of the current
#'  item as \code{misc = list(enemies = c("i1", i2))}. Or, one can enter
#'  Sympson-Hetter exposure control parameter K:
#'  \code{misc = list(sympson_hetter_k = .75)}.
#'
#' @return An \code{\link{Item-class}} class object.
#'
#' @export
#'
#' @importFrom methods new slot<-
#'
#' @author Emre Gonulates
#'
#' @examples
#' # Create 2PL item:
#' item(a = 1.2, b = -0.94)
#' item(a = 1.2, b = -0.94, model = "2PL")
#' # Specify scaling constant D:
#' item(a = 1.2, b = -0.94, D = 1.7)
#'
#' # Add additional item specifications:
#' # Add item_id
#' item(a = 1.2, b = -0.94, item_id = "My-Item-1")
#' # Add content
#' item(a = 1.2, b = -0.94, item_id = "My-Item-1", content = "Geometry")
#' # Add additional parameter
#' item(a = 1.2, b = -0.94, misc = list(sympson_hetter_k = 1))
#' # Add any argument to 'misc' field
#' i1 <- item(a = 1.2, b = -0.94, item_id = "item1", content = "Earth Science",
#'            misc = list(key = "C", operational = TRUE, type = "MC",
#'                        enemies = c("i2", "i3")))
#' # Access fields
#' i1$misc
#' i1$misc$key
#' i1$misc$operational
#' i1$misc$enemies
#' i1$a
#' i1$b
#' i1$D
#' i1$parameters
#' i1$item_id
#' i1$content
#'
#'
#' # Rasch Model
#' item(b = 1.2)
#' item(b = 1.2, model = "Rasch")
#'
#' # 1PL model:
#' item(b = 1.2, model = "1PL")
#' item(b = 1.2, D = 1)
#'
#' # 3PL model:
#' item(a = 0.92, b = 2.7, c = 0.17)
#' item(a = 0.92, b = 2.7, c = 0.17, model = "3PL")
#' item(a = 0.92, b = 2.7, c = 0.17, D = 1.7, model = "3PL")
#'
#' # 4PL model:
#' item(a = 0.92, b = 2.7, c = 0.17, d = 0.98)
#' item(a = 0.92, b = 2.7, c = 0.17, d = 0.98, model = "4PL")
#' item(a = 0.92, b = 2.7, c = 0.17, d = 0.92, D = 1.7, model = "4PL")
#' item(parameters = list(a = 0.92, b = 2.7, c = 0.17, d = 0.92, D = 1.7),
#'      model = "4PL")
#'
#' # Create a GRM model
#' item(a = 1.9, b = c(-1, 0.82, 1.5), model = "GRM")
#' item(parameters = list(a = 1.9, b = c(-1, 2), D = 1), model = "GRM")
#'
#' # Create a GPCM model
#' item(a = 1.9, b = c(-1.6, -0.09, 1.25), model = "GPCM")
#' item(parameters = list(a = 1.9, b = c(-1, 2), D = 1), model = "GPCM")
#'
#' # Create a GPCM2 model (Reparameterized GPCM model)
#' item(a = 1.9, b = 0.65, d = c(-1.6, -0.09, 1.25), model = "GPCM2")
#' item(parameters = list(a = 1.9, b = 0.65, d = c(-1.6, -0.09, 1.25), D = 1.7),
#'      model = "GPCM2")
#'
#' # Create a PCM model
#' item(b = c(-0.7, 0.72, 1.9), model = "PCM")
#' item(parameters = list(b = c(-1, 2)), model = "PCM")
#'
#' # Add additional arguments to items
#' i1 <- item(a = 1.2, b = 2)
#' i1 <- item(i1, item_id = "new_item_id", content = "Algebra")
item <- function(..., model = NULL, item_id = NULL, parameters = NULL,
                 se = NULL, content = NULL, misc = NULL) {
  args <- list(...)
  clean_to_vector <- function(x) unname(unlist(as.vector(x)))

  # Check if the first element is already an Item object, then the other
  # slots can be updated.
  if (length(args) > 0 && is(args[[1]], "Item")) {
    object <-  args[[1]]
    args[[1]] <- NULL
    # Check if the model is NULL.
    # If it is NULL, first check whether parameters argument exists, if it does,
    # try to use that to get the item model. If 'parameters' argument is NULL
    # also, try the '...' argument to get item parameters and the model.
  } else if (is.null(model)) {
    # Check parameters, if it exists use it to find the model name. If it is
    # also NULL then try to get the arguments '...' and match the model
    # that matches the most.
    if (is.null(parameters)) {
      if (is.null(names(args))) {
        stop(paste0("Insufficient parameters. Please give more information ",
                    "about the item parameters or specify the model ",
                    "explicitly."))
      }
      # First check whether there is b parameter, it means it is either UIRT
      # models or PCM, GRM or GPCM. Actually since GRM and GPCM has the same
      # item parameter names, if there is 'a' value and 'b' values then it
      # will be assumed that the item is "GRM". In order to create a GPCM,
      # user must specify 'model = "GPCM"'.
      if ("b" %in% names(args)) {
        # Check the length of "b" parameter, if it is 1, then it is a
        # dichotomous IRT model. Otherwise, it is a polytomous IRT model
        # Dichotomous item
        if (length(args$b) == 1 && is.numeric(clean_to_vector(args$b))) {
          # Check if "a" parameter exists
          if ("a" %in% names(args)) { # 2PL, 3PL or 4PL
            # parameters <- list(a = clean_to_vector(args$a),
            #                    b = clean_to_vector(args$b))
            if ("c" %in% names(args)) { # 3PL or 4PL
              # parameters$c <- clean_to_vector(args$c)
              if ("d" %in% names(args)) { # 4PL
                object <- new("4PL",
                              a = clean_to_vector(args$a),
                              b = clean_to_vector(args$b),
                              c = clean_to_vector(args$c),
                              d = clean_to_vector(args$d))
                args$a <- NULL
                args$b <- NULL
                args$c <- NULL
                args$d <- NULL
              } else {# 3PL
                object <- new("3PL",
                              a = clean_to_vector(args$a),
                              b = clean_to_vector(args$b),
                              c = clean_to_vector(args$c))
                args$a <- NULL
                args$b <- NULL
                args$c <- NULL
              }
            } else {# 2PL
              object <- new("2PL",
                            a = clean_to_vector(args$a),
                            b = clean_to_vector(args$b))
              args$a <- NULL
              args$b <- NULL
            }
            # Check if D parameter exists:
            if ("D" %in% names(args)) {
              object@D <- clean_to_vector(args$D)
              args$D <- NULL
            }
          } else if ("D" %in% names(args)) { # 1PL
            object <- new("1PL",
                          b = clean_to_vector(args$b),
                          D = clean_to_vector(args$D))
            args$b <- NULL
            args$D <- NULL
          } else {# Rasch model
            object <- new("Rasch",
                          b = clean_to_vector(args$b))
            args$b <- NULL
          }
        } else {# Polytomous item.
          # If there is "a" parameter then it is a "GRM" otherwise it is a
          # "PCM" item
          if ("a" %in% names(args)) { # A "GRM" item
            object <- new("GRM",
                          a = clean_to_vector(args$a),
                          b = clean_to_vector(args$b),
                          D = ifelse("D" %in% names(args),
                                     clean_to_vector(args$D),
                                     DEFAULT_D_SCALING))
            args$a <- NULL
            args$b <- NULL
            args$D <- NULL
          } else {# A "PCM" item
            object <- new("PCM",
                          b = clean_to_vector(args$b))
            args$b <- NULL
          }
        }
      } else if (all(c("d", "a")  %in% names(args))) {# It is M2PL or M3PL model
        if ("c" %in% names(args)) { # M3PL
          object <- new("M3PL",
                        a = clean_to_vector(args$a),
                        d = clean_to_vector(args$d),
                        c = clean_to_vector(args$c))
          args$a <- NULL
          args$d <- NULL
          args$c <- NULL
        } else {# M2PL
          object <- new("M2PL",
                        a = clean_to_vector(args$a),
                        d = clean_to_vector(args$d))
          args$a <- NULL
          args$d <- NULL
        }
        object@D <- ifelse("D" %in% names(args), clean_to_vector(args$D),
                           DEFAULT_D_SCALING_MIRT)
        args$D <- NULL
      } else {
        stop(paste0("Insufficient parameters. Please give more information ",
                    "about the item parameters or specify the model ",
                    "explicitly."))
      }
    } else {# parameters is not NULL
      # Coerce parameters to "..." and rerun the function. The parameters in
      # "..." will be ignored. If there is multiple parameters in "..." and
      # "parameters" argument raise error.

      # Check whether "..." and "parameters" conflict
      if (is.null(names(parameters)) ||
          (!is.null(names(args)) && any(names(parameters) %in% names(args)))) {
        stop(paste0("Conflicting parameters in '...' and 'parameters' ",
                    "argument."))
      }
      # pass the function to the item function as argument
      # args <- parameters
      return(do.call(what = "item",
                     args = c(parameters,
                              model = model, item_id = item_id,
                              parameters = NULL, se = se, content = content,
                              misc = list(c(misc, args)))))
    }
    if ("D" %in% names(args)) args[["D"]] <- NULL

    # If there is a model and it is valid then create an item based on it.
  } else if (model %in% names(PMODELS)) {
    # Check whether 'parameters' argument is null or not. If it is NULL,
    # then look for the item parameters in the '...', except "D". If "D" is
    # missing by default, make it DEFAULT_D_SCALING or DEFAULT_D_SCALING_MIRT.
    # If it is not NULL, check whether it is valid. If 'parameters' argument
    # is valid, then use it in the model. If not raise an error.

    par_names <- names(PMODELS[[model]]$parameters)
    if (is.null(parameters)) {
      parameters <- list()

      par_sizes <- sapply(PMODELS[[model]]$parameters, function(i) i$size)

      error_message <- paste0(
          "Incomplete parameters. Make sure to provide parameters in the ",
          "following format: \nitem(model = '", model, "', ",
          paste0(par_names, collapse = " = , "), " = , .....)")
      if ("D" %in% par_names && !"D" %in% names(args))
        args[["D"]] <- DEFAULT_D_SCALING

      # Check for parameters like 'b' that might be entered as b1, b2, b3, ...
      for (i in 1:length(par_sizes)) {
        par_name <- names(par_sizes)[i]
        if (is.null(names(args)) ||
            (!par_name %in% names(args) && par_name != "D")) {
          # Check whether the names of the parameters are presented
          # as: b1, b2, b3 etc
          if (par_sizes[i] > 1 && !par_name %in% names(args) &&
              any(temp <- grepl(pattern = paste0("^", par_name, "[0-9]+$"),
                                names(args))) &&
              all(paste0(par_name, 1:sum(temp)) %in% names(args))
              ) {
            temp <- names(args[temp])
            args[[par_name]] <- clean_to_vector(unlist(args[temp][order(temp)]))
            args[temp] <- NULL
          } else stop(error_message, call. = FALSE)
        }
      }
      object <- do.call(new, args = c(Class = model,
                                      lapply(args[par_names], clean_to_vector)))
    } else if (
      is.list(parameters) &&
      (
        # Full item parameters with D
        (length(parameters) == length(par_names) &&
         all(par_names %in% names(parameters))) ||
        # Full item parameters without D is fine, simply set D to default later
        ("D" %in% par_names &&
         length(parameters) == (length(par_names) - 1) &&
         all(setdiff(par_names, "D") %in% names(parameters)))
        )
      ) {
      # if scaling parameter is necessary but it is not present, set a default
      # value to it
      if (is.null(parameters$D) && "D" %in% par_names)
        parameters$D <- DEFAULT_D_SCALING
      # object <- new("Item", model = model,
      #               parameters = lapply(parameters, clean_to_vector))

      object <- do.call(new, args = c(Class = model, parameters))

      # Raise an error if "parameters" argument is not valid:
    } else {
       stop(paste0("Invalid item parameters. Item parameters for the model '",
                   model , "' should be a list with the following names: ",
                   paste0("'", par_names, "'", collapse = ", "), "."))
    }
    if ("D" %in% names(args)) args[["D"]] <- NULL
  } else  {
    stop(paste0("Invalid 'model' specification. Model name should be one of ",
                "the following: ",
                paste0(names(PMODELS), collapse = ", "), "."))
  }

  # Add additional arguments

  ### item_id ###
  # item_id
  if (!is.null(item_id)) {
    object@item_id <- item_id
  } else if ("item_id" %in% tolower(names(args))) {
    for (id_name in c("item_id", "ITEM_ID", "Item_id", "Item_ID", "Item_Id"))
      if (id_name %in% names(args)) {
        object@item_id <- args[[id_name]]
        args[[id_name]] <- NULL
        break
      }
  }
  # else if ("id" %in% tolower(names(args))) {
  #   for (id_name in c("ID", "Id", "iD"))
  #     if (id_name %in% names(args)) {
  #       object@item_id <- args[[id_name]]
  #       args[[id_name]] <- NULL
  #       break
  #     }
  # }

  ### D ###
  if ("D" %in% names(args) &&
      # "D" is indeed one of the parameters
      "D" %in% names(PMODELS[[class(object)]]$parameters)) {
    object@D <- args$D
  }

  ### SE Parameters ###
  se_par_names <- unlist(sapply(PMODELS[[class(object)]]$parameters,
                                function(i) i$se))
  if (!is.null(se) &&
      is.list(se) &&
      length(se) == length(se_par_names) &&
      (all(names(se) %in% se_par_names) ||
       all(names(se) %in% names(se_par_names)))
      ) {
    for (i in 1:length(se_par_names)) {
      slot(object, se_par_names[i]) <- if (
        se_par_names[i] %in% names(se))
        clean_to_vector(se[[se_par_names[i]]]) else if (
        names(se_par_names)[i] %in% names(se))
          clean_to_vector(se[[names(se_par_names)[i]]]) else NULL
    }
  }
  # Add standard error of parameters if they are presented in arguments as
  # "se_a" or "se_b", etc.
  if (any(se_par_names %in% names(args))) {
    for (i in which(se_par_names %in% names(args))) {
      slot(object, se_par_names[i]) <- clean_to_vector(args[[se_par_names[i]]])
    }
  }

  ### Content ###
  if (!is.null(content)) object@content <- content

  ### Misc ###
  # Remove all of the slots or parameter names from 'args'
  args <- args[setdiff(names(args), slotNames(object))]
  # Put all of the arguments as misc field.
  if (is.null(misc)) {
    if (length(args) > 0) object@misc <- args
  } else if (length(args) > 0) {
    object@misc <- c(misc, args)
  } else
    object@misc <- misc

  # Check the validity of the object and return the object if it is valid, else
  # raise an error.
  if (validObject(object))
    return(object)
}

###############################################################################@
############################# is.Item ##########################################
###############################################################################@
#' Check whether an object is an \code{\link{Item-class}}
#' @param x an object that is checked for whether being a
#'   \code{\link{Item-class}} object or not
#'
#' @export
#'
#' @importFrom methods is
#'
#' @rdname is
#'
#' @author Emre Gonulates
#'
#' @examples
#' i1 <- item(a = 1, b = 2)
#' is.Item(i1)
#' # Alternatively:
#' is(i1, "Item")
#'
#' # Not an item:
#' is.Item("abc")
#'
is.Item <- function(x){is(x,"Item")}


###############################################################################@
############################# print.Item #######################################
###############################################################################@
#' Print an \code{\link{Item-class}} object
#'
#' @param x An \code{\link{Item-class}} object to be printed.
#' @param ... Passed parameters.
#'
#' @author Emre Gonulates
#'
#' @keywords internal
#'
print.Item <- function(x, ...)
{
  model_name <- c(class(x))
  cat(paste0("A '", model_name, "' item.\n"))
  if (!is.null(x@item_id))
    # cat(paste0(format_text("ID:", italic = TRUE), "      ", x@item_id, "\n"))
    cat(paste0("Item ID:", "      ", x@item_id, "\n"))
    # cat("\033[3;mID:      \033[0;m", x@item_id, "\n",sep = "")

  # Color print: https://stackoverflow.com/a/57031762/2275286
  cat(paste0("Model:", "   ", model_name, " (",
             PMODELS[[model_name]]$verbose_name,")\n"))
  # cat(paste0(format_text("Model:", italic = TRUE), "   ",
  #            format_text(model_name, bold = TRUE), "\n"))
  # cat("\033[3;mModel:  \033[0;m \033[1;m", x@model , "\033[0;m\n",sep = "")

  if (!is.null(x@content))
    cat(paste0("Content:", " ",
               ifelse(length(x@content) == 1, paste0(x@content),
                      paste0(x@content, collapse = "; ")), "\n"))
    # cat(paste0(format_text("Content:", italic = TRUE), " ",
    #            ifelse(length(x@content) == 1, paste0(x@content),
    #                   paste0(x@content, collapse = "; ")), "\n"))
    # cat("\033[3;mContent:\033[0;m ", ifelse(length(x@content) == 1,
    #                              paste0(x@content),
    #                              paste0(x@content, collapse = "; ")),
    #     "\n",sep = "")
  cat(paste0("Model Parameters:", "\n"))
  # cat(paste0(format_text("Model Parameters:", italic = TRUE), "\n"))
  # cat("\033[3;mModel Parameters:\033[0;m\n")
  par_names <- names(PMODELS[[model_name]]$parameters)

  for (p in par_names) {
    temp_se <- PMODELS[[model_name]]$parameters[[p]]$se
    if (!is.null(temp_se) && !is.null(temp_se <- slot(x, temp_se)))
      temp_se <- paste0(" (S.E. = ", temp_se, ")")
    cat(sprintf(" %2s = %s\n", p, paste0(slot(x, p), temp_se,
                                         collapse = ";  ")))
  }
  # se_par_names <- unlist(sapply(PMODELS[[model_name]]$parameters, `[[`, "se"))
  # pars <- unlist(sapply(par_names, function(i) slot(x, i)))
  # print(pars)
  cat("\n")

  print_with_quotes <- function(x)
    ifelse(test = is(x, "character"), yes = paste0("\"", x, "\""),
           no = paste0(x))
  if (!is.null(x@misc)) {
    # cat("\033[3;mMisc:\033[0;m \n",sep = "")
    cat("Misc:", "\n")
    # cat(format_text("Misc:", italic = TRUE), "\n")
    for (i in seq_len(length(x@misc))) {
      if (length(x@misc[[i]]) == 1) {
        cat(paste0("  ", names(x@misc)[i], ": ",
                   print_with_quotes(unlist(x@misc[i])), "\n"))
      } else
        cat(paste0("  ", names(x@misc)[i], ": ",
                   paste0(sapply(unlist(x@misc[i]), print_with_quotes),
                          collapse = ", "), "\n"))
    }
  }
  cat("--------------------------")
}


###############################################################################@
############################# show.Item ########################################
###############################################################################@
#' Show an \code{\link{Item-class}} object
#'
#' @param object An \code{\link{Item-class}} object.
#'
#' @export
#'
#' @keywords internal
#'
#' @rdname show
#'
#' @author Emre Gonulates
#'
#' @importFrom methods show
#'
setMethod("show", "Item", function(object) {print.Item(object)})




###############################################################################@
############################# $ method #########################################
###############################################################################@
#' Get slots from an \code{\link{Item-class}} object.
#'
#' @param x An \code{\link{Item-class}} object.
#' @param name Name of the parameter.
#'
#'   Available values:
#'   \describe{
#'     \item{\strong{\code{'item_id'}}}{Extract \code{'item_id'} of an
#'       \code{\link{Item-class}} object.}
#'     \item{\strong{\code{'id'}}}{Extract \code{'item_id'} of an
#'       \code{\link{Item-class}} object.}
#'     \item{\strong{\code{'model'}}}{Extract the \code{'model'} of an
#'       \code{\link{Item-class}} object.}
#'     \item{\strong{\code{'parameters'}}}{Extract the \code{'parameters'} of an
#'       \code{\link{Item-class}} object.}
#'     \item{\strong{\code{'se_parameters'}}}{Extract the standard error of
#'       parameters of an \code{\link{Item-class}} object.}
#'     \item{\strong{\code{'content'}}}{Extract the \code{'content'} slot of an
#'       \code{\link{Item-class}} object.}
#'     \item{\strong{\code{'misc'}}}{Extract the \code{'misc'} slot of an
#'       \code{\link{Item-class}} object.}
#'     \item{\strong{\code{'max_score'}}}{Extract the maximum possible score
#'       of an \code{\link{Item-class}} object. Minimum score is assumed to be
#'       0.}
#'   }
#'
#' @return This operation will return the desired slot.
#'
#' @importFrom stats setNames
#' @importFrom methods slot slotNames
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @include item-class.R
#'
#' @examples
#' item1 <- item(model =  "3PL", item_id = 'item23', content = 'Geometry',
#'               misc = list(enemies = c("item1", "item2"), key = "C"),
#'               parameters = list(b = 2, c = .12, a = 1.2, D = 1))
#' # Get individual parameters
#' item1$a
#' item1$b
#' item1$D
#' # Get item 'model'
#' item1$model
#' # Get all parameters
#' item1$parameters
#' # Get item ID
#' item1$item_id
#' # Get item content
#' item1$content
#' # Get misc values
#' item1$misc
#' # Get maximum possible score of item
#' item1$max_score
#'
#' # Get elements of misc directly:
#' item1$misc$key  # "C"
#' item1$key  # "C"
#'
setMethod("$", "Item",
          function(x, name)
          {
            switch(name,
                   "model" = return(c(class(x))),
                   "id" = return(x@item_id),
                   "parameters" = return(setNames(lapply(names(
                     PMODELS[[class(x)]]$parameters), function(n)
                       slot(x, n)), names(PMODELS[[class(x)]]$parameters))),
                   "se" = {
                     se_par_names <- unlist(sapply(
                       PMODELS[[class(x)]]$parameters, `[[`, "se"))
                     return(setNames(lapply(se_par_names, slot, object = x),
                                     se_par_names))
                     },
                   "max_score" = return(get_max_possible_score_item_cpp(x)),
                   {
                     if (name %in% slotNames(x)) {
                       slot(x, name)
                       } else x@misc[[name]]
                   }
                   )
          })



###############################################################################@
############################# $<- method #######################################
###############################################################################@
#' Set values to parameters or components of \code{\link{Item-class}} object
#'
#' @param x An \code{\link{Item-class}} object.
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
#'
#' @examples
#' itm <- new("3PL", item_id = 'item23', content = 'Geometry',
#'             misc = list(enemies = c("item1", "item2")),
#'             b = 2, c = .12, a = 1.2, D = 1)
#' itm$a <- 2
#' itm$D <- 1.7
#' itm$item_id <- "Item-111"
#' itm$content <- 'Algebra'
#' itm$se_a <- 2.2
#' # Set all misc fields like this
#' itm$misc <- list(enemies = c("item5"), strands = c("A4", "C2"))
#'
#' # Add a misc field
#' itm$key <- "C"
#'
#' # Remove a misc field
#' itm$enemies <- NULL
#'
setMethod(
  "$<-", "Item",
  function(x, name, value) {
    switch(name,
           'id' = {x@item_id <- value},
		       'item_id' = {x@item_id <- value},
           'content' = {x@content <- value},
           'misc' = {x@misc <- value},
           # The default is checking item
           if (name %in% slotNames(x)) {
             slot(x, name) <- value
           } else {
             if (is.null(x@misc)) x@misc <- list()
             x@misc[[name]] <- value
           }
           )
    if (validObject(x)) return(x)
  }
  )



###############################################################################@
###############################################################################@
###############################################################################@
############################# as.data.frame ####################################
###############################################################################@
###############################################################################@
###############################################################################@


###############################################################################@
############################# as.data.frame (Item) #############################
###############################################################################@
#' Convert an \code{\link{Item-class}} object into a \code{data.frame}.
#'
#' @description  This function converts \code{\link{Item-class}} objects to a
#'   \code{data.frame} object.
#'
#' @param x An \code{\link{Item-class}} object
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
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname as.data.frame
#'
#' @examples
#' item1 <- generate_item()
#' as.data.frame(item1)
#'
#' item2 <- generate_item(model = "Rasch", item_id = "i1",
#'                        misc = list(type = "MC", op = TRUE, c("i1", "i2")))
#' as.data.frame(item2)
#'
#' item3 <- generate_item(model = "GRM")
#' as.data.frame(item3)
#'
as.data.frame.Item <- function(x, row.names = NULL, optional = FALSE, ...,
                               include_se = TRUE)
{
  model_name <- c(class(x))
  if (is.null(x@item_id))
    result <- data.frame(model = model_name) else
      result <- data.frame(item_id = x@item_id, model = model_name)

  for (s in setdiff(slotNames(x), c("item_id", "misc")))
    if (!is.null(slot(x, s))) result[[s]] <- slot(x, s)
  # par_names <- names(PMODELS[[model_name]]$parameters)
  # par_sizes <- unlist(sapply(PMODELS[[model_name]]$parameters, `[[`, "size"))
  # se_par_names <- unlist(sapply(PMODELS[[model_name]]$parameters, `[[`, "se"))
  if (!include_se) {
    se_par_names <- unlist(sapply(PMODELS[[model_name]]$parameters, `[[`, "se"))
    result <- result[, !colnames(result) %in% se_par_names]
  }

  for (i in seq_along(slot(x, "misc"))) {
    if (names(x@misc)[i] != "") {
      if (length(x@misc[[i]]) == 1) {
        result[[names(x@misc)[i]]] <- x@misc[[i]]
      } else {
        result[[names(x@misc)[i]]] <- I(list(x@misc[[i]]))
      }
    }
  }
  # for (i in seq_along(slot(x, "misc")))
  #   if (length(x@misc[[i]]) == 1 && names(x@misc)[i] != "")
  #     result[[names(x@misc)[i]]] <- x@misc[[i]]
  return(result)
}


#' This function is for models where some of the parameters needs to be written
#' in long format.
#' @param x An item object
#' @param long_pars A string vector listing the parameters that has parameter
#'   sizes longer than 1.
#' @param include_se Whether to include standard error parameters or not.
#' @noRd
#'
.as_data_frame_item_long_pars <- function(x, long_pars, include_se = TRUE) {
  if (is.null(x@item_id))
    result <- data.frame(model = c(class(x))) else
      result <- data.frame(item_id = x@item_id, model = c(class(x)))
  for (s in setdiff(slotNames(x), c("item_id", "misc"))) {
    value <- slot(x, s)
    if (!is.null(value)) {
      if (s %in% long_pars) {
        for (i in 1:length(value)) result[[paste0(s, i)]] <- value[i]
      } else result[[s]] <- value
    }
  }
  if (!include_se) {
    se_par_names <- unlist(sapply(PMODELS[[result$model]]$parameters,
                                  `[[`, "se"))
    result <- result[, !grepl(paste0("^", se_par_names, collapse = "|"),
                              colnames(result))]
  }

  # Add Misc
  for (i in seq_along(slot(x, "misc"))) {
    # if (length(x@misc[[i]]) == 1 && names(x@misc)[i] != "")
    if (names(x@misc)[i] != "") {
      if (length(x@misc[[i]]) == 1) {
        result[[names(x@misc)[i]]] <- x@misc[[i]]
      } else {
        result[[names(x@misc)[i]]] <- I(list(x@misc[[i]]))
      }
    }
  }
  return(result)
}

###############################################################################@
############################# as.data.frame.GRM ################################
###############################################################################@
#' Convert a \code{\link{GRM-class}} object into a \code{data.frame}.
#'
#' @param x An \code{\link{GRM-class}} object
#' @param row.names \code{NULL} or a character vector giving the row name for
#'   the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting
#'   column names
#' @param ... additional arguments
#' @param include_se If \code{TRUE}, and items have \code{se_parameters},
#'   those will be included in the data frame.
#'
#' @return A data frame representation of the GRM item.
#'
#' @include item-class.R
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname as.data.frame
#'
#' @examples
#' item1 <- generate_item(model = "GRM", item_id = "i1")
#' as.data.frame(item1)
as.data.frame.GRM <- function(x, row.names = NULL, optional = FALSE, ...,
                              include_se = TRUE) {
  .as_data_frame_item_long_pars(x = x, long_pars = c("b", "se_b"),
                                include_se = include_se)
}

###############################################################################@
############################# as.data.frame.PCM ################################
###############################################################################@
#' Convert a \code{\link{PCM-class}} object into a \code{data.frame}.
#'
#' @param x An \code{\link{PCM-class}} object
#' @param row.names \code{NULL} or a character vector giving the row name for
#'   the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting
#'   column names
#' @param ... additional arguments
#' @param include_se If \code{TRUE}, and items have \code{se_parameters},
#'   those will be included in the data frame.
#'
#' @return A data frame representation of the PCM item.
#'
#' @include item-class.R
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname as.data.frame
#'
#' @examples
#' item1 <- generate_item(model = "PCM", item_id = "i1")
#' as.data.frame(item1)
as.data.frame.PCM <- function(x, row.names = NULL, optional = FALSE, ...,
                              include_se = TRUE) {
  .as_data_frame_item_long_pars(x = x, long_pars = c("b", "se_b"),
                                include_se = include_se)
}


###############################################################################@
############################# as.data.frame.GPCM ###############################
###############################################################################@
#' Convert a \code{\link{GPCM-class}} object into a \code{data.frame}.
#'
#' @param x An \code{\link{GPCM-class}} object
#' @param row.names \code{NULL} or a character vector giving the row name for
#'   the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting
#'   column names
#' @param ... additional arguments
#' @param include_se If \code{TRUE}, and items have \code{se_parameters},
#'   those will be included in the data frame.
#'
#' @return A data frame representation of the GPCM item.
#'
#' @include item-class.R
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname as.data.frame
#'
#' @examples
#' item1 <- generate_item(model = "GPCM", item_id = "i1")
#' as.data.frame(item1)
as.data.frame.GPCM <- function(x, row.names = NULL, optional = FALSE, ...,
                              include_se = TRUE) {
  .as_data_frame_item_long_pars(x = x, long_pars = c("b", "se_b"),
                                include_se = include_se)
}

###############################################################################@
############################# as.data.frame.GPCM2 ##############################
###############################################################################@
#' Convert a \code{\link{GPCM2-class}} object into a \code{data.frame}.
#'
#' @param x An \code{\link{GPCM2-class}} object
#' @param row.names \code{NULL} or a character vector giving the row name for
#'   the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting
#'   column names
#' @param ... additional arguments
#' @param include_se If \code{TRUE}, and items have \code{se_parameters},
#'   those will be included in the data frame.
#'
#' @return A data frame representation of the GPCM2 item.
#'
#' @include item-class.R
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname as.data.frame
#'
#' @examples
#' item1 <- generate_item(model = "GPCM2", item_id = "i1")
#' as.data.frame(item1)
as.data.frame.GPCM2 <- function(x, row.names = NULL, optional = FALSE, ...,
                              include_se = TRUE) {
  .as_data_frame_item_long_pars(x = x, long_pars = c("d", "se_d"),
                                include_se = include_se)
}

###############################################################################@
############################# as.data.frame.M2PL ###############################
###############################################################################@
#' Convert a \code{\link{M2PL-class}} object into a \code{data.frame}.
#'
#' @param x An \code{\link{M2PL-class}} object
#' @param row.names \code{NULL} or a character vector giving the row name for
#'   the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting
#'   column names
#' @param ... additional arguments
#' @param include_se If \code{TRUE}, and items have \code{se_parameters},
#'   those will be included in the data frame.
#'
#' @return A data frame representation of the M2PL item.
#'
#' @include item-class.R
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname as.data.frame
#'
#' @examples
#' item1 <- generate_item(model = "M2PL", item_id = "i1")
#' as.data.frame(item1)
as.data.frame.M2PL <- function(x, row.names = NULL, optional = FALSE, ...,
                              include_se = TRUE) {
  .as_data_frame_item_long_pars(x = x, long_pars = c("a", "se_a"),
                                include_se = include_se)
}

###############################################################################@
############################# as.data.frame.M3PL ###############################
###############################################################################@
#' Convert a \code{\link{M3PL-class}} object into a \code{data.frame}.
#'
#' @param x An \code{\link{M3PL-class}} object
#' @param row.names \code{NULL} or a character vector giving the row name for
#'   the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting
#'   column names
#' @param ... additional arguments
#' @param include_se If \code{TRUE}, and items have \code{se_parameters},
#'   those will be included in the data frame.
#'
#' @return A data frame representation of the M3PL item.
#'
#' @include item-class.R
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname as.data.frame
#'
#' @examples
#' item1 <- generate_item(model = "M3PL", item_id = "i1")
#' as.data.frame(item1)
as.data.frame.M3PL <- function(x, row.names = NULL, optional = FALSE, ...,
                              include_se = TRUE) {
  .as_data_frame_item_long_pars(x = x, long_pars = c("a", "se_a"),
                                include_se = include_se)
}




###############################################################################@
############################# get_slot_names_item ##############################
###############################################################################@
#' Get slot names of the Item object
#'
#' @param x An \code{\link{Item-class}} object
#' @param type A string that specifies which slots to be extracted. Possible
#'   values are:
#'   \describe{
#'     \item{\strong{\code{'all'}}}{Get all slots of the item object.}
#'     \item{\strong{\code{'pars'}}}{Get parameter names of the item object.}
#'     \item{\strong{\code{'pars_df'}}}{Get parameter names of the item object
#'       that can appear in a data.frame. For example, a "GRM" object has "b"
#'       parameter that can have a length up to 100 numbers. Instead of
#'       returning the name "b", the function will return "b1", "b2", .., until
#'       to the actual length of "b" slot.}
#'     \item{\strong{\code{'se'}}}{Get names of the slots that holds the
#'       standard errors}
#'     \item{\strong{\code{'se_df'}}}{Get names of the slots that holds
#'       standard error that can appear in a data.frame. For example, a "GRM"
#'       object has "b" parameter that can have a length up to 100 numbers.
#'       Instead of returning the name "se_b", the function will return "se_b1",
#'       "se_b2", .., until to the actual length of "se_b" slot. If there are
#'       not se paramarmeter set for this item, the function will return
#'       \code{NULL}.}
#'   }
#'
#' @return A vector of names of the slots.
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' # GPCM item
#' itm <- generate_item(model = "GPCM")
#' get_slot_names_item(itm, "all")
#' get_slot_names_item(itm, "pars")
#' get_slot_names_item(itm, "pars_df")
#' get_slot_names_item(itm, "se")
#' get_slot_names_item(itm, "se_df")
#' get_slot_names_item(itm, c("pars", "se"))
#'
#' # Get the parameters of each item in an item pool
#' ip <- generate_ip(model = c("GRM", "3PL", "4PL", "GPCM2"))
#' sapply(ip$item_list, get_slot_names_item, type = "pars")
#' }
get_slot_names_item <- function(x, type = "all") {
  if (!is(x, "Item"))
    stop("Invalid 'x' value. Please provide an \"Item\" object.")
  all_slots <- slotNames(x)
  if ("all" %in% type) return(all_slots)

  model <- as.character(class(x))
  output <- c()

  if ("pars" %in% type) {
    par_names <- unlist(sapply(PMODELS[[model]]$parameters, `[[`, "name"))
    output <- c(output, par_names)
  }

  # Get all possible parameter names that can be seen if item is converted
  # to a data.frame
  if ("pars_df" %in% type) {
    par_names <- unlist(sapply(PMODELS[[model]]$parameters, `[[`, "name"))
    par_sizes <- unlist(sapply(PMODELS[[model]]$parameters, `[[`, "size"))
    for (i in seq_along(par_names)) {
      if (par_sizes[i] == 1) {
        output <- c(output, par_names[i])
      } else if (par_sizes[i] > 1) {
        output <- c(output, paste0(par_names[i],
                                   seq_along(slot(x, name = par_names[i]))))
      }
    }
  }

  if ("se" %in% type) {
    se_par_names <- unlist(sapply(PMODELS[[model]]$parameters, `[[`, "se"))
    output <- c(output, se_par_names)
  }

  if ("se_df" %in% type) {
    se_par_names <- unlist(sapply(PMODELS[[model]]$parameters, `[[`, "se"))
    par_sizes <- unlist(sapply(PMODELS[[model]]$parameters, `[[`, "size"))
    for (i in names(se_par_names)) {
      if (!is.null(slot(x, name = se_par_names[i]))) {
        if (par_sizes[i] == 1) {
          output <- c(output, se_par_names[i])
        } else if (par_sizes[i] > 1) {
          output <- c(output, paste0(
            se_par_names[i], seq_along(slot(x, name = se_par_names[i]))))
        }
      }
    }
  }
  return(output)
}



