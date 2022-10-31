setClassUnion(name = "listNULL", members = c("list","NULL"))
setClassUnion(name = "characterNULL", members = c("character","NULL"))
setClassUnion(name = "numericNULL", members = c("numeric","NULL"))
setClassUnion(name = "integerNULL", members = c("integer","NULL"))
setClassUnion(name = "mNumeric", members = c("missing", "numeric"))
setClassUnion(name = "mCharacter", members = c("missing", "character"))
setClassUnion(name = "mLogical", members = c("missing", "logical"))
# Create a class union for: numeric, matrix, data.frame, list
setClassUnion(name = "numMatDfListChar",
              members = c("numeric", "character", "matrix", "data.frame",
                          "list"))

###############################################################################@
############################# Item class #######################################
###############################################################################@
#' An S4 class to represent an Item
#' @description
#' \code{Item} is a class to represent an item. An object in Item class should
#' have a \code{model} name and \code{parameters}.
#'
#' The model that item \code{parameters} represents. Currently,
#'   following models are available:
#'     \describe{
#'       \item{\code{"Rasch"}}{
#'         Rasch Model.
#'
#'         Required parameters:
#'
#'         \describe{
#'           \item{\code{"b"}}{Item difficulty parameter.}
#'           }
#'
#'         Probability of correct response at ability estimate \eqn{\theta}:
#'
#'         \deqn{P(\theta) = \frac{e^{(\theta - b)}}{1+e^{(\theta - b)}}}
#'
#'         Model family: Unidimensional Item Response Theory (UIRT) Models
#'         }
#'       \item{\code{"1PL"}}{
#'         Unidimensional One-Parameter Logistic Model.
#'
#'         Required parameters:
#'
#'         \describe{
#'           \item{\code{"b"}}{Item difficulty parameter.}
#'           \item{\code{"D"}}{Scaling constant. Default value is \code{1}.}
#'           }
#'
#'         Probability of correct response at ability estimate \eqn{\theta}:
#'
#'         \deqn{P(\theta) = \frac{e^{D(\theta - b)}}{1+e^{D(\theta - b)}}}
#'
#'         Model family: Unidimensional Item Response Theory (UIRT) Models
#'         }
#'       \item{\code{"2PL"}}{
#'         Unidimensional Two-Parameter Logistic Model.
#'
#'         Required parameters:
#'
#'         \describe{
#'           \item{\code{"a"}}{Item discrimination parameter.}
#'           \item{\code{"b"}}{Item difficulty parameter.}
#'           \item{\code{"D"}}{Scaling constant. Default value is \code{1}.}
#'           }
#'
#'         Probability of correct response at ability estimate \eqn{\theta}:
#'
#'         \deqn{P(\theta) = \frac{e^{Da(\theta - b)}}{1+e^{Da(\theta - b)}}}
#'
#'         Model family: Unidimensional Item Response Theory (UIRT) Models
#'         }
#'       \item{\code{"3PL"}}{
#'         Unidimensional Three-Parameter Logistic Model.
#'
#'         Required parameters:
#'
#'         \describe{
#'           \item{\code{"a"}}{Item discrimination parameter.}
#'           \item{\code{"b"}}{Item difficulty parameter.}
#'           \item{\code{"c"}}{Pseudo-guessing parameter (lower asymptote).}
#'           \item{\code{"D"}}{Scaling constant. Default value is \code{1}.}
#'           }
#'
#'         Probability of correct response at ability estimate \eqn{\theta}:
#'
#'         \deqn{P(\theta) = c + (1-c) \frac{e^{Da(\theta - b)}}{1+e^{Da(\theta - b)}}}
#'
#'         Model family: Unidimensional Item Response Theory (UIRT) Models
#'         }
#'       \item{\code{"4PL"}}{
#'         Unidimensional Four-Parameter Logistic Model.
#'
#'         Required parameters:
#'
#'         \describe{
#'           \item{\code{"a"}}{Item discrimination parameter.}
#'           \item{\code{"b"}}{Item difficulty parameter.}
#'           \item{\code{"c"}}{Pseudo-guessing parameter (lower asymptote).}
#'           \item{\code{"d"}}{Upper asymptote parameter.}
#'           \item{\code{"D"}}{Scaling constant. Default value is \code{1}.}
#'           }
#'
#'         Probability of correct response at ability estimate \eqn{\theta}:
#'
#'         \deqn{P(\theta) = c + (d-c) \frac{e^{Da(\theta - b)}}{1+e^{Da(\theta - b)}}}
#'
#'         Model family: Unidimensional Item Response Theory (UIRT) Models
#'         }
#'       \item{\code{"GRM"}}{
#'         Graded Response Model
#'
#'         Required parameters:
#'
#'         \describe{
#'           \item{\code{"a"}}{Item discrimination parameter.}
#'           \item{\code{"b"}}{Item threshold parameters (a vector of values).
#'             Each value refers to the ability level for which the probability
#'             of responding at or above that category is equal to 0.5. }
#'           \item{\code{"D"}}{Scaling constant. Default value is \code{1}.}
#'           }
#'
#'         Probability of scoring at or above the category \eqn{k}:
#'
#'         \deqn{P^*_k(\theta) = \frac{e^{Da(\theta - b_k)}}{1+e^{Da(\theta - b_k)}}}
#'
#'         Probability of responding at category \eqn{k} where the possible
#'         scores are \eqn{0, \ldots, m}:
#'
#'         \deqn{P_0(\theta) = 1 - P^*_1(\theta)}
#'         \deqn{P_1(\theta) = P^*_1(\theta) - P^*_2(\theta)}
#'         \deqn{\cdots}
#'         \deqn{P_k(\theta) = P^*_{k}(\theta) - P^*_{k+1}(\theta)}
#'         \deqn{\cdots}
#'         \deqn{P_m(\theta) = P^*_{m}(\theta)}
#'
#'         Model family: Polytomous Item Response Theory (PIRT) Models
#'         }
#'       \item{\code{"GPCM"}}{
#'         Generalized Partial Credit Model
#'
#'         Required parameters:
#'
#'         \describe{
#'           \item{\code{"a"}}{Item discrimination parameter.}
#'           \item{\code{"b"}}{Item step difficulty parameters (a vector of
#'             values).}
#'           \item{\code{"D"}}{Scaling constant. Default value is \code{1}.}
#'           }
#'
#'         Probability of scoring at category \eqn{k}:
#'
#'         \deqn{P_k(\theta) = \frac{exp[\sum_{v = 0}^{k} Da(\theta - b_v)]}
#'           {\sum_{c = 0}^{m-1}exp[\sum_{v = 0}^{c}Da(\theta - b_v)]}}
#'
#'         Model family: Polytomous Item Response Theory (PIRT) Models
#'         }
#'       \item{\code{"PCM"}}{
#'         Partial Credit Model (Masters, 1982)
#'
#'         Required parameters:
#'
#'         \describe{
#'           \item{\code{"b"}}{Item step difficulty parameters (a vector of
#'             values).}
#'           }
#'
#'         Probability of scoring at category \eqn{k}:
#'
#'         \deqn{P_k(\theta) = \frac{exp[\sum_{v = 0}^{k} (\theta - b_v)]}{\sum_{c = 0}^{m-1}exp[\sum_{v = 0}^{c}(\theta - b_v)]}}
#'
#'         Model family: Polytomous Item Response Theory (PIRT) Models
#'         }
#'       \item{\code{"GPCM2"}}{
#'         An alternative parametrization of Generalized Partial Credit Model
#'         \code{"GPCM"} where \eqn{b_k = b - d_k}. See Muraki (1997),
#'         Equation 15 on page 164.
#'
#'         Required parameters:
#'
#'         \describe{
#'           \item{\code{"a"}}{Item discrimination parameter.}
#'           \item{\code{"b"}}{Location parameter.}
#'           \item{\code{"d"}}{A vector of threshold parameters.}
#'           \item{\code{"D"}}{Scaling constant. Default value is \code{1}.}
#'           }
#'
#'         Probability of scoring at category \eqn{k}:
#'
#'         \deqn{P_k(\theta) = \frac{exp[\sum_{v = 0}^{k} Da(\theta - b + d_v)]}{\sum_{c = 0}^{m-1}exp[\sum_{v = 0}^{c}Da(\theta - b + d_v)]}}
#'
#'         Model family: Polytomous Item Response Theory (PIRT) Models
#'         }
#'     }
#'
#'   A model must be specified for the construction of an \code{Item} object.
#'
#' @slot item_id Item ID. Default value is \code{NULL}.
#' @slot content Content information for the Item object.
#' @slot misc This slot is a list where one can put any information about
#'  the Item object. For example, one can enter the ID's of the enemies of the current
#'  Item as \code{misc = list(enemies = c("i1", i2))}. Or, one can enter
#'  Sympson-Hetter exposure control parameter K:
#'  \code{misc = list(sympson_hetter_k = .75)}.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @references
#'
#'   Masters, G. N. (1982). A Rasch model for partial credit scoring.
#'   \emph{Psychometrika}, 47, 149–174.
#'
#'   Muraki, E. (1992). A generalized partial credit model:
#'   Application of an EM algorithm. \emph{Applied Psychological Measurement},
#'   16, 159–176.
#'
setClass(Class = "Item",
         slots = c(item_id = "characterNULL",
                   content = "characterNULL",
                   misc = "listNULL")
         )

# The default scaling parameter value
DEFAULT_D_SCALING <-  1 # 1.702
DEFAULT_D_SCALING_MIRT <- 1 # 1.702


# @slot se_parameters Standard error of the item parameters. This should be
#   a list of standard error values. For example, for "2PL", if the parameters
#   are \code{list(a = 1.2, b = -0.22)}, the standard error values of
#   parameters can be either \code{NULL} (which is the default value) or
#   \code{list(a = 0.24, b = 0.42)}. None of the standard error values can
#   be smaller than 0. Individual SE values can be \code{NA}. For example,
#   \code{list(a = 0.24, b = NA)} is acceptable, whereas
#   \code{list(a = 0.24, b = NULL)} is not acceptable.
#
#   For models like polytomous items, the SE values should match the parameter
#   values in length. For example, if the parameter values of a \code{"GPCM"}
#   is \code{parameters = list(a = 1.4, b = c(-1, 0.42, 2.1), D = 1.7)}, then
#   the SE values should be like
#   \code{se_parameters = list(a = .2, b = c(.32, 0.34, .3))}. Since the
#   scaling parameter \code{D} is constant, it does not have a standard error.

#### PModels ####
# List of currently implemented models for Item class
# PMODELS : Psychometric Models
# Get all Unidimensioal IRT (UIRT) models
# names(PMODELS)[sapply(PMODELS, function(x) x$model_family == 'UIRT')]
PMODELS <- list(
  'Rasch' = list(parameters = list(
    b = list(name = 'b', size = 1, se = "se_b",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 0)),
    model_family = "UIRT",
    verbose_name = "Rasch Model"),
  '1PL' = list(parameters = list(
    b = list(name = 'b', size = 1, se = "se_b",
             description = "Difficulty Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 0),
    D = list(name = 'D', size = 1, se = NULL,
             description = "Scaling Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = DEFAULT_D_SCALING)),
    model_family = "UIRT",
    verbose_name = "One-Parameter Logistic Model"),
  '2PL' = list(parameters = list(
    a = list(name = 'a', size = 1, se = "se_a",
             description = "Discrimination Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 1),
    b = list(name = 'b', size = 1, se = "se_b",
             description = "Difficulty Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 0),
    D = list(name = 'D', size = 1, se = NULL,
             description = "Scaling Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = DEFAULT_D_SCALING)),
    model_family = "UIRT",
    verbose_name = "Two-Parameter Logistic Model"),
  '3PL' = list(parameters = list(
    a = list(name = 'a', size = 1, se = "se_a",
             description = "Discrimination Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 1),
    b = list(name = 'b', size = 1, se = "se_b",
             description = "Difficulty Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 0),
    c = list(name = 'c', size = 1, se = "se_c", min = 0, max = 1,
             description = "Pseudo-Guessing Parameter",
             default_value = 0),
    D = list(name = 'D', size = 1, se = NULL,
             description = "Scaling Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = DEFAULT_D_SCALING)),
    model_family = "UIRT",
    verbose_name = "Three-Parameter Logistic Model"),
  '4PL' = list(parameters = list(
    a = list(name = 'a', size = 1, se = "se_a",
             description = "Discrimination Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 1),
    b = list(name = 'b', size = 1, se = "se_b",
             description = "Difficulty Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 0),
    c = list(name = 'c', size = 1, se = "se_c", min = 0, max = 1,
             description = "Pseudo-Guessing Parameter",
             default_value = 0),
    d = list(name = 'd', size = 1, se = "se_d", min = 0, max = 1,
             description = "Upper Asymptote Parameter",
             default_value = 1),
    D = list(name = 'D', size = 1, se = NULL,
             description = "Scaling Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = DEFAULT_D_SCALING)),
    model_family = "UIRT",
    verbose_name = "Four-Parameter Logistic Model"),
  # 'M1PL' = list(parameters = list(
  #   d = list(name = 'd', size = 1, se = "se_d",
  #            description = "Intercept Parameter",
  #            min = -.Machine$double.xmax, max = .Machine$double.xmax,
  #            default_value = 0),
  #   D = list(name = 'D', size = 1, se = NULL,
  #            description = "Scaling Parameter",
  #            min = -.Machine$double.xmax, max = .Machine$double.xmax,
  #            default_value = DEFAULT_D_SCALING_MIRT)),
  #   model_family = "MIRT",
  #               verbose_name = "Multidimensional One-Parameter Logistic Model"),
  'M2PL' = list(parameters = list(
    a = list(name = 'a', size = 100, se = "se_a",
             description = "Slope Parameters",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 1),
    d = list(name = 'd', size = 1, se = "se_d",
             description = "Intercept Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 0),
    D = list(name = 'D', size = 1, se = NULL,
             description = "Scaling Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = DEFAULT_D_SCALING_MIRT)),
    model_family = "MIRT",
    verbose_name = "Multidimensional Two-Parameter Logistic Model"),
  'M3PL' = list(parameters = list(
    a = list(name = 'a', size = 100, se = "se_a",
             description = "Slope Parameters",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 1),
    c = list(name = 'c', size = 1, se = "se_c", min = 0, max = 1,
             description = "Pseudo-Guessing Parameter",
             default_value = 0),
    d = list(name = 'd', size = 1, se = "se_d",
             description = "Intercept Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 0),
    D = list(name = 'D', size = 1, se = NULL,
             description = "Scaling Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = DEFAULT_D_SCALING_MIRT)),
    model_family = "MIRT",
    verbose_name = "Multidimensional Three-Parameter Logistic Model"
                ),
  'GRM' = list(parameters = list(
    a = list(name = 'a', size = 1, se = "se_a",
             description = "Discrimination Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 1),
    b = list(name = 'b', size = 100, se = "se_b",
             description = "Threshold Parameters",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = c(-1, 0, 1)),
    D = list(name = 'D', size = 1, se = NULL,
             description = "Scaling Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = DEFAULT_D_SCALING)),
    model_family = "PIRT",
    verbose_name = "Graded Response Model"),
  'PCM' = list(parameters = list(
    b = list(name = 'b', size = 100, se = "se_b",
             description = "Step Difficulty Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = c(-1, 0, 1))),
    model_family = "PIRT",
    verbose_name = "Partial Credit Model"),
  'GPCM' = list(parameters = list(
    a = list(name = 'a', size = 1, se = "se_a",
             description = "Discrimination Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 1),
    b = list(name = 'b', size = 100, se = "se_b",
             description = "Step Difficulty Parameters",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = c(-1, 0, 1)),
    D = list(name = 'D', size = 1, se = NULL,
             description = "Scaling Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = DEFAULT_D_SCALING)),
    model_family = "PIRT",
    verbose_name = "Generalized Partial Credit Model"),
  'GPCM2' = list(parameters = list(
    a = list(name = 'a', size = 1, se = "se_a",
             description = "Discrimination Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 1),
    b = list(name = 'b', size = 1, se = "se_b",
             description = "Overall Location Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = 0),
    d = list(name = 'd', size = 100, se = "se_d",
             description = "Threshold Parameters",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = c(-1, 0, 1)),
    D = list(name = 'D', size = 1, se = NULL,
             description = "Scaling Parameter",
             min = -.Machine$double.xmax, max = .Machine$double.xmax,
             default_value = DEFAULT_D_SCALING)),
    model_family = "PIRT",
    verbose_name = "Reparameterized Generalized Partial Credit Model")
  )

# Unidimensional psychometric models for dichotomous items
UNIDIM_DICHO_MODELS <- names(PMODELS)[sapply(PMODELS, function(x)
        x$model_family == "UIRT")]

# Psychometric models dichotomous items
DICHOTOMOUS_MODELS <- c(UNIDIM_DICHO_MODELS, "M2PL", "M3PL")

# Unidimensional psychometric models for polytomous items
UNIDIM_POLY_MODELS <- names(PMODELS)[sapply(PMODELS, function(x)
        x$model_family == "PIRT")]
# Psychometric models polytomous items
POLYTOMOUS_MODELS <- c(UNIDIM_POLY_MODELS)

###############################################################################@
############################# initialize (Item) ################################
###############################################################################@
#' @noRd
#' @title This function initializes the \code{Item} object.
#'
#' @importFrom methods callNextMethod
#'
setMethod("initialize", "Item",
          function(.Object, item_id = NULL, content = NULL, misc = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@item_id <- item_id
  .Object@content <- content
  .Object@misc <- misc
  # Check validity of the object
  validObject(.Object)
  .Object
})



###############################################################################@
############################# setValidity (Item) ###############################
###############################################################################@
# Here are some rules
# Each Item object should:
# * Have model and parameters slot
# * Have specified number of parameters. No more, no less.
# * Have proper names for parameters: all names should be listed in
#   names(PMODELS[[model_name]]$parameters) and should be unique.
# * Length of each sub-parameter should be 1 for IRTDichotomous models.
# * Each sub-parameter should be "numeric"
# * not be NULL or NA
# * not include NA in parameters slot
#
# TO-DO
# * se_parameters should also be named accordingly. And relevant checks should
#   be done to conform with parameter values.
#' @noRd
#' @title This function sets some validity rules for \code{Item} object.
#'
#' @name Item-class::setValidity()
#'
#' @param Class The class of the object.
#'
#'
setValidity(
  Class = "Item",
  function(object)
  {
    # ############# LOCAL FUNCTIONS #############################################@
    # # This function finds the parName (parameter names) within the
    # # pars (parameters)
    # # @description
    # #
    # # @param pars Parameter vector. Elements can have names or not.
    # # @param parName A chacacter vector with length 1. Parameter name to
    # # be searched within the names of \code{pars}.
    # # @param irtModel whether the pars are the IRT model or not.
    # # Specifically 1-4 parameter IRT models. For this function to work when
    # # irtModel is TRUE, parName should be either "a", "b", "c" or "d".
    # #
    # # @return the value of the parameter searched.
    # #
    # # @examples
    # # findParameter(c(a=1, b=2,c=.1, d=.9), "d")
    # # findParameter(c(1, 2, .1, .9), "c")
    # # findParameter(list(a = 2, b = .22), "a")
    # # findParameter(list(a = 2, b = .22, c = .2), c("a", "c"))
    # # findParameter(list(a = c(1.2, 2), b = .22, c = .2), "a")
    # findParameter <- function(pars, parName, irtModel = TRUE)
    # {
    #   if (any(names(pars) %in% parName) &&
    #       (sum(names(pars) %in% parName) == 1))
    #   {
    #     result <- pars[names(pars) %in% parName]
    #   } else if (irtModel)
    #     result <- pars[which(letters %in% parName)]
    #   return(unlist(result))
    # }
    #
    # checkCParameter <- function(pars)
    # {
    #   # Find c parameter from the list of pars
    #   # Examples:
    #   # checkCParameter(object@parameters)
    #   cPar <- findParameter(pars, "c", irtModel = TRUE)
    #   if ((cPar < 0) || (cPar > 1))
    #     stop(paste0("Invalid 'c' parameter. 'c' parameter in IRT ",
    #                 "parameterization cannot be smaller than 0 or larger ",
    #                 "than 1. Please check your 'c' parameter"), call. = FALSE)
    # }
    # ############## End of LOCAL FUNCTIONS #####################################@

    # # ----------------------- Check model ------------------------------- #
    # # Check the model, currently only irt1PM, irt2PM,
    # # irt3PM, irt4PM, mirt1PM, mirt2PM and mirt3PM is used
    # if (is.null(object@model) || (length(object@model) != 1) ||
    #       !(object@model %in% names(PMODELS)))
    #   stop(paste0("Invalid model. Item model should be specified ",
    #               "correctly. It can be either: ",
    #               paste0(names(PMODELS), collapse = ", ")), call. = FALSE)
    # # This will signify the model name in PMODELS
    # model_name <- object@model
    #
    # # ----------------------- Check parameters ------------------------------- #
    # # Object parameters cannot be NULL or NA
    # if (is.null(object@parameters) || any(is.na(object@parameters)))
    #   stop("Invalid parameter. Item parameters cannot be NULL or NA.",
    #        call. = FALSE)
    #
    # # All parameters should be numeric
    # if (!all(sapply(object@parameters, "class") %in% c("integer", "numeric")))
    #   stop(paste0("Invalid parameters. All parameters should be numeric."),
    #        call. = FALSE)
    #
    # # Check for proper naming of parameters. Parameter names should be unique
    # # and all should correspond one-to-one with PMODELS[[model_name]]$parameters
    # if (is.null(parNames <- names(object@parameters))) {
    #   stop(paste0("Invalid parameter names. Parameter names of Item class ",
    #               "cannot be NULL. Please give relevant names."), call. = FALSE)
    # } else if ((
    #   length(parNames) !=  length(PMODELS[[model_name]]$parameters)) ||
    #   length(unique(parNames)) != length(PMODELS[[model_name]]$parameters)
    #   ) {
    #   stop(paste0("Invalid parameter names. Parameter names of Item class ",
    #               "should be unique and complete. Please give relevant names."),
    #        call. = FALSE)
    # } else if (!all(parNames %in% names(PMODELS[[model_name]]$parameters)))
    #   stop(paste0("Invalid parameter names. Parameter names for ",
    #               model_name," model should be ",
    #               paste0(names(PMODELS[[model_name]]$parameters),
    #                      collapse = ", "),
    #               ". Please give relevant names."), call. = FALSE)
    #
    # # Number of parameters should be as specified in PMODELS parameters
    # if (length(object@parameters) != length(PMODELS[[model_name]]$parameters))
    #   stop(paste0("Invalid parameters. Number of parameters for ",
    #               model_name," model should be ",
    #               length(PMODELS[[model_name]]$parameters), "."), call. = FALSE)
    #
    # # Size of the model parameters should be matching to the sizes designated
    # # in PMODELS.
    # # Also the magnitudes of parameter values should be within min-max values
    # # designated in PMODELS
    # for (p in PMODELS[[model_name]]$parameters) {
    #   temp_par <- object@parameters[[p$name]]
    #   if (length(temp_par) < 1 || length(temp_par) > p$size)
    #     stop(paste0("Invalid parameters. For \"", model_name,"\" model the ",
    #                 "size of the \"", p$name, "\" should be ",
    #                 ifelse(p$size == 1, 1, paste0(p$size, " or less")), "."),
    #          call. = FALSE)
    #   if (any(temp_par < p$min) || any(temp_par > p$max))
    #     stop(paste0("Invalid parameters. The values of \"", p$name, "\" ",
    #                 "parameters should be between ", prettyNum(p$min), " and ",
    #                 prettyNum(p$max), "."), call. = FALSE)
    # }
    # if (PMODELS[[model_name]]$model_family == "UIRT") {
    #   # Length of all sub-parameters should be 1 for
    #   if (any(sapply(object@parameters, FUN = "length") != 1))
    #     stop(paste0("Invalid parameters. For Unidimensional IRT models, the ",
    #                 "length of all parameters should be 1."), call. = FALSE)
    #   switch(object@model,
    #          "3PL" = {
    #            checkCParameter(object@parameters)
    #          },
    #          "4PL" = {
    #            checkCParameter(object@parameters)
    #            checkDParameter <- function(pars)
    #            {
    #              cPar <- findParameter(pars, "c", irtModel = TRUE)
    #              dPar <- findParameter(pars, "d", irtModel = TRUE)
    #              if ((dPar < 0) || (dPar > 1) || (dPar < cPar))
    #                stop(paste0("Invalid 'd' parameter. 'd' parameter in IRT ",
    #                            "parameterization cannot be smaller than 0, ",
    #                            "larger than 1 and smaller than 'c' parameter. ",
    #                            "Please check your 'd' parameter."),
    #                     call. = FALSE)
    #            }
    #            checkDParameter(object@parameters)
    #          }
    #   )
    # } else if (PMODELS[[model_name]]$model_family == "MIRT") {
    #   # d parameter should have length 1.
    #   if (length(object@parameters$d) != 1)
    #     stop(paste0("Invalid parameters. The length of 'd' parameter should ",
    #                 "be 1."), call. = FALSE)
    #   # Check c parameter
    #   if (object@model == "M3PL")
    #   {
    #     # c parameter should have length 1.
    #     if (length(object@parameters$c) != 1)
    #       stop(paste0("Invalid parameters. The length of 'c' parameter should ",
    #                   "be 1."), call. = FALSE)
    #     # c parameter should be between 0 and 1.
    #     checkCParameter(object@parameters)
    #   }
    # } else if (PMODELS[[model_name]]$model_family == "PIRT") {
    #   # item location parameter "b" cannot be a vector of length more than 1
    #   if (object@model == "GPCM2") {
    #     if (length(object@parameters$b) != 1)
    #       stop("In 'GPCM2' model, the item location parameter 'b' should be ",
    #            "a single value (i.e. length(b) should be 1).", call. = FALSE)
    #   }
    #
    # }
    #
    # # ----------------------- Check se_parameters ---------------------------- #
    # if (!is.null(object@se_parameters)) {
    #   se_par_names <- unlist(sapply(PMODELS[[model_name]]$parameters,
    #                                 function(x) if (x$se) x$name))
    #
    #   if (is.null(names(object@se_parameters)) ||
    #       !all(names(object@se_parameters) %in% se_par_names)||
    #       !all(se_par_names %in% names(object@se_parameters))
    #       )
    #     stop(paste0("Invalid 'se_parameters' values.\n'se_parameters' should ",
    #                 "be a list with elements named: ",
    #                 paste0("'", se_par_names, "'", collapse = ", "), "."),
    #          call. = FALSE)
    #   if (length(object@se_parameters) != length(se_par_names))
    #     stop(paste0("Invalid 'se_parameters' values. 'se_parameters' length ",
    #                 "for ", model_name," model should be ",
    #                 length(se_par_names), "."), call. = FALSE)
    #
    #   # Individual se_parameters cannot be NULL
    #   if (all(any(sapply(object@se_parameters, is.null))))
    #     stop(paste0("Invalid 'se_parameters' values. Individual elements of ",
    #                 "'se_parameters' cannot be NULL."),
    #          call. = FALSE)
    #
    #   # The length of each se_parameter should be the same as parameters, i.e.
    #   # for example for "GPCM", if there are 3 threshold parameters, there
    #   # should be three standard errors for each threshold parameter.
    #   if (!all(sapply(object@parameters[se_par_names], length)[se_par_names] ==
    #            sapply(object@se_parameters, length)[se_par_names]))
    #     stop(paste0("Invalid 'se_parameters' values. All of the elements of ",
    #                 "'se_parameters' should have the same length as the ",
    #                 "corresponding element in 'parameters'."),
    #          call. = FALSE)
    #
    #
    #   # All SE parameters should be numeric if it is not NULL or NA
    #   if (!all(sapply(object@se_parameters, FUN = function(x)
    #     all(is.na(x)) | (class(x) == "numeric"))))
    #     stop(paste0("Invalid 'se_parameters' values. All standard error values",
    #                 " of item parameters ('se_parameters') should be numeric."),
    #          call. = FALSE)
    #   # All se_parameters should be larger than 0 or they are NA
    #   if (any(!is.na(unlist(object@se_parameters)) &
    #           unlist(object@se_parameters) < 0))
    #     stop(paste0("Invalid 'se_parameters' values. Standard error values of ",
    #                 "item parameters ('se_parameters') cannot be smaller ",
    #                 "than 0."), call. = FALSE)
    # }

    # ----------------------- Check item_id ---------------------------------- #
    # Check the length of item_id vector
    if (!is.null(object@item_id) && (length(object@item_id) != 1))
      stop(paste0("Invalid 'item_id'. Item ID should have length 1, ",
                  "or be NULL."), call. = FALSE)
  })




############################# check_item_parameters ############################
#' This function checks parameters based on Pmodel constraints.
#' @noRd
#'
check_item_parameters <- function(object) {

  # ----------------------- Check parameters --------------------------------- #
  # Size of the model parameters should be matching to the sizes designated
  # in PMODELS.
  # Also the magnitudes of parameter values should be within min-max values
  # designated in PMODELS
  model_name <- c(class(object))
  for (p in PMODELS[[model_name]]$parameters) {
    temp_par <- slot(object, p$name)
    if (length(temp_par) < 1 || length(temp_par) > p$size)
      stop(paste0("Invalid parameters. For \"", model_name,"\" model the ",
                  "size of the \"", p$name, "\" parameter should be ",
                  ifelse(p$size == 1, 1, paste0(p$size, " or less")), "."),
           call. = FALSE)
    if (any(is.na(temp_par)) || any(temp_par < p$min) ||
        any(temp_par > p$max))
      stop(paste0("Invalid parameters. The values of \"", p$name, "\" ",
                  "parameters should be between ", prettyNum(p$min), " and ",
                  prettyNum(p$max), "."), call. = FALSE)

    # ----------------------- Check se --------------------------------------- #
    # se_par_names <- unlist(sapply(PMODELS[[model_name]]$parameters, `[[`, "se"))

    # The length of each se_parameter should be the same as parameters, i.e.
    # for example for "GPCM", if there are 3 threshold parameters, there
    # should be three standard errors for each threshold parameter.

    if (!is.null(p$se) && !is.null(temp_se <- slot(object, p$se))) {
      if (length(temp_se) != length(temp_par))
        stop(paste0("Invalid '", p$se, "'. The length of '", p$se, "' should ",
                    "be equal to the length of the parameter '", p$name, "'."),
             call. = FALSE)
      # All se_parameters should be larger than 0 or they are NA
      if (any(!is.na(temp_se) & temp_se < 0))
        stop(paste0("Invalid '", p$se, "' values. Standard error values of ",
                    "item parameters cannot be smaller than 0."), call. = FALSE)
    }
  }
}


###############################################################################@
############################# Rasch class ######################################
###############################################################################@
#' Rasch model
#'
#' @slot b Item difficulty parameter
#' @slot se_b Standard error of item difficulty parameter
#'
#' @export
#'
#' @author Emre Gonulates
setClass(Class = "Rasch",
         slots = c(b = "numeric", se_b = "numericNULL"),
         prototype = prototype(b = 0, se_b = NULL),
         contains = "Item"
         )


############################# initialize (Rasch) ##############################@
#' @noRd
#' @title This function initializes \code{Rasch} object.
#'
#' @importFrom methods callNextMethod
#'
setMethod("initialize", "Rasch",
          function(.Object, b = 0, se_b = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@b <- b
  .Object@se_b <- se_b
  # Check validity of the object
  validObject(.Object)
  .Object
})

############################# setValidity (Rasch) #############################@
#' @noRd
#' @title This function sets some validity rules for \code{Rasch} object.
#'
#' @name Rasch-class::setValidity()
#'
#' @param Class The class of the object.
#'
setValidity(
  Class = "Rasch",
  function(object) {
    check_item_parameters(object)
  }
)


###############################################################################@
############################# 1PL class ########################################
###############################################################################@
#' One-Parameter Logistic IRT model
#'
#' @slot b Item difficulty parameter
#' @slot D Scaling constant
#' @slot se_b Standard error of item difficulty parameter
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname irt1PL
#'
setClass(Class = "1PL",
         slots = c(b = "numeric", D = "numeric", se_b = "numericNULL"),
         prototype = prototype(b = 0, D = DEFAULT_D_SCALING, se_b = NULL),
         contains = "Item"
         )


############################# initialize (1PL) ################################@
#' @noRd
#' @title This function initializes \code{1PL} object.
#'
#' @importFrom methods callNextMethod
#'
setMethod("initialize", "1PL",
          function(.Object, b = 0, D = DEFAULT_D_SCALING, se_b = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@b <- b
  .Object@D <- D
  .Object@se_b <- se_b
  # Check validity of the object
  validObject(.Object)
  .Object
})

############################# setValidity (1PL) ###############################@
#' @noRd
#' @title This function sets some validity rules for \code{1PL} object.
#'
#' @name 1PL-class::setValidity()
#'
#' @param Class The class of the object.
#'
setValidity(
  Class = "1PL",
  function(object) {
    check_item_parameters(object)
  }
)



###############################################################################@
############################# 2PL class ########################################
###############################################################################@
#' Two-Parameter Logistic IRT model
#'
#' @slot a Item discrimination parameter
#' @slot b Item difficulty parameter
#' @slot D Scaling constant
#' @slot se_a Standard error of item discrimination parameter
#' @slot se_b Standard error of item difficulty parameter
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname irt2PL
#'
setClass(Class = "2PL",
         slots = c(a = "numeric", b = "numeric", D = "numeric",
                   se_a = "numericNULL", se_b = "numericNULL"),
         prototype = prototype(a = 1, b = 0, D = DEFAULT_D_SCALING,
                               se_a = NULL, se_b = NULL),
         contains = "Item"
         )


############################# initialize (2PL) ################################@
#' @noRd
#' @title This function initializes \code{2PL} object.
#'
#' @importFrom methods callNextMethod
#'
setMethod("initialize", "2PL",
          function(.Object, a = 1, b = 0, D = DEFAULT_D_SCALING,
                   se_a = NULL, se_b = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@a <- a
  .Object@b <- b
  .Object@D <- D
  .Object@se_a <- se_a
  .Object@se_b <- se_b
  # Check validity of the object
  validObject(.Object)
  .Object
})


############################# setValidity (2PL) ###############################@
#' @noRd
#' @title This function sets some validity rules for \code{2PL} object.
#'
#' @name 2PL-class::setValidity()
#'
#' @param Class The class of the object.
#'
setValidity(
  Class = "2PL",
  function(object) {
    check_item_parameters(object)
  }
)



###############################################################################@
############################# 3PL class ########################################
###############################################################################@
#' Three-Parameter Logistic IRT model
#'
#' @slot a Item discrimination parameter
#' @slot b Item difficulty parameter
#' @slot c Guessing parameter
#' @slot D Scaling constant
#' @slot se_a Standard error of item discrimination parameter
#' @slot se_b Standard error of item difficulty parameter
#' @slot se_c Standard error of guessing  parameter
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname irt3PL
#'
setClass(Class = "3PL",
         slots = c(a = "numeric", b = "numeric", c = "numeric", D = "numeric",
                   se_a = "numericNULL", se_b = "numericNULL",
                   se_c = "numericNULL"),
         prototype = prototype(a = 1, b = 0, c = 0, D = DEFAULT_D_SCALING,
                               se_a = NULL, se_b = NULL, se_c = NULL),
         contains = "Item"
         )

############################# initialize (3PL) ################################@
#' @noRd
#' @title This function initializes \code{3PL} object.
#'
#' @importFrom methods callNextMethod
#'
setMethod("initialize", "3PL",
          function(.Object, a = 1, b = 0, c = 0, D = DEFAULT_D_SCALING,
                   se_a = NULL, se_b = NULL, se_c = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@a <- a
  .Object@b <- b
  .Object@c <- c
  .Object@D <- D
  .Object@se_a <- se_a
  .Object@se_b <- se_b
  .Object@se_c <- se_c
  # Check validity of the object
  validObject(.Object)
  .Object
})

############################# setValidity (3PL) ###############################@
#' @noRd
#' @title This function sets some validity rules for \code{3PL} object.
#'
#' @name 3PL-class::setValidity()
#'
#' @param Class The class of the object.
#'
setValidity(
  Class = "3PL",
  function(object) {
    check_item_parameters(object)
  }
)


###############################################################################@
############################# 4PL class ########################################
###############################################################################@
#' Three-Parameter Logistic IRT model
#'
#' @slot a Item discrimination parameter
#' @slot b Item difficulty parameter
#' @slot c Guessing parameter
#' @slot d Upper-asymptote Parameter
#' @slot D Scaling constant
#' @slot se_a Standard error of item discrimination parameter
#' @slot se_b Standard error of item difficulty parameter
#' @slot se_c Standard error of guessing  parameter
#' @slot se_d Standard error of upper-asymptote  parameter
#'
#' @export
#'
#' @rdname irt4PL
#'
#' @author Emre Gonulates
setClass(Class = "4PL",
         slots = c(a = "numeric", b = "numeric", c = "numeric", d = "numeric",
                   D = "numeric",
                   se_a = "numericNULL", se_b = "numericNULL",
                   se_c = "numericNULL", se_d = "numericNULL"),
         prototype = prototype(a = 1, b = 0, c = 0, d = 1, D = DEFAULT_D_SCALING,
                               se_a = NULL, se_b = NULL, se_c = NULL,
                               se_d = NULL),
         contains = "Item")

############################# initialize (4PL) ################################@
#' @noRd
#' @title This function initializes \code{4PL} object.
#'
#' @importFrom methods callNextMethod
#'
setMethod("initialize", "4PL",
          function(.Object, a = 1, b = 0, c = 0, d = 1, D = DEFAULT_D_SCALING,
                   se_a = NULL, se_b = NULL, se_c = NULL, se_d = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@a <- a
  .Object@b <- b
  .Object@c <- c
  .Object@d <- d
  .Object@D <- D
  .Object@se_a <- se_a
  .Object@se_b <- se_b
  .Object@se_c <- se_c
  .Object@se_d <- se_d
  # Check validity of the object
  validObject(.Object)
  .Object
})

############################# setValidity (4PL) ###############################@
#' @noRd
#' @title This function sets some validity rules for \code{4PL} object.
#'
#' @name 4PL-class::setValidity()
#'
#' @param Class The class of the object.
#'
setValidity(
  Class = "4PL",
  function(object) {
    check_item_parameters(object)
  }
)



###############################################################################@
############################# GRM class ########################################
###############################################################################@
#' Graded Response Model
#'
#' @slot a Item discrimination parameter
#' @slot b A vector of threshold parameters
#' @slot D Scaling constant
#' @slot se_a Standard error of item discrimination parameter
#' @slot se_b A vector of standard error of item threshold parameters
#'
#' @export
#'
#' @author Emre Gonulates
setClass(Class = "GRM",
         slots = c(a = "numeric", b = "numeric", D = "numeric",
                   se_a = "numericNULL", se_b = "numericNULL"),
         prototype = prototype(a = 1, b = 0, D = DEFAULT_D_SCALING,
                               se_a = NULL, se_b = NULL),
         contains = "Item")

############################# initialize (GRM) ################################@
#' @noRd
#' @title This function initializes \code{GRM} object.
#'
#' @importFrom methods callNextMethod
#'
setMethod("initialize", "GRM",
          function(.Object, a = 1, b = 0, D = DEFAULT_D_SCALING,
                   se_a = NULL, se_b = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@a <- a
  .Object@b <- b
  .Object@D <- D
  .Object@se_a <- se_a
  .Object@se_b <- se_b
  # Check validity of the object
  validObject(.Object)
  .Object
})

############################# setValidity (GRM) ###############################@
#' @noRd
#' @title This function sets some validity rules for \code{GRM} object.
#'
#' @name GRM-class::setValidity()
#'
#' @param Class The class of the object.
#'
setValidity(
  Class = "GRM",
  function(object) {
    check_item_parameters(object)
  }
)


###############################################################################@
############################# GPCM class #######################################
###############################################################################@
#' Generalized Partial Credit Model
#'
#' @slot a Item discrimination parameter
#' @slot b A vector of threshold parameters
#' @slot D Scaling constant
#' @slot se_a Standard error of item discrimination parameter
#' @slot se_b A vector of standard error of item threshold parameters
#'
#' @export
#'
#' @author Emre Gonulates
setClass(Class = "GPCM",
         slots = c(a = "numeric", b = "numeric", D = "numeric",
                   se_a = "numericNULL", se_b = "numericNULL"),
         prototype = prototype(a = 1, b = 0, D = DEFAULT_D_SCALING,
                               se_a = NULL, se_b = NULL),
         contains = "Item")


############################# initialize (GPCM) ###############################@
#' @noRd
#' @title This function initializes \code{GPCM} object.
#'
#' @importFrom methods callNextMethod
#'
setMethod("initialize", "GPCM",
          function(.Object, a = 1, b = 0, D = DEFAULT_D_SCALING,
                   se_a = NULL, se_b = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@a <- a
  .Object@b <- b
  .Object@D <- D
  .Object@se_a <- se_a
  .Object@se_b <- se_b
  # Check validity of the object
  validObject(.Object)
  .Object
})


############################# setValidity (GPCM) ###############################@
#' @noRd
#' @title This function sets some validity rules for \code{GPCM} object.
#'
#' @name GPCM-class::setValidity()
#'
#' @param Class The class of the object.
#'
setValidity(
  Class = "GPCM",
  function(object) {
    check_item_parameters(object)
  }
)



###############################################################################@
############################# PCM class ########################################
###############################################################################@
#' Partial Credit Model
#'
#' @slot b A vector of threshold parameters
#' @slot se_b A vector of standard error of item threshold parameters
#'
#' @export
#'
#' @author Emre Gonulates
setClass(Class = "PCM",
         slots = c(b = "numeric", se_b = "numericNULL"),
         prototype = prototype(b = 0, se_b = NULL),
         contains = "Item")


############################# initialize (PCM) ################################@
#' @noRd
#' @title This function initializes \code{PCM} object.
#'
#' @importFrom methods callNextMethod
#'
setMethod("initialize", "PCM",
          function(.Object, b = 0, se_b = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@b <- b
  .Object@se_b <- se_b
  # Check validity of the object
  validObject(.Object)
  .Object
})


############################# setValidity (PCM) ###############################@
#' @noRd
#' @title This function sets some validity rules for \code{PCM} object.
#'
#' @name PCM-class::setValidity()
#'
#' @param Class The class of the object.
#'
setValidity(
  Class = "PCM",
  function(object) {
    check_item_parameters(object)
  }
)


###############################################################################@
############################# GPCM2 class ######################################
###############################################################################@
#' Reparameterized Generalized Partial Credit Model
#'
#' @slot a Item discrimination parameter
#' @slot b Overall location parameter
#' @slot d A vector of threshold parameters
#' @slot D Scaling constant
#' @slot se_a Standard error of item discrimination parameter
#' @slot se_b Standard error of overall location parameter
#' @slot se_d A vector of standard error of item threshold parameters
#'
#' @export
#'
#' @author Emre Gonulates
setClass(Class = "GPCM2",
         slots = c(a = "numeric", b = "numeric", d = "numeric", D = "numeric",
                   se_a = "numericNULL", se_b = "numericNULL",
                   se_d = "numericNULL"),
         prototype = prototype(a = 1, b = 0, d = 0, D = DEFAULT_D_SCALING,
                               se_a = NULL, se_b = NULL, se_d = NULL),
         contains = "Item")


############################# initialize (GPCM2) ##############################@
#' @noRd
#' @title This function initializes \code{GPCM2} object.
#'
#' @importFrom methods callNextMethod
#'
setMethod("initialize", "GPCM2",
          function(.Object, a = 1, b = 0, d = 0, D = DEFAULT_D_SCALING,
                   se_a = NULL, se_b = NULL, se_d = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@a <- a
  .Object@b <- b
  .Object@d <- d
  .Object@D <- D
  .Object@se_a <- se_a
  .Object@se_b <- se_b
  .Object@se_d <- se_d
  # Check validity of the object
  validObject(.Object)
  .Object
})


############################# setValidity (GPCM2) ##############################@
#' @noRd
#' @title This function sets some validity rules for \code{GPCM2} object.
#'
#' @name GPCM2-class::setValidity()
#'
#' @param Class The class of the object.
#'
setValidity(
  Class = "GPCM2",
  function(object) {
    check_item_parameters(object)
  }
)


###############################################################################@
############################# M3PL class #######################################
###############################################################################@
#' Multidimensional Three-Parameter Logistic Model
#'
#' @slot a Slope Parameters
#' @slot d Intercept Parameter
#' @slot c Pseudo-Guessing Parameter
#' @slot D Scaling constant
#' @slot se_a Standard errors of slope parameters
#' @slot se_d Standard error of intercept parameter
#' @slot se_c Standard error of pseudo-guessing  parameter
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname M3PL
#'
setClass(Class = "M3PL",
         slots = c(a = "numeric", d = "numeric", c = "numeric", D = "numeric",
                   se_a = "numericNULL", se_d = "numericNULL",
                   se_c = "numericNULL"),
         prototype = prototype(a = 1, d = 0, c = 0, D = DEFAULT_D_SCALING_MIRT,
                               se_a = NULL, se_d = NULL, se_c = NULL),
         contains = "Item"
         )

############################# initialize (M3PL) ###############################@
#' @noRd
#' @title This function initializes a \code{M3PL} object.
#'
#' @importFrom methods callNextMethod
#'
setMethod("initialize", "M3PL",
          function(.Object, a = 1, d = 0, c = 0, D = DEFAULT_D_SCALING_MIRT,
                   se_a = NULL, se_d = NULL, se_c = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@a <- a
  .Object@d <- d
  .Object@c <- c
  .Object@D <- D
  .Object@se_a <- se_a
  .Object@se_d <- se_d
  .Object@se_c <- se_c
  # Check validity of the object
  validObject(.Object)
  .Object
})


############################# setValidity (M3PL) ##############################@
#' @noRd
#' @title This function sets some validity rules for \code{M3PL} object.
#'
#' @name M3PL-class::setValidity()
#'
#' @param Class The class of the object.
#'
setValidity(
  Class = "M3PL",
  function(object) {
    check_item_parameters(object)
  }
)




###############################################################################@
############################# M2PL class #######################################
###############################################################################@
#' Multidimensional Two-Parameter Logistic Model
#'
#' @slot a Slope Parameters
#' @slot d Intercept Parameter
#' @slot D Scaling constant
#' @slot se_a Standard errors of slope parameters
#' @slot se_d Standard error of intercept parameter
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @rdname M2PL
#'
setClass(Class = "M2PL",
         slots = c(a = "numeric", d = "numeric", D = "numeric",
                   se_a = "numericNULL", se_d = "numericNULL"),
         prototype = prototype(a = 1, d = 0, D = DEFAULT_D_SCALING_MIRT,
                               se_a = NULL, se_d = NULL),
         contains = "Item"
         )

############################# initialize (M2PL) ###############################@
#' @noRd
#' @title This function initializes a \code{M2PL} object.
#'
#' @importFrom methods callNextMethod
#'
setMethod("initialize", "M2PL",
          function(.Object, a = 1, d = 0, D = DEFAULT_D_SCALING_MIRT,
                   se_a = NULL, se_d = NULL, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object@a <- a
  .Object@d <- d
  .Object@D <- D
  .Object@se_a <- se_a
  .Object@se_d <- se_d
  # Check validity of the object
  validObject(.Object)
  .Object
})


############################# setValidity (M2PL) ##############################@
#' @noRd
#' @title This function sets some validity rules for \code{M2PL} object.
#'
#' @name M2PL-class::setValidity()
#'
#' @param Class The class of the object.
#'
setValidity(
  Class = "M2PL",
  function(object) {
    check_item_parameters(object)
  }
)

# ###############################################################################@
# ############################# M1PL class #######################################
# ###############################################################################@
# #' Multidimensional One-Parameter Logistic Model
# #'
# #' @slot a Slope Parameters
# #' @slot d Intercept Parameter
# #' @slot D Scaling constant
# #' @slot se_a Standard errors of slope parameters
# #' @slot se_d Standard error of intercept parameter
# #'
# #' @export
# #'
# #' @author Emre Gonulates
# #'
# #' @rdname M1PL
# #'
# setClass(Class = "M1PL",
#          slots = c(a = "numeric", d = "numeric", D = "numeric",
#                    se_a = "numericNULL", se_d = "numericNULL"),
#          prototype = prototype(a = 1, d = 0, D = DEFAULT_D_SCALING_MIRT,
#                                se_a = NULL, se_d = NULL),
#          contains = "Item"
#          )
#
# ############################# initialize (M1PL) ###############################@
# #' @noRd
# #' @title This function initializes a \code{M1PL} object.
# #'
# #' @importFrom methods callNextMethod
# #'
# setMethod("initialize", "M1PL",
#           function(.Object, a = 1, d = 0, D = DEFAULT_D_SCALING_MIRT,
#                    se_a = NULL, se_d = NULL, ...) {
#   .Object <- callNextMethod(.Object, ...)
#   .Object@a <- a
#   .Object@d <- d
#   .Object@D <- D
#   .Object@se_a <- se_a
#   .Object@se_d <- se_d
#   # Check validity of the object
#   validObject(.Object)
#   .Object
# })















# Per advice from:
# http://r-pkgs.had.co.nz/src.html#c-best-practices
.onUnload <- function (libpath) {
  library.dynam.unload("irt", libpath)
}
