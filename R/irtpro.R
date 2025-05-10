# {
# library(irt)
# wrap_text <- irt:::wrap_text
# resp <- sim_resp(generate_ip(n = 15), rnorm(200), prop_missing = .2)
#
#   x = resp
#   model = "3PL"
#   target_dir = file.path("C:/temp/irtpro1")
#   analysis_name = "irtpro_calibration"
#   items = NULL
#   examinee_id_var = NULL
#   group_var = NULL
#   reference_group = NULL
#   estimation_method = "BAEM"
#   estimation_args = list(`E-Step` = c(500, 1e-005),
#                          SE = "S-EM",
#                          `M-Step` = c(500, 1e-009),
#                          Quadrature = c(49, 6),
#                          SEM = 0.001,
#                          SS = 1e-005)
#   scoring_method = "MAP"
#   scoring_args = list(Mean = 0, SD = 1)
#   misc_args = list(Decimal = 4, Processors = 1, `Min Exp` = 1)
#   print_extra = c("StdRes", "CTLD", "M2", "GOF", "Loadings", "P-Nums",
#                   "Diagnostic")
#   constraints = NULL
#   priors = data.frame(
#     model = c("1PL", "2PL", "2PL", "3PL", "3PL", "3PL"),
#     parameter = c("Intercept[0]", "Slope[0]", "Intercept[0]",
#                   "Slope[0]", "Intercept[0]", "Guessing[0]"),
#     prior_dist = c("Normal", "Lognormal", "Normal", "Lognormal", "Normal",
#                    "Beta"),
#     prior_par_1 = c(0, 0, 0, 0, 0, 4),
#     prior_par_2 = c(2, 1, 2, 1, 2, 16)
#     )
#   overwrite = TRUE
#   show_output_on_console = TRUE
#   irtpro_exe_dir = file.path("C:/Program Files/IRTPRO 6.0")
#   }

############################################################################@###
############################# irtpro_create_data_file ######################@###
############################################################################@###


#' Create IRTPRO Data File
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' resp <- sim_resp(generate_ip(n = 15), rnorm(200), prop_missing = .2)
#' irtpro_create_data_file(x = resp)
#' irtpro_create_data_file(x = resp, items = paste0("Item-", 1:7))
#'
#'
#' n_examinee <- 500
#' resp <- sim_resp(generate_ip(model = sample(c("3PL", "GPCM2"), 20, T)),
#'                  rnorm(n_examinee), prop_missing = .2)
#' resp <- cbind.data.frame(examinee_id = paste0("Ex", 1:n_examinee),
#'                          group = sample(c("A", "B"), n_examinee, TRUE),
#'                          resp)
#' output <- irtpro_create_data_file(
#'   x = resp,
#'   items = NULL,
#'   examinee_id_var = "examinee_id",
#'   group_var = "group",
#'   reference_group = "A",
#'   target_path = file.path(getwd(), "irtpro_data.ssig"),
#'   overwrite = TRUE)
#'
#' }
#'
irtpro_create_data_file <- function(
  x,
  items = NULL,
  model = "3PL",
  examinee_id_var = NULL,
  group_var = NULL,
  reference_group = NULL,
  target_path = file.path(getwd(), "irtpro_data.ssig"),
  irtpro_exe_dir = file.path("C:/Program Files/IRTPRO 6.0"),
  overwrite = FALSE) {

  # x = resp
  # items = NULL
  # model = "3PL"
  # examinee_id_var = NULL
  # group_var = NULL
  # reference_group = "A"
  # target_path = file.path("C:/temp/irtpro1", "irtpro_data.ssig")
  # irtpro_exe_dir = file.path("C:/Program Files/IRTPRO 6.0")
  # overwrite = TRUE
  #
  # examinee_id_var = "examinee_id"
  # group_var = "group"


  if (!(inherits(x, c("matrix", "data.frame", "Response_set"))))
    stop("Invalid data file. Data file should be either a matrix, data.frame ",
         "or a 'Response_set' object.", call. = FALSE)

  irtpro_asciiexe_path <- file.path(irtpro_exe_dir, "ASCII2SSIG64.exe")
  if (!file.exists(irtpro_asciiexe_path)) {
    stop("The file 'ASCII2SSIG64.exe' cannot be found in the IRTPRO system ",
         "directory:\n  \"", irtpro_exe_dir,
         "\"\nData files cannot be created without this file.")
  }

  # Check path
  target_path <- normalizePath(target_path, mustWork = FALSE)
  target_path_csv <- gsub(pattern = "\\.ssig$", replacement = ".csv",
                          target_path)
  target_dir <- dirname(target_path)
  if (!grepl("(.*)\\.ssig$", target_path))
    stop("Invalid 'target_path' argument. Please provide a valid file name ",
         "with '.ssig' extension as a target_path", call. = FALSE)
  # Make sure the directory exists
  if (!dir.exists(target_dir))
    dir.create(path = target_dir, recursive = TRUE)
  if (!dir.exists(target_dir))
    stop(paste0("The directory for IRTPRO data file cannot be created at: \n",
                target_dir, "\nPlease create directory manually.",
                call. = FALSE))

  # If x is Response_set, convert it to a data.frame and add examinee_id_var and
  # group_var
  if (is(x, "Response_set")) {
    x_matrix <- as.matrix(x, output = "score")
    if (!is.null(group_var) && is_atomic_vector(group_var)) {
      if (length(group_var) == 1 && is.character(group_var) &&
          is_atomic_vector(do.call("$", list(x = x, name = group_var)))) {
        x_matrix <- cbind(do.call("$", list(x = x, name = group_var)),
                          data.frame(x_matrix))
        colnames(x_matrix)[1] <- group_var
      } else if (length(group_var) == nrow(x_matrix)) {
        x_matrix <- cbind(group_var, data.frame(x_matrix))
        group_var <- "grouping_variable"
        colnames(x_matrix)[1] <- group_var
      } else {
        stop("Invalid 'group_var' value. For 'response_set' objects, please ",
             "attach the group information to the response set by: ",
             "'x$<group_var> <- values'. Please see the examples.")
      }
    }
    x <- x_matrix
  }

  ### Examinee ID ###
  # Create the examinee_id field, if it is NULL, automatically the row names
  # of the data will be the subject ids.
  examinee_id <- rownames(x)
  # Check whether examinee_id_var is valid
  if (!is.null(examinee_id_var) && (
    length(examinee_id_var) != 1 ||
    !(examinee_id_var %in% 1:ncol(x) ||
      (!is.null(colnames(x)) && examinee_id_var %in% colnames(x)))
    ))
    stop("Invalid 'examinee_id_var' argument. 'examinee_id_var' value should ",
         "be a column number or a column name.", call. = FALSE)
  if (!is.null(examinee_id_var)) {
    examinee_id <- paste0(x[, examinee_id_var, drop = TRUE])
  }
  if (is.null(examinee_id)) examinee_id <- paste0(1:nrow(x))

  ### Item IDs ###
  # If items are NULL, then use all of the items.
  if (is.null(items)) {
    # Check if the column names are NULL
    if (is.null(colnames(x)))
      colnames(x) <- paste0("Item-", 1:ncol(x))
    items <- colnames(x)

    # Make sure to remove examinee_id_var
    if (!is.null(examinee_id_var)) {
      if (examinee_id_var %in% items) {
        items <- items[items != examinee_id_var]
      } else {
        items <- items[-examinee_id_var]
      }
    }
    # Make sure to remove group_var
    if (!is.null(group_var)) {
      if (group_var %in% items) {
        items <- items[items != group_var]
      } else {
        items <- items[-group_var]
      }
    }
  }
  # Make sure items are valid
  if (!is.null(items) && (
    any(duplicated(items)) ||
    !(all(items %in% colnames(x)) || all(items %in% 1:ncol(x)) )
    ))
    stop("Invalid 'items' argument. The elements of 'items' argument should ",
         "be a character vector of column names of the 'x'. All elements ",
         "should be unique.", call. = FALSE)


  ### Prepare responses ###
  resp <- x[, items]
  # Check if the responses are valid:
  if (!all(sapply(unique(as.vector(unlist(resp))), function(x) is.na(x) ||
                  x %in% c(-1, 0:99))))
    stop("Invalid response data. The response values should be either ",
         "0, 1, ..., 99, or missing (i.e. NA or -1).", call. = FALSE)
  # Convert missing data to "-1"
  resp <- apply(resp, 2, function(x) ifelse(is.na(x), -1, x))
  num_of_items <- ncol(resp)

  ### Group ###
  # For now group will not be implemented
  # group_info <- irtpro_create_group_info(x = x, group_var = group_var,
  #                                        reference_group = reference_group)
  group_info <- NULL

  ### Prepare the data file and formal statement ###
  # Add ID
  data_output <- data.frame(examinee_id = examinee_id)

  # Add group if there is
  if (!is.null(group_info)) {
    data_output <- cbind.data.frame(group = group_info$group, data_output)
  }

  # Add response data:
  data_output <- cbind.data.frame(data_output, resp)

  if (overwrite || !file.exists(target_path_csv))
    utils::write.csv(x = data_output, file = target_path_csv, row.names = FALSE)

  command <- paste0('"', irtpro_asciiexe_path, '" "', target_path_csv, '" ',
                    "/delim=',' /header")

  if (overwrite || !file.exists(target_path)) system(command)
  if (!file.exists(target_path)) {
    stop("IRTPRO data file cannot be created:\n  \"", target_path, "\"")
  }

  # Determine the models
  if (is.null(model)) {
    models <- c()
  } else if (length(model) == length(items)) {
    models <- setNames(model, items)
  } else if (length(model) == 1) {
    models <- setNames(rep(model, num_of_items), items)
  } else {
    stop("Invalid 'model' argument. The length of 'model' argument should be ",
         "either one if all items should follow the same psyhometric model, ",
         "or the length of 'model' should be equal to ", num_of_items,
         " (the number of items entering the calibration).")
  }
  codes <- list()
  for (i in 1:num_of_items) {
    temp_codes <- sort(setdiff(unique(resp[, items[i], drop = TRUE]), -1))
    # In case all item scores are missing, the item will be marked as a
    # dichotomous item
    temp_max_score <- max(c(1, temp_codes))
    if (temp_max_score > 1) { # polytomous item
      temp_codes <- 0:temp_max_score
    } else { # dichotomous item
      temp_codes <- 0:1
    }
    # Check models:
    if (is.null(model)) {
      models <- c(models, setNames(ifelse(temp_max_score == 1, "3PL", "GPCM2"),
                                   items[i]))
    } else if (temp_max_score > 1 && !models[i] %in% c("GPCM2", "GRM")) {
      warning("Incorrect 'model' argument. For item '", items[i], "', ",
              "the maximum score is ", temp_max_score, " but the model for ",
              "this item has been specified as '", models[i], "'. This item ",
              "will be specified as 'GPCM2' for the analysis.")
      models[i] <- setNames("GPCM2", items[i])
    }

    codes <- c(codes, setNames(list(temp_codes), items[i]))
  }

  return(list(x = x,
              items = items,
              codes = codes,
              models = models,
              examinee_ids = examinee_id,
              data_file_path = target_path,
              num_of_items = num_of_items,
              num_of_groups = 1, # group_info$num_of_groups,
              group_info = group_info, # group_info$group_info,
              reference_group = reference_group # group_info$reference_group,
              ))
}

############################################################################@###
############################# irtpro_read_pars #############################@###
############################################################################@###

#' Function to read IRTPRO parameters file.
#'
#' @param control_file_path File path for the IRTPRO syntax file.
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' irtpro_read_pars()
#' }
irtpro_read_pars <- function(
  target_dir = getwd(),
  analysis_name = "irtpro_calibration",
  test_name = "Test1",
  D = 1) {
  # control_file_path <- normalizePath(control_file_path, mustWork = FALSE)

  ### Read the Parameters ###
  output_files <- list.files(target_dir)
  ## Read Asymptotic Covariance Matrix ##
  temp_fn <- output_files[grepl(paste0("^", analysis_name, "\\.", test_name,
                                       "-cov\\.txt$"), output_files)]
  if (length(temp_fn) > 0 && file.exists(file.path(target_dir, temp_fn))) {
    asym_cov_mat <- utils::read.table(file = file.path(target_dir, temp_fn),
                               header = FALSE, sep = ",")
    # The last column is all NA, so remove it.
    asym_cov_mat <- asym_cov_mat[, -ncol(asym_cov_mat)]
    se <- sqrt(diag(as.matrix(asym_cov_mat)))
  }
  ## Read the parameters
  temp_fn <- output_files[grepl(paste0("^", analysis_name, "\\.", test_name,
                                       "-prm\\.txt$"), output_files)]
  stopifnot(file.exists(file.path(target_dir, temp_fn)))
  pars <- max(utils::count.fields(file.path(target_dir, temp_fn), "\t"))
  pars <- utils::read.table(file = file.path(target_dir, temp_fn),
                     header = FALSE, fill = TRUE, col.names = paste0("V", 1:pars),
                     sep = "\t")
  # skip the last row
  pars <- pars[-nrow(pars), ]
  item_ids <- pars[, 1]
  item_models <- pars[, 3]
  num_categories <- pars[, 4]
  item_list <- vector("list", nrow(pars))

  # length(item_ids) + sum(num_categories - 1) + 7
  se_count <- 1
  for (i in 1:length(item_ids)) {
    row <- pars[i, !apply(pars[i, ], 2, is.na)]
    stopifnot(row[,1] == item_ids[i]) # Make sure the item names match
    if (item_models[i] == 1) { # 1 = "3PL"
      # Columns are: , guessing_c
      colnames(row) <- c("item_id", "num_dim", "item_type", "num_cat",
                         "a", "c_intercept", "logit_g", "b", "g")
      # For dichotomous models there should be two categories.
      stopifnot(row$num_cat == 2)
      item_list[[i]] <- item(model = "3PL", a = row$a/D, b = row$b, c = row$g,
                             D = D, item_id = row$item_id)
      se_count <- se_count + 3 # se ofr three parameters: a, b, g
    } else if (item_models[i] == 2 && num_categories[i] == 2) { # 2 = "2PL"
      # Columns are: , guessing_c
      colnames(row) <- c("item_id", "num_dim", "item_type", "num_cat",
                         "a", "c_intercept", "b")
      # For dichotomous models there should be two categories.
      stopifnot(row$num_cat == 2)
      item_list[[i]] <- item(model = "2PL", a = row$a/D, b = row$b, D = D,
                             item_id = row$item_id)
      se_count <- se_count + 2 # se for two parameters: a, b
    } else if (item_models[i] == 2 && num_categories[i] > 2) { # 2 = "Graded"
      # Columns are: , guessing_c
      colnames(row) <- c("item_id", "num_dim", "item_type", "num_cat",
                         "a", paste0("c", 1:(num_categories[i] - 1)),
                         paste0("b", 1:(num_categories[i] - 1))
                         )
      item_list[[i]] <- item(
        model = "GRM", a = row$a/D,
        b = unlist(row[, paste0("b", 1:(num_categories[i] - 1))]),
        D = D, item_id = row$item_id)
      # There is 1 'se' for "a" parameter, "num_categories[i] - 1" se's for
      # thresholds
      se_count <- se_count + 1 + num_categories[i] - 1
    } else if (item_models[i] == 3) {  # 3 = "GP Credit"
      colnames(row)[1:7] <- c("item_id", "num_dim", "item_type", "num_cat",
                              "gpc", "a", "tmattype")
      num_cat <- row$num_cat
      b <- row[, ncol(row) - num_cat]
      d <- row[, (ncol(row) - num_cat + 2):ncol(row)]
      item_list[[i]] <- item(model = "GPCM2", a = row$a/D, b = b, d = d, D = D,
                             item_id = row$item_id)
      # There is 1 'se' for "a" parameter, "num_categories[i] - 1" se's for
      # thresholds
      se_count <- se_count + 1 + num_categories[i] - 1
    }
  }
  return(itempool(item_list))
}


############################################################################@###
############################# irtpro_read_scores ###########################@###
############################################################################@###

#' Function to read IRTPRO parameters file.
#'
#' @param control_file_path File path for the IRTPRO syntax file.
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
#' @examples
#' \dontrun{
#' irtpro_read_scores(
#'   target_dir = file.path("C:/temp/irtpro1"),
#'   analysis_name = "irtpro_calibration")
#' }
irtpro_read_scores <- function(
  target_dir = getwd(),
  analysis_name = "irtpro_calibration",
  examinee_ids = NULL) {

  ### Read the Parameters ###
  output_files <- list.files(target_dir)
  ## Read the parameters
  temp_fn <- output_files[grepl(paste0("^", analysis_name,"(.*)-sco\\.txt$"),
                                output_files)]
  stopifnot(file.exists(file.path(target_dir, temp_fn)))
  scores <- readLines(file.path(target_dir, temp_fn))
  scores <- gsub("^[ ]+", "", scores)
  scores <- gsub("[ ]+", ",", scores)
  scores <- utils::read.delim(text = scores, header = FALSE, sep = ",")
  scores <- scores[, seq(ncol(scores) - 1, ncol(scores)), drop = FALSE]
  colnames(scores) <- c("ability", "se")
  if (!is.null(examinee_ids) && length(examinee_ids) == nrow(scores)) {
    scores <- cbind.data.frame(examinee_id = examinee_ids, scores)
  }

  if (requireNamespace("tibble")) {
    return(tibble::as_tibble(scores))
  } else return(scores)
}


############################################################################@###
############################################################################@###
############################################################################@###
############################################################################@###
############################################################################@###




#' Item Calibration via IRTPRO
#'
#' @description \code{est_irtpro} runs the IRTPRO in batch mode.
#'
#'   This function requires IRTPRO already installed on your computer. The
#'   R program is designed to work on IRTPRO 6.0.
#'
#'   NOTE that sometimes IRTPRO requires administrative privileges to run each
#'   time it is opened. You can reopen R or RStudio with administrator
#'   privileges (right click R or RStudio icon in start menu and select 'More' >
#'   'Run as administrator') to prevent IRTPRO to ask administrator permission
#'   each time it is run.
#'
#' @param x Either a \code{data.frame}, \code{matrix} or
#'   \code{\link{Response_set-class}} object.
#'
#'   It is assumed that item values start from 0 and goes to number of distinct
#'   categories minus one. So, for example, for a polytomous items with four
#'   categories, the score values are assumed to be 0, 1, 2, 3. Recode the data
#'   to follow this pattern.
#' @param model A string or a vector of strings to specify the psychometric
#'   model of the items. Either provide a single model for all items or provide
#'   a vector with the same length as the number of \code{items} where each
#'   value is one of the following: One-parameter logistic model (\code{"1PL"}),
#'   Two-parameter logistic model (\code{"2PL"}), three-parameter logistic model
#'   (\code{"3PL"}), Generalized Partial Credit model (\code{"GPCM2"}), Graded
#'   Response Model (\code{"GRM"}).
#' @param target_dir The directory/folder where the IRTPRO analysis and data
#'   files will be saved. The default value is the current working directory,
#'   i.e. \code{get_wd()}.
#' @param D Scaling constant. The default value is \code{1}. If, for
#'   \code{"2PL"}, \code{"3PL"} or \code{"GRM"} models, the item parameters
#'   needs to be converted to the commonly used normal scale where \code{D =
#'   1.7} or \code{D = 1.702}, change this value. The item discrimination
#'   parameters estimated by IRTPRO will be divided to \code{D} to get
#'   parameters on the new scale.
#' @param analysis_name A short file name that will be used for the data files
#'   created for the analysis.
#' @param items A vector of column names of the \code{x} that
#'   represents the responses. Default value is \code{NULL} where all items
#'   in \code{x} are assumed to be entering the calibration.
#' @param examinee_id_var The column name or number that contains individual
#'   subject IDs. If none is provided (i.e. \code{examinee_id_var = NULL}), the
#'   program will check whether the data provided has row names.
#' @param group_var The column name or number that contains group membership
#'   information if multi-group calibration is desired. Currently, this function
#'   cannot read multi-group calibration results. The default value is
#'   \code{NULL}, where no multi-group analysis will be performed.
#' @param reference_group Represent which group's ability distribution will be
#'   set to mean = 0 and standard deviation = 1. For example, if the value is 1,
#'   then the group whose code is 1 will have ability distribution with mean 0
#'   and standard deviation 1. The default value is \code{NULL}.
#' @param estimation_method A string that can take one of the following values:
#'   \code{"BAEM"} (Bock-Aitkin), \code{"ADQ"} (Adaptive Quadrature).
#'   The methods \code{"MHRM"} (Metropolis-Hastings Robbins-Monro) and
#'   \code{"MHRM"} are not available at this time via this program.
#' @param estimation_args A list with named arguments that will specify the
#'   estimation. Please use one of the following list templates for each
#'   estimation method.
#'   \describe{
#'     \item{\code{"BAEM"}}{\code{list(`E-Step` = c(500, 1e-005),
#'                                     SE = "S-EM",
#'                                     `M-Step` = c(500, 1e-009),
#'                                     Quadrature = c(49, 6),
#'                                     SEM = 0.001,
#'                                     SS = 1e-005)}}
#'     \item{\code{"ADQ"}}{\code{list(`E-Step` = c(100, 0.001),
#'                                    SE = "S-EM",
#'                                    Quadrature = c(9, "GH"),
#'                                    Adaptation = "EAP",
#'                                    Trust = "Fast")}}
#'   }
#'
#'   In the \code{"BAEM"} estimation method, the quadrature default `c(49, 6)`
#'   means the number of quadrature points is equal to 49 and the integration
#'   range from -6 and 6 (maximum value: 6).
#'
#'   In the \code{"ADQ"} estimation method, the quadrature default `c(9, "GH")`
#'   means the number of quadrature points is equal to 9 and the integration
#'   method is Gauss-Hermite.
#'
#' For \code{"SE"} element, the options are \code{"S-EM"}, \code{"M-Step"},
#' \code{"Xpd"}, and \code{"Sandwich"}. See the IRTPRO manual for details.
#' @param scoring_method A string that can take one of the following values:
#'   \code{"EAP"} for Expected-a-Posteriori or \code{"MAP"} for
#'   Maximum-a-Posteriori.
#' @param scoring_args A list with named arguments that will specify the
#'   scoring. The program will automatically add \code{"Score Persons"}.
#'   Following list elements can also be specified (last two elements are
#'   optional):
#'   \code{list(Mean = 0, SD = 1, Minimum = <Minimum Score>,
#'              Maximum = <Maximum Score>)}
#'
#' @param misc_args A list with named arguments that will specify the
#'   miscellaneous arguments such as the number of decimals for the estimated
#'   parameters, the number of processors, etc.
#'   The following elements can be changed:
#'   \code{list(Decimal = 4, Processors = 1, `Min Exp` = 1)}
#'
#' @param print_extra A string vector specifying additional results to be
#'   printed:
#'   `StdRes` (Print table of standardized residuals)
#'   `CTLD` (Compute Chen-Thissen LD and item fit statistics)
#'   `M2` (Compute limited-information overall model fit statistics)
#'   `GOF` (Print each item's goodness of fit frequency table)
#'   `Loadings` (Print factor loadings)
#'   `P-Nums` (Print parameter numbers)
#'   `Diagnostic` (Print diagnostic information)
#' @param constraints A vector of sting commands for constraints section of the
#'   syntax. It is usually used to constrain a parameter to a certain value.
#'   Usually it has the following format:
#'   \code{"Equal = (G1, Item Name, Parameter), (G2, Item Name, Parameter);"}
#'   Here is an example:
#'   \code{c(
#'     "Equal = (G1, Item_1, Slope[0]), (G2, Item_1, Slope[0]);",
#'     "Equal = (G1, Item_1, Intercept[0]), (G2, Item_1, Intercept[0]);",
#'     "Equal = (G1, Item_2, Slope[0]), (G2, Item_2, Slope[0]);",
#'     "Equal = (G1, Item_2, Intercept[0]), (G2, Item_2, Intercept[0]);")}
#'   or:
#'   \code{c("(Item_1, Slope[0]) = 1.3;",
#'           "(Item_1, Intercept[0]) = 2.1;",
#'           "(Item_2, Slope[0]) = 0.7;",
#'           "(Item_2, Intercept[0]) = -1.2;")}
#' @param priors A list that specifies the prior parameters. There are three
#'   possible options.
#'
#'   The value can be \code{NULL} where no prior information will be used.
#'
#'   The value can be a data frame with the following format:
#'   Column names: \code{item_id, parameter, prior_dist, prior_par_1,
#'   prior_par_2}.
#'   \code{item_id} column should match item IDs.
#'   \code{parameter} should be following one of the \code{"Slope[0]"},
#'   \code{"Intercept[0]"}, or \code{"Guessing[0]"}.
#'   \code{prior_par} column should be one of the following values:
#'   \code{"Lognormal"}, \code{"Normal"}, \code{"Beta"}.
#'   \code{prior_par_1} and \code{prior_par_2} should be numeric values for the
#'   prior parameters. For \code{"Normal"} or \code{"Lognormal"},
#'   \code{prior_par_1} can be 0 (mean) and \code{prior_par_2} can be 1
#'   (standard deviation.
#'   For \code{"Beta"}, \code{prior_par_1} can be 4 (mean) and
#'   \code{prior_par_2} can be 16.
#'
#'   The value can be a data frame with the following format if all items
#'   for a model should follow the same priors:
#'   Column names:\code{model, parameter, prior_dist, prior_par_1, prior_par_2}.
#'   The \code{model} column should match the \code{model} argument of the
#'   function. See the \code{model} argument's description to see the available
#'   options.
#' @param overwrite If \code{TRUE} and there are already an IRTPRO analysis
#'   files in the target path with the same name, these file will be
#'   overwritten.
#' @param show_output_on_console logical (not NA), indicates whether to capture
#'   the output of the command and show it on the R console. The default value
#'   is \code{TRUE}.
#' @param irtpro_exe_dir The location of the \code{"ASCII2SSIG64.exe"} and
#'   \code{"IRTPROx64.exe"}. The default location is
#'   \code{file.path("C:/Program Files/IRTPRO 6.0")}.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' \dontrun{
#' resp <- sim_resp(generate_ip(n = 15), rnorm(200), prop_missing = .2)
#' irtpro_calib <- est_irtpro(x = resp, model = "3PL",
#'                            target_dir = file.path("C:/temp/irtpro1"),
#'                            overwrite = TRUE)
#'
#' n_examinee <- 500
#' resp <- sim_resp(generate_ip(model = sample(c("3PL", "GPCM2"), 20, T)),
#'                  rnorm(n_examinee), prop_missing = .2)
#' resp <- cbind.data.frame(examinee_id = paste0("Ex", 1:n_examinee),
#'                          group = sample(c("A", "B"), n_examinee, TRUE),
#'                          resp)
#' irtpro_calib_mixed <- est_irtpro(
#'   x = resp,
#'   items = NULL,
#'   examinee_id_var = "examinee_id",
#'   group_var = "group",
#'   target_dir = file.path("C:/temp/irtpro2"),
#'   overwrite = TRUE)
#' }
#'

est_irtpro <- function(
  x = NULL,
  model = "3PL",
  target_dir = getwd(),
  D = 1,
  analysis_name = "irtpro_calibration",
  items = NULL,
  examinee_id_var = NULL,
  group_var = NULL,
  reference_group = NULL,
  estimation_method = c("BAEM", "ADQ", "MHRM", "MCMC"),
  estimation_args = list(`E-Step` = c(500, 1e-005),
                         SE = "S-EM",
                         `M-Step` = c(500, 1e-009),
                         Quadrature = c(49, 6),
                         SEM = 0.001,
                         SS = 1e-005),
  scoring_method = c("EAP", "MAP"),
  scoring_args = list(Mean = 0, SD = 1),
  misc_args = list(Decimal = 4, Processors = 1, `Min Exp` = 1),
  print_extra = c("StdRes", "CTLD", "M2", "GOF", "Loadings", "P-Nums",
                  "Diagnostic"),
  constraints = NULL,
  priors = data.frame(
    model = c("1PL", "2PL", "2PL", "3PL", "3PL", "3PL"),
    parameter = c("Intercept[0]", "Slope[0]", "Intercept[0]",
                  "Slope[0]", "Intercept[0]", "Guessing[0]"),
    prior_dist = c("Normal", "Lognormal", "Normal", "Lognormal", "Normal",
                   "Beta"),
    prior_par_1 = c(0, 0, 0, 0, 0, 4),
    prior_par_2 = c(2, 1, 2, 1, 2, 16)
    ),
  overwrite = FALSE,
  show_output_on_console = TRUE,
  irtpro_exe_dir = file.path("C:/Program Files/IRTPRO 6.0")
  ) {

  # x = resp
  # model = "3PL"
  # target_dir = file.path("C:/temp/irtpro1")
  # analysis_name = "irtpro_calibration"
  # items = NULL
  # examinee_id_var = NULL
  # group_var = NULL
  # reference_group = NULL
  # estimation_method = "BAEM"
  # estimation_args = list(`E-Step` = c(500, 1e-005),
  #                        SE = "S-EM",
  #                        `M-Step` = c(500, 1e-009),
  #                        Quadrature = c(49, 6),
  #                        SEM = 0.001,
  #                        SS = 1e-005)
  # scoring_method = "MAP"
  # scoring_args = list(Mean = 0, SD = 1)
  # misc_args = list(Decimal = 4, Processors = 1, `Min Exp` = 1)
  # print_extra = c("StdRes", "CTLD", "M2", "GOF", "Loadings", "P-Nums", "Diagnostic")
  # constraints = NULL
  # priors = data.frame(
  #   model = c("1PL", "2PL", "2PL", "3PL", "3PL", "3PL"),
  #   parameter = c("Intercept[0]", "Slope[0]", "Intercept[0]",
  #                 "Slope[0]", "Intercept[0]", "Guessing[0]"),
  #   prior_dist = c("Normal", "Lognormal", "Normal", "Lognormal", "Normal",
  #                  "Beta"),
  #   prior_par_1 = c(0, 0, 0, 0, 0, 4),
  #   prior_par_2 = c(2, 1, 2, 1, 2, 16)
  #   )
  # overwrite = TRUE
  # show_output_on_console = TRUE
  # irtpro_exe_dir = file.path("C:/Program Files/IRTPRO 6.0")

  result <- list(
                 # ctt = NULL,
                 # failed_items = NULL,
                 syntax = NULL,
                 input = c(as.list(environment()), call = match.call()),
                 score = NULL,
                 ip = NULL
                 )

  result$input$x <- NULL


  # RDS file where the output of the calibration will be saved.
  result_fn <- file.path(target_dir, paste0(analysis_name, ".rds"))
  if (!overwrite && file.exists(result_fn)) {
    temp_result <- readRDS(result_fn)
    # The names in input that can differ but not affect calibration results
    temp <- c("overwrite", "call", "show_output_on_console")
    if (identical(temp_result$input[!names(temp_result$input) %in% temp],
                  result$input[!names(temp_result$input) %in% temp]))
      return(temp_result)
  }

  estimation_method <- match.arg(estimation_method)
  scoring_method <- match.arg(scoring_method)


  available_models <- c("1PL", "2PL", "3PL", "GPCM2", "GRM")
  if (!all(model %in% available_models))
    stop("Invalid 'model' argument. All elements of the 'model' argument ",
         " should be either '", paste0(available_models, collapse = "', '"),
         "'.", call. = FALSE)


  # Check priors argument
  if (!is.null(priors)) {
    if (!inherits(priors, "data.frame")) {
      stop("Invalid 'priors' argument. 'priors' argument should either be ",
           "NULL or be a data.frame object.")
    }
    if (!all(c("parameter", "prior_dist", "prior_par_1", "prior_par_2") %in%
             colnames(priors))) {
      stop("Invalid 'priors' argument. 'priors' should have the following ",
           "column names: 'parameter', 'prior_dist', 'prior_par_1', ",
           "'prior_par_2'.")
    }
    if (!any(c("item_id", "model") %in% colnames(priors))) {
      stop("Invalid 'priors' argument. 'priors' should have either one of the ",
           "following column names: 'model' or 'item_id'.")
    }
  }
  irtpro_command <- file.path(irtpro_exe_dir, "IRTPROx64.exe")

  test_name <- "Test1"

  # If overwrite is TRUE, delete important output files:
  if (overwrite) {
    temp_file_list <- list.files(target_dir, include.dirs = FALSE)
    temp <- temp_file_list[grepl(
      paste0("^", analysis_name, "\\.(irtpro|csv|ssig|rds)$"), temp_file_list)]
    suppressWarnings(file.remove(file.path(target_dir, temp)))
    temp <- temp_file_list[grepl(paste0("^", analysis_name, "\\.", test_name,
                                        "-(cov|dbg|inf|irt|prm|sco|ssc)",
                                        "\\.(irtplot|txt|htm)$"), temp_file_list)]
    suppressWarnings(file.remove(file.path(target_dir, temp)))
  }

  syntax_file_path <- file.path(target_dir, paste0(analysis_name, ".irtpro"))

  data_output <- irtpro_create_data_file(
    x = x,
    items = items,
    model = model,
    examinee_id_var = examinee_id_var,
    group_var = group_var,
    reference_group = reference_group,
    target_path = file.path(target_dir, paste0(analysis_name, ".ssig")),
    irtpro_exe_dir = irtpro_exe_dir,
    overwrite = overwrite)
  # for non-multigroup data num_of_groups is 0.
  num_of_groups <- data_output$num_of_groups
  group_info <- data_output$group_info
  reference_group <- data_output$reference_group
  models <- data_output$models
  codes <- data_output$codes
  items <- data_output$items
  examinee_ids <- data_output$examinee_ids

  # in case x is Response_set, make sure x is matrix after this point.
  x <- data_output$x
  tab <- "    "

  ## Project
  # Provide the name of the project
  syntax_text <- "Project:"
  syntax_text <- c(syntax_text,
                   paste0(tab, "Name = ", analysis_name, ";"))

  ## Data
  # Provide the location and the name of the .SSIG data file for the
  # calibration. If the control file and the `.SSIG` data are in the same
  # folder, a ".\\\" followed by the name of the dataset with `.SSIG`
  # extension should work. Otherwise, the entire path of the data set must
  # be given.
  syntax_text <- c(syntax_text, "", "Data:")
  syntax_text <- c(syntax_text,
                   paste0(tab, "File = .\\",
                          basename(data_output$data_file_path), ";"))

  ## Analysis
  # Provide the name and mode of the analysis. There are three
  # options for the keyword “mode”: traditional, calibration, and scoring.
  syntax_text <- c(syntax_text, "", "Analysis:")
  syntax_text <- c(syntax_text,
                   paste0(tab, "Name = ", test_name, ";"),
                   paste0(tab, "Mode = CALIBRATION;"))

  ## Title and Comments
  # Provide the name and mode of the analysis. There are three
  # options for the keyword “mode”: traditional, calibration, and scoring.
  syntax_text <- c(syntax_text, "", "Title:",
                   paste0(tab, analysis_name),
                   "", "Comments:", "")

  ## Estimation
  # Provide the parameter estimation method. The first key word in this
  # command is “Method”, which has three options: BAEM (Bock-Aitkin),
  # ADQ (Adaptive Quadrature), and MHRM (Metropolis-Hastings Robbins-Monro).
  # Once the method is specified, the values for the remaining key words are
  # determined.
  syntax_text <- c(syntax_text, "", "Estimation:")
  syntax_text <- c(syntax_text,
                   paste0(tab, "Method = ", estimation_method, ";"))
  syntax_text <- c(
    syntax_text,
    paste0(tab, names(estimation_args), " = ",
           sapply(estimation_args, paste, collapse = ", "), ";"))

  ## Save
  # PRM (Item parameter estimates –prm.txt)
  # COV (Asymptotic covariance matrix of the parameter estimates –cov.txt)
  # INF (Information values, unidimensional models only –inf.txt)
  # POL (Inter item polychoric correlations, unidimensional models only –pol.txt)
  # FAC (Factor loadings –fac.txt)
  # IRT (Main output in ASCII text format –irt.txt, -sss.txt and –ssc.txt)
  # DBG (Debugging output –dbg.txt)
  syntax_text <- c(syntax_text, "", "Save:",
                   paste0(tab, "PRM, COV, INF, POL, FAC, IRT, DBG"))

  ## Scoring
  # Provide the theta estimate for each student included in the calibration.
  syntax_text <- c(syntax_text, "", "Scoring:",
                   paste0(tab, "Pattern = ", scoring_method, ";"),
                   paste0(tab, "Score Persons;"),
                   paste0(tab, names(scoring_args), " = ", scoring_args,
                          ";")
                   )
  ## Miscellaneous
  # To specify the number of decimals for the estimated parameters,
  # the number of processors, etc.
  syntax_text <- c(syntax_text, "", "Miscellaneous:",
                   paste0(tab, names(misc_args), " = ", misc_args,
                          ";"),
                   paste0(tab, "Print ", paste0(print_extra, collapse = ", "),
                          ";")
                   )

  ## Groups
  # Specify the grouping variable for a multiple-group analysis
  syntax_text <- c(syntax_text, "", "Groups:")

  ## Group
  # To specify the dimensionality, items included in the IRT calibration,
  # the score categories of the items, and the calibration models (2PL or 3PL)
  # for dichotomous items, GP credit for polytomous items).
  syntax_text <- c(syntax_text, "", "Group :",
                   paste0(tab, "Dimension = 1;"))
  syntax_text <- c(
    syntax_text,
    strsplit(wrap_text(paste0("Items = ", paste0(items, collapse = ", "), ";"),
                       tab = tab), split = "\n", fixed = TRUE)[[1]]
    )
  # Add codes
  for (i in 1:length(items)) {
    temp_model <- switch(tolower(models[i]),
                         `1pl` = "1PL", `2pl` = "2PL", `3pl` = "3PL",
                         `gpcm2` = "GP Credit", `grm` = "Graded")
    syntax_text <- c(
      syntax_text,
      # Add Codes
      paste0(tab, "Codes(", items[i], ") = ",
             paste0(codes[[i]], "(", codes[[i]], ")", collapse = ","), ";"),
      # Add Model
      paste0(tab, "Model(", items[i], ") = ", temp_model, ";")
      )
    # Add GammaMatrix for polytomous items
    if (models[i] %in% c("GPCM2", "GRM")) {
      syntax_text <- c(syntax_text,
                       paste0(tab, "GammaMatrix(", items[i], ") = Trend;"))
    }
  }
  # Add Mean and Covariance
  syntax_text <- c(syntax_text, "Mean = 0.0;", "Covariance = 1.0;")


  ## Constraints
  syntax_text <- c(syntax_text, "", "Constraints:", constraints)

  ## Priors
  # Set up the prior values. It is needed when the extreme b parameter occurs.
  if (!is.null(priors)) {
    syntax_text <- c(syntax_text, "", "Priors:")

    if ("item_id" %in% colnames(priors)) {
      if (!all(priors$item_id %in% items)) {
        stop("Invalid 'priors' argument. All of the values in the 'item_id' ",
             "column should be in the response data. Following item ids ",
             "cannot be found in the response data:",
             paste0(priors$item_id[!priors$item_id %in% items], collapse = ", "))
      }
      syntax_text <- c(
        syntax_text,
        paste0(tab, "(", priors$item_id, ", ", priors$parameter, ") = ",
               priors$prior_dist, ", ", priors$prior_par_1, ", ",
               priors$prior_par_2, ";"))
    } else if ("model" %in% colnames(priors)) {
      for (i in 1:length(items)) {
        temp_priors <- priors[priors$model == models[i], ]

        for (j in seq_len(nrow(temp_priors))) {
          syntax_text <- c(
            syntax_text,
            paste0(tab, "(", items[i], ", ", temp_priors$parameter[j], ") = ",
                   temp_priors$prior_dist[j], ", ", temp_priors$prior_par_1[j],
                   ", ", temp_priors$prior_par_2[j], ";"))
        }
      }
    }
  }


  syntax_text <- c(syntax_text, "") # add empty line to the end
  result$syntax <- syntax_text

  ## Save the IRTPRO control file ##
  if (overwrite || !file.exists(syntax_file_path))
    writeLines(text = syntax_text, syntax_file_path, sep = "\n")

  ## Run the Analysis ##
  check_file <- function(extension) {
    temp <- list.files(target_dir)
    fp <- file.path(target_dir, file.path(temp[
      grepl(paste0("^", analysis_name,"(.*)\\.", extension, "$"), temp)]))
    return(length(fp) > 0 && all(file.exists(fp)))
  }

  if (overwrite || !check_file("htm")) {
    # The full command to run:
    command <- paste0('"', irtpro_command, '" -Run "', syntax_file_path, '"')
    # cat(command, "\n")
    # Run the command only if the file is not already exists.
    counter <- 0 # make sure that the loop ends
    while (!file.exists(syntax_file_path) && (20 > (counter <- counter + 1))) {
      # cat(paste0("Waiting IRTPRO syntax to be ready. (", counter, " sec.)\n"))
      Sys.sleep(1)
    }
    # system(command, intern = TRUE, show.output.on.console = TRUE, wait = FALSE,
    #        invisible = FALSE)
    system("cmd.exe", input = command)

    # Wait until the analysis ends.
    counter <- 0 # make sure that the loop ends
    total_wait_secs <- max(round(prod(dim(data_output$x)) / 200), 120)
    # message(paste0("\n\nWaiting IRTPRO to complete the analysis. \nStarted at ",
    #                format(Sys.time(), "%T"), ". Approximate wait time is ",
    #                round(total_wait_secs/60, 1), " minutes.\n"))
    temp_fn <- file.path(target_dir, paste0(
      analysis_name, ".", test_name, "-", c("prm", "sco"), ".txt"))
    # pb <- utils::txtProgressBar(min = 0, max = total_wait_secs, style = 3)
    while (TRUE) {
      counter <- counter + 5
      Sys.sleep(5)
      if (check_file("errlog")) break
      if (counter > total_wait_secs) {
        warning("Warning: IRTPRO ran significantly more than expected. ",
                "Try setting up 'overwrite = FALSE' and rerun the analysis ",
                "if you see the following files in target directory:\n",
                paste0(temp_fn, collapse = "\n"))
        break
      }
      # Check whether parameter file and score files has been created.
      # "irtpro_calibration.Test1-prm.txt" + "irtpro_calibration.Test1-sco.txt"
      if (all(file.exists(temp_fn))) break
      # utils::setTxtProgressBar(pb, counter)
    }
    # close(pb)
  }
  if (check_file("errlog")) {
    stop(paste0("An error occured during IRTPRO analysis. Please check:\n ",
                file.path(target_dir, paste0(analysis_name, ".errlog"))))
  } else if (check_file("htm")) {
    # cat(paste0("\n\nFinished analysis at ", format(Sys.time(), "%T"),
    #            "and reading the item parameters from the IRTPRO output.\n"))
    result$ip <- irtpro_read_pars(target_dir = target_dir,
                                  analysis_name = analysis_name,
                                  test_name = test_name, D = D)
    result$score <- irtpro_read_scores(target_dir = target_dir,
                                       analysis_name = analysis_name,
                                       examinee_ids = examinee_ids)
  } else stop(paste0(
    "\nIRTPRO did not produce any results so far. Possibly the analysis is ",
    "taking longer than the anticipated time and IRTPRO is still running ",
    "in the background. \n"))


  saveRDS(object = result, file = result_fn)
  return(result)
}

