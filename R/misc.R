
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% get_max_possible_total_score %%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' Calculate the maximum score of a set of items
#' @param ip An \code{\link{Itempool-class}} object.
#' @param resp (optional) A response vector or a response matrix. The contents
#'   are not important. The function only checks whether an element is missing
#'   or not. If an element is missing, then that item will not count towards
#'   the maximum possible score. If the maximum score of all items are needed,
#'   set \code{resp = NULL}.
#'
#' @return A vector of numbers showing the maximum possible scores.
#'
#' @export
#'
#' @author Emre Gonulates
#'
#' @examples
#' ip <- generate_ip(n = 10)
#' get_max_possible_total_score(ip)
#' # A mixture of dichotomous and polytomous items
#' ip <- generate_ip(model = c("3PL", "GRM", "3PL", "GRM", "GRM"),
#'                   n_categories = c(2, 5, 2, 4, 6))
#' # 1 + 4 + 1 + 3 + 5 = 14
#' get_max_possible_total_score(ip)
get_max_possible_total_score <- function(ip, resp = NULL) {
  if (!is(ip, "Itempool"))
    stop("ip should be an 'Itempool' object.")
  max_scores <- ip$item_max_score
  if (!is.null(resp)) {
    ip_size <- length(max_scores)
    if (is.vector(resp)) {
      if (length(resp) != ip_size)
        stop(paste0("The number of elements of the 'resp' vector should be ",
                    ip_size, ".\n"))
      return(sum(max_scores[!is.na(resp)]))
    } else if (is.matrix(resp)) {
      if (ncol(resp) != ip_size)
        stop(paste0("The number of columns of the 'resp' matrix should be ",
                    ip_size, ".\n"))
      return(rowSums((1*!is.na(resp)) * matrix(
        max_scores, ncol = ncol(resp), nrow = nrow(resp), byrow = TRUE)))
    }
  }
  return(sum(max_scores))
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% convert_resp %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Convert responses to wide format
#'
#' @param resp A \code{matrix} or \code{data.frame} containing the item
#'   responses. By default, it is assumed that the first column is examinee
#'   ids, second column is item ID's and third column is responses.
#' @param examinee_id_col Column name or the column number of the examinee ID
#'   column.
#' @param item_id_col Column name or the column number of the item ID column.
#' @param resp_col Column name or the column number of the response column.
#' @param to \code{wide}: convert response data from long to wide.
#'   \code{long}: convert response data from wide to long.
#'
#' @return A response \code{matrix} in wide format where row names are
#'   examinee ID's and column names are item ID's.
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
convert_resp <- function(resp, examinee_id_col = NULL, item_id_col = NULL,
                         resp_col = NULL, to = "wide") {
  examinee_id_col <- ifelse(is.null(examinee_id_col), 1, examinee_id_col)
  item_id_col <- ifelse(is.null(item_id_col), 2, item_id_col)
  resp_col <- ifelse(is.null(resp_col), 3, resp_col)
  # Order the columns of resp vector and remove unnecessary columns
  resp <- resp[, c(examinee_id_col, item_id_col, resp_col)]
  colnames(resp) <-  c("examinee_id", "item_id", "score")
  # Get unique examinee and item ids
  examinee_id <- as.character(unique(resp[[examinee_id_col]]))
  item_id <- as.character(unique(resp[[item_id_col]]))
  # Create a full item response vector in case there are missing values
  temp <- cbind(expand.grid(item_id = item_id, examinee_id = examinee_id,
                              stringsAsFactors = FALSE)[, 2:1])
  temp <- merge(temp, resp, by = c("examinee_id", "item_id"), all.x = TRUE)
  return(matrix(temp[["score"]], ncol = length(item_id), byrow = TRUE,
                dimnames = list(examinee_id, item_id)))
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% format_text %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Colourise text for display in the terminal.
#'
#' If R is not currently running in a system that supports terminal colours
#' the text will be returned unchanged.
#'
#' Allowed colours are: black, blue, brown, cyan, dark gray, green, light
#' blue, light cyan, light gray, light green, light purple, light red,
#' purple, red, white, yellow
#'
#' # This function is literally taken from Hadley Wickham's testthat package:
#' https://github.com/r-lib/testthat/blob/
#'   717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r
#'
#' More formatting options can be found here:
#' https://stackoverflow.com/a/33206814/2275286
#'
#' @param text character vector
#' @param fg foreground colour, defaults to white
#' @param bold whether the text should appear in bold
#' @param italic whether the text should appear in italic
#'
#' @author Emre Gonulates
#'
#' @keywords internal
#'
#' @noRd
#'
format_text <- function(text, fg = "black",
                        # bg = NULL,
                        bold = FALSE,
                        italic = FALSE) {

  # Examples:
  # print(format_text("Yellow color", "yellow"))
  # cat(format_text("Blue color", "blue"), "\n")
  # cat(format_text("Gray text", "light gray"), "\n")
  # cat(format_text("Bold and green", "green", bold = TRUE), "\n")
  # cat(format_text("Italic and purple", "purple", italic = TRUE), "\n")
  .fg_colours <- c(
    "black" = "0;30",
    "blue" = "0;34",
    "green" = "0;32",
    "cyan" = "0;36",
    "red" = "0;31",
    "purple" = "0;35",
    "brown" = "0;33",
    "light gray" = "0;37",
    "dark gray" = "1;30",
    "light blue" = "1;34",
    "light green" = "1;32",
    "light cyan" = "1;36",
    "light red" = "1;31",
    "light purple" = "1;35",
    "yellow" = "1;33",
    "white" = "1;37"
  )

  .bg_colours <- c(
    "black" = "40",
    "red" = "41",
    "green" = "42",
    "brown" = "43",
    "blue" = "44",
    "purple" = "45",
    "cyan" = "46",
    "light gray" = "47"
  )

  rcmd_running <- function() {
    nchar(Sys.getenv('R_TESTS')) != 0
  }



  term <- Sys.getenv()["TERM"]
  colour_terms <- c("xterm-color","xterm-256color", "screen", "screen-256color")

  if(rcmd_running() || !any(term %in% colour_terms, na.rm = TRUE)) {
    return(text)
  }

  col_escape <- function(col) {
    paste0("\033[", col, "m")
  }


  col <- .fg_colours[tolower(fg)]
  # if (!is.null(bg)) {
  #   col <- paste0(col, .bg_colours[tolower(bg)], sep = ";")
  # }

  # Add bold face
  col <- ifelse(bold, paste0(col, ";1"), col)
  col <- ifelse(italic, paste0(col, ";3"), col)

  init <- col_escape(col)
  reset <- col_escape("0")
  paste0(init, text, reset)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% is_integer ###%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#' Check whether a value is integer
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
is_integer <- function(x, tol = 10 * .Machine$double.eps) {
  if (is.atomic(x) && !is.matrix(x) && length(x) > 0) {
    return(sapply(x, function(k) is.numeric(k) &&
             (abs(k - round(k)) <= tol & !is.infinite(k))))
  } else if (is.null(x)) {
    return(FALSE)
  } else if (length(x) == 0) {
    return(FALSE)
  } else if (inherits(x, "data.frame")) {
    return(sapply(x, is_integer))
  } else if (is.matrix(x)) {
    return(apply(x, 2, is_integer))
  }
  return(rep(FALSE, length(x)))
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% is_single_value %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' Check whether an object is a single length vector
#'
#' \code{NULL} is not acceptable and returns \code{FALSE}
#'
#' @param x An object to be tested
#' @param class class of the object. It can be either \code{NULL},
#'   \code{"numeric"}, \code{"integer"}, \code{"character"}, \code{"logical"},
#'   \code{"complex"} or \code{Date}. When it is \code{NULL}, it can be either
#'   one of the five classes above.
#' @param accept_na If \code{TRUE}, the object is allowed to be a \code{NA}.
#'
#' @return Either \code{TRUE} or \code{FALSE}
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
#' @examples
#' is_single_value(12)   # TRUE
#' is_single_value(12, class = "numeric")   # TRUE
#' is_single_value(12, class = "character")   # FALSE
#' is_single_value("12", class = "character")   # TRUE
#' is_single_value(12, class = "logical")   # FALSE
#'
#' is_single_value(c(12, 18), class = "numeric")   # FALSE
#' is_single_value(12L, class = "numeric")   # FALSE
#' is_single_value(12L, class = "integer")   # TRUE
#'
#' is_single_value(12, class = c("numeric", "integer"))   # TRUE
#' is_single_value(12L, class = c("numeric", "integer"))   # TRUE
#' is_single_value(1:5, class = c("numeric", "integer"))   # FALSE
#'
#' is_single_value(as.Date("2021-01-01"))   # TRUE
#'
is_single_value <- function(x, class = NULL, accept_na = FALSE) {
  # if (is.null(accept_na) || !is.atomic(accept_na) || is.matrix(accept_na) ||
  #     length(accept_na) != 1 || !is.logical(accept_na))
  #   stop("Invalid 'accept_na'.")
  if (is.null(x)) return(FALSE)
  if (!accept_na && all(is.na(x))) return(FALSE)
  # First check if the value is a single value
  if (!is.atomic(x) || is.matrix(x) || length(x) > 1) return(FALSE)

  # Check class
  acceptable_classes <- c("numeric", "integer", "character", "logical",
                          "complex", "Date")
  if (!is.null(class) && !all(class %in% acceptable_classes))
    stop("'class' should be either 'numeric', 'integer', 'character', ",
         "'logical', 'complex' or NULL.", call. = FALSE)

  if (is.null(class)) class <- acceptable_classes

  if (length(class) == 1 && class == "integer") return(is_integer(x))

  return(class(x) %in% class)
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% is_atomic_vector %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' Check whether an object is an atomic vector
#'
#' \code{NULL} is not acceptable and returns \code{FALSE}
#'
#' @param x An object to be tested
#' @param class class of the object. It can be either \code{NULL},
#'   \code{"numeric"}, \code{"integer"}, \code{"character"}, \code{"logical"},
#'   \code{"complex"} or \code{"Date"}. When it is \code{NULL}, it can be either
#'   one of the five classes above.
#' @param accept_na If \code{TRUE}, the object is allowed to be a \code{NA}.
#'
#' @return Either \code{TRUE} or \code{FALSE}
#'
#' @keywords internal
#'
#' @author Emre Gonulates
#'
#' @noRd
#'
#' @examples
#' is_atomic_vector(1:6)
#' is_atomic_vector(1:6, class = "integer")
#' is_atomic_vector(1:6, class = "numeric")
#' is_atomic_vector(1:6, class = c("integer", "numeric"))
is_atomic_vector <- function(
  x,
  class = c("numeric", "integer", "character", "logical", "complex", "Date"),
  accept_na = TRUE) {
  # if (is.null(accept_na) || !is.atomic(accept_na) || is.matrix(accept_na) ||
  #     length(accept_na) != 1 || !is.logical(accept_na))
  #   stop("Invalid 'accept_na'.")
  if (is.null(x) || !is.atomic(x) || is.matrix(x)) return(FALSE)
  if (!accept_na && any(is.na(x))) return(FALSE)

  # Check class
  acceptable_classes <- c("numeric", "integer", "character", "logical",
                          "complex", "Date")
  if (!is.null(class) && !all(class %in% acceptable_classes))
    stop("'class' should be either 'numeric', 'integer', 'character', ",
         "'logical', 'complex' or NULL.", call. = FALSE)

  if (is.null(class)) class <- acceptable_classes

  if (length(class) == 1 && class == "integer") return(all(is_integer(x)))

  return(class(x) %in% class)
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% rmsd %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' Calculate Root Mean Square Deviation (RMSD)
#'  (or Root Mean Square Error (RMSE))
#'
#' @description The formula for RMSD is:
#'
#' \deqn{RMSD = \sqrt{\frac{\sum_{i = 1}^n(\hat \theta - \theta)^2}{n}}}
#'
#' @param est A numeric vector of estimated values
#' @param true A numeric vector of true values
#'
#' @return A number representing RMSD.
#'
#' @author Emre Gonulates
#'
#' @keywords internal
#'
#' @examples
#' true <- rnorm(10)
#' est <- true + runif(10)
#'
rmsd <- function(est, true) {

  if (!is_atomic_vector(true, class = c("numeric", "integer")) ||
      !is_atomic_vector(est, class = c("numeric", "integer")))
    stop("Both 'est' and 'true' should be numeric.")
  if (length(est) != length(true))
    stop("The lengths of 'est' and 'true' should be equal")

  sqrt(mean((est - true)^2, na.rm = TRUE))
}


