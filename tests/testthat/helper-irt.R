
#' Run a piece of code with a given seed
#'
#' @details
#' This function runs the code in isolation and resumes to the regular seed
#' after it completes running the code.
#'
#' Kudos to MrFlick: https://stackoverflow.com/a/56192474/2275286
#'
#' @param seed An integer representing the seed
#' @param code Code to be run (see examples for a demo)
#'
#' @noRd
#'
#' @examples
#' # Run the code with a set seed:
#' run_with_seed(12, {
#'   mean(rnorm(30))
#' })
#'
#' # Resume to the regular seed
#' mean(rnorm(30))
#'
run_with_seed <- function(seed, code) {
  code <- substitute(code)
  orig.seed <- .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  set.seed(seed)
  eval.parent(code)
}
