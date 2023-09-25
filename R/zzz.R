

.onUnload <- function(libpath) {
  library.dynam.unload("irt", libpath)
}

# This is to inject the `.data` object to the global environment.
# This is mainly for ggplot2 functions `aes` arguments.
# See: https://rlang.r-lib.org/reference/dot-data.html
#
if (getRversion() >= "2.15.1") utils::globalVariables(c(".data"))
