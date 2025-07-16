#' @keywords internal
.onAttach <- function(libname, pkgname) {
  rjuliabugs::setup_juliaBUGS()
}

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Calling setup_juliaBUGS() to use rjuliabugs package \n")
}
