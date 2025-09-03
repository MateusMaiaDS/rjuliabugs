#' Convert to posterior::rvar format
#'
#' Generic function to convert an object to a [`posterior::rvar`] representation.
#' This is typically used to convert Markov Chain Monte Carlo (MCMC) output into a more flexible and vectorized format.
#'
#' @param x An object to convert (e.g., a `rjuliabugs` object or a 3D numeric array).
#' @param n_mcmc (For arrays only) Number of Markov Chain Monte Carlo (MCMC) chains. Required if `x` is an array.
#' @param ... Further arguments passed to specific methods.
#'
#' @return An object of class `rvar`, or an updated `rjuliabugs` object with `params` converted to `rvar`.
#'
#' @seealso [posterior::rvar()]
#'
#' @export
#' @md

as_rvar <- function(x, ...) {
  UseMethod("as_rvar")
}


#' Convert to coda::mcmc or mcmc.list format
#'
#' Generic function to convert Markov Chain Monte Carlo (MCMC) output to [`coda::mcmc`] or [`coda::mcmc.list`] format.
#'
#' @param x An object to convert (e.g., a `rjuliabugs` object or a 3D array).
#' @param ... Further arguments passed to specific methods.
#'
#' @return An object of class `mcmc`, `mcmc.list`, or a `rjuliabugs` object with updated `params`.
#'
#' @seealso [coda::as.mcmc()], [coda::mcmc()]
#'
#' @export
#' @md

as_mcmc <- function(x, ...) {
  UseMethod("as_mcmc")
}

#' Convert to posterior::draws format
#'
#' Generic function to convert an object to a [`posterior::draws`] representation
#' (e.g., `draws_array`, `draws_matrix`, or other formats).
#'
#' @param x An object to convert (e.g., a `rjuliabugs` object or a 3D array).
#' @param ... Further arguments passed to specific methods.
#'
#' @return A `draws` object (e.g., `draws_array`) or a modified `rjuliabugs` object.
#'
#' @seealso [posterior::as_draws()]
#'
#' @export
#' @md
as_draws <- function(x, ...) {
  UseMethod("as_draws")
}
