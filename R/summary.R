#' Summary of MCMC Chains from RJuliaBugs
#'
#' Retrieves and displays summary statistics of MCMC chains generated via RJuliaBugs,
#' using Julia commands executed through the \code{JuliaCall} package.
#' The function prints a limited number of rows of the summary and indicates if rows are omitted.
#'
#' @param object An object containing the Julia sampler name accessible via \code{object$sampler_name}.
#' @param digits Number of digits to display when printing the summary (default is 4).
#' @param n_display Maximum number of rows to display in the printed summary (default is 10).
#'
#' @return A \code{data.frame} with the summarized statistics extracted from the Julia sampler.
#'
#' @details
#' This function calls Julia commands through \code{JuliaCall} to fetch summary statistics
#' from an MCMC chain and then formats and prints a portion of the result in R.
#' If the total number of rows exceeds \code{n_display}, a visual indicator is printed
#' to show that some rows have been omitted.
#'
#' @examples
#' \dontrun{
#' # Assuming 'object' is an rjulibugs object
#' summary.rjuliabugs(object, digits = 3, n_display = 8)
#' }
#'
#' @importFrom JuliaCall julia_eval
#' @export
summary.rjuliabugs <- function(object, digits = 4, n_display = 10) {

  JuliaCall::julia_eval("using DataFrames")
  JuliaCall::julia_eval(paste0("df = DataFrame(MCMCChains.summarystats(",object$sampler_name,").nt)"),need_return = "Julia")
  JuliaCall::julia_eval(paste0("df.parameters = String.(df.parameters)"),need_return = "Julia")
  JuliaCall::julia_eval(paste0("df.ess_bulk = Float64.(df.ess_bulk)"),need_return = "Julia")

  summary_obj <- JuliaCall::julia_eval(paste0("df"))

  cat("Summary Statistics:\n\n")
  (print(summary_obj[1:n_display,,drop=FALSE],digits = digits,row.names = FALSE))

  n_display_val <- min(n_display, nrow(summary_obj))


  if (nrow(summary_obj) > n_display_val) {
    cat(format("         ⋮\n", width = 14))
    cat(sprintf("  %d rows omitted\n", nrow(summary_obj) - 10))
  }

  return(summary_obj)
}
