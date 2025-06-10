#' Summary Method for JuliaBUGS Sampler Output
#'
#' Provides a summary of the results from a JuliaBUGS sampler object, including MCMC settings,
#' summary statistics, and optionally quantiles.
#'
#' @param object An object of class `rjuliabugs` containing a reference to a Julia MCMC sampler.
#' @param digits Integer. Number of significant digits to display in printed summary statistics.
#' @param n_display Integer. Maximum number of rows of summary statistics to display.
#' @param get_summary Logical. If `TRUE`, returns the summary statistics as part of the result.
#' @param get_quantiles Logical. If `TRUE`, includes quantile statistics in the result.
#' @param julia_summary_only Logical. If `TRUE`, displays the Julia summary object directly in Julia, skipping R summary output.
#'
#' @return If `julia_summary_only = TRUE`, returns invisibly `NULL` after displaying the Julia object.
#'         Otherwise, returns a list possibly containing elements:
#' \describe{
#'   \item{summary}{A data frame of summary statistics from Julia.}
#'   \item{quantiles}{A data frame of quantiles if `get_quantiles = TRUE`.}
#' }
#'
#' @details
#' This method wraps Julia's `MCMCChains.summarystats` and `MCMCChains.quantile` to extract and display
#' results in R using the `JuliaCall` interface. It also extracts key MCMC settings like number of chains,
#' iterations, and samples per chain. The printed summary is truncated to `n_display` rows.
#'
#' @importFrom JuliaCall julia_eval
#' @method summary rjuliabugs
#' @export
summary.rjuliabugs <- function(object,
                               digits = 4, n_display = 10,
                               get_summary = TRUE,
                               get_quantiles = TRUE,
                               julia_summary_only = FALSE) {

  if(julia_summary_only){
    JuliaCall::julia_eval(paste0("display(",object$sampler_name,")"),need_return = "Julia")
    return(invisible(NULL))
  } else {
    JuliaCall::julia_eval("using DataFrames")

    JuliaCall::julia_eval(paste0("df = DataFrame(MCMCChains.summarystats(",object$sampler_name,").nt)"),need_return = "Julia")
    JuliaCall::julia_eval(paste0("df.parameters = String.(df.parameters)"),need_return = "Julia")
    JuliaCall::julia_eval(paste0("df.ess_bulk = Float64.(df.ess_bulk)"),need_return = "Julia")

    ## Getting params names
    str_params_name <- unlist(JuliaCall::julia_eval(paste0("String.(keys(get_params(",object$sampler_name,")))"),need_return = "R"))
    str_params_name <- str_params_name[1:(which(str_params_name == "lp") - 1)]

    ## Retrieving the MCMC settings
    mcmc_settings <- break_string_to_numeric(as.character(JuliaCall::julia_eval(paste0("range(",object$sampler_name,")"),need_return = "Julia")))
    mcmc_n_chains <- break_string_to_numeric(as.character(JuliaCall::julia_eval(paste0("size(",object$sampler_name,", 3)"),need_return = "Julia")))
    cat("Summary of JuliaBUGS sampler:\n\n")
    cat(paste0("Iterations        = ",mcmc_settings[3],"\n"))
    cat(paste0("Number of chains = ",mcmc_n_chains,"\n"))
    cat(paste0("Number of posterior samples = ",floor((mcmc_settings[3]-mcmc_settings[1]+1)/mcmc_settings[2]),"\n"))
    cat(paste0("Thinning parameter = ",mcmc_settings[2],"\n"))
    cat(paste0("Samples per chain = ",mcmc_settings[1],":",mcmc_settings[3],"\n\n"))

    summary_obj <- JuliaCall::julia_eval(paste0("df"))

    cat("Summary Statistics:\n\n")
    (print(summary_obj[1:n_display,,drop=FALSE],digits = digits,row.names = FALSE))

    n_display_val <- min(n_display, nrow(summary_obj))


    if (nrow(summary_obj) > n_display_val) {
      cat(format("         ⋮\n", width = 14))
      cat(sprintf("  %d rows omitted\n", nrow(summary_obj) - 10))
    }

    if(get_summary){
      summary_list <- list(summary = summary_obj)
    } else {
      summary_list <- NULL
    }

    if(get_quantiles){
      JuliaCall::julia_eval(paste0("qt_df = DataFrame(MCMCChains.quantile(",object$sampler_name,").nt)"),need_return = "Julia")
      JuliaCall::julia_eval(paste0("qt_df.parameters = String.(qt_df.parameters)"),need_return = "Julia")
      quantile_obj <- JuliaCall::julia_eval("qt_df",need_return = "R")
      quantile_list <- list(quantiles = quantile_obj)
    } else {
      quantile_list <- NULL
    }

    return(append(summary_list,quantile_list))

  }

}
