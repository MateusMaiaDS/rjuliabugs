#' Summary Method for JuliaBUGS Sampler Output
#'
#' Provides a summary of the results from a JuliaBUGS sampler object, including MCMC settings,
#' summary statistics, and optionally quantiles.
#'
#' @param object An object of class `rjuliabugs` containing a reference to a Julia MCMC sampler.
#' @param ... Additional optional arguments. Supported options:
#' * `digits`: Integer. Number of significant digits to display. Default: 4.
#' * `n_display`: Integer. Number of rows of summary statistics to show. Default: 10.
#' * `get_summary`: Logical. If `TRUE`, returns summary statistics in the output list. Default: `FALSE`.
#' * `get_quantiles`: Logical. If `TRUE`, returns quantiles in the output list. Default: `FALSE`.
#' * `julia_summary_only`: Logical. If `TRUE`, displays the Julia summary directly and returns `NULL` invisibly. Default: `FALSE`.
#' @md
#'
#' @return If `julia_summary_only = TRUE`, returns `NULL` invisibly.
#' Otherwise, returns a list possibly containing:
#' * `summary`: Data frame of summary statistics (if `get_summary = TRUE`).
#' * `quantiles`: Data frame of quantiles (if `get_quantiles = TRUE`).
#'
#' @details
#' This method wraps Julia's `MCMCChains.summarystats` and `MCMCChains.quantile` to extract and display
#' results in R using the `JuliaCall` interface. It also extracts key MCMC settings like number of chains,
#' iterations, and samples per chain. The printed summary is truncated to `n_display` rows.
#'
#' @importFrom JuliaCall julia_eval
#' @method summary rjuliabugs
#' @export
#' @md
summary.rjuliabugs <- function(object, ...) {
  args <- list(...)

  digits <- if (!is.null(args$digits)) args$digits else 4
  n_display <- if (!is.null(args$n_display)) args$n_display else 10
  get_summary <- if (!is.null(args$get_summary)) args$get_summary else FALSE
  get_quantiles <- if (!is.null(args$get_quantiles)) {
    args$get_quantiles
  } else {
    FALSE
  }
  julia_summary_only <- if (!is.null(args$julia_summary_only)) {
    args$julia_summary_only
  } else {
    FALSE
  }

  if (julia_summary_only) {
    JuliaCall::julia_eval(
      paste0("display(", object$name, ")"),
      need_return = "Julia"
    )
    return(invisible(NULL))
  } else {
    JuliaCall::julia_eval("using DataFrames")

    JuliaCall::julia_eval(
      paste0("df = DataFrame(MCMCChains.summarystats(", object$name, ").nt)"),
      need_return = "Julia"
    )
    JuliaCall::julia_eval(
      "df.parameters = String.(df.parameters)",
      need_return = "Julia"
    )
    JuliaCall::julia_eval(
      "df.ess_bulk = Float64.(df.ess_bulk)",
      need_return = "Julia"
    )

    str_params_name <- unlist(JuliaCall::julia_eval(
      paste0("String.(keys(get_params(", object$name, ")))"),
      need_return = "R"
    ))
    str_params_name <- str_params_name[1:(which(str_params_name == "lp") - 1)]

    mcmc_settings <- break_string_to_numeric(as.character(JuliaCall::julia_eval(
      paste0("range(", object$name, ")"),
      need_return = "Julia"
    )))
    mcmc_n_chains <- break_string_to_numeric(as.character(JuliaCall::julia_eval(
      paste0("size(", object$name, ", 3)"),
      need_return = "Julia"
    )))

    cat("Summary of JuliaBUGS sampler:\n\n")
    cat(paste0("Iterations        = ", mcmc_settings[3], "\n"))
    cat(paste0("Number of chains  = ", mcmc_n_chains, "\n"))
    cat(paste0(
      "Number of posterior samples = ",
      floor((mcmc_settings[3] - mcmc_settings[1] + 1) / mcmc_settings[2]),
      "\n"
    ))
    cat(paste0("Thinning parameter = ", mcmc_settings[2], "\n"))
    cat(paste0(
      "Samples per chain = ",
      mcmc_settings[1],
      ":",
      mcmc_settings[3],
      "\n\n"
    ))

    summary_obj <- JuliaCall::julia_eval("df")

    cat("Summary Statistics:\n\n")
    print(
      summary_obj[1:n_display, , drop = FALSE],
      digits = digits,
      row.names = FALSE
    )

    n_display_val <- min(n_display, nrow(summary_obj))
    if (nrow(summary_obj) > n_display_val) {
      cat(format("         ...\n", width = 14)) # replaced ⋮ with ASCII
      cat(sprintf("  %d rows omitted\n", nrow(summary_obj) - n_display_val))
    }

    summary_list <- if (get_summary) list(summary = summary_obj) else NULL

    if (get_quantiles) {
      JuliaCall::julia_eval(
        paste0("qt_df = DataFrame(MCMCChains.quantile(", object$name, ").nt)"),
        need_return = "Julia"
      )
      JuliaCall::julia_eval(
        "qt_df.parameters = String.(qt_df.parameters)",
        need_return = "Julia"
      )
      quantile_obj <- JuliaCall::julia_eval("qt_df", need_return = "R")
      quantile_list <- list(quantiles = quantile_obj)
    } else {
      quantile_list <- NULL
    }

    if (is.null(summary_list) && is.null(quantile_list)) {
      return(invisible(NULL))
    } else {
      return(append(summary_list, quantile_list))
    }
  }
}
