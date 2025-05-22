#' Assign an Integer Value to a Julia Variable
#'
#' This function wraps `JuliaCall::julia_assign` to assign a value from R to a Julia variable,
#' and then explicitly casts the variable to the `Integer` type in Julia.
#'
#' @param x A character string. The name of the Julia variable to assign the value to.
#' @param value The R object to assign to the Julia variable. This will be passed to Julia.
#'
#' @details
#' The function first assigns the value from R to a variable named `x` in the Julia session
#' using `JuliaCall::julia_assign`. It then forces Julia to cast the variable to the
#' `Integer` type using Julia's `Integer()` constructor.
#'
#' Note: This function assumes that the `value` provided is compatible with Julia's `Integer` type.
#' If it is not, an error will be thrown by Julia.
#'
#' @return Invisibly returns `NULL`. The main effect is side-effects in the Julia session.
#'
#' @examples
#' \dontrun{
#' julia_assign_int("x", 3.5)  # Will be cast to 3L in Julia
#' JuliaCall::julia_eval("x")  # Returns 3 (as Integer in Julia)
#' }
#'
#' @export
julia_assign_int <- function(x, value) {
  JuliaCall::julia_assign(x = x, value = value)
  JuliaCall::julia_eval(paste0(x, " = Integer.(", x, ")"))
}


#' Extract Parameter Samples from a Turing.jl MCMCChains Object
#'
#' This function extracts posterior samples for specified parameters from a Julia
#' `Chains` object (from the `Turing.jl` package) using `JuliaCall`.
#'
#' It supports extracting one or more parameters and returns the result as a numeric matrix in R.
#'
#' @param params A character vector of parameter names (as defined in the Julia model) to extract.
#' @param julia_sampler A character string giving the name of the Julia object in the current Julia session,
#'   which must be a `Chains` object (from the `MCMCChains.jl` package).
#'
#' @details
#' This function builds a Julia expression of the form `Array(chains[:, [:param1, :param2], :])`
#' to extract values for the specified parameters from the sampler object. The result is evaluated
#' in Julia and returned to R as a numeric matrix.
#'
#' The function reshapes the output if only one parameter is extracted to ensure a consistent matrix format.
#'
#' @return A numeric matrix where rows represent samples and columns represent parameters.
#'
#' @examples
#' \dontrun{
#' JuliaCall::julia_setup()
#' JuliaCall::julia_library("Turing")
#' JuliaCall::julia_command("... define model and sample into 'chains' ...")
#' samples <- get_params(c("alpha", "beta"), "chains")
#' head(samples)
#' }
#'
#' @export
get_params <- function(params, julia_sampler) {
  if (!is.character(params)) {
    stop("`params` must be a character vector of parameter names.")
  }

  if (!is.character(julia_sampler)) {
    stop("`julia_sampler` must be the name of a Julia object (as a string).")
  }

  params_names <- paste0("[", paste0(paste0(":", params), collapse = ","), "]")
  params_values <- JuliaCall::julia_eval(
    paste0("Array(", julia_sampler, "[:,", params_names, ",:])"),
    need_return = "R"
  )

  if (length(dim(params_values)) == 0) {
    param_values <- matrix(params_values, ncol = 1)
    colnames(param_values) <- params
  } else if (length(dim(params_values)) == 2) {
    param_values <- params_values
    colnames(param_values) <- params
  } else {
    param_values <- params_values
  }

  return(param_values)
}

