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

  JuliaCall::julia_eval("julia_params = get_params(sampler_juliaBUGS)",need_return = "Julia")

  post_samples <- vector("list",length = length(params))

  for(i in 1:length(params)){
     post_samples[[i]] <- JuliaCall::julia_eval(paste0("Float64.(julia_params.",params[i],".data)"),
                                                need_return = "R")
  }

  post_samples <- do.call(cbind,post_samples)

  colnames(post_samples) <- params

  return(post_samples)
}


#' Wrap BUGS Model for Julia
#'
#' Wraps a BUGS model string with `model = @bugs begin` and `end`,
#' if it is not already wrapped. This is useful for preparing BUGS models
#' for use with Julia packages that expect this specific block structure.
#'
#' @param model_code A character string containing the body of a BUGS model.
#'   If the model already starts with `model = @bugs begin` and ends with `end`,
#'   the function returns it unchanged.
#'
#' @return A character string with the BUGS model wrapped properly in Julia-compatible syntax.
#'
#' @examples
#' model_body <- "
#'   for i in 1:N
#'     r[i] ~ dbin(p[i], n[i])
#'     b[i] ~ dnorm(0.0, tau)
#'     p[i] = logistic(alpha0 + alpha1 * x1[i] + alpha2 * x2[i] + alpha12 * x1[i] * x2[i] + b[i])
#'   end
#'   alpha0 ~ dnorm(0.0, 1.0E-6)
#'   tau ~ dgamma(0.001, 0.001)
#'   sigma = 1 / sqrt(tau)
#' "
#' wrap_model_to_juliaBUGS(model_body)
#'
#' @export
wrap_model_to_juliaBUGS <- function(model_code){

  trimmed_code <- trimws(model_code)

  starts_correctly <- grepl("^model = @bugs begin", trimmed_code)
  ends_correctly <- grepl("end\\s*$", trimmed_code)

  if(starts_correctly && ends_correctly){
    return(trimmed_code)
  } else {
    return(paste0('model = @bugs begin\n',
                  trimmed_code,
                  '\nend'))
  }
}


#' Convert BUGS Model to Julia's `@bugs` Macro Format
#'
#' Formats a BUGS model string to Julia's `@bugs("""...""", convert_var_name, true)` syntax,
#' used in the Julia ecosystem for running BUGS models. By default, this macro converts
#' R-style variable names (e.g., `a.b.c`) to Julia-style (`a_b_c`). You can disable this behavior
#' by setting `convert_var_name = FALSE`.
#'
#' @param model_code A character string containing the BUGS model.
#' @param convert_var_name Logical; if `TRUE` (default), R-style variable names such as `a.b.c`
#'   are translated to `a_b_c` in the Julia model. Set to `FALSE` to preserve original names.
#'
#' @return A character string formatted as a Julia `@bugs` macro call.
#'
#' @examples
#' model <- "
#'   for i in 1:N
#'     y[i] ~ dnorm(mu, tau)
#'   end
#'   mu ~ dnorm(0.0, 1.0E-6)
#'   tau ~ dgamma(0.001, 0.001)
#' "
#' bugs2juliaBUGS(model)
#' bugs2juliaBUGS(model, convert_var_name = FALSE)
#'
#' @export
bugs2juliaBUGS <- function(model_code,
                           convert_var_name = TRUE) {

  trimmed_code <- trimws(model_code)
  julia_bool <- c("TRUE" = "true", "FALSE" = "false")

  return(paste0(
    'model = @bugs("""\n',
    trimmed_code,
    '\n""", ',
    julia_bool[as.character(convert_var_name)],
    ', true)'
  ))
}

#' Setup Julia Environment for JuliaBUGS
#'
#' Installs and loads required Julia packages for using JuliaBUGS with R via JuliaCall.
#'
#' This function checks for the required Julia packages for running `JuliaBUGS` and installs them if they are not already installed.
#' It also loads these packages into the current Julia session. Optionally, users can provide additional Julia packages to install and load.
#'
#' @param extra_packages A character vector of additional Julia packages to install and load.
#'   Default is `NULL`, which means only the core packages needed for JuliaBUGS will be handled.
#'
#' @details
#' The function uses `JuliaCall::julia_install_package_if_needed()` to install core Julia packages:
#' \itemize{
#'   \item LogDensityProblemsAD
#'   \item ReverseDiff
#'   \item AdvancedHMC
#'   \item AbstractMCMC
#'   \item LogDensityProblems
#'   \item MCMCChains
#'   \item JuliaBUGS
#' }
#' After installation, these packages are loaded in the Julia session with the `using` statement.
#' Any additional packages specified in `extra_packages` will also be installed and loaded.
#'
#' @examples
#' \dontrun{
#' # Setup Julia with default required packages
#' setup_juliaBUGS()
#'
#' # Setup Julia with additional packages
#' setup_juliaBUGS(extra_packages = c("Distributions", "Turing"))
#' }
#'
#' @return This function is called for its side effects (installing and loading Julia packages). It returns `NULL` invisibly.
#'
#' @seealso [JuliaCall::julia_install_package_if_needed()], [JuliaCall::julia_eval()]
#'
#' @export
setup_juliaBUGS <- function(extra_packages = NULL,
                            verify_package = TRUE,
                            install_from_dev = FALSE){

  cat("Preparing JuliaBUGS setup... \n")
  julia <- JuliaCall::julia_setup(installJulia=TRUE)

  # Install all dependencies if needed
  if(verify_package){
    JuliaCall::julia_install_package_if_needed("LogDensityProblemsAD")
    JuliaCall::julia_install_package_if_needed("ReverseDiff")
    JuliaCall::julia_install_package_if_needed("AdvancedHMC")
    JuliaCall::julia_install_package_if_needed("AbstractMCMC")
    JuliaCall::julia_install_package_if_needed("LogDensityProblems")
    JuliaCall::julia_install_package_if_needed("MCMCChains")
  }
  if(install_from_dev && verify_package){
    JuliaCall::julia_eval('Pkg.add(url="https://github.com/TuringLang/JuliaBUGS.jl.git")')
  } else if (verify_package) {
    JuliaCall::julia_install_package_if_needed("JuliaBUGS")
  }

  # Loading those libraries
  JuliaCall::julia_eval("using LogDensityProblemsAD, ReverseDiff, AdvancedHMC, AbstractMCMC, LogDensityProblems, MCMCChains, JuliaBUGS")

  if (!is.null(extra_packages)) {
    for (i in seq_along(extra_packages)) {
      JuliaCall::julia_install_package_if_needed(extra_packages)
      JuliaCall::julia_eval(paste0("using ", extra_packages[i]))
    }
  }
}

#' Convert Numeric Elements in a List to Integer or Float
#'
#' This function takes a list of numeric vectors and returns a new list
#' where each numeric element is automatically converted to either an
#' integer (if it is a whole number) or kept as a float (numeric).
#' It also preserves the original names of vector elements, if any.
#'
#' @param data A list of numeric vectors.
#'
#' @return A list of the same structure where each numeric element is coerced to the appropriate type:
#' integers for whole numbers, and floats otherwise. Names are preserved.
#'
#' @examples
#' input_list <- list(
#'   a = c(x = 1.0, y = 2.5, z = 3.0),
#'   b = c(foo = 4.0, bar = 5.1),
#'   c = c(6, 7, 8)
#' )
#' convert_numeric_types(input_list)
#'
#' @export
convert_numeric_types <- function(data) {
  result <- lapply(data, function(vec) {

    # Get existing names (can be NULL)
    nms <- names(vec)

    # Process each element
    converted <- sapply(vec, function(x) {
      if (is.na(x)) {
        return(NA)
      } else if (x %% 1 == 0) {
        return(as.integer(x))
      } else {
        return(as.numeric(x))
      }
    }, USE.NAMES = FALSE)

    # Re-assign names if they exist
    if (!is.null(nms)) names(converted) <- nms

    return(converted)
  })

  return(result)
}


