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


#' Extract Parameter Samples from a Turing.jl MCMCChains Object (internal use only)
#'
#' This function extracts posterior samples for specified parameters from a Julia
#' `Chains` object (from the `Turing.jl` package) using `JuliaCall`.
#'
#' It supports extracting one or more parameters and returns the result as a numeric matrix in R.
#'
#' @param params A character vector of parameter names (as defined in the Julia model) to extract.
#' @param name A character string giving the name of the Julia object in the current Julia session,
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
#' @keywords internal

get_params_from_name_raw <- function(params, name) {
  if (!is.character(params)) {
    stop("`params` must be a character vector of parameter names.")
  }

  if (!is.character(name)) {
    stop("`name` must be the name of a Julia object (as a string).")
  }

  params_names <- paste0("[", paste0(paste0(":", params), collapse = ","), "]")

  JuliaCall::julia_eval(
    paste0("julia_params = get_params(", name, ")"),
    need_return = "Julia"
  )

  post_samples <- vector("list", length = length(params))

  for (i in 1:length(params)) {
    post_samples[[i]] <- JuliaCall::julia_eval(
      paste0("Float64.(julia_params.", params[i], ".data)"),
      need_return = "R"
    )
  }

  mcmc_n_chains <- as.numeric(
    (JuliaCall::julia_eval(
      paste0("size(", name, ", 3)"),
      need_return = "Julia"
    ))
  )

  if (mcmc_n_chains == 1) {
    post_samples <- do.call(cbind, post_samples)
    colnames(post_samples) <- params
  } else {
    post_samples_array <- array(
      dim = c(
        nrow(post_samples[[1]]),
        ncol(post_samples[[1]]),
        length(post_samples)
      )
    )
    for (i in 1:length(params)) {
      post_samples_array[,, i] <- post_samples[[i]]
    }

    dimnames(post_samples_array)[[3]] <- params

    post_samples <- post_samples_array
  }

  return(post_samples)
}

#' Wrap Bayesian Updating for Gibbs Sampling (BUGS) Model for Julia
#'
#' Wraps a Bayesian Updating for Gibbs Sampling (BUGS) model string with `model = @bugs begin` and `end`,
#' if it is not already wrapped. This is useful for preparing BUGS models
#' for use with Julia packages that expect this specific block structure.
#'
#' @param model_code A character string containing the body of a Bayesian Updating for Gibbs Sampling (BUGS) model.
#'   If the model already starts with `model = @bugs begin` and ends with `end`,
#'   the function returns it unchanged.
#'
#' @return A character string with the BUGS model wrapped properly in Julia-compatible syntax.
#'
#' @examples
#' \dontrun{
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
#'}
#'
#' @export
wrap_model_to_juliaBUGS <- function(model_code) {
  trimmed_code <- trimws(model_code)

  starts_correctly <- grepl("^model = @bugs begin", trimmed_code)
  ends_correctly <- grepl("end\\s*$", trimmed_code)

  if (starts_correctly && ends_correctly) {
    return(trimmed_code)
  } else {
    return(paste0('model = @bugs begin\n', trimmed_code, '\nend'))
  }
}


#' Convert Bayesian Updating for Gibbs Sampling (BUGS) Model to Julia's `@bugs` Macro Format
#'
#' This function formats a Bayesian Updating for Gibbs Sampling (BUGS) model string
#' into Julia's `@bugs("""...""", convert_var_name, true)` macro syntax, used to run
#' BUGS models in Julia. By default, R-style variable names (e.g., `a.b.c`) are converted
#' to Julia-style (`a_b_c`). You can disable this behavior by setting `convert_var_name = FALSE`.
#'
#' @param model_code A character string containing the BUGS model code.
#' @param convert_var_name Logical; if \code{TRUE} (default), R-style variable names
#'   (e.g., \code{a.b.c}) are converted to Julia-style (\code{a_b_c}). Set to \code{FALSE}
#'   to preserve the original names.
#'
#' @return
#' A character string representing a valid Julia `@bugs` macro call. This string can
#' be evaluated in Julia to define the BUGS model with optional variable name conversion.
#'
#' @examples
#' \dontrun{
#' model <- "
#'   for i in 1:N
#'     y[i] ~ dnorm(mu, tau)
#'   end
#'   mu ~ dnorm(0.0, 1.0E-6)
#'   tau ~ dgamma(0.001, 0.001)
#' "
#' bugs2juliaBUGS(model)
#' bugs2juliaBUGS(model, convert_var_name = FALSE)
#' }
#'
#' @export
bugs2juliaBUGS <- function(model_code, convert_var_name = TRUE) {
  trimmed_code <- trimws(model_code)
  julia_bool <- c("TRUE" = "true", "FALSE" = "false")
  not_has_model_block <- !grepl("model\\s*\\{", model_code)

  return(paste0(
    'model = @bugs("""\n',
    trimmed_code,
    '\n""", ',
    julia_bool[as.character(convert_var_name)],
    ', ',
    julia_bool[as.character(not_has_model_block)],
    ')'
  ))
}

#' Setup Julia Environment for JuliaBUGS
#'
#' Installs and loads the required Julia packages to use JuliaBUGS via JuliaCall in R.
#'
#' This function checks whether the core Julia packages needed for running \code{JuliaBUGS} are installed,
#' installs any missing ones, and loads them into the current Julia session.
#' Optionally, additional Julia packages can be installed and loaded by specifying them via \code{extra_packages}.
#'
#' @param extra_packages Character vector of additional Julia packages to install and load.
#'   Defaults to \code{NULL}, meaning only the core packages are handled.
#' @param verify_package Logical; if \code{TRUE}, verifies and installs missing core packages. Default is \code{TRUE}.
#' @param install_from_dev Logical; if \code{TRUE}, installs \code{JuliaBUGS} from its development repository. Default is \code{FALSE}.
#' @param verbose Logical. If `FALSE` will ommit any message from the function to indicate
#' the setup progress.
#' @param ... Additional arguments passed to \code{JuliaCall::julia_setup()}, such as \code{installJulia = TRUE}.
#'
#' @details
#' The core Julia packages installed (if needed) are:
#' * Serialization
#' * LogDensityProblemsAD
#' * ReverseDiff
#' * AdvancedHMC
#' * AbstractMCMC
#' * LogDensityProblems
#' * MCMCChains
#' * DataFrames
#' * JuliaBUGS
#'
#' After installation, all these packages are loaded in the Julia session using \code{using}.
#' Any additional packages provided via \code{extra_packages} are also installed and loaded.
#'
#' @return Invisibly returns \code{NULL}. The function is called for its side effects.
#'
#' @examples
#' \dontrun{
#' # Setup Julia with core packages only
#' setup_juliaBUGS()
#'
#' # Setup Julia with additional packages
#' setup_juliaBUGS(extra_packages = c("Distributions", "Turing"))
#' }
#'
#' @seealso \code{\link[JuliaCall]{julia_install_package_if_needed}}, \code{\link[JuliaCall]{julia_eval}}
#'
#' @md
#' @export
#' @md
setup_juliaBUGS <- function(
  extra_packages = NULL,
  verify_package = TRUE,
  install_from_dev = FALSE,
  verbose = TRUE,
  ...
) {

  if(verbose){
      cat("Preparing JuliaBUGS setup... ")
  }

  # Add special handling for Linux CI to avoid segfaults
  julia_args <- list(...)
  if (Sys.getenv("CI") == "true" && grepl("linux", R.version$os)) {
    julia_args$installJulia <- FALSE
    julia_args$rebuild <- FALSE
  }

  julia <- do.call(JuliaCall::julia_setup, julia_args)

  # Install all dependencies if needed
  if (verify_package) {
    JuliaCall::julia_install_package_if_needed("RCall")
    JuliaCall::julia_install_package_if_needed("Suppressor")
    JuliaCall::julia_install_package_if_needed("Serialization")
    JuliaCall::julia_install_package_if_needed("LogDensityProblemsAD")
    JuliaCall::julia_install_package_if_needed("ReverseDiff")
    JuliaCall::julia_install_package_if_needed("AdvancedHMC")
    JuliaCall::julia_install_package_if_needed("AbstractMCMC")
    JuliaCall::julia_install_package_if_needed("LogDensityProblems")
    JuliaCall::julia_install_package_if_needed("MCMCChains")
    JuliaCall::julia_install_package_if_needed("DataFrames")
  }
  if (install_from_dev && verify_package) {
    JuliaCall::julia_eval(
      'import Pkg; Pkg.add("JuliaBUGS")'
    )
  } else if (verify_package) {
    JuliaCall::julia_install_package_if_needed("JuliaBUGS")
  }

  # Loading those libraries
  JuliaCall::julia_eval(
    "using RCall, Suppressor, Serialization, LogDensityProblemsAD, ReverseDiff, AdvancedHMC, AbstractMCMC, LogDensityProblems, MCMCChains, DataFrames, JuliaBUGS"
  )

  if (!is.null(extra_packages)) {
    for (i in seq_along(extra_packages)) {
      JuliaCall::julia_install_package_if_needed(extra_packages)
      JuliaCall::julia_eval(paste0("using ", extra_packages[i]))
    }
  }
  if(verbose){
      cat("DONE!\n")
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
#' \dontrun{
#' input_list <- list(
#'   a = c(x = 1.0, y = 2.5, z = 3.0),
#'   b = c(foo = 4.0, bar = 5.1),
#'   c = c(6, 7, 8)
#' )
#' convert_numeric_types(input_list)
#'}
#' @export
#' @md
convert_numeric_types <- function(data) {
  result <- lapply(data, function(vec) {
    # Get existing names (can be NULL)
    nms <- names(vec)

    # Process each element
    converted <- sapply(
      vec,
      function(x) {
        if (is.na(x)) {
          return(NA)
        } else if (x %% 1 == 0) {
          return(as.integer(x))
        } else {
          return(as.numeric(x))
        }
      },
      USE.NAMES = FALSE
    )

    # Re-assign names if they exist
    if (!is.null(nms)) {
      names(converted) <- nms
    }

    return(converted)
  })

  return(result)
}

break_string_to_numeric <- function(input_string) {
  parts <- strsplit(input_string, ":")[[1]]
  numeric_parts <- as.numeric(parts)
  return(numeric_parts)
}

#' Delete an object from the Julia Main environment
#'
#' This function removes a variable or object from the Julia `Main` module using JuliaCall.
#' It is useful for cleaning up or resetting objects defined in the Julia environment from R.
#'
#' @param obj_name A character string specifying the name of the Julia object to be deleted.
#'   This should correspond to a variable or symbol previously defined in the Julia `Main` module.
#'
#'
#' @examples
#' \dontrun{
#' JuliaCall::julia_command("x = 10")  # Define a Julia variable
#' delete_julia_obj("x")               # Delete it
#' }
#'
#' @export
#' @return
#' No return value, called for side effects.
#'
#' @md
delete_julia_obj <- function(obj_name) {
  JuliaCall::julia_eval(paste0("Base.delete_binding(Main, :", obj_name, ")"))
  return(invisible(NULL))
}

#' Ensure Unique Name for Julia Sampler (Internal)
#'
#' Checks whether a Julia variable name is already defined in the `Main` module.
#' If the name exists, appends random characters until a unique name is found.
#'
#' @param name A character string representing a Julia object name.
#'
#' @return A unique name not currently defined in Julia.
#'
#' @keywords internal
#' @md
check_sampler_is_defined <- function(name) {
  if (!is.character(name) || length(name) != 1) {
    stop("`name` must be a single character string.")
  }

  sampler_is_defined <- JuliaCall::julia_eval(
    paste0("isdefined(Main, :", name, ")"),
    need_return = "R"
  )

  new_name <- FALSE

  while (isTRUE(sampler_is_defined)) {
    name_old <- name
    name <- paste0(name, "_", sample(letters, 1), sample(0:9, 1))
    sampler_is_defined <- JuliaCall::julia_eval(
      paste0("isdefined(Main, :", name, ")"),
      need_return = "R"
    )
    new_name <- TRUE
  }

  if (new_name) {
    warning(sprintf(
      "The object '%s' was already defined in the Julia environment. The name has been changed to '%s'.",
      name_old,
      name
    ))
  }

  return(name)
}

# ===== Conversion posterior class functions =============== #

#' @rdname as_rvar
#' @export
#' @importFrom posterior rvar
#' @return
#' An object of class \code{"rjuliabugs"} (a named list) with the following elements:
#' \describe{
#'   \item{params}{Posterior samples, converted to the requested format:
#'         \code{rvar} for \code{as_rvar},
#'         \code{mcmc}/\code{mcmc.list} for \code{as_mcmc},
#'         \code{draws_array} for \code{as_draws}.}
#'   \item{name}{The name of the Julia sampler object.}
#'   \item{sampler}{The sampler object returned by \code{AbstractMCMC.sample} in Julia.}
#'   \item{n_threads}{Number of Julia threads detected.}
#'   \item{mcmc}{A list of MCMC configuration parameters, now including \code{posterior_type}
#'         indicating the format of \code{params}.}
#'   \item{control}{Control options passed to and used by the sampler.}
#' }
#' @md

as_rvar.rjuliabugs <- function(x, ...) {
  n_chain <- x$mcmc$n_chain
  x$params <- posterior::rvar(x = x$params, nchains = n_chain)
  x$mcmc$posterior_type <- "rvar"
  return(x)
}

#' @rdname as_mcmc
#' @export
#' @importFrom coda as.mcmc as.mcmc.list mcmc
#' @md
#' @return
#' An object of class \code{"rjuliabugs"} (a named list) with the following elements:
#' \describe{
#'   \item{params}{Posterior samples converted to \code{coda::mcmc} if a single chain,
#'         or \code{coda::mcmc.list} if multiple chains.}
#'   \item{name}{The name of the Julia sampler object (unchanged).}
#'   \item{sampler}{The sampler object returned by \code{AbstractMCMC.sample} in Julia (unchanged).}
#'   \item{n_threads}{Number of Julia threads detected (unchanged).}
#'   \item{mcmc}{MCMC configuration parameters; \code{posterior_type} is updated to \code{"mcmc"}.}
#'   \item{control}{Control options passed to and used by the sampler (unchanged).}
#' }
as_mcmc.rjuliabugs <- function(x, ...) {
  n_chain <- x$mcmc$n_chain
  x$params <- if (n_chain == 1) {
    coda::as.mcmc(x$params)
  } else {
    coda::as.mcmc.list(
      lapply(seq(dim(x$params)[2]), function(i) {
        coda::mcmc(x$params[, i, ])
      })
    )
  }
  x$mcmc$posterior_type <- "mcmc"
  return(x)
}


#' @rdname as_draws
#' @export
#' @importFrom posterior as_draws
#' @md
#' @return
#' An object of class \code{"rjuliabugs"} (a named list) with the following elements:
#' \describe{
#'   \item{params}{Posterior samples converted to \code{posterior::draws_array}.}
#'   \item{name}{The name of the Julia sampler object (unchanged).}
#'   \item{sampler}{The sampler object returned by \code{AbstractMCMC.sample} in Julia (unchanged).}
#'   \item{n_threads}{Number of Julia threads detected (unchanged).}
#'   \item{mcmc}{MCMC configuration parameters; \code{posterior_type} is updated to \code{"draws"}.}
#'   \item{control}{Control options passed to and used by the sampler (unchanged).}
#' }
as_draws.rjuliabugs <- function(x, ...) {
  x$params <- posterior::as_draws(x$params)
  x$mcmc$posterior_type <- "draws"
  return(x)
}



#' @rdname as_rvar
#' @export
#' @md
#' @return
#' An object of class \code{rvar} (from the \pkg{posterior} package). The input 3D array
#' (iterations × chains × parameters) is converted into a posterior \code{rvar} object,
#' where each parameter is represented as a random variable across iterations and chains.
as_rvar.array <- function(x, n_mcmc = NULL, ...) {
  if (is.null(n_mcmc)) {
    stop("You must provide `n_mcmc` when using `as_rvar()` on an array.")
  }
  posterior::rvar(x = x, nchains = n_mcmc)
}


#' @rdname as_mcmc
#' @export
#' @md
#' @return
#' Returns posterior samples converted to \code{coda::mcmc} if the array has one chain,
#' or \code{coda::mcmc.list} if multiple chains. Input must be a 3D array
#' (iterations × chains × parameters). Each column corresponds to a parameter, and each row
#' corresponds to an iteration.
as_mcmc.array <- function(x, ...) {
  dims <- dim(x)
  if (length(dims) != 3) {
    stop("Input array must be 3D: iterations x chains x parameters")
  }

  n_chains <- dims[2]

  if (n_chains == 1) {
    coda::as.mcmc(x[, 1, ])
  } else {
    coda::as.mcmc.list(
      lapply(seq(n_chains), function(i) {
        coda::mcmc(x[, i, ])
      })
    )
  }
}


#' @rdname as_draws
#' @export
#' @md
#' @return
#' An object of class \code{draws_array} (from the \pkg{posterior} package). The 3D array
#' (iterations × chains × parameters) is converted to a \code{draws_array}, preserving the
#' chain structure and parameter names.
as_draws.array <- function(x, ...) {
  posterior::as_draws(x)
}


# ============================================================ #

#' Extract Posterior Samples from an `rjuliabugs` S3 Object
#'
#' Extracts posterior samples for specified parameters from a fitted `rjuliabugs` object. Output can be returned
#' in several formats depending on downstream analysis requirements.
#'
#' @param rjuliabugs An S3 object of class `rjuliabugs`, typically returned by a call to the `juliaBUGS()` function.
#' Must contain a `params` 3D array and `mcmc` list with fields `params_to_save` and `n_chain`.
#' @param pars Character vector of parameter names to extract. If `NULL`, defaults to `rjuliabugs$mcmc$params_to_save`.
#' @param type Character string indicating output type: one of `"array"` (default), `"rvar"`, `"mcmc"`, or `"draws"`.
#' @param include Logical; if `TRUE`, extract only `pars`; if `FALSE`, exclude `pars`.
#'
#' @return The posterior samples in the specified format:
#' \itemize{
#'   \item a 3D `array` [iterations × chains × parameters],
#'   \item a `posterior::rvar` object,
#'   \item a `coda::mcmc` or `coda::mcmc.list`,
#'   \item or a `posterior::draws_array`/`draws_list`.
#' }
#'
#' @importFrom posterior rvar as_draws
#' @importFrom coda mcmc as.mcmc as.mcmc.list
#' @export
extract <- function(rjuliabugs, pars = NULL, type = "array", include = TRUE) {
  if (is.null(pars)) {
    pars <- rjuliabugs$mcmc$params_to_save
  }

  post_samples <- if (include) {
    rjuliabugs$params[,,
      pars %in% dimnames(rjuliabugs$params)[[3]],
      drop = FALSE
    ]
  } else {
    rjuliabugs$params[,,
      !pars %in% dimnames(rjuliabugs$params)[[3]],
      drop = FALSE
    ]
  }

  # Getting n_chain
  n_chain <- rjuliabugs$mcmc$n_chain

  if (type == "array") {
    post_samples <- as.array(post_samples)
  } else if (type == "rvar") {
    post_samples <- posterior::rvar(x = post_samples, nchains = n_chain)
  } else if (type == "mcmc") {
    post_samples <- if (rjuliabugs$mcmc$n_chain == 1) {
      coda::as.mcmc(post_samples)
    } else {
      coda::as.mcmc.list(lapply(seq(dim(post_samples)[2]), function(x) {
        coda::mcmc(post_samples[, x, ])
      }))
    }
  } else if (type == "draws") {
    post_samples <- posterior::as_draws(post_samples)
  } else {
    stop("No valid type.")
  }

  return(post_samples)
}
