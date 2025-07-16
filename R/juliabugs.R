#' Run a Julia HMC Sampler for a BUGS-like Probabilistic Model
#'
#' Executes a Hamiltonian Monte Carlo (HMC) sampler in Julia from R, using a model
#' specified in Julia or in BUGS syntax. It compiles the model, converts data,
#' sets sampler parameters, and returns posterior samples in various formats. The setup
#' for the HMC sampler uses Not-U-Turn Sampler (NUTS) with the target acceptance probability
#' (\eqn{\delta}=0.8) for step size adaptation.
#'
#' @param data A named list of numeric values (integer or double). All elements must be named.
#' @param model A character string with the model definition, either in Julia-compatible format or BUGS syntax.
#' @param params_to_save Character vector with the names of model parameters to extract from the sampler output.
#' @param name Character. Name for the sampler object created in Julia (must be a valid Julia variable name).
#' @param n_iter Integer. Total number of MCMC iterations. Default is 2000.
#' @param n_warmup Integer. Number of iterations used warm-up or tuning (e.g., adaption steps in NUTS). Default is `floor(n_iter / 2)`.
#' @param n_discard Integer. Number of initial samples to be completely discarded. Default is `n_warmup`, i.e: discard all the iterations used as adaptation steps.
#' @param n_thin Integer. Thinning interval. Default is 1 (no thinning).
#' @param n_chain Integer. Number of MCMC chains. Default is 1.
#' @param use_parallel Logical. Whether to use `AbstractMCMC.MCMCThreads()` for parallel sampling. Default is `TRUE`.
#' @param posterior_type Character. Format of the posterior samples. One of `"array"`, `"rvar"`, `"mcmc"`, or `"draws"`. Default is `"array"`.
#' @param force_setup_juliaBUGS Logical. If `TRUE`, forces reinitialization of the Julia environment via `setup_juliaBUGS()`. Default is `FALSE`.
#' @param control Optional list of control parameters. Supported entries:
#'   \describe{
#'     \item{`data_convert_int`}{Logical. If `TRUE`, coerces numeric values to integers when possible. Default is `TRUE`.}
#'     \item{`convert_var_name`}{Logical. If `TRUE`, automatically renames variables in the BUGS model. Default is `FALSE`.}
#'     \item{`julia_model`}{Logical. If `TRUE`, assumes the model is already in Julia format. Default is `FALSE`.}
#'   }
#' @param ... Additional arguments passed to `setup_juliaBUGS()`.
#'
#' @return An object of class `"rjuliabugs"` containing:
#' \describe{
#'   \item{params}{Posterior samples, in the format specified by `posterior_type`.}
#'   \item{name}{Character string identifying the Julia sampler object.}
#'   \item{sampler}{Sampler object returned by `AbstractMCMC.sample`.}
#'   \item{n_threads}{Number of Julia threads detected.}
#'   \item{mcmc}{List of MCMC configuration parameters.}
#'   \item{control}{Control options used in the sampler setup.}
#' }
#'
#' @details
#' This function relies on Julia packages `LogDensityProblems`, `AdvancedHMC`, and `AbstractMCMC`.
#' Gradients are computed via `ReverseDiff`. The model is compiled before sampling.
#'
#' The `posterior_type` argument determines the return format:
#' \itemize{
#'   \item `"array"`: 3D numeric array (iterations × chains × parameters).
#'   \item `"rvar"`: `posterior::rvar` object.
#'   \item `"mcmc"`: `coda::mcmc` (single chain) or `mcmc.list` (multiple chains).
#'   \item `"draws"`: `posterior::draws_array`.
#' }
#'
#' @note
#' You must call `setup_juliaBUGS()` at least once before using this function.
#' If parallel sampling is requested but only one Julia thread is available,
#' a warning is issued and sampling will run serially.
#'
#' @importFrom JuliaCall julia_call julia_eval julia_assign
#' @importFrom coda as.mcmc as.mcmc.list
#' @importFrom posterior rvar as_draws
#'
#' @examples
#' \dontrun{
#' model <- "model = @model ... end"
#' data <- list(N = 10, x = rnorm(10))
#' result <- juliaBUGS(
#'   data = data,
#'   model = model,
#'   params_to_save = c("mu"),
#'   name = "my_sampler"
#' )
#' }
#'
#' @export
juliaBUGS <- function(data,
                      model,
                      params_to_save,
                      name = "sampler_juliaBUGS",
                      n_iter = 2000,
                      n_warmup= floor(n_iter/2),
                      n_discard = n_warmup,
                      n_thin = 1,
                      n_chain = 1,
                      use_parallel = TRUE,
                      posterior_type = "array",
                      force_setup_juliaBUGS = FALSE,
                      control = NULL,
                      ...){

  # Checking if the name is defined, and generating a new one if is not the case
  name <- check_sampler_is_defined(name = name)

  if(!(posterior_type %in% c("array","rvar","mcmc","draws"))){
    stop("Insert a valid posterior_type. The available options are: 'array','rvar','mcmc' and 'draws'.")
  }


  if(!is.character(name)){
    stop("Insert a valid name.")
  }

  # Formatting the BUGS code to be used by juliaBUGS.jl
  if(!is.null(control$julia_model) && control$julia_model!=FALSE ){
    model <- wrap_model_to_juliaBUGS(model_code = model)
  } else {

    ## Setting the default to convert_var_name as FALSE
    if(is.null(control$convert_var_name)){
      convert_var_name <- FALSE
    }

    model <- bugs2juliaBUGS(model_code = model,convert_var_name = convert_var_name)

  }

  # Verify over the data obj
  if(!is.list(data)){
    stop("Invalid data object. Data must be a named list")
  }

  # Verify over the data obj
  if(is.null(names(data))){
    stop("No names in the data object. The data object must be a named list")
  }

  # Checking all params to save are in the model
  check_params_to_save <- sapply(params_to_save,function(x){grepl(pattern = x,x = model)})

  if(!all(check_params_to_save)){
    stop(paste0("The params ",paste0(names(check_params_to_save)[!check_params_to_save],collapse = ", ")," are not part of the model."))
  }

  # Checking the number of threads() in Julia are correctly specified
  n_threads <- JuliaCall::julia_call("Threads.nthreads",need_return = "R")


  if(n_threads==1){
    warning("Number of threads identified in Julia enviroment is equal to one and AbstractMCMC.sample will be run serially not in parallel. To a correct specification of the number of threads see PUT LINK HERE for a complete documentation.\n")
    use_parallel <- FALSE
  }

  # Setting up the Julia Environment
  setup_juliaBUGS(...)

  # Setting default configuration for control parameters
  if(is.null(control)){
    data_convert_int <- TRUE
  } else {
    data_convert_int <- if(is.null(control$data_convert_int)) TRUE else control$data_convert_int
  }

  if(data_convert_int){
    data <- convert_numeric_types(data = data)
  }

  # Converting the data object into a JuliaNamedTuple
  class(data) <- "JuliaNamedTuple"
  JuliaCall::julia_assign(x = "data", data)
  JuliaCall::julia_eval("model = compile(model,data)")

  JuliaCall::julia_eval("ad_model = ADgradient(:ReverseDiff, model)")
  JuliaCall::julia_eval("D = LogDensityProblems.dimension(model)")
  JuliaCall::julia_eval("initial = rand(D)")


  # Conver MCMC parameters to int for julia.
  julia_assign_int("n_iter", as.integer(n_iter))
  julia_assign_int("n_warmup", as.integer(n_warmup))
  julia_assign_int("n_thin", as.integer(n_thin))
  julia_assign_int("n_discard", as.integer(n_discard))
  julia_assign_int("n_chain", as.integer(n_chain))

  # Defining which type of computational scheme will be used
  parallel_scheme <- ifelse(use_parallel,"AbstractMCMC.MCMCThreads()","AbstractMCMC.MCMCSerial()")

  cat("Initialising AbstractMCMC.sample()...")

  JuliaCall::julia_eval(paste0(name," = AbstractMCMC.sample(ad_model,
                                                                     AdvancedHMC.NUTS(0.8),
                                                                     ",parallel_scheme,",
                                                                     n_iter,
                                                                     n_chain;
                                                                     chain_type = Chains,
                                                                     n_adapts = n_warmup,
                                                                     init_params = initial,
                                                                     discard_initial = n_discard,
                                                                     thinning = n_thin)"))
  cat(" DONE!\n")


  params_raw <- if(!is.null(params_to_save)){
    get_params_from_name(name = name,
               params = params_to_save)
  }

  # Converting the posterior type
  if(posterior_type == "array"){

    params <- params_raw

  } else if (posterior_type == "rvar") {

    params <- posterior::rvar(x = params_raw,nchains = n_chain)

  } else if (posterior_type == "mcmc"){

    params <- if(n_chain == 1) {
      coda::as.mcmc(params_raw)
    } else {
      coda::as.mcmc.list(lapply(seq(dim(params_raw)[2]),function(x){coda::mcmc(params_raw[,x,])}))
    }

  } else if(posterior_type == "draws"){

    params <- posterior::as_draws(params_raw)

  } else {

    stop("Insert a valid posterior_type.")

  }

  ## Creating rjuliabugs obj
  rjuliabugs <- list(params = params,
                     name = name,
                     sampler = JuliaCall::julia_eval(name,need_return = "R"),
                     n_threads = n_threads,
                     mcmc = list(n_iter = 2000,
                                 n_warmup= floor(n_iter/2),
                                 n_discard = n_warmup,
                                 n_thin = 1,
                                 n_chain = 1,
                                 use_parallel = TRUE,
                                 posterior_type = "array"),
                     control = control)


  class(rjuliabugs) <- "rjuliabugs"

  return(rjuliabugs)

}
