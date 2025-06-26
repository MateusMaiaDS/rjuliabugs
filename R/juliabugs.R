#' Run a Julia MCMC Sampler from R using a Custom BUGS-like Model
#'
#' This function sets up and runs a Hamiltonian Monte Carlo (HMC) sampler in Julia using a user-defined probabilistic model.
#' It manages model compilation, data conversion, parallelization settings, and various posterior output formats.
#'
#' @param data A named list containing the data to be passed to the model. All elements must be numeric (integer or double). Names are required.
#' @param model A character string with the model definition in a Julia-compatible format or in BUGS syntax to be converted.
#' @param params_to_save Character vector with the names of model parameters to extract from the sampler output. If `NULL`, no posterior samples will be returned.
#' @param sampler_name Character. A name for the sampler object to be created within Julia.
#' @param n_iter Integer. Total number of MCMC iterations. Default is 2000.
#' @param n_warmup Integer. Number of warm-up (adaptation) iterations. Default is `floor(n_iter / 2)`.
#' @param n_discard Integer. Number of initial samples to discard. Default is `n_warmup`.
#' @param n_thin Integer. Thinning interval for retained samples. Default is 1 (no thinning).
#' @param n_chain Integer. Number of MCMC chains to run. Default is 1.
#' @param use_parallel Logical. Whether to use multi-threaded sampling with `AbstractMCMC.MCMCThreads()`. Default is `TRUE`.
#' @param posterior_type Character. Format of the posterior returned. One of `"array"`, `"rvar"`, `"mcmc"`, or `"draws"`. Default is `"array"`.
#' @param control Optional list of control options. Supported entries:
#'   \describe{
#'     \item{`data_convert_int`}{Logical. Whether to coerce numeric values in `data` to integers when possible. Default is `TRUE`.}
#'     \item{`convert_var_name`}{Logical. Whether to rename variables in the model code. Default is `FALSE`.}
#'     \item{`julia_model`}{Logical. If `TRUE`, skips BUGS-to-Julia translation. Default is `FALSE`.}
#'   }
#' @param ... Additional arguments passed to `setup_juliaBUGS()`.
#'
#' @return An object of class `"rjuliabugs"` containing:
#' \describe{
#'   \item{params}{Posterior samples for the selected parameters, in the format specified by `posterior_type`.}
#'   \item{sampler_name}{Character string identifying the sampler object created in Julia.}
#'   \item{sampler}{The full Julia sampler object as returned by `AbstractMCMC.sample`.}
#'   \item{n_threads}{Number of Julia threads detected during execution.}
#' }
#'
#' @details
#' Internally, this function uses `LogDensityProblems`, `AdvancedHMC`, and `AbstractMCMC` from Julia.
#' Gradient calculations are performed with `ReverseDiff`, and data is passed using a `JuliaNamedTuple`.
#' The model code is compiled using `compile(...)` before sampling.
#'
#' The `posterior_type` argument determines the format of the returned posterior:
#' - `"array"`: a 3D numeric array (samples × chains × parameters).
#' - `"rvar"`: a `posterior::rvar` object.
#' - `"mcmc"`: a `coda::mcmc` or `coda::mcmc.list`.
#' - `"draws"`: a `posterior::draws_array` object.
#'
#' @note
#' You must run `setup_juliaBUGS()` before using this function to initialize Julia and load the required packages.
#' Ensure the Julia environment has all necessary dependencies installed.
#' If parallel sampling is requested but only one Julia thread is available, a warning will be issued and serial sampling will be used instead.
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
#'   sampler_name = "my_sampler"
#' )
#' }
#'
#' @export
juliaBUGS <- function(data,
                      model,
                      params_to_save,
                      sampler_name,
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

  # Verifying if the sampler name already exist
  sampler_is_defined <- JuliaCall::julia_eval(paste0("isdefined(Main,:",sampler_name,")"),need_return = "R")
  print(sampler_is_defined)

  if(sampler_is_defined){
    warning(paste0("The object '", sampler_name, "' was already defined in the Julia environment and has been overwritten."))
  }

  if(!(posterior_type %in% c("array","rvar","mcmc","draws"))){
    stop("Insert a valid posterior_type. The available options are: 'array','rvar','mcmc' and 'draws'.")
  }


  if(!is.character(sampler_name)){
    stop("Insert a valid sampler_name.")
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
  JuliaCall::julia_eval(model)
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

  JuliaCall::julia_eval(paste0(sampler_name," = AbstractMCMC.sample(ad_model,
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
    get_params(params = params_to_save,
               sampler_name = sampler_name)
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


  rjuliabugs <- list(params = params,
                     sampler_name = sampler_name,
                     sampler = JuliaCall::julia_eval(sampler_name,need_return = "R"),
                     n_threads = n_threads)

  class(rjuliabugs) <- "rjuliabugs"

  return(rjuliabugs)

}
