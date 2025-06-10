#' Run a Julia MCMC Sampler from R using a Custom BUGS-like Model
#'
#' This function sets up and runs a Hamiltonian Monte Carlo (HMC) sampler from Julia using a user-defined probabilistic model.
#' It handles data validation, automatic type conversion for Julia, and allows parameter selection for posterior sampling.
#'
#' @param data A named list of data to be passed to the model. Each element must be numeric or integer. Names are required.
#' @param model A character string representing the Julia model definition. Must be a valid expression in Julia.
#' @param params_to_save Names of model parameters to extract from the Julia sampler. Must match identifiers in the `model` string.
#' if NULL no parameters will be returned.
#' @param n_iter Integer. Number of total MCMC iterations. Default is 2000.
#' @param n_warmup Integer. Number of warm-up (adaptation) iterations. Default is `floor(n_iter / 2)`.
#' @param n_discard Integer. Number of initial samples to discard. Default is `n_warmup`.
#' @param n_thin Integer. Thinning interval for saving samples. Default is 1 (no thinning).
#' @param control Optional list of control parameters. Currently supports:
#'   - `data_convert_int`: Logical. Whether to attempt converting numeric values to integers if they are whole numbers. Default is `TRUE`.
#'
#' @return An object of class `"rjuliabugs"` containing:
#' \describe{
#'   \item{params}{A named list of posterior samples for the selected parameters.}
#'   \item{sampler}{The full Julia `Chains` object with all samples.}
#' }
#'
#' @details
#' This function sets up and compiles the model in Julia, uses ReverseDiff for gradient calculations,
#' and samples using `AdvancedHMC.NUTS`. It assumes the Julia environment has already been initialized with
#' the necessary packages (`LogDensityProblems`, `AdvancedHMC`, `AbstractMCMC`, etc.).
#'
#' @note
#' You must call `setup_juliaBUGS()` before using this function to initialize the Julia environment
#' and load dependencies. This function assumes that the Julia environment and model are correctly configured.
#'
#' @examples
#' \dontrun{
#' model <- "model = @model ... end"
#' data <- list(N = 10, x = rnorm(10))
#' result <- juliaBUGS(data = data, model = model, params_to_save = c("mu"))
#' }
#'
#' @export
juliaBUGS <- function(data,
                      model,
                      params_to_save,
                      n_iter = 2000,
                      n_warmup= floor(n_iter/2),
                      n_discard = n_warmup,
                      n_thin = 1,
                      n_chain = 1,
                      sampler_name = NULL,
                      control = NULL){

  # Defining the sampler name
  if(is.null(sampler_name)){
    sampler_name <- "sampler_juliaBUGS"
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

  # Setting up the Julia Enviroment
  setup_juliaBUGS()

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

  JuliaCall::julia_eval("ad_model = ADgradient(:ReverseDiff, model; compile=Val(true))")
  JuliaCall::julia_eval("D = LogDensityProblems.dimension(model)")
  JuliaCall::julia_eval("initial = rand(D)")


  # Conver MCMC parameters to int for julia.
  julia_assign_int("n_iter", as.integer(n_iter))
  julia_assign_int("n_warmup", as.integer(n_warmup))
  julia_assign_int("n_thin", as.integer(n_thin))
  julia_assign_int("n_discard", as.integer(n_discard))
  julia_assign_int("n_chain", as.integer(n_chain))



  print(JuliaCall::julia_eval(paste0(sampler_name," = AbstractMCMC.sample(ad_model,
                                                                         NUTS(0.8),
                                                                         AbstractMCMC.MCMCSerial(),
                                                                         n_iter,
                                                                         n_chain;
                                                                         chain_type = Chains,n_adapts = n_warmup,discard_initial = n_discard,thinning = n_thin)")))

  params <- if(!is.null(params_to_save)){
    get_params(params = params_to_save,
               julia_sampler = sampler_name)
  }

  rjuliabugs <- list(params = params,
                     sampler_name = sampler_name,
                     sampler = JuliaCall::julia_eval(sampler_name,need_return = "R"))

  class(rjuliabugs) <- "rjuliabugs"

  return(rjuliabugs)

}
