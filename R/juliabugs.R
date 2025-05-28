juliaBUGS <- function(data,
                      model,
                      n_iter = 2000,
                      n_burnin = floor(n_iter/2),
                      n_thin = 1){

  cat("Setting up Julia environment...")
  cat("\n")
  # Setting up the Julia Enviroment
  setup_juliaBUGS()

  # Converting the data object into a JuliaNamedTuple
  class(data) <- "JuliaNamedTuple"
  JuliaCall::julia_assign(x = "data", data)
  JuliaCall::julia_eval("model = compile(model,data)")


}
