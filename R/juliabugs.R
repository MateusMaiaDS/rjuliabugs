juliaBUGS <- function(data,
                      model,
                      n_iter = 2000,
                      n_burnin = floor(n_iter/2),
                      n_thin = 1){


  JuliaCall::julia_install_package_if_needed("JuliaBUGS")



}
