rm(list=ls())
devtools::load_all()
JuliaCall::julia_install_package_if_needed("JuliaBUGS")
JuliaCall::julia_eval("using JuliaBUGS")
JuliaCall::julia_eval("

model = @bugs begin
  for i in 1:N
    r[i] ~ dbin(p[i], n[i])
    b[i] ~ dnorm(0.0, tau)
    p[i] = logistic(alpha0 + alpha1 * x1[i] + alpha2 * x2[i] + alpha12 * x1[i] * x2[i] + b[i])
  end
  alpha0 ~ dnorm(0.0, 1.0E-6)
  alpha1 ~ dnorm(0.0, 1.0E-6)
  alpha2 ~ dnorm(0.0, 1.0E-6)
  alpha12 ~ dnorm(0.0, 1.0E-6)
  tau ~ dgamma(0.001, 0.001)
  sigma = 1 / sqrt(tau)
end
")


data <- list(
  r = c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46, 10, 8, 10, 8, 23, 0, 3, 22, 15, 32, 3),
  n = c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79, 13, 16, 30, 28, 45, 4, 12, 41, 30, 51, 7),
  x1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  x2 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
  N = 21
)


JuliaCall::julia_install_package_if_needed("LogDensityProblemsAD")
JuliaCall::julia_install_package_if_needed("ReverseDiff")
JuliaCall::julia_install_package_if_needed("AdvancedHMC")
JuliaCall::julia_install_package_if_needed("AbstractMCMC")
JuliaCall::julia_install_package_if_needed("LogDensityProblems")
JuliaCall::julia_install_package_if_needed("MCMCChains")

JuliaCall::julia_eval("using LogDensityProblemsAD, ReverseDiff, AdvancedHMC, AbstractMCMC, LogDensityProblems, MCMCChains")

class(data) <- "JuliaNamedTuple"
JuliaCall::julia_assign(x = "data", data)
JuliaCall::julia_eval("model = compile(model,data)")

JuliaCall::julia_eval("ad_model = ADgradient(:ReverseDiff, model; compile=Val(true))")
JuliaCall::julia_eval("D = LogDensityProblems.dimension(model)")
JuliaCall::julia_eval("initial = rand(D)")

n_samples <- as.integer(2000)
n_adapts <- as.integer(1000)
n_thinning <- 1
n_chains <- 1
julia_assign_int("n_samples", n_adapts)
julia_assign_int("n_adapts", n_adapts)
julia_assign_int("n_thinning", n_thinning)
julia_assign_int("n_chains", n_chains)

# JuliaCall::julia_eval("n_samples = Integer(n_samples)")
# JuliaCall::julia_eval("n_adapts = Integer(n_adapts)")
# JuliaCall::julia_eval("n_thinning = Integer(n_thinning)")
# JuliaCall::julia_eval("n_chains = Integer(n_chains)")

JuliaCall::julia_eval("
samples_and_stats = AbstractMCMC.sample(
  ad_model,
  AdvancedHMC.NUTS(0.65),
  n_samples,
  chain_type = Chains,
  n_adapts = n_adapts,
  discard_initial = n_adapts,
  thinning = n_thinning
)
")

tau_samples <- JuliaCall::julia_eval('Float64.(collect(values(samples_and_stats["tau"][:,1,1])))',need_return = "R")
alpha0_samples <- JuliaCall::julia_eval('Float64.(collect(values(samples_and_stats["alpha0"][:,1,1])))',need_return = "R")

pars <- c("tau","alpha0","alpha1","alpha2")
JuliaCall::julia_assign(x = "pars",pars)

par_names <- paste0("[",paste0(paste0(":",pars),collapse = ","),"]")
alpha0_samples <- JuliaCall::julia_eval(paste0('Array(samples_and_stats[:,',par_names,',:])'),need_return = "R")
