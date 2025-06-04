rm(list = ls())
set.seed(123)

# Simulación de datos (igual que en tu código original)
N <- 100
alpha_true <- 2
beta_true <- 3
sigma_true <- 1
x <- sort(runif(N, 0, 10))
y <- rnorm(N, mean = alpha_true + beta_true * x, sd = sigma_true)

data <- list(
  N = N,
  x = x,
  y = y
)


# model <- "
# model = @bugs begin
#   for i in 1:N
#     y[i] ~ dnorm(mu[i], tau)
#     mu[i] = alpha + beta * x[i]
#     y_pred[i] ~ dnorm(mu[i], tau)
#   end
#   alpha ~ dnorm(0, 1.0E-4)
#   beta ~ dnorm(0, 1.0E-4)
#   tau ~ dgamma(0.001, 0.001)
#   sigma = 1 / sqrt(tau)
# end
# "
#
# model <- wrap_model_to_juliaBUGS(model_code = model)

bugs_code <- "
# Likelihood
  for (i in 1:N) {
    y[i] ~ dnorm(mu[i], sigma^-2.0)
    mu[i] <- alpha + beta * x[i]
    y_pred[i] ~ dnorm(mu[i], sigma^-2.0) # This is the key extra line to include to get the posterior predictive
  }

  # Priors
  alpha ~ dnorm(0, 100.0^-2.0)
  beta ~ dnorm(0, 100.0^-2.0)
  sigma ~ dunif(0, 10.0)
"

model <- bugs2juliaBUGS(bugs_code)

n_iter = 2000
n_warmup= floor(n_iter/2)
n_discard = n_warmup
n_thin = 1

params_to_save <- c("alpha","beta","sigma")

posterior <- juliaBUGS(data = data,
                       model = model,
                       params_to_save = params_to_save,
                       n_iter = n_iter,
                       n_warmup = n_warmup,
                       n_discard = n_discard,
                       n_thin = 1)

library(bayesplot)

mcmc_areas(posterior$params,
           pars = params_to_save,
           prob = 0.8)


alpha0_sigma_post <- get_params(params = c("alpha","sigma"),julia_sampler =  "sampler_juliaBUGS")
mcmc_trace(x = posterior$params,pars =  c("alpha","sigma"),
           n_warmup = 0,
           facet_args = list(nrow = 2, labeller = label_parsed))


aux <- summary.rjuliabugs(posterior,digits = 3,n_display = 10)
