rm(list=ls())
set.seed(42)
devtools::load_all()

data <- list(
  r = c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46, 10, 8, 10, 8, 23, 0, 3, 22, 15, 32, 3),
  n = c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79, 13, 16, 30, 28, 45, 4, 12, 41, 30, 51, 7),
  x1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  x2 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
  N = 21
)

model <- "
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
"

# bugs_code <- "
# model{
#     for( i in 1 : N ) {
#         r[i] ~ dbin(p[i],n[i])
#         b[i] ~ dnorm(0.0,tau)
#         logit(p[i]) <- alpha0 + alpha1 * x1[i] + alpha2 * x2[i] +
#         alpha12 * x1[i] * x2[i] + b[i]
#     }
#     alpha0 ~ dnorm(0.0,1.0E-6)
#     alpha1 ~ dnorm(0.0,1.0E-6)
#     alpha2 ~ dnorm(0.0,1.0E-6)
#     alpha12 ~ dnorm(0.0,1.0E-6)
#     tau ~ dgamma(0.001,0.001)
#     sigma <- 1 / sqrt(tau)
# }"

# model <- wrap_model_to_juliaBUGS(model_code = model)
# bugs2juliaBUGS(bugs_code)

n_iter = 10000
n_warmup= floor(n_iter/2)
n_discard = n_warmup
n_thin = 5
n_chain = 4
params_to_save <- c("alpha0","alpha1","alpha2","alpha12","sigma")

posterior <- juliaBUGS(data = data,
          model = model,
          params_to_save = params_to_save,
          n_iter = n_iter,
          n_warmup = n_warmup,
          n_discard = n_discard,
          n_chain = n_chain,use_parallel = FALSE,
          n_thin = n_thin)

library(bayesplot)

mcmc_areas(posterior$params,
           pars = params_to_save,
           prob = 0.8)


alpha0_sigma_post <-get_params(params = c("alpha0","sigma"),
                               sampler_name = posterior$sampler_name)
mcmc_trace(x = posterior$params,pars =  c("alpha0","sigma"),
           n_warmup = 0,
           facet_args = list(nrow = 2))
summary.rjuliabugs(posterior,julia_summary_only = TRUE)


# using any other posterior
posterior::summarise_draws(posterior$params)

# Any other diagnostic is also available by
help("diagnostics", "posterior")
ess_basic(posterior$params)
