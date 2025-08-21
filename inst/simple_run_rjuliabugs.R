# rm(list=ls())
# devtools::load_all()


data <- list(
  r = c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46, 10, 8, 10, 8, 23, 0, 3, 22, 15, 32, 3),
  n = c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79, 13, 16, 30, 28, 45, 4, 12, 41, 30, 51, 7),
  x1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  x2 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
  N = 21
)

model_def <- "
model {
    for (i in 1:N) {
        r[i] ~ dbin(p[i], n[i])
        b[i] ~ dnorm(0.0, tau)
        logit(p[i]) <- alpha0 + alpha1 * x1[i] + alpha2 * x2[i] +
                       alpha12 * x1[i] * x2[i] + b[i]
    }
    alpha0 ~ dnorm(0.0, 1.0E-6)
    alpha1 ~ dnorm(0.0, 1.0E-6)
    alpha2 ~ dnorm(0.0, 1.0E-6)
    alpha12 ~ dnorm(0.0, 1.0E-6)
    tau ~ dgamma(0.001, 0.001)
    sigma <- 1 / sqrt(tau)
}
"


library(rjuliabugs)

posterior <- juliaBUGS(
  data = data,
  model_def = model_def,
  params_to_save = c("alpha0", "alpha1", "alpha2", "alpha12", "sigma"),
  n_iter = 1500,
  n_warmup = 500,
  n_discard = 500,
  n_chain = 1,
  use_parallel = FALSE,
  n_thin = 1
)
