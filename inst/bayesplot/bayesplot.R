library("bayesplot")
library("rstanarm")
library("ggplot2")
library(JuliaCall)
fit <- stan_glm(mpg ~ ., data = mtcars)
posterior <- as.matrix(fit)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(posterior,
           pars = c("cyl", "drat", "am", "wt"),
           prob = 0.8) + plot_title

color_scheme_set("red")
ppc_dens_overlay(y = fit$y,
                 yrep = posterior_predict(fit, draws = 50))


library("rstan")
