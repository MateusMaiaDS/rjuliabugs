pkgname <- "rjuliabugs"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "rjuliabugs-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('rjuliabugs')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("bugs2juliaBUGS")
### * bugs2juliaBUGS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: bugs2juliaBUGS
### Title: Convert BUGS Model to Julia's '@bugs' Macro Format
### Aliases: bugs2juliaBUGS

### ** Examples

model <- "
  for i in 1:N
    y[i] ~ dnorm(mu, tau)
  end
  mu ~ dnorm(0.0, 1.0E-6)
  tau ~ dgamma(0.001, 0.001)
"
bugs2juliaBUGS(model)
bugs2juliaBUGS(model, convert_var_name = FALSE)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("bugs2juliaBUGS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("convert_numeric_types")
### * convert_numeric_types

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: convert_numeric_types
### Title: Convert Numeric Elements in a List to Integer or Float
### Aliases: convert_numeric_types

### ** Examples

input_list <- list(
  a = c(x = 1.0, y = 2.5, z = 3.0),
  b = c(foo = 4.0, bar = 5.1),
  c = c(6, 7, 8)
)
convert_numeric_types(input_list)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("convert_numeric_types", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("delete_julia_obj")
### * delete_julia_obj

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: delete_julia_obj
### Title: Delete an object from the Julia Main environment
### Aliases: delete_julia_obj

### ** Examples

## Not run: 
##D JuliaCall::julia_command("x = 10")  # Define a Julia variable
##D delete_julia_obj("x")               # Delete it
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("delete_julia_obj", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_params")
### * get_params

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_params
### Title: Extract and convert posterior parameters
### Aliases: get_params

### ** Examples

## Not run: 
##D get_params(rjuliabugs = fit, params = c("alpha", "beta"), posterior_type = "array")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_params", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("get_params_from_name")
### * get_params_from_name

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: get_params_from_name
### Title: Extract Parameter Samples from a Turing.jl MCMCChains Object
### Aliases: get_params_from_name

### ** Examples

## Not run: 
##D JuliaCall::julia_setup()
##D samples <- get_params(name = "sampler_juliaBUGS",params = c("alpha", "beta"))
##D head(samples)
## End(Not run)

## Not run: 
##D JuliaCall::julia_setup()
##D JuliaCall::julia_library("Turing")
##D JuliaCall::julia_command("... define model and sample into 'chains' ...")
##D samples <- get_params(c("alpha", "beta"), "chains")
##D head(samples)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("get_params_from_name", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("juliaBUGS")
### * juliaBUGS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: juliaBUGS
### Title: Run a Julia HMC Sampler for a BUGS-like Probabilistic Model
### Aliases: juliaBUGS

### ** Examples

## Not run: 
##D model_def <- "model = @model ... end"
##D data <- list(N = 10, x = rnorm(10))
##D result <- juliaBUGS(
##D   data = data,
##D   model_def = model_def,
##D   params_to_save = c("mu"),
##D   name = "my_sampler"
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("juliaBUGS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("julia_assign_int")
### * julia_assign_int

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: julia_assign_int
### Title: Assign an Integer Value to a Julia Variable
### Aliases: julia_assign_int

### ** Examples

## Not run: 
##D julia_assign_int("x", 3.5)  # Will be cast to 3L in Julia
##D JuliaCall::julia_eval("x")  # Returns 3 (as Integer in Julia)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("julia_assign_int", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("load_rjuliabugs")
### * load_rjuliabugs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: load_rjuliabugs
### Title: Load an 'rjuliabugs' Object and Restore the Julia State
### Aliases: load_rjuliabugs

### ** Examples

## Not run: 
##D model <- load_rjuliabugs("my_model.rds")
##D # model$name now contains the (possibly updated) name used in Julia
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("load_rjuliabugs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("save_rjuliabugs")
### * save_rjuliabugs

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: save_rjuliabugs
### Title: Save an 'rjuliabugs' Object and Its Julia State
### Aliases: save_rjuliabugs

### ** Examples

## Not run: 
##D save_rjuliabugs(my_model, file = "my_model", chains_file = "chains.jls")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("save_rjuliabugs", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("setup_juliaBUGS")
### * setup_juliaBUGS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: setup_juliaBUGS
### Title: Setup Julia Environment for JuliaBUGS
### Aliases: setup_juliaBUGS

### ** Examples

## Not run: 
##D # Setup Julia with core packages only
##D setup_juliaBUGS()
##D 
##D # Setup Julia with additional packages
##D setup_juliaBUGS(extra_packages = c("Distributions", "Turing"))
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("setup_juliaBUGS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wrap_model_to_juliaBUGS")
### * wrap_model_to_juliaBUGS

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wrap_model_to_juliaBUGS
### Title: Wrap BUGS Model for Julia
### Aliases: wrap_model_to_juliaBUGS

### ** Examples

model_body <- "
  for i in 1:N
    r[i] ~ dbin(p[i], n[i])
    b[i] ~ dnorm(0.0, tau)
    p[i] = logistic(alpha0 + alpha1 * x1[i] + alpha2 * x2[i] + alpha12 * x1[i] * x2[i] + b[i])
  end
  alpha0 ~ dnorm(0.0, 1.0E-6)
  tau ~ dgamma(0.001, 0.001)
  sigma = 1 / sqrt(tau)
"
wrap_model_to_juliaBUGS(model_body)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wrap_model_to_juliaBUGS", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
