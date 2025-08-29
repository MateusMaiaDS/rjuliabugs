test_that("intro vignette example runs on Julia", {

  # Skip everything on CRAN
  if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    skip("Skipping test on CRAN")
  }

  # Only run when Julia is available (e.g., in CI)
  if (Sys.which("julia") == "") {
    skip("Julia is not available on PATH; skipping intro example test.")
  }

  # Ensure R_HOME is visible to Julia's RCall (extra safety in addition to CI setup)
  Sys.setenv(R_HOME = R.home())

  # Setup Julia environment
  capture.output({
    if (Sys.getenv("CI") == "true") {
      # In CI: packages are pre-installed, skip verification
      rjuliabugs::setup_juliaBUGS(verify_package = TRUE)
    } else {
      # Local development: verify packages
      rjuliabugs::setup_juliaBUGS(verify_package = TRUE)
    }
  })

  # Data from the intro vignette (seeds example)
  data <- list(
    r = c(10, 23, 23, 26, 17, 5, 53, 55, 32, 46, 10, 8, 10, 8, 23, 0, 3, 22, 15, 32, 3),
    n = c(39, 62, 81, 51, 39, 6, 74, 72, 51, 79, 13, 16, 30, 28, 45, 4, 12, 41, 30, 51, 7),
    x1 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    x2 = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
    N = 21
  )

  # BUGS model from the intro vignette
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

  # Keep the sampler tiny for CI
  params_to_save <- c("alpha0", "alpha1", "alpha2", "alpha12", "sigma")

  posterior <- rjuliabugs::juliaBUGS(
    data = data,
    model_def = model_def,
    params_to_save = params_to_save,
    n_iter = 200,
    n_warmup = 100,
    n_discard = 100,
    n_chain = 1,
    use_parallel = FALSE,
    n_thin = 1,
    progress = TRUE
  )

  # Basic structure checks
  expect_s3_class(posterior, "rjuliabugs")

  # Check if params exist and have the right structure
  expect_true(!is.null(posterior$params))

  # For a single chain with array output, we might get a 2D array (iterations x parameters)
  # or a 3D array (iterations x chains x parameters)
  dims <- dim(posterior$params)

  if (length(dims) == 2) {
    # 2D array: iterations x parameters
    expect_equal(dims[2], length(params_to_save))
    expect_gt(dims[1], 0)  # some posterior draws returned
    # Check parameter names
    expect_true(all(params_to_save %in% colnames(posterior$params)))
  } else if (length(dims) == 3) {
    # 3D array: iterations x chains x parameters
    expect_equal(dims[2], 1)  # 1 chain
    expect_equal(dims[3], length(params_to_save))
    expect_gt(dims[1], 0)  # some posterior draws returned
    # Check parameter names
    expect_true(all(params_to_save %in% dimnames(posterior$params)[[3]]))
  } else {
    stop("Unexpected dimensions for posterior$params")
  }
})

