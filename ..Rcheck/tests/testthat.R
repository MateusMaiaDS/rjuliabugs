library(testthat)

if (Sys.getenv("CI") == "true" && grepl("linux", R.version$os)) {
  cat("Skipping rjuliabugs tests on Linux CI to avoid JuliaCall segfaults.\n")
} else {
  library(rjuliabugs)
  test_check("rjuliabugs")
}

