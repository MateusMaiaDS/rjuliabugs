% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary.rjuliabugs}
\alias{summary.rjuliabugs}
\title{Summary Method for JuliaBUGS Sampler Output}
\usage{
\method{summary}{rjuliabugs}(
  object,
  digits = 4,
  n_display = 10,
  get_summary = FALSE,
  get_quantiles = FALSE,
  julia_summary_only = FALSE
)
}
\arguments{
\item{object}{An object of class `rjuliabugs` containing a reference to a Julia MCMC sampler.}

\item{digits}{Integer. Number of significant digits to display in printed summary statistics.}

\item{n_display}{Integer. Maximum number of rows of summary statistics to display.}

\item{get_summary}{Logical. If `TRUE`, returns the summary statistics as part of the result.}

\item{get_quantiles}{Logical. If `TRUE`, includes quantile statistics in the result.}

\item{julia_summary_only}{Logical. If `TRUE`, displays the Julia summary object directly in Julia, skipping R summary output.}
}
\value{
If `julia_summary_only = TRUE`, returns invisibly `NULL` after displaying the Julia object.
        Otherwise, returns a list possibly containing elements:
\describe{
  \item{summary}{A data frame of summary statistics from Julia. If `get_summary = TRUE.`}
  \item{quantiles}{A data frame of quantiles if `get_quantiles = TRUE`.}
}
}
\description{
Provides a summary of the results from a JuliaBUGS sampler object, including MCMC settings,
summary statistics, and optionally quantiles.
}
\details{
This method wraps Julia's `MCMCChains.summarystats` and `MCMCChains.quantile` to extract and display
results in R using the `JuliaCall` interface. It also extracts key MCMC settings like number of chains,
iterations, and samples per chain. The printed summary is truncated to `n_display` rows.
}
