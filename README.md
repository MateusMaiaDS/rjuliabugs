rjuliabugs
================

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![R build
status](https://github.com/MateusMaiaDS/rjuliabugs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MateusMaiaDS/rjuliabugs/actions)
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/rjuliabugs)](https://CRAN.R-project.org/package=rjuliabugs) -->
<!-- badges: end -->

# rjuliabugs

**rjuliabugs** is an R package that provides a bridge between R and
[JuliaBUGS](https://turing.ml/dev/), the BUGS-style Bayesian modeling
interface developed in Julia as part of the
[Turing.jl](https://turing.ml/dev/) probabilistic programming ecosystem.

This interface enables R users to access and run JuliaBUGS models from
within R, leveraging modern Bayesian methods such as Hamiltonian Monte
Carlo (HMC) and Automatic Differentiation (AD) while maintaining
compatibility with R’s robust ecosystem for diagnostics and
visualization.

## Installation

You can install the development version of **rjuliabugs** from
[GitHub](https://github.com/MateusMaiaDS/rjuliabugs) with:

``` r
# install.packages("remotes")
remotes::install_github("MateusMaiaDS/rjuliabugs")
```

# Troubleshooting and `JuliaCall` setup

## Contributing

Contributions are welcome! If you encounter a bug or have a feature
request, please open an
[issue](https://github.com/MateusMaiaDS/rjuliabugs/issues).

## License

This package is licensed under the MIT License. See the
[LICENSE](LICENSE) file for details.

## Acknowledgements

This project is part of a Google Summer of Code 2025 initiative under
the [Turing.jl](https://turing.ml/dev/) organization.
