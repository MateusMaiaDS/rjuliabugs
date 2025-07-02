<p align="center">
  <img src="man/figures/svg_juliabugs_logo" height="160" />
</p>

# rjuliabugs

**`rjuliabugs`** is an R package that provides a seamless interface between R and [`JuliaBUGS.jl`](https://github.com/TuringLang/JuliaBUGS.jl), a BUGS-style probabilistic programming framework built on top of [`Turing.jl`](https://github.com/TuringLang/Turing.jl). It allows users to define, fit, and extract results from Bayesian models implemented in Julia using a familiar BUGS-like syntax, all from within R.

---

## 📦 Installation

Install the development version of `rjuliabugs` from GitHub:

```r
# You need devtools or remotes
install.packages("remotes")
remotes::install_github("MateusMaiaDS/rjuliabugs")
```
