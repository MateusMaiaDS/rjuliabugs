
# rjuliabugs <img src="man/figures/logo.png" align="right" alt="" />

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- [![R build status](https://github.com/MateusMaiaDS/rjuliabugs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MateusMaiaDS/rjuliabugs/actions) -->
<!-- [![R-CMD-check](https://github.com/MateusMaiaDS/rjuliabugs/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MateusMaiaDS/rjuliabugs/actions/workflows/R-CMD-check.yaml) -->
<!-- badges: end -->

**rjuliabugs** is an R package that provides a bridge between R and
[JuliaBUGS](https://github.com/TuringLang/JuliaBUGS.jl?tab=readme-ov-file),
the [BUGS](https://en.wikipedia.org/wiki/WinBUGS)-style Bayesian
modeling interface developed in Julia as part of the
[Turing.jl](https://turing.ml/dev/) probabilistic programming ecosystem.

**JuliaBUGS** allows users to define models using the familiar BUGS
syntax while leveraging the speed and flexibility of the Julia language.
It enables automatic translation of BUGS code into modern probabilistic
programs that run efficiently with advanced inference engines such as
Hamiltonian Monte Carlo (HMC) — all accessible via Turing.jl.

With `rjuliabugs`, R users can run BUGS models/code through JuliaBUGS
and take advantage of the inference algorithms available in Turing.jl,
without leaving the R environment. This not only provides a seamless
path to adopt faster (including
[parallelization](#using-rjuliabugs-in-parallel)) and more flexible
sampling methods but also allows integration with R’s extensive
post-processing ecosystem (e.g., `bayesplot`, `posterior`, `coda`). It
lowers the barrier for existing BUGS users to adopt modern Bayesian
tools without abandoning their existing model codebase.

## Installation

You can install the development version of **rjuliabugs** from
[GitHub](https://github.com/MateusMaiaDS/rjuliabugs) with:

``` r
# install.packages("remotes")
remotes::install_github("MateusMaiaDS/rjuliabugs")
```

⚠️ **Loading the package**:  
When loading the package using `library(rjuliabugs)` for the first time
in an R session, it may take longer than most R packages because it
needs to initialize the Julia environment through
`JuliaCall::julia_setup()`, which can be time-consuming. Using the
package for the first time may also involve installing all the required
Julia libraries for `JuliaBUGS`—such as `AbstractMCMC` and others—if
they are not already installed.

## Using `rjuliabugs`: Setup, Troubleshooting, Parallelization Settings,and Contribution

For further guidance, the rest of this README summarises most of the FAQ
and additional instructions for installing Julia and setting
`rjuliabugs`, troubleshooting, and running `rjuliabugs` in parallel. We
recommend reading the following sections:

- [Installing Julia](#installing-julia)
- [Troubleshooting JuliaCall setup:](#troubleshooting-juliacall-setup)
  - [Julia not found](#error-type-julia-not-found)
  - [R not found](#error-type-r_home-not-found)
  - [Other installation issues](#other-installation-issues)
- [Using `rjuliabugs` in parallel](#using-rjuliabugs-in-parallel)

As this is an open-source project, collaboration is welcome. Further
details can be found at:

- [Contributing](#contributing)
- [License](#license)
- [Acknowledgements](#acknowledgements)

Last, for complete documentation with working examples and more details
on `rjuliabugs` functionalities, see the
[vignette](https://turinglang.org/JuliaBUGS.jl/stable/example/).

See also, [JuliaBUGS
Documentation](https://turinglang.org/JuliaBUGS.jl/stable/),[JAGS Source
Code](https://sourceforge.net/p/mcmc-jags/code-0/ci/default/tree/).

## Installing `Julia`

We recommend installing Julia using
[`juliaup`](https://github.com/JuliaLang/juliaup), the **official Julia
version manager**. It provides a reliable and maintainable way to
install, update, and manage Julia versions across platforms. Using
`juliaup` is preferred over downloading Julia manually because it makes
upgrading Julia seamless and safe, supports managing multiple versions,
and ensures compatibility with R-Julia integration tools like
`JuliaCall`. To install, follow the instructions:

### Windows (via PowerShell)

Open PowerShell (as Administrator) and run:

``` powershell
winget install --id=JuliaLang.Julia -e
```

> Requires Windows 10/11 with
> [winget](https://learn.microsoft.com/en-us/windows/package-manager/winget/)
> available.

### macOS (via Homebrew)

If you use [Homebrew](https://brew.sh), run:

``` sh
brew install juliaup
juliaup add release
```

This installs `juliaup` and sets the latest stable Julia version as
default.

### Ubuntu (via APT)

On recent Ubuntu versions, you can install via APT:

``` sh
sudo apt update
sudo apt install juliaup
juliaup add release
```

If `juliaup` is not available via APT on your system, follow manual
installation instructions at the [Juliaup GitHub
page](https://github.com/JuliaLang/juliaup#installation).

#### Verify Installation

After installation, open a terminal and run:

``` sh
julia
```

This should start the Julia REPL with the installed version. You are now
ready to use Julia with `rjuliabugs`.

## Troubleshooting `JuliaCall` setup

The `rjuliabugs` package relies on the
[`JuliaCall`](https://github.com/JuliaInterop/JuliaCall) package to
communicate with Julia from R. Errors may occur if Julia is not properly
installed or if `JuliaCall` cannot locate the Julia binary. The most
common errors include:

### Error type: “`Julia` Not Found”

The `JULIA_HOME` environment variable tells R where to find Julia. If
it’s not set correctly, `rjuliabugs` will not be able to communicate
with Julia via `JuliaCall`.

1.  Open your terminal or command prompt.  
2.  Check if `JULIA_HOME` is set by running:

<!-- -->

    echo $JULIA_HOME

> (On Windows PowerShell use: `echo $Env:JULIA_HOME`)

3.  If empty or incorrect, and you installed Julia using `juliaup`, the
    Julia binary is typically located at:

<!-- -->

    ~/.juliaup/bin

You can set the environment variable for the current session with:

    export JULIA_HOME="$HOME/.juliaup/bin"

> (On Windows PowerShell use: `$Env:JULIA_HOME="$HOME/.juliaup/bin`“)

Replace the path with the correct location if your installation differs
(e.g., if you’re on Windows, it might be something like
`"C:/Users/YourName/AppData/Local/Programs/Julia-1.x.x/bin"`).

4.  **To make this setting permanent (so you don’t need to set it every
    time):**

    - **On macOS/Linux:**  
      Open your shell configuration file (e.g., `.bashrc`, `.zshrc`)
      with a text editor:

      ``` bash
      nano ~/.bashrc
      ```

      or

      ``` bash
      nano ~/.zshrc  
      ```

      Add the following line at the end of the file:

      ``` bash
      export JULIA_HOME="$HOME/.juliaup/bin"  
      ```

      Save and exit (`Ctrl+O`, `Enter`, `Ctrl+X` in nano).  
      Then reload your shell configuration or restart your terminal:

      ``` bash
      source ~/.bashrc
      ```

      *(or `source ~/.zshrc` if using zsh)*

    - **On Windows:**

      1.  Search for “Environment Variables” in the Start menu and open
          “Edit the system environment variables.”  
      2.  Click “Environment Variables.”  
      3.  Under “User variables” or “System variables,” click “New…”  
      4.  Set the variable name as `JULIA_HOME` and the value as the
          path to Julia’s binary installed by juliaup (e.g.,
          `C:\Users\your_user\AppData\Local\Microsoft\WindowsApps`).  
      5.  Click OK to save all dialogs.  
      6.  Restart your terminal or R session for changes to take effect.

5.  Verify it is set correctly by re-running:

``` bash
echo $JULIA_HOME  
```

> (or in R use: `Sys.getenv("JULIA_HOME")`)

Setting `JULIA_HOME` correctly ensures that `JuliaCall` can launch
Julia, which is required for `rjuliabugs` to function.

### Error type: “`R_HOME` not found”

To ensure `rjuliabugs` works correctly, set the `R_HOME` environment
variable so Julia can locate your R installation.

1.  Open your terminal or command prompt.
2.  Check if `R_HOME` is set by running:

``` bash
echo $R_HOME
```

(on Windows PowerShell use `echo $Env:R_HOME`)

3.  If empty, open R and run:

``` r
R.home()
```

4.  Copy the printed R installation path.
5.  Set the environment variable in your terminal session with:

<!-- -->

    export R_HOME="PASTE_YOUR_PATH_HERE"

(On Windows PowerShell, use: `$Env:R_HOME="PASTE_YOUR_PATH_HERE"`)

6.  **To make this setting permanent (applies every time you open a
    terminal or run R):**

    - **On macOS/Linux:**  
      Open your shell configuration file with a text editor (e.g.,
      nano):

      ``` bash
      nano ~/.bashrc
      ```

      or if you use zsh:

      ``` bash
      nano ~/.zshrc
      ```

      Add the line:

      ``` bash
      export R_HOME="PASTE_YOUR_PATH_HERE"
      ```

      Save and exit (`Ctrl+O`, `Enter`, `Ctrl+X` in nano).  
      Then reload your shell configuration or restart your terminal:

      ``` bash
      source ~/.bashrc
      ```

      *(or `source ~/.zshrc` if using zsh)*

    - **On Windows:**

      1.  Search for “Environment Variables” in the Start menu and open
          “Edit the system environment variables.”  
      2.  Click “Environment Variables.”  
      3.  Under “User variables” or “System variables,” click “New…”  
      4.  Set the variable name as `R_HOME` and the value as the full R
          path you copied.  
      5.  Click OK to save all dialogs.  
      6.  Restart your terminal or R session for changes to take effect.

7.  Verify by reopening the terminal and running the echo command from
    step 2.

This ensures Julia, when called from R via JuliaCall, can find your R
installation and prevents initialization errors in `rjuliabugs`.

### Other installations issues

Many of the most common installation problems related to `JuliaCall` are
well documented in the [Troubleshooting and Ways to Get
Help](https://github.com/JuliaInterop/JuliaCall#troubleshooting-and-ways-to-get-help)
section of the `JuliaCall` GitHub page.

If you encounter any issues while setting up `rjuliabugs` on your system
that are not covered there, feel free to open an
[issue](https://github.com/MateusMaiaDS/rjuliabugs/issues) on this
repository. We’re happy to help!

## Using `rjuliabugs` in parallel

The `JuliaBUGS` library in Julia supports parallel sampling through
`AbstractMCMC`. While in a native Julia session this can be easily
enabled by launching Julia with the `-t <n_threads>` flag, using
multithreaded sampling via `rjuliabugs` requires additional setup.
Specifically, you must define the environment variable
`JULIA_NUM_THREADS=n_threads`, where `n_threads` is the number of
threads (or CPU cores) you wish to use for parallel computation. This
ensures that `rjuliabugs` can properly initialize Julia with
multithreading support when running multiple chains in parallel from
within an R session. The instructions on how to configure
`JULIA_NUM_THREADS` and enable multithreading in your environment, are
the following:

1.  Open your terminal or command prompt.  
2.  Check what is set as `JULIA_NUM_THREADS` is set by running:

``` bash
echo $JULIA_NUM_THREADS
```

> (On Windows PowerShell use: `echo $Env:JULIA_NUM_THREADS`)

3.  If empty or incorrectly set, decide how many threads you want Julia
    to use (e.g., 4), then set the environment variable in your terminal
    session:

``` bash
export JULIA_NUM_THREADS=4
```

> (On Windows PowerShell use: `$Env:JULIA_NUM_THREADS="4"`)

4.  **To make this setting permanent (applies every time you open R and
    Julia):**

    - **On macOS/Linux:**  
      Open your shell configuration file (e.g., `.bashrc`, `.zshrc`)
      with a text editor:

      ``` bash
      nano ~/.bashrc
      ```

      or

      ``` bash
      nano ~/.zshrc  
      ```

      Add the following line at the end of the file:

      ``` bash
      export JULIA_NUM_THREADS=4  
      ```

      Save and exit (`Ctrl+O`, `Enter`, `Ctrl+X` in nano).  
      Then reload your shell configuration or restart your terminal:

      ``` bash
      source ~/.bashrc
      ```

      *(or `source ~/.zshrc` if using zsh)*

    - **On Windows:**

      1.  Search for “Environment Variables” in the Start menu and open
          “Edit the system environment variables.”  
      2.  Click “Environment Variables.”  
      3.  Under “User variables” or “System variables,” click “New…”  
      4.  Set the variable name as `JULIA_NUM_THREADS` and the value as
          the number of threads (e.g., `4`).  
      5.  Click OK to save all dialogs.  
      6.  Restart your terminal or R session for changes to take effect.

5.  Verify it is set correctly by re-running:

``` bash
echo $JULIA_NUM_THREADS  
```

> (or in R use: `Sys.getenv("JULIA_NUM_THREADS")`)

Setting `JULIA_NUM_THREADS` ensures Julia can execute in parallel where
possible when running models through `rjuliabugs`.

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
