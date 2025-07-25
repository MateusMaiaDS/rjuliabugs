% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_load_rjuliabugs.R
\name{save_rjuliabugs}
\alias{save_rjuliabugs}
\title{Save an \code{rjuliabugs} Object and Its Julia State}
\usage{
save_rjuliabugs(rjuliabugs_model, file, chains_file = NULL)
}
\arguments{
\item{rjuliabugs_model}{An object of class \code{rjuliabugs}, containing at least
the fields \code{name} (Julia object name as a string) and \code{chains_file}.}

\item{file}{A character string giving the base name or path for saving the \code{.rds} file.
If the extension \code{.rds} is missing, it will be appended automatically.}

\item{chains_file}{Optional character string giving the path where the Julia
object should be serialized. The file name should have the \code{.jls} extension.
If \code{NULL}, uses the \code{chains_file} field from \code{rjuliabugs_model}.}
}
\value{
Returns \code{invisible(NULL)}. Used saving both the
Julia object and the R \code{rjuliabugs} object to disk.
}
\description{
Serializes the Julia object contained in an \code{rjuliabugs} object and saves
the entire object as an \code{.rds} file. The Julia object is saved separately
using Julia's \code{Serialization.serialize}. The file path can be passed manually,
or retrieved from the \code{chains_file} slot in the object.
}
\examples{
\dontrun{
save_rjuliabugs(my_model, file = "my_model", chains_file = "chains.jls")
}

}
