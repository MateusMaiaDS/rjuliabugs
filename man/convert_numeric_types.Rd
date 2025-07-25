% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{convert_numeric_types}
\alias{convert_numeric_types}
\title{Convert Numeric Elements in a List to Integer or Float}
\usage{
convert_numeric_types(data)
}
\arguments{
\item{data}{A list of numeric vectors.}
}
\value{
A list of the same structure where each numeric element is coerced to the appropriate type:
integers for whole numbers, and floats otherwise. Names are preserved.
}
\description{
This function takes a list of numeric vectors and returns a new list
where each numeric element is automatically converted to either an
integer (if it is a whole number) or kept as a float (numeric).
It also preserves the original names of vector elements, if any.
}
\examples{
input_list <- list(
  a = c(x = 1.0, y = 2.5, z = 3.0),
  b = c(foo = 4.0, bar = 5.1),
  c = c(6, 7, 8)
)
convert_numeric_types(input_list)

}
