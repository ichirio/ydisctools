% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catx.R
\name{catx}
\alias{catx}
\title{Concatenate strings with a separator, excluding empty strings and NA values}
\usage{
catx(sep, ...)
}
\arguments{
\item{sep}{A character string used to separate the concatenated values.}

\item{...}{Vectors or character strings to concatenate. Empty strings and \code{NA} values are ignored.}
}
\value{
A character vector where each element is a concatenation of the input strings, separated by \code{sep}.
}
\description{
\code{catx} concatenates multiple vectors or character strings using a specified separator, while
excluding any empty strings (\code{""}) and \code{NA} values.
}
\details{
This function is useful when you want to concatenate multiple values but avoid including
empty strings or \code{NA} values in the result. The function internally uses \code{paste} to handle
string concatenation and \code{pmap_chr} to iterate over multiple inputs.
}
\examples{
# Basic usage
catx("", "Hello", "", NA, "World")
# Returns: "HelloWorld"

# Using a space as a separator
catx(" ", "Hello", "", NA, "World")
# Returns: "Hello World"

# Concatenating multiple vectors with commas
catx(",", c("A", NA), c("", "B"), "C")
# Returns: "A,C" "B"
}
