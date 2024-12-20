% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comp_char_date.R
\name{comp_char_date}
\alias{comp_char_date}
\title{Compare Character Dates}
\usage{
comp_char_date(x, y, mark = "<=")
}
\arguments{
\item{x}{A character string representing the first date.}

\item{y}{A character string representing the second date.}

\item{mark}{A character string representing the comparison operator.
Must be one of "<", "<=", ">", ">=", "==", "!=". Default is "<=".}
}
\value{
A logical value indicating the result of the comparison, or NA if the input is invalid.
}
\description{
This function compares two character dates based on a specified comparison operator.
}
\examples{
comp_char_date("2023-10-01", "2023-10-02", "<=") # TRUE
comp_char_date("2023-10-01", "2023-10-02", ">")  # FALSE
comp_char_date("2023-10-01", "2023-10-01", "==") # TRUE

}
