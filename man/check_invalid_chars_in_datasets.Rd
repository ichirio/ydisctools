% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_invalid_chars.R
\name{check_invalid_chars_in_datasets}
\alias{check_invalid_chars_in_datasets}
\title{Check for Invalid Characters in Multiple Datasets}
\usage{
check_invalid_chars_in_datasets(datasets, question = FALSE, report_file = NULL)
}
\arguments{
\item{datasets}{A list of data frames in which to check for invalid characters.}

\item{question}{A logical value indicating whether to detect and remove the question mark character. Default is FALSE.}
}
\value{
A data frame with the results of the invalid character checks for all datasets combined.
}
\description{
This function checks for invalid characters in multiple datasets and combines the results into a single data frame.
}
\examples{
library(dplyr)
library(purrr)
df1 <- data.frame(text = c("Hello\x01World", "Example\x0AString"))
df2 <- data.frame(text = c("Another\x0DTest", "More\x0AData"))
datasets <- list(df1, df2)
check_invalid_chars_in_datasets(datasets)
check_invalid_chars_in_datasets(datasets, question = TRUE)
}
