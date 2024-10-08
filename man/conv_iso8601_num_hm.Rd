% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conv_iso8601.R
\name{conv_iso8601_num_hm}
\alias{conv_iso8601_num_hm}
\title{Convert POSIXct to ISO 8601 Format (Date and Time without Seconds)}
\usage{
conv_iso8601_num_hm(date)
}
\arguments{
\item{date}{A POSIXct object or character string.}
}
\value{
A character string in ISO 8601 format ("YYYY-MM-DDTHH:MM") if the input is a POSIXct object, otherwise returns the input as a character string.
}
\description{
This function converts a POSIXct object to an ISO 8601 formatted datetime string ("YYYY-MM-DDTHH:MM"), excluding seconds.
}
\examples{
conv_iso8601_num_hm(as.POSIXct("2023-09-15 14:30:00"))
conv_iso8601_num_hm("2023-09-15T14:30")
}
