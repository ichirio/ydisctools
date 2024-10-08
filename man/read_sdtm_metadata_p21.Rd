% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sdtm_metadata.R
\name{read_sdtm_metadata_p21}
\alias{read_sdtm_metadata_p21}
\title{Read SDTM Metadata from P21 Spec File}
\usage{
read_sdtm_metadata_p21(spec)
}
\arguments{
\item{spec}{A character string specifying the path to the P21 Excel file.}
}
\value{
A list containing two data frames: \code{datasets} and \code{variables}, which hold the metadata for datasets and variables respectively.
}
\description{
This function reads SDTM metadata from a P21 Excel file, extracting dataset and variable metadata.
}
