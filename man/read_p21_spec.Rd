% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_p21_spec.R
\name{read_p21_spec}
\alias{read_p21_spec}
\alias{read_p21_spec_datasets}
\alias{read_p21_spec_variables}
\alias{read_p21_spec_codelists}
\alias{read_p21_spec_valuelevels}
\alias{read_p21_spec_dictionaries}
\alias{read_p21_spec_methods}
\alias{read_p21_spec_comments}
\alias{read_p21_spec_documents}
\alias{read_p21_spec_define}
\title{Read P21 Specification Files}
\usage{
read_p21_spec(spec)

read_p21_spec_datasets(spec, domain_sheet)

read_p21_spec_variables(spec, variables_sheet)

read_p21_spec_codelists(spec, codelists_sheet)

read_p21_spec_valuelevels(spec, valuelevel_sheet)

read_p21_spec_dictionaries(spec, dictionaries_sheet)

read_p21_spec_methods(spec, methods_sheet)

read_p21_spec_comments(spec, comments_sheet)

read_p21_spec_documents(spec, documents_sheet)

read_p21_spec_define(spec, define_sheet)
}
\arguments{
\item{spec}{A character string specifying the path to the P21 Excel file.}

\item{domain_sheet}{A character string specifying the name of the sheet containing dataset metadata.}

\item{variables_sheet}{A character string specifying the name of the sheet containing variable metadata.}

\item{codelists_sheet}{A character string specifying the name of the sheet containing codelist metadata.}

\item{valuelevel_sheet}{A character string specifying the name of the sheet containing value level metadata.}

\item{dictionaries_sheet}{A character string specifying the name of the sheet containing dictionary metadata.}

\item{methods_sheet}{A character string specifying the name of the sheet containing method metadata.}
}
\value{
A data frame containing the specified metadata.
}
\description{
These functions read metadata from P21 specification Excel files. They handle different
types of metadata including datasets, variables, codelists, value levels, dictionaries,
and methods.
}
