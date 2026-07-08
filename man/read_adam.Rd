% Generated manually for ydisctools (hand-maintained; keep in sync with the
% roxygen block in R/ars_run.R).
\name{read_adam}
\alias{read_adam}
\title{Read an ADaM dataset by format priority}
\usage{
read_adam(path)
}
\arguments{
\item{path}{Dataset stem (e.g. \code{"data/ADSL"}) or a full file name; any
recognised extension is stripped and the folder is searched by priority.
Matching is case-insensitive on the extension and the stem.}
}
\value{
A data frame (tibble) with the dataset contents.
}
\description{
Reads one ADaM dataset from a \emph{stem} (a path without extension),
picking the actual file present on disk by format priority when several
same-named files coexist: \strong{\code{.xpt} > \code{.sas7bdat} >
\code{.rds} > \code{.rda}/\code{.rdata} > \code{.csv}}.  This is what
\code{\link{ars_generate_ard}} wires the siera-generated programmes to
call, so a study's real transport/SAS/R datasets are read directly instead
of siera's hardcoded \code{.csv} (siera 0.5.6 only emits
\code{readr::read_csv()}).
}
\details{
The readers: \code{.xpt} via \code{haven::read_xpt()}, \code{.sas7bdat} via
\code{haven::read_sas()}, \code{.rds} via \code{\link{readRDS}}, \code{.rda}
/ \code{.rdata} via \code{\link{load}} (the object named after the stem,
else the first loaded object), \code{.csv} via \code{readr::read_csv()}.
For the \pkg{haven} formats value labels are zapped
(\code{haven::zap_labels()}) so a \code{haven_labelled} column behaves like
the plain vector the \code{.csv} route would yield.
}
\examples{
\dontrun{
# reads data/ADSL.xpt if present, else .sas7bdat, .rds, .rda, .csv
adsl <- read_adam("data/ADSL")
}
}
\seealso{
\code{\link{ars_generate_ard}}
}
