\name{validate_submission}
\alias{validate_submission}
\title{Validate a study-data folder}
\usage{
validate_submission(
  path,
  standard = c("SDTM", "SEND", "ADaM"),
  define = NULL,
  rules = NULL,
  ct = NULL,
  version_label = "openstudyval 0.1.0"
)
}
\arguments{
\item{path}{Directory of datasets, or a vector of dataset files.}

\item{standard}{One of \code{"SDTM"}, \code{"SEND"}, or \code{"ADaM"}.}

\item{define}{Optional path to define.xml. If \code{NULL}, a file named
\code{define.xml} in \code{path} is used when present.}

\item{rules}{Optional data frame of extra rules. See \code{\link{add_custom_rules}}.}

\item{ct}{Optional data frame of controlled terminology. Column names are
variable names and values are permitted terms.}

\item{version_label}{Character label stored on the report.}
}
\value{An object of class \code{osv_report}.}
\description{
Reads datasets, then runs XPORT constraints, standard-specific structural
checks, the built-in rule catalog, technical-rejection criteria, and an
optional define.xml cross-check.
}
\details{
The result is not a regulatory certification. Agency engines remain the
reference for a live submission.
}
\examples{
report <- validate_submission(example_study("sdtm"), standard = "SDTM")
summary(report)
}
\seealso{\code{\link{validate_datasets}}, \code{\link{write_validation_report}}}
