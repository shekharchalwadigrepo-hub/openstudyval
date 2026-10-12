\name{osv_report}
\alias{print.osv_report}
\alias{summary.osv_report}
\alias{print.summary.osv_report}
\alias{as.data.frame.osv_report}
\title{Validation report object}
\usage{
\method{print}{osv_report}(x, ...)

\method{summary}{osv_report}(object, ...)

\method{as.data.frame}{osv_report}(x, row.names = NULL, optional = FALSE, ...)
}
\arguments{
\item{x, object}{An \code{osv_report}.}

\item{row.names}{Ignored. Present for the \code{as.data.frame} generic.}

\item{optional}{Ignored. Present for the \code{as.data.frame} generic.}

\item{...}{Passed to further methods.}
}
\value{
\code{summary} returns a data frame of one row per rule.
\code{as.data.frame} returns the issue-level data frame.
}
\description{
An \code{osv_report} contains issue rows, a dataset inventory, the standard,
and the engine label. Severity order is Reject, Error, Warning, Note.
}
\examples{
report <- validate_submission(example_study("sdtm"), standard = "SDTM")
print(report)
as.data.frame(report)
}
