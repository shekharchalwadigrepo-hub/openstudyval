\name{write_validation_report}
\alias{write_validation_report}
\title{Write an HTML or CSV validation report}
\usage{
write_validation_report(report, file)
}
\arguments{
\item{report}{An \code{osv_report} from \code{\link{validate_submission}}.}

\item{file}{Path ending in \code{.html} or \code{.csv}. An HTML report also
writes a sibling CSV.}
}
\value{The written path, invisibly.}
\description{
Writes a submission-style issue summary. The HTML layout is original and is
not a Pinnacle 21 report.
}
\examples{
report <- validate_submission(example_study("sdtm"), standard = "SDTM")
dest <- file.path(tempdir(), "osv-report.html")
write_validation_report(report, dest)
}
