\name{example_study}
\alias{example_study}
\title{Path to a bundled example study}
\usage{
example_study(which = c("sdtm", "adam"))
}
\arguments{
\item{which}{\code{"sdtm"} or \code{"adam"}.}
}
\value{Character path to the example directory.}
\description{
Example studies intentionally contain conformance issues so a first
validation run produces a non-empty report.
}
\examples{
example_study("sdtm")
}
