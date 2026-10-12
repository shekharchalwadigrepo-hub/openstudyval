\name{validate_datasets}
\alias{validate_datasets}
\title{Validate datasets already in memory}
\usage{
validate_datasets(
  datasets,
  standard = c("SDTM", "SEND", "ADaM"),
  define = NULL,
  rules = NULL,
  ct = NULL,
  version_label = "openstudyval 0.1.0",
  path = NULL
)
}
\arguments{
\item{datasets}{Named list of data frames, as returned by
\code{\link{read_study_data}}.}

\item{standard}{One of \code{"SDTM"}, \code{"SEND"}, or \code{"ADaM"}.}

\item{define}{Optional define.xml path.}

\item{rules}{Optional extra rules.}

\item{ct}{Optional controlled terminology data frame.}

\item{version_label}{Report label.}

\item{path}{Original folder, used only for technical-rejection file checks.}
}
\value{An object of class \code{osv_report}.}
\description{
Same engine as \code{\link{validate_submission}} for data already loaded.
}
\examples{
study <- read_study_data(example_study("adam"))
validate_datasets(study, standard = "ADaM")
}
