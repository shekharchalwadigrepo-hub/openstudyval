\name{read_study_data}
\alias{read_study_data}
\title{Read a study-data folder}
\usage{
read_study_data(path)
}
\arguments{
\item{path}{Directory containing study datasets, or a character vector of
file paths.}
}
\value{
A named list of data frames. Each element has a \code{source_file}
attribute. Names are the upper-case file stem.
}
\description{
CSV and RDS files are read with base R. SAS transport files (\code{.xpt})
are read with \pkg{haven} when that package is installed.
}
\examples{
study <- read_study_data(example_study("sdtm"))
names(study)
}
