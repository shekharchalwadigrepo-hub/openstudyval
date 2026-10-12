\name{rules_catalog}
\alias{rules_catalog}
\alias{explain_rule}
\alias{add_custom_rules}
\alias{severity_levels}
\title{Rule catalog helpers}
\usage{
rules_catalog(standard = NULL)

explain_rule(rule_id)

add_custom_rules(rules, catalog = rules_catalog())

severity_levels()
}
\arguments{
\item{standard}{Optional filter: \code{"SDTM"}, \code{"SEND"}, \code{"ADaM"},
\code{"XPT"}, or \code{"TRC"}.}

\item{rule_id}{Rule identifier, such as \code{"SDTM024"}.}

\item{rules}{Data frame of additional rules with the catalog columns.}

\item{catalog}{Catalog to extend. Defaults to \code{rules_catalog()}.}
}
\value{
\code{rules_catalog} and \code{explain_rule} return a data frame.
\code{add_custom_rules} returns the combined catalog.
\code{severity_levels} returns a character vector.
}
\description{
The catalog is an original implementation of publicly described expectations.
Messages are not copied from Pinnacle 21 or from CDISC conformance-rule
publications. Supported check types are \code{dataset_required},
\code{variable_required}, \code{codelist}, \code{iso8601}, \code{key_unique},
\code{one_to_one}, \code{nonmissing}, and \code{regex}.
}
\examples{
head(rules_catalog("SDTM"))
explain_rule("SDTM024")
severity_levels()
}
