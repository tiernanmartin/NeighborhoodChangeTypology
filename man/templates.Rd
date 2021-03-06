% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/templates.R
\name{make_data_template}
\alias{make_data_template}
\alias{make_metadata_template}
\alias{make_variable_template}
\alias{make_indicator_template}
\alias{make_indicator_dimension_template}
\alias{make_indicator_type_template}
\alias{make_indicator_value_template}
\title{Make Template Objects}
\usage{
make_data_template()

make_metadata_template()

make_variable_template()

make_indicator_template()

make_indicator_dimension_template()

make_indicator_type_template()

make_indicator_value_template()
}
\value{
a \code{tibble}
}
\description{
Return a \code{tibble} containing the common set of columns and
column types.
The following template objects are available:
\itemize{
\item \code{data_template}
\item \code{variable_template}
\item \code{indicator_template}
}
}
