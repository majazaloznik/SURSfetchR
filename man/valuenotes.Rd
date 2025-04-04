% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_from_px.R
\name{get_valuenotes_dimension}
\alias{get_valuenotes_dimension}
\alias{get_valuenotes_level}
\alias{get_valuenotes_unit}
\title{Family of helper functions to extract units from VALUENOTES list}
\usage{
get_valuenotes_dimension(x)

get_valuenotes_level(x)

get_valuenotes_unit(x, con, schema = "platform")
}
\arguments{
\item{x}{a character string, either the element name (for dimension and level)
or the element itself (for the unit)}
}
\value{
character string of cleaned up dimension name, dimension level label or
unit name (which is what we are after in the end).
}
\description{
Some SURS matrices have the units for individual levels saved in a list
in the VALUENOTES slot of the px. metadata. These need to be regexed outta there
with the following three functions.
}
\keyword{internal}
