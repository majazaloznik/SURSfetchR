% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getStructure_API.R
\name{get_field_hierarchy}
\alias{get_field_hierarchy}
\title{Extract only the field hierarchy from the full one}
\usage{
get_field_hierarchy(full_df)
}
\arguments{
\item{full_df}{data frame output from  \link[SURSfetchR]{get_full_structure}}
}
\value{
a data frame with 4 columns and a row for each field.
}
\description{
Extracts only rows describing fields from the full hierarchy.
}
