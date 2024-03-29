% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_from_px.R
\name{get_px_metadata}
\alias{get_px_metadata}
\title{Get the metadata for an individual table}
\usage{
get_px_metadata(id)
}
\arguments{
\item{id}{character vector of length 1 with code of matrix. Can be with or
without the .px extension.}
}
\value{
A data.frame with eight columns and single row
}
\description{
In addition to the  \link[SURSfetchR]{get_table_levels} function, which gets the table's
dimensions and levels, this one gets some other metadata from the .px file, which are
not available to the pxweb library but use the pxR library. These are the creation date,
units and notes, which are parsed as json and some other minor things.
}
