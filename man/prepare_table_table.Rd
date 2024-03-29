% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_structure.R
\name{prepare_table_table}
\alias{prepare_table_table}
\title{Prepare table to insert into `table` table}
\usage{
prepare_table_table(code_no)
}
\arguments{
\item{code_no}{the matrix code (e.g. 2300123S)}
}
\value{
a dataframe with the `code`, `name`, `source_id`, `url`, and `notes` columns
for this table.
}
\description{
This one is really straightforward, and slightly superfluous, since it just
uses the \link[SURSfetchR]{get_px_metadata} function and removes two columns.
Returns table ready to insert into the `table` table with the db_writing family
of functions.
}
