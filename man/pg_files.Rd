% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pg.R
\name{pg_files}
\alias{pg_files}
\title{Extract file info from Project Gutenberg metadata}
\usage{
pg_files(x)
}
\arguments{
\item{x}{metadata as returned by \code{\link[=pg_metadata]{pg_metadata()}}}
}
\value{
Returns a \link[tibble:tibble-package]{tibble} with one row per file
associated with the given Project Gutenberg work.
}
\description{
Extract file info from Project Gutenberg metadata
}
