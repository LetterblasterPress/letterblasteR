% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dev.R
\name{inst}
\alias{inst}
\title{Find paths to package files (internal function)}
\usage{
inst(...)
}
\arguments{
\item{...}{character vectors, specifying subdirectory and file(s) within this
package. The default, none, returns the root of the package. Wildcards are
not supported.}
}
\value{
Returns a character vector of file paths that matched \code{...} or an
empty string if none matched.
}
\description{
R packages may include arbitrary files such as drivers and templates that are
installed alongside the code itself. This function is a simple wrapper to
\code{\link[base:system.file]{base::system.file()}} to find paths for this package.
}
\examples{
# Where is the custom dictionary for this installation?
letterblasteR:::inst("WORDLIST")
}
