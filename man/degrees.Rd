% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ratios.R
\name{degrees}
\alias{degrees}
\title{Convert proportions to angles}
\usage{
degrees(h, w = 1)
}
\arguments{
\item{h, w}{pairwise height & width vectors representing input proportions}
}
\value{
Returns a vector of angles in degrees
}
\description{
This trigonometric helper calculates the angle of the diagonal for a
rectangle of given dimensions.
}
\examples{
degrees(0, 1)
degrees(1, 1)
degrees(1, 0)
}
