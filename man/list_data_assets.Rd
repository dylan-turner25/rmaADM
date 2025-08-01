% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{list_data_assets}
\alias{list_data_assets}
\title{List asset names from the latest GitHub release}
\usage{
list_data_assets()
}
\value{
A character vector of file names (assets) in the latest release.
}
\description{
Retrieves the metadata for the most recent release of the \strong{rmaADM} repository
on GitHub and extracts the names of all attached release assets.
}
\examples{
\dontrun{
files = list_data_assets()
}
}
\keyword{internal}
