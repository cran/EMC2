% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3_funcs.R
\name{get_prior.emc}
\alias{get_prior.emc}
\alias{get_prior}
\title{Get Prior}
\usage{
\method{get_prior}{emc}(emc)

get_prior(emc)
}
\arguments{
\item{emc}{an emc object}
}
\value{
A prior with class emc.prior
}
\description{
Extracts prior from an emc object
}
\examples{
get_prior(samples_LNR)
}
