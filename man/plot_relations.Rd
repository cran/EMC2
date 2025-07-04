% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/factor_analysis.R
\name{plot_relations}
\alias{plot_relations}
\title{Plot Group-Level Relations}
\usage{
plot_relations(
  emc = NULL,
  stage = "sample",
  plot_cred = FALSE,
  plot_means = TRUE,
  only_cred = TRUE,
  nice_names = NULL,
  selection = "correlation",
  use_par = NULL,
  ...
)
}
\arguments{
\item{emc}{An EMC2 object, commonly the output of \code{run_emc()}.}

\item{stage}{Character. The stage from which to take the samples, defaults to
the sampling stage \code{sample}.}

\item{plot_cred}{Boolean. Whether to plot the 95 percent credible intervals or not}

\item{plot_means}{Boolean. Whether to plot the means or not}

\item{only_cred}{Boolean. Whether to only plot credible values}

\item{nice_names}{Character string. Alternative names to give the parameters}

\item{selection}{Character. Whether to plot correlations or loadings}

\item{use_par}{Character. Which parameters to include. If null, includes all.}

\item{...}{Optional additional arguments}
}
\value{
No return value, creates a plot of group-level relations
}
\description{
An adjusted version of the \code{corrplot} package function \code{corrplot()} tailored
to \code{EMC2} and the plotting of estimated correlations.
}
\examples{
# For a given set of hierarchical model samples we can make a
# correlation matrix plot.
plot_relations(samples_LNR, only_cred = TRUE, plot_cred = TRUE)
# We can also only plot the correlations where the credible interval does not include zero
plot_relations(samples_LNR, plot_means = TRUE, only_cred = TRUE)

}
