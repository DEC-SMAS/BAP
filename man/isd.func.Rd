% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isd.R
\name{isd.func}
\alias{isd.func}
\title{ISD Base Function}
\usage{
isd.func(isd.df, comm.df, isd.model)
}
\arguments{
\item{isd.df}{= a data frame with the ISD models.}

\item{comm.df}{= a data frame of observed community percentages.}

\item{isd.model}{= the particular ISD model to compare with the
community percentages (e.g. "TOXIC").}
}
\value{
The percentage of similarity the observed community has
to specific model community.
}
\description{
ISD Base Function
}
