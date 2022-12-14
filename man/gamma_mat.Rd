% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variance.R
\name{gamma_mat}
\alias{gamma_mat}
\title{Gamma matrix}
\usage{
gamma_mat(x, sigma, type = "adf")
}
\arguments{
\item{x}{A numeric matrix of observations.}

\item{sigma}{Covariance matrix of the data.}

\item{type}{One of \code{adf}, \code{normal} and \code{elliptical}.}
}
\value{
The sample estimate of the gamma matrix.
}
\description{
Calculate the gamma matrix from a matrix of observations.
}
\keyword{internal}
