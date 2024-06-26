% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.r
\name{rlaplace}
\alias{rlaplace}
\title{Generate random numbers from a Laplace distribution.}
\usage{
rlaplace(n, location, scale)
}
\arguments{
\item{n}{The number of random samples to generate.}

\item{location}{The location parameter (mean) of the Laplace distribution.}

\item{scale}{The scale parameter (spread) of the Laplace distribution.}
}
\value{
A numeric vector containing \code{n} random numbers following
the Laplace distribution.
}
\description{
This function generates random numbers from a Laplace distribution
with given location and scale parameters.
}
\examples{
rlaplace(10, 0, 1)

}
