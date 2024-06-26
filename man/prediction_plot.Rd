% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/privacy.r
\name{prediction_plot}
\alias{prediction_plot}
\title{Prediction Plot}
\usage{
prediction_plot(original_data, k, anon_data, n = 1000, dist = euc_dist)
}
\arguments{
\item{original_data}{The original data}

\item{k}{The amount of neighbouring points to consider (recommended range: from 5 to 10)}

\item{anon_data}{The anonymized data}

\item{n}{The number of points to plot (default: 1000)}

\item{dist}{The distance measure to use. (Defaults to eucledian distance)}
}
\description{
Plot the measures of predictive disclosure risk in an nonoverlapping sample against the measures of the same type in the anonymous data.
}
\examples{
\dontrun{
original_data <- as.data.frame(matrix(1:6, ncol = 2))
anon_data <- as.data.frame(matrix(7:12, ncol = 2))
prediction_plot(original_data, k = 2, anon_data)
}
}
