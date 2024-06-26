% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/privacy.r
\name{prediction_all_list}
\alias{prediction_all_list}
\title{Prediction All Measures list version}
\usage{
prediction_all_list(datalist, k, dist = euc_dist)
}
\arguments{
\item{datalist}{A list containing original and anonymous data}

\item{k}{Number of kth-nearest records to consider.}

\item{dist}{Distance function to use (default: euc_dist).}
}
\value{
A list containing two sub-lists:
  - original_list: Contains the measures for the original_data without a anon_data.
  - anon_list: Contains the measures for the original_data with the provided anon_data.
}
\description{
Wrapper function to calculate all the measures for anonymous and original data,
store them into a list in the order of prediction distance,
prediction ambiguity, and prediction uncertainty,
and return the list.
}
\examples{
\dontrun{
original_data <- as.data.frame(matrix(1:6, ncol = 2))
anon_data <- as.data.frame(matrix(7:12, ncol = 2))
prediction_all(original_data, k = 2, anon_data)
}
}
