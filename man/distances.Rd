% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/privacy.r
\name{distances}
\alias{distances}
\title{Calculate the distances between rows of two datasets.}
\usage{
distances(original_data, anon_data = NULL, dist = euc_dist)
}
\arguments{
\item{original_data}{A data frame representing the original dataset.}

\item{anon_data}{A data frame representing the reference dataset.}

\item{dist}{A function that calculates the distance between two rows of a data frame.}
}
\value{
A numeric vector containing the minimum distances between rows of the
        \code{original_data} and \code{anon_data}.
}
\description{
This function calculates the distances between each row of the
\code{original_data} and every row of the \code{anon_data} using a specified
distance function (\code{dist}). The datasets are first one-hot encoded to enable
distance calculations. The function then scales the datasets to have mean 0 and
standard deviation 1. If anon_data is NULL, calculate the distance between each row
of the \code{original_data} against the rest of the \code{original_data}. Otherwise
the function calculates the distance between
each row of the \code{original_data} against every row of the \code{anon_data}.
}
