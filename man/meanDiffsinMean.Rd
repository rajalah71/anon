% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/similarity.R
\name{meanDiffsinMean}
\alias{meanDiffsinMean}
\title{Mean Difference in Column Means}
\usage{
meanDiffsinMean(dataframelist)
}
\arguments{
\item{dataframelist}{A list of data frame lists. Each item contains a list of data frames; an original data, anonymized data and test data.}
}
\value{
A list containing two elements: "Mean" (mean difference in column means for all dataframes) and "Sd" (standard deviations of column means).
}
\description{
Calculate the mean difference in column means between data frames in a list, each scaled by a reference data frame.
}
