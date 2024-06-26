% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.r
\name{mediansAll}
\alias{mediansAll}
\title{Calculate Median Differences in Means, Variances, and Correlation Matrices}
\usage{
mediansAll(dataframelist)
}
\arguments{
\item{dataframelist}{A list of data frames to compare.}
}
\description{
Calculate median differences in means, variances, and correlation matrices between data frames in a list.
}
\examples{
\dontrun{
# Example usage:
data_list <- list(reference_data, data_frame1, data_frame2)
mediansAll(data_list)
}
}
