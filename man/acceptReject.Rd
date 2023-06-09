% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{acceptReject}
\alias{acceptReject}
\title{Perform simple acceptance/rejection of parameter samples}
\usage{
acceptReject(parameterSamples, distances, tolerance)
}
\arguments{
\item{parameterSamples}{Matrix of parameter samples (each row represents a set of parameter values)}

\item{distances}{Euclidean distance between observed and simulated data}

\item{tolerance}{Tolerance threshold for accepting parameter samples}
}
\value{
acceptedSamples Matrix of accepted parameter samples

weights Vector of weights assigned to accepted parameter samples
}
\description{
Perform simple acceptance/rejection of parameter samples
}
