% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sumeth_multi.R
\name{sumeth_multi}
\alias{sumeth_multi}
\title{Summarizes the results of multivariate meta-analyses conducted with imputed datasets}
\usage{
sumeth_multi(Q_mat, U_list, method)
}
\arguments{
\item{Q_mat}{A matrix (m × p) of estimates from m imputed datasets and p outcomes}

\item{U_list}{A list of m covariance matrices (p × p) for each imputation}

\item{method}{A character string indicating the imputation method used (for reporting)}
}
\value{
A data.frame with pooled estimates, SEs, 95% CIs, and outcome labels
}
\description{
Analyzes the results of multiple imputed datasets by using Rubin's Rules.
}
