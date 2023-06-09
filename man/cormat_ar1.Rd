% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cormat.R
\name{cormat_ar1}
\alias{cormat_ar1}
\title{Create an AR(1) correlation matrix}
\usage{
cormat_ar1(m, rho, d = TRUE)
}
\arguments{
\item{m}{integer, dimension}

\item{rho}{numeric, correlation parameter in (0,1)}

\item{d}{binary vector of length m, whereby TRUE/FALSE (alternatively 1/0)
indicate active/inactive components of underlying random vector.}
}
\value{
\eqn{R_{ij} = \rho^{|i-j|}}
}
\description{
Create an AR(1) correlation matrix
}
