% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_instance.R
\name{process_instance}
\alias{process_instance}
\title{Analyze simulated synthetic datasets.}
\usage{
process_instance(
  instance = NULL,
  contrast = "cases::define_contrast('raw', NA)",
  benchmark = 0.5,
  alpha = 0.05,
  alternative = "greater",
  adjustment = "none",
  transformation = "none",
  analysis = "co-primary",
  regu = "c(1,1/2,1/4)",
  pars = "list()",
  ...,
  data = NULL,
  job = list(id = NA)
)
}
\arguments{
\item{instance}{(list) \cr generated via \link{generate_instance_lfc} or \link{generate_instance_roc}.}

\item{contrast}{(\code{cases_contrast}) \cr specified via \code{\link{define_contrast}}}

\item{benchmark}{(numeric) \cr value to compare against (RHS), should have same length as data
or length one if all benchmark values are identical.}

\item{alpha}{(numeric) \cr significance level (default: 0.05)}

\item{alternative}{(character) \cr specify alternative hypothesis}

\item{adjustment}{(character) \cr specify type of statistical adjustment taken to address multiplicity}

\item{transformation}{(character) \cr define transformation to ensure results
(e.g. point estimates, confidence limits) lie in unit interval ("none" (default) or "logit")}

\item{analysis}{(character) \cr  "co-primary" (default; only option currently)}

\item{regu}{(numeric | logical) \cr vector of length 3, specify type of shrinkage.
Alternatively, logical of length one (TRUE := c(2, 1, 1/2), FALSE := c(0, 0, 0))}

\item{pars}{(list) \cr further parameters given as named list}

\item{...}{(any) \cr additional named parameters}

\item{data}{(NULL) \cr ignored (for batchtools compatibility)}

\item{job}{(NULL) \cr for batchtools compatibility, do not change}
}
\value{
(list) \cr standardized evaluation results
}
\description{
Process data instances, a list of multiple datasets generated via
\link{generate_instance_lfc} or \link{generate_instance_roc}. This function
applies \link{evaluate} to all datasets.

\strong{This function is only needed for simulation via batchtools, not relevant in interactive use!}
}
\details{
Utilizes same arguments as \link{evaluate} unless mentioned otherwise above.
}
