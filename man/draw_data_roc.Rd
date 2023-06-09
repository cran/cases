% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_data_roc.R
\name{draw_data_roc}
\alias{draw_data_roc}
\title{Generate binary data (ROC model)}
\usage{
draw_data_roc(
  n = 100,
  prev = c(0.5, 0.5),
  random = FALSE,
  m = 10,
  auc = seq(0.85, 0.95, length.out = 5),
  rho = c(0.25, 0.25),
  dist = c("normal", "exponential"),
  e = 10,
  k = 100,
  delta = 0,
  modnames = paste0("model", 1:m),
  corrplot = FALSE,
  ...
)
}
\arguments{
\item{n}{integer, total sample size}

\item{prev}{numeric, disease and healthy prevalence (adds up to 1)}

\item{random}{logical, random sampling (TRUE) or fixed prevalence (FALSE)}

\item{m}{integer, number of models}

\item{auc}{numeric, vector of AUCs of biomarkers}

\item{rho}{numeric, vector (length 2) of correlations between biomarkers}

\item{dist}{character, either "normal" or "exponential" specifying the subgroup biomarker distributions}

\item{e}{numeric, emulates better (worse) model selection quality with higher (lower) values of e}

\item{k}{integer, technical parameter which adjusts grid size}

\item{delta}{numeric, specify importance of sensitivity and specificity (default 0)}

\item{modnames}{character, model names (length m)}

\item{corrplot}{logical (default: FALSE), if TRUE do not return data but instead plot correlation
matrices for final binary data}

\item{...}{further arguments (currently unused)}
}
\value{
Generated binary dataset
}
\description{
Generate binary data (ROC model)
}
\examples{
data <- draw_data_roc()
head(data)
}
