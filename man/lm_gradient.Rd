% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm_gradient.R
\name{lm_gradient}
\alias{lm_gradient}
\title{Fitting Linear Models with gradient}
\usage{
lm_gradient(
  b,
  formula,
  data = NULL,
  maxit = 1000,
  tolerance = 1e-05,
  stepsize = 1e-04,
  fun = "sd"
)
}
\arguments{
\item{b}{initial values for beta}

\item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted}

\item{data}{an optional data frame, list or environment. If not found in data, the variables are taken from environment(formula)}

\item{maxit}{number of max iterations of the algorithm}

\item{tolerance}{the algorithm will stop if tolerance is reached}

\item{stepsize}{size of each step (only for gradient descent)}

\item{fun}{function can be \code{sd} for steepest descent or \code{gd} for gradient descent}
}
\value{
the function returns an object of class \code{"gradient"}
}
\description{
\code{lm_gradient} is used to fit linear model with gradient or steepest descent.
}
