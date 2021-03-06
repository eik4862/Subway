% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{future}
\alias{future}
\title{Future forecasting procedure}
\usage{
future(
  fit,
  ndays = 30,
  method = c("one-shot", "one-step"),
  ...,
  real = NULL,
  plot = TRUE
)
}
\arguments{
\item{fit}{Fitted ARIMA model(\code{ARIMA} object) which will be used for forecasting.}

\item{ndays}{The number of days to be forecasted. (default = 30)}

\item{method}{Method of prediction.
Should be one of \code{"one-shot"} or \code{"one-step"}. (default = "one-shot")}

\item{...}{Extra parameters for SARIMA fitting.
Refer to \code{\link{Arima}}.}

\item{real}{Realizations of future values.
This parameter is mandatory for one-step forecasting, while it is only optional for one-shot forecasting.
Also, for one-step forecasting, the number of realized future values must be greater than \code{ndays}.
It should be either \code{ts} object or \code{numeric} vector. (default = NULL)}

\item{plot}{If \code{TRUE}, the predicted future values will be plotted with 95\% and 80\% CI,
some of the past observations, and future realizations if given.
Otherwise, not. (default = TRUE)}
}
\value{
A list containing the following elements:\tabular{ll}{
  \code{mean} \tab Predicted future values.\cr
  \tab\cr
  \code{CI} \tab Data frame holding lower and upper bounds of 80\% and 95\% CI.\cr
  \tab\cr
  \code{statistics} \tab A numeric vector holding prediction diagnostics,
  RMSE(Root Mean Square Error) and MAE(Mean Absolute Error).
  If the number of predicted future values and the number of given future realizations differ,
  it just ignores extra information in the longer one. (Returned only if \code{real} is given.)\cr
  \tab\cr
  \code{plot} \tab \code{ggplot} object of the plot. (Returned only if \code{plot} is TRUE.)\cr
}
}
\description{
Take fitted ARIMA model and forecast future values.
If the realization of future values are given, it also computes prediction diagnostics.
For prediction, it supports two methods:
One-shot prediction predicts all future values only using the given model.
One-step prediction predicts one step ahead and refit the model with the realized future value for the next prediction.
As noticed, for one-step prediction user must provide realized future values.
}
\examples{
# 228 is the station code for SNU
# We want to predict the last 100 total count of passengers
data <- get.subway(228)
obs <- data$total[1:(nrow(data) - 100)]
real <- data$total[(nrow(data) - 99):nrow(data)]

# Fit SARIMA model
fit <- auto.arima(ts(obs, frequency = 7))

# One-shot prediction
future(fit, ndays = 100, method = "one-shot", real = real)

# One-step prediction
future(fit, ndays = 100, method = "one-step", real = real)
}
\author{
Sanghyun Park, Daun Jeong, and Sehun Kim
}
