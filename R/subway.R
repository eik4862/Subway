#' Passenger count data of Seoul subway
#'
#' A dataset containing the number of passengers who get on and off
#' from Seoul subway.
#' Data is collected automatically by credit card DB system at each station
#' and recorded daily from 1/1/2015 to 30/4/2020.
#'
#' @format A data frame with 1111007 rows and 6 variables:
#' \describe{
#'   \item{date}{recorded date}
#'   \item{code}{station code}
#'   \item{on}{number of passengers who get on the subway}
#'   \item{off}{number of passengers who get off the subway}
#'   \item{total}{total number of passengers who get on and off the subway, sum of \code{on} and \code{off}}
#'   \item{diff}{difference in the number of passengers who get on and off the subway, \code{on} - \code{off}}
#' }
#' @source \url{https://data.seoul.go.kr/dataList/OA-12914/S/1/datasetView.do#}
"subway"
