#' Weekly average NO2 and PM2.5 data
#'
#' Weekly average NO2 and PM2.5  data for 1000 hypothetical births.
#'
#' @docType data
#'
#' @usage data(AirPollWeekly)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#' @details PM2.5 data has column names PM_1, ..., PM_37 and NO2 data has column names NO2_1, ..., NO2_37.
#'
#'
#' @examples
#' data(AirPollWeekly)
#' matplot(t(AirPollWeekly[1:5,-c(1:2)]),type="l")
"AirPollWeekly"