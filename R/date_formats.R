#' @title Convert datetime values output by iFormBuilder
#'
#' @description Date and time values output from iFormBuilder can be difficult
#'   to work with. The output will typically look like:
#'   \code{"2014-11-13T15:04:00+00.00"}. The \code{idate_time()} function
#'   converts datetime outputs to standard format \code{"2014-10-13 08:04:03"}
#'   taking timezone into consideration.
#'
#' @rdname idate_time
#' @param dts A vector of datetimes as string values
#' @param timezone A character string in standard format specifying timezone.
#'   Use \code{OlsonNames()} for a list of standard time zone character strings.
#' @seealso \code{\link{Sys.timezone}} for timezones
#' @examples
#' # Create vector that straddles change in daylight savings time
#' # Daylight savings time occurred: 2017-03-12 02:00:00
#' dts = c("2017-03-12T01:10:00+00:00", "", NA, "2017-03-12T02:10:00+00:00")
#'
#' # Convert to standard format
#' idate_time(dts)
#' @export
idate_time <- function (dts, timezone = "America/Los_Angeles") {
  gmt = character(length(dts))
  for( i in 1:length(dts)) {
    if (is.na(dts[i]) | dts[i] == "") {
      gmt[i] = as.character(NA)
    } else if (nchar(dts[i]) == 25 &
               substr(dts[i], nchar(dts[i]) - 5, nchar(dts[i])) == "+00:00") {
      # Create gmt time value
      gmt[i] <- paste0(substr(dts[i], 1, 10), " ", substr(dts[i], 12, 19))
      pmt <-  as.POSIXct(gmt[i], tz = "GMT")
      pmt <-  lubridate::with_tz(pmt, timezone)
      gmt[i] <-  format(pmt, format = "%Y-%m-%d %H:%M:%S")
    } else {
      stop("Date or time values are in an unexpected format")
    }
  }
  gmt
}

