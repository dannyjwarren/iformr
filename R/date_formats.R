#' @title Convert datetime values output by iFormBuilder
#'
#' @description Date and time values output from iFormBuilder using either the
#'   API functions or the data feed function can be difficult to work with. The
#'   output may look like: \code{"2014-11-13T15:04:00+00.00"} or: \code{Fri Apr
#'   08 2016 08:20:02 GMT-0700 (PDT)}, depending on which function is used. The
#'   \code{idate_time(), and itext_time()} functions convert these datetime
#'   outputs to standard format \code{"2014-10-13 08:04:03"}, taking timezone
#'   into consideration.
#'
#' @rdname idate_time
#' @param dts A vector of datetimes as string values
#' @param timezone A character string in standard format specifying timezone.
#'   Use \code{OlsonNames()} for a list of standard time zone character strings.
#' @seealso [timezones()]
#' @examples
#' # Create vector that straddles change in daylight savings time
#' dts = c("2014-10-13T15:04:03+00:00", "", NA, "2014-11-13T15:04:03+00:00")
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

#' @title Convert datetime values output by iFormBuilder
#'
#' @description Date and time values output from iFormBuilder using either the
#'   API functions or the data feed function can be difficult to work with. The
#'   output may look like: \code{"2014-11-13T15:04:00+00.00"} or: \code{Fri Apr
#'   08 2016 08:20:02 GMT-0700 (PDT)}, depending on which function is used. The
#'   \code{idate_time(), and itext_time()} functions convert these datetime
#'   outputs to standard format \code{"2014-10-13 08:04:03"}, taking timezone
#'   into consideration.
#'
#' @rdname itext_time
#' @param dts A vector of datetimes as string values
#' @param timezone A character string in standard format specifying timezone.
#'   Use \code{OlsonNames()} for a list of standard time zone character strings.
#' @param create_tz A character string in standard format specifying timezone.
#' @seealso [timezones()]
#' @examples
#' # Create vector that straddles change in daylight savings time
#' dts = c("", NA, "Fri Apr 08 2016 08:20:02 GMT-0700 (PDT)",
#'         "Fri Feb 08 2016 08:20:02 GMT-0700 (PST)")
#'
#' # Convert to standard format
#' itext_time(dts)
#' @export
itext_time <- function (dts, create_tz = "America/Los_Angeles", timezone = "America/Los_Angeles") {
  gmt = character(length(dts))
  for( i in 1:length(dts)) {
    if (is.na(dts[i]) | dts[i] == "") {
      gmt[i] = as.character(NA)
    } else if (nchar(dts[i]) > 25 & stringi::stri_detect_fixed(dts[i], "GMT") == TRUE) {
      # Create gmt time value
      gdt <-  format(as.Date(substr(dts[i], 5, 15), format = "%b %d %Y"))
      gmt[i] <- paste0(gdt, " ", substr(dts[i], 17, 24))
      pmt <-  as.POSIXct(gmt[i], tz = create_tz)
      pmt <-  lubridate::with_tz(pmt, timezone)
      gmt[i] <-  format(pmt, format = "%Y-%m-%d %H:%M:%S")
    } else {
      stop("Date or time values are in an unexpected format")
    }
  }
  gmt
}
