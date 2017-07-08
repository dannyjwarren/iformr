#' Stream reach breaks for spawning ground surveys.
#'
#' A dataset containing a list of stream reaches typically surveyed for
#' returning salmon in NW Washington State. The dataset is formatted as an
#' iFormBuilder segmented option list. One or more reach breaks are indicated
#' for each stream as river mile location points. Streams are indicated by the
#' unique stream_id value following the equals sign in condition_value.
#'
#' @format A data frame with 186 rows and 4 variables:
#' \describe{
#'   \item{key_value}{unique id}
#'   \item{label}{point along stream, in river mile}
#'   \item{condition_value}{stream filter, unique stream id after equals sign}
#'   \item{sort_order}{unique integer sequence, starting at zero, incrementing by one}
#' }
"locations"
