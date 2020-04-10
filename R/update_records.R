#' Update records (up to 100)
#'
#' Updates up to 100 records in an iFormBuilder page.
#'
#' @rdname update_records
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param page_id Integer ID of the page to perform the update on.
#' @param access_token Access token produced by
#' \code{\link{get_iform_access_token}}
#' @param record_data A dataframe containing the update data.The dataframe must
#' contain columns for the fields to update, along with an 'id' column
#' containing the record IDs to update.
#' @return The updated record ID or IDs.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'}
#' @export
#' @import httr
update_records <- function(server_name, profile_id, page_id, access_token,
                           record_data) {
  stop("Update functionality has not yet been implemented.")

  page_id = 3681833


  # Check number of update records
  if (nrow(record_data) > 100) {
    message(sprintf("update_records is limited to 100 records per call but %s
                    records were provided. The update data will be truncated to
                    100 records. If using the sync_table function, calling the
                    function multiple times will push all changed data for
                    update", nrow(record_data)))
    record_data <- record_data[1:100,]
  }
  # Convert record field names to be IFB compliant
  format_name_not_id <- function(x) {
    ifelse(x == "id", x, format_name(x))
  }
  names(record_data) <- sapply(names(record_data), format_name_not_id)
  # Url and bearer for query to create new records
  up_records_url <- paste0(api_v60_url(server_name = server_name),
                           profile_id, "/pages/", page_id,
                           "/records?fields=&limit=100&offset=0")
  bearer <- paste0("Bearer ", access_token)
  # Field names in update data
  up_flds <- names(record_data)
  record_data[is.na(record_data)] <- ""
  # Get list of elements in existing page, default fields plus Label
  ifb_flds <- retrieve_element_list(server_name, profile_id, access_token,
                                    page_id, fields = 'label')
  # Fields in page
  ifb_flds <- ifb_flds$name
  # Fields in source table also in IFB
  fields <- intersect(up_flds, ifb_flds)
  fields <- c(fields, "id")
  # Remove columns from update record data not in IFB data
  record_data <- record_data[ , (names(record_data) %in% fields)]
  # Check that ID column is in incoming data
  if (!("id" %in% colnames(record_data))) {
    stop("Input record data does not contain a record id field.")
  }
  # Check that ID is numeric
  if (class(record_data$id) != 'numeric') {
    stop("Input data has non-numeric id column.")}
  # Check that ID is unique and non-null in incoming data
  if (any(is.na(record_data$id))) {
    stop("Input data has NA values in id column")}
  if ("" %in% record_data$id) {
    stop("Input data has blank values in id column.")}
  if (any(duplicated(record_data$id))) {
    stop("Input data has duplicate values in id column")}
  ### Convert record data to JSON - need to create a JSON array that looks like
  ### this for each record:
  #[
  # {
  #   'id': 4,
  #   'fields': [
  #     {
  #       'element_name': 'comments',
  #       'value': 'no picture was taken'
  #     }
  #     ]
  # }, etc]
  # Array to hold record values; each item is a list
  value_array = c()
  # For each value record, create the nested structure
  for (row in 1:nrow(record_data)) {
    # Blank list for record
    item <- list()
    # ID of the record
    item$id <- record_data$id[row]
    # Subset to field values per record not including id
    values <- list(record_data[row, colnames(record_data) != "id"])
    # Pivot to key value structure
    values_df <- lapply(values, function(x){tidyr::gather(x,
                                                          key = "element_name",
                                                          value = "value", na.rm = T)})
    item$fields <- values_df
    # Append to array
    value_array = c(value_array, item)
  }
  # Reassign list item names to 'fields'
  #names(values) <- rep_len("fields", nrow(record_data))

  # Convert to JSON
  json <- jsonlite::toJSON(value_array, pretty=T)
  message(json)
  # Execute API call to insert records
  r <- httr::PUT(url = up_records_url,
                 httr::add_headers('Authorization' = bearer),
                 body = values,
                 encode = "json")
  httr::stop_for_status(r)
  response <- httr::content(r, type = "application/json")
  return(response)
}
