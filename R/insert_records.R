#' Create new records
#'
#' Creates new records in an iFormBuilder page.
#'
#' @rdname create_new_records
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param page_id Integer ID of the page to insert new records.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @param record_data A dataframe containing the data to be added to the page.
#' @return The created record ID or IDs.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'}
#' @export
#' @import tidyr
create_new_records <- function(server_name, profile_id, page_id, access_token, record_data) {
  # Url and bearer for query to create new records
  new_records_url <- paste0(api_v60_url(server_name = server_name),
                            profile_id, "/pages/", page_id, "/records")
  bearer <- paste0("Bearer ", access_token)
  # Field names in new data
  new_flds <- names(record_data)
  record_data[is.na(record_data)] <- ""
  # Get list of elements in existing page, default fields plus Label
  ifb_flds <- retrieve_element_list(server_name, profile_id, access_token,
                                    page_id, fields = 'label')
  # Fields in page
  ifb_flds <- ifb_flds$name
  # Fields in source table also in IFB
  fields <- intersect(new_flds, ifb_flds)
  # Remove columns from new record data not in IFB data
  record_data <- record_data[ , (names(record_data) %in% fields)]
  # Convert record data to JSON.
  # TODO BD 8/20/2018: Way to do this with JSONlite package
  #
  values = ""
  for (row in 1:nrow(record_data)){
    if (row != nrow(record_data)){
      values = paste0(values,'{"fields": [')
      for (field in 1:length(fields)){
        fldname = fields[field]
        fldvalue = record_data[row,fldname]
        if (field != length(fields)){
          values = paste0(values,'{"element_name": "',fldname,'", "value": "',fldvalue,'"},')
        }
        else {
          values = paste0(values,'{"element_name": "',fldname,'", "value": "',fldvalue,'"}]},')
        }
      }
    }
    else {
      values = paste0(values,'{"fields": [')
      for (field in 1:length(fields)){
        fldname = fields[field]
        fldvalue = record_data[row,fldname]
        if (field != length(fields)){
          values = paste0(values,'{"element_name": "',fldname,'", "value": "',fldvalue,'"},')
        }
        else {
          values = paste0(values,'{"element_name": "',fldname,'", "value": "',fldvalue,'"}]}')
        }
      }
    }
  }
  values = paste0('[',values,']')
  # Execute API call to insert records
  r <- httr::POST(url = new_records_url,
                  httr::add_headers('Authorization' = bearer),
                  body = values,
                  encode = "json")
  httr::stop_for_status(r)
  response <- httr::content(r, type = "application/json")
  return(response)
}
