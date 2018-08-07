#' Add element to page.
#'
#' Adds a new element to a page.
#'
#' @rdname create_element
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{iformr::get_iform_access_token}
#' @param page_id Page ID where the new element will be created.
#' @param name String of the element DCN. The provided name will be converted to be
#'   IFB database compliant (no special characters, all lowercase, spaces replaced
#'   with underscores.)
#' @param label Label for the element.
#' @param description Text description of the element.
#' @param data_type Integer indicating data type of the element. List of iFormBuilder
#'   data types: [https://iformbuilder.zendesk.com/hc/en-us/articles/201702880-Data-Types-and-Related-Details](https://iformbuilder.zendesk.com/hc/en-us/articles/201702880-Data-Types-and-Related-Details)
#' @param data_size *Optional* - length of the element; defaults to 100.
#' @param optionlist_id *Optional* - id of the option list to assign if a Select,
#'   Picklist, or Multi Picklist element is created.
#' @return ID of the new element.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Create element
#' new_element <- create_element(
#'   server_name = "your_server_name",
#'   profile_id = "your_profile_id",
#'   access_token = access_token,
#'   page_id = "existing_page_id",
#'   name = "new_dcn_name",
#'   label = "new_element_label",
#'   description = "This is a new element.",
#'   data_type = 1
#'   }
#' @export
create_element = function(server_name, profile_id, access_token, page_id, name, label,
                          description = "", data_type, data_size = 100, optionlist_id) {
  message(paste0("Creating element: ", name))
  create_element_url <- paste0(api_v60_url(server_name = server_name),
                               profile_id, "/pages/", page_id, "/elements")
  bearer <- paste0("Bearer ", access_token)
  if (!missingArg(optionlist_id)) {
    page_attributes <- list(name=name, label=label, description=description,
                            data_type=data_type, data_size=data_size,
                            optionlist_id=optionlist_id)
  }
  else {
    page_attributes <- list(name=name, label=label, description=description,
                            data_type=data_type, data_size=data_size)
  }
  page_attributes <- jsonlite::toJSON(page_attributes, auto_unbox = T)
  r <- httr::POST(url = create_element_url,
                  httr::add_headers('Authorization' = bearer),
                  body = page_attributes,
                  encode = "json")
  try(httr::stop_for_status(r))
  elementid <- httr::content(r, type = "application/json")
}

#' Retrieve a List of Elements
#'
#' Retrieves a list of all the elements contained in a page.
#'
#' @rdname retrieve_element_list
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{iformr::get_iform_access_token}
#' @param page_id Page ID where the new element will be created.
#' @param fields *Optional* - Defaults to 'all', which returns all fields for each
#'   element. Optionally, a character vector of fields to return can be provided.
#' @return Dataframe containing a row for each element and a column for each
#'   element field.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Get list of elements in form
#' elements <- retrieve_element_list(
#'   server_name = "your_server_name",
#'   profile_id = "your_profile_id",
#'   access_token = access_token,
#'   page_id = "existing_page_id",
#'   fields = 'all'
#'   }
#' @export
retrieve_element_list <- function(server_name, profile_id, access_token,
                                  page_id, fields = 'all') {
  message(paste0("Retrieving elements from: ", page_id))
  # List of fields API call can return
  all_fields <- c('global_id', 'version', 'label', 'description', 'data_type',
                  'data_size', 'created_date', 'created_by', 'modified_date',
                  'modified_by', 'widget_type', 'sort_order', 'optionlist_id',
                  'default_value', 'low_value', 'high_value', 'dynamic_value',
                  'is_required', 'condition_value', 'client_validation',
                  'is_disabled', 'reference_id_1', 'attachment_link',
                  'is_readonly', 'validation_message', 'smart_tbl_search',
                  'smart_tbl_search_col', 'is_encrypt', 'is_hide_typing',
                  'keyboard_type', 'dynamic_label', 'weighted_score',
                  'localizations')
  if (fields == 'all') {fields <- all_fields}
  else {
    # Field vector to lower
    fields <- sapply(fields, tolower)
    # Only include fields having a match in all fields
    fields <- intersect(fields, all_fields)
  }
  #field vector to string for url
  fields <- paste(fields, collapse = ',')
  fields <- paste0(fields, '%3A%3E')

  url <- paste0(api_v60_url(server_name = server_name),
                profile_id, "/pages/", page_id, "/elements?fields=",
                fields, "&limit=100&offset=0")
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  if (r$status_code != 200){stop('Could not retrieve elements from page.')}
  json <- httr::content(r, type = "text")
  elements <- jsonlite::fromJSON(json)
}
