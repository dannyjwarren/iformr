#' Get list of the first 100 pages (i.e., forms, or subforms) in a profile
#'
#' Sends a request to the iFormBuilder API to get a listing of forms and
#' subforms in the current profile. Returns a dataframe with form id and form
#' name. Limited to 100 records per API call.
#'
#' @rdname get_pages_list
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The id number of your profile
#' @param limit The maximum number of form ids to return
#' @param offset Skips the offset number of ids before beginning to return
#' @param access_token The access_token required to establish communication with
#'   the API
#' @return A dataframe of all forms (pages) in the given profile
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Get the id and name of all forms in profile
#' forms_list <- get_pages_list(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   access_token = access_token)
#'
#' # Inspect
#' forms_list
#' }
#' @export
get_pages_list <- function(server_name,
                           profile_id,
                           limit = 1000,
                           offset = 0,
                           access_token) {
  pages_uri <- paste0(api_v60_url(server_name = server_name),
                      profile_id, "/pages?fields=fields&limit=",
                      limit,
                      "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = pages_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  pgs <- httr::content(r, type = "application/json")
  pgsx <- integer(length(pgs))
  pgs_id <- unlist(lapply(seq_along(pgsx),
                          function(i) pgsx[i] <- pgs[[i]]$id))
  pgs_name <- unlist(lapply(seq_along(pgsx),
                            function(i) pgsx[i] <- pgs[[i]]$name))
  dplyr::data_frame(id = pgs_id, name = pgs_name)
}


#' Get list of all pages (i.e., form, or subform) in a profile
#'
#' Retrieves a list of ALL pages in a profile, in chunks of 100 (limit).
#' Returns a tibble with form id and form name.
#' @rdname get_all_pages_list
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{iformr::get_iform_access_token}
#' @return Tibble of two columns containing the page ID and page name:
#'   id <int>, name <chr>
#' @export
get_all_pages_list <- function(server_name, profile_id, access_token) {
  # Blank tibble
  page_list = dplyr::tibble(id = integer(), name = character())
  # Start looping at list 0, in chunks of 100 (limit per api call)
  offset = 0
  while (T) {
    # Get chunk of 100
    chunk = get_pages_list(server_name, profile_id,
                           limit = 100, offset = offset, access_token)
    #append to option list tibble
    for (row in 1:nrow(chunk)) {
      newid = chunk$id[row]
      newname = chunk$name[row]
      page_list = dplyr::add_row(page_list, id = newid, name = newname)
    }
    # If the chunk is less than 100 escape the loop
    if (length(chunk$id) < 100) {break}
    # Increment offset by 100
    offset = offset + 100
  }
  return(page_list)
}


#' Get id of a single page (i.e., form, or subform) given a form name
#'
#' Sends a request to the iFormBuilder API to get the id number of a single
#' form. You only need to supply the name of the option list. Returns an integer
#' id for the given form.
#'
#' @rdname get_page_id
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The id number of your profile
#' @param page_name The name of the form or subform
#' @param limit The maximum number of form ids to return
#' @param offset Skips the offset number of ids before beginning to return
#' @param access_token The access_token required to establish communication with
#'   the API
#' @return An integer id for the given form
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Get the id of a single form in the profile given the form name
#' form_id <- get_page_id(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   page_name = "spawning_ground_p",
#'   access_token = access_token)
#'
#' # Inspect the form_id
#' form_id
#' }
#' @export
get_page_id <- function(server_name,
                        profile_id,
                        page_name,
                        limit = 1000,
                        offset = 0,
                        access_token) {
  pages_uri <- paste0(api_v60_url(server_name = server_name),
                      profile_id,
                      "/pages?fields=fields&limit=", limit,
                      "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = pages_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  pgs <- httr::content(r, type = "application/json")
  pgsx <- integer(length(pgs))
  pgs_id <- unlist(lapply(seq_along(pgsx),
                          function(i) pgsx[i] <- pgs[[i]]$id))
  pgs_name <- unlist(lapply(seq_along(pgsx),
                            function(i) pgsx[i] <- pgs[[i]]$name))
  pg = dplyr::data_frame(id = pgs_id, name = pgs_name)
  pg$id[pg$name == page_name]
}

#' Get list of all record ids in a single page (i.e., form, or subform)
#'
#' Sends a request to the iFormBuilder API to get a list of all record ids in a
#' given form or subform.
#'
#' @rdname get_page_record_list
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The id number of your profile
#' @param page_id The id number of the form
#' @param limit The maximum number of form ids to return
#' @param offset Skips the offset number of ids before beginning to return
#' @param access_token The access_token required to establish communication with
#'   the API
#' @return A vector of record ids from the given form
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Get the id of a single form in the profile given the form name
#' form_id <- get_page_id(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   page_name = "your_form_p",
#'   access_token = access_token)
#'
#' # Get a list of all record ids in the specified form
#' record_ids <- get_page_record_list(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   page_id = form_id,
#'   access_token = access_token)
#'
#' # Inspect the top five record_ids
#' head(record_ids, 5)
#' }
#' @export
get_page_record_list <- function(server_name,
                                 profile_id,
                                 page_id,
                                 limit = 1000,
                                 offset = 0,
                                 access_token) {
  recordlist_uri <- paste0(api_v60_url(server_name = server_name),
                           profile_id,
                           "/pages/", page_id,
                           "/records?fields=fields&limit=", limit,
                           "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = recordlist_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  rcrd <- httr::content(r, type = "application/json")
  rcrdx <- integer(length(rcrd))
  unlist(lapply(seq_along(rcrdx),
                function(i) rcrdx[i] <- rcrd[[i]]$id))
}

#' Get a single record from a page (i.e., form, or subform)
#'
#' Sends a request to the iFormBuilder API to get a single record from a form or
#' subform given a record id.
#'
#' @rdname get_page_record
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The id number of your profile
#' @param page_id The id for the form
#' @param record_id The id for the specific record to return
#' @param access_token The access_token required to establish communication with
#'   the API
#' @return Dataframe of a single record from the given form
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Get the id of a single form in the profile given the form name
#' form_id <- get_page_id(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   page_name = "your_form_p",
#'   access_token = access_token)
#'
#' # Get a list of all record ids in the specified form
#' record_ids <- get_page_record_list(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   page_id = form_id,
#'   access_token = access_token)
#'
#' # Inspect the top five record_ids
#' head(record_ids, 5)
#'
#' # Get the first record in the list
#' single_record_id = record_ids[1]
#'
#' # Get a single record from a form or subform
#' single_form_record <- get_page_record(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   page_id = form_id,
#'   record_id = single_record_id,
#'   access_token = access_token)
#'
#' # Inspect the first five columns of the single record dataframe
#' single_form_record[,1:5]
#' }
#' @export
get_page_record <- function(server_name,
                            profile_id,
                            page_id,
                            record_id,
                            access_token) {
  pagerecord_uri <- paste0(api_v60_url(server_name = server_name),
                           profile_id,
                           "/pages/", page_id,
                           "/records/", record_id)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = pagerecord_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  rcrd <- httr::content(r, type = "application/json")
  rcru <- unlist(lapply(rcrd,
                        function(x) ifelse(is.null(x), as.character(NA), x)))
  as.data.frame(t(rcru), stringsAsFactors = FALSE)
}

#' @title Get multiple records for a set of fields in a page (i.e., form, or
#'   subform)
#'
#' @description Sends a request to the iFormBuilder API to get the first 1000
#'   records or less in a given form or subform for a specific set of fields
#'   (columns). Specify how many records to retrieve using the \code{limit}
#'   parameter. Specify how many records to skip before starting to retrieve
#'   records using the \code{offset} parameter.
#'
#' @details This will likely be the primary function used to retrieve records
#'   from the iFormBuilder API. When a set of records is downloaded using this
#'   function the retrieved data will contain the record id as the first column.
#'   By archiving these ids, each request for new records can incorporate the
#'   last downloaded id in the \code{fields} parameter to only pull newly
#'   submitted records. See example below.
#'
#' @section Warning: This function should \strong{only} be used to request
#'   records from one form or subform at a time. \strong{Do not} assume it will
#'   work if records from more than one form are incorporated in the
#'   \code{fields} parameter. If you request multiple fields but the function
#'   only returns the id value, and does not throw an error, this is a strong
#'   indication that you did not specify the fields correctly. You may have
#'   requested a field that does not exist.
#'
#' @rdname get_selected_page_records
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The id number of your profile
#' @param page_id The id of the form or subform
#' @param fields A set of data fields (columns) to return
#' @param limit The maximum number of records to return
#' @param offset Skips the offset number of records before beginning to return
#' @param access_token The access_token required to establish communication with
#'   the API
#' @return A dataframe of records for the specified fields (columns)
#' @examples
#' # Specify the fields (columns) to be returned
#' field_list <- c(
#'  "surveyors", "survey_start_datetime", "survey_method",
#'  "stream", "survey_end_time")
#'
#' # Collapse vector of column names into a single string
#' form_fields <- paste(field_list, collapse = ',')
#'
#' \dontrun{
#' # Set id to ascending order and pull only records greater than the last_id
#' since_id <- 5
#' parent_form_fields <- paste0("id:<(>\"", since_id, "\"),", form_fields)
#'
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Get the id of a single form in the profile given the form name
#' form_id <- get_page_id(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   page_name = "your_form_p",
#'   access_token = access_token)
#'
#' # Get multiple records for a set of columns from a form or subform
#' parent_form_records <- get_selected_page_records(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   page_id = form_id,
#'   fields = parent_form_fields,
#'   access_token = access_token)
#'
#' # Inspect the first three rows and first five columns of the dataframe
#' parent_form_records[1:3,1:5]
#' }
#' @export
get_selected_page_records <- function(server_name,
                                      profile_id,
                                      page_id,
                                      fields = "fields",
                                      limit = 100,
                                      offset = 0,
                                      access_token) {
  recordlist_uri <- paste0(api_v60_url(server_name = server_name),
                           profile_id,
                           "/pages/", page_id,
                           "/records?fields=", fields,
                           "&limit=", limit,
                           "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = recordlist_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  rcrd <- httr::content(r, type = "application/json")
  rcrd <- lapply(rcrd, rm_nulls)
  rcrd <- as.data.frame(do.call(rbind, lapply(rcrd, rbind)))
  rcrd <- dplyr::as_data_frame(rcrd)
  rcrd[] <- lapply(rcrd, unlist)
  rcrd
}


#' @title Get all records for a set of fields in a page (i.e., form, or
#'   subform)
#'
#' @description Sends a request to the iFormBuilder API to get all records in a
#'   given form or subform for a specific set of fields (columns). This function
#'   can be used to exceed the API limit of 1000 records per request. It will loop
#'   through chunks of 1000 records at a time until all records have been retrieved.
#'   You can also specify chunk size (< 1000) using the \code{limit} parameter.
#'   Specify how many records to skip before starting to retrieve records using
#'   the \code{offset} parameter.
#'
#' @details This function should be used with caution. It should only be used in
#'   those rare cases when you know that there are more than 1000 records that
#'   need to be retrieved in a single call. Be observant for warnings. It may
#'   on occassion throw errors.
#'
#' @section Warning: This function should \strong{only} be used to request
#'   records from one form or subform at a time. \strong{Do not} assume it will
#'   work if records from more than one form are incorporated in the
#'   \code{fields} parameter. If you request multiple fields but the function
#'   only returns the id value, and does not throw an error, this is a strong
#'   indication that you did not specify the fields correctly. You may have
#'   requested a field that does not exist.
#'
#' @rdname get_all_records
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The id number of your profile
#' @param page_id The id of the form or subform
#' @param fields A set of data fields (columns) to return
#' @param limit The maximum number of records to return
#' @param offset Skips the offset number of records before beginning to return
#' @param access_token The access_token required to establish communication
#'   with the API
#' @param field_string A character string defining the data fields to return,
#'   including the id value where data retrieval should start. See the example
#'   above for how to define the \code{parent_form_fields} string when using
#'   the \code{get_selected_page_records()} function.
#' @param since_id The id value defining the first record to retrieve.
#' @return A dataframe of records for the specified fields (columns)
#' @examples
#' # Specify the fields (columns) to be returned
#' field_list <- c("surveyors", "survey_start_datetime", "survey_method",
#'                 "stream", "survey_end_time")
#'
#' # Collapse vector of column names into a single string
#' form_fields <- paste(field_list, collapse = ',')
#'
#' \dontrun{
#' # Set id to ascending order and pull only records greater than the last_id
#' since_id <- 5
#' field_string <- paste0("id:<(>\"", since_id, "\"),", form_fields)
#'
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Get the id of a single form in the profile given the form name
#' form_id <- get_page_id(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   page_name = "your_form_p",
#'   access_token = access_token)
#'
#' # Get all existing records for a set of columns from a form or subform
#' parent_form_records <- get_all_records(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   page_id = form_id,
#'   fields = "fields",
#'   limit = 1000,
#'   offset = 0,
#'   access_token = access_token,
#'   field_string,
#'   since_id)
#' }
#' @export
get_all_records = function(server_name,
                           profile_id,
                           page_id,
                           fields = "fields",
                           limit = 1000,
                           offset = 0,
                           access_token,
                           field_string,
                           since_id) {
  since_id = since_id
  since_fields = paste0("id:<(>\"", since_id, "\"),", field_string)
  subform_records = get_selected_page_records(
    server_name = server_name,
    profile_id = profile_id,
    page_id = page_id,
    fields = since_fields,
    limit = 1000,
    access_token = access_token)
  all_records = subform_records
  while (nrow(subform_records) == 1000) {
    since_id = max(all_records$id)
    since_fields = paste0("id:<(>\"", since_id, "\"),", field_string)
    subform_records = get_selected_page_records(
      server_name = server_name,
      profile_id = profile_id,
      page_id = page_id,
      fields = since_fields,
      limit = 1000,
      access_token = access_token)
    all_records = rbind(all_records, subform_records)
  }
  all_records
}


#' Create page
#'
#' Creates a new page in the given profile with the name and label specified.
#' The name provided will be converted to iFormBuilder standards; punctuation
#' and whitespace replaced with _ and all text to lowercase.
#'
#' @rdname create_page
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{iformr::get_iform_access_token}
#' @param name String of new page name; coerced to iFormBuilder table
#'   name conventions.
#' @param label String of the label for the new page.
#' @return Integer of the new page ID.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Create new page
#' new_page_id <- create_page(
#'   server_name = "your_server_name",
#'   profile_id = "your_profile_id",
#'   access_token = access_token,
#'   name = "new_form_name",
#'   label = "New Form Label")
#'   }
#' @export
create_page = function(server_name, profile_id, access_token, name, label) {
  # Remove whitespace, punctuation, etc from name
  name <- tolower(gsub('([[:punct:]])|\\s+','_',name))
  message(paste0("Creating page: ", name))
  create_page_url <- paste0(api_v60_url(server_name = server_name),
                            profile_id, "/pages")
  bearer <- paste0("Bearer ", access_token)
  page_attributes = paste0('{"name": "',name,'", "label": "',label,'"}')
  r <- httr::POST(url = create_page_url,
                  httr::add_headers('Authorization' = bearer),
                  body = page_attributes,
                  encode = "json")
  httr::stop_for_status(r)
  page_id <- httr::content(r, type = "application/json")
  return(page_id$id)
}

#' Copy page
#'
#' Copies a page to a new page in the profile.
#' @rdname copy_page
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{iformr::get_iform_access_token}
#' @param page_id Integer of the page ID to copy.
#' @return Integer of the new page ID.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Copy page
#' new_page_id <- copy_page(
#'   server_name = "your_server_name",
#'   profile_id = "your_profile_id",
#'   access_token = access_token,
#'   page_id = "existing_page_id"
#'   }
#' @export
copy_page = function(server_name, profile_id, access_token, page_id) {
  copy_page_url <- paste0(api_v60_url(server_name = server_name),
                          profile_id, "/pages/", page_id)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::VERB(verb="COPY", url = copy_page_url,
                  httr::add_headers('Authorization' = bearer),
                  encode = "json")
  httr::stop_for_status(r)
  new_page_id <- httr::content(r, type = "application/json")
  return(new_page_id$id)
}

#' Rename page
#'
#' Renames a page given a page_id and new name and label. The name provided
#' will be converted to iFormBuilder standards; punctuation and whitespace
#' replaced with _ and all text to lowercase.
#'
#' @rdname rename_page
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{iformr::get_iform_access_token}
#' @param page_id Integer of the page ID to rename.
#' @param name String of renamed page name; coerced to iFormBuilder
#'   table name conventions.
#' @param label String of the renamed page label.
#' @return Integer of the page ID.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Rename page
#' rename_page_id <- rename_page(
#'   server_name = "your_server_name",
#'   profile_id = "your_profile_id",
#'   access_token = access_token,
#'   page_id = "existing_page_id",
#'   name = "new_page_name",
#'   label = "new_page_label"
#'   }
#' @export
rename_page = function(server_name, profile_id, access_token,
                       page_id, name, label) {
  # Remove whitespace, punctuation, etc from name
  name <- tolower(gsub('([[:punct:]])|\\s+','_',name))
  rename_page_url <- paste0(api_v60_url(server_name = server_name),
                            profile_id, "/pages/", page_id)
  bearer <- paste0("Bearer ", access_token)
  page_attributes = paste0('{"name": "',name,'", "label": "',label,'"}')
  r <- httr::PUT(url = rename_page_url,
                 httr::add_headers('Authorization' = bearer),
                 body = page_attributes,
                 encode = "json")
  httr::stop_for_status(r)
  returned_page_id <- httr::content(r, type = "application/json")
  return(returned_page_id$id)
}


#' Compose a url to get data via the data feed mechanism
#'
#' Creates a url with username and password embedded that can be used to
#' download data using the data-feed mechanism instead of the API. In general,
#' this should be avoided, as the API mechanisms are much safer. Returns a json
#' file with all records submitted since the specified \code{since_id}.
#'
#' @rdname data_feed_url
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param parent_form_id The id of the parent form
#' @param profile_id The id number of your profile
#' @param parent_form_name The name of the parent form
#' @param since_id The record id indicating where to start downloading
#' @param user_label The name given to the username in the .Renviron file
#' @param pw_label Skips the offset number of records before beginning
#'   to return records
#' @return A url that can be used to request form data
#' @examples
#' \dontrun{
#' # Generate a url to retrieve data via the data feed mechanism
#' url <- data_feed_url(
#'   server_name = "your_server_name",
#'   parent_form_id = 456789,
#'   profile_id = 123456,
#'   parent_form_name = "spawning_ground_p",
#'   since_id = 3,
#'   user_label = "your_user_label",
#'   pw_label = "your_pw_label")
#'
#' # Retrieve the form data into an R list
#' form_data <- jsonlite::fromJSON(url)
#' }
#' @export
data_feed_url = function(server_name,
                         parent_form_id,
                         profile_id,
                         parent_form_name,
                         since_id,
                         user_label,
                         pw_label) {
  paste0(base_url(server_name), "/exzact/dataScoringJSON.php?",
         "PAGE_ID=", parent_form_id, "&",
         "TABLE_NAME=_data", profile_id, "_", parent_form_name, "&",
         "SINCE_ID=", since_id,"&",
         "USERNAME=", iform_user(user_label), "&",
         "PASSWORD=", iform_pw(pw_label))
}

# Get username from the .Renviron file
# Only needed to get username when downloading data using the data feed
# mechanism. Avoid if possible. It is much safer to use the API.
iform_user <- function(user_label) {
  Sys.getenv(user_label)
}

# Get password from the .Renviron file
iform_pw <- function(pw_label) {
  Sys.getenv(pw_label)
}

# Remove nulls from elements in a list
rm_nulls <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}


#' Create form from dataframe
#'
#' Creates a form based on a dataframe. Dataframe classes are cast as
#' element types in the form.
#'
#' @rdname data2form
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{iformr::get_iform_access_token}
#' @param name String of new page name; coerced to iFormBuilder
#'   table name conventions.
#' @param label String of the label for the new page.
#' @param data A dataframe whose structure will be used to
#'   create the new form.
#' @return The page ID of the created form.
#' @examples
#' # Create a dataframe with some basic form fields
#' dat = tibble::tibble(survey_id = NA_integer_,
#'                      survey_datetime = as.POSIXct(NA, tz = "UTC"),
#'                      surveyor = NA_character_,
#'                      start_point = NA_real_,
#'                      fish_species = NA_integer_,
#'                      fish_count = NA_integer_,
#'                      end_point = NA_real_,
#'                      comment_text = NA_character_,
#'                      survey_completed = TRUE)
#'
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Create new form from dataframe
#' new_form <- data2form(
#'   server_name = "your_server_name",
#'   profile_id = "your_profile_id",
#'   access_token = access_token,
#'   name = "new_form_to_create",
#'   label = "New form based on an R dataframe",
#'   data = dat)
#' }
#' @export
data2form = function(server_name, profile_id, access_token,
                     name, label, data) {
  # Remove whitespace, punctuation, etc from name
  name <- tolower(gsub('([[:punct:]])|\\s+','_', name))
  # Create empty form
  page_id <- create_page(server_name, profile_id, access_token, name, label)
  # Get field classes of input data
  field_classes <- sapply(data, class)
  # List mapping data classes to IFB element types
  ifb_types <- list("character" = 1, "numeric" = 2, "integer" = 2,
                    "double" = 2, "POSIXct" = 5, "logical" = 6)
  # For each field in input data
  for (field in names(field_classes)) {
    # Class of field
    class <- field_classes[[field]][1]
    # ifb element type for field
    data_type <- ifb_types[[class]]
    # Label as proper case
    label <- gsub('_',' ', field)
    label <- stringr::str_to_title(label)
    # Add element to page
    create_element(server_name, profile_id, access_token, page_id,
                   name=field, label, description="", data_type)
  }
  return(page_id)
}

#' Create new records
#'
#' Creates new records in an iFormBuilder page. The page must already
#' contain at least one record.
#'
#' @rdname create_new_records
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param page_id Integer ID of the page to insert new records.
#' @param access_token Access token produced by \code{iformr::get_iform_access_token}
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
create_new_records <- function(server_name, profile_id, page_id, access_token, record_data) {
  # Url and bearer for query to create new records
  new_records_url <- paste0(api_v60_url(server_name = server_name),
                            profile_id, "/pages/", page_id, "/records")
  bearer <- paste0("Bearer ", access_token)
  # Field names in new data
  new_flds <- names(record_data)
  record_data[is.na(record_data)] <- ""
  # Get list of elements in existing page
  ifb_flds <- iformr::retrieve_element_list(server_name, profile_id, access_token,
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


#' Sync table
#'
#' Syncs a dataframe with the contents of an IFB page. An example use case
#' is syncing an existing database table with a Smart Table Search form
#' in IFB. TODO: Add support for data column comparison to update records.
#'
#' @rdname sync_table
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{iformr::get_iform_access_token}
#' @param data A dataframe containing the data to be synced with the page.
#' @param form_name The name of a page to sync the source data to; if the page does
#' not exist, it will be created.
#' @param label *Optional* - String of the label to be used if a new page is created.
#' If a label is not provided and a new page is created, the form_name argument will
#' be used to create a page label.
#' @param uid The name of the column in the source and IFB data that uniquely identifies
#' a record.
#' @return The page ID of the existing or created form.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Add new data to form
#' sync_table(server_name, profile_id, access_token,
#'   data = "new_dataframe", form_name = "my_form",
#'   uid = "unique_id_col")
#' }
#' @export
sync_table <- function(server_name, profile_id, access_token,
                       data, form_name, label, uid){
  # Convert the dataframe to be Smart Table search friendly -
  # Date fields to UNIX epoch string, everything else to string
  for (fld in names(data)) {
    # Class of field
    class <- data[[fld]][1]
    if (identical(class,"POSIXct")) {
      data[[fld]] <- as.numeric(data[[fld]])
    }
    else {
      data[[fld]] <- as.character(data[[fld]])
    }
  }
  # Get a list of all the pages in the profile
  page_list <- iformr::get_all_pages_list(server_name, profile_id, access_token)
  # Remove whitespace, punctuation, etc from name
  form_name <- tolower(gsub('([[:punct:]])|\\s+','_', form_name))
  # If the form does not exist, create it
  if (form_name %in% page_list$name == F){
    message("Form ",form_name," does not yet exist.")
    # If form label has not been provided, create a label from the form name
    if (methods::missingArg(label)) {
      label <- stringr::str_to_title(gsub('_',' ', form_name))
    }
    # Create page for table data
    page_id <- iformr::data2form(server_name, profile_id, access_token,
                                 name = form_name, label, data)
  }
  else {
    # Get id of existing page
    page_id <- page_list[page_list$name == form_name ,]$id
  }
  # If something goes wrong with request
  stopifnot(page_id > 0)
  # Field names of input table to lowercase to match IFB
  names(data) <- tolower(names(data))
  # Fields names in source table
  src_flds <- names(data)
  # Get list of elements in existing page
  ifb_flds <- iformr::retrieve_element_list(server_name, profile_id, access_token,
                                            page_id, fields = 'label')
  # Fields in page from elements dataframe
  ifb_flds <- ifb_flds$name
  # Fields in source table also in page
  flds <- intersect(src_flds, ifb_flds)
  # Fields in both tables collapsed to string
  fldstr <- paste(flds, collapse = ',')
  # Check that UID column is in both datasets
  uid <- tolower(uid)
  if (!(uid %in% src_flds))
  {stop(paste0("UID column ",uid," is missing from source data."))}
  if (!(uid %in% ifb_flds))
  {stop(paste0("UID column ",uid," is missing from IFB data."))}
  # Pull all data in IFB table
  i_data <- iformr::get_all_records(server_name, profile_id, page_id, fields = "fields",
                                    limit = 1000, offset = 0, access_token,
                                    field_string = fldstr, since_id = 0)
  # If there is data in IFB
  if (nrow(i_data) > 0) {
    # Find data in source table that is not in IFB table
    new_data <- dplyr::anti_join(data, i_data, by=uid)
    # Remove columns from source table not in IFB table
    new_data <- new_data[ , (names(new_data) %in% flds)]
  }
  else {
    new_data <- data
  }
  # Upload new data to IFB
  message(paste0(nrow(new_data), " new records will be added to ",form_name))
  upload <- create_new_records(server_name, profile_id, page_id,
                               access_token, record_data = new_data)
}


#' Form metadata
#'
#' Builds a Markdown document containing metadata for a given
#' iFormBuilder form by querying the API for page and element
#' level information. By utilizing the description fields during
#' form building, detailed metadata can be built afterward using
#' this function.
#'
#' @rdname form_metadata
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{iformr::get_iform_access_token}
#' @param page_id ID of the form to get metadata from.
#' @param file Filename of the output Markdown file.
#' @param subforms **Optional** - Indicates if metadata should be generated for subforms.
#' @return
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Add new data to form
#' sync_table(server_name, profile_id, access_token,
#'   data = "new_dataframe", form_name = "my_form",
#'   uid = "unique_id_col")
#' }
#' @export
form_metadata <- function(server_name, profile_id, access_token,
                       page_id, file, subforms=T) {

}
