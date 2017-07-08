#' Get a listing of all option lists in a profile
#'
#' Sends a request to the iFormBuilder API to get a listing of all option lists
#' currently posted in the given profile.
#'
#' @rdname get_option_lists
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The ID number of your profile
#' @param limit The maximum number of option lists to return
#' @param offset Skips the offset number of options before beginning to return
#' @param access_token The access_token required to establish communication with
#'   the API
#' @return A listing of all option lists in the given profile
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Get the id and name of all option lists in profile
#' option_lists <- get_option_lists(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   access_token = access_token)
#'
#'   # Inspect
#'   option_lists
#' }
#' @export
get_option_lists <- function(server_name,
                             profile_id,
                             limit = 1000,
                             offset = 0,
                             access_token) {
  optionlists_uri <- paste0(api_v60_url(server_name = server_name),
                            profile_id,
                            "/optionlists?fields=fields&limit=", limit,
                            "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = optionlists_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  opts <- httr::content(r, type = "application/json")
  optx <- integer(length(opts))
  opt_id <- unlist(lapply(seq_along(optx),
                          function(i) optx[i] <- opts[[i]]$id))
  opt_name <- unlist(lapply(seq_along(optx),
                            function(i) optx[i] <- opts[[i]]$name))
  dplyr::data_frame(id = opt_id, name = opt_name)
}

#' Get the ID of a single option list given an option list name
#'
#' Sends a request to the iFormBuilder API to get the ID number for a single
#' option list. You only need to supply the name of the option list.
#'
#' @rdname get_option_list_id
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The ID number of your profile
#' @param option_list_name The name of the option list
#' @param limit The maximum number of option lists to return
#' @param offset Skips the offset number of options before beginning to return
#' @param access_token The access_token required to establish communication with
#'   the API
#' @return A listing of all option lists in the given profile
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Get the id of a single option list given the name
#' streams_option_list_id <- get_option_list_id(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   option_list_name = "SGS-Streams",
#'   access_token = access_token)
#' }
#' @export
get_option_list_id <- function(server_name,
                               profile_id,
                               option_list_name,
                               limit = 1000,
                               offset = 0,
                               access_token) {
  optionlists_uri <- paste0(api_v60_url(server_name = server_name),
                            profile_id,
                            "/optionlists?fields=fields&limit=", limit,
                            "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = optionlists_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  opts <- httr::content(r, type = "application/json")
  optx <- integer(length(opts))
  opt_id <- unlist(lapply(seq_along(optx),
                          function(i) optx[i] <- opts[[i]]$id))
  opt_name <- unlist(lapply(seq_along(optx),
                            function(i) optx[i] <- opts[[i]]$name))
  opt <- dplyr::data_frame(id = opt_id, name = opt_name)
  opt$id[opt$name == option_list_name]
}

#' Create a new option list
#'
#' Sends a request to the iFormBuilder API to create a new option list. The new
#' option list will be created with the name you supply.
#'
#' @rdname create_new_option_list
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The ID number of your profile
#' @param option_list_name A character name for the new option list
#' @param access_token The access_token required to establish communication with
#'   the API
#' @return The ID of the new option list
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Create a new empty option list
#' new_option_list_id <- create_new_option_list(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   option_list_name = "SGS-Streams",
#'   access_token = access_token)
#'
#' # Inspect new option list id
#' option_list_id

#' # If successful, result should look something like:
#' # [1] 4423966
#' }
#' @export
create_new_option_list <- function(server_name,
                                   profile_id,
                                   option_list_name,
                                   access_token) {
  optionlist_uri <- paste0(api_v60_url(server_name = server_name),
                           profile_id, "/optionlists")
  bearer <- paste0("Bearer ", access_token)
  r <- httr::POST(url = optionlist_uri,
                  httr::add_headers('Authorization' = bearer),
                  body = list(name = option_list_name),
                  encode = "json")
  httr::stop_for_status(r)
  list_id = httr::content(r, type = "application/json")$id
  if (!length(list_id) == 1) {
    stop("No optionlist id was returned")
  }
  return(list_id)
}

#' Add option values to new option list
#'
#' Sends a request to the iFormBuilder API to append a list of options in json
#' format to an existing option list. Make sure that all options list key_values
#' are unique or the list will not be posted.
#'
#' @rdname add_options_to_list
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The ID number of your profile
#' @param optionlist_id The ID number for the option list
#' @param option_values A list of option values in json format
#' @param access_token The access_token required to establish communication with
#'   the API
#' @return A vector of option list element IDs, one for each option in the list
#' @examples
#' # The locations dataset is an example of a segmented option list
#' head(locations, 5)

#' # Convert locations dataframe to json
#' location_json <- jsonlite::toJSON(locations, auto_unbox = TRUE)
#'
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Create a new empty option list
#' new_option_list_id <- create_new_option_list(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   option_list_name = "SGS-StreamLocations",
#'   access_token = access_token)
#'
#' # Inspect new option list id
#' new_option_list_id
#'
#' # Add option elements from locations dataset to the new option list
#' option_ids <- add_options_to_list(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   optionlist_id = new_option_list_id,
#'   option_values = location_json
#'   access_token = access_token)
#'
#' # Inspect the first five new option list element IDs.
#' head(option_ids, 5)
#' }
#' @export
add_options_to_list <- function(server_name,
                                profile_id,
                                optionlist_id,
                                option_values,
                                access_token) {
  dup_chk <- jsonlite::fromJSON(option_values)
  if(!all(names(dup_chk) %in% c("sort_order", "label", "key_value", "condition_value"))) {
    stop(cat("\nUnrecognized option list names.\nNames can only consist of:\n",
             "'sort_order', 'label', 'key_value', or 'condition_value'.\n"))
  }
  dup_chk <- dup_chk[, names(dup_chk)[!names(dup_chk) %in% "sort_order"]]
  if(any(duplicated(dup_chk))) {
    stop(cat("\nThere are duplicated items in the option list\n"))
  }
  options_uri <- paste0(api_v60_url(server_name = server_name),
                        profile_id,
                        "/optionlists/", optionlist_id,
                        "/options")
  bearer <- paste0("Bearer ", access_token)
  r <- httr::POST(url = options_uri,
                  httr::add_headers('Authorization' = bearer),
                  body = option_values,
                  encode = "json")
  httr::stop_for_status(r)
  as.vector(unlist(httr::content(r, type = "application/json")))
}

#' Delete all or some options in an option list
#'
#' Sends a request to the iFormBuilder API to delete a list of option elements.
#' The elements to delete are specified by a .json list of element ids. Sort
#' order will automatically be reassigned after deleting specified elements.
#'
#' @rdname delete_options_in_list
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The ID number of your profile
#' @param optionlist_id The ID number for the option list
#' @param fields Placeholder for fields to delete, not yet implemented
#' @param id_values A .json list of ids for elements to delete
#' @param limit The maximum number of option elements to delete
#' @param offset Skips the offset number of options before beginning to delete
#' @param access_token The access_token required to establish communication with
#'   the API
#' @return A vector of option list elements that were deleted
#' @examples
#' \dontrun{
#' # Define .json list of ids for elements to delete
#' # Replace example values below with your own
#'   id_values = data_frame(id = c(663487010, 663487013))
#'   id_values_json = jsonlite::toJSON(id_values, auto_unbox = TRUE)
#'
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Delete specified elements from option list
#' deleted_ids <- delete_options_in_list(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   optionlist_id = your_option_list_id,
#'   id_values = id_values_json,
#'   access_token = access_token)
#'
#' # Inspect the first five deleted ids
#' head(deleted_ids, 5)
#' }
#' @export
delete_options_in_list <- function(server_name,
                                   profile_id,
                                   optionlist_id,
                                   fields = "fields",
                                   id_values,
                                   limit = 1000,
                                   offset = 0,
                                   access_token) {
  options_uri <- paste0(api_v60_url(server_name = server_name),
                        profile_id,
                        "/optionlists/", optionlist_id,
                        "/options?fields=", fields,
                        "&limit=", limit,
                        "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::DELETE(url = options_uri,
                    httr::add_headers('Authorization' = bearer),
                    body = id_values,
                    encode = "json")
  httr::stop_for_status(r)
  as.vector(unlist(httr::content(r, type = "application/json")))
}

#' Get list of option_ids for a given element
#'
#' Sends a request to the iFormBuilder API to get a list of all element IDs in
#' an option list for a specific field. For example: \code{key_value}.
#' Returns a dataframe with ids and attributes of the specified element.
#'
#' @rdname get_option_list_element_ids
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The ID number of your profile
#' @param optionlist_id The ID number for the option list
#' @param element The specific option list element. For example:
#'   "condition_value".
#' @param limit The maximum number of option element IDs to return
#' @param offset Skips the offset number of options before beginning to return
#' @param access_token The access_token required to establish communication with
#'   the API
#' @return A dataframe with id and attributes for the selected element.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Get the element ids
#' element_ids <- get_option_list_element_ids(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   optionlist_id = your_option_list_id,
#'   element = "key_value",
#'   access_token = access_token)
#'
#' # Inspect the first five element ids
#' head(element_ids, 5)
#' }
#' @export
get_option_list_element_ids <- function(server_name,
                                        profile_id,
                                        optionlist_id,
                                        element,
                                        limit = 1000,
                                        offset = 0,
                                        access_token) {
  fields = paste0("id:<," , element)
  if(!element %in% c("sort_order", "label", "key_value", "condition_value")) {
    stop(cat("Unrecognized element value.\nElement must be one of:\n",
             "'sort_order', 'label', 'key_value', or 'condition_value'.\n",
             "You entered:", element))
  }
  optionlists_uri <- paste0(api_v60_url(server_name = server_name),
                            profile_id,
                            "/optionlists/", optionlist_id,
                            "/options?fields=", fields,
                            "&limit=", limit,
                            "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = optionlists_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  opts <- httr::content(r, type = "application/json")
  optx <- integer(length(opts))
  opt_id <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$id))
  opt_element <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]][element]))
  dat = dplyr::data_frame(id = opt_id, element_name = opt_element)
  names(dat) <- c("id", element)
  dat
}

#' Get core elements in an option list
#'
#' Sends a request to the iFormBuilder API to return the core option list
#' elements. Function will return the id, sort_order, label, key_value, and
#' condition_value.
#'
#' @rdname get_core_option_list_elements
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The ID number of your profile
#' @param optionlist_id The ID number for the option list
#' @param limit The maximum number of option list items to return
#' @param offset Skips the offset number of options before beginning to return
#' @param access_token The access_token required to establish communication with
#'   the API
#' @return A dataframe of the core option list elements
#' @export
get_core_option_list_elements <- function(server_name,
                                          profile_id,
                                          optionlist_id,
                                          limit = 1000,
                                          offset = 0,
                                          access_token) {
  optionlists_uri <- paste0(api_v60_url(server_name = server_name),
                            profile_id,
                            "/optionlists/", optionlist_id,
                            "/options?fields=sort_order,label,condition_value",
                            "&limit=", limit,
                            "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = optionlists_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  opts <- httr::content(r, type = "application/json")
  optx <- integer(length(opts))
  opt_id <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$id))
  opt_sort_order <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$sort_order))
  opt_label <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$label))
  opt_key <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$key_value))
  opt_cond <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$condition_value))
  dat = dplyr::data_frame(id = opt_id, sort_order = opt_sort_order, label = opt_label,
                   key_value = opt_key, condition_value = opt_cond)
  dat
}

#' Update values in an existing option list
#'
#' Sends a request to the iFormBuilder API to update existing values in an
#' option list. Option values for the specified fields will be updated to the
#' new values supplied in the json object `option_values`. Do not use the fields
#' parameter. It has not been implemented yet.
#'
#' @rdname update_options_in_list
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
#' @param profile_id The ID number of your profile
#' @param optionlist_id The ID number for the option list
#' @param option_values A json object containing new option list values
#' @param limit The maximum number of option list items to return
#' @param fields Placeholder for fields to update, not yet implemented
#' @param offset Skips the offset number of options before beginning to update
#' @param access_token The access_token required to establish communication with
#'   the API
#' @return A vector of option IDs for elements that were updated
#' @export
update_options_in_list <- function(server_name,
                                   profile_id,
                                   optionlist_id,
                                   option_values,
                                   fields = "fields",
                                   limit = 1000,
                                   offset = 0,
                                   access_token = access_token) {
  options_uri <- paste0(api_v60_url(server_name = server_name),
                        profile_id,
                        "/optionlists/", optionlist_id,
                        "/options?fields=", fields,
                        "&limit=", limit,
                        "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::PUT(url = options_uri,
                 httr::add_headers('Authorization' = bearer),
                 body = option_values,
                 encode = "json")
  httr::stop_for_status(r)
  as.vector(unlist(httr::content(r, type = "application/json")))
}

# Define the API url
api_v60_url <- function(server_name) {
  paste0(base_url(server_name), "/exzact/api/v60/profiles/")
}

