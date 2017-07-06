#' Get a listing of all option lists in a profile
#'
#' Sends a request to iFormBuilder for an access_token. This is needed
#' in order to authorize communication with the iFormBuilder API.
#' If you do not have a dedicated server your company_name will be `app`.
#' For the Washington Dept of Fish and Wildlife, company_name is `wdfw`.
#'
#' @rdname get_option_lists
#' @param company_name The company name as encoded in the url
#' @param profile_id The ID number of your profile
#' @param limit The maximum number of option lists to request
#' @param offset Sets the starting point for selecting option lists
#' @param access_token The access_token required to establish
#' @return An access_token that expires after ten minutes
#' @export
# Function to get a list of all option lists in a profile
get_option_lists <- function(company_name, profile_id, limit = 100, offset = 0, access_token) {
  optionlists_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists?fields=fields&limit=",
                            limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = optionlists_uri,
                 add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  opts <- content(r, type = "application/json")
  optx <- integer(length(opts))
  opt_id <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$id))
  opt_name <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$name))
  data_frame(id = opt_id, name = opt_name)
}

# Function to get the ID of a single option list given an option list name
getOptionListID <- function(company_name, profile_id, option_list_name, limit = 100, offset = 0, access_token) {
  optionlists_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists?fields=fields&limit=",
                            limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = optionlists_uri,
                 add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  opts <- content(r, type = "application/json")
  optx <- integer(length(opts))
  opt_id <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$id))
  opt_name <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$name))
  opt <- data_frame(id = opt_id, name = opt_name)
  opt$id[opt$name == option_list_name]
}

# Function to create a new option list. Returns an optionlist ID
createNewOptionList <- function(company_name, profile_id, option_list_name, access_token) {
  optionlist_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists")
  bearer <- paste0("Bearer ", access_token)
  r <- httr::POST(url = optionlist_uri,
                  add_headers('Authorization' = bearer),
                  body = list(name = option_list_name),
                  encode = "json")
  httr::stop_for_status(r)
  list_id = content(r, type = "application/json")$id
  if (!length(list_id) == 1) {
    stop("No optionlist id was returned")
  }
  return(list_id)
}

# Function to add option values to new option list. Returns a vector of option IDs.
addOptionsToList <- function(company_name, profile_id, optionlist_id, option_values, access_token) {
  # Check for duplicate values in option_values
  dup_chk <- jsonlite::fromJSON(option_values)
  if(!all(names(dup_chk) %in% c("sort_order", "label", "key_value", "condition_value"))) {
    stop(cat("\nUnrecognized option list names.\nNames can only consist of:\n",
             "'sort_order', 'label', 'key_value', or 'condition_value'.\n"))
  }
  dup_chk <- dup_chk %>% select(-sort_order)
  if(any(duplicated(dup_chk))) {
    stop(cat("\nThere are duplicated items in the option list\n"))
  }
  dup_chk <- dup_chk %>% select()
  options_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists/", optionlist_id, "/options")
  bearer <- paste0("Bearer ", access_token)
  r <- httr::POST(url = options_uri,
                  add_headers('Authorization' = bearer),
                  body = option_values,
                  encode = "json")
  httr::stop_for_status(r)
  as.vector(unlist(content(r, type = "application/json")))
}

# Delete options in an option list. Allows to select the specific fields to delete
deleteOptionsInList <- function(company_name, profile_id, optionlist_id, fields = fields,
                                limit = 100, offset = 0, access_token) {
  options_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists/", optionlist_id,
                        "/options?fields=", fields, "&limit=", limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::DELETE(url = options_uri,
                    add_headers('Authorization' = bearer),
                    encode = "json")
  httr::stop_for_status(r)
  as.vector(unlist(content(r, type = "application/json")))
}

# Function to get a list of option_ids in an option list for a given element (i.e., condition_value).
getOptionListElementIDs <- function(company_name, profile_id, optionlist_id, element, limit = 100,
                                    offset = 0, access_token) {
  fields = paste0("id:<," , element)
  if(!element %in% c("sort_order", "label", "key_value", "condition_value")) {
    stop(cat("Unrecognized element value.\nElement must be one of:\n",
             "'sort_order', 'label', 'key_value', or 'condition_value'.\n",
             "You entered:", element))
  }
  optionlists_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists/", optionlist_id,
                            "/options?fields=", fields, "&limit=", limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = optionlists_uri,
                 add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  opts <- content(r, type = "application/json")
  optx <- integer(length(opts))
  opt_id <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$id))
  opt_element <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]][element]))
  dat = data_frame(id = opt_id, element_name = opt_element)
  names(dat) <- c("id", element)
  dat
}

# Function retrieve a dataframe with core option elements in the list.
# Will only retrieve id, sort_order, label, key_value, condition_value
getCoreOptionListElements <- function(company_name, profile_id, optionlist_id, limit = 1000,
                                  offset = 0, access_token) {
  optionlists_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists/", optionlist_id,
                            "/options?fields=sort_order,label,condition_value", "&limit=", limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = optionlists_uri,
                 add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  opts <- content(r, type = "application/json")
  optx <- integer(length(opts))
  opt_id <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$id))
  opt_sort_order <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$sort_order))
  opt_label <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$label))
  opt_key <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$key_value))
  opt_cond <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$condition_value))
  dat = data_frame(id = opt_id, sort_order = opt_sort_order, label = opt_label,
                   key_value = opt_key, condition_value = opt_cond)
  dat
}

# Function to update values in an existing option list. Returns a vector of option IDs that were updated.
updateOptionsInList <- function(company_name, profile_id, optionlist_id, option_values, fields = fields,
                                limit = 100, offset = 0, access_token = access_token) {
  options_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists/", optionlist_id,
                        "/options?fields=", fields, "&limit=", limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::PUT(url = options_uri,
                 add_headers('Authorization' = bearer),
                 body = option_values,
                 encode = "json")
  httr::stop_for_status(r)
  as.vector(unlist(content(r, type = "application/json")))
}

