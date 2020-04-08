#'
#' Retrieve a list of all users in a profile
#'
#' Retrieves a list of all users in a profile. Core API call [Retrieve a List of Users](
#' https://iformbuilder.docs.apiary.io/#reference/user-resource/user-collection/retrieve-a-list-of-users)
#'
#' @rdname retrieve_all_users
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @return Dataframe containing all usernames and user IDs in the profile.
#'
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Get a list of all usernames and user IDs in a profile
#' all_users_info <- retrieve_all_users(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   access_token = access_token)
#'
# Pull out first user_id in the list
#' user_id = all_users_info$id[1]
#'
#' @export
retrieve_all_users <- function(server_name, profile_id, access_token) {
  # Build URL
  retrieve_all_users_uri <- paste0(api_v60_url(server_name = server_name),
                                   profile_id, "/users")
  bearer <- paste0("Bearer ", access_token)
  # Build request
  r <- httr::GET(url = retrieve_all_users_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  user_ids = httr::content(r, type = "application/json")
  userx <- integer(length(user_ids))
  usr_id <- unlist(lapply(seq_along(userx),
                          function(i) userx[i] <- user_ids[[i]]$id))
  usr_name <- unlist(lapply(seq_along(userx),
                            function(i) userx[i] <- user_ids[[i]]$username))
  user_ids <- tibble::tibble(id = usr_id, username = usr_name)
  return(user_ids)
}

#'
#' Retrieve a user
#'
#' Retrieves information for a specific user. Core API call [Retrieve a User](
#' https://iformbuilder.docs.apiary.io/#reference/user-resource/user/retrieve-a-user)
#'
#' @rdname retrieve_user
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param user_id Integer of the user ID to retrieve.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @return List containing the user details.
#'
#' #' # Get a list of details for a given user
#' all_users_info <- retrieve_all_users(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   user_id = user_id,
#'   access_token = access_token)
#'
#' @export
retrieve_user <- function(server_name, profile_id,
                          user_id, access_token) {
  # Build URL
  retrieve_user_uri <- paste0(api_v60_url(server_name = server_name),
                              profile_id, "/users/", user_id)
  bearer <- paste0("Bearer ", access_token)
  # Build request
  r <- httr::GET(url = retrieve_user_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  user_ids = httr::content(r, type = "application/json")
  return(user_ids)
}
