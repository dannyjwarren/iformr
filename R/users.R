#' Retrieve a user
#'
#' Retrieves information for a specific user. Core API call [Retrieve a User](
#' https://iformbuilder.docs.apiary.io/#reference/user-resource/user/retrieve-a-user)
#'
#' @rdname retrieve_user
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @param user_id Integer of the user ID to retrieve.
#' @return List containing the user details.
#' @export
retrieve_user <- function(server_name, profile_id,
                          access_token, user_id) {
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



