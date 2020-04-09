#' Retrieve a profile
#'
#' Retrieves a profile. Core API call [Retrieve a Profile](
#' https://iformbuilder.docs.apiary.io/#reference/profile-resource/profile/retrieve-a-profile)
#'
#' @rdname retrieve_profile
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @return Dataframe containing profile attributes
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Get a list of all usernames and user IDs in a profile
#' profile_info <- retrieve_profile(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   access_token = access_token)
#'}
#' @export
retrieve_profile <- function(server_name, profile_id, access_token) {
  # Build URL
  profile_uri <- paste0(api_v60_url(server_name = server_name), profile_id)
  bearer <- paste0("Bearer ", access_token)
  # Build request
  r <- httr::GET(url = profile_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  profile_info <- httr::content(r, type = "application/json")
  profile_info <- unlist(lapply(profile_info,
                          function(x) ifelse(is.null(x), as.character(NA), x)))
  profile_info <- tibble::as_tibble(t(profile_info))
  return(profile_info)
}

#' Retrieve company info
#'
#' Retrieves company (profile) info. Core API call [Retrieve Company Info](
#' https://iformbuilder.docs.apiary.io/#reference/profile-resource/company-info/retrieve-company-info)
#'
#' @rdname retrieve_company_info
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name
#' @param profile_id Integer of the iFormBuilder profile ID
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @return Dataframe containing company info attributes
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Get a list of all usernames and user IDs in a profile
#' company_info <- retrieve_company_info(
#'   server_name = "your_server_name",
#'   profile_id = 123456,
#'   access_token = access_token)
#'}
#' @export
retrieve_company_info <- function(server_name, profile_id, access_token) {
  # Build URL
  company_uri <- paste0(api_v60_url(server_name = server_name), profile_id,
                        "/company_info")
  bearer <- paste0("Bearer ", access_token)
  # Build request
  r <- httr::GET(url = company_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  company <- httr::content(r, type = "application/json")
  company <- unlist(lapply(company,
                           function(x) ifelse(is.null(x), as.character(NA), x)))
  company <- tibble::as_tibble(t(company))
  return(company)
}

