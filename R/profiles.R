#' Retrieve a profile
#'
#' Retrieves a profile. Core API call [Retrieve a Device License](
#' https://iformbuilder.docs.apiary.io/#reference/profile-resource/profile/retrieve-a-profile)
#'
#' @rdname retrieve_profile
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @return List containing profile attributes.
#' @import httr
#' @export
retrieve_profile <- function(server_name, profile_id, access_token) {
  # Build URL
  profile_uri <- paste0(api_v60_url(server_name = server_name), profile_id)
  message(profile_uri)
  bearer <- paste0("Bearer ", access_token)
  # Build request
  r <- httr::GET(url = profile_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  profile <- httr::content(r, type = "application/json")
  return(profile)
}


## TODO: Update a profile

#' Retrieve company info
#'
#' Retrieves company info. Core API call [Retrieve a Device License](
#' https://iformbuilder.docs.apiary.io/#reference/profile-resource/company-info/retrieve-company-info)
#'
#' @rdname retrieve_company_info
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @return List containing company info.
#' @import httr
#' @export
retrieve_profile <- function(server_name, profile_id, access_token) {
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
  return(company)
}

## TODO: Update company info
## TODO: Retrieve a list of profiles
## TODO: Create new profile


