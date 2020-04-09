#'
#' Retrieve a list of device licenses
#'
#' Retrieves a list of all device licenses in a profile.
#' Core API call [Retrieve a List of Device Licenses](
#' https://iformbuilder.docs.apiary.io/#reference/device-license-resource/device-license-collection/retrieve-a-list-of-device-licenses)
#'
#' @rdname retrieve_all_device_licenses
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @return Dataframe containing the ID, username, and device_id for
#'   all device licenses in a profile
#' @import httr
#'
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Retrieve a list of all device licenses in a profile
#' all_device_license_info <- retrieve_all_device_licenses(
#'   server_name = "your_server_name",
#'   profile_id = "your_profile_id",
#'   access_token = access_token)
#'
#' @export
retrieve_all_device_licenses <- function(server_name, profile_id, access_token) {
  # Build URL
  all_licenses_uri <- paste0(api_v60_url(server_name = server_name),
                             profile_id, "/licenses")
  bearer <- paste0("Bearer ", access_token)
  # Build request
  r <- httr::GET(url = all_licenses_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  all_lic <- httr::content(r, type = "application/json")
  licx <- integer(length(all_lic))
  usrid <- unlist(lapply(seq_along(licx),
                         function(i) licx[i] <- all_lic[[i]]$id))
  usrname <- unlist(lapply(seq_along(licx),
                           function(i) licx[i] <- all_lic[[i]]$username))
  dev_id <- unlist(lapply(seq_along(licx),
                          function(i) licx[i] <- all_lic[[i]]$device_id))
  all_lic <- tibble::tibble(id = usrid, username = usrname,
                            device_id = dev_id)
  return(all_lic)
}

#' Retrieve a device license
#'
#' Retrieves a single device license.
#' Core API call [Retrieve a Device License](
#' https://iformbuilder.docs.apiary.io/#reference/device-license-resource/device-license/retrieve-a-device-license)
#'
#' @rdname retrieve_device_license
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param license_id Integer id of the device license.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @return List containing attributes of the device license
#' @import httr
#'
#' @examples
#' \dontrun{
#'
#' # Pull out the first device license
#' license_id = all_device_license_info$id[1]
#'
#' # Retrieve details for a single device license
#' single_device_license_info <- retrieve_device_license(
#'   server_name = "your_server_name",
#'   profile_id = "your_profile_id",
#'   license_id = license_id,
#'   access_token = access_token)
#'
#' @export
retrieve_device_license <- function(server_name, profile_id,
                                    access_token, license_id) {
  # Build URL
  license_uri <- paste0(api_v60_url(server_name = server_name),
                        profile_id, "/licenses/", license_id)
  bearer <- paste0("Bearer ", access_token)
  # Build request
  r <- httr::GET(url = license_uri,
                 httr::add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  device_license <- httr::content(r, type = "application/json")
  return(device_license)
}
