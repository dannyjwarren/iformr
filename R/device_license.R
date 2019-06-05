#' Retrieve a device license
#'
#' Retrieves a single device license. Core API call [Retrieve a Device License](
#' https://iformbuilder.docs.apiary.io/#reference/device-license-resource/device-license/retrieve-a-device-license)
#'
#' @rdname retrieve_device_license
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @param license_id Integer id of the device license.
#' @return Dataframe containing attributes of the device license
#' @import httr
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
