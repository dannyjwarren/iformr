#' Send notifications
#'
#' Sends a notification to a user or users. Core API call [Send Notifications](
#' https://iformbuilder.docs.apiary.io/#reference/notification-resource/notification/send-notifications)
#'
#' @rdname send_notifications
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @param message String containing the message to send.
#' @param users Numerical vector containing one or more user IDs,
#' ie c(1234, 3456)
#' @return Vector of the user IDs that were successfully notified.
#' @import httr
#' @export
send_notifications <- function(server_name, profile_id,
                              access_token, message, users) {
  # Build URL
  notify_uri <- paste0(api_v60_url(server_name = server_name),
                           profile_id, "/notifications")
  bearer <- paste0("Bearer ", access_token)
  # Build request
  r <- httr::POST(url = notify_uri,
                  httr::add_headers('Authorization' = bearer),
                  body = list(message = message,
                              users = users),
                  encode = "json")
  httr::stop_for_status(r)
  user_ids = httr::content(r, type = "application/json")$id
  return(user_ids)
}
