#'
#' @title Request an access_token
#'
#' @description Sends a request to iFormBuilder for an access_token. This is
#'   needed in order to authorize communication with the iFormBuilder API. If
#'   you do not have a dedicated server then replace \code{server_name} with
#'   \code{app}.
#'
#' @details For \code{client_key_name} use the name you assigned to the
#'   client_key in your .Renviron file. For \code{client_secret_name} use the
#'   name you assigned to the client_secret in your .Renviron file. The
#'   \code{client_key_name} and \code{client_secret_name}, along with their
#'   respective values \strong{must} be in your .Renviron file. This function
#'   will not work otherwise. Please see the README file at
#'   \url{https://github.com/arestrom/iformr} for additional information.
#'   Three tries will be attempted to retrive the token. If all attempts
#'   fail, a warning will be displayed indicating HTTP status.
#'
#' @rdname get_iform_access_token
#' @param server_name The server name as encoded in the url:
#'   for example: `https//server_name.iformbuilder.com`
#' @param client_key_name The name given to the client_key in your .Renviron
#'   file
#' @param client_secret_name The name given to the client_secret in your
#'   .Renviron file
#' @return An access_token that expires after ten minutes
#' @examples
#' \dontrun{
#' # Get access_token, assuming you do not have a dedicated server
#' # Edit client_key and client_secret arguments as needed.
#' access_token <- get_iform_access_token(
#'   server_name = "app",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#' }
#'
#' \dontrun{
#' # Get access_token, assuming your dedicated server is "wdfw"
#' access_token <- get_iform_access_token(
#'   server_name = "wdfw",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#' }
#'
#' @export
get_iform_access_token <- function(server_name, client_key_name, client_secret_name) {
  client_key <- iform_key(client_key_name)
  client_secret <- iform_secret(client_secret_name)
  token_uri <- token_url(server_name = server_name)
  if (has_key(client_key)) {
    claim <- jose::jwt_claim(
      iss = client_key, aud = token_uri,
      exp = unclass(Sys.time() + 600),
      iat = unclass(Sys.time()))
  } else {
    stop("Client Key failed to load")
  }
  if (has_secret(client_secret)) {
    signed_encoded <- jose::jwt_encode_hmac(
      claim = claim, secret = client_secret,
      size = 256, header = jwt_header())
  } else {
    stop("Client Secret failed to load")
  }
  body <- list(grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
               assertion = signed_encoded)
  r <- httr::RETRY("POST", url = token_uri, body = body,
                  httr::add_headers('Content-Type' = 'application/x-www-form-urlencoded'),
                  encode = "form", pause_cap = 10)
  httr::warn_for_status(r, task = "request for access_token")
  acc_token <- httr::content(r, type = "application/json")$access_token
  return(acc_token)
}

# Un-exported functions ============================================

# Get application client_key from .Renviron file
iform_key <- function(app_key_name) {
  Sys.getenv(app_key_name)
}

# Get application client_secret from .Renviron file
iform_secret <- function(app_secret_name) {
  Sys.getenv(app_secret_name)
}

# Check if the application client_key was obtained
has_key <- function(client_key) !identical(client_key, "")

# Check if application client_secret was obtained
has_secret <- function(client_secret) !identical(client_secret, "")

# Define base_url
base_url <- function(server_name) {
  paste0("https://", server_name, ".iformbuilder.com")
}

# Define token_url
token_url <- function(server_name) {
  paste0(base_url(server_name), "/exzact/api/oauth/token")
}

# Define the jwt header
jwt_header <- function() {
  list(
    alg = 'HS256',
    typ = 'JWT'
  )
}

# Define the API url
api_v60_url <- function(server_name) {
  paste0(base_url(server_name), "/exzact/api/v60/profiles/")
}

