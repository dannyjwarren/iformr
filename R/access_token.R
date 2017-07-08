#' Define the jwt payload
#' @rdname jwt_payload
#' @param client_key The client_key assigned to the application
#' @param iat The issue time
#' @param exp The expiration time
#' @param token_uri The uri for requesting the access_token
#' @param duration The length of time between issue time and expiration time
#' @return The json web token payload as a list
#' @export
jwt_payload <- function(client_key, iat = NULL, exp = NULL, token_uri, duration = 600L) {
  if (is.null(iat)) {
    iat <- as.integer(lubridate::now("GMT"))
  }
  if (is.null(exp)) {
    exp <- as.integer(lubridate::now("GMT")) + duration
  }
  list(
    iss = client_key,
    iat = iat,
    aud = token_uri,
    exp = exp
  )
}

#' Generate encoded request token
#' @rdname jencode
#' @param jheader The jwt_header in json format
#' @param jpayload The jwt_payload in json format
#' @param client_secret The client_secret assigned to the application
#' @return The signed and encoded request token
#' @export
jencode <- function(jheader, jpayload, client_secret) {
  base_sign <- paste0(base64url(jheader), ".", base64url(jpayload))
  signed_hex <- openssl::sha256(base_sign, client_secret)
  signed_raw <- hex_to_raw(signed_hex)
  if (!any('00' %in% signed_raw)) {
    signed_char <- rawToChar(signed_raw)
    signed <- base64url(signed_char)
    request_token <- paste0(base_sign, ".", signed)
  } else {
    request_token <- ""
  }
  request_token
}

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
#'   \code{client_key_name} and \code{client_secret_name}, along with
#'   their respective values \strong{must} be in your .Renviron file.
#'   This function will not work otherwise. Please see the README file
#'   at \url{https://github.com/arestrom/iformr} for directions.
#'
#' @rdname get_iform_access_token
#' @param server_name The server name as encoded in the url:
#'   `https//server_name.iformbuilder.com`
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
  jheader <- jsonlite::toJSON(jwt_header(), auto_unbox = TRUE)
  client_key <- iform_secret(client_key_name)
  client_secret <- iform_secret(client_secret_name)
  token_uri <- token_url(server_name = server_name)
  if (has_key(client_key)) {
    payload <- jsonlite::toJSON(jwt_payload(client_key = client_key,
                                            token_uri = token_uri), auto_unbox = TRUE)
  } else {
    stop("Client Key failed to load")
  }
  if (has_secret(client_secret)) {
    encoded <- jencode(jheader = jheader, jpayload = payload, client_secret = client_secret)
  } else {
    stop("Client Secret failed to load")
  }
  while (encoded == "") {
    payload <- jsonlite::toJSON(jwt_payload(client_key = client_key,
                                            token_uri = token_uri), auto_unbox = TRUE)
    encoded <- jencode(jheader = jheader, jpayload = payload, client_secret = client_secret)
  }
  body <- list(grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
               assertion = encoded)
  r <- httr::POST(url = token_uri,
                  body = body,
                  httr::add_headers('Content-Type' = 'application/x-www-form-urlencoded'),
                  encode = "form")
  httr::stop_for_status(r)
  acc_token = httr::content(r, type = "application/json")$access_token
  if (identical(acc_token, "")) stop("No access_token was returned", call. = FALSE)
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

# Encode to url-safe base64
# Taken from the httr package. Will be replaced in
# a future version by importing the base64url package
base64url <- function(x) {
  if (is.character(x)) {
    x <- charToRaw(x)
  }
  out <- chartr('+/', '-_', openssl::base64_encode(x))
  gsub("=+$", "", out)
}

# Get raw output from sha256() signing
# Taken from openssl package. Will be replaced in
# a future version by importing the jose package.
hex_to_raw <- function(str){
  stopifnot(length(str) == 1)
  str <- gsub("[ :]", "", str)
  len <- nchar(str)/2
  out <- raw(len)
  for(i in 1:len){
    out[i] <- as.raw(as.hexmode(substr(str, 2*i-1, 2*i)))
  }
  out
}

