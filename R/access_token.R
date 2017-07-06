
#' Get application client_key from .Renviron file
#' @rdname iform_key
#' @param app_key_name The name given to the client_key in the .Renviron file
#' @return The API client_key for the application
#' @export
iform_key <- function(app_key_name) {
  Sys.getenv(app_key_name)
}

#' Get application client_secret from .Renviron file
#' @rdname iform_secret
#' @param app_secret_name The name given to the client_secret in the .Renviron file
#' @return The API client_secret for the application
#' @export
iform_secret <- function(app_secret_name) {
  Sys.getenv(app_secret_name)
}

#' Check if the application client_key was obtained
#' @rdname has_key
#' @param client_key The string value of the client_key
#' @return TRUE if client_key was returned, else FALSE
#' @export
has_key <- function(client_key) !identical(client_key, "")

#' Check if application client_secret was obtained
#' @rdname has_secret
#' @param client_secret The string value of the client_secret
#' @return TRUE if client_secret was returned, else FALSE
#' @export
has_secret <- function(client_secret) !identical(client_secret, "")

#' Define base_url
#'
#' Use `base_url()` to define the base section of the request url.
#' If you do not have a dedicated server the server_name will be `app`
#' @rdname base_url
#' @param server_name The server name as encoded in the url: `https//server_name.iformbuilder.com`
#' @return The base iFormBuilder url with \code{server_name} encoded
#' @export
base_url <- function(server_name) {
  paste0("https://", server_name, ".iformbuilder.com")
}

#' Define token_url
#' @rdname token_url
#' @param server_name The server name as encoded in the url: `https//server_name.iformbuilder.com`
#' @return A url for requesting an access_token with \code{server_name} encoded
#' @export
token_url <- function(server_name) {
  paste0(base_url(server_name), "/exzact/api/oauth/token")
}

#' Define the jwt header. From httr
#' @rdname jwt_header
#' @return The json web token header as a list
#' @export
jwt_header <- function() {
  list(
    alg = 'HS256',
    typ = 'JWT'
  )
}

#' Define the jwt payload. From httr
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

#' Encode to base64 using url-safe function. From httr
#' @rdname base64url
#' @param x A string to be encoded to base64
#' @return The base64 encoded string
#' @export
base64url <- function(x) {
  if (is.character(x)) {
    x <- charToRaw(x)
  }
  out <- chartr('+/', '-_', openssl::base64_encode(x))
  gsub("=+$", "", out)
}

#' Get raw output from sha256() signing. From openssl
#' @rdname hex_to_raw
#' @param str A string to be converted from hex to raw format
#' @return The raw string
#' @export
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

# Get access token from iFormBuilder
#'
#' Sends a request to iFormBuilder for an access_token. This is needed
#' in order to authorize communication with the iFormBuilder API.
#' If you do not have a dedicated server your server_name will be `app`.
#' For the Washington Dept of Fish and Wildlife, server_name is `wdfw`.
#'
#' @rdname get_iform_access_token
#' @param server_name The server name as encoded in the url: `https//server_name.iformbuilder.com`
#' @param app_key_name The name given to the client_key in the .Renviron file
#' @param app_secret_name The name given to the client_secret in the .Renviron file
#' @return An access_token that expires after ten minutes
#' @export
get_iform_access_token <- function(server_name, app_key_name, app_secret_name) {
  jheader <- jsonlite::toJSON(jwt_header(), auto_unbox = TRUE)
  client_key <- iform_secret(app_key_name)
  client_secret <- iform_secret(app_secret_name)
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


