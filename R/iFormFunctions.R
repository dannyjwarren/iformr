#================================================================================
# Functions to access the iformbuilder API
#
# Notes:
#  1. Instructions to set up an .Renviron file with client_key and client_secret in
#     your R home directory can be found in the Appendix section of:
#     https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
#  2. To identify your R home directory. Enter normalizePath("~/") in the R console.
#     Normally this will be C:\\Users\\YourLogin\\.
#  3. Copy the .Renviron file to this directory. Edit the file to only list your own
#     client_key and client_secret...your credentials to obtain an access token.
#  4. Restart R. .Renviron is processed only at the start of an R session.
#  5. Use Sys.getenv() to grab your credentials. See code below, lines 40 and 45.
#  6. Functions modeled on vignettes and examples from httr github repository.
#
# ToDo:
#  1. Refactor to use iform_parse function after testing complete.
#  2. Investigate how null bit is created in payload. What are the triggers?
#
# Resources:
#  1. Package building tools: http://www.masalmon.eu/2017/06/17/automatictools/
#  2. Package building:       https://datascienceplus.com/how-to-make-and-share-an-r-package-in-3-steps/
#
# AS 2017-06-18
#================================================================================

# Make sure needed packages are installed
if (!require(httr)) install.packages('httr')
if (!require(jsonlite)) install.packages('jsonlite')
if (!require(openssl)) install.packages('openssl')
if (!require(lubridate)) install.packages('lubridate')
if (!require(curl)) install.packages('curl')

# Load packages
library(httr)
library(jsonlite)
library(openssl)
library(lubridate)
library(curl)

#===============================================================================
# Access token functions: Mine
#===============================================================================

# Function to get application client_key for API
iform_key <- function(app_key_name) {
  Sys.getenv(app_key_name)
}

# Function to get application client_secret for API
iform_secret <- function(app_secret_name) {
  Sys.getenv(app_secret_name)
}

# Function to get application username for data feed
iform_user <- function(user_label) {
  Sys.getenv(user_label)
}

# Function to get application password for datafeed
iform_pw <- function(pwd_label) {
  Sys.getenv(pwd_label)
}

# Check if application client_key was obtained
has_key <- function(client_key) !identical(client_key, "")

# Check if application client_secret was obtained
has_secret <- function(client_secret) !identical(client_secret, "")

# Function to parse output
iform_parse <- function(r) {
  text <- content(r, type = "application/json", as = "text")
  if (identical(text, "")) stop("No output to parse", call. = FALSE)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

# Function to define base_url
base_url <- function(company) {
  paste0("https://", company, ".iformbuilder.com")
}

# Function to define token_url
token_url <- function(company) {
  paste0(base_url(company), "/exzact/api/oauth/token")
}

# Function to define optionlist_url
api_v60_url <- function(company) {
  paste0(base_url(company), "/exzact/api/v60/profiles/")
}

# Define header: Standard HS256
jwt_header <- function() {
  list(
    alg = 'HS256',
    typ = 'JWT'
  )
}

# Define payload
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

# Encode to base64 using url-safe function:
base64url <- function(x) {
  if (is.character(x)) {
    x <- charToRaw(x)
  }
  out <- chartr('+/', '-_', openssl::base64_encode(x))
  gsub("=+$", "", out)
}

# From openssl. Need raw output from sha256() signing
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

# Generate encoded request token
jencode <- function(jheader, jpayload, client_secret) {
  # Set header and claimset to variables
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

# Define function to get access token
getIformAccessToken <- function(company_name, app_key_name, app_secret_name) {
  # Define request token
  jheader <- jsonlite::toJSON(jwt_header(), auto_unbox = TRUE)
  client_key <- iform_secret(app_key_name)
  #cat(client_key, "")
  client_secret <- iform_secret(app_secret_name)
  #cat(client_secret)
  token_uri <- token_url(company = company_name)
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
  # Ugly hack to deal with occasional nul bit. Investigate later.
  while (encoded == "") {
    payload <- jsonlite::toJSON(jwt_payload(client_key = client_key,
                                            token_uri = token_uri), auto_unbox = TRUE)
    encoded <- jencode(jheader = jheader, jpayload = payload, client_secret = client_secret)
  }
  # Make call to iformbuilder
  body <- list(grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
               assertion = encoded)
  r <- httr::POST(url = token_uri,
                  body = body,
                  add_headers('Content-Type' = 'application/x-www-form-urlencoded'),
                  encode = "form")
  httr::stop_for_status(r)
  acc_token = httr::content(r, type = "application/json")$access_token
  if (identical(acc_token, "")) stop("No access_token was returned", call. = FALSE)
  return(acc_token)
}

#==========================================================================================
# Option list functions
#==========================================================================================

# Function to get a list of all option lists in a profile
getOptionLists <- function(company_name, profile_id, limit = 100, offset = 0, access_token) {
  optionlists_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists?fields=fields&limit=",
                            limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = optionlists_uri,
                 add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  opts <- content(r, type = "application/json")
  optx <- integer(length(opts))
  opt_id <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$id))
  opt_name <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$name))
  data_frame(id = opt_id, name = opt_name)
}

# Function to get the ID of a single option list given an option list name
getOptionListID <- function(company_name, profile_id, option_list_name, limit = 100, offset = 0, access_token) {
  optionlists_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists?fields=fields&limit=",
                            limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = optionlists_uri,
                 add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  opts <- content(r, type = "application/json")
  optx <- integer(length(opts))
  opt_id <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$id))
  opt_name <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$name))
  opt <- data_frame(id = opt_id, name = opt_name)
  opt$id[opt$name == option_list_name]
}

# Function to create a new option list. Returns an optionlist ID
createNewOptionList <- function(company_name, profile_id, option_list_name, access_token) {
  optionlist_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists")
  bearer <- paste0("Bearer ", access_token)
  r <- httr::POST(url = optionlist_uri,
                  add_headers('Authorization' = bearer),
                  body = list(name = option_list_name),
                  encode = "json")
  httr::stop_for_status(r)
  list_id = content(r, type = "application/json")$id
  if (!length(list_id) == 1) {
    stop("No optionlist id was returned")
  }
  return(list_id)
}

# Function to add option values to new option list. Returns a vector of option IDs.
addOptionsToList <- function(company_name, profile_id, optionlist_id, option_values, access_token) {
  # Check for duplicate values in option_values
  dup_chk <- jsonlite::fromJSON(option_values)
  if(!all(names(dup_chk) %in% c("sort_order", "label", "key_value", "condition_value"))) {
    stop(cat("\nUnrecognized option list names.\nNames can only consist of:\n",
             "'sort_order', 'label', 'key_value', or 'condition_value'.\n"))
  }
  dup_chk <- dup_chk %>% select(-sort_order)
  if(any(duplicated(dup_chk))) {
    stop(cat("\nThere are duplicated items in the option list\n"))
  }
  dup_chk <- dup_chk %>% select()
  options_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists/", optionlist_id, "/options")
  bearer <- paste0("Bearer ", access_token)
  r <- httr::POST(url = options_uri,
                  add_headers('Authorization' = bearer),
                  body = option_values,
                  encode = "json")
  httr::stop_for_status(r)
  as.vector(unlist(content(r, type = "application/json")))
}

# Delete options in an option list. Allows to select the specific fields to delete
deleteOptionsInList <- function(company_name, profile_id, optionlist_id, fields = fields,
                                limit = 100, offset = 0, access_token) {
  options_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists/", optionlist_id,
                        "/options?fields=", fields, "&limit=", limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::DELETE(url = options_uri,
                    add_headers('Authorization' = bearer),
                    encode = "json")
  httr::stop_for_status(r)
  as.vector(unlist(content(r, type = "application/json")))
}

# Function to get a list of option_ids in an option list for a given element (i.e., condition_value).
getOptionListElementIDs <- function(company_name, profile_id, optionlist_id, element, limit = 100,
                                    offset = 0, access_token) {
  fields = paste0("id:<," , element)
  if(!element %in% c("sort_order", "label", "key_value", "condition_value")) {
    stop(cat("Unrecognized element value.\nElement must be one of:\n",
             "'sort_order', 'label', 'key_value', or 'condition_value'.\n",
             "You entered:", element))
  }
  optionlists_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists/", optionlist_id,
                            "/options?fields=", fields, "&limit=", limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = optionlists_uri,
                 add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  opts <- content(r, type = "application/json")
  optx <- integer(length(opts))
  opt_id <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$id))
  opt_element <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]][element]))
  dat = data_frame(id = opt_id, element_name = opt_element)
  names(dat) <- c("id", element)
  dat
}

# Function retrieve a dataframe with core option elements in the list.
# Will only retrieve id, sort_order, label, key_value, condition_value
getCoreOptionListElements <- function(company_name, profile_id, optionlist_id, limit = 1000,
                                  offset = 0, access_token) {
  optionlists_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists/", optionlist_id,
                            "/options?fields=sort_order,label,condition_value", "&limit=", limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = optionlists_uri,
                 add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  opts <- content(r, type = "application/json")
  optx <- integer(length(opts))
  opt_id <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$id))
  opt_sort_order <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$sort_order))
  opt_label <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$label))
  opt_key <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$key_value))
  opt_cond <- unlist(lapply(seq_along(optx), function(i) optx[i] <- opts[[i]]$condition_value))
  dat = data_frame(id = opt_id, sort_order = opt_sort_order, label = opt_label,
                   key_value = opt_key, condition_value = opt_cond)
  dat
}

# Function to update values in an existing option list. Returns a vector of option IDs that were updated.
updateOptionsInList <- function(company_name, profile_id, optionlist_id, option_values, fields = fields,
                                limit = 100, offset = 0, access_token = access_token) {
  options_uri <- paste0(api_v60_url(company = company_name), profile_id, "/optionlists/", optionlist_id,
                        "/options?fields=", fields, "&limit=", limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::PUT(url = options_uri,
                 add_headers('Authorization' = bearer),
                 body = option_values,
                 encode = "json")
  httr::stop_for_status(r)
  as.vector(unlist(content(r, type = "application/json")))
}

#==========================================================================================
# Functions to work with forms and subforms
#==========================================================================================

# Function to get a list of all pages (forms, parent and subforms) in a profile
getPagesList <- function(company_name, profile_id, limit = 100, offset = 0, access_token) {
  pages_uri <- paste0(api_v60_url(company = company_name), profile_id, "/pages?fields=fields&limit=",
                      limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = pages_uri,
                 add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  pgs <- content(r, type = "application/json")
  pgsx <- integer(length(pgs))
  pgs_id <- unlist(lapply(seq_along(pgsx), function(i) pgsx[i] <- pgs[[i]]$id))
  pgs_name <- unlist(lapply(seq_along(pgsx), function(i) pgsx[i] <- pgs[[i]]$name))
  data_frame(id = pgs_id, name = pgs_name)
}

# Function to get the ID of a single page (form) given a form name
getPageID <- function(company_name, profile_id, page_name, limit = 100, offset = 0, access_token) {
  pages_uri <- paste0(api_v60_url(company = company_name), profile_id, "/pages?fields=fields&limit=",
                      limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = pages_uri,
                 add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  pgs <- content(r, type = "application/json")
  pgsx <- integer(length(pgs))
  pgs_id <- unlist(lapply(seq_along(pgsx), function(i) pgsx[i] <- pgs[[i]]$id))
  pgs_name <- unlist(lapply(seq_along(pgsx), function(i) pgsx[i] <- pgs[[i]]$name))
  pg = data_frame(id = pgs_id, name = pgs_name)
  pg$id[pg$name == page_name]
}

# Function to get list of all record IDs in a page (form). Returns a vector of Record IDs from form
getPageRecordList <- function(company_name, profile_id, page_id, limit = 100, offset = 0, access_token) {
  recordlist_uri <- paste0(api_v60_url(company = company_name), profile_id, "/pages/", page_id,
                           "/records?fields=fields&limit=", limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = recordlist_uri,
                 add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  rcrd <- content(r, type = "application/json")
  rcrdx <- integer(length(rcrd))
  unlist(lapply(seq_along(rcrdx), function(i) rcrdx[i] <- rcrd[[i]]$id))
}

# Function to get a single record from a page (form). Returns a dataframe of single record from the given form
getPageRecord <- function(company_name, profile_id, page_id, record_id, access_token) {
  pagerecord_uri <- paste0(api_v60_url(company = company_name), profile_id, "/pages/", page_id,
                           "/records/", record_id)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = pagerecord_uri,
                 add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  rcrd <- content(r, type = "application/json")
  rcru <- unlist(lapply(rcrd, function(x) ifelse(is.null(x), as.character(NA), x)))
  as.data.frame(t(rcru), stringsAsFactors = FALSE)
}

# Function to get a specific set of fields in a page (form). Returns a dataframe of the fields specified.
getSelectedPageRecords <- function(company_name, profile_id, page_id, fields = fields, limit = 100, offset = 0, access_token) {
  recordlist_uri <- paste0(api_v60_url(company = company_name), profile_id, "/pages/", page_id,
                           "/records?fields=", fields, "&limit=", limit, "&offset=", offset)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::GET(url = recordlist_uri,
                 add_headers('Authorization' = bearer),
                 encode = "json")
  httr::stop_for_status(r)
  rcrd <- content(r, type = "application/json")
  rcrd <- lapply(rcrd, rm_nulls)
  rcrd <- as.data.frame(do.call(rbind, lapply(rcrd, rbind))) # See if double rbind necessary again.
  rcrd <- as_data_frame(rcrd)
  rcrd[] <- lapply(rcrd, unlist)
  rcrd
}

#==============================================================================================
# Functions for data feed. Preferable to use API functions above
#==============================================================================================

# Function to generate url for data feed
dataFeedURL = function(company_name, parent_form_id, profile_id, parent_form_name,
                       since_id, user_label, pw_label) {
  paste0(base_url(company_name), "/exzact/dataScoringJSON.php?",
         "PAGE_ID=", parent_form_id, "&",
         "TABLE_NAME=_data", profile_id, "_", parent_form_name, "&",
         "SINCE_ID=", since_id,"&",
         "USERNAME=", iform_user(user_label), "&",
         "PASSWORD=", iform_pw(pw_label))
}

#==============================================================================================
# General data processing functions for iFormBuilder data
#==============================================================================================

#================================================================================================
# Function to convert date and datetime values from GMT as output by iFormBuilder local.
# Example format:  2014-11-13T15:04:00+00.00
# dts = c("2014-10-13T15:04:03+00:00", "", NA, "2014-11-13T15:04:03+00:00")
# Test either side of DST
# iDateTime(dts)

iDateTime <- function (dts, timezone = "America/Los_Angeles") {
  gmt = character(length(dts))
  for( i in 1:length(dts)) {
    if (is.na(dts[i]) | dts[i] == "") {
      gmt[i] = as.character(NA)
    } else if (nchar(dts[i]) == 25 &
               substr(dts[i], nchar(dts[i]) - 5, nchar(dts[i])) == "+00:00") {
      # Create gmt time value
      gmt[i] <- paste0(substr(dts[i], 1, 10), " ", substr(dts[i], 12, 19))
      pmt <-  as.POSIXct(gmt[i], tz = "GMT")
      pmt <-  with_tz(pmt, timezone)
      gmt[i] <-  format(pmt, format = "%Y-%m-%d %H:%M:%S")
    } else {
      stop("Date or time values are in an unexpected format")
    }
  }
  gmt
}

#================================================================================================
# Function to convert date and datetime values from GMT as output by iFormBuilder local.
# Example format:  Fri Apr 08 2016 08:20:02 GMT-0700 (PDT)
# dts = c("", NA, "Fri Apr 08 2016 08:20:02 GMT-0700 (PDT)", "Fri Feb 08 2016 08:20:02 GMT-0700 (PST)")
# Test either side of DST
# iTextTime(dts)

iTextTime <- function (dts, create_tz = "America/Los_Angeles", timezone = "America/Los_Angeles") {
  gmt = character(length(dts))
  for( i in 1:length(dts)) {
    if (is.na(dts[i]) | dts[i] == "") {
      gmt[i] = as.character(NA)
    } else if (nchar(dts[i]) > 25 & stri_detect_fixed(dts[i], "GMT") == TRUE) {
      # Create gmt time value
      gdt <-  format(as.Date(substr(dts[i], 5, 15), format = "%b %d %Y"))
      gmt[i] <- paste0(gdt, " ", substr(dts[i], 17, 24))
      pmt <-  as.POSIXct(gmt[i], tz = create_tz)
      pmt <-  with_tz(pmt, timezone)
      gmt[i] <-  format(pmt, format = "%Y-%m-%d %H:%M:%S")
    } else {
      stop("Date or time values are in an unexpected format")
    }
  }
  gmt
}

# Function to convert Upper case to capital case. From R base documentation
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# Function to remove nulls from records in a list.
rm_nulls <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}
