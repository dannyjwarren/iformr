#'
#' Format a page or element name to be IFB compliant
#'
#' Replaces whitespace and punctuation in an element/form name
#' with _ and converts name to lowercase.
#' Checks name against list of IFB reserved words, appending a '2'
#' after the name if it is in the reserved word list.
#'
#' @rdname format_name
#' @author Bill DeVoe, \email{William.DeVoe@@maine.gov}
#' @param name String of new page or element name.
#' @return IFB compliant name.
#' @export
format_name <- function(name) {
  name <- tolower(gsub('([[:punct:]])|\\s+','_', name))
  if (name %in% reserved_words) {
    warning(paste0(name, " is a reserved word, renaming as ", name, "2"))
    name <- paste0(name, '2')
  }
  return(name)
}
#'
#' Create page
#'
#' Creates a new page in the given profile with the name and label specified.
#' The name provided will be converted to iFormBuilder standards; punctuation
#' and whitespace replaced with _ and all text to lowercase.
#'
#' @rdname create_page
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{iformr::get_iform_access_token}
#' @param name String of new page name; coerced to iFormBuilder table
#'   name conventions.
#' @param label String of the label for the new page.
#' @return Integer of the new page ID.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Create new page
#' new_page_id <- create_page(
#'   server_name = "your_server_name",
#'   profile_id = "your_profile_id",
#'   access_token = access_token,
#'   name = "new_form_name",
#'   label = "New Form Label")
#' }
#' @export
create_page = function(server_name, profile_id, name, label, access_token) {
  # Format page name to be IFB complaint
  name <- format_name(name)
  message(paste0("Creating page: ", name))
  create_page_url <- paste0(api_v60_url(server_name = server_name),
                            profile_id, "/pages")
  bearer <- paste0("Bearer ", access_token)
  page_attributes = paste0('{"name": "',name,'", "label": "',label,'"}')
  r <- httr::POST(url = create_page_url,
                  httr::add_headers('Authorization' = bearer),
                  body = page_attributes,
                  encode = "json")
  httr::stop_for_status(r)
  page_id <- httr::content(r, type = "application/json")
  return(page_id$id)
}

#' Copy page
#'
#' Copies a page to a new page in the profile.
#' @rdname copy_page
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param page_id Integer of the page ID to copy.
#' @param access_token Access token produced by \code{iformr::get_iform_access_token}
#' @return Integer of the new page ID.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Copy page
#' new_page_id <- copy_page(
#'   server_name = "your_server_name",
#'   profile_id = "your_profile_id",
#'   access_token = access_token,
#'   page_id = "existing_page_id"
#' }
#'
#' @export
copy_page = function(server_name, profile_id, page_id, access_token) {
  copy_page_url <- paste0(api_v60_url(server_name = server_name),
                          profile_id, "/pages/", page_id)
  bearer <- paste0("Bearer ", access_token)
  r <- httr::VERB(verb="COPY", url = copy_page_url,
                  httr::add_headers('Authorization' = bearer),
                  encode = "json")
  httr::stop_for_status(r)
  new_page_id <- httr::content(r, type = "application/json")$id
  return(new_page_id)
}

#' Rename page
#'
#' Renames a page given a page_id and new name and label. The name provided
#' will be converted to iFormBuilder standards; punctuation and whitespace
#' replaced with _ and all text to lowercase.
#'
#' @rdname rename_page
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{iformr::get_iform_access_token}
#' @param page_id Integer of the page ID to rename.
#' @param name String of renamed page name; coerced to iFormBuilder
#'   table name conventions.
#' @param label String of the renamed page label.
#' @return Integer of the page ID.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Rename page
#' rename_page_id <- rename_page(
#'   server_name = "your_server_name",
#'   profile_id = "your_profile_id",
#'   access_token = access_token,
#'   page_id = "existing_page_id",
#'   name = "new_page_name",
#'   label = "new_page_label")
#'   }
#' @export
rename_page = function(server_name, profile_id, page_id,
                       name, label, access_token) {
  # Format new page name as IFB compliant
  name <- format_name(name)
  rename_page_url <- paste0(api_v60_url(server_name = server_name),
                            profile_id, "/pages/", page_id)
  bearer <- paste0("Bearer ", access_token)
  page_attributes = paste0('{"name": "', name, '", "label": "', label, '"}')
  r <- httr::PUT(url = rename_page_url,
                 httr::add_headers('Authorization' = bearer),
                 body = page_attributes,
                 encode = "json")
  httr::stop_for_status(r)
  returned_page_id <- httr::content(r, type = "application/json")$id
  return(returned_page_id)
}

#' Delete page
#'
#' Deletes a single page (form) from a profile. Use caution when deleting forms.
#' It should only be done with consideration for existing data referencing the
#' form.
#'
#' @rdname delete_page
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param page_id ID of the option list to be deleted.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @return ID of the page that was deleted.
#' @examples
#' \dontrun{
#'
#' # Pull out ID of page to delete
#' page_to_delete = forms_list$id[forms_list$name == "test_form_p"]
#'
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Delete option list
#' deleted_page_id <- delete_page(
#'   server_name = "your_server_name",
#'   profile_id = "your_profile_id",
#'   page_id,
#'   access_token = access_token)
#' }
#'
#' @export
delete_page <- function(server_name, profile_id,
                        page_id, access_token) {
  delete_page_uri <- paste0(api_v60_url(server_name = server_name),
                            profile_id, "/pages/", page_id)
  bearer <- paste0("Bearer ", access_token)
  # No body, DELETE HTTP method
  r <- httr::DELETE(url = delete_page_uri,
                    httr::add_headers('Authorization' = bearer),
                    encode = "json")
  httr::stop_for_status(r)
  response <- httr::content(r, type = "application/json")$id
  return(response)
}


#' Form metadata
#'
#' Builds a Markdown document containing metadata for a given
#' iFormBuilder form by querying the API for page and element
#' level information. By utilizing the description fields during
#' form building, detailed metadata can be built afterward using
#' this function.
#'
#' @rdname form_metadata
#' @author Bill DeVoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by
#' \code{\link{get_iform_access_token}}
#' @param page_id ID of the form to get metadata from.
#' @param filename Filename of the output Markdown file.
#' @param subforms **Optional** - Indicates if metadata should be generated for
#' subforms.
#' Defaults to True.
#' @param sub  **Optional** - Defaults to False. Used by function to
#' self-reference and append subform metadata to beginning file.
#' @return Add this later.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Create metadata for form.
#' form_metadata(server_name, profile_id, access_token,
#'                 page_id = 012345, filename = "metadata", subforms = T)
#' }
#' @export
#' @import tidyr
#' @import knitr
#' @import dplyr
form_metadata <- function(server_name, profile_id, access_token,
                          page_id, filename, subforms=T, sub=F) {
  # If not appending subform data to an existing file
  if (sub == F) {
    # Add Markdown extension if it was not provided
    if (!(endsWith(filename, ".md")) || !(endsWith(filename, ".Rmd"))) {
      filename <- paste0(filename,".md")
    }
    # Create/overwrite output file
    file.create(filename)
  }
  # Get metadata for page
  page <- retrieve_page(server_name, profile_id, access_token, page_id)
  elements <- retrieve_element_list(server_name, profile_id,
                                    access_token, page_id)
  # Convert date columns
  page$created_date <- idate_time(page$created_date, Sys.timezone())
  page$modified_date <- idate_time(page$modified_date, Sys.timezone())
  elements$created_date <- idate_time(elements$created_date, Sys.timezone())
  elements$modified_date <- idate_time(elements$modified_date, Sys.timezone())
  # Convert data type to label using data_types from sysdata.rda
  elements$data_type <- unlist(data_types[as.character(elements$data_type)],
                               use.names = F)
  # Replace option list IDs with option list name
  # TODO: Add this.
  # Replace blank fields with NA so they will not be added to metadata
  elements[elements == ''] <- NA
  # Blank vector for subform IDs
  subs <- c()
  # Open md file connection
  conn <- file(filename, 'a')
  # Write form title
  if (sub == F) {
    cat("# Parent Form: ",page$label,"\n",file=conn)
  }
  else {
    cat("# Sub Form: ",page$label,"\n",file=conn)
  }
  # Collapse page detail list to table
  page_data <- do.call(rbind, page)
  page_data <- data.frame(Value=page_data[,1])
  # Write page table to md file
  md <- knitr::kable(page_data, format = 'markdown')
  cat(md, sep = "\n", file = conn)
  # For each element, write a table to the md document
  cat("## Element Details\n", file = conn)
  for (row in 1:nrow(elements)) {
    # Element label
    label <- elements[row, 'label']
    # Element type
    type <- elements[row, 'data_type']
    # If a subform, append subform page id (data_size) to subform list
    if (type == 'Subform') {
      subs <- c(subs, elements[row, 'data_size'])
    }
    # Subsample element dataframe to element
    element <- elements[row,]
    # Gather element columns to rows
    field <- tidyr::gather(element, key = "Attribute", value = "Value",
                           na.rm = T, convert = FALSE, factor_key = FALSE)
    rownames(field) <- c()
    # Write to markdown file
    cat("### ", label, "\n", file = conn)
    md <- knitr::kable(field, format = 'markdown')
    cat(md, sep = "\n", file = conn)
    cat("\n", file = conn)
  }
  # Self-reference function to build subform metadata
  for (sub in subs) {
    form_metadata(server_name, profile_id, access_token,
                  page_id=sub, filename, subforms = T, sub = T)
  }
  # Close file connection
  close(conn)
}

#' Create form from dataframe
#'
#' Creates a form based on a dataframe. Dataframe classes are cast as
#' element types in the form.
#'
#' @rdname data2form
#' @author Bill DeVoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by
#' \code{\link{get_iform_access_token}}
#' @param name String of new page name; coerced to iFormBuilder
#'   table name conventions.
#' @param label String of the label for the new page.
#' @param data A dataframe whose structure will be used to
#'   create the new form.
#' @return The page ID of the created form.
#' @examples
#' # Create a dataframe with some basic form fields
#' dat = tibble::tibble(survey_id = NA_integer_,
#'                      survey_datetime = as.POSIXct(NA, tz = "UTC"),
#'                      surveyor = NA_character_,
#'                      start_point = NA_real_,
#'                      fish_species = NA_integer_,
#'                      fish_count = NA_integer_,
#'                      end_point = NA_real_,
#'                      comment_text = NA_character_,
#'                      survey_completed = TRUE)
#'
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Create new form from dataframe
#' new_form <- data2form(
#'   server_name = "your_server_name",
#'   profile_id = "your_profile_id",
#'   access_token = access_token,
#'   name = "new_form_to_create",
#'   label = "New form based on an R dataframe",
#'   data = dat)
#' }
#'
#' @export
data2form = function(server_name, profile_id, access_token,
                     name, label, data) {
  # Format page name to be IFB compliant
  name <- format_name(name)
  # Format data column names to be IFB compliant
  names(data) <- sapply(names(data), format_name)
  # Create empty form
  page_id <- create_page(server_name, profile_id, access_token, name, label)
  # Get field classes of input data
  field_classes <- sapply(data, class)
  # List mapping data classes to IFB element types
  ifb_types <- list("character" = 1, "numeric" = 2, "integer" = 2,
                    "double" = 2, "POSIXct" = 5, "logical" = 6)
  # For each field in input data
  for (field in names(field_classes)) {
    # Class of field
    class <- field_classes[[field]][1]
    # ifb element type for field
    data_type <- ifb_types[[class]]
    # Label as proper case
    label <- gsub('_',' ', field)
    label <- stringr::str_to_title(label)
    # Add element to page
    create_element(server_name, profile_id, access_token, page_id,
                   name=field, label, description="", data_type)
  }
  return(page_id)
}

#' @title Truncate form
#'
#' @description Removes all records from a page, leaving the page structure.
#' USE WITH CAUTION!
#'
#' @rdname truncate_form
#' @author Bill DeVoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by
#' \code{\link{get_iform_access_token}}
#' @param page_id Integer ID of the form to truncate.
#' @return Boolean True if succesful.
#' @export
truncate_form <- function(server_name, profile_id,
                          access_token, page_id) {
  # Get all record IDs from the form
  record_ids <- get_all_records(server_name, profile_id, page_id,
                                fields = "fields", limit = 1000,
                                offset = 0, access_token, field_string = "id",
                                since_id = 0)
  record_ids <- record_ids$id
  # Delete them all
  delete_records(server_name, profile_id, access_token, page_id, record_ids)
}


