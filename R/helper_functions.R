#' Sync table
#'
#' Syncs a dataframe with the contents of an IFB page. If the page does
#' not yet exist, it will be created and populated with the source data.
#'
#' An example use case is syncing an existing database table with a
#' Smart Table Search form in IFB. All columns in the source data
#' with matching columns in the form data are synced. Data is synced
#' in a parent-to-child fashion:
#' - Rows in the source data not in the form data are added (based on
#' the unique identifier from the \code{uid} argument.)
#' - Rows in the form data that are not present in the source data
#' can be optionally removed using the \code{delete} argument.
#' - Rows in the form data are updated if any of their fields differ
#' from the matching row in the source data. Updating can be disabled
#' by passing False to the \code{update} argument.
#'
#' @rdname sync_table
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @param data A dataframe containing the data to be synced with the page.
#' @param form_name The name of a page to sync the source data to; if the page does
#' not exist, it will be created.
#' @param label *Optional* - String of the label to be used if a new page is created.
#' If a label is not provided and a new page is created, the form_name argument will
#' be used to create a page label.
#' @param uid The name of the column in the source and IFB data that uniquely identifies
#' a record.
#' @param update *Optional* Defaults to True - If True, records in the form data
#' will be updated if the matching record in the source data is different.
#' @param delete *Optional* Defaults to False - If True, records in the form data
#' not present in the source data will be removed.
#' @return The page ID of the existing or created form.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Add new data to form
#' sync_table(server_name, profile_id, access_token,
#'   data = "new_dataframe", form_name = "my_form",
#'   uid = "unique_id_col")
#' }
#' @export
#' @import methods
#' @import dplyr
sync_table <- function(server_name, profile_id, access_token,
                       data, form_name, label, uid, update = T,
                       delete = F){
  # Convert the dataframe to be Smart Table search friendly - all columns character
  data <- dplyr::mutate_all(data, as.character)
  # Format input data column names to be IFB compliant
  names(data) <- sapply(names(data), format_name)
  # Get a list of all the pages in the profile
  page_list <- get_all_pages_list(server_name, profile_id, access_token)
  # Form name to IFB compliant name
  form_name <- format_name(form_name)
  # If the form does not exist, create it
  if (form_name %in% page_list$name == F){
    message("Form ",form_name," does not yet exist.")
    # If form label has not been provided, create a label from the form name
    if (methods::missingArg(label)) {
      label <- stringr::str_to_title(gsub('_',' ', form_name))
    }
    # Create page for table data
    page_id <- data2form(server_name, profile_id, access_token,
                                 name = form_name, label, data)
  }
  else {
    # Get id of existing page
    page_id <- page_list[page_list$name == form_name ,]$id
  }
  # If something goes wrong with request
  stopifnot(page_id > 0)
  # Fields names in source table
  src_flds <- names(data)
  # Get list of elements in existing page
  ifb_flds <- retrieve_element_list(server_name, profile_id, access_token,
                                            page_id, fields = 'name')
  # Fields in page from elements dataframe
  ifb_flds <- ifb_flds$name
  # Fields in source table also in page
  flds <- intersect(src_flds, ifb_flds)
  # Fields in both tables collapsed to string
  fldstr <- paste(flds, collapse = ',')
  # Check that UID column is in both datasets
  uid <- tolower(uid)
  if (!(uid %in% src_flds))
  {stop(paste0("UID column ",uid," is missing from source data."))}
  if (!(uid %in% ifb_flds))
  {stop(paste0("UID column ",uid," is missing from IFB data."))}
  # Check that UID is unique and non-null in incoming data
  if (any(is.na(data[[uid]]))) {stop(paste0("Input data has NA values in UID column ",uid))}
  if ("" %in% data[[uid]]) {stop(paste0("Input data has blank values in UID column ",uid))}
  if (any(duplicated(data[[uid]]))) {stop(paste0("Input data has duplicate values in UID column ",uid))}
  # Pull all data in IFB table
  i_data <- get_all_records(server_name, profile_id, page_id, fields = "fields",
                                    limit = 1000, offset = 0, access_token,
                                    field_string = fldstr, since_id = 0)
  # If there is data in IFB
  if (nrow(i_data) > 0) {
    # Find data in source table that is not in IFB table
    new_data <- dplyr::anti_join(data, i_data, by=uid)
    # Remove columns from source table not in IFB table
    new_data <- new_data[ , (names(new_data) %in% flds)]
  }
  else {
    new_data <- data
  }
  # Upload new data to IFB
  message(paste0(nrow(new_data), " new records will be added to ",form_name))
  if (nrow(new_data) > 0) {
    upload <- create_new_records(server_name, profile_id, page_id,
                               access_token, record_data = new_data)
  }
  # Remove data from IFB if delete option is true
  if (delete == T && nrow(i_data) > 0) {
    # Refresh table of data from IFB since data
    # may have been added
    i_data <- get_all_records(server_name, profile_id, page_id, fields = "fields",
                              limit = 1000, offset = 0, access_token,
                              field_string = fldstr, since_id = 0)
    # UIDs in form data NOT in source data
    del_data <- dplyr::anti_join(i_data, data, by=uid)
    # Delete records
    message(paste0(nrow(del_data), " records will be removed from ",form_name))
    if (nrow(del_data) != 0) {
    del <- delete_records(server_name, profile_id,
                                  access_token, page_id,
                                  record_ids = del_data$id)
    }
  }
  # Update data in IFB if update option true
  if (update == T) {
    # Natural anti-join gets all records where fields do not match
    #up_data <- dplyr::anti_join(data, i_data)
    # Filter by unique uid
    #up_data <- dplyr::distinct(up_data, uid)
    #message(paste0(nrow(up_data), " records will be updated in ",form_name))
    #TODO: call to update data
    message("Update functionality has not yet been implemented...")
  }
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
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @param page_id ID of the form to get metadata from.
#' @param filename Filename of the output Markdown file.
#' @param subforms **Optional** - Indicates if metadata should be generated for subforms.
#' Defaults to True.
#' @param sub  **Optional** - Defaults to False. Used by function to self-reference
#' and append subform metadata to beginning file.
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
  elements <- retrieve_element_list(server_name, profile_id, access_token, page_id)
  # Convert date columns
  page$created_date <- idate_time(page$created_date, Sys.timezone())
  page$modified_date <- idate_time(page$modified_date, Sys.timezone())
  elements$created_date <- idate_time(elements$created_date, Sys.timezone())
  elements$modified_date <- idate_time(elements$modified_date, Sys.timezone())
  # Convert data type to label using data_types from sysdata.rda
  elements$data_type <- unlist(data_types[as.character(elements$data_type)], use.names = F)
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
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
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



#' Format a page or element name to be IFB compliant
#'
#' Replaces whitespace and punctuation in an element/form name
#' with _ and converts name to lowercase.
#' Checks name against list of IFB reserved words, appending a '2'
#' after the name if it is in the reserved word list.
#'
#' @rdname format_name
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param name String of new page or element name.
#' @return IFB compliant name.
#' @export
format_name <- function(name) {
  name <- tolower(gsub('([[:punct:]])|\\s+','_', name))
  if (name %in% reserved_words) {
    warning(paste0(name," is a reserved word, renaming as ",name,"2"))
    name <- paste0(name,'2')
    }
  return(name)
}

#' @title Get photos from form
#'
#' @description
#'     Downloads all of the photos taken with the photo element in a form
#' to a local directory, naming the files based on another field in the form.
#' Since the filenames produced in IFB are jibberish to people, the recommended practice
#' is to include a dynamically calculated element to uniquely identify each photo.
#' For example, in a workflow with a trip form, station subform, and station_photos
#' subform, the station_photos form would include a photoid field that created unique
#' photoids by concaternation of the trip ID, site ID, and index of the photo record.
#'
#'     Support is also provided for writing form data to the EXIF data of the downloaded
#' image. This can be useful as images from iFormBuilder have limited EXIF data.
#' To enable this functionality, provide the full path to exiftool.exe, available
#' from \url{https://www.sno.phy.queensu.ca/~phil/exiftool/} If ExifTool is available,
#' the following form metadata will be added to the image file:
#' \itemize{
#' \item Date the photo was taken (CREATED_DATE field) to EXIF tag \emph{CreateDate}
#' \item The iFormBuilder user who took the photo (CREATED_BY field) to EXIF tag \emph{Artist}
#' \item CREATED_DEVICE_ID field to EXIF tag \emph{CameraSerialNumber}
#' \item CREATED_LOCATION field will be parsed into EXIF tags:
#' \itemize{
#' \item \emph{GPSLatitude} and \emph{GPSLongitude}
#' \item \emph{GPSLatitudeRef} and \emph{GPSLongitudeRef} for N/S and E/W hemispheres respectively.
#' \item \emph{GPSAltitude}
#' \item \emph{GPSAltitudeRef} set to 0 if above sea level or 1 if below.
#' }
#' \item EXIF tag \emph{Software} will be set to "Zerion iFormBuilder"
#' \item If the comment argument is provided, the text from the comment field in the form
#'  will be added to the EXIF tag \emph{ImageDescription}
#'  }
#' @rdname get_photos
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @param page_id Integer ID of the form to download photos from.
#' @param photo Character string of the photo element's DCN.
#' @param photoid Character string of the DCN to use as a filename
#' for the photo. Must contain unique values.
#' @param output Path of the output directory for the downloaded photos.
#' @param exif Optional; the path to exiftool.exe. If provided, ExifTool will be used
#' to add metadata to the image files.
#' @param comment Optional; the DCN of a field to append to the EXIF tag "ImageDescription".
#' @return Boolean True if succesful.
#' @examples
#' \dontrun{
#' # Get access_token
#' access_token <- get_iform_access_token(
#'   server_name = "your_server_name",
#'   client_key_name = "your_client_key_name",
#'   client_secret_name = "your_client_secret_name")
#'
#' # Download photos from a form
#'
#' }
#' @export
get_photos <- function(server_name, profile_id, access_token,
                       page_id, photo, photoid, output,
                       exif, comment) {
  # Make a list of fields to get from the form
  flds <- c(photo, photoid, "created_date", "created_device_id",
            "created_location", "created_by")
  if (!(missingArg(comment))) {flds <- c(flds, comment)}
  fldstr <- paste(flds, collapse = ',')
  # Get all the data from the page
  data <- get_all_records(server_name, profile_id, page_id,
                          access_token = access_token, field_string = fldstr, since_id = 0)
  # Check that photo and photoid columns exist
  if (!(photo %in% colnames(data))) {stop(paste0("Form data is missing photo element ",photo))}
  if (!(photoid %in% colnames(data))) {stop(paste0("Form data is missing photo id ",photoid))}
  # Check that photoids are unique
  if (any(is.na(data[[photoid]]))) {stop(paste0("Form data has NA values in photoid field ",photoid))}
  if ("" %in% data[[photoid]]) {stop(paste0("Form data has blank values in photoid field ",photoid))}
  if (any(duplicated(data[photoid]))) {stop(paste0("Form data has non-unique values in photoid field ",photoid))}
  # Check that output path exists, and if not create it
  if (!(dir.exists(output))) {dir.create(file.path(output))}
  # Flag for if EXIF exists
  exifmeta <- ifelse(!(missingArg(exif)) && file.exists(exif), T, F)
  # Flag for comment
  descrip <- ifelse(!(missingArg(comment)) && comment %in% names(data), T, F)
  # Download each photo from the photo DCN and name it with the photoid
  for (row in 1:nrow(data)) {
    url <- data[[photo]][row]
    filename <- paste0(data[[photoid]][row],'.jpg')
    path <- file.path(output, filename, fsep = "\\")
    # Download image file
    try(download.file(url, path, quiet = FALSE, mode = "wb"))
    # Next image if EXIF not used
    if (exifmeta == F) {next}
    # Next image if photo did not download successfully
    #if (!(exists(path))) {next}
    # Metadata fields
    created <- data[["created_date"]][row]
    device <- data[["created_device_id"]][row]
    user <- data[["created_by"]][row]
    loc <- strsplit(data[["created_location"]][row], split = ":")[[1]]
    # If loc is blank
    if (length(loc) < 1) {loc <- c(0,0,0)}
    message(loc)
    desc_text <- ifelse(descrip == T, data[[comment]], "No description available.")
    # Parse location to invidiual variables
    lat <- as.numeric(loc[1])
    lon <- as.numeric(loc[2])
    hemi1 <- ifelse(lat > 0, "N", "S")
    hemi2 <- ifelse(lon > 0, "E", "W")
    lat <- abs(lat)
    lon <- abs(lon)
    alt <- as.numeric(loc[3])
    altref <- ifelse(alt > 0, "above", "below")
    # Append to photo metadata
    cmd <- sprintf('"%s" -GPSLongitudeRef="%s" -GPSLongitude="%s"
                   -GPSLatitudeRef="%s" -GPSLatitude="%s"
                   -CreateDate="%s" -Artist="%s"
                   -CameraSerialNumber="%s" -GPSAltitude="%s"
                   -GPSAltitudeRef="%s" -Software="Zerion iFormBuilder"
                   -GPSMapDatum="WGS-84" -ImageDescription="%s"
                   -overwrite_original
                   "%s"',
                   exif, hemi2, lon, hemi1, lat, created,
                   user, device, alt, altref, desc_text, path)
    message(cmd)
    system(cmd)
  }
  return(T)
}

#' @title Truncate form
#'
#' @description Removes all records from a page, leaving the page structure.
#' USE WITH CAUTION!
#'
#' @rdname truncate_form
#' @author Bill Devoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by \code{\link{get_iform_access_token}}
#' @param page_id Integer ID of the form to truncate.
#' @return Boolean True if succesful.
#' @export
truncate_form <- function(server_name, profile_id,
                          access_token, page_id) {
  # Get all record IDs from the form
  record_ids <- get_all_records(server_name, profile_id, page_id, fields = "fields",
                  limit = 1000, offset = 0, access_token,
                  field_string = "id", since_id = 0)
  record_ids <- record_ids$id
  # Delete them all
  delete_records(server_name, profile_id, access_token, page_id, record_ids)
}



