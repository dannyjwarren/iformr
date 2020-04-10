#
#' @title Update iFormBuilder option lists
#'
#' @description For an input dataframe of option lists, adds new option lists
#' to the profile, adds new options to existing option lists, and updates
#' condition_value for existing options.
#'
#' @rdname update_option_lists
#' @param access_token Access token produced by
#' \code{\link[iformr]{get_iform_access_token}}
#' @param server_name String of the iFormBuilder server name
#' @param profile_id Integer of the iFormBuilder profile ID
#' @param option_lists A dataframe containing option lists withname, key_value,
#' label, condition_value, and sort_order.
#' @return No return value.
#' @export
#' @import dplyr
#' @import jsonlite
#' @importFrom methods missingArg
update_option_lists <- function(access_token, server_name,
                                profile_id, option_lists) {
  # Column names of option list df to lowercase
  colnames(option_lists) <- tolower(colnames(option_lists))
  # Check incoming options for correct columns
  option_flds <- c("name", "key_value", "label",
                   "condition_value", "sort_order")
  for (fld in option_flds) {
    if (!(fld %in% colnames(option_lists))) {
      stop(paste0(fld, " column is missing from option_lists dataframe."))
    }
  }
  # Select only option list columns
  option_lists <- dplyr::select(option_lists,
                                name,
                                key_value,
                                label,
                                condition_value,
                                sort_order)
  # Truncate key_value and label to 100 characters (IFB max)
  option_lists$key_value <- strtrim(option_lists$key_value, 100)
  option_lists$label <- strtrim(option_lists$label, 100)
  # Convert option list name to be IFB friendly
  option_lists$name <- format_name(option_lists$name)
  # Vector of unique option list names
  option_list_names <- unique(option_lists$name)
  # Get all option lists from IFB
  ifb_option_lists <- get_all_option_lists(server_name = server_name,
                                           profile_id = profile_id,
                                           access_token = access_token)
  # For each incoming option list name, check if an option list with the same
  # name exists in the IFB profile. If no, create it, if yes update the
  # existing list.
  for (name in option_list_names){
    # Subset of options in option_lists df
    options <- option_lists[option_lists$name == name,]
    # Option list already exists in IFB profile - update it
    if (name %in% ifb_option_lists$name) {
      #message("Option list already exists: ",name)
      # Get list id
      listid <- ifb_option_lists[ifb_option_lists$name == name,]$id
      # Try to get options from IFB option list, if there are any
      test <- try(
        ifb_options <- get_core_option_list_elements(server_name = server_name,
                                                     profile_id = profile_id,
                                                     optionlist_id = listid,
                                                     limit = 1000,
                                                     offset = 0,
                                                     access_token = access_token)
      )
      # If the option list was blank and threw an error, all incoming options
      # are new options
      if("try-error" %in% class(test)) {new_options <- options}
      # Else get incoming options not in i_options
      else {
        new_options <- dplyr::anti_join(options, ifb_options, by="key_value")}
      # If new options exist, upload them
      if (nrow(new_options) > 0) {
        # New options to JSON
        new_options = jsonlite::toJSON(new_options, auto_unbox = TRUE)
        # Append new options to option list
        message("Adding new options to ",name)
        try(add_options_to_list(server_name = server_name,
                                profile_id = profile_id,
                                optionlist_id=listid,
                                option_values=new_options,
                                access_token = access_token))
      }
      ## Update option lists condition value
      # Join options from incoming options to option from IFB on key_value
      join <- dplyr::inner_join(options, ifb_options, by="key_value")
      # Options that are different in MARVIN from IFB
      options_changed <- join[which(join$condition_value.x !=
                                      join$condition_value.y),]
      # If there are no option updates, skip to next code
      if (nrow(options_changed) < 1) {next}
      message("Updating options in ",name)
      # Select and rename fields for new option values
      options_changed <- dplyr::select(options_changed,
                                       key_value = id,
                                       label = label.x,
                                       condition_value = condition_value.x,
                                       sort_order = sort_order.x)
      # Convert to JSON for update
      updated_options_json <- jsonlite::toJSON(options_changed,
                                               auto_unbox = TRUE)
      # Commit option list updates to API
      try(iformr::update_options_in_list(server_name = server_name,
                                         profile_id = profile_id,
                                         optionlist_id = listid,
                                         option_values = updated_options_json,
                                         fields = "fields",
                                         limit = 1000,
                                         offset = 0,
                                         access_token = access_token))
    }
    else {
      # Create new option list and populate it
      message("Creating new option list: ",name)
      listid <- create_new_option_list(server_name = server_name,
                                       profile_id = profile_id,
                                       option_list_name = name,
                                       access_token = access_token)
      # If list ID is valid
      if (listid > 0) {
        # Convert options to to JSON
        new_options = jsonlite::toJSON(options, auto_unbox = TRUE)
        # Append new options to new option list
        try(add_options_to_list(server_name = server_name,
                                profile_id = profile_id,
                                optionlist_id = listid,
                                option_values = new_options,
                                access_token = access_token)
        )
      }
      else {message("Option list ",name," could not be created.")}
    }
  }
}



#' Sync table
#'
#' Syncs a dataframe with the contents of an IFB page. If the page does
#' not yet exist, it will be created and populated with the source data. An
#' example use case is syncing an existing database table with a
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
#' @author Bill DeVoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by
#' \code{\link{get_iform_access_token}}
#' @param data A dataframe containing the data to be synced with the page.
#' @param form_name The name of a page to sync the source data to; if the page
#' does not exist, it will be created.
#' @param label *Optional* - String of the label to be used if a new page is
#' created.
#' If a label is not provided and a new page is created, the form_name argument
#' will be used to create a page label.
#' @param uid The name of the column in the source and IFB data that uniquely
#' identifies a record.
#' @param update *Optional* Defaults to True - If True, records in the form data
#' will be updated if the matching record in the source data is different.
#' @param delete *Optional* Defaults to False - If True, records in the form
#' data not present in the source data will be removed.
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
  # Convert the dataframe to be Smart Table search friendly - all columns char
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
  if (any(is.na(data[[uid]]))) {
    stop(paste0("Input data has NA values in UID column ",uid))
  }
  if ("" %in% data[[uid]]) {
    stop(paste0("Input data has blank values in UID column ",uid))
  }
  if (any(duplicated(data[[uid]]))) {
    stop(paste0("Input data has duplicate values in UID column ",uid))
  }
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
    i_data <- get_all_records(server_name, profile_id, page_id,
                              fields = "fields",
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
    message("Update functionality has not yet been implemented...")
    # Natural anti-join gets all records where fields do not match
    up_data <- dplyr::anti_join(data, i_data)
    message(paste0(nrow(up_data), " records will be updated in ",form_name))
    up_data
    #TODO: call to update data
  }
}
