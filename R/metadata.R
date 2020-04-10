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
