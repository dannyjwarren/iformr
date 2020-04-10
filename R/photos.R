#' @title Get photos from form
#'
#' @description
#'     Downloads all of the photos taken with the photo element in a form
#' to a local directory, naming the files based on another field in the form.
#' Since the filenames produced in IFB are jibberish to people, the recommended
#' practice is to include a dynamically calculated element to uniquely identify
#' each photo. For example, in a workflow with a trip form, station subform, and
#'  station_photos subform, the station_photos form would include a photoid
#' field that created unique photoids by concaternation of the trip ID, site ID,
#'  and index of the photo record.
#'
#'     Support is also provided for writing form data to the EXIF data of the
#' downloaded image. This can be useful as images from iFormBuilder have limited
#'  EXIF data. To enable this functionality, provide the full path to
#' exiftool.exe, available from
#' \url{https://www.sno.phy.queensu.ca/~phil/exiftool/} If ExifTool is
#' available, the following form metadata will be added to the image file:
#' \itemize{
#' \item Date the photo was taken (CREATED_DATE field) to #' EXIF tag
#' \emph{CreateDate}
#' \item The iFormBuilder user who took the photo (CREATED_BY field) to EXIF tag
#'  \emph{Artist}
#' \item CREATED_DEVICE_ID field to EXIF tag \emph{CameraSerialNumber}
#' \item CREATED_LOCATION field will be parsed into EXIF tags:
#' \itemize{
#' \item \emph{GPSLatitude} and \emph{GPSLongitude}
#' \item \emph{GPSLatitudeRef} and \emph{GPSLongitudeRef} for N/S and E/W
#' hemispheres respectively.
#' \item \emph{GPSAltitude}
#' \item \emph{GPSAltitudeRef} set to 0 if above sea level or 1 if below.
#' }
#' \item EXIF tag \emph{Software} will be set to "Zerion iFormBuilder"
#' \item If the comment argument is provided, the text from the comment field in
#'  the form will be added to the EXIF tag \emph{ImageDescription}
#'  }
#' @rdname get_photos
#' @author Bill DeVoe, \email{William.DeVoe@@maine.gov}
#' @param server_name String of the iFormBuilder server name.
#' @param profile_id Integer of the iFormBuilder profile ID.
#' @param access_token Access token produced by
#' \code{\link{get_iform_access_token}}
#' @param page_id Integer ID of the form to download photos from.
#' @param photo Character string of the photo element's DCN.
#' @param photoid Character string of the DCN to use as a filename
#' for the photo. Must contain unique values.
#' @param output Path of the output directory for the downloaded photos.
#' @param exif Optional; the path to exiftool.exe. If provided, ExifTool will be
#'  used
#' to add metadata to the image files.
#' @param comment Optional; the DCN of a field to append to the EXIF tag
#' "ImageDescription".
#' @param overwrite Optional; should photos already existing in the download
#' directory be overwritten? Defaults to false.
#' @return Boolean True if successfull.
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
                       exif, comment, overwrite = F) {
  # Make a list of fields to get from the form
  flds <- c(photo, photoid, "created_date", "created_device_id",
            "created_location", "created_by")
  if (!(missingArg(comment))) {flds <- c(flds, comment)}
  fldstr <- paste(flds, collapse = ',')
  # Get all the data from the page
  data <- get_all_records(server_name, profile_id, page_id,
                          access_token = access_token,
                          field_string = fldstr,
                          since_id = 0)
  # Check that photo and photoid columns exist
  if (!(photo %in% colnames(data))) {
    stop(paste0("Form data is missing photo element ",photo))
  }
  if (!(photoid %in% colnames(data))) {
    stop(paste0("Form data is missing photo id ",photoid))
  }
  # Check that photoids are unique
  if (any(is.na(data[[photoid]]))) {
    stop(paste0("Form data has NA values in photoid field ",photoid))
  }
  if ("" %in% data[[photoid]]) {
    stop(paste0("Form data has blank values in photoid field ",photoid))
  }
  if (any(duplicated(data[photoid]))) {
    stop(paste0("Form data has non-unique values in photoid field ",photoid))
  }
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
    # If the file already exists and overwrite = F, skip photo
    if (file.exists(path) && overwrite == F) {next}
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
    desc_text <- ifelse(descrip == T, data[[comment]],
                        "No description available.")
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
    system(cmd)
  }
  return(T)
}
