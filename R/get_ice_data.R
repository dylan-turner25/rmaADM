#' Download and clean Insurance Control Elements tables
#'
#' @description
#' `get_ice_data()` retrieves all “YTD” ICE (Insurance Control Elements) text files
#' from the specified directory on the RMA public FTP site for one or more years,
#' downloads them to a temporary location, reads them as pipe-delimited data,
#' applies internal cleaning routines, and returns the combined dataset. Original
#' text files are discarded after reading.
#'
#' @param years
#'   Integer vector of calendar years to download (e.g. `2012:2020`). Defaults to `2012`.
#'
#' @param ice_url
#'   Character string giving the base URL of the ICE directory on the RMA FTP site.
#'   Must end with a slash. Defaults to
#'   `"https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/"`.
#'
#' @param selected_ice
#'   Character vector of keyword(s) or regular expressions to filter the filenames.
#'   Only ICE files whose names match at least one element of `selected_ice` will be
#'   downloaded. If `NULL`, all “YTD” files are processed.
#'
#' @return
#' A single `data.table` (invisibly coercible to `data.frame`) containing the cleaned
#' ICE data for all requested years. If no matching files are found or all downloads
#' fail, returns an empty `data.table`.
#'
#'
#' @import dplyr
#' @importFrom stringr str_extract str_match_all
#' @importFrom data.table rbindlist
#' @importFrom utils download.file
#' @export
#'
#' @examples
#' \dontrun{
#' # Download & process ICE tables for 2018 and 2019,
#' # filtering for any file with “IceAOExpenseSubsidy” in its name
#' ice_df <- get_ice_data(
#'   years        = 2018:2019,
#'   selected_ice = "IceAOExpenseSubsidy"
#' )
#' }
get_ice_data <- function(
    years        = 2012,
    ice_url      = "https://pubfs-rma.fpac.usda.gov/pub/References/insurance_control_elements/PASS/",
    selected_ice = NULL) {
  dt <- data.table::rbindlist(
    lapply(years, function(year) {
      tryCatch({
        ## locate and filter links ending in "YTD.txt"
        download_links <- locate_download_link(
          year        = year,
          ice_url     = ice_url,
          data_source = "ice"
        ) %>% unlist(use.names = FALSE)
        download_links <- grep("YTD\\.txt$", download_links, value = TRUE)

        ## filter by user-supplied patterns, if any
        if(!is.null(selected_ice)) {
          pattern <- paste(selected_ice, collapse = "|")
          download_links <- grep(pattern, download_links, value = TRUE)
        }

        dt  <-  data.table::rbindlist(
          lapply(download_links, function(download_link) {
            tryCatch({
              ## download, read, clean
              tmp <- tempfile(fileext = ".txt")
              utils::download.file(download_link, destfile = tmp, mode = "wb")
              dt  <- readr::read_delim(tmp, delim = "|",col_names = TRUE, show_col_types = FALSE)
              dt  <- clean_data(dt)
              dt
              }, error = function(e) {NULL})
          }),fill = TRUE)

      }, error = function(e) {NULL})
    }),fill = TRUE)
  gc()
  return(dt)
}
