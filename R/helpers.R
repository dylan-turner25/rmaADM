
#' Locate the download link for the actuarial data master
#'
#' @param year the year of the actuarial data master to download
#' @param adm_url the url where the adm FTP site is
#'
#' @returns a list of the data and layout file urls with the time the file was last updated on RMA's server
#'
#' @importFrom stringr str_match_all str_extract
#' @import dplyr
#'
#' @examples \dontrun{locate_download_link(year = 2012)}
locate_download_link <- function(year = 2012,
                                 adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/"){

  # read in the webpage
  html <- suppressWarnings(paste0(readLines(adm_url), collapse = "\n"))

  # locate all the links
  links <- as.character(data.frame(stringr::str_match_all(html,
                                                          "href=\"(.*?)\""))[, 1])

  # get the link with the matching year
  link <- links[grepl(year,links)]
  link <- link[!grepl("test",link)]

  # apply some cleaning opperations
  link <- gsub("href=\"", "", link)
  link <- gsub("\"", "", link)
  link <- gsub("\\./", "", link)
  link <- paste0(adm_url, link)

  # navigate the the cleaned link to get the correct sublink
  html <- suppressWarnings(paste0(readLines(link), collapse = "\n"))

  # Extract the <pre> block where the file info resides
  pre_block <- str_extract(html, "<pre>.*?</pre>")

  # Extract date, time, size, and filename from each line using regex
  matches <- str_match_all(
    pre_block,
    "(\\d{2}/\\d{2}/\\d{4})\\s+(\\d{2}:\\d{2}\\s+[AP]M)\\s+(\\d+)\\s+<a href=\"\\.\\/(.*?)\">"
  )[[1]]

  # Convert to data frame
  file_info <- data.frame(
    date = matches[, 2],
    time = matches[, 3],
    size_bytes = as.numeric(matches[, 4]),
    filename = matches[, 5],
    stringsAsFactors = FALSE
  )


  # combine date and time into a single POSIXct column
  file_info <- file_info %>%
    mutate(
      datetime = as.POSIXct(paste(date, time), format = "%m/%d/%Y %I:%M %p", tz = "EST")
    )

  # filter the file info to only include the data and layout files
  file_info <- file_info %>%
    filter(grepl("ytd|layout", tolower(filename)))


  # add the base url and year to the file names
  file_info <- file_info %>%
    mutate(filename = paste0(adm_url,year,"/",filename))

  # convert the links to a list and name them
  links <- as.list(file_info$filename)

  # name the link that contains "YTD" as "data"
  names(links)[which(grepl("YTD|ytd",links))] <- "data"

  # name the link that contains "Layout" as "layout"
  names(links)[which(grepl("Layout|layout",links))] <- "layout"

  # add the update date to the links
  links$update_date <- file_info$datetime[grepl("YTD",file_info$filename)]

  # unlist layout and data links
  links$data <- unlist(links$data)
  links$layout <- unlist(links$layout)

  # return the links
  return(links)

}



#' Download the adm files for a given year
#'
#' This function downloads the raw adm files, applies some minor cleaning operations,
#' and converts them to .rds format for better compression.
#'
#' @param years the years of the actuarial data master to download
#' @param adm_url the url where the adm FTP site is
#' @param dir the directory to save the files to
#' @param helpers_only if TRUE, only keeps the helper files (i.e. files smaller than 1 mb)
#' @param helpers_size_threshold If `helpers_only` is `TRUE`, `helpers_size_threshold` indicates the size (in mb) above which data sets are kept. I.e. anything below `helpers_size_threshold` is assumed to be a helper dataset.
#' @param keep_source_files if TRUE, keeps the original zip files in the year directory. If FALSE, they will be deleted.
#' @returns the data and layout files for the given year
#' @importFrom utils download.file unzip
#' @importFrom readr read_delim
#' @import cli
#'
#' @examples \dontrun{download_adm(year = 2012)}
download_adm <- function(years = 2012,
                         adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/",
                         dir = "./data-raw",
                         helpers_only = TRUE,
                         helpers_size_threshold = 5,
                         keep_source_files = FALSE){
  # if dir directory doesn't exist, create it
  if(!dir.exists(dir)) {
    dir.create(dir)
  }


  # loop over each year
  for(year in years){

    # create a year directory if it doesn't already exist
    if(!dir.exists(paste0(dir,"/",year))) {
      dir.create(paste0(dir,"/",year))
    }


    # check if there is already a file in the dir
    if (dir.exists(paste0(dir,"/",year))) {
      # get the list of files in the directory
      files <- list.files(paste0(dir,"/",year), full.names = TRUE)

      # if there are no files, set last_modified to NULL
      if (length(files) == 0) {
        last_modified <- NULL
      } else {
        # get the most recent file in the directory
        most_recent_file <- files[which.max(file.info(files)$mtime)]

        # get the time when the most recent file was last modified
        last_modified <- file.info(most_recent_file)$mtime
      }
    }

    # locate the urls for the data and adm
    urls <- locate_download_link(year = year, adm_url = adm_url)

    # check if the update date is greater than the last modified date
    skip = F
    if (!is.null(last_modified) && urls$update_date < last_modified) {
      cli::cli_alert_info(paste0("The data for ",year," is already up to date. Skipping download."))
      skip = T
    }

    # if skip = F, proceed with downloading the data
    if(skip == F){

    # download the data
    utils::download.file(urls[['data']],
                  destfile=paste0(dir,"/",year,"/adm_ytd_",year,".zip"),
                  mode="wb")

    # extract the data zip files
    utils::unzip(paste0(dir,"/",year,"/adm_ytd_",year,".zip"),
          exdir = paste0(dir,"/",year))

    # check if the layout url exists (typically doesn't prior to 2011)
    if("layout" %in% names(urls)){
      # download the layout file
      utils::download.file(urls[['layout']],
                    destfile=paste0(dir,"/",year,"/layout_",year,".zip"),
                    mode="wb")

      # extract the layout zip files
      utils::unzip(paste0(dir,"/",year,"/layout_",year,".zip"),
            exdir = paste0(dir,"/",year))
    }

    # get file paths for all txt files
    files <- list.files(paste0(dir,"/",year), full.names = TRUE, pattern = "\\.txt")

    # if helpers_only is TRUE, remove any files larger than 1mb
    if(helpers_only){
      # get the file sizes
      file_sizes <- file.info(files)$size

      # for any files larger than 1 mb, delete the file
      to_delete <- files[file_sizes > (1024* helpers_size_threshold)^2]

      # delete the files
      if(length(to_delete) > 0){
        file.remove(to_delete)
        cli::cli_alert_info(paste0("Deleted ", length(to_delete), paste0(" files larger than ",helpers_size_threshold," mb. To keep these files and clean them, set `helper_only = TRUE`." )))
      }

      # get updated list of file paths
      files <- list.files(paste0(dir,"/",year), full.names = TRUE, pattern = "\\.txt")

    }

    # set up a progress bar
    cli::cli_progress_bar("Converting .txt files to .rds", total = length(files))

    # loop over each txt file and convert it to .rds
    for(f in files){

      cli::cli_progress_update(status = paste0("cleaning ",f))

      # get the file name without the extension
      file_name <- clean_file_name(f, file_type_out = "rds")

      # suppress read_delim console output
      data <- readr::read_delim(f, delim = "|", col_names = TRUE, show_col_types = FALSE)

      # clean the data
      data <- clean_data(data)

      # save the file as an .rds
      saveRDS(data, file = file_name)

      # delete the original .txt file
      file.remove(f)
    }


    # remove the original zip files
    if(keep_source_files == FALSE){
      file.remove(list.files(paste0(dir,"/",year), full.names = TRUE, pattern = "\\.zip"))
    }

    }

    # close the progress bar
    cli::cli_progress_done()

  }

}


#' Download and convert USDA RMA Actuarial Data Master files (.txt) to .rds, memory-efficiently
#'
#' Downloads ADM files for the specified years, extracts them,
#' converts each `.txt` to `.rds` using `data.table::fread()` (character columns by default)
#' for lower memory overhead, cleans via `clean_data()`, and optionally
#' removes helper files and source archives.
#'
#' @param years Integer vector. Years to download (e.g., `c(2012, 2013)`).
#' @param adm_url Character. Base URL for the ADM repository.
#' @param dir Character. Output directory for download and processing.
#' @param helpers_only Logical. If `TRUE`, deletes `.txt` files larger than `helpers_size_threshold` (in MB).
#' @param helpers_size_threshold Numeric. Size threshold (in MB) for helper file removal.
#' @param keep_source_files Logical. If `FALSE`, removes downloaded `.zip` archives after extraction.
#' @return Invisibly returns `NULL`. Processed `.rds` files are written to disk.
#' @export
#' @importFrom data.table fread rbindlist
#' @importFrom cli cli_alert_info cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom utils download.file unzip
download_adm2 <- function(
    years = 2012,
    adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/",
    dir = "./data-raw",
    helpers_only = TRUE,
    helpers_size_threshold = 5,
    keep_source_files = FALSE
) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }

  for (year in years) {
    year_dir <- file.path(dir, as.character(year))
    if (!dir.exists(year_dir)) {
      dir.create(year_dir)
    }

    # determine the most recent modification time of any file in year_dir
    existing_files <- list.files(year_dir, full.names = TRUE)
    if (length(existing_files) == 0) {
      last_modified <- NULL
    } else {
      most_recent <- existing_files[which.max(file.info(existing_files)$mtime)]
      last_modified <- file.info(most_recent)$mtime
    }

    # locate download URLs
    urls <- locate_download_link(year = year, adm_url = adm_url)

    # skip if already up to date
    if (!is.null(last_modified) && urls$update_date < last_modified) {
      cli::cli_alert_info("Data for {year} is up to date; skipping.")
      next
    }

    # download & unzip data
    data_zip <- file.path(year_dir, sprintf("adm_ytd_%s.zip", year))
    utils::download.file(urls$data, data_zip, mode = "wb")
    utils::unzip(data_zip, exdir = year_dir)
    if (!keep_source_files) file.remove(data_zip)

    # optionally download & unzip layout
    if ("layout" %in% names(urls)) {
      layout_zip <- file.path(year_dir, sprintf("layout_%s.zip", year))
      utils::download.file(urls$layout, layout_zip, mode = "wb")
      utils::unzip(layout_zip, exdir = year_dir)
      if (!keep_source_files) file.remove(layout_zip)
    }

    # list .txt files
    txt_files <- list.files(year_dir, pattern = "\\.txt$", full.names = TRUE)

    # remove large helpers if requested
    if (helpers_only && length(txt_files)) {
      sizes <- file.info(txt_files)$size
      to_del <- txt_files[sizes > (1024^2) * helpers_size_threshold]
      if (length(to_del)) {
        file.remove(to_del)
        cli::cli_alert_info("Deleted %d helper files > %s MB", length(to_del), helpers_size_threshold)
      }
      txt_files <- setdiff(txt_files, to_del)
    }

    # convert each .txt to .rds
    for (f in txt_files) {

      # Set chunk size for reading large files
      chunk_size <- 1000000  # Adjust based on your memory constraints

      # Get total number of rows to determine chunks
      total_rows <- data.table::fread(
        input = f,
        sep = "|",
        select = 1L,
        showProgress = FALSE
      ) |> nrow()

      # Initialize variables
      out_rds <- sub("\\.txt$", ".rds", f)
      first_chunk <- TRUE
      total_chunks <- ceiling(total_rows / chunk_size)



      # Read and process in chunks
      for (i in seq_len(total_chunks)) {
        start_row <- (i - 1) * chunk_size + 1
        end_row <- min(i * chunk_size, total_rows)

        # Read chunk
        dt <- data.table::fread(
          input = f,
          sep = "|",
          colClasses = "character",
          showProgress = FALSE,
          skip = start_row - 1,
          nrows = end_row - start_row + 1
        )

        # Clean chunk
        dt <- clean_data(dt)

        if(i == 1){
          dt_names = names(dt)
        } else {
          colnames(dt) <- dt_names
        }



        # Save/append to RDS
        if (first_chunk) {
          saveRDS(dt, out_rds, compress = "xz")
          first_chunk <- FALSE
        } else {
          # Read existing data, combine with new chunk, and save
          existing_dt <- readRDS(out_rds)
          combined_dt <- rbind(existing_dt, dt)
          saveRDS(combined_dt, out_rds, compress = "xz")
          rm(existing_dt, combined_dt)
        }


        # Cleanup chunk
        rm(dt)
        gc()
      }


      # Final cleanup
      file.remove(f)

    }
  }

  invisible(NULL)
}


#' Clean the file name
#'
#' @param file_name the file path of the file name to clean
#' @param file_type_out the type of file (currently only supports rds)
#'
#' @returns a version of the file name that is cleaned (i.e. snake case, .rds suffix, no extraneous information)
#'
#' @examples \dontrun{clean_file_name("./data-raw/2012_A01100_YieldAndTyield_YTD.txt")}
clean_file_name <- function(file_name, file_type_out = "rds"){

  # split the file path into the directory and file name
  parts <- strsplit(file_name, "/")[[1]]

  # file suffix
  suffix <- parts[length(parts)]

  # remove any instance of alphabetic character followed by 5 numbers
  #suffix <- gsub("[A-Za-z]\\d{5}", "", suffix)

  # remove the first 4 digits from the suffix if the first 4 digits are numeric digits
  suffix <- gsub("^[0-9]{4}", "", suffix)

  # remove "YTD"
  suffix <- gsub("YTD", "", suffix)

  # remove "_" unless "_" follows a number
  suffix <- gsub("(?<![0-9])_(?![0-9])", "", suffix, perl = TRUE)

  # convert from camel case to snake case
  suffix <- gsub("([a-z])([A-Z])", "\\1_\\2", suffix)
  suffix <- tolower(suffix)

  # split the suffix by the period
  suffix_parts <- strsplit(suffix, "\\.")[[1]]

  # replace the last part with the desired file type
  suffix_parts[length(suffix_parts)] <- file_type_out

  # paste the suffix parts back together
  suffix <- paste(suffix_parts, collapse = ".")

  # paste the directory and file name back together
  file_name <- paste(parts[-length(parts)], collapse = "/")

  # paste the file name and suffix together
  file_name <- paste0(file_name, "/", suffix)

  # return the file name
  return(file_name)

}


#' Apply standardized data cleaning opperations
#'
#' @param df a data frame to clean
#'
#' @returns a cleaned data frame
#' @importFrom janitor clean_names
#' @importFrom readr type_convert
#'
#' @examples \dontrun{clean_data(df)}
clean_data <- function(df){

  # clean column names
  df <- janitor::clean_names(df)

  # enforce data types
  df <- suppressMessages(readr::type_convert(df))

  # identify date
  date_cols <- grep("date", names(df), value = TRUE)

  # try to parse dates
  for(col in date_cols){
    input = as.character(df[[col]]) # convert to character
    input <- as.character(gsub("[^0-9]", "", input)) # remove non-numeric characters
    try({
      converted_dates <- readr::parse_date(input, format = "%Y%m%d", na = c("", "NA"))
      # if converted dates are not all NA
      if(!all(is.na(converted_dates))) {
        df[[col]] <- converted_dates
      }
    })
  }

  # return the df
  return(df)

}


#' Get File Information from a Directory
#'
#' Scans a specified directory for files with a given suffix and returns a data frame
#' containing their file paths, sizes in bytes, and sizes in megabytes.
#'
#' @param directory A character string specifying the path to the directory to scan.
#'   Defaults to \code{"./data-raw"}.
#' @param file_suffix A character string specifying the file suffix to match.
#'   Defaults to \code{".rds"}.
#'
#' @return A data frame with columns:
#'   \item{file_path}{Full file path}
#'   \item{size_bytes}{File size in bytes}
#'   \item{size_mb}{File size in megabytes}
#'
#' @examples
#' \dontrun{
#' get_file_info()
#' get_file_info(directory = "./my-data", file_suffix = ".csv")
#' }
#'
get_file_info <- function(directory = "./data-raw", file_suffix = ".rds") {
  # Get list of all files recursively
  files <- list.files(path = directory, recursive = TRUE, full.names = TRUE, pattern = file_suffix)

  # Filter only actual files (not directories)
  files <- files[file.info(files)$isdir == FALSE]

  # Get file sizes
  sizes <- file.info(files)$size

  # Create data frame
  df <- data.frame(
    file_path = files,
    size_bytes = sizes,
    size_mb = sizes / (1024 * 1024), # Convert to MB
    stringsAsFactors = FALSE
  )

  return(df)
}



#' Build Helper Datasets from Raw RDS Files
#'
#' Processes raw `.rds` files from a specified directory, filters them based on year and size,
#' combines matching files into unified datasets, saves them as `.rda` files, and generates
#' corresponding documentation entries in `./R/helper_data.R`.
#'
#' @param years A numeric vector of years used to filter the input `.rds` files by year in their file paths.
#' @param size_threshold any files below the size_threshold (in mb) are assumed to be helper data sets and kept. Applied to the maximum file size across all years.
#' @param dir A character string specifying the directory containing the raw `.rds` files.
#'   Defaults to \code{"./data-raw"}.
#'
#' @details This function:
#' \itemize{
#'   \item Filters `.rds` files in the target directory to those matching specified years.
#'   \item Excludes files larger than 1MB and those containing "rate" in the filename.
#'   \item Binds rows of files with matching names across years into a single dataset.
#'   \item Saves each dataset to the \code{./data/} folder as `.rda` files.
#'   \item Writes or appends roxygen-style documentation entries to \code{./R/helper_data.R}.
#'   \item Renames the existing \code{helper_data.R} file with the current date before overwriting.
#' }
#'
#' Each resulting dataset will be available for use with \code{data()} if the package is rebuilt.
#'
#' @return No return value.
#' @import dplyr
#' @importFrom purrr map
#' @examples \dontrun{
#' build_helper_datasets(years = 2020:2022)
#' }
build_helper_datasets <- function(years,dir = "./data-raw", size_threshold = 1 ){

  # id "./data" doesn't exist, create it
  if(!dir.exists("./data")) {
    dir.create("./data")
  }


  file_info <- get_file_info(directory = dir,
                              file_suffix = ".rds")

  # Keep only file info for the years specified
  file_info <- file_info[grepl(paste(years, collapse = "|"), file_info$file_path), ]

  # For each year, extract the year from the file path and convert to numeric
  file_info$year <- as.numeric(gsub(".*?/(\\d{4})/.*", "\\1", file_info$file_path))

  # Add a column with the file name without any of the parent folders
  file_info$file_name <- gsub(paste0(dir, "/"), "", file_info$file_path)
  file_info$file_name <- gsub("[0-9]{4}/", "", file_info$file_name)
  file_info$file_name <- gsub(".rds", "", file_info$file_name)

  # extract the actuarial code from each file path (e.x. A00010)
  file_info$adm_code <- substr(basename(file_info$file_path),6,11)

  # keep only files that are less than the size threshold (in MB). Applied to
  # maximum size over all years
  max_sizes <- file_info %>%
    group_by(.data$adm_code) %>%
    summarize(max_size = max(.data$size_mb)) %>%
    filter(.data$max_size < size_threshold)

  file_info <- file_info %>%
    filter(.data$adm_code %in% max_sizes$adm_code)

  # if "./R/data.R" already exists, rename the file name with the data appended
  if(file.exists("./R/helper_data.R")){
    file.rename("./R/helper_data.R", paste0("./R/helper_data_", Sys.Date(), ".R"))
  }

  # create a new file with the header for the data.R file
  write("#' @title Actuarial Data Master Helper Datasets\n",
        file = "./R/helper_data.R", append = FALSE)

  # for each unique value in the file_name column,
  # load all the datasets corresponding to that file name as
  # specified by the file_path and then row bind them together
  # and save them as a .rds file with the name of the file_name
  for(f in unique(file_info$adm_code)){


    # get the file paths for the current file name
    file_paths <- file_info[file_info$adm_code == f, "file_path"]

    # load the datasets
    data <- file_paths %>%
      purrr::map(~ {
        df <- readRDS(.x) # read in the data frame
        df[] <- lapply(df, as.character)  # Convert all columns to character
        df # return the data frame
      }) %>%
      dplyr::bind_rows() # bind rows

    # convert the columns back to their most approriate data type
    data <- suppressMessages(readr::type_convert(data))


    # Remove both prefix and suffix to get file name to export
    file_out <- unique(gsub("^\\d{4}_[A-Za-z]\\d{5}_|_YTD|\\.rds", "",
                     basename(file_paths)))


    # Dynamically assign the name of the data to the value in f
    assign(file_out, data)

    # Save the named object to an .rda file
    save(list = file_out, file = paste0("./data/", file_out, ".rda"), compress = "xz")


    # add a documentation entry in ./data/data.R for the dataset
    doc_entry <- paste0("#' @name ", file_out, "\n",
                        "#' @title ", file_out, "\n",
                        "#' @description A combined dataset for ", file_out, "\n",
                        "#' @format A data frame with ", nrow(data), " rows and ", ncol(data), " columns covering ",min(data$reinsurance_year),"-",max(data$reinsurance_year),".\n",
                        "#' @source Actuarial Data Master\n",
                        "#' @usage data(",file_out,")", "\n",
                        paste0('"',file_out,'"'))
    # append the doc entry to the data.R file
    write(doc_entry, file = "./R/helper_data.R", append = TRUE)
  }




}

