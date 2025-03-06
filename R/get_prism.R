#' Download data from PRISM
#'
#' Downloads an entire year of data from PRISM using `get_prism_dailys()`
#' and returns the download path so it works with `format = 'file'` in `targets`
#'
#' @param year year of data to be downloaded
#' @param variable variable to get
#' @param prism_dir base directory for PRISM data.  Subfolders will be created for the variable and year
#'
#' @return path to folder for that year of data
get_prism <- function(year, variable = c("tmin", "tmax", "tmean"), prism_dir = "data/prism") {
  variable <- match.arg(variable)
  cli::cli_alert("Downloading PRISM {variable} data for {year}")
  
  #check if path exists and create if not
  year_dir <- fs::dir_create(fs::path(prism_dir, variable, year))
  
  #make sequence of dates
  date_start <- lubridate::make_date(year = year)
  date_end <- lubridate::make_date(year = year, month = 12, day = 31)
  dates <- seq(date_start, date_end, "day")
  
  #make list of requests
  #api documentation: https://prism.oregonstate.edu/documents/PRISM_downloads_web_service.pdf
  
  base_url <- "https://services.nacse.org/prism/data/public/4km"
  req <- 
    httr2::request(base_url) |> 
    httr2::req_user_agent("University of Arizona CCT Data Science (https://datascience.cct.arizona.edu/)") |> 
    httr2::req_url_path_append(variable) |> 
    httr2::req_retry(max_tries = 10)
  
  reqs <- purrr::map(dates, \(x) req |> httr2::req_url_path_append(format(x, "%Y%m%d")))
  
  # retrieve filenames
  head_reqs <- purrr::map(reqs, \(x) httr2::req_method(x, "HEAD"))
  head_resps <- httr2::req_perform_sequential(head_reqs, progress = "Retrieving filenames")
  filenames <- head_resps |> 
    purrr::map_chr(\(x) {
      x$headers$`Content-Disposition` |> 
        stringr::str_remove("filename=") |>
        stringr::str_remove_all('\\"') |> 
        fs::path()})
  filepaths <- fs::path(year_dir, filenames)
  # Figure out which ones need to be downloaded
  files_exist <- fs::file_exists(filepaths)
  
  reqs_to_perform <- reqs[!files_exist]
  files_to_dl <- filepaths[!files_exist]
  
  #check for provisional files that need to be replaced
  provisional_filename <- stringr::str_replace(files_to_dl, "stable", "provisional")
  already_downloaded <- fs::dir_ls(fs::path(year_dir))
  provisional_to_be_replaced <- 
    already_downloaded[already_downloaded %in% provisional_filename]
  files_replacing_provisional <- files_to_dl[fs::file_exists(provisional_filename)]
  stopifnot(length(provisional_to_be_replaced) == length(files_replacing_provisional))
  
  if (length(files_replacing_provisional) > 0) {
    cli::cli_alert_warning("Stable versions will be downloaded to replace {length(files_replacing_provisional)} provisional files")
    #remove provisional versions
    fs::file_delete(provisional_to_be_replaced)
  }
  
  cli::cli_alert_info("{length(reqs_to_perform)} files to download")
  
  if (length(reqs_to_perform) > 0) {
    resp <- 
      httr2::req_perform_sequential(
        reqs_to_perform,
        paths = files_to_dl,
        progress = "Downloading"
      )
    #check that they are actually zip files and if not change extension to .txt
    
    purrr::walk(files_to_dl, \(x) {
      is_zip <- check_zip_file(x)
      if (!isTRUE(is_zip)) {
        warning(is_zip)
        file.rename(x, fs::path_ext_set(x, "txt"))
      }
    })
  } else {
    cli::cli_alert_info("Skipping downloading.")
  }
  
  #return the path to the folder so targets tracks the whole dang thing
  invisible(year_dir)
}


#from https://github.com/ropensci/prism/blob/master/R/prism_webservice.R
check_zip_file <- function(x) {
  zfile <- readLines(x, warn = FALSE)
  
  # zip files have "PK\....." in their first line
  is_zip <- grepl("^PK\003\004", zfile[1])
  
  if (is_zip) {
    return(is_zip)
  } else{
    return(zfile)
  }
}
