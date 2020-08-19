#' @title Read GTFS data into a list of data.tables
#' @description Read files of a zipped GTFS feed and load them to memory as a list of data.tables.
#' It will load the following files: "shapes.txt", "stop_times.txt", "stops.txt", "trips.txt",
#' "agency.txt", "calendar.txt", "routes.txt", and "frequencies.txt", with
#' this last four being optional. If one of the mandatory files does not exit,
#' this function will stop with an error message.
#' @param gtfszip A zipped GTFS data.
#' @return A list of data.tables, where each index represents the respective GTFS file name.
#' @export
#' @examples
#' poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"))
read_gtfs <- function(gtfszip){
  if(!file.exists(gtfszip))
    stop(paste0("File '", gtfszip, "' does not exist"))

  # Unzip files
  tempd <- file.path(tempdir(), "gtfsdir") # create tempr dir to save GTFS unzipped files
  unlink(normalizePath(paste0(tempd, "/", dir(tempd)), mustWork = FALSE), recursive = TRUE) # clean tempfiles in that dir
  utils::unzip(zipfile = gtfszip, exdir = tempd, overwrite = TRUE) # unzip files
  unzippedfiles <- list.files(tempd) # list of unzipped files

  result <- list()

  myread <- function(file) suppressWarnings(data.table::fread(paste0(tempd, "/", file), encoding = "UTF-8"))

  # read files to memory
  if("agency.txt"      %chin% unzippedfiles){result$agency      <- myread("agency.txt")}
  if("routes.txt"      %chin% unzippedfiles){result$routes      <- myread("routes.txt")}
  if("stops.txt"       %chin% unzippedfiles){result$stops       <- myread("stops.txt")}      else{stop("File stops.txt is missing")}
  if("stop_times.txt"  %chin% unzippedfiles){result$stop_times  <- myread("stop_times.txt")} else{stop("File stop_times.txt is missing")}
  if("shapes.txt"      %chin% unzippedfiles){result$shapes      <- myread("shapes.txt")}     else{stop("File shapes.txt is missing")}
  if("trips.txt"       %chin% unzippedfiles){result$trips       <- myread("trips.txt")}      else{stop("File trips.txt is missing")}
  if("calendar.txt"    %chin% unzippedfiles){result$calendar    <- myread("calendar.txt")}
  if("frequencies.txt" %chin% unzippedfiles){result$frequencies <- myread("frequencies.txt")}

  if(is.null(result$shapes)     || dim(result$shapes)[1] == 0)     stop("shapes.txt is empty in the GTFS file")
  if(is.null(result$trips)      || dim(result$trips)[1] == 0)      stop("trips.txt is empty in the GTFS file")
  if(is.null(result$stops)      || dim(result$stops)[1] == 0)      stop("stops.txt is empty in the GTFS file")
  if(is.null(result$stop_times) || dim(result$stop_times)[1] == 0) stop("stop_times.txt is empty in the GTFS file")

  mysub <- function(value) sub("^24:", "00:", value)
    
  result$stop_times[, departure_time := data.table::as.ITime(mysub(departure_time), format = "%H:%M:%OS")]
  result$stop_times[, arrival_time := data.table::as.ITime(mysub(arrival_time), format ="%H:%M:%OS")]

  if(!is.null(result$frequencies)){
    result$frequencies[, start_time := data.table::as.ITime(mysub(start_time), format = "%H:%M:%OS")]
    result$frequencies[, end_time := data.table::as.ITime(mysub(end_time), format = "%H:%M:%OS")]
  }

  result$stops$stop_id <- as.character(result$stops$stop_id)
  result$stop_times$stop_id <- as.character(result$stop_times$stop_id)

  return(result)
}
