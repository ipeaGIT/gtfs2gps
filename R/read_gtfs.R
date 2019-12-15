#' @title Read GTFS data into a list of data.tables
#' @description Read files of a zipped GTFS feed and load them to memory as a list of data.tables.
#' It will load the following files: "agency.txt", "calendar.txt", "routes.txt", "shapes.txt", 
#' "stop_times.txt", "stops.txt", "trips.txt", and "frequencies.txt", with
#' this last one being optional. If one of the mandatory files does not exit,
#' this function will stop with an error message.
#' @param gtfszip A zipped GTFS data.
#' @export
#' @examples \donttest{
#' library(gtfs2gps)
#'
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#' }
read_gtfs <- function(gtfszip){
  if(!file.exists(gtfszip))
    stop(paste0("File '", gtfszip, "' does not exist"))

  # Unzip files
  tempd <- file.path(tempdir(), "gtfsdir") # create tempr dir to save GTFS unzipped files
  unlink(normalizePath(paste0(tempd, "/", dir(tempd)), mustWork = FALSE), recursive = TRUE) # clean tempfiles in that dir
  utils::unzip(zipfile = gtfszip, exdir = tempd, overwrite = TRUE) # unzip files
  unzippedfiles <- list.files(tempd) # list of unzipped files

  result <- list()

  # read files to memory
  if("agency.txt"      %in% unzippedfiles){result$agency      <- data.table::fread(paste0(tempd,"/agency.txt"),      encoding="UTF-8")}  else{stop("File agency.txt is missing")}
  if("routes.txt"      %in% unzippedfiles){result$routes      <- data.table::fread(paste0(tempd,"/routes.txt"),      encoding="UTF-8")}  else{stop("File routes.txt is missing")}
  if("stops.txt"       %in% unzippedfiles){result$stops       <- data.table::fread(paste0(tempd,"/stops.txt"),       encoding="UTF-8")}  else{stop("File stops.txt is missing")}
  if("stop_times.txt"  %in% unzippedfiles){result$stop_times  <- data.table::fread(paste0(tempd,"/stop_times.txt"),  encoding="UTF-8")}  else{stop("File stop_times.txt is missing")}
  if("shapes.txt"      %in% unzippedfiles){result$shapes      <- data.table::fread(paste0(tempd,"/shapes.txt"),      encoding="UTF-8")}  else{stop("File shapes.txt is missing")}
  if("trips.txt"       %in% unzippedfiles){result$trips       <- data.table::fread(paste0(tempd,"/trips.txt"),       encoding="UTF-8")}  else{stop("File trips.txt is missing")}
  if("calendar.txt"    %in% unzippedfiles){result$calendar    <- data.table::fread(paste0(tempd,"/calendar.txt"),    encoding="UTF-8")}  else{stop("File calendar.txt is missing")}
  if("frequencies.txt" %in% unzippedfiles){result$frequencies <- data.table::fread(paste0(tempd,"/frequencies.txt"), encoding="UTF-8")}

  mysub <- function(value) sub("^24:", "00:", value)

  if(!is.null(result$stop_times)){
    result$stop_times[, departure_time := data.table::as.ITime(mysub(departure_time), format = "%H:%M:%OS")]
    result$stop_times[, arrival_time := data.table::as.ITime(mysub(arrival_time), format ="%H:%M:%OS")]
  }
  
  if(!is.null(result$frequencies)){
    result$frequencies[, start_time := data.table::as.ITime(mysub(start_time), format = "%H:%M:%OS")]
    result$frequencies[, end_time := data.table::as.ITime(mysub(end_time), format = "%H:%M:%OS")]
  }
  
  return(result)
}
