


#' Convert time string to seconds after midnight
#'
#' Converts strings in the "HH:MM:SS" format to seconds after midnight.
#'
#' @param string A string in "HH:MM:SS" format.
#'
#' @return The seconds after midnight of a given time string as an integer.
#'
#' @noRd
string_to_seconds <- function(string) {
  
  checkmate::assert_character(string)
  
  split_string <- strsplit(string, ":", fixed = TRUE)
  
  seconds_from_midgnight <- vapply(
    split_string,
    function(i) sum(as.integer(i) * c(3600L, 60L, 1L)),
    integer(1)
  )
  
  # "" strings result in seconds_to_midnight = 0. find those and replace to NA
  
  index_na <- which(lengths(split_string) == 0)
  seconds_from_midgnight[index_na] <- NA_integer_
  
  return(seconds_from_midgnight)
  
}



#' Convert seconds after midnight to time string
#'
#' Converts seconds after midnight as integers to strings in the "HH:MM:SS"
#' format.
#'
#' @param seconds An integer.
#'
#' @return A time-representing string.
#'
#' @noRd
seconds_to_string <- function(seconds) {
  
  checkmate::assert_integer(seconds)
  
  time_string <- data.table::fifelse(
    is.na(seconds),
    "",
    paste(
      formatC(seconds %/% 3600, width = 2, format = "d", flag = 0),
      formatC((seconds %% 3600) %/% 60, width = 2, format = "d", flag = 0),
      formatC((seconds %% 3600) %% 60, width = 2, format = "d", flag = 0),
      sep = ":"
    )
  )
  
  return(time_string)
  
}
