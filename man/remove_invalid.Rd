% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_gtfs.R
\name{remove_invalid}
\alias{remove_invalid}
\title{Remove invalid objects from GTFS data}
\usage{
remove_invalid(gtfs_data, only_essential = TRUE, prompt_invalid = FALSE)
}
\arguments{
\item{gtfs_data}{A list of data.tables read using gtfs2gps::reag_gtfs().}

\item{only_essential}{Remove only the essential files? The essential files are all but 
agency and calendar. Default is TRUE, which means that agency-routes and trips-calendar relations
will not be processed as restrictions to remove objects.}

\item{prompt_invalid}{Show the invalid objects. Default is FALSE.}
}
\value{
A subset of the input GTFS data.
}
\description{
Remove all invalid (and therefore unnecessary) objects from GTFS data. That is,
agency-routes relation (agency_id), routes-trips relation (route_id), 
trips-shapes relation (shape_id), trips-frequencies relation (trip_id),
trips-stop_times relation (trip_id), stop_times-stops relation (stop_id),
and trips-calendar relation (service_id),
recursively, until GTFS data does not reduce its size anymore.
}
\examples{
poa <- read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps"), remove_invalid = FALSE)
object.size(poa)
subset <- remove_invalid(poa)
object.size(subset)
}