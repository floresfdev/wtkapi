#' Parse the content of an API response
#' @description Get the content of a JSON API response.
#'
#' @param response Object with the API response.
#'
#' @return The content of the JSON API response.
#' @export
#'
#' @importFrom httr headers content
#' @importFrom jsonlite fromJSON
#'
#' @examples
parse_response_content <- function(response) {
    response_content <- list()

    if (headers(response)$`content-type` == "application/json") {
        response_content <-
            fromJSON(content(response, "text", encoding = "ISO-8859-1"))
    }

    return(response_content)
}


#' Project latitude and longitude to Lambert Conformal Conic coordinates
#' @description Given the latitude and longitude of a point, compute the projection to Lambert Conformal Conic coordinates.
#'
#' @param latitude The latitude of the point (WGS84).
#' @param longitude The longitude of the point (WGS84).
#'
#' @return A named list with elements `coord_y` for the latitude and `coord_x` for the longitude, both as LCC coordinates.
#' @export
#'
#' @import rgdal
#' @import sp
#'
#' @examples
latlong_to_LCC <- function(latitude, longitude) {
    lcc_coordinates <- list()

    proj_latlong_string <- "+proj=longlat +datum=WGS84"
    proj_lcc_string <-
        "+proj=lcc +lat_1=30 +lat_2=60 +lat_0=38.47240422490422 +lon_0=-96.0 +x_0=0 +y_0=0 +ellps=sphere +units=m +no_defs"

    spatial_points <- data.frame(coord_x = longitude,
                                 coord_y = latitude)

    coordinates(spatial_points) <- c("coord_x", "coord_y")
    proj4string(spatial_points) <- CRS(proj_latlong_string)
    spatial_points <- spTransform(spatial_points, CRS(proj_lcc_string))

    lcc_coordinates <- list(coord_x = as.numeric(spatial_points$coord_x),
                            coord_y = as.numeric(spatial_points$coord_y))

    return(lcc_coordinates)
}


#' Convert latitude and longitude to indices in the WIND Toolkit dataset
#' description Given the latitude and longitude of a point, compute the corresponding indices on the WIND Toolkit dataset for the nearest point.
#'
#' @param latitude The latitude of the point (WGS84).
#' @param longitude The longitude of the point (WGS84).
#'
#' @return A named list with elements `y` and `x` for the corresponding indices on the WIND Toolkit dataset.
#' @export
#'
#' @examples
latlong_to_index <- function(latitude, longitude) {
    coord_index <- list()

    origin_latitude <- 19.62406     # According to DB, point (y,x) = (0,0)
    origin_longitude <- -123.30661  # According to DB, point (y,x) = (0,0)
    origin_LCC <- latlong_to_LCC(origin_latitude, origin_longitude)

    point_LCC <- latlong_to_LCC(latitude, longitude)

    delta_x <- point_LCC$coord_x - origin_LCC$coord_x
    delta_y <- point_LCC$coord_y - origin_LCC$coord_y

    coord_index <- list(x = as.integer(round(delta_x / 2000, digits = 0)),
                        y = as.integer(round(delta_y / 2000, digits = 0)))

    return(coord_index)
}


#' Convert a datetime to an index in the WIND Toolkit dataset
#' @description Given a datetime, compute the corresponding index on the WIND Toolkit dataset.
#'
#' @param datetime The datetime point as POSIX or string, in format YYYY-MM-DD HH:MM:SS.
#'
#' @return The index on the WIND Toolkit dataset for the datetime.
#' @export
#'
#' @importFrom lubridate ymd_hms
#'
#' @examples
datetime_to_index <- function(datetime) {
    datetime_index <- 0

    origin_datetime <- ymd_hms("2007-01-01 00:00:00")

    point_datetime <- ymd_hms(datetime, truncated = 3)
    stopifnot(!is.na(point_datetime))
    stopifnot(any(class(point_datetime) %in% c("POSIXct", "POSIXt")))

    datetime_index <- difftime(point_datetime, origin_datetime, units = "hours")

    return(datetime_index)
}


#' Convert a datetime range to the indices range in the WIND Toolkit dataset
#' @description Given two datetime (from and to), compute the corresponding indices range on the WIND Toolkit dataset.
#'
#' @param datetime_from The initial datetime point of the range as POSIX or string, in format YYYY-MM-DD HH:MM:SS.
#' @param datetime_to The final datetime point of the range as POSIX or string, in format YYYY-MM-DD HH:MM:SS.
#'
#' @return A named list with elements `t_from` and `t_to` for the corresponding indices range on the WIND Toolkit dataset.
#' @export
#'
#' @examples
datetime_to_indices <- function(datetime_from, datetime_to) {
    datetime_indices <- list()

    t_from <- datetime_to_index(datetime_from)
    t_to <- datetime_to_index(datetime_to) + 1

    datetime_indices <- list(t_from = t_from,
                             t_to = t_to)

    return(datetime_indices)
}


#' Compute a named list with datetime ranges and step.
#' @description Helper function to get formatted data for use in other functions.
#'
#' @param datetime_from The initial datetime point of the range as string, in format YYYY-MM-DD HH:MM:SS.
#' @param datetime_to The final datetime point of the range as string, in format YYYY-MM-DD HH:MM:SS.
#' @param datetime_step The size of the step within the range, with 1 being each hour, 2 every other hour, etc.
#'
#' @return A named list with elements `datetime_from`, `datetime_to`, `datetime_step`.
#' @export
#'
#' @examples
compute_datetime_info <- function(datetime_from, datetime_to, datetime_step) {
    list(datetime_from = datetime_from,
         datetime_to = datetime_to,
         datetime_step = datetime_step)
}
