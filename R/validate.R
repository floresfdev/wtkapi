#' Check if the endpoint is a valid URL
#' @description Validates that the parameter \code{endpoint} starts with either
#'   \code{"http://"} or \code{"https://"}.
#'
#' @param endpoint The endpoint URL.
#'
#' @return TRUE if the endpoint starts with \code{"http://"} or
#'   \code{"https://"}, FALSE otherwise.
#'
#' @export
#'
#' @importFrom purrr map2_lgl
#'
#' @examples
is_endpoint <- function(endpoint) {
    protocols_list <- c("http://", "https://")
    return(any(map2_lgl(endpoint, protocols_list, startsWith)))
}


#' Check if the host is a valid NREL host
#' @description Validates that the parameter \code{host} starts with
#'   \code{"/nrel/"} and ends with \code{".h5"}.
#'
#' @param host The host address of the NREL HDF5 file.
#'
#' @return TRUE if the host is a valid NREL host, FALSE otherwise.
#'
#' @export
#'
#' @examples
is_nrel_host <- function(host) {
    return(startsWith(host, "/nrel/") && endsWith(host, ".h5"))
}
