#' Check if the endpoint is a valid URL
#' @description Validates that the parameter `endpoint` starts with either `http://` or `https://`.
#'
#' @param endpoint The endpoint URL
#'
#' @return TRUE if the endpoint starts with `http://` or `https://`, FALSE otherwise
#' @export
#'
#' @importFrom purrr map2_lgl
#'
#' @examples
is_endpoint <- function(endpoint) {
    protocols_list <- c("http://", "https://")
    any(map2_lgl(endpoint, protocols_list, startsWith))
}


#' Check if the host is a valid NREL host
#' @description Validates that the parameter `host` starts with `/nrel/` and ends with `.h5`.
#'
#' @param host The host address of the NREL HDF5 file
#'
#' @return TRUE if the host is a valid NREL host, FALSE otherwise
#' @export
#'
#' @examples
is_nrel_host <- function(host) {
    startsWith(host, "/nrel/") && endsWith(host, ".h5")
}
