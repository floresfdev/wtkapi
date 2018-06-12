#' Get the list of datasets available in the WIND Toolkit.
#' @description Get a dataframe with the datasets information from the WIND Toolkit.
#'
#' @param endpoint Endpoint of the dataset.
#' @param host Host with files of the dataset.
#' @param api_key API key to access the dataset.
#'
#' @return A dataframe (tibble) containing the datasets available via the WIND Toolkit API.
#' @export
#'
#' @import tibble
#' @import dplyr
#' @importFrom glue glue
#' @importFrom httr GET stop_for_status
#'
#' @examples
get_datasets <- function(endpoint, host, api_key) {
    datasets <- tibble()

    # Check param values
    stopifnot(is_endpoint(endpoint))
    stopifnot(is_nrel_host(host))
    stopifnot(api_key != "")

    # Main API call
    main_url <- glue("{endpoint}?host={host}&api_key={api_key}")
    main_response <- GET(url = main_url)
    stop_for_status(main_response)

    main_response_content <- parse_response_content(main_response)

    # Root group
    root_group_url <-
        as.tibble(main_response_content$hrefs) %>%
        filter(rel == "root") %>%
        pull(href) %>%
        paste0(., "&api_key={api_key}")

    root_group_url <- glue(root_group_url)
    root_group_response <- GET(url = root_group_url)
    stop_for_status(root_group_response)

    root_group_content <- parse_response_content(root_group_response)

    # Links
    links_url <-
        as.tibble(root_group_content$hrefs) %>%
        filter(rel == "links") %>%
        pull(href) %>%
        paste0(., "&api_key={api_key}")

    links_url <- glue(links_url)
    links_response <- GET(url = links_url)
    stop_for_status(links_response)

    links_content <- parse_response_content(links_response)

    # Datasets
    datasets <- as_tibble(links_content$links)

    return(datasets)
}


#' Get the URL of a specific dataset available via the WIND Toolkit API.
#'
#' @param datasets The dataframe with datasets retrieved from `get_datasets()`
#' @param dataset_title A string with the specific title of the dataset, e.g. `"windspeed_80m"`, `"temperature_80m"`, `"relativehumidity_2m"`.
#'
#' @return A string with the URL of the dataset. Empty if the `dataset_title` is not valid.
#' @export
#'
#' @import tibble
#' @import dplyr
#'
#' @examples
get_dataset_url <- function(datasets, dataset_title) {
    dataset_url <-
        datasets %>%
        filter(title == dataset_title) %>%
        pull(target)

    return(dataset_url)
}


#' Get a portion of the WIND Toolkit dataset as a tidy dataframe for a datetime range and a geographic nearest point.
#'
#' @param datasets The dataframe with datasets retrieved from `get_datasets()`
#' @param api_key API key to access the dataset.
#' @param dataset_titles A string vector containing the dataset titles to retrieve, e.g. `c("windspeed_80m", "temperature_80m", "relativehumidity_2m")`. Avoid the special datasets `datetime` and `coordinates`, which are added by default in the process.
#' @param datetime_info A named list (as formatted by `compute_datetime_info()`) with the datetime range and step.
#' @param latitude The latitude of the point (WGS84).
#' @param longitude The longitude of the point (WGS84).
#'
#' @return A dataframe (tibble) in tidy format with the values of each information requested for the datetime range and the geographic nearest point.
#' @export
#'
#' @import tibble
#' @import dplyr
#' @importFrom lubridate ymd_hms
#' @importFrom glue glue
#' @importFrom stringr str_replace fixed
#'
#' @examples
get_dataset <- function(datasets, api_key, dataset_titles, datetime_info, latitude, longitude) {
    # Parse params
    datetime_from <- ymd_hms(datetime_info$datetime_from, truncated = 3)
    datetime_to <- ymd_hms(datetime_info$datetime_to, truncated = 3)
    datetime_step <- as.numeric(datetime_info$datetime_step)

    # Validate
    stopifnot(length(dataset_titles) > 0)
    stopifnot(all(dataset_titles %in% datasets$title))
    stopifnot(!any(dataset_titles %in% c("datetime", "coordinates")))

    stopifnot(any(class(datetime_from) %in% c("POSIXct", "POSIXt"))
              && !is.na(datetime_from))
    stopifnot(any(class(datetime_to) %in% c("POSIXct", "POSIXt"))
              && !is.na(datetime_to))
    stopifnot(!is.na(datetime_step))

    stopifnot(latitude != 0)
    stopifnot(longitude != 0)

    # Add special datasets to vector
    dataset_titles <- c("datetime", "coordinates", dataset_titles)

    # Compute indices and selection string
    coord_index <- latlong_to_index(latitude, longitude)
    datetime_indices <- datetime_to_indices(datetime_from, datetime_to)

    # Selection strings with format start:stop:skip (# of steps)
    string_datetime <- glue("{datetime_indices$t_from}:{datetime_indices$t_to}:{datetime_step}")
    # TODO: Allow range of coordinates (line and box)
    string_coord_y <- glue("{coord_index$y}:{coord_index$y + 1}")
    string_coord_x <- glue("{coord_index$x}:{coord_index$x + 1}")


    for (dataset_title in dataset_titles) {
        if (dataset_title == "coordinates") {
            select_string <- "/value?select=[{string_coord_y},{string_coord_x}]"
        } else if (dataset_title == "datetime") {
            select_string <- "/value?select=[{string_datetime}]"
        } else {
            select_string <- "/value?select=[{string_datetime},{string_coord_y},{string_coord_x}]"
        }

        dataset_url <- get_dataset_url(datasets, dataset_title)
        stopifnot(!is_empty(dataset_url))

        dataset_url <-
            str_replace(dataset_url,
                        fixed(as.character(glue("?host={host}"))),
                        paste0(select_string, "&host={host}&api_key={api_key}"))

        dataset_url <- glue(dataset_url)
        dataset_response <- GET(url = dataset_url)
        stop_for_status(dataset_response)

        dataset_content <- parse_response_content(dataset_response)

        if (dataset_title == "coordinates") {
            dataset_values <- dataset_content$value[,,1:2]
        } else if (dataset_title == "datetime") {
            dataset_values <- ymd_hms(dataset_content$value)
        } else {
            dataset_values <- dataset_content$value[,,1]
        }

        # First cycle, the dataset is created with tibble()...
        if (!exists(quo_name(quo(dataset)))) {
            dataset <- tibble(!!quo_name(dataset_title) := dataset_values)
        } else {
            # ... next cycles, the column is added to the dataset
            if (dataset_title == "coordinates") {
                dataset <-
                    dataset %>%
                    mutate(latitude = dataset_values[[1]]) %>%
                    mutate(longitude = dataset_values[[2]])
            } else {
                dataset <-
                    dataset %>%
                    mutate(!!quo_name(dataset_title) := dataset_values)
            }
        }

    }


    return(dataset)
}
