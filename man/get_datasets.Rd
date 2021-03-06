% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\name{get_datasets}
\alias{get_datasets}
\title{Get the list of datasets available in the WIND Toolkit.}
\usage{
get_datasets(endpoint, host, api_key)
}
\arguments{
\item{endpoint}{Endpoint of the dataset.}

\item{host}{Host with files of the dataset.}

\item{api_key}{API key to access the dataset.}
}
\value{
A dataframe (tibble) containing the datasets available via the WIND
Toolkit API.
}
\description{
Get a dataframe with the datasets information from the WIND
Toolkit.
}
