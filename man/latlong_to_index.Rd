% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{latlong_to_index}
\alias{latlong_to_index}
\title{Convert latitude and longitude to indices in the WIND Toolkit dataset}
\usage{
latlong_to_index(latitude, longitude)
}
\arguments{
\item{latitude}{The latitude of the point (WGS84).}

\item{longitude}{The longitude of the point (WGS84).}
}
\value{
A named list with elements \code{y} and \code{x} for the
corresponding indices on the WIND Toolkit dataset.
}
\description{
Given the latitude and longitude of a point, compute the
corresponding indices on the WIND Toolkit dataset for the nearest point.
}
