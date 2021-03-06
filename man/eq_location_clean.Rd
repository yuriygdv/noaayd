% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleandata.R
\name{eq_location_clean}
\alias{eq_location_clean}
\title{Clean the Location Name}
\usage{
eq_location_clean(data)
}
\arguments{
\item{data}{A raw NOAA dataset as a dataframe that has been already read into R}
}
\value{
a dataframe with clean location name
}
\description{
cleans the LOCATION_NAME column by stripping out the country name (including the colon)
and converts names to title case (as opposed to all caps).
}
\note{
the function does not read data from a file on a disk and removes NA locations
}
\examples{
\dontrun{
eq_clean_data("rawdata")
}

}
