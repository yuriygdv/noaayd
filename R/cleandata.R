#' Clean the raw data
#'
#' This is a function that reads a raw NOAA data file and creates date column
#' by uniting the year, month, day and converting it to the Date class.
#' Also, it converts LATITUDE and LONGITUDE columns to numeric class
#'
#' @param data A raw NOAA dataset as a dataframe that has been already read into R
#'
#' @note the function does not read data from a file on a disk
#'
#' @return a clean dataframe with new date column
#'
#' @examples
#' \dontrun{
#' eq_clean_data("rawdata")
#' }
#'
#' @export
eq_clean_data <- function(data){
    data <- dplyr::filter(data, YEAR > 0 & !base::is.na(MONTH) & !base::is.na(DAY))
    data <- dplyr::mutate(data, DATE = base::paste(YEAR, MONTH, DAY, sep="-" ))
    data <- dplyr::mutate(data, DATE = base::as.Date(DATE, "%Y-%m-%d"))
    data <- dplyr::mutate(data, LATITUDE = base::as.numeric(LATITUDE), LONGITUDE = base::as.numeric(LONGITUDE))
    data <- dplyr::mutate(data, DEATHS = base::as.numeric(DEATHS), EQ_PRIMARY = base::as.numeric(EQ_PRIMARY))

    data <- eq_location_clean(data)
}



#' Clean the Location Name
#'
#' cleans the LOCATION_NAME column by stripping out the country name (including the colon)
#' and converts names to title case (as opposed to all caps).
#'
#' @param data A raw NOAA dataset as a dataframe that has been already read into R
#'
#'
#' @note the function does not read data from a file on a disk and removes NA locations
#'
#' @return a dataframe with clean location name
#'
#' @importFrom dplyr filter %>%
#'
#' @importFrom glue collapse
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eq_clean_data("rawdata")
#' }
#'
eq_location_clean <- function(data) {
    data <- dplyr::filter(data, !base::is.na(LOCATION_NAME))

    for (n in 1:base::length(data$LOCATION_NAME)) {
        lname <- data$LOCATION_NAME[n]
        lname <- base::sub(pattern = ":$" , replacement = "" , x = lname)
        lname <- base::sub(pattern = ":\\s\\w$" , replacement = "" , x = lname)
        start <- base::regexpr(":[^:]*$", lname)
        stop <- base::regexpr(".$", lname)
        lname <- base::substr(lname, start = start+2, stop = stop)
        s <- base::tolower(lname)
        s <- base::unlist(strsplit(s, ";|,|\\s|;\\s"))
        base::substr(s, base::regexpr("\\w{1}",s), base::regexpr("\\w{1}",s)) <- base::toupper(base::substr(s, base::regexpr("\\w{1}",s),base::regexpr("\\w{1}",s)))
        s <- glue::collapse(s, sep = " ")
        data$LOCATION_NAME[n] <- s
    }
    return(data)
}
