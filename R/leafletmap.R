#' Mapping the earthquake epicenters
#'
#' Mapping the earthquake epicenters and providing some annotations with the mapped data.
#' Takes an argument data containing the filtered data frame with earthquakes to visualize.
#' The function maps the epicenters (LATITUDE/LONGITUDE) and annotates each point with in
#' pop up window containing annotation data stored in a column of the data frame.
#' The user can choose which column is used for the annotation in the pop-up with
#' a function argument named annot_col. Each earthquake is shown with a circle,
#' and the radius of the circle is proportional to the earthquake's magnitude (EQ_PRIMARY).
#'
#' @param data data containing the filtered data frame with earthquakes to visualize
#' @param annot_col column used for the annotation in the pop-up
#'
#' @return A map
#'
#' @importFrom dplyr select %>%
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @examples
#' \dontrun{
#' readr::read_delim("Data/earthquakes.txt", delim = "\t") %>%
#'                   eq_clean_data() %>%
#'                   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'                   eq_map(annot_col = "DATE")
#' }
#'
#' @export
eq_map <- function(data, annot_col){

    data <- data %>% dplyr::select(EQ_PRIMARY = EQ_PRIMARY, LONGITUDE = LONGITUDE, LATITUDE = LATITUDE, annot = annot_col)

    leaflet::leaflet() %>% leaflet::addTiles() %>%
        leaflet::addCircleMarkers(data = data,
                         radius = ~ EQ_PRIMARY,
                         lng = ~ LONGITUDE,
                         lat = ~ LATITUDE,
                         weight = 2, opacity = 0.5,
                         popup = ~ annot)
}



#' Html label for the interactive map created with the eq_map function
#'
#' Creates more interesting pop-ups for the interactive map created with the eq_map function.
#' Takes the dataset as an argument and creates an HTML label that can be used as the annotation
#' text in the leaflet map. This function puts together a character string for each earthquake
#' that will show the cleaned by the eq_location_clean location, the magnitude EQ_PRIMARY,
#' and the total number of deaths TOTAL_DEATHS, with boldface labels for each.
#' If an earthquake is missing values for any of these, both the label and the value will be
#' skipped for that element of the tag.
#'
#' @param data cleaned by the eq_location_clean dataset
#'
#' @return an html label
#'
#' @examples
#' \dontrun{
#' readr::read_delim("Data/earthquakes.txt", delim = "\t") %>%
#'                   eq_clean_data() %>%
#'                   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'                   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'                   eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(data){
    base::ifelse(base::is.na(data$LOCATION_NAME)|base::is.na(data$EQ_PRIMARY)|base::is.na(data$TOTAL_DEATHS),
                 NA,
                 base::paste("<b>Location</b>", data$LOCATION_NAME,  "<br />", "<b>Magnitude:</b>", data$EQ_PRIMARY, "<br />", "<b>Total death:</b>", data$TOTAL_DEATHS) )

}



