#' Marking and annotating earthquake epicenters on the map.
#'
#' @param df Cleaned NOAA data.frame.
#' @param annot_col Column used for annotation
#'
#' @return Leaflet map of the earthquakes epicenters with annotations.
#'
#'
#' @examples
#' \dontrun{
#' readr::read_delim("eq.txt", delim = "\t") %>%
#' eq_clean_data() %>% 
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   eq_map(annot_col = "DATE")
#' }
#'
#' @import dplyr leaflet
#'
#' @export

eq_map = function(df, annot_col = "DATE") {
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = df$LONGITUDE, lat = df$LATITUDE,
                              radius = as.numeric(df$EQ_PRIMARY), popup = df[[annot_col]],
                              stroke = FALSE, fillOpacity = 0.5)
}

#' More interesting pop-ups for the interactive map used with the eq_map() function
#'
#' @param df Cleaned NOAA data.frame.
#'
#' @return HTML labels to annotate leaflet map.
#'
#'
#' @examples
#' \dontrun{
#' readr::read_delim("eq.txt", delim = "\t") %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' }
#'
#' @export

eq_create_label = function(df){
  paste(ifelse(is.na(df$LOCATION_NAME),"", paste("<b>Location: </b>",df$LOCATION_NAME,"<br/>")),
        ifelse(is.na(df$EQ_PRIMARY),"", paste("<b>Magnitude: </b>",df$EQ_PRIMARY,"<br/>")),
        ifelse(is.na(df$TOTAL_DEATHS),"", paste("<b>Total deaths: </b>",df$TOTAL_DEATHS,"<br/>")))
}
