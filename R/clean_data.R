#' Cleaning NOAA locations
#'
#' @param loc Raw data.frame with data from NOAA website.
#'
#' @return The data.frame with cleaned LOCATION_NAME.
#'
#' @examples
#' \dontrun{
#' library(readr)
#' df = readr::read_delim("eq.txt", delim = "\t")
#' clean_df = eq_location_clean(df)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom stringr str_to_title str_remove
#' 
#' @export

eq_location_clean = function(loc) {
  loc %>%
    dplyr::mutate(LOCATION_NAME = stringr::str_to_title(stringr::str_remove(LOCATION_NAME, "^.*: +")))
  
}

#' Cleaning NOAA dates, geographic coordinates and location name
#'
#' @param df Raw data.frame with data from NOAA website.
#'
#' @return Cleaned data.frame with formatted date, longitude/latitude columns and location name.
#'
#'
#' @examples
#' \dontrun{
#' df = readr::read_delim("eq.txt", delim = "\t")
#' clean_df = eq_clean_data(df)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom lubridate ymd
#'
#' @export

eq_clean_data = function(df) {
    df %>%
    dplyr::mutate(DATE = paste(YEAR, MONTH, DAY, ''),
                  DATE = lubridate::ymd(DATE)) %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE),
                  LONGITUDE = as.numeric(LONGITUDE)) %>%
    eq_location_clean()
}