filename = system.file("extdata", "eq.txt", package="noaase")
df = readr::read_delim(filename, delim = '\t')
clean_df = eq_clean_data(df)

#' Testting eq_clean_data function
testthat::expect_that(clean_df$DATE, testthat::is_a('Date'))
testthat::expect_that(clean_df$LATITUDE, testthat::is_a('numeric'))
testthat::expect_that(clean_df$LONGITUDE, testthat::is_a('numeric'))

#' Testing eq_location_clean function
clean_df = eq_location_clean(clean_df)
testthat::expect_that(clean_df,testthat::is_a('data.frame'))
testthat::expect_true('LOCATION_NAME' %in% (colnames(clean_df)))

#' Testing eq_map function
testthat::expect_that(clean_df %>%
                        dplyr::filter(COUNTRY == 'MEXICO' & lubridate::year(DATE) >= 2000) %>%
                        eq_map(annot_col = 'DATE'), testthat::is_a('leaflet'))

#' Testing eq_create_label function
testthat::expect_that(eq_create_label(clean_df), testthat::is_a('character'))

testthat::expect_that(clean_df %>%
                        dplyr::filter(COUNTRY == 'MEXICO' & lubridate::year(DATE) >= 2000) %>%
                        dplyr::mutate(popup_text = eq_create_label(.)) %>%
                        eq_map(annot_col = 'popup_text'), testthat::is_a('leaflet'))

#' Testing geom_timeline function
testthat::expect_that(clean_df %>%
                        dplyr::filter(COUNTRY == c("MEXICO","USA") & lubridate::year(DATE) >= 2010) %>%
                        ggplot(aes(x = DATE,
                                   y = COUNTRY,
                                   color = TOTAL_DEATHS,
                                   size = EQ_PRIMARY)) +
                        geom_timeline(alpha = .7), testthat::is_a('ggplot'))

#' Testing geom_timeline_label function
testthat::expect_that(clean_df %>%
                        dplyr::filter(COUNTRY == c("MEXICO","USA") & lubridate::year(DATE) >= 2010) %>%
                        ggplot(aes(x = DATE,
                                   y = COUNTRY,
                                   color = TOTAL_DEATHS,
                                   size = EQ_PRIMARY)) +
                        geom_timeline(alpha = .7) +
                        geom_timeline_label(aes(label = LOCATION_NAME), n_max = 4), testthat::is_a('ggplot'))