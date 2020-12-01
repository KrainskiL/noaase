**Travis Build Status**

[![Travis-CI Build Status](https://travis-ci.org/KrainskiL/noaase.svg?branch=master)](https://travis-ci.org/KrainskiL/noaase)

`noaase` is the R package developed as a capstone project in
["Mastering Software Development in R Specialization"](https://www.coursera.org/specializations/r).

Goal of the project is to build software package in R to clean and visualize data from the ["NOAA Significant Earthquake Database"](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1).

## Installation instructions

```{r, eval=FALSE}
library(devtools)
devtools::install_github('KrainskiL/noaase', build_vignettes = TRUE)
library(noaase)
```

## Functions in `noaase` package:

**Data cleaning:**

* `eq_clean_data` produce cleaned `DATE` column, convert `LATITUDE` and `LONGITUDE` to numeric type and clean `LOCATION_NAME` using `eq_location_clean` function.
* `eq_location_clean` removes country name from `LOCATION_NAME` and convert text to title case.

**ggplot2 geoms:**

* `geom_timeline` creates earthquakes timeline with magnitude and the number of associated deaths within picked countries.
* `geom_timeline_label` adds annotations to the `n_max` largest earthquakes on the timeline.

**leaflet maps:**

* `eq_map` creates interactive map with earthquakes epicenters and annotation based on dataset column e.g. date.
* `eq_create_label` creates HTML label ("Location", "Total deaths", "Magnitude") to use as annotation on leaflet map.

## Examples

Download and load the data from NOAA Website or use data delivered with the package

```{r eval = FALSE}
filename = system.file("extdata", "eq.txt", package="noaase")
library(readr)
df = readr::read_delim(filename, delim = "\t")
```

Clean the data using `eq_clean_data` function.

```{r eval = FALSE}
clean_df = eq_clean_data(df)
```

To produce interactive `leaflet` map with earthquakes epicenters (annotated with date) in Mexico since 2000 use `eq_map` function:

```{r eval = FALSE}
clean_df %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  eq_map(annot_col = "DATE")
```

To produce similar map as in example above, but with annotated Location, Total deaths and Magnitude use the `eq_create_label` before the `eq_map` function:

```{r eval = FALSE}
clean_df %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")
```

To create timeline of earthquakes with magnitude, death statistics and date of occurrence use `ggplot` with `geom_timeline` geom:

```{r eval = FALSE}
clean_df %>%
 dplyr::filter(COUNTRY == c("MEXICO","USA") & lubridate::year(DATE) >= 2010) %>%
 ggplot(aes(x = DATE,
            y = COUNTRY,
            color = TOTAL_DEATHS,
            size = EQ_PRIMARY)) +
 geom_timeline(alpha = .7) +
 theme(legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) +
 ggtitle("Earthquakes timeline in USA and Mexico") +
 labs(size = "Richter scale value", color = "# deaths")
```

Use the `geom_timeline_label` geom for adding annotations to the `n_max` largest earthquakes:

```{r eval = FALSE}
clean_df %>%
  dplyr::filter(COUNTRY == c("MEXICO","USA") & lubridate::year(DATE) >= 2010) %>%
  ggplot(aes(x = DATE,
             y = COUNTRY,
             color = TOTAL_DEATHS,
             size = EQ_PRIMARY)) +
  geom_timeline(alpha = .7) +
  geom_timeline_label(aes(label = LOCATION_NAME), n_max = 4) +
  theme(legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(hjust = 0.5)) +
  ggtitle("Earthquakes timeline in USA and Mexico") +
  labs(size = "Richter scale value", color = "# deaths")
```
