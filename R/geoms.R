#' Timeline for earthquakes
#'
#' @param mapping Set of aesthetics created by aes.
#' @param data Cleaned NOAA data.frame
#' @param stat A string of statistical transformation.
#' @param position Position adjustment.
#' @param na.rm A logical for dealing with missing values.
#' @param show.legend A logical for showing legend or not.
#' @param inherit.aes A logical of whether or not override default aesthetics.
#' @param ... Other arguments passed to layer
#'
#' @details A timeline is a visualization of earthquakes ordered by date of occurrence, point's color corresponds to the number of deaths
#' and the size represents the magnitude of earthquake.
#' 
#' @examples
#' \dontrun{
#' clean_df %>%
#' dplyr::filter(COUNTRY == c("MEXICO","USA") & lubridate::year(DATE) >= 2010) %>%
#' ggplot(aes(x = DATE,
#'            y = COUNTRY,
#'            color = TOTAL_DEATHS,
#'            size = EQ_PRIMARY)) +
#' geom_timeline(alpha=.7) +
#' theme(legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(hjust=0.5)) +
#' ggtitle("Earthquakes timeline") +
#' labs(size = "Richter scale value", color = "# deaths")
#' }
#' @importFrom ggplot2 layer
#' 
#' @export
geom_timeline = function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE, show.legend = NA,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' GeomTimeline
#'
#' Geom object used to draw timeline
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid segmentsGrob gpar pointsGrob gList
#'
#' @export
GeomTimeline = ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                                 required_aes = "x",
                                 default_aes = ggplot2::aes(y=0, colour="black", shape=19, size=1, stroke = 0.5, alpha = 0.5, fill = NA),
                                 draw_key = ggplot2::draw_key_point,
                                 draw_panel = function(data, panel_params, coord) {
                                   coords <- coord$transform(data, panel_params)
                                   grid::gList(
                                     grid::pointsGrob(
                                       coords$x, coords$y,
                                       pch = coords$shape,
                                       gp = grid::gpar(col = alpha(coords$colour, coords$alpha),
                                                       fill = alpha(coords$fill, coords$alpha),
                                                       fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                                                       lwd = coords$stroke * .stroke / 2)
                                     ),
                                     grid::segmentsGrob(
                                       x0 = min(coords$x), y0 = coords$y, x1 = max(coords$x), y1 = coords$y,
                                       gp = grid::gpar(col = "black", lwd = 1)
                                     )
                                   )
                                 }
)

#' Timeline label
#'
#' @param mapping Set of aesthetics created by aes.
#' @param data Cleaned NOAA data.frame
#' @param stat A string of statistical transformation.
#' @param position Position adjustment.
#' @param na.rm A logical for dealing with missing values.
#' @param show.legend A logical for showing legend or not.
#' @param inherit.aes A logical of whether or not override default aesthetics.
#' @param ... Other arguments passed to layer.
#'
#' @details Adds annotations to the `n_max` largest earthquakes (used with geom_timeline).
#' @examples
#' \dontrun{
#' clean_df %>%
#' dplyr::filter(COUNTRY == c("MEXICO","USA") & lubridate::year(DATE) >= 2010) %>%
#' ggplot(aes(x = DATE,
#'            y = COUNTRY,
#'            color = TOTAL_DEATHS,
#'            size = EQ_PRIMARY)) +
#' geom_timeline(alpha = .7) +
#' geom_timeline_label(aes(label = LOCATION_NAME), n_max = 4) +
#' theme(legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(hjust=0.5)) +
#' ggtitle("Earthquakes timeline") +
#' labs(size = "Richter scale value", color = "# deaths")
#' }
#' @import ggplot2 grid
#' @importFrom dplyr filter mutate
#' @export
geom_timeline_label = function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = FALSE,
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' GeomTimelineLabel
#' 
#' Geom object for drawing timeline labels
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid segmentsGrob gpar textGrob gList
#' @importFrom dplyr slice arrange_ group_by_ %>%
#' @export
GeomTimelineLabel = ggplot2::ggproto("GeomTimelinelabel", ggplot2::Geom,
                                      required_aes = c("x","label"),
                                      default_aes = ggplot2::aes(y=0, n_max=0, y_length=1),
                                      draw_key = ggplot2::draw_key_point,
                                      draw_panel = function(data, panel_params, coord) {
                                        
                                        if (data$n_max[1]>0){
                                          if (data$y[1]==0){
                                            data<- data %>%
                                              dplyr::arrange(~ desc(size)) %>%
                                              dplyr::slice(1:data$n_max[1])
                                          }
                                          else {
                                            data<- data %>%
                                              dplyr::arrange(~ desc(size)) %>%
                                              dplyr::group_by(~ y) %>%
                                              dplyr::slice(1:data$n_max[1])
                                          }
                                        }
                                        if (!data$y[1]==0){
                                          data$y_length = dim(table(data$y))
                                        }
                                        
                                        coords = coord$transform(data, panel_params)
                                        grid::gList(
                                          grid::segmentsGrob(
                                            x0 = coords$x, y0 = coords$y, x1 = coords$x, y1 = (.2/coords$y_length)+coords$y,
                                            gp = grid::gpar(col = "black", lwd = .5)
                                          ),
                                          grid::textGrob(
                                            label = coords$label,
                                            x = coords$x, y = (.2/coords$y_length)+coords$y , just = "left", rot = 45
                                          )
                                        )
                                      }
)