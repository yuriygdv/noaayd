#'  Plotting a time line of earthquakes
#'
#'  This is a function thatplots a time line of earthquakes ranging from xmin to xmaxdates
#'  with a point for each earthquake. Optional aesthetics include color, size, and alpha (for transparency).
#'  The xaesthetic is a date and an optional y aesthetic is a factor indicating some stratification
#'  in which case multiple time lines will be plotted for each level of the factor (e.g. country).
#'
#' @param mapping ggplot parameter
#' @param data ggplot parameter
#' @param stat ggplot parameter
#' @param position ggplot parameter
#' @param na.rm ggplot parameter
#' @param show.legend ggplot parameter
#' @param inherit.aes ggplot parameter
#'
#' @return a plot with the earthquakes on the timeline
#'
#' @examples
#' \dontrun{
#' ggplot(clean_data, aes(x=DATE, size = EQ_PRIMARY, colour = DEATHS)) + geom_timeline(alpha = 0.5) +
#'       theme(panel.background = element_blank(), legend.position = "bottom", axis.line.x = element_line(size = 1)) +
#'       scale_x_date() +scale_size_continuous(name = "Richter scale value")
#'
#' ggplot(aes(x=DATE, size = EQ_PRIMARY, colour = DEATHS)) + geom_timeline(aes(y = COUNTRY), alpha = 0.5) +
#'       theme(panel.background = element_blank(), legend.position = "bottom", axis.line.x = element_line(size = 1)) +
#'       scale_x_date() +
#'       scale_size_continuous(name = "Richter scale value")
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = GeomTimeLine, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

library(ggplot2)
library(grid)
library(scales)

GeomTimeLine <- ggproto("GeomTimeLine", Geom,
                        required_aes = c("x"),
                        default_aes = aes(size = 0.5, shape = 16, colour = "grey25", fill = "grey25", alpha = 0.5, stroke=2, y=NA),
                        draw_key = draw_key_point,

                        draw_panel = function(data, panel_scales, coord) {

                            if (all(is.na(data$y))){
                                data <- dplyr::mutate(data, y_coord = 0.2)
                            } else {
                                canvas_height <- ifelse(max(data$y)>10, 0.98, 0.95)
                                vert_interv <- canvas_height/max(data$y)
                                data <- dplyr::mutate(data, y_coord = (vert_interv*y-0.4/max(data$y)))
                            }

                            ## Transform the data  & print out the structure of the 'coords' object
                            coords <- coord$transform(data, panel_scales)

                            ## Construct a grid grob

                            c1 <- grid::circleGrob(
                                x = coords$x,
                                y = coords$y_coord,
                                r = grid::unit(0.0075*coords$size, "npc"),
                                gp = gpar(col = coords$colour, fill = coords$colour, lty = 1, alpha = coords$alpha)
                            )

                            s1 <- grid::segmentsGrob(x0=min(coords$x), x1 = max(coords$x), y0=coords$y_coord, y1=coords$y_coord,
                                               gp = gpar(col = "grey", lty = 1, alpha = 0.5))

                            grid::gTree(children = gList(s1, c1))

                        })



#'  Plotting a time line of earthquakes and labels
#'
#'  This is a function thatplots a time line of earthquakes ranging from xmin to xmaxdates
#'  with a point for each earthquake and a label showing the location.
#'  Optional aesthetics include color, size, and alpha (for transparency).
#'  The xaesthetic is a date and an optional y aesthetic is a factor indicating some stratification
#'  in which case multiple time lines will be plotted for each level of the factor (e.g. country).
#'
#' @param mapping ggplot parameter
#' @param data ggplot parameter
#' @param stat ggplot parameter
#' @param position ggplot parameter
#' @param na.rm ggplot parameter
#' @param show.legend ggplot parameter
#' @param inherit.aes ggplot parameter
#'
#' @return a plot with a timeline of earthquakes and labels
#'
#' @examples
#' \dontrun{
#' ggplot(aes(x=DATE, size = EQ_PRIMARY, colour = DEATHS, label = LOCATION_NAME)) + geom_timeline_label(aes(y = COUNTRY), alpha = 0.5, n_max=5) +
#'        theme(panel.background = element_blank(), legend.position = "bottom", axis.line.x = element_line(size = 1)) +
#'        scale_x_date() +
#'        scale_size_continuous(name = "Richter scale value")
#'
#' ggplot(aes(x=DATE, size = EQ_PRIMARY, colour = DEATHS, label = LOCATION_NAME)) + geom_timeline_label(alpha = 0.5, n_max=5) +
#'        theme(panel.background = element_blank(), legend.position = "bottom", axis.line.x = element_line(size = 1)) +
#'        scale_x_date() +
#'        scale_size_continuous(name = "Richter scale value")
#' }
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = GeomTimeLineLabel, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}



GeomTimeLineLabel <- ggproto("GeomTimeLineLabel", Geom,
                             required_aes = c("x", "label"),
                             default_aes = aes(size = 1, shape = 16, colour = "grey25", fill = "grey25", alpha = 0.5, stroke=2, y=NA, n_max=1),
                             draw_key = draw_key_point,

                             draw_panel = function(data, panel_scales, coord) {

                                 indices <- order(data$size, decreasing = TRUE)[1:data$n_max[1]]
                                 lowest <- min(data$size[indices])
                                 data <- mutate(data, label = ifelse(size>=lowest, label, NA))

                                 if (all(is.na(data$y))){
                                     data <- mutate(data, y_coord = 0.2)
                                 } else {
                                     canvas_height <- ifelse(max(data$y)>7, 0.98, 0.95)
                                     vert_interv <- canvas_height/max(data$y)
                                     data <- mutate(data, y_coord = (vert_interv*y-0.4/max(data$y)))
                                 }

                                 ## Transform the data  & print out the structure of the 'coords' object
                                 coords <- coord$transform(data, panel_scales)

                                 ## Construct a grid grob

                                 c1 <- circleGrob(
                                     x = coords$x,
                                     y = coords$y_coord,
                                     r = grid::unit(0.0075*coords$size, "npc"),
                                     gp = gpar(col = coords$colour, fill = coords$colour, lty = 1, alpha = coords$alpha)
                                 )

                                 s_horiz <- segmentsGrob(x0=min(coords$x), x1 = max(coords$x), y0=coords$y_coord, y1=coords$y_coord,
                                                         gp = gpar(col = "grey", lty = 1, alpha = 0.5, lwd = 2))

                                 coords <- filter(coords, !is.na(label))

                                 s_vert <- segmentsGrob(x0=coords$x, x1 = coords$x, y0=coords$y_coord, y1=(coords$y_coord+0.15/(sqrt(length(coords$y_coord)))),
                                                        gp = gpar(col = "grey50", lty = 1, alpha = 0.8))

                                 t1 <- textGrob(coords$label, x = unit(coords$x, "npc"), y = unit((coords$y_coord+0.15/(sqrt(length(coords$y_coord)))), "npc"),
                                                just = "left", hjust = NULL, vjust = NULL, rot = 45,
                                                check.overlap = FALSE, default.units = "npc",
                                                name = NULL, gp=gpar(alpha = 0.8), vp = NULL)

                                 gTree(children = gList(s_horiz, c1, s_vert, t1))

                             })

