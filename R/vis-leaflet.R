#' Visualize with leaflet
#'
#' @export

vis_leaflet <- function(variable, indicator, year_slice,
                        palette = c("RdYlGn"), scale = c("numeric", "quantile"),
                        show_real_map = TRUE, show_labels = TRUE,
                        legend_title = TRUE, reverse = FALSE) {
    scale <- match.arg(scale)
    variable <- geocode(variable) %>%
        dplyr::filter(year == year_slice) %>%
        sf::st_as_sf()
    map <- leaflet::leaflet(variable)
    ind <- variable[[indicator]]

    if(all(is.na(ind))) {
        return(map)
    }

    # Add Polygons

    if(is.factor(ind) | is.character(ind))
        pal <- leaflet::colorFactor(palette = palette,
                           domain = ind, reverse = reverse)
    else if(scale == "numeric")
        pal <- leaflet::colorNumeric(palette = palette,
                            domain = ind, reverse = reverse)
    else if(scale == "quantile")
        pal <- leaflet::colorQuantile(palette = palette,
                             domain = ind, reverse = reverse)

    # Highlighting and Labelling

    if(show_labels) {
        highlight <- leaflet::highlightOptions(color = "white", weight = 30,
                                      bringToFront = TRUE)
        labels <- paste0(
            "<strong>", variable$country, "</strong>", "<br/>",
            ind) %>% lapply(htmltools::HTML)
        labelOptions <- leaflet::labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
    }
    else {
        highlight <- NULL
        labels <- NULL
        labelOptions <- NULL
    }

    map <- map %>%
        leaflet::addPolygons(stroke = FALSE,
                    fillColor = as.formula(paste0("~pal(", indicator, ")")),
                    fillOpacity = .7,
                    opacity = 1,
                    color = "white",
                    weight = 5,
                    smoothFactor = 1,
                    highlight = highlight,
                    label = labels,
                    labelOptions = labelOptions) %>%
        addLegend_decreasing(pal = pal, values = as.formula(paste0("~", indicator)),
                             opacity = .7,
                             title = ifelse(legend_title, indicator, NA),
                             position = "bottomright", decreasing = T)

    if(show_real_map) map <- map  %>% leaflet::addTiles(options = leaflet::tileOptions(minZoom = 2, zoomDelta =0.1))
    map
}
