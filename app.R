library(magrittr)

# devtools::install_github('ricardo-bion/ggradar')

# Load data ====================================================================

load('data/indicators.rda')
load('data/model.rda')

# Add legend decreasing ========================================================

addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft",
                                                    "topleft"), pal, values, na.label = "NA", bins = 7, colors,
                                  opacity = 0.5, labels = NULL, labFormat = leaflet::labelFormat(),
                                  title = NULL, className = "info legend", layerId = NULL,
                                  group = NULL, data = leaflet::getMapData(map), decreasing = FALSE) {
    position <- match.arg(position)
    type <- "unknown"
    na.color <- NULL
    extra <- NULL
    if (!missing(pal)) {
        if (!missing(colors))
            stop("You must provide either 'pal' or 'colors' (not both)")
        if (missing(title) && inherits(values, "formula"))
            title <- deparse(values[[2]])
        values <- leaflet::evalFormula(values, data)
        type <- attr(pal, "colorType", exact = TRUE)
        args <- attr(pal, "colorArgs", exact = TRUE)
        na.color <- args$na.color
        if (!is.null(na.color) && grDevices::col2rgb(na.color, alpha = TRUE)[[4]] ==
            0) {
            na.color <- NULL
        }
        if (type != "numeric" && !missing(bins))
            warning("'bins' is ignored because the palette type is not numeric")
        if (type == "numeric") {
            cuts <- if (length(bins) == 1)
                pretty(values, bins)
            else bins

            if (length(bins) > 2)
                if (!all(abs(diff(bins, differences = 2)) <=
                         sqrt(.Machine$double.eps)))
                    stop("The vector of breaks 'bins' must be equally spaced")
            n <- length(cuts)
            r <- range(values, na.rm = TRUE)
            cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
            n <- length(cuts)
            p <- (cuts - r[1])/(r[2] - r[1])
            extra <- list(p_1 = p[1], p_n = p[n])
            p <- c("", paste0(100 * p, "%"), "")
            if (decreasing == TRUE){
                colors <- pal(rev(c(r[1], cuts, r[2])))
                labels <- rev(labFormat(type = "numeric", cuts))
            }else{
                colors <- pal(c(r[1], cuts, r[2]))
                labels <- rev(labFormat(type = "numeric", cuts))
            }
            colors <- paste(colors, p, sep = " ", collapse = ", ")

        }
        else if (type == "bin") {
            cuts <- args$bins
            n <- length(cuts)
            mids <- (cuts[-1] + cuts[-n])/2
            if (decreasing == TRUE){
                colors <- pal(rev(mids))
                labels <- rev(labFormat(type = "bin", cuts))
            }else{
                colors <- pal(mids)
                labels <- labFormat(type = "bin", cuts)
            }

        }
        else if (type == "quantile") {
            p <- args$probs
            n <- length(p)
            cuts <- quantile(values, probs = p, na.rm = TRUE)
            mids <- quantile(values, probs = (p[-1] + p[-n])/2,
                             na.rm = TRUE)
            if (decreasing == TRUE){
                colors <- pal(rev(mids))
                labels <- rev(labFormat(type = "quantile", cuts, p))
            }else{
                colors <- pal(mids)
                labels <- labFormat(type = "quantile", cuts, p)
            }
        }
        else if (type == "factor") {
            v <- sort(unique(na.omit(values)))
            colors <- pal(v)
            labels <- labFormat(type = "factor", v)
            if (decreasing == TRUE){
                colors <- pal(rev(v))
                labels <- rev(labFormat(type = "factor", v))
            }else{
                colors <- pal(v)
                labels <- labFormat(type = "factor", v)
            }
        }
        else stop("Palette function not supported")
        if (!any(is.na(values)))
            na.color <- NULL
    }
    else {
        if (length(colors) != length(labels))
            stop("'colors' and 'labels' must be of the same length")
    }
    legend <- list(colors = I(unname(colors)), labels = I(unname(labels)),
                   na_color = na.color, na_label = na.label, opacity = opacity,
                   position = position, type = type, title = title, extra = extra,
                   layerId = layerId, className = className, group = group)
    leaflet::invokeMethod(map, data, "addLegend", legend)
}

# Geocode ======================================================================

geocode <- function(variable) {
    world <- spData::world %>% dplyr::select(iso_a2, geom)
    return(variable %>% dplyr::inner_join(world, by = c(country_id = 'iso_a2')))
}

# Leaflet visualization

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

# UI ===========================================================================

ui <- function() {
    country_id <-
        indicators %>%
        dplyr::select(country_id, country) %>%
        unique()

    vec_country_id <- country_id$country_id
    names(vec_country_id) <- country_id$country

    vec_dimension <- unique(model$dimension)
    names(vec_dimension) <- stringr::str_to_title(vec_dimension)

    ui <- shiny::fluidPage(

        # Application title
        shiny::titlePanel(
            shiny::tags$h1('Cicero', align = 'center'),
            windowTitle = "Cicero"
        ),

        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::selectInput('country_id', 'Country', vec_country_id, multiple=TRUE),
                shiny::selectInput('year', 'Year', 1990:2018),
                shiny::selectInput('dimension', 'Dimension', vec_dimension),
                shiny::uiOutput('subdimension_out'),
                shiny::uiOutput('indicator_out'),
                shiny::htmlOutput('metainfo'),
                width=3
            ),

            shiny::mainPanel(
                shiny::column(
                    width = 7,
                    leaflet::leafletOutput('map', height = 400),
                    shiny::plotOutput('timeseries', height = 400)
                ),
                shiny::column(
                    width = 5,
                    shiny::plotOutput('spider', height = 400),
                    DT::dataTableOutput('ranking')
                )
            )
        )
    )
}

# Server =======================================================================

server <- function(input, output) {

    if(exists("start_country") == FALSE){
        start_country <- TRUE
    }

    output$subdimension_out <- shiny::renderUI({
        vec_subdimension <-
            model %>%
            dplyr::filter(dimension == input$dimension) %>%
            magrittr::extract2('subdimension') %>%
            unique()
        names(vec_subdimension) <-
            stringr::str_to_title(vec_subdimension)
        selectInput('subdimension', 'Subdimension', vec_subdimension)
    })

    output$indicator_out <- shiny::renderUI({
        indicator <-
            model %>%
            dplyr::filter(dimension == input$dimension,
                          subdimension == input$subdimension) %>%
            magrittr::extract2('indicator.name') %>%
            unique()
        vec_indicator <- indicator %>%
            stringr::str_trim() %>%
            stringr::str_replace_all('\\W', '_')
        names(vec_indicator) <- stringr::str_to_title(indicator)
        selectInput('indicator', 'Indicator', vec_indicator)
    })

    zoom_fun <- function(size){
        if(size == 'large') ret <- 3
        else ret <- 2
    }

    output$map <- leaflet::renderLeaflet({
        if(is.numeric(indicators[[input$indicator]]))
            indicators[[input$indicator]] <- round(indicators[[input$indicator]], digits = 2)
        scale_dir <-
            model %>%
            dplyr::filter(indicator.name == stringr::str_replace_all(input$indicator, '_', ' ')) %>%
            magrittr::extract2('scale.direction')
        vis_leaflet(indicators, indicator = input$indicator,
                    year_slice = input$year, legend_title = FALSE,
                    reverse = (scale_dir == 'low')) %>%
            leaflet::setView(lng = 0, lat = 30,  zoom = zoom_fun('small'))
    })

    output$timeseries <- shiny::renderPlot({
        shiny::validate(
            shiny::need(length(input$country_id) > 0, "Provide countries")
        )
        indicator_name <- input$indicator %>% stringr::str_replace_all('_', ' ') %>%
            stringr::str_to_title()
        ggplot2::ggplot(indicators %>%
                            dplyr::filter(country_id %in% input$country_id) %>%
                            dplyr::rename(current_indicator= !!(input$indicator),
                                          `Country` = country,
                                          Year = year) %>%
                            dplyr::mutate(rounded_indicator = current_indicator %>% round(2)) %>%
                            dplyr::mutate(text = paste0('Country: ', Country,
                                                        '<br>Year: ', Year,
                                                        '<br>', indicator_name, ': ', rounded_indicator)),
                        ggplot2::aes_string(x = "Year", y = "current_indicator", colour = "Country", text = "text", group = "Country")) +
            ggplot2::geom_point() + ggplot2::geom_line() +  ggplot2::ylab(paste0(input$indicator)) +
            ggplot2::xlab("Year") + ggplot2::theme_minimal() + ggplot2::labs(colour = "Country") +
            ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "bottom",
                           legend.text = ggplot2::element_text(size = 12),
                           legend.key.size = ggplot2::unit(24, 'pt'),
                           axis.text = ggplot2::element_text(size = 12),
                           axis.title = ggplot2::element_text(size = 14)) +
            ggplot2::geom_smooth(method = "loess", se = FALSE, size = .3) +
            ggplot2::ylab(indicator_name) +
            ggplot2::scale_color_brewer(palette = "Dark2")
    })

    output$metainfo <- shiny::renderText({
        row <-
            model %>%
            dplyr::filter(indicator.name == stringr::str_replace_all(input$indicator, '_', ' '))
        out <- paste0('<b>Source:</b> ', row$source)
        if(!is.na(row$scaling)) {
            out <- paste0(out, '<br><b>Scaling:</b> ', row$scaling)
        }
        if(!is.na(row$Definition)) {
            out <- paste0(out, '<br><b>Definition:</b> ', row$Definition)
        }
        if(!is.na(row$Explanation)) {
            out <- paste0(out, '<br><b>Explanation:</b> ', row$Explanation)
        }
        out
    })

    output$spider <- shiny::renderPlot({
        shiny::validate(
            shiny::need(length(input$country_id) > 0, "Provide countries")
        )
        theme_radar <-
            ggplot2::theme(text = ggplot2::element_text(size = 16),
                           legend.position = 'bottom',
                           legend.text = ggplot2::element_text(size = 12),
                           legend.box.margin = ggplot2::margin(-15, 0, 0),
                           legend.margin = ggplot2::margin(-5, 0, 0),
                           legend.key.size = ggplot2::unit(8, 'pt')
            )
        fragility_radar <- indicators %>%
            dplyr::select(country_id, year, country, authority,
                          capacity, legitimacy,
                          social_cohesion) %>% na.omit() %>%
            dplyr::filter(year == input$year, country_id %in% input$country_id) %>%
            dplyr::select(-year, -country_id)

        # Plot
        p <- ggradar::ggradar(axis.labels =c("Authority", "Capacity", "Legitimacy", "S. Cohesion"),
                              grid.label.size = 4,
                              fragility_radar, axis.label.size  = 4, group.line.width = 0.9,
                              group.point.size = 3, gridline.mid.colour = 'grey', plot.extent.x.sf = 1.15,
                              grid.min = -4, grid.mid = 0, grid.max = 4,
                              values.radar = c("", "", "")) +
            theme_radar +
            ggplot2::scale_color_brewer(palette = "Dark2")
        p
    })

    output$ranking <- DT::renderDataTable({
        shiny::validate(
            shiny::need(any(input$indicator %in% colnames(indicators)), "loading")
        )
        selection <-
            indicators %>%
            dplyr::filter(year == input$year) %>%
            dplyr::select(country, indicator = !!input$indicator, country_id)
        scale_dir <-
            model %>%
            dplyr::filter(stringr::str_replace_all(indicator.name, '\\W', '_') == input$indicator) %>%
            magrittr::extract2('scale.direction')
        if(scale_dir == 'low'){
            ordered_selection <-
                selection %>%
                dplyr::arrange(indicator)
        }
        else{
            ordered_selection <-
                selection %>%
                dplyr::arrange(dplyr::desc(indicator))
        }
        rk <- which(ordered_selection$country_id == input$country_id[1])
        country_names <-
            ordered_selection %>%
            dplyr::filter(country_id %in% input$country_id) %>%
            magrittr::extract2('country')
        data_selection <-
            ordered_selection %>%
            dplyr::select(country, indicator)
        out <-
            DT::datatable(
                na.omit(data_selection),
                options = list(
                    searching = FALSE,
                    displayStart = max(rk - 6, 0),
                    pageLength = 11
                ),
                colnames = c(
                    'Country',
                    input$indicator %>%
                        stringr::str_replace_all('_', ' ') %>%
                        stringr::str_to_title()
                )
            ) %>%
            DT::formatRound(columns = 'indicator')
        if(length(country_names) > 0)
            out <-
            out %>%
            DT::formatStyle(
                target = "row", columns = "country",
                backgroundColor = DT::styleEqual(
                    country_names,
                    rep("skyblue", length(country_names))
                )
            )
        out
    })
}

app <- shiny::shinyApp(ui = ui, server = server)
