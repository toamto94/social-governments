server <- function(input, output) {
    if(exists("start_country") == FALSE){
        start_country <- TRUE
    }

    library(odbc)
    library(configr)
    library(RPostgreSQL)
    library(DBI)
	library(readr)
	
	

    param <- configr::read.config("C:/Users/fabia/Desktop/social-governments/config2.ini")
    drv <- DBI::dbDriver("PostgreSQL")
	sql_statement <- read_file("C:/Users/fabia/Desktop/social-governments/SQL/1.sql")
    con <- tryCatch(dbConnect(drv, dbname = param$dbname,
                              host = param$host, port = param$port,
                              user = param$user, password = param$password), error = function(e) e)
    if(any(!(class(con) == "error"))){
        indicatorss <- DBI::dbGetQuery(con, sql_statement)
    } else {load("data/df_indicators.RData")}

    output$subdimension_out <- shiny::renderUI({
        vec_subdimension <-
            model %>%
            dplyr::filter(dimension == input$dimension) %>%
            magrittr::extract2('subdimension') %>%
            unique()
        names(vec_subdimension) <-
            stringr::str_to_title(vec_subdimension)
        selectInput('subdimension', 'Subdimension', vec_subdimension, selected = "Fragility")
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
        selectInput('indicator', 'Indicator', vec_indicator, selected = "Fragility")
    })

    output$x_indicator_out <- shiny::renderUI({
        indicator <-
            model %>%
            magrittr::extract2('indicator.name') %>%
            unique()
        vec_indicator <- indicator %>%
            stringr::str_trim() %>%
            stringr::str_replace_all('\\W', '_')
        names(vec_indicator) <- stringr::str_to_title(indicator)
        selectInput('x_indicator', 'x-Indicator', vec_indicator, selected = vec_indicator[5])
    })
    output$y_indicator_out <- shiny::renderUI({
        indicator <-
            model %>%
            magrittr::extract2('indicator.name') %>%
            unique()
        vec_indicator <- indicator %>%
            stringr::str_trim() %>%
            stringr::str_replace_all('\\W', '_')
        names(vec_indicator) <- stringr::str_to_title(indicator)
        selectInput('y_indicator', 'y-Indicator', vec_indicator, selected = "Fragilty")
    })




    zoom_fun <- function(size){
        if(size == 'large') ret <- 3
        else ret <- 2
    }

    output$map <- leaflet::renderLeaflet({
        shiny::validate(
            shiny::need(any(input$indicator %in% colnames(indicatorss)), "loading")
        )
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

    output$timeseries <- plotly::renderPlotly({
        shiny::validate(
            shiny::need(any(input$indicator %in% colnames(indicators)), "loading")
        )
        indicator_name <- input$indicator %>% stringr::str_replace_all('_', ' ') %>%
            stringr::str_to_title()
        ggg <- ggplot2::ggplot(indicators %>%
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
            ggplot2::xlab("Year") + ggplot2::labs(colour = "Country") +
            ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "bottom",
                           legend.text = ggplot2::element_text(size = 12),
                           legend.key.size = ggplot2::unit(24, 'pt'),
                           axis.text = ggplot2::element_text(size = 12),
                           axis.title = ggplot2::element_text(size = 14)) +
            ggplot2::theme_minimal() +
            ggplot2::geom_smooth(method = "loess", se = FALSE, size = .3) +
            ggplot2::ylab(indicator_name) +
            ggplot2::scale_color_brewer(palette = "Dark2")
        plotly::ggplotly(ggg, tooltip = "text") %>%
            plotly::layout(legend = list(
                orientation = "h", y = ggg$coordinates$limits$y[1]-ggg$coordinates$limits$y[2]
            )
            )
    })

    output$association <- plotly::renderPlotly({
        shiny::validate(
            shiny::need(any(input$x_indicator %in% colnames(indicators)), "loading")
        )
        x_indicator_name <- input$x_indicator %>% stringr::str_replace_all('_', ' ') %>%
            stringr::str_to_title()
        y_indicator_name <- input$y_indicator %>% stringr::str_replace_all('_', ' ') %>%
            stringr::str_to_title()
        ass_indicators <- indicators  %>%
            dplyr::rename(current_x_indicator= !!(input$x_indicator),
                          current_y_indicator= !!(input$y_indicator),
                          `Country` = country,
                          Year = year)
        ass_indicators <- ass_indicators %>%
            dplyr::mutate(rounded_x_indicator = current_x_indicator %>% round(2),
                          rounded_y_indicator = current_y_indicator %>% round(2))
        smoothing <- lm(ass_indicators[["current_y_indicator"]]~ ass_indicators[["current_x_indicator"]])
        plotly::ggplotly(ggplot2::ggplot(ass_indicators %>%
                            dplyr::mutate(text = paste0('Country: ', Country,
                                                        '<br>Year: ', Year,
                                                        '<br>', x_indicator_name, ': ', rounded_x_indicator,
                                                        '<br>', y_indicator_name, ': ', rounded_y_indicator)),
                        ggplot2::aes_string(x = "current_x_indicator", y = "current_y_indicator", text = "text")) +
            ggplot2::geom_point(
                ggplot2::aes_string(x = "current_x_indicator", y = "current_y_indicator",color = "Year"),
                size = 0.3) + ggplot2::theme_minimal()  +
            ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "right",
                           legend.text = ggplot2::element_text(size = 12),
                           legend.key.size = ggplot2::unit(24, 'pt'),
                           axis.text = ggplot2::element_text(size = 12),
                           axis.title = ggplot2::element_text(size = 14)) +
            ggplot2::ylab(y_indicator_name) + ggplot2::xlab(x_indicator_name), tooltip = "text")
    })



    output$metainfo <- shiny::renderText({
        shiny::validate(
            shiny::need(any(input$indicator %in% colnames(indicators)), "loading")
        )
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

