ui <- function() {
    country_id <-
        indicators %>%
        dplyr::select(country_id, country) %>%
        unique()

    vec_country_id <- country_id$country_id
    names(vec_country_id) <- country_id$country

    vec_dimension <- unique(model$dimension)
    names(vec_dimension) <- stringr::str_to_title(vec_dimension)

    ui <- shinydashboard::dashboardPage(
        header <- shinydashboard::dashboardHeader(
            title = "Social Bonds",
            tags$li(class="dropdown",
                    tags$a(href="https://github.com/toamto94/social-governments/",
                    shiny::icon("github"), "Source Code", target = "blank")),
            tags$li(class="dropdown",
                    tags$a(href="http://github.com/",
                           shiny::icon("search"), "", target = "blank")),
            tags$li(class="dropdown",
                    tags$a(href="http://github.com/",
                           shiny::icon("user"), "", target = "blank")),
            tags$li(class="dropdown",
                    tags$a(href="http://github.com/",
                           shiny::icon("file"), "", target = "blank"))
        ),
        sidebar <- shinydashboard::dashboardSidebar(
            shinydashboard::sidebarMenu(
                shinydashboard::menuItem("Main Page", tabName = "dashboard"
                         ),
                shinydashboard::menuItem("Accociation Analysis", tabName = "association", icon = icon("chart-line")),

                shiny::selectInput('country_id', 'Country', vec_country_id, multiple=T, selected = vec_country_id[1]),
                shiny::sliderInput(
                    "year",
                    "Year",
                    min = 1990, max = 2018, value = 2017, sep = ""
                ),
                shiny::selectInput('dimension', 'Dimension', vec_dimension),
                shiny::uiOutput('subdimension_out'),
                shiny::uiOutput('indicator_out'),
                shiny::htmlOutput('metainfo')),

            # MA color in header
            tags$head(tags$style(HTML('
                .logo {
                   background-color: #002060 !important;
                }
                .navbar {
                   background-color: #002060 !important;
                }
                .main-header .logo {
                    font-family: "Tw Cen MT", "Century Gothic", Arial, serif;
                    font-weight: bold;
                    font-size: 40px;
                }
                /* body white */
                .content-wrapper, .right-side {
                   background-color: #FFFFFF;
                   }

                              ')))


        ),
        body <- shinydashboard::dashboardBody(
            tags$style(HTML("

                            .box.box-solid.box-primary>.box-header {
                            color:#FFFFFF;
                            background:#002060
                            }

                            .box.box-solid.box-primary{
                            header-background-color:#FFFFFF;
                            border-bottom-color:#002060;
                            border-left-color:#002060;
                            border-right-color:#002060;
                            border-top-color:#002060;
                            background-color:#FFFFFF
                            }

                            ")),
            shinydashboard::tabItems(
                shinydashboard::tabItem(tabName = "dashboard",
                          shiny::fluidRow(
                                      shinydashboard::box(
                                          title = actionLink("titleId", "Map", icon = icon("refresh")),
                                          leaflet::leafletOutput('map'),status = "primary", solidHeader = T, collapsible = T),
                                      shinydashboard::box(
                                          title = actionLink("titleId", "Timeseries", icon = icon("refresh")),
                                          plotly::plotlyOutput('timeseries'), status = "primary", solidHeader = T, collapsible = T)
                          ),
                          shiny::fluidRow(
                              shinydashboard::box(
                                  title = actionLink("titleId", "Spider", icon = icon("refresh")),
                                  shiny::plotOutput('spider'), status = "primary", solidHeader = T, collapsible = T),
                              shinydashboard::box(
                                  title = actionLink("titleId", "Rank", icon = icon("refresh")),
                                  DT::dataTableOutput('ranking'), status = "primary", solidHeader = T, collapsible = T)
                          )



                ),
                shinydashboard::tabItem(tabName = "association",
                                        shiny::fluidPage(
                                        #shiny::fluidRow(
                                        shinydashboard::box(width = 12,
                                            title = actionLink("titleId", "Scatterplot", icon = icon("refresh")),
                                            plotly::plotlyOutput('association'), status = "primary", solidHeader = T, collapsible = T),
                                       # ),
                                        shiny::fluidRow(shinydashboard::box(shiny::uiOutput('x_indicator_out')),
                                                        shinydashboard::box(shiny::uiOutput("y_indicator_out")))
                                        )


                )
            )


        )
    )

}
