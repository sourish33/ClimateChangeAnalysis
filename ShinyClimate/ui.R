#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(fontawesome)
library(dashboardthemes)

# google_font_link <- tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=New+Cicle:wght@400;700&display=swap")
google_font_link <- tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;700&display=swap")

header <- dashboardHeader(title = "Climate Change Dashboard",
                          titleWidth = 350)

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Introduction", tabName = "intro", icon = icon("dashboard")),
  menuItem("CO2", tabName = "co2", icon = icon("dashboard")),
  menuItem(
    "Temperature",
    tabName = "temperature",
    icon = icon("dashboard")
  ),
  menuItem("Sea Levels", tabName = "oceans", icon = icon("dashboard"))
))
body <- dashboardBody(
  ## changing theme
  shinyDashboardThemes(
    # theme = "blue_gradient"
    # theme = "poor_mans_flatly"
    # theme = "flat_red"
    # theme = "grey_light"
    # theme = "onenote"
    theme = "purple_gradient"
  ),
  google_font_link,
  tags$head(
    tags$style(
      HTML("
      body {
        font-family: 'New Cicle', sans-serif;
      }
    .irs-grid-text {
      font-size: 12px;
    }
    .irs--shiny .irs-min,.irs--shiny .irs-max {
      font-size: 12px;
    }
    .irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single {
      font-size: 10px;
    }

      .slider-animate-button {
        font-size: 20pt !important;
      }
      .selectize-dropdown {
      background-color: rgb(141,192,241) !important;
    }
    ")
    )
  ),
  tabItems(
    # Zeroth tab content
    tabItem(tabName = "intro",
            fluidRow(
              box(
                width = 12,
                title = 'Introduction',
                status = 'primary',
                solidHeader = TRUE,
              )
              ),
            fluidRow(
              # column(1, actionButton("previous", "Previous", style = "margin-bottom: 10px;")),
              # column(11,align ="right", actionButton("next", "Next", style = "margin-bottom: 10px; margin-right:auto"))
              column(12, class="d-flex justify-content-between, col-sm-12 col-lg-8", actionButton("previous", "Previous", style = "margin-bottom: 10px;"), actionButton("next", "Next", style = "margin-bottom: 10px;"))
            ),
            fluidRow(
              column(width=12, class = "col-sm-12 col-lg-8", align ="center", 
              imageOutput("image")
              )
            ),

    ),
    
    # First tab content
    tabItem(tabName = "co2",
            fluidRow(
              box(
                width = 12,
                title = 'Carbon Dioxide Dashboard',
                status = 'primary',
                solidHeader = TRUE,
              )
            ),
            fluidRow(
              tabBox(
                id = "tabset_co2",
                width = 12,
                tabPanel("Concentrations",
                         fluidRow(
                           column(6,
                                  infoBoxOutput("progressBox", width="100%")
                           ),
                           column(6,
                                  selectInput(
                                    "timescale",
                                    "Choose a timescale",
                                    choices = c("Current", "2000 years", "800000 years"),
                                    selected = "Current",
                                    width = "100%"
                                  )
                           )
                         ),
                         fluidRow(
                           box(
                             width = 12,
                             plotOutput("conc_plot")
                           )
                         )
                ),
                tabPanel("Production",
                         sidebarLayout(
                           sidebarPanel(
                             radioButtons(
                               "choice",
                               "Select by:",
                               choices = c("Groups", "Countries"),
                               selected = "Groups"
                             ),
                             tags$hr(),
                             # Add a horizontal rule for spacing
                             uiOutput("dropdown")
                           ),
                           # GDP plot for country
                           mainPanel(
                             box(
                               width = 12,
                               plotOutput("CO2Plot")
                             )
                           )
                         )
                ),
                tabPanel("Top Emitters",
                         fluidRow(
                           column(
                             4,
                             infoBox("Total", "51 Gigatons", icon = icon("smog"), width = "100%"),
                           ),
                           column(
                             8,
                             sliderInput("year", "Year:",
                                         min = 1970, max = 2021,
                                         value = 1970, sep = "", step = 1,
                                         animate = animationOptions(interval = 500, loop = FALSE),
                                         width = "100%" 
                             ),
                           )
                         ),
                         fluidRow(
                           box(
                             width = 12,
                             plotOutput("top_emitter_plot")
                           )
                         )
                )
              )
            )),
    
    # Second tab content
    tabItem(tabName = "temperature",
            fluidRow(
              box(
                width = 12,
                title = 'Temperature Dashboard',
                status = 'primary',
                solidHeader = TRUE,
              )
            ),
            fluidRow(
              tabBox(
                id="tabset_temp",
                width=12,
                tabPanel("Global Temperature Anomalies",
                         fluidRow(
                           infoBox("Latest Temperature Anomaly (2023)", "1.17 °C", icon = icon("thermometer-three-quarters"), width=8, subtitle = "Source: NASA"),
                         ),
                         fluidRow(
                           box(
                             width = 12,
                             plotOutput("temp_anomaly_plot")
                           )
                         )
                ),
                tabPanel("Temperature Anomalies by Country",
                         fluidRow(
                           column(
                             6,
                             infoBoxOutput("tempBox", width="100%")
                           ),
                           column(
                             6,
                             selectInput(
                               "temp_countries",
                               "Choose a Country",
                               choices = temp_countries,
                               selected = "World",
                             )
                           )
                         ),
                         fluidRow(
                           box(
                             width = 12,
                             plotOutput("temperature_plot")
                           )
                         )
                )
              )
            )
    ),
    
    # Third tab content
    tabItem(tabName = "oceans",
            fluidRow(
              box(
                width = 12,
                title = 'Sea Levels',
                status = 'primary',
                solidHeader = TRUE,
              )
            ),
            fluidRow(
            tabBox(
                id="tabset_ocean",
                width=12,
                tabPanel("Recent Sea-Levels",
                         fluidRow(
                           column(
                             8,
                             infoBox("Latest Sea Level (2022)", "85 mm", icon = icon("tint"), width=12, subtitle = "Source: NOAA"),
                           ),
                           column(
                             4,
                             selectInput(
                               "ocean",
                               "Choose a Sea",
                               choices = oceans,
                               selected = "World",
                             )
                           )
                         ),
                         fluidRow(
                           box(
                             width = 12,
                             plotOutput("all_sea_levels")
                           )
                         )
                ),
                tabPanel("Historical Sea Levels",
                         # fluidRow(
                         #   infoBox("Latest Sea Level", "100 mm", icon = icon("tint"), width=8, subtitle = "Source: NOAA"),
                         # ),
                         fluidRow(
                           box(
                             width = 12,
                             plotOutput("global_sea_levels")
                           )
                         )
                ),
                tabPanel("Sea Level Trends",
                         fluidRow(
                           infoBox("Global Sea Level Trend", "3.02 mm/year since 1993", icon = icon("tint"), width=8, subtitle = "Source: NOAA"),
                         ),
                         fluidRow(
                           box(
                             width = 12,
                             plotOutput("global_sea_level_trends")
                           )
                         )
                ),
              )
            )
    )
  ))

  
  dashboardPage(header,
                sidebar,
                body)
  
  
