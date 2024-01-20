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

selected_countries = reactive({
  gdp_le |>
    filter(Continent == input$continent) |>
    pull(Country) |>
    unique() |>
    sort()
})

header <- dashboardHeader(title = "Climate Change Dashboard",
                          titleWidth = 350)

# sidebar <-dashboardSidebar(
#   sidebarMenu(
#     menuItem("CO2", tabName = "co2", icon = icon("dashboard")),
#     menuItem("Temperature", tabName = "temperature", icon = icon("dashboard")),
#     menuItem("Oceans", tabName = "oceans", icon = icon("dashboard")),
#   )
# )
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("CO2", tabName = "co2", icon = icon("dashboard")),
  menuItem(
    "Temperature",
    tabName = "temperature",
    icon = icon("dashboard")
  ),
  menuItem("Oceans", tabName = "oceans", icon = icon("dashboard"))
))

body <- dashboardBody(
  ### changing theme
  shinyDashboardThemes(
    # theme = "blue_gradient"
    # theme = "poor_mans_flatly"
    # theme = "flat_red"
    # theme = "grey_light"
    # theme = "onenote"
    theme = "purple_gradient"
  ),
  tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
  tabItems(
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
                         )),
                tabPanel("Top Emitters",
                         fluidRow(
                           column(
                             4,
                             infoBox("Total", "51 Gigatons", icon = icon("industry"), width = "100%"),
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
                         
                ),
                
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
                tabPanel("Global Temperature Anomaly",
                         fluidRow(
                           infoBox("Average Anomaly", "0.89 C | 1.6 F", icon = icon("industry"), width = "60%"),
                         ),
                         fluidRow(
                           box(
                             width = 12,
                             plotOutput("temperature_plot")
                           )
                         )
                ),
                
                
              )
            )
            
    ),
    
    # Third tab content
    tabItem(tabName = "oceans",
            box(
              width = 12,
              title = 'Sea level Dashboard',
              status = 'primary',
              solidHeader = TRUE,
            )
    )
  ))

dashboardPage(header,
              sidebar,
              body)


# # Define UI for application that draws a histogram
# shinyUI(
#   navbarPage("Greenhouse Gas Dashboard",
#              tabPanel(
#                "Production",
#                sidebarLayout(
#                  sidebarPanel(
#                    sliderInput("yr_range", "Period:",
#                                min = 1970, max = 2021, value = c(1970, 2021),
#                                step = 1, sep = ""),
#                    tags$hr(),  # Add a horizontal rule for spacing
#                    radioButtons("choice", "Select by:", choices = c("Groups", "Countries")),
#                    tags$hr(),  # Add a horizontal rule for spacing
#                    uiOutput("dropdown")
#                  ),
#                  # GDP plot for country
#                  mainPanel(
#                    tabsetPanel(
#                      tabPanel("Emissions", plotOutput("CO2Plot"))
#                    )
#
#                  )
#                )
#              ),
#              tabPanel("Top Emitters",
#                       sidebarLayout(
#                         sidebarPanel(
#                           sliderInput("year", "Year:",
#                                       min = 1970, max = 2021,
#                                       value = 1970, sep = "", step=1,
#                                       animate =
#                                         animationOptions(interval = 500, loop = FALSE)),
#                         ),
#                         mainPanel(
#                           tabsetPanel(
#                             tabPanel("Top 5 Emitters", plotOutput("top_emitter_plot"))
#                           )
#                         )
#                       )
#
#              ),
#              tabPanel("CO2 Concentration",
#                       sidebarLayout(
#                         sidebarPanel(
#                           selectInput("timescale", "Choose a timescale", choices = c("Current", "2000 years", "800000 years"))
#                         ),
#                         # GDP plot for country
#                         mainPanel(
#                           tabsetPanel(
#                             tabPanel("Concentrations", plotOutput("conc_plot"))
#                           )
#                         )
#                       )
#
#                       ),
#   )
# )
