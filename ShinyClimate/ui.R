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

body <- dashboardBody(tabItems(
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
              id = "tabset1",
              width = 12,
              tabPanel("Concentrations",
                       sidebarLayout(
                         sidebarPanel(selectInput(
                           "timescale",
                           "Choose a timescale",
                           choices = c("Current", "2000 years", "800000 years"),
                           selected = "Current"
                         )),
                         # GDP plot for country
                         mainPanel(tabsetPanel(tabPanel(
                           "Concentrations", plotOutput("conc_plot")
                         )))
                       )),
              tabPanel("Production",
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput(
                             "yr_range",
                             "Period:",
                             min = 1970,
                             max = 2021,
                             value = c(1970, 2021),
                             step = 1,
                             sep = ""
                           ),
                           tags$hr(),
                           # Add a horizontal rule for spacing
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
                         mainPanel(tabsetPanel(tabPanel(
                           "Emissions", plotOutput("CO2Plot")
                         )))
                       )),
               tabPanel("Top Emitters",
                        fluidRow(
                          tabsetPanel(
                            tabPanel("Top 5 Emitters", plotOutput("top_emitter_plot"))
                          )
                        ),
                        fluidRow(
                          column(
                            12, 
                            style="d-flex justify-content-center", 
                                 box(
                                   title = "Controls",
                                   sliderInput("year", "Year:",
                                               min = 1970, max = 2021,
                                               value = 1970, sep = "", step=1,
                                               animate =
                                                 animationOptions(interval = 500, loop = FALSE)),
                                 )
                                 
                                 )


                        ),

               ),
              
            )
          )),
  
  # Second tab content
  tabItem(tabName = "temperature",
          h2("Temperature")),
  
  # Third tab content
  tabItem(tabName = "oceans",
          h2("Oceans"))
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
