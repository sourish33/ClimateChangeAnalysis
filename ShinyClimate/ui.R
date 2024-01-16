#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

selected_countries = reactive({
  gdp_le |> 
    filter(Continent == input$continent) |>
    pull(Country) |>
    unique() |>
    sort()})

# Define UI for application that draws a histogram
shinyUI(
  navbarPage("CO2 Dashboard",
             tabPanel(
               "CO2 Production",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("country", 
                               label = "Select a country", 
                               choices = countries, 
                               selected = countries[1]),
                   selectInput("category", 
                               label = "Select a category", 
                               choices = categories, 
                               selected = categories[1])
                 ),
                 # GDP plot for country
                 mainPanel(
                   tabsetPanel(
                     tabPanel("GDP", plotOutput("CO2Plot"))
                   )
                   
                 )
               )
             ),
             tabPanel("GDP vs LE", 
                      # fluidRow(
                      #   style = "display: flex; align-items: center; justify-content: center;",
                      #   selectInput("year", 
                      #                 label = "Select a Year", 
                      #                 choices = years, 
                      #                 selected = years[1])
                      # 
                      # ),
                      # fluidRow(
                      #   style = "margin-left: auto; margin-right: auto; max-width: 75%;",
                      #     plotOutput("GDP_vs_LE_plot"),
                      #     plotOutput("Log_GDP_vs_LE_plot")
                      # )
                      
                      ),
  )
)
