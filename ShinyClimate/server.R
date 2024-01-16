library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  # Define the choices based on radio button selection
  choices <- reactive({
    if (input$choice == "Countries") {
      return(countries)
    } else {
      return(categories)
    }
  })
  
  ppm_data <- reactive({
    if (input$timescale == "Current"){
      return(ppm_df)
    }
    if (input$timescale == "2000 years"){
      return(ppm_df_2k)
    }
    return(ppm_df_800k)
  })
  
  
  output$dropdown <- renderUI({
    selectInput("selection", "Select a country/group:", choices = choices())
  })
  
  output$CO2Plot <- renderPlot({
    country = input$selection
    y1 = input$yr_range[1]
    y2= input$yr_range[2]
    years <- as.numeric(substring(names(co2_df)[7:ncol(co2_df)], 2))
    row1 <- co2_df |> filter(Country == country & CTS_Code == "ECNGDE" & Gas_Type == "Greenhouse gas")
    ghg_values <- as.numeric(row1[1, 7:ncol(co2_df)])/1000
    row2 <- co2_df |> filter(Country == country & Industry == "Energy" & Gas_Type == "Carbon dioxide")
    co2_values <- as.numeric(row2[1, 7:ncol(co2_df)])/1000
    
    # Create data frames for each type
    ghg_data <- data.frame(years = years, values = ghg_values, type = rep("Total", length(ghg_values)))
    co2_data <- data.frame(years = years, values = co2_values, type = rep("CO2", length(co2_values)))
    
    # Combine data for plotting
    plot_data <- rbind(ghg_data, co2_data)
    
    # Plotting
    plot_data |>
      filter(years >= y1 & years <= y2) |>
      ggplot(aes(x = years, y = values, color = type)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(title = paste("Greenhouse Gas Emissions from 1970 to 2021 for", country),
           x = "Years",
           y = "Gigatons of CO2 equivalent") +
      theme_minimal()
  })
  
  output$conc_plot <- renderPlot({
    ppm_data() |>
      ggplot(aes(x = Numeric_date, y = Value)) +
      geom_line() +
      geom_point(size = 0.5) +
      labs(title = paste("Atmospheric CO2 concentrations"),
           x = "Years",
           y = "CO2 concentration (ppm)") +
      theme_minimal()
  })
  

}
