library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  # Define the choices based on radio button selection
  choices <- reactive({
    if (input$choice == "Countries") {
      return(list(options = countries, text = "Select a Country", selected = "United States"))
    } else {
      return(list(options = categories, text = "Select a Group", selected = "World"))
    }
  })
  
  ppm_data <- reactive({
    if (input$timescale == "Current"){
      return(list(data = ppm_df, title_fragment = "from 1958-present", xlabel = "Year"))
    }
    if (input$timescale == "2000 years"){
      return(list(data = ppm_df_2k, title_fragment = "over 2000 years", xlabel = "Year"))
    }
    return(list(data = ppm_df_800k, title_fragment = "over 800K years", xlabel = "Thousands of years before today"))
  })
  
  
  output$dropdown <- renderUI({
    options = choices()$options
    text = choices()$text
    selected = choices()$selected
    selectInput("selection", text, choices = options, selected = selected)
  })
  
  output$CO2Plot <- renderPlot({
    country = input$selection
    y1 = 1970
    y2= 2021
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
    df <- ppm_data()$data
    title_fragment <-ppm_data()$title_fragment
    xlabel <- ppm_data()$xlabel
    df |>
      ggplot(aes(x = Numeric_date, y = Value)) +
      geom_line() +
      geom_point(size = 0.5) +
      labs(title = paste("Atmospheric CO2 concentrations", title_fragment),
           x = xlabel,
           y = "CO2 concentration (ppm)") +
      theme_minimal()
  })
  
  output$top_emitter_plot <- renderPlot({
    yr <- paste("F",as.character(input$year), sep="")
    co2_df |> 
      filter(ISO2 != "ZZ" & CTS_Code == "ECNGDE" & Gas_Type == "Greenhouse gas") |>
      arrange(desc(!!sym(yr))) |>  
      slice(1:10) |>
      ggplot(aes(x = reorder(Country, desc(!!sym(yr))), y = !!sym(yr)/1000)) +  
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = paste("Top 10 Greenhouse Gas Emitters in", input$year),
           x = "Country",
           y = "Gigatons of CO2 equivalent") +
      scale_y_continuous(limits = c(0, 15.2)) + 
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, face = "bold"),  # X-axis labels bold
            axis.text.y = element_text(size = 10, face = "bold"))  # Y-axis labels bold
    
  })
  

}
