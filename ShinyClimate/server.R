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
      theme_classic()
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
      theme_classic()
  })
  
  output$top_emitter_plot <- renderPlot({
    yr <- paste("F",as.character(input$year), sep="")
    total_emissions <- co2_df |> filter(Country == "World" & CTS_Code == "ECNGDE" & Gas_Type == "Greenhouse gas") |> pull(!!sym(yr))/1000
    total_emissions <- round(total_emissions)
    co2_df |> 
      drop_na()|>
      filter(ISO2 != "ZZ" & CTS_Code == "ECNGDE" & Gas_Type == "Greenhouse gas") |>
      arrange(desc(!!sym(yr))) |>  
      slice(1:10) |>
      ggplot(aes(x = reorder(Country, desc(!!sym(yr))), y = !!sym(yr)/1000)) +  
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = paste("Top 10 Greenhouse Gas Emitters in", input$year),
           x = "Country",
           y = "Gigatons of CO2 equivalent") +
      scale_y_continuous(limits = c(0, 15.2)) + 
      theme_classic() +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, face = "bold"),  # X-axis labels bold
            axis.text.y = element_text(size = 10, face = "bold")) + # Y-axis labels bold
      annotate("text", x = Inf, y = Inf, label = paste("World Total Emissions in", input$year, ":", total_emissions, "Gigatons"),
               hjust = 1, vjust = 1, size = 6) 
    
  })
  
  output$progressBox <- renderInfoBox({
    infoBox(
      "Current CO2 Concentration", paste("422 ppm"), icon = icon("cloud"),
      fill = TRUE,
      subtitle = "Source: NASA"
    )
  })
  

  
  output$temp_anomaly_plot <- renderPlot({
    anomalies |> ggplot(aes(x = Year)) +
      geom_line(aes(y = No_smoothing, color = "Annual Mean"), linetype = "solid") +
      geom_point(aes(y = No_smoothing, color = "Annual Mean"), size = 2) +
      geom_line(aes(y = Lowess, color = "Smoothed (Lowess)"), linetype = "solid") +
      labs(title = "Global Land-Ocean Temperature Index", x = "Year", y = "Values") +
      scale_color_manual(values = c("Annual Mean" = "blue", "Smoothed (Lowess)" = "red")) +
      theme_classic() +
      theme(
        legend.position = c(0.2, 0.9),  
        legend.background = element_rect(color = "black", fill = "white"),
        legend.title= element_blank()
      )
  }
  )
  
  
  output$tempBox <- renderInfoBox({
    country <- input$temp_countries
    curtemp <- temps[temps$Year == 2021, country]
    curtemp <- round(curtemp, digits=2)
    
    if (!is.na(curtemp) && length(curtemp) > 0) {
      infoBox("Above Baseline (2021)", paste(curtemp, "°C"), icon = icon("thermometer-three-quarters"))
    } else {
      infoBox("Above Baseline", "Data not available", icon = icon("thermometer-three-quarters"))
    }
  })
  
  
  output$temperature_plot <- renderPlot({
    country <- input$temp_countries
    temps |>
      ggplot(aes(x = Year, y = !!sym(country), fill = !!sym(country))) +
      geom_bar(stat = "identity") +
      scale_fill_gradient(low = "blue", high = "red") +  # Adjustinh the color scale
      labs(x = "Year", y = "Value", title = paste("Change in °C from 1951-1980 baseline: ", country)) +
      theme_classic()
  })
  
  
  output$all_sea_levels <- renderPlot({
    
    sealevels <- read_csv('../ShinyClimate/data/Change_in_Mean_Sea_Levels.csv')
    which_sea <- input$ocean
    
    colors <- c('TOPEX' = 'blue',
                'Jason.1' = 'green',
                'Jason.2' = 'red',
                'Jason.3' = 'purple')
    
    sealevels |> 
      filter(Ocean == which_sea) |>
      ggplot(aes(x = Decimal_Date, y = Value, color = Satellite)) +
      geom_line() +
      geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed', color = 'black') +  # Add trend line
      labs(title = paste('Change in Mean Sea-level:',which_sea) , x = 'Date', y = 'Change in mean sea-level (mm)') +
      scale_color_manual(values = colors) +
      scale_x_continuous(breaks = seq(1992, 2023, by = 4))
    
  })

  
  
  
  
}
