library(shiny)
library(tidyverse)
co2_df <- read_csv('data/co2_cleaned.csv')

countries <- co2_df |>
  pull(Country) |>
  unique() |>
  sort()
# 
# continents <- gdp_le |>
#   pull(Continent) |>
#   unique() |>
#   sort()
# 
# years <- gdp_le |>
#   pull(Year) |>
#   unique() |>
#   sort()
