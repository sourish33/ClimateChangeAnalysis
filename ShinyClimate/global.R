library(shiny)
library(tidyverse)
co2_df <- read_csv('data/co2_cleaned.csv')

countries <- co2_df |>
  filter(ISO2 != 'ZZ') |>
  pull(Country) |>
  unique() |>
  sort()

categories <-co2_df |>
  filter(ISO2 == 'ZZ') |>
  pull(Country) |>
  unique() |>
  sort()

ppm_df <- read_csv('data/data_modern_ppm.csv')
ppm_df_2k <- read_csv('data/data_2000.csv')
ppm_df_800k <- read_csv('data/data_800k.csv')

temps <- read_csv('../ShinyClimate/data/temperature.csv')
anomalies <- read_csv('../ShinyClimate/data/LandOceanTemperatureIndex.csv')
temp_countries <- temps |> colnames()
temp_countries <- temp_countries[-1]

sealevels <- read_csv('../ShinyClimate/data/Change_in_Mean_Sea_Levels.csv')
oceans <- sealevels|>
  pull(Ocean)  |>
  unique() |>
  sort()
