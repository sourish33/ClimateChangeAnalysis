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

