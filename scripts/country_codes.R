library(dplyr)
library(rvest)


url <- "https://da.wikipedia.org/wiki/ISO_3166-1"

tbls <- url %>% 
  read_html() %>%
  html_nodes("table")

ISO_codes <- html_table(tbls[1][[1]])

geojson <- "https://raw.githubusercontent.com/datasets/geo-countries/master/data/countries.geojson"
csv <- "https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv"

