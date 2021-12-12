library(shiny)
library(shinydashboard)
library(plyr) # For data manipulation
library(dplyr) # For data manipulation
# library(DT) # For table visualizations
# library(highcharter) # for interactive plots
library(plotly) # for interactive plots
#library(r2d3) # for advanced interactive plots
library(LDAvis) # For topic models
library(stm) # for stm models
library(highcharter) # for plot display
library(leaflet)
library(stringr)
# library(RColorBrewer) # To color worldclouds. Requirement of wordlclouds
# library(wordcloud) # To create wordclouds
library(wordcloud2) # Two create wordclouds

# Load data ---------------------------------------------------------------
tokens <- readRDS("data/tokens.rds") # All tokens, filtered
lda_model <- readRDS("data/lda_model.rds")
stm_model <- readRDS("data/stm_model.rds")
lemma <- readRDS("data/lemma.rds") # All lematized values unfiltered
sentiment <- readRDS("data/sentiments.rds") # Sentiment for year
countries <- readRDS("data/country_speech.rds") # Countries, country code, lat, lon and # mentions
source_year <- readRDS("data/source_year.rds") # Sources and year of source
source_year_en <- readRDS("data/source_year_eng.rds") # Sources and year of source
geojson <- readRDS("data/countries.rds") # Library containing geographic information

# Formatting data ---------------------------------------------------------
# Number of distinct headwords
n_dist_t_headword <- nrow(distinct(tokens, headword))

## Years ----
years <- unique(source_year$year) %>% sort()
year_min <-  min(sentiment$year)
year_max <-  max(sentiment$year)
year_span <-  year_max-year_min+1
years_en <- unique(source_year_en$year) %>% sort()
year_min_en <-  min(source_year_en$year)
year_max_en <-  max(source_year_en$year)
year_span_en <-  year_max_en-year_min_en+1

## Languages ----
languages <- c("DK", "EN")

## Words ----
words_all <-  unique(lemma$token) %>% sort()
words_tokens_all <- unique(tokens$headword) %>% sort()
words_count_unique <- length(words_all)
most_common <- max(tokens$n_hword_total)
most_common_any_year <- max(tokens$n_hword_year)
number_of_rarity <- length(unique(arrange(tokens, desc(n_hword_total))$n_hword_total))

# Highchart options -------------------------------------------------------
opts <- getOption("highcharter.options")
opts$lang$decimalPoint <- "."
options(highcharter.options = opts)

## Highchart sorting function ----
hc_norevese <- function(x){
  x %>% 
  hc_chart(
    inverted = F
  ) %>% 
  hc_xAxis(
    reversed = F
  ) %>% 
  hc_legend(
    reversed = F
  )
}

# Sorting ----
cmatch <- function(left, right){
  for(item in left){
    if(item %in% right){
      return(T)
    }
    
  }
  return(F)
  
}

# Map ----------------------------------------------------------------------

## Function for sorting geojson ----
poly_prep <- function(polygons, countries, years) {
  countries <- countries %>% 
    dplyr::filter(year %in% years) %>% 
    dplyr::select(year, n, code, countries) %>% 
    dplyr::group_by(code) %>% 
    dplyr::summarise(
      countries = str_to_title(unique(countries)),
      n = sum(n)
    )
  countries
  poly <- subset(polygons, ISO_A2 %in% unique(countries$code))
  poly@data <- poly@data %>%
    dplyr::left_join(countries, by = c("ISO_A2" = "code"))
  poly
}

pal <- colorNumeric(c("#a3dcff", "#007bff"), NULL)

