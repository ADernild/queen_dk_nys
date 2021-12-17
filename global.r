library(shiny)
library(shinydashboard)
library(plyr) # For data manipulation
library(dplyr) # For data manipulation
library(tidyr) # For data manipulation
library(DT) # For table visualizations
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
library(colorBlindness) # For colors
# library(cartography) # For colors

# Load data ---------------------------------------------------------------
tokens_dk <- readRDS("data/tokens.rds") # All tokens, filtered
tokens_en <- readRDS("data/tokens_en.rds") # All tokens, filtered
lda_model <- readRDS("data/lda_model.rds") # LDA model
stm_model_da <- readRDS("data/stm_model.rds") # STM model
stm_model_en <- readRDS("data/stm_model_en.rds") # STM model
thoughts_da <- readRDS("data/thoughts.rds") # sentences belonging to topics (topic proportion 45%)
thoughts_en <- readRDS("data/thoughts_en.rds") # sentences belonging to topics (topic proportion 45%)
lemma_dk <- readRDS("data/lemma.rds") # All lematized values unfiltered
lemma_en <- readRDS("data/lemma.rds") # All lematized values unfiltered
sentiment <- readRDS("data/sentiments.rds") # Sentiment for year
sentiment_en <- readRDS("data/sentiments_en.rds") # Sentiment for year
countries <- readRDS("data/country_speech.rds") # Countries, country code, lat, lon and # mentions
countries_en <- readRDS("data/country_speech_eng.rds") # Countries, country code, lat, lon and # mentions
source_year <- readRDS("data/source_year.rds") # Sources and year of source
source_year_en <- readRDS("data/source_year_eng.rds") # Sources and year of source
geojson <- readRDS("data/countries.rds") # Library containing geographic information

# Formatting data ---------------------------------------------------------
tokens <- tokens_dk
lemma <- lemma_dk

# Number of distinct headwords
n_dist_t_headword <- nrow(distinct(tokens, stemmed))

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
words_tokens_all <- tokens %>%
  mutate(wordisnum = as.integer(suppressWarnings(ifelse(!is.na(as.numeric(stemmed)),1,0)))) %>% 
  arrange(wordisnum, stemmed) %>% 
  .$stemmed %>% 
  unique()
words_count_unique <- length(words_all)
most_common <- max(tokens$n_stem_total)
most_common_any_year <- max(tokens$n_stem_year)
number_of_rarity <- length(unique(arrange(tokens, desc(n_stem_total))$n_stem_total))


# Colors ------------------------------------------------------------------
# Chosen with https://coolors.co/ using refference picture:
# https://upload.wikimedia.org/wikipedia/commons/thumb/a/a2/Drottning_Margrethe_av_Danmark_crop.jpg/210px-Drottning_Margrethe_av_Danmark_crop.jpg
# Note: using our own colorpalette is cool and stuff, but i have not optimized it greatly for colorblindness, so using a standard scheme might be better.
colors_of_the_queen <- c(
  "#4D5749", # Falling royal star of the queen / Ebony
  "#BB3F56", # Her majesty lipstick / English red
  "#BACFF6", # Her majesty earing  / Light blue steel
  "#ECC1BE", # Pink queen / Baby pink
  "#E3BF9D", # Rising royal star of the queen / Desert sand
  "#9C5C52", # Royal dimple / redwood
  "#435961", # Royal sweather blue / Deep space sparkle
  "#9F9080", # Royal hair / Grullo
  "#321403", # Royal brown button / Black bean
  "#C0BFBB" # Hair of the majestic / Gray X 11 gray
)

## For multiple series points/continuous ----
col_multi <- c(
  # colorBlindness::paletteMartin[2:(length(colorBlindness::paletteMartin))], # Good palette, but black for the first value is not good.
  rev(Blue2DarkRed18Steps)[1:7],
  rev(Blue2DarkRed18Steps)[11:length(Blue2DarkRed18Steps)],
  Blue2DarkOrange18Steps[2:length(Blue2DarkOrange18Steps)] # It's fine. Most importantly, none of the colors are redundant, though some are similar
) %>% 
  as.character

## Two types ----
col_dual <- c( # Based on colorBlindness::paletteMartin
  "#00CCCC", # Light blue
  "#A50021" # red
)

## For 2 or 4 series ----
col_tri_pos <- c( # Based on colorBlindness::paletteMartin
  "#A50021", # Dark red - representing false
  "#00CCCC", # dark blue - representing true
  "#65FFFF" # Light blue - representing true again
)

col_tri_neg <- c( # Based on colorBlindness::paletteMartin
  "#A50021", # Dark red - representing false
  "#FF7856", # Orange - representing false again
  "#00CCCC" # dark blue - representing true
)

col_quad <- c( # Based on colorBlindness::paletteMartin
  "#00CCCC", # dark blue - representing true
  "#A50021", # Dark red - representing false
  "#65FFFF", # Light blue - representing true again
  "#FF7856" # Orange - representing false again
)

# For comparing 2, sum, and something else ----
col_quad_sum <- c( # Based on colorBlindness::paletteMartin
  "#00CCCC", # dark blue - representing true
  "#A50021", # Dark red - representing false
  "#DB6D00", # Orange - representing sum (red and green combined)
  "#662700" # Brown - representing something unrelated to the others
)

## For levels ----
col_red_gradient <- c(
  "#A50021",
  "#ffd1c1"
)

col_blu_gradient <- c(
  "#00CCCC",
  "#dec7ff"
)

# Highchart options -------------------------------------------------------
opts <- getOption("highcharter.options")
opts$lang$decimalPoint <- "."
options(highcharter.options = opts)

## Highcharts functions ----
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

hc_queencol <- function(x){
  x %>% hc_colors(colors_of_the_queen)
}

hc_multicol <- function(x){
  x %>% hc_colors(col_multi)
}

hc_dualcol <- function(x){
  x %>% hc_colors(col_dual)
}

hc_dualcol_rev <- function(x){
  x %>% hc_colors(rev(col_dual))
}

hc_tricol_pos <- function(x){
  x %>% hc_colors(col_tri_pos)
}

hc_tricol_neg <- function(x){
  x %>% hc_colors(col_tri_neg)
}

hc_quadcol <- function(x){
  x %>% hc_colors(col_quad)
}

hc_quadcol_custom <- function(x){
  x %>% hc_colors(c(
    "#A50021", # Dark red - representing false
    "#FF7856", # Orange - representing false again
    "#00CCCC", # dark blue - representing true
    "#65FFFF" # Light blue - representing true again
  ))
}

hc_quadcolsum <- function(x){
  x %>% hc_colors(col_quad_sum)
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
    dplyr::select(year, n, code, countries, sentiment, sentence_full) %>% 
    dplyr::group_by(code) %>% 
    dplyr::summarise(
      countries = str_to_title(unique(countries)),
      year = list(year),
      n_year = list(n),
      sentence = list(sentence_full),
      sentiment_year = list(sentiment),
      sentiment = mean(sentiment),
      n = sum(n)
    )
  countries
  poly <- subset(polygons, ISO_A2 %in% unique(countries$code))
  poly@data <- poly@data %>%
    dplyr::left_join(countries, by = c("ISO_A2" = "code"))
  poly
}

pal <- colorNumeric(rev(col_red_gradient), NULL)


# Text --------------------------------------------------------------------
whatViz <- function(text){
  p(class="whatViz",
    span("\u25CF", title="What"),
    text
  )
}
whyViz <- function(text){
  p(class="whyViz",
    span("\u25A0", title="Why"),
    text
  )
}
howViz <- function(text){
  p(class="howViz", title="How",
    span("\u25B2"),
    text
  )
}
