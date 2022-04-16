source("scripts/scraping.R")
source("scripts/cleaning.R")
source("scripts/preprocessing.R")
source("scripts/sentiment_analysis.R")
# source("scripts/sentiment_sentences.R")
# source("scripts/stm.R")

detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()

library(shiny) # For interactive interface
library(shinydashboard) # For dasghboard design and structure - enables tabs and various html elements
library(plyr) # For data manipulation
library(dplyr) # For data manipulation
library(tidyr) # For data manipulation
library(DT) # For table visualizations
library(plotly) # for interactive plots
library(LDAvis) # For topic models
library(stm) # for stm models
library(highcharter) # for plot display
library(leaflet)
library(stringr)
library(wordcloud2) # Two create wordclouds
library(colorBlindness) # For colors

# Load data ---------------------------------------------------------------
tokens <- readRDS("data/tokens.rds") # All tokens, filtered
lda_model <- readRDS("data/lda_model.rds") # LDA model
stm_model_da <- readRDS("data/stm_model.rds") # STM model
thoughts <- readRDS("data/thoughts.rds") # sentences belonging to topics (topic proportion 45%)
# lemma <- readRDS("data/lemma.rds") # All lematized values unfiltered
sentiment <- readRDS("data/sentiments.rds") # Sentiment for year
# countries <- readRDS("data/country_speech.rds") # Countries, country code, lat, lon and # mentions
# geojson <- readRDS("data/countries.rds") # Library containing geographic information
article_lib <- readRDS("data/article_library.rds") # File containing UUID, Article name


# Formatting data ---------------------------------------------------------
## Words ----
n_dist_t_headword <- nrow(distinct(tokens, stemmed)) # Number of distinct headwords
words_all <-  unique(tokens$word) %>% sort()
words_tokens_all <- tokens %>%
  mutate(wordisnum = as.integer(suppressWarnings(ifelse(!is.na(as.numeric(stemmed)),1,0)))) %>% 
  arrange(wordisnum, stemmed) %>% 
  .$stemmed %>% 
  unique()
words_count_unique <- length(words_all)
most_common <- max(tokens$n_stem_total)
most_common_any_year <- max(tokens$n_stem)
number_of_rarity <- length(unique(arrange(tokens, desc(n_stem_total))$n_stem_total))
named_id <- article_lib$uuid
names(named_id) <- article_lib$title
topic_frame <- data.frame(topic = names(thoughts$index)) %>% 
  mutate(uuid = thoughts$uuid[topic],
         docs = thoughts$docs[topic]) %>% 
  rowwise() %>% 
  mutate(doc_len = length(docs))
n_unique_sentences <- sum(topic_frame$doc_len)
sections <- article_lib$section %>% 
  unique() %>% 
  sort()
authors <- article_lib$authors %>% 
  str_split(", ") %>% 
  unlist() %>% 
  unique() %>% 
  .[. != ""] %>% 
  sort()
authors <-  c("Not set", authors)
locations <- article_lib$location %>% 
  unique() %>% 
  sort()


# Colors ------------------------------------------------------------------
## For multiple series points/continuous ----
col_multi <- c(
  # colorBlindness::paletteMartin[2:(length(colorBlindness::paletteMartin))], # Good palette, but black for the first value is not good.
  rev(Blue2DarkRed18Steps)[1:7],
  rev(Blue2DarkRed18Steps)[11:length(Blue2DarkRed18Steps)],
  Blue2DarkOrange18Steps[2:length(Blue2DarkOrange18Steps)] # It's fine. Most importantly, none of the colors are redundant, though some are similar
) %>% 
  as.character

## Two types ----
col_single <- c( # Based on tv2fyn colors
  "#d21e1e" # red
)

col_dual <- c( # Based on tv2fyn colors
  "#162c40", #  blue
  "#d21e1e" # red
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

# For comparing 2, sum, and something else ----
col_five_sum <- c( # Based on colorBlindness::paletteMartin
  "#662700", # Red brown
  "#DB6D00", # Orange - representing sum (red and green combined)
  "#A50021", # Dark red - representing false
  "#F72735", # Midpoint of red and orrange - representing range? Yeah let's go with that
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

hc_fivecolsum <- function(x){
  x %>% hc_colors(col_five_sum)
}

# Sorting ----
cmatch <- function(needle, haistack){ # faster solution
  ifelse(length(needle[needle %in% haistack]) > 0, T, F)
}

# Map ----------------------------------------------------------------------
# Denmark -----------------------------------------------------------------
# geofile_denmark = "utils/geofile_denmark" # Too slow
geofile_denmark = "utils/denmark.geo.json"
geodata_denmark <- rgdal::readOGR(geofile_denmark, use_iconv = TRUE, encoding = "UTF-8") # Read geojson contents

# Southern Denmark ---------------------------------------------------------
geofile_southjutland = "utils/geofile_southjutland"
geodata_southjutland <- rgdal::readOGR(geofile_southjutland, use_iconv = TRUE, encoding = "UTF-8") # Read geojson contents

# Fyn ----------------------------------------------------------------------
geofile_fyn = "utils/geofile_fyn"
geodata_fyn <- rgdal::readOGR(geofile_fyn, use_iconv = TRUE, encoding = "UTF-8") # Read geojson contents

# Komunes ------------------------------------------------------------------
geofile_komunes = "utils/geofile_komunes"
geodata_komunes <- rgdal::readOGR(geofile_komunes, use_iconv = TRUE, encoding = "UTF-8") # Read geojson contents

# geodata_frame <- geodata@data %>% as_tibble()  # For observing data


# Text ---------------------------------------------------------------------
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
