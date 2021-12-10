library(shiny)
library(dplyr) # for code manipulation, data tables, data manipulation, dependencies and a lot more
library(DT) # For table visualizations
# library(highcharter) # for interactive plots
library(plotly) # for interactive plots
#library(r2d3) # for advanced interactive plots
library(LDAvis)
library(stm) # for stm models
library(highcharter) # for table display
library(shinydashboard)
library(plyr)


# Load data ---------------------------------------------------------------
tokens <- readRDS("data/tokens.rds") # All tokens, filtered
lda_model <- readRDS("data/lda_model.rds")
stm_model <- readRDS("data/stm_model.rds")
lemma <- readRDS("data/lemma.rds") # All lematized values unfiltered
sentiment <- readRDS("data/sentiments.rds") # Sentiment for year

# Formatting data ---------------------------------------------------------
# Number of distinct headwords
n_dist_t_headword <- nrow(distinct(tokens, headword))

## Years ----
year_min <-  min(sentiment$year)
year_max <-  max(sentiment$year)
year_span <-  year_max-year_min+1

## Languages ----
languages <- c("Danish", "English")

## Words ----
words_all <-  unique(lemma$token) %>% sort()
words_tokens_all <- unique(tokens$headword) %>% sort()
words_count_unique <- length(words_all)


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
    reversed = T
  )
}
