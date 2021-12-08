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


# Load data ---------------------------------------------------------------
tokens <- readRDS("data/tokens.rds") # All tokens, filtered
lda_model <- readRDS("data/lda_model.rds")
stm_model <- readRDS("data/stm_model.rds")
lemma <- readRDS("data/lemma.rds") # All lematized values unfiltered
sentiment <- readRDS("data/sentiments.rds") # Sentiment for year

## Formatting data --------------------------------------------------------
# Number of distinct headwords
n_dist_t_headword <- nrow(distinct(tokens, headword))
