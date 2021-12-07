library(shiny)
library(dplyr) # for code manipulation, data tables, data manipulation, dependencies and a lot more
library(DT) # For table visualizations
# library(highcharter) # for interactive plots
library(plotly) # for interactive plots
#library(r2d3) # for advanced interactive plots
library(LDAvis)
library(stm)

# Load data ---------------------------------------------------------------
# Todo

## Tokens -----------------------------------------------------------------
tokens <- readRDS("data/tokens.rds")
lda_model <- readRDS("data/lda_model.rds")
stm_model <- readRDS("data/stm_model.rds")
