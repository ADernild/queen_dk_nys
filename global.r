library(shiny) # For interactive interface
library(shinydashboard) # For dasghboard design and structure - enables tabs and various html elements

# Data handling -----------------------------------------------------------
## Update data ------------------------------------------------------------
source("scripts/update_core_data.R")


## Load data  -------------------------------------------------------------
source("scripts/load_article_data.R") # Article data
source("scripts/load_map_data.R") # Map data
