library(shiny) # For interactive interface
library(shinydashboard) # For dasghboard design and structure - enables tabs and various html elements

# Data handling -----------------------------------------------------------
## Update data ------------------------------------------------------------
# source("scripts/update_core_data.R") # Execution halts when this is run in docker image for shiny server


## Load data  -------------------------------------------------------------
source("scripts/load_article_data.R") # Article data
source("scripts/load_map_data.R") # Map data
