# Update  all data ----------------------------------------------------------
# Scrapes content, updates libarary, creates tokens and token sentiment,
# sentiment for sentences and articles, and runs stm analysis
source("scripts/detach_packages.R")
source("scripts/scraping.R")
source("scripts/cleaning.R")
source("scripts/preprocessing.R")
source("scripts/sentiment_analysis.R")
source("scripts/sentiment_sentences.R")
source("scripts/stm.R")
