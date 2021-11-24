# Web scraping The Queen of Denmark's New Years speeches from kongehuset.dk

# Libraries
library(tidyverse)
library(rvest)

# Extracting urls of speeches
urls <- read_html("https://www.kongehuset.dk/monarkiet-i-danmark/nytarstaler/hendes-majestat-dronningens-nytarstaler") %>%
  html_nodes(".field-name-field-section-links") %>%
  html_nodes(".field-items") %>%
  html_nodes("a") %>%
  html_attr("href")

# Binding column years to speeches and saving as csv
speech_year_url <- cbind(year = 2020:2001, urls) %>% 
  as.data.frame()

# Scraping function
scrape <- function(link) {
  speech <- read_html(link) %>% 
  html_nodes(css = '.pad, .field-item.even p') %>% 
    html_text()
  return(speech)
}

# Scraping speeches
speeches <- lapply(speech_year_url$urls, scrape) %>% 
  lapply(paste, collapse = "\n") #collapsing LoL (list of lists)

# Coercing to data.frame and adding year variable
df <- do.call(rbind, speeches) %>% 
  data.frame(year = speech_year_url$year) %>% 
  write.csv("data/new_year_speeches_2001-2020.csv") #saving as csv
