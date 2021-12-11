# Web scraping The Queen of Denmark's New Years speeches from kongehuset.dk in english
# Libraries
library(dplyr)
library(rvest)

# Extracting urls of speeches
urls <- read_html("https://www.kongehuset.dk/en/the-monarchy-in-denmark/new-year-address/#read-the-queen-s-new-year-addresses") %>%
  html_nodes(".accordion__container__item__content") %>% 
  html_nodes("p") %>% 
  html_nodes("a") %>% 
  html_attr("href")


# Binding column urls to years kongehuset
last_year <- as.integer(format(Sys.Date(), "%Y")) - 1 # Last year i.e., the year of the lastest speech
kongehuset <- data.frame(year = last_year:2010, urls)[-10,]

# Scraping functions
scrape_kongehuset <- function(url) {
  speech <- read_html(url) %>% 
    html_nodes(css = '.rich-text__container__content p') %>% 
    html_text()
  return(speech)
}

# Scraping speeches
speeches_kongehuset <- lapply(kongehuset$urls, scrape_kongehuset) %>% 
  lapply(paste, collapse = " ") %>%  #collapsing LoL (list of lists)
  unlist()

df <- data.frame(speech = speeches_kongehuset, year = kongehuset$year) %>% 
  write.csv("data/new_year_speeches_eng_2010-2020.csv", row.names = F)

# Saving link and year for english speeches
kongehuset[2:1] %>% 
  arrange(year) %>% 
  saveRDS("data/source_year_eng.rds")
