# Web scraping The Queen of Denmark's New Years speeches from kongehuset.dk and dansketaler.dk
# Libraries
library(dplyr)
library(rvest)
library(purrr)

# Extracting urls of speeches
urls <- read_html("https://www.kongehuset.dk/monarkiet-i-danmark/nytarstaler/#laes-de-seneste-nytaarstaler") %>%
  html_nodes(".accordion__container__item__content") %>% 
  html_nodes(".field-item") %>% 
  html_nodes("a") %>% 
  html_attr("href")

# Converting local urls to global urls
local_urls <- is.na(str_match(urls, "^https://www.kongehuset.dk")) # Checking if url is a local url
urls[local_urls] <- lapply(urls[local_urls], function(x) paste0("https://www.kongehuset.dk", x)) %>%  # making local urls global
  unlist()

kongehuset_urls <- urls

# Binding column urls to years kongehuset
last_year <- as.integer(format(Sys.Date(), "%Y")) - 1 # Last year i.e., the year of the lastest speech
kongehuset <- data.frame(year = last_year:2001, urls)

# Making urls for speeches on dansketaler.dk
urls <- lapply(2000:1972, function(x) paste0("https://dansketaler.dk/tale/dronningens-nytaarstale-", x,"/print/")) %>% 
  unlist()

# Binding column urls to years dansketaler.dk
dansketaler <- data.frame(year = 2000:1972, urls)

# Fixing years that doesn't follow normal link structure
dansketaler$urls[25] <- "https://dansketaler.dk/tale/h-m-dronning-margrethe-iis-nytaarstale-1976/print/"
dansketaler$urls[22] <- "https://dansketaler.dk/tale/christian-9-palae-amalienborg/print/"


# Scraping functions
scrape_kongehuset <- function(url) {
  speech <- read_html(url) %>% 
  html_nodes(css = '.rich-text__container__content p') %>% 
    html_text()
  return(speech)
}

scrape_dansketaler <- function(url) {
  speech <- read_html(url) %>% 
    html_nodes(css='.entry-content p') %>% 
    html_text()
  return(speech)
}

# Scraping speeches
speeches_kongehuset <- lapply(kongehuset$urls, scrape_kongehuset) %>% 
  lapply(paste, collapse = " ") %>%  #collapsing LoL (list of lists)
  unlist()

speeches_dansketaler <- lapply(dansketaler$urls, scrape_dansketaler) %>% 
  lapply(paste, collapse = " ") %>% 
  unlist()

# Coercing to data.frame and adding year variable
df <- data.frame(speech = c(speeches_kongehuset, speeches_dansketaler), year = c(kongehuset$year, dansketaler$year)) %>% 
  write.csv("data/new_year_speeches_1972-2020.csv", row.names = F, fileEncoding = "UTF-8") #saving as csv

# Saving meta file with year and url
source_year <- data.frame(Source = c(kongehuset_urls, urls), year = c(kongehuset$year, dansketaler$year)) %>% 
  map_df(rev)
saveRDS(source_year, "data/soure_year.rds")
