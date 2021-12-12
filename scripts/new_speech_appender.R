# Scraping new speech from kongehuset.dk
library(dplyr)
library(rvest)
library(stringr)

# Extracting urls of speeches
link <- "https://www.kongehuset.dk/monarkiet-i-danmark/nytarstaler/#laes-de-seneste-nytaarstaler"
last_year <- as.integer(format(Sys.Date(), "%Y")) - 1 # Last year i.e., the year of the lastest speech

url_extractor <- function(link, last_year) {
  url <- read_html(link) %>%
    html_nodes(".field-item") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  url_text <- read_html(link) %>%
    html_nodes(".field-item") %>% 
    html_nodes("a") %>% 
    html_text()
  
  urls <- tibble(url, url_text)
  urls <- urls[!is.na(str_extract(urls$url_text, as.character(last_year))),]
}

urls <- url_extractor(link, last_year)
urls$url_text <- last_year

# Converting local urls to global urls
local_urls <- is.na(str_match(urls$url, "^https://www.kongehuset.dk")) # Checking if url is a local url
urls$url[local_urls] <- lapply(urls$url[local_urls], function(x) paste0("https://www.kongehuset.dk", x)) %>%  # making local urls global
  unlist()

# Scraping functions
scrape_kongehuset <- function(url) {
  speech <- read_html(url) %>% 
    html_nodes(css = '.rich-text__container__content p') %>% 
    html_text()
  return(speech)
}

# Scraping newest speech
new_speech <- scrape_kongehuset(urls$url) %>% 
  paste(collapse = " ")

new_speech <- data.frame(speech = new_speech, year = last_year)

# Saving newest speech to new .csv
df <- read.csv(paste0("data/new_year_speeches_1972-", (last_year-1), ".csv")) %>% 
  rbind(new_speech) %>% 
  write.csv(paste0("data/new_year_speeches_1972-", (last_year), ".csv"))

         