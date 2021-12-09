# Scraping new speech from kongehuset.dk
library(dplyr)
library(rvest)

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

# Binding column urls to years kongehuset
last_year <- as.integer(format(Sys.Date(), "%Y")) - 1 # Last year i.e., the year of the lastest speech
kongehuset <- data.frame(year = last_year:2001, urls)

# Scraping functions
scrape_kongehuset <- function(url) {
  speech <- read_html(url) %>% 
    html_nodes(css = '.rich-text__container__content p') %>% 
    html_text()
  return(speech)
}

# Scraping newest speech
new_speech <- scrape_kongehuset(kongehuset$urls[1]) %>% 
  paste(collapse = " ")

new_speech <- data.frame(speech = new_speech, year = last_year)

# Saving newest speech to new .csv
df <- read.csv(paste0("data/new_year_speeches_1972-", (last_year-1), ".csv")) %>% 
  rbind(new_speech) %>% 
  write.csv(paste0("data/new_year_speeches_1972-", (last_year), ".csv"))

         