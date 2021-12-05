# Web scraping The Queen of Denmark's New Years speeches from kongehuset.dk

# Libraries
library(dplyr)
library(rvest)

# Extracting urls of speeches
urls <- read_html("https://www.kongehuset.dk/monarkiet-i-danmark/nytarstaler/#laes-de-seneste-nytaarstaler") %>%
  html_nodes(".accordion__container__item__content") %>% 
  html_nodes(".field-item") %>% 
  html_nodes("a") %>%
  html_attr("href")

local_urls <- is.na(str_match(urls, "^https://www.kongehuset.dk"))

urls[local_urls] <- lapply(urls[local_urls], function(x) paste0("https://www.kongehuset.dk", x))

# Binding column years to speeches and saving as csv
last_year <- as.integer(format(Sys.Date(), "%Y")) - 1
speech_year_url <- cbind(year = last_year:2001, urls) %>% 
  as.data.frame()

# Scraping function
scrape <- function(link) {
  speech <- read_html(link) %>% 
  html_nodes(css = '.rich-text__container__content p') %>% 
    html_text()
  return(speech)
}

# Scraping speeches
speeches <- lapply(speech_year_url$urls, scrape) %>% 
  lapply(paste, collapse = " ") #collapsing LoL (list of lists)

# Coercing to data.frame and adding year variable
df <- do.call(rbind, speeches) %>% 
  cbind(year = last_year:2001) %>% 
  as.data.frame() %>%  
  write.csv("data/new_year_speeches_2001-2020.csv", row.names = F) #saving as csv
