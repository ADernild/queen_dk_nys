library(dplyr)
library(rvest)
library(stringr)

setwd("../")

# Danish country names, ISO codes and coordinates
url <- "https://da.wikipedia.org/wiki/ISO_3166-1" # Wikipedia ISO codes and danish country names
csv <- "https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv" # ISO codes and coordinates

# Scraping wikipedia table with ISO codes and country names
tbls <- url %>% 
  read_html() %>%
  html_nodes("table")

ISO_codes <- na.omit(html_table(tbls[1][[1]]))

# Reading in csv of coordinates and ISO codes
coords <- read.csv(csv)

# Joining danish country names with coordinates
coords <- coords %>%
  right_join(ISO_codes, by = c("Numeric.code" = "Numerisk")) %>% 
  select(c("Land", "Country", "Latitude..average.", "Longitude..average.", "Alpha.2.code")) %>% 
  unique()

names(coords) <- c("land", "country", "lat", "long", "code")
coords$land <- tolower(coords$land)
coords$land[coords$land == "oman"] <- "[^r]oman"
coords$country <- tolower(coords$country)
coords$country[coords$country == "oman"] <- "[^r]oman"

coords <- na.omit(coords)
# Reading in speeches
df <- read.csv("data/nys_sentences.csv")
df_eng <- read.csv("data/nys_sentences_eng.csv")

df$sentences <- paste("", df$sentences)
df_eng$sentences <- paste("", df_eng$sentences)

match_countries <- function(df, coords, lang="da"){
  if(lang == "da"){
    matches <- data.frame(t(sapply(seq(length(df$sentences)), function(i) list(countries=unlist(str_match_all(df$sentences[i], unique(coords$land))),
                                                                               year=df$years[i],
                                                                               sentiment=df$polarity[i],
                                                                               sentence=df$sentences[i],
                                                                               sentence_full=df$sentences_full[i]))))
    matches <- matches[lapply(matches$countries, length)>0,]
    
    matches <- tidyr::unnest(matches, cols = c(countries, year, sentiment, sentence, sentence_full))
    
    data <- matches %>% 
      group_by(year, countries) %>% 
      dplyr::summarise(
        sentence = list(sentence),
        sentence_full = list(sentence_full),
        sentiment = mean(sentiment),
        n = n()
      )
    
    coords <- coords %>% 
      select("land", "lat", "long", "code") %>% 
      unique()
    
    data <- data %>% 
      left_join(coords, by = c("countries" = "land"))
    
  }else if(lang == "en"){
    
    matches <- data.frame(t(sapply(seq(length(df$sentences)), function(i) list(countries=unlist(str_match_all(df$sentences[i], coords$country)),
                                                                               year=df$years[i],
                                                                               sentiment=df$polarity[i],
                                                                               sentence=df$sentences[i],
                                                                               sentence_full=df$sentences_full[i]))))
    matches <- matches[lapply(matches$countries, length)>0,]
    
    matches <- tidyr::unnest(matches, cols = c(countries, year, sentiment, sentence, sentence_full))
    
    data <- matches %>% 
      group_by(year, countries) %>% 
      dplyr::summarise(
        sentence = list(sentence),
        sentence_full = list(sentence_full),
        sentiment = mean(sentiment),
        n = n()
      )
    
    coords <- coords %>% 
      select("country", "lat", "long", "code") %>% 
      unique()
    
    data <- data %>% 
      left_join(coords, by = c("countries" = "country"))
  }
  data$code <- str_squish(data$code)
  return(data)
}

data_da <- match_countries(df, coords, lang="da")
data_eng <- match_countries(df_eng, coords, lang="en")

saveRDS(data_da, "data/country_speech.rds") # saving countries matched
saveRDS(data_eng, "data/country_speech_eng.rds") # saving countries matched
