library(dplyr)
library(rvest)
library(stringr)

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
  select(c("Land", "Latitude..average.", "Longitude..average.", "Alpha.2.code")) %>% 
  unique()

names(coords) <- c("land", "lat", "long", "code")
coords$land <- tolower(coords$land)

# Reading in speeches
df <- read.csv("data/nys_sentences.csv")

matches <- data.frame(t(sapply(seq(length(df$sentences)), function(i) list(countries=unlist(str_match_all(df$sentences[i], coords$land)),
                                                                year=df$years[i],
                                                                sentiment=df$polarity[i],
                                                                sentence_id=i))))
matches <- matches[lapply(matches$countries, length)>0,]

matches <- tidyr::unnest(matches, cols = c(countries, year, sentiment, sentence_id))

# Summarizing matched countries to number pr. year
data <- matches %>% 
  group_by(year, countries) %>% 
  dplyr::summarise(
    ids = list(sentence_id),
    sentiment = mean(sentiment),
    n = n()
  )

# Joining coords to matched countries
data <- data %>% 
  left_join(coords, by = c("countries" = "land"))

data$code <- str_squish(data$code) # the ISO codes has whitespace for some reason

saveRDS(data, "data/country_speech.rds") # saving countries matched
