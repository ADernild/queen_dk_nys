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

last_year <- as.integer(format(Sys.Date(), "%Y")) - 1 # Last year i.e., the year of the lastest speech

# Reading in speeches
df <- read.csv(paste0("data/nys_1972-", last_year, "_cleaned.csv"))

# Matching country names with speeches
matched_countries <- lapply(df$speech, function(x) str_match_all(x, coords$land))

data <- data.frame(year=c(), countries=c())
for(i in 1:length(matched_countries)) {
  year <- c(df$year[i])
  countries <- c(unlist(matched_countries[[i]]))
  data <- rbind(data, data.frame(year, countries))
}

# Summarizing matched countries to number pr. year
data <- data %>% 
  group_by(year, countries) %>% 
  dplyr::summarise(
    n = n()
  )

# Joining coords to matched countries
data <- data %>% 
  left_join(coords, by = c("countries" = "land"))

data$code <- str_squish(data$code) # the ISO codes has whitespace for some reason

saveRDS(data, "data/country_speech.rds") # saving countries matched
