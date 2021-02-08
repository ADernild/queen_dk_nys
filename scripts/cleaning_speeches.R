library(tidyverse)

# Cleaning function
clean_speech <- function(x) {
  x %>%
    str_replace_all("\\\n", " ") %>% # removes linebreaks
    str_replace_all("[^[:alnum:]:,.]", " ") %>% # removes special characters except .,
    str_to_lower() %>% # Converts to lower case
    str_squish() # Removes leading, trailing and middle whitespace
}

# Importing data
df <- read.csv("data/new_year_speeches_2001-2020.csv")[2:3]

names(df) <- c("speech", "year") # changing first variable name

df$speech <- clean_speech(df$speech) %>% 
  write.csv("data/nys_2001-2020_cleaned.csv")

