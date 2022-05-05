library(dplyr)
library(stopwords)
# library(SnowballC)

## Stopwords ----
### ... from stopwords defined by Bertel Torp ----
berteltorp_stopwords <- read.table("https://gist.githubusercontent.com/berteltorp/0cf8a0c7afea7f25ed754f24cfc2467b/raw/305d8e3930cc419e909d49d4b489c9773f75b2d6/stopord.txt", encoding = "UTF-8") # Stopwords defined by Bertel Torp https://gist.github.com/berteltorp

### ... from stopwords defined by snowball ----
snowball_stopwords <- stopwords(language = "da", source = "snowball") %>%  # Stopwords defined by snowball (Richard Boulton & Olly Betts) http://snowball.tartarus.org/algorithms/danish/stop.txt
  as.data.frame()
names(snowball_stopwords) <- "V1"

### ... from stopwords defined by Max Festersen Hansen & Alexander Ibsen Dernild ----
#custom_stop_words <- read.table("utils/custom_stopwords.txt", encoding = "UTF-8") # Custom stopwords defined by Max F.H. & Alexander I.D.

## Combining stopwords ----
stop_words <- full_join(berteltorp_stopwords, snowball_stopwords, by = "V1") %>% # Combine lists
  # full_join(custom_stop_words, by = "V1") %>% 
  arrange(V1) %>%  # Sort alphabetically
  rename(word = V1)

saveRDS(stop_words, "utils/stopwords.rds")
write.csv(stop_words, "utils/stopwords.csv", row.names = F, fileEncoding = "UTF-8")

# Unsetting ----
rm(list=ls())
detach("package:dplyr", unload=TRUE)
detach("package:stopwords", unload=TRUE)
