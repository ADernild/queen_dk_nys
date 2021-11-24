library(tidyverse)
library(stm)

setwd("../")

df <- read.csv("nys_2001-2020_cleaned.csv", encoding = "UTF-8")

# df$x <- clean_speech(df$x)

processed <- textProcessor(df$speech, metadata = df, removestopwords = T, language='da')

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

q_nys <- stm(out$documents, out$vocab, K = 20, prevalence =~ s(year),
             max.em.its = 75, data = out$meta, init.type = "Spectral")
