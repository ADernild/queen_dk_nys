library(tidyverse)
library(stm)

df <- read.csv("data/nys_2001-2020_cleaned.csv")

processed <- textProcessor(df$speech, metadata = df, removestopwords = T, language='da')

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

q_nys <- stm(out$documents, out$vocab, K = 0, prevalence =~ s(year),
             max.em.its = 75, data = out$meta, init.type = "Spectral")

plot.STM(q_nys)

labelTopics(q_nys)

