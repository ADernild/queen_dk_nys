# Library
library(dplyr)
library(stringr)
# devtools::install_github("ADernild/stm") # Made some changes to STM package
library(stm)
library(stopwords)
library(SnowballC)

# Reading data
df <- read.csv("data/sentences.csv", encoding = "UTF-8")

# Making stopwords list
stop_words <- read.csv("utils/stopord.txt", header=F) %>% 
  rbind(data.frame(V1 = stopwords::stopwords(language = "da", source = "snowball"))) %>% 
  unique() %>%
  rbind("danmark", "danske") %>% 
  rename(word = V1)

# STM 
make_stm_model <- function(docs, df, covariates, stop_words, language="danish") {
  # Preprocessing documents for use in STM
  processed <- textProcessor(docs, metadata=df, stem=T, customstopwords=stop_words, removenumbers = F, language=language)
  out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
  
  # finding best amount of topics K
  best_model <- searchK(out$documents, out$vocab, K=c(10:35),
                        init.type="Spectral", proportion=0.1,
                        heldout.seed=1337, prevalence=covariates,
                        data=out$meta)

  best_k <- best_model$results$K[which.max(best_model$results$heldout)][[1]]

  # Training model
  model <- stm(out$documents, out$vocab, K=best_k, prevalence=covariates,
               max.em.its=300, data=out$meta, init.type="Spectral", verbose=F)

  thoughts <- findThoughts(model, out$meta$sentences_full, n=200, thresh = 0.45)
  
  thoughts$years <- lapply(thoughts$index, function(i) out$meta$years[i])
  thoughts$polarity <- lapply(thoughts$index, function(i) out$meta$polarity[i])
  
  # Saving model results in list
  stm_model <- list(mod=model,
                    docs=out$documents,
                    best=best_model)

  
  list(stm_model = stm_model, thoughts = thoughts)
}

stm_da <- make_stm_model(df$sentences, df, covariates=formula(~uuid + polarity), stop_words$word)
# Saving model as rds
saveRDS(stm_da$stm_model, file="data/stm_model.rds")
saveRDS(stm_da$thoughts, file="data/thoughts.rds")
