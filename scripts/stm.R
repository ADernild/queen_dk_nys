# Library
library(dplyr)
library(stringr)
# devtools::install_github("ADernild/stm") # Made some changes to STM package
library(stm)
library(stopwords)
library(SnowballC)
library(tm)
library(tictoc)
tic()

# Reading data
df <- read.csv("data/sentences.csv", encoding = "utf-8")
# df <- readRDS("data/sentences.rds")

# Making stopwords list
stop_words <- read.csv("utils/stopord.txt", header=F) %>% 
  rbind(data.frame(V1 = stopwords::stopwords(language = "da", source = "snowball"))) %>% 
  unique() %>%
  rbind("danmark", "danske") %>% 
  rename(word = V1)

# STM 
make_stm_model <- function(docs, df, covariates, stop_words, language="danish") {
  # Preprocessing documents for use in STM
  processed <- textProcessor(docs, metadata=df, stem=T, customstopwords=stop_words, removenumbers = T, language=language)
  out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
  
  # # finding best amount of topics K
  # best_model <- searchK(out$documents, out$vocab, K=c(15,30,45,60,75,90,105),
  #                       init.type="Spectral", proportion=0.1,
  #                       heldout.seed=1337, prevalence=covariates,
  #                       data=out$meta,
  #                       cores=parallel::detectCores())
  # 
  best_model <- readRDS("data/best_model.rds") # Generated before model creation
  best_k <- best_model$results$K[which.max(best_model$results$heldout)][[1]]
# Training model
  model <- stm(out$documents, out$vocab, K=100, prevalence=covariates,
             max.em.its=300, data=out$meta, init.type="Spectral", verbose=T)

  thoughts <- findThoughts(model, out$meta$sentences_full, n=200, thresh = 0.45)

  thoughts$uuid <- lapply(thoughts$index, function(i) out$meta$uuid[i])
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

detach("package:dplyr", unload=TRUE)
detach("package:stringr", unload=TRUE)
detach("package:stm", unload=TRUE)
detach("package:stopwords", unload=TRUE)
detach("package:SnowballC", unload=TRUE)
detach("package:tm", unload=TRUE)
time <- toc()
saveRDS(time, "data/stm_time.rds")
write.csv(time, "data/stm_time.csv", row.names = F, fileEncoding = "UTF-8")
