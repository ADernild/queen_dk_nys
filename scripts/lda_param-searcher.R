# File is not used in dashboard
library(tm)
library(textmineR)
library(tidytext)

df <- read.csv("data/nys_2001-2020_cleaned.csv")

stop_words <- read.csv("utils/custom_stopwords.txt", header=F) %>% 
  rbind(read.csv("utils/stopord.txt", header=F)) %>% 
  rbind(stopwords::stopwords(language = "da", source = "snowball")) %>% 
  unique() %>% 
  rbind("danmark", "danske") %>% 
  rename(word = V1)

dtm <- CreateDtm(doc_vec = df$speech, 
                 doc_names = df$year,
                 ngram_window = c(1,2),
                 stopword_vec = stop_words)

params <- list(k = c(10, 15, 20, 30, 40, 50),
               iter = c(400, 500, 600, 700),
               burnin = c(120, 140, 160, 180, 200),
               alpha = c(0.5, 0.3, 0.1, 0.05, 0.01),
               beta = c(0.7, 0.5, 0.2, 0.1, 0.05))

results <- data.frame(k = c(),
                      iter = c(),
                      burnin = c(),
                      alpha = c(),
                      beta = c(),
                      r2 = c())

model <- 0
models <- nrow(expand.grid(params))

for (k in params$k) {
  for (iter in params$iter) {
    for (burnin in params$burnin) {
      for (alpha in params$alpha) {
        for (beta in params$beta) {
          lda <- FitLdaModel(dtm = dtm,
                             k = k,
                             iterations = iter,
                             burnin = burnin,
                             alpha = alpha,
                             beta = beta,
                             optimize_alpha = T,
                             calc_likelihood = T,
                             calc_coherence = T,
                             calc_r2 = T)
          
          res <- data.frame(k = c(k),
                            iter = c(iter),
                            burnin = c(burnin),
                            alpha = c(alpha),
                            beta = c(beta),
                            r2 = c(lda$r2))
          
          results <- rbind(results, res)
          model <- model + 1
          print(paste(model, "of", models , "models"))
        }
      }
    }
  }
}

results[which.max(results$r2),]
