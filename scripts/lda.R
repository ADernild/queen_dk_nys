# File is not used in dashboard
library(tm)
library(textmineR)
library(tidytext)
library(LDAvis)
library(ggplot2)
library(ldatuning)

df <- read.csv("data/sentences.csv")

stop_words <- read.csv("utils/custom_stopwords.txt", header=F) %>% 
  rbind(read.csv("utils/stopord.txt", header=F)) %>% 
  rbind(stopwords::stopwords(language = "da", source = "snowball")) %>% 
  unique() %>% 
  rbind("danmark", "danske") %>% 
  rename(word = V1)

df$sent_nr <- 1:1586

dtm <- CreateDtm(doc_vec = df$sentences, 
                 doc_names = df$sent_nr,
                 ngram_window = c(1,2),
                 stopword_vec = stop_words)

dtm <- dtm[,colSums(dtm) > 2]

queen_lda <- FitLdaModel(dtm = dtm,
                         k = 20, 
                         iterations = 700,
                         burnin = 160,
                         alpha = 0.01,
                         beta = 0.05,
                         optimize_alpha = T,
                         calc_likelihood = T,
                         calc_coherence = T,
                         calc_r2 = T)

queen_results <- list(phi = queen_lda$phi,
                      theta = queen_lda$theta,
                      doc.length = unlist(lapply(df$speech, str_count)),
                      vocab = queen_lda$data@Dimnames[[2]],
                      term.frequency = TermDocFreq(queen_lda$data)$term_freq)

saveRDS(queen_results, file = "data/lda_model.rds")


# Some other visualizations (not used)
# queen_topics <- tidy(queen_lda, matrix = 'beta')
# 
# queen_top_terms <- queen_topics %>% 
#   group_by(topic) %>% 
#   slice_max(beta, n = 10) %>% 
#   ungroup() %>% 
#   arrange(topic, -beta)
# 
# queen_top_terms %>% 
#   mutate(term = reorder_within(term, beta, topic)) %>% 
#   ggplot(aes(beta, term, fill = factor(topic))) +
#   geom_col(show.legend = F) + 
#   facet_wrap(~ topic, scales = 'free') +
#   scale_y_reordered()
