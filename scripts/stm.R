# Library
library(dplyr)
library(stringr)
# devtools::install_github("ADernild/stm") # Made some changes to STM package
library(stm)
library(stopwords)
library(SnowballC)
# library(igraph)
# library(networkD3)

# Reading data
# df <- read.csv("data/nys_2001-2020_cleaned.csv")
df <- read.csv("data/nys_sentences.csv")

# Making stopwords list
stop_words <- read.csv("utils/custom_stopwords.txt", header=F) %>% 
  rbind(read.csv("utils/stopord.txt", header=F)) %>% 
  rbind(stopwords::stopwords(language = "da", source = "snowball")) %>% 
  unique() %>%
  rbind("danmark", "danske") %>% 
  rename(word = V1)

# STM 
make_stm_model <- function(docs, df, covariates, stop_words) {
  # Preprocessing documents for use in STM
  processed <- textProcessor(docs, metadata = df, stem=F, customstopwords = stop_words, language = "danish")
  out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
  
  # finding best amount of topics K
  best_model <- searchK(out$documents, out$vocab, K=c(10:35),
                        init.type="Spectral", proportion = 0.1,
                        prevalence=covariates, data=out$meta,
                        cores=parallel::detectCores())

  best_k <- best_model$results$K[which.max(best_model$results$heldout)][[1]]

  # Training model
  model <- stm(out$documents, out$vocab, K = best_k, prevalence =covariates,
               max.em.its = 300, data = out$meta, init.type = "Spectral")

  # Saving model results in list
  stm_model <- list(mod = model,
                    docs = out$documents)

  # Saving model as rds
  saveRDS(stm_model, file = "data/stm_model.rds")
  stm_model
}

make_stm_model(df$sentences, df, covariates=formula(~years + polarity), stop_words$word)

# processed <- textProcessor(df$sentences, metadata = df, stem=F, customstopwords = stop_words$word, language = "danish")
# 
# out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
# 
# best_model <- searchK(out$documents, out$vocab, K=c(10, 19:39, 40),
#                       init.type = "Spectral", proportion = 0.1,
#                       prevalence =~ years + polarity, data=out$meta,
#                       cores = parallel::detectCores())
# 
# best_k <- best_model$results$K[which.max(best_model$results$heldout)]
# 
# q_nys <- stm(out$documents, out$vocab, K = best_k, prevalence =~years + polarity,
#              max.em.its = 300, data = out$meta, init.type = "Spectral")
# 
# stm_model <- list(mod = q_nys,
#                   docs = out$documents)
# 
# # Saving model
# saveRDS(stm_model, file = "data/stm_model.rds")

# stmCorrViz::stmCorrViz(q_nys, "test.html", documents_raw = df$speech, documents_matrix = processed$documents)

# # Topic correlations
# x <- topicCorr(q_nys)
# 
# # Topic labels i.e., most probable words etc.
# topic_labels <- labelTopics(q_nys)
# 
# lift <- topic_labels$lift %>% 
#   as.data.frame() %>% 
#   group_by(row_number()) %>% 
#   summarize(topic = paste(c(V1, V2, V3, V4, V5, V6, V7), collapse="; "))
# 
# # Finding speeches and words related to topics
# thoughts <- findThoughts(q_nys, processed$meta$speech)
# 
# docs_containing_topics <- thoughts$index
# 
# docs <- paste(docs_containing_topics) %>% 
#   str_replace_all("[c( )]", "") %>% 
#   str_replace_all("13:11", "13,12,11") %>% 
#   str_replace_all("15:17", "15,16,17")
#   
# # Correlation matrix of only significantly correlated topics
# topics <- 1:nrow(x$posadj)
# t <- x$posadj[topics, topics]
# t2 <- x$cor[topics, topics]
# t2[t < 1] <- 0
# 
# # Network igraph object
# g <- graph.adjacency(t2, mode='undirected', weighted = T,
#                      diag = F)
# 
# # Converting igraph object to networkD3 object
# i <- igraph_to_networkD3(g)
# i$nodes$group <- lift$topic
# i$nodes$labels <- lift$topic
# i$nodes$docs <- docs
# 
# i$nodes$name <- paste("Topic", i$nodes$name, "in doc:", i$nodes$docs)
# 
# # Creating D3 network graph
# network <- forceNetwork(Links = i$links, Nodes = i$nodes,
#              Source = 'source', Target = 'target',
#              NodeID = 'name', Value = 'value', Group = 'group',
#              linkDistance = JS("function(d){return d.value * 10}"),
#              linkWidth = JS("function(d) {return d.value * 2}"),
#              charge = -100,
#              zoom = T,
#              fontSize = 12)
# 
# saveNetwork(network, "network.html")
