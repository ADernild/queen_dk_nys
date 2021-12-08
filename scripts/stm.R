# Library
library(dplyr)
library(stringr)
# devtools::install_github("ADernild/stm") # Made some changes to STM package
library(stm)
library(stopwords)
library(SnowballC)
library(igraph)
library(networkD3)

# Reading data
df <- read.csv("data/nys_2001-2020_cleaned.csv")
sentences <- read.csv("data/nys_sentences.csv")
tokens <- readRDS("data/tokens.rds")

df <- df %>% 
  left_join(tokens %>% 
  group_by(year) %>% 
  summarize(
    polarity = mean(polarity)
  ), by = "year")

# Making stopwords list
stop_words <- read.csv("utils/custom_stopwords.txt", header=F) %>% 
  rbind(read.csv("utils/stopord.txt", header=F)) %>% 
  rbind(stopwords::stopwords(language = "da", source = "snowball")) %>% 
  unique() %>%
  rbind("danmark", "danske") %>% 
  rename(word = V1)

# STM 
processed <- textProcessor(df$sentences, metadata = df, stem=T, customstopwords = stop_words$word, language = "danish")

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)


best_model <- searchK(out$documents, out$vocab, K=c(5, 10, 20, 30, 40, 50),
                      init.type = "Spectral", proportion = 0.1,
                      prevalence =~ years + polarity, data=out$meta)

plot.searchK(best_model)


# STM 
processed_sent <- textProcessor(sentences$sentences, metadata = sentences, stem=T, customstopwords = stop_words$word, language = "danish")

out_sent <- prepDocuments(processed_sent$documents, processed_sent$vocab, processed_sent$meta)
set.seed(1337)
best_model <- searchK(out_sent$documents, out_sent$vocab, K=c(5, 10, 20, 30, 40, 50),
                      init.type = "Spectral", proportion = 0.1,
                      prevalence =~s(years), data=out_sent$meta)

plot.searchK(best_model)

q_nys <- stm(out$documents, out$vocab, K = 20, prevalence =~years + polarity,
             max.em.its = 300, data = out$meta, init.type = "Spectral")

stm_model <- list(mod = q_nys,
                  docs = out_sent$documents)

# Saving model
saveRDS(stm_model, file = "data/stm_model.rds")


# stmCorrViz::stmCorrViz(q_nys, "test.html", documents_raw = df$speech, documents_matrix = processed$documents)

# Topic correlations
x <- topicCorr(q_nys)

# Topic labels i.e., most probable words etc.
topic_labels <- labelTopics(q_nys)

lift <- topic_labels$lift %>% 
  as.data.frame() %>% 
  group_by(row_number()) %>% 
  summarize(topic = paste(c(V1, V2, V3, V4, V5, V6, V7), collapse="; "))

# Finding speeches and words related to topics
thoughts <- findThoughts(q_nys, processed$meta$speech)

docs_containing_topics <- thoughts$index

docs <- paste(docs_containing_topics) %>% 
  str_replace_all("[c( )]", "") %>% 
  str_replace_all("13:11", "13,12,11") %>% 
  str_replace_all("15:17", "15,16,17")
  
# Correlation matrix of only significantly correlated topics
topics <- 1:nrow(x$posadj)
t <- x$posadj[topics, topics]
t2 <- x$cor[topics, topics]
t2[t < 1] <- 0

# Network igraph object
g <- graph.adjacency(t2, mode='undirected', weighted = T,
                     diag = F)

# Converting igraph object to networkD3 object
i <- igraph_to_networkD3(g)
i$nodes$group <- lift$topic
i$nodes$labels <- lift$topic
i$nodes$docs <- docs

i$nodes$name <- paste("Topic", i$nodes$name, "in doc:", i$nodes$docs)

# Creating D3 network graph
network <- forceNetwork(Links = i$links, Nodes = i$nodes,
             Source = 'source', Target = 'target',
             NodeID = 'name', Value = 'value', Group = 'group',
             linkDistance = JS("function(d){return d.value * 10}"),
             linkWidth = JS("function(d) {return d.value * 2}"),
             charge = -100,
             zoom = T,
             fontSize = 12)

saveNetwork(network, "network.html")
