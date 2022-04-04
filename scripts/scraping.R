# Scraping Articles from TV2 Fyn by using the bazo API
# Documentation: https://developer.bazo.dk/
# API: public.fyn.bazo.dk

# Libraries ----
library(dplyr)
library(jsonlite) # API stuff
library(stringr) # To handle whitespace

# Functions
cleanFun <- function(htmlString) { # Source: https://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
  return(gsub("<.*?>", "", htmlString))
}

# Set initial data for api ----
public <- "https://public.fyn.bazo.dk"
ver <- "/v1/"
public <- paste(public, ver, sep="")

# Load or create library of articles ----
if(file.exists("data/article_library.rds")){
  # Load library if it exists
  library <- readRDS("data/article_library.rds")
} else {
  # Create library if it doesn't exist
  columns= c("uuid", "title", "date_published_at", "date_updated_at", "link", "content") # Define names
  library <- data.frame(matrix(nrow = 0, ncol = length(columns))) # Create dataframe
  colnames(library) = columns # Set names
  library <- library %>% # Define types
    mutate(uuid = as.character(uuid),
           title = as.character(title),
           date_published_at = as.Date(date_published_at),
           date_updated_at = as.Date(date_updated_at),
           link = as.character(link),
           content = as.character(content)
    )
}

# Make call for list of articles ----
page_size <- 100 # Max 100 in API
article_list_call <- paste(public, "articles?page[size]=", page_size, "&page[number]=", 1, sep="") # Define call
api_json <- fromJSON(article_list_call, flatten = FALSE)
total_pages <- api_json$meta$last_page
total_articles <- api_json$meta$total
page_num <- 10
results <- page_size * page_num

for(i in 1:page_num){
  library_bf <- nrow(library)
  if(i!=1){
    article_list_call <- paste(public, "articles?page[size]=", page_size, "&page[number]=", i, sep="") # Define call
    api_json <- fromJSON(article_list_call, flatten = FALSE)
  }
  uuid <- api_json$data$uuid
  trumpet <- api_json$data$trumpet
  title <- api_json$data$title
  date_published_at <- api_json$data$date_published_at %>% 
    as.POSIXct(format = "%Y-%m-%dT%H:%M:%S.000000Z") # Format time as time
  date_updated_at <- api_json$data$date_updated_at %>% 
    as.POSIXct(format = "%Y-%m-%dT%H:%M:%S.000000Z") # Format time as time
  link <- api_json$data$canonical
  
  # Get content
  content <- api_json$data$content
  # Format content as string with text only
  for(j in 1:length(content)){
    if(nrow(content[j][[1]])>0){
      content_elem <- content[j]
      content_elem <- content_elem %>% 
        as.data.frame() %>% 
        filter(type == "Text") # Filter for text in content (images and readmore might be interesting too)
      content_elem <- content_elem$content$html %>% # Get text
        paste(sep=" ", collapse="\n") %>%   # Format as single string
        cleanFun # Remove html
      content_elem <- str_replace(gsub("\\s+", " ", str_trim(content_elem)), "B", "b") # Remove repeated, trailing and leading whitespace
      content[j] <- content_elem
    } else{
      content[j] <- ""
    }
  }
  
  new_results <- data.frame(uuid = uuid,
                   trumpet = trumpet,
                   title = title,
                   date_published_at = date_published_at,
                   date_updated_at = date_updated_at,
                   link = link,
                   content = unlist(content)
                   ) %>% 
    mutate(title = str_trim(
      ifelse(is.na(trumpet), title, paste(trumpet, title))
    )) %>% 
    select(!trumpet)

  
  library <- full_join(library, new_results, by = c("uuid", "title", "date_published_at", "date_updated_at", "link", "content")) # Join article list with existing article list
  library_mid <- nrow(library)
  # Formart library
  library <- library %>% 
    mutate(date_published_at = as.POSIXct(date_published_at, format = "%Y-%m-%dT%H:%M:%S.000000Z"),
           date_updated_at = as.POSIXct(date_updated_at, format = "%Y-%m-%dT%H:%M:%S.000000Z")) %>% 
    arrange(desc(date_updated_at), desc(date_published_at), title, uuid) %>% 
    distinct(uuid, .keep_all = TRUE)
  library_af <- nrow(library)
  library_updated <- library_mid - library_af
  library_dif <- library_af - library_bf
  #library_updated <- page_size-sum(table(api_json$date_updated_at[api_json$date_updated_at %in% tail(library$date_updated_at, page_size)]))
  print(paste(sep="", "Progress: ", i, "/", page_num, ". Articles looped trhough: ", page_size*i, "/", results, "/", total_articles, ". Entries added: ", library_dif, ". Entries updated: ", library_updated, ". Entries in library: ", library_af)) # Print status
}

# Save library ----
# > Save library as rds ----
saveRDS(library, "data/article_library.rds")

# > Save library as csv ----
library %>% 
  write.csv("data/article_library.csv", row.names = F, fileEncoding = "UTF-8")
