# Scraping Articles from TV2 Fyn by using the bazo API
# Documentation: https://developer.bazo.dk/
# API: public.fyn.bazo.dk

print("scrabing.R")

# Libraries ----
library(dplyr)
library(jsonlite) # API stuff
library(stringr) # To handle whitespace

# Settings ----
check_content <- T
new_entries <- 0

# Functions ----
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
  columns= c("uuid", "title", "date_published_at", "date_updated_at", "section", "authors", "link", "location", "content") # Define names
  library <- data.frame(matrix(nrow = 0, ncol = length(columns))) # Create dataframe
  colnames(library) = columns # Set names
  library <- library %>% # Define types
    mutate(uuid = as.character(uuid),
           title = as.character(title),
           date_published_at = as.Date(date_published_at),
           date_updated_at = as.Date(date_updated_at),
           section = as.character(section),
           authors = as.character(authors),
           link = as.character(link),
           location = as.character(location),
           content = as.character(content)
    )
}

# Make call for list of articles ----
page_size <- 100 # Max 100 in API
article_list_call <- paste(public, "articles?page[size]=", page_size, "&page[number]=", 1,
                           "&include[0]=activeContentRevision.publishedPrimaryLocation",
                           "&include[1]=activeContentRevision.authors",
                           "&include[2]=activeContentRevision.publishedPrimarySection",
                           sep="") # Define call
api_json <- fromJSON(article_list_call, flatten = FALSE)
# article_call <- paste(public, "articles/96f37867-5ffb-46b1-830e-5074b0f84e64",
#                            sep="") # Define call
# api_single <- fromJSON(article_call, flatten = FALSE)
total_pages <- api_json$meta$last_page
total_articles <- api_json$meta$total
page_num <- 13
results <- page_size * page_num

# Call and loop ----
for(i in 1:page_num){
  library_bf <- nrow(library)
  if(i!=1){
    article_list_call <- paste(public, "articles?page[size]=", page_size, "&page[number]=", i, 
                               "&include[0]=activeContentRevision.publishedPrimaryLocation",
                               "&include[1]=activeContentRevision.authors",
                               "&include[2]=activeContentRevision.publishedPrimarySection",
                               sep="") # Define call
    api_json <- fromJSON(article_list_call, flatten = FALSE)
  }
  uuid <- api_json$data$uuid
  trumpet <- api_json$data$trumpet
  title <- api_json$data$title
  loc <- api_json$data$primary_location$name
  date_published_at <- api_json$data$date_published_at %>% 
    as.POSIXct(format = "%Y-%m-%dT%H:%M:%S.000000Z") # Format time as time
  date_updated_at <- api_json$data$date_updated_at %>% 
    as.POSIXct(format = "%Y-%m-%dT%H:%M:%S.000000Z") # Format time as time
  link <- api_json$data$canonical
  section <- api_json$data$primary_section$name
  
  # Get authors
  authors <- api_json$data$authors
  # Format authors as string with text only
  for(j in 1:length(authors)){
    if(nrow(authors[j][[1]])>0){
      authors_elem <- authors[j]
      authors_elem <- authors_elem %>% 
        as.data.frame()
        # filter(type == "Text") # Filter for text in authors (images and readmore might be interesting too)
      authors_elem <- authors_elem$name %>% # Get text
        paste(sep=", ", collapse=", ") # Format as single string
      authors[j] <- authors_elem
    } else{
      authors[j] <- ""
    }
  }
  authors <- unlist(authors)
  
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
                   section = section,
                   authors = authors,
                   link = link,
                   location = loc,
                   content = unlist(content)
                   ) %>% 
    mutate(title = str_trim(
      ifelse(is.na(trumpet), title, paste(trumpet, title))
    )) %>% 
    select(!trumpet)
  
  ## Add relevant geocodes ----
  new_results <- new_results %>% 
    rowwise() %>% 
    mutate(geocode = ifelse(location == "Middelfart","0410",
                            ifelse(location == "Assens","0420",
                                   ifelse(location == "Faaborg-Midtfyn","0430",
                                          ifelse(location == "Kerteminde","0440",
                                                 ifelse(location == "Nyborg","0450",
                                                        ifelse(location == "Odense","0461",
                                                               ifelse(location == "Svendborg","0479",
                                                                      ifelse(location == "Nordfyn","0480",
                                                                             ifelse(location == "Langeland","0482",
                                                                                    ifelse(location == "Ærø","0492",
                                                                                           ifelse(location == "Danmark","+45",
                                                                                                  ifelse(location == "Region Syddanmark","1083",
                                                                                                         ifelse(location == "Fyn","DK031",
                                                                                                                "Unknown"
                                                                                                         )))))))))))))) %>% 
    ungroup()
  
  library <- full_join(library, new_results, by = c("uuid", "title", "date_published_at", "date_updated_at", "section", "authors", "link", "location", "geocode", "content")) # Join article list with existing article list
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
  # if(check_content == T && library_dif == 0 && library_updated ==0){
  new_entries <- new_entries + library_dif
  if(check_content == T && library_dif == 0){
    # print("Terminated loop prematurely, as no entries were added or updated. To disable check, set check_content to false.")
    print("Terminated loop prematurely, as no entries were added. To disable check, set check_content to false.")
    break;
  }
}

# Save library ----
# > Save library as rds ----
saveRDS(library, "data/article_library.rds")
saveRDS(new_entries , "data/new_entries.rds")

# > Save library as csv ----
library %>% 
  write.csv("data/article_library.csv", row.names = F, fileEncoding = "UTF-8")
