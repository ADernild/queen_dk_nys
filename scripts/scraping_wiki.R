# Scraping Margrethe 2. wiki page -----------------------------------------
library("rvest") # to scrape

# Scrape webpage ----
url <- "https://da.wikipedia.org/wiki/Margrethe_2."
# kept_columns <- c("Record high °F (°C)","Record low °F (°C)")
webpage <- read_html(url)

# Filter tables ----
tbls <- html_nodes(webpage, ".infobox")
# Filter for infobox (first is allways editmode with when searching for infobox, and can be ignored)
queen_info_table <- tbls[1]
queen_info_table <- as.character(queen_info_table)

# Replace content ----
## Replace links ----
queen_info_table <- gsub("/wiki/", "https://da.wikipedia.org/wiki/", queen_info_table)
queen_info_table <- gsub("#Titler,_pr%C3%A6dikater,_%C3%A6resbevisninger_og_%C3%A6resudn%C3%A6vnelser", "https://da.wikipedia.org/wiki/Margrethe_2.#Titler,_pr%C3%A6dikater,_%C3%A6resbevisninger_og_%C3%A6resudn%C3%A6vnelser", queen_info_table)

## Set links to open in new tab ----
queen_info_table <- gsub("href", "target='_blank' href", queen_info_table)

## Remove functional text
queen_info_table <- gsub("Se liste", "", queen_info_table)

# Save as html ----
fileConn<-file("www/queen_info_table.html", encoding = "UTF-8")
writeLines(queen_info_table, fileConn)
close(fileConn)
