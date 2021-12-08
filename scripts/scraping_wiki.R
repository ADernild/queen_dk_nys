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

fileConn<-file("www/queen_info_table.html", encoding = "UTF-8")
writeLines(queen_info_table, fileConn)
close(fileConn)
