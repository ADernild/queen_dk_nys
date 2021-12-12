# Scraping Margrethe 2. wiki page -----------------------------------------
library("rvest") # to scrape

# Scrape webpage ----
url <- "https://en.wikipedia.org/wiki/Margrethe_II_of_Denmark"
# kept_columns <- c("Record high °F (°C)","Record low °F (°C)")
webpage <- read_html(url)

# Filter tables ----
tbls <- html_nodes(webpage, ".infobox")
# Filter for infobox
queen_info_table <- tbls[1]
queen_info_table <- as.character(queen_info_table)

# Replace content ----
## Replace links ----
queen_info_table <- gsub("/wiki/", "https://en.wikipedia.org/wiki/", queen_info_table)
queen_info_table <- gsub("#Family", "https://en.wikipedia.org/wiki/Margrethe_II_of_Denmark#Family", queen_info_table)
queen_info_table <- gsub("#cite_note", "https://en.wikipedia.org/wiki/Margrethe_II_of_Denmark#cite_note", queen_info_table)

## Set links to open in new tab ----
queen_info_table <- gsub("href", "target='_blank' href", queen_info_table)

## Remove functional text
queen_info_table <- gsub("show", "", queen_info_table)
queen_info_table <- gsub("See list", "", queen_info_table)

# Save as html ----
fileConn<-file("www/queen_info_table_en.html", encoding = "UTF-8")
writeLines(queen_info_table, fileConn)
close(fileConn)
