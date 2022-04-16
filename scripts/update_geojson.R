# Create/Update geodata
library(dplyr) # For data manipulation
library(rgdal)

# General information -----------------------------------------------------
# Alt. source for denmark: https://geojson-maps.ash.ms/
# Region code for southern Denmark: 1083
# District-code for Fyn: DK031
# Komunal codes: https://danmarksadresser.dk/adressedata/kodelister/kommunekodeliste - Of interest: 410, 420, 430, 440, 445, 450, 461, 479, 480, 482 & 492

# Denmark -----------------------------------------------------------------

geojson_url_denmark = "https://api.dataforsyningen.dk/regioner?format=geojson" # DAWA API Link for geojson map of southern jutland
geofile_denmark = "utils/geofile_denmark"
download.file(URLencode(geojson_url_denmark), geofile_denmark) # Download contents to temp file
# geodata_denmark <- rgdal::readOGR(geofile_denmark, use_iconv = TRUE, encoding = "UTF-8") # Read geojson contents

# Southern Denmark --------------------------------------------------------
geojson_url_southjutland = "https://api.dataforsyningen.dk/regioner/1083?format=geojson" # DAWA API Link for geojson map of southern jutland
geofile_southjutland = "utils/geofile_southjutland"
download.file(URLencode(geojson_url_southjutland), geofile_southjutland) # Download contents to temp file
# geodata_southjutland <- rgdal::readOGR(geofile_southjutland, use_iconv = TRUE, encoding = "UTF-8") # Read geojson contents

# Fyn ---------------------------------------------------------------------
geojson_url_fyn = "https://api.dataforsyningen.dk/landsdele/DK031?format=geojson" # DAWA API Link for geojson map of Fyn
geofile_fyn = "utils/geofile_fyn"
download.file(URLencode(geojson_url_fyn), geofile_fyn) # Download contents to temp file
# geodata_fyn <- rgdal::readOGR(geofile_fyn, use_iconv = TRUE, encoding = "UTF-8") # Read geojson contents

# Komunes -----------------------------------------------------------------
geojson_url_komunes = "https://api.dataforsyningen.dk/kommuner?kode=410|420|430|440|445|450|461|479|480|482|492&format=geojson" # DAWA API Link for geojson map of komunes on Fyn
geofile_komunes = "utils/geofile_komunes"
download.file(URLencode(geojson_url_komunes), geofile_komunes) # Download contents to temp file
# geodata_komunes <- rgdal::readOGR(geofile_komunes, use_iconv = TRUE, encoding = "UTF-8") # Read geojson contents
