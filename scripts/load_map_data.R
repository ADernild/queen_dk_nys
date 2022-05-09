# Load map data -----------------------------------------------------------
## Denmark -----------------------------------------------------------------
geodata_denmark <- readRDS("utils/denmark.geo.rds") # Read geojson contents

## Southern Denmark ---------------------------------------------------------
geodata_southjutland <- readRDS("utils/geofile_southjutland.rds") # Read geojson contents

## Fyn ----------------------------------------------------------------------
geodata_fyn <- readRDS("utils/geofile_fyn.rds") # Read geojson contents

## Komunes ------------------------------------------------------------------
geodata_komunes <- readRDS("utils/geofile_komunes.rds") # Read geojson contents

## geodata_frame <- geodata@data %>% as_tibble()  # For observing data
