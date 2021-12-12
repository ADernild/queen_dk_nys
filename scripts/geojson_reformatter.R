geojson <- rgdal::readOGR("data/countries.geojson")

saveRDS(geojson, "data/countries.rds")
