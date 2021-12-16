geojson <- rgdal::readOGR("https://raw.githubusercontent.com/datasets/geo-countries/master/data/countries.geojson")

saveRDS(geojson, "data/countries.rds")
