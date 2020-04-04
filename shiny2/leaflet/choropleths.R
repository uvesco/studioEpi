library(geojson)
# https://rstudio.github.io/leaflet/choropleths.html
states <- geojson_read("us_states.json", what = "sp")
