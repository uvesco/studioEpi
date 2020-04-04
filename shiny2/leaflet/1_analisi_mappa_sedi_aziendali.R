# mappa sedi aziendali ----------------
library(rgdal)
library(RColorBrewer)
library(maptools)
library(leaflet)
library(htmlwidgets)
library(RCurl)

province <- table(data[4])
province <- substr(names(province), 1,3)
province <- data.frame(COD_PRO = as.integer(province), num = as.vector(tabella))
prov2008G <-readOGR("./geodata/amministrativi_ita/prov2008_s.shp")
#proj4string(prov2008G) <- CRS("+init=epsg:32632")

# EPSG <- make_EPSG()
# llCRS<-CRS(EPSG[grep("32632", EPSG$code), 3])
# rm(EPSG)
colori<-colorRamp(c("yellow", "red")) ## (x) , x in [0,1]
prov2008G <- merge(prov2008G, province)

cols <- brewer.pal(3, "YlOrRd")
pal <- colorRampPalette(c("yellow", "red"))(10)
prov2008G$cols[!is.na(prov2008G$num)] <- rgb(colorRamp(pal)(prov2008G$num[!is.na(prov2008G$num)]/10), max=255)

# trasformo le coordinate in google mercatore:
# CRS.new <- CRS("+init=epsg:3857")
# prov2008G <- spTransform(prov2008G, CRS.new)
geo_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # necessario trasformare in L/L WGS84
prov2008G <- spTransform(prov2008G,geo_proj)

proj4string(prov2008)
proj4string(prov2008G)

plot(prov2008G, col=prov2008G$cols)

########### Oggetto Mappa ----------------

records <- prov2008G$num
records[is.na(records)] <- 0
labels <- sprintf(
	"<strong>Provincia di %s:</strong><br/>%g record",
	prov2008G$NOME_PRO, records
) %>% lapply(htmltools::HTML)


m <- leaflet(prov2008G) %>%
	setView(12, 41.5, 5) %>%
	# addTiles(group = "OSM (default)") %>%
	# addProviderTiles(providers$Esri.WorldStreetMap, group = "Street") %>%
	addProviderTiles(providers$Esri.WorldTerrain, group = "Terrain")

m %>% addPolygons(
	fillColor = prov2008G$cols,
	weight = 2,
	opacity = 1,
	color = "#00000000",
	dashArray = "3",
	fillOpacity = 0.7,
	highlight = highlightOptions(
		weight = 2,
		color = "#BBB",
		dashArray = "",
		fillOpacity = 0.7,
		bringToFront = TRUE),
	label = labels,
	labelOptions = labelOptions(
		style = list("font-weight" = "normal", padding = "3px 8px"),
		textsize = "15px",
		direction = "auto"))


