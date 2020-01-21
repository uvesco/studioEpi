# mappa sedi aziendali ----------------
library(rgdal)
library(RColorBrewer)
tabella <- table(data[4])
province <- substr(names(tabella), 1,3)
province <- data.frame(COD_PRO = as.integer(province), num = as.vector(tabella))
prov2008<-readOGR("./geodata/amministrativi_ita/prov2008_s.shp")
EPSG <- make_EPSG()
llCRS<-CRS(EPSG[grep("32632", EPSG$code), 3])
rm(EPSG)
colori<-colorRamp(c("yellow", "red")) ## (x) , x in [0,1]

cols <- brewer.pal(3, "YlOrRd")
pal <- colorRampPalette(c("yellow", "red"))(10)
prov2008$cols[!is.na(prov2008$num)] <- rgb(colorRamp(pal)(prov2008$num[!is.na(prov2008$num)]/10), max=255)

plot(prov2008, col=prov2008$cols)

