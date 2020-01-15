#2do: cambiare tipo di campo a campi particolari 

### pacchetti necessari
library(readr)
#library(dplyr)
#library(tidyr)
#require(googleformr)
library(splitstackshape)

# importazione dati

data <- read_csv("/mnt/dati/umberto/Documenti/importanza_1_sync/unaapi/questionario_BIP_UNAAPI/2019-11-23_Studio epidemiologico dell’impatto delle pratiche apistiche e dell’alimentazione artificiale sulla fitness delle colonie.csv")

data <- as.data.frame(data)

# cerco campi delimitati internamente da ";": ottengo il numero di celle contenenti un ";" per ciascun campo
# necessario controllo manuale
campi <- data.frame(oldcolnames = colnames(data), # vecchi nome di campo
										newcolnames = rep(NA, dim(data)[2]), # nuovi nomi di campo
										prog = formatC(1:dim(data)[2], width = 4, format = "d", flag = "0"), # progressivo colonne originali
										numsemicolon = rep(0, dim(data)[2]))# numero di righe contenenti almeno un punto e virgola
			
for(i in 1:dim(data)[2]){
	campi[i, 4] <- sum(grepl(";", data[, i])) #calcolo delle righe per ogni colonne in cui c'è almeno un ;
}; rm(i)

campiScMult <- which(campi$numsemicolon > 0) #sono i campi in cui c'è un punto e virgola, che corrispondono circa alle scelte multiple
#ma possono mancare campi in sezioni non attivate, campi in cui è stata sempre solo scelta una opzione, rientrare campi in cui è stato usato il
#punto e virgola per altre ragioni
#2do: sarebbe utile inserire un record funzionale che sia completo, che non usi mai il nei campi di testo e che in ogni scelta multipla utilizzi due opzioni
#i campi "altro" finiscono comunque in colonne a parte (potrebbero essere riconosciuti dal fatto che c'i sono c'è solo 1 record con la scelta)
#dal record funzionale si estrarrebbero facilmente tutti i numero di campo che devono essere trattati come scelta multipla

#campi[campi$numsemicolon > 0, 2] <- make.names(paste(campi$prog[campi$numsemicolon > 0], abbreviate(campi[campi$numsemicolon > 0, 1]), ssep = "."))

colnames(data) <- campi$prog #cambio temporaneo dei nomi di campo con progressivi per poter riordinare nomi e non averli troppo lunghi 

for(i in campiScMult){
	data <- cSplit_e(data, i, ";", type="character", fill=0) #divido i campi a scelta multipla in campi sì/no
}; rm(i)

data <- data[, sort(colnames(data))] #dato che i campi sì/no sono al fondo (per funzione cSplit_e), li rimetto a posto

colnamesNewData <- as.data.frame(colnames(data))
colnamesNewData <- merge(colnamesNewData, campi, by.x = "colnames(data)", by.y = "prog", sort = T, all.x = T)
colnamesNewData$oldcolnames <- as.character(colnamesNewData$oldcolnames)
colnamesNewData$oldcolnames[is.na(colnamesNewData$oldcolnames)] <- ""
colnamesNewData$newcolnames <- paste0(colnamesNewData$`colnames(data)`, "-", colnamesNewData$oldcolnames)
#sum(colnames(data)!= colnamesNewData$`colnames(data)`)#check
#head(colnamesNewData$newcolnames)
colnames(data) <- colnamesNewData$newcolnames
head(colnames(data))

write.xlsx(data, "2019-11-26_dati_espansi.xlsx")


# mappa sedi aziendali ----------------
require(rgdal)
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

