### script per l'importazione dei dati dal file csv esportato da google form ###

#2do: cambiare tipo di campo a campi particolari 

### pacchetti necessari non li carico ma lo richiamo nel codice
# library(readr)
# library(splitstackshape)

# importazione dati

# data <- read_csv("/mnt/dati/umberto/Documenti/importanza_1_sync/unaapi/questionario_BIP_UNAAPI/2019-11-23_Studio epidemiologico dell’impatto delle pratiche apistiche e dell’alimentazione artificiale sulla fitness delle colonie.csv")
data <- readr::read_csv("../dati/2020-01-09_dati.csv")

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
#i campi "altro" finiscono comunque in colonne a parte (potrebbero essere riconosciuti dal fatto che c'è solo 1 record con la scelta, ma potrebbe essere stata selezionata più volte)
#dal record funzionale si estrarrebbero facilmente tutti i numero di campo che devono essere trattati come scelta multipla

#campi[campi$numsemicolon > 0, 2] <- make.names(paste(campi$prog[campi$numsemicolon > 0], abbreviate(campi[campi$numsemicolon > 0, 1]), ssep = "."))

colnames(data) <- campi$prog #cambio temporaneo dei nomi di campo con progressivi per poter riordinare nomi e non averli troppo lunghi 

for(i in campiScMult){
	data <- splitstackshape::cSplit_e(data, i, ";", type="character", fill=0) #divido i campi a scelta multipla in campi sì/no
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
#head(colnames(data))

rm(list = c("campi", "colnamesNewData", "campiScMult")) # eliminazione di oggetti provvisori utilizzati per la lavorazione

# domanda 0005: eliminazione di doppioni da campo "altro"
#colnames(data)[grepl("^0005", colnames(data))]
data$`0005_Cera-` <- data$`0005_Cera-` | data$`0005_cera-` | data$`0005_propoli, cera-`
data$`0005_Propoli-` <- data$`0005_Propoli-` | data$`0005_Proponi essiccata-` | data$`0005_propoli-` | data$`0005_propoli, cera-`
#2do aggiungere l'opzione in google form "servizi didattici" e correggere il record "servizi di didattici", soluzione provvisoria:
colnames(data)[which(colnames(data) == "0005_Servizi di didattici-")] <- "0005_Servizi didattici-"
#colonne da cancellare
col2del <- which(colnames(data) %in% c("0005_cera-", "0005_propoli, cera-", "0005_Proponi essiccata-", "0005_propoli-")) #posizioni delle colonne da cancellare
data <- data[, - col2del] #eliminate le colonne in posizione col2del
rm(col2del) #eliminato l'oggetto col2del

# cambio tipo di dati ----
data[ , grepl( "^....-20\\." , names( data ) ) ] <- factor(data[ , grepl( "^....-20\\." , names( data ) ) ], levels = c("0-5%", "6-10%", "11-15%", "16-20%"))
data[ , grepl( "^....-21\\." , names( data ) ) ] <- factor(data[ , grepl( "^....-21\\." , names( data ) ) ], levels = c("Minori", "Uguali","Maggiori",  "Non so"))



# calcolo mortalità ----
# mortalità invernale
data$cMinv <-
	(1 - (data[, grepl("^....-16\\." , names(data))] /
					data[, grepl("^....-10\\." , names(data))])) * 100

data$cMinv[data$cMinv < 0] <- NA

# mortalità estiva
data$cMest <-
	(1 - (data[, grepl("^....-5\\." , names(data))] /
					data[, grepl("^....-4\\." , names(data))])) * 100

data$cMest[data$cMest < 0] <- NA


data <- data[-which(data$`0007-4.	Numero di colonie vive al 1 aprile 2018` ==7000),]

# esportazione dati
xlsx::write.xlsx(data, paste0(Sys.Date(), "_dati_espansi.xlsx"))
saveRDS(data, "./shiny/data.RDS")

#2do: aggiungere nord, centro e sud sulla base della classificazione della sede aziendale
