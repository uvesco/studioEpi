sum(data$`0005_Api regine-`, na.rm = T) # numero di produttori di regine

tabella <- table(data$`0005_Miele-`)

png("output/0005_miele.png")
pie(tabella, 
		labels = paste0(
			round(prop.table(tabella) * 100, 1), 
			"%"), 
		col = c("white", "red"),
		main = "Produttori di miele"
		)
dev.off()

#tab_miele_regine <- table(data$`0005_Miele-`, data$`0005_Api regine-`)

tabella <- table(data$`0005_Api regine-`)

png("output/0005_regine.png")
pie(tabella, 
		labels = paste0(
			round(prop.table(tabella) * 100, 1), 
			"%"), 
		col = c("white", "red"),
		main = "Produttori di regine"
)
dev.off()

#grafico produzioni
#creo dataframe con solo le colonne della domanda 0005
data_dom <- data[ , grepl( "0005" , names( data ) ) ]
data_dom <- data_dom[ , -1] # elimino la prima colonna (grezzo con ancora i ;)

percentuali <- round(apply(data_dom, 2, mean) * 100, digits = 1) # ottengo le percentuali di ciascun
names(percentuali) <- gsub("0005_", "", names(percentuali))
names(percentuali) <- gsub("-", "", names(percentuali))

percentuali <- sort(percentuali, decreasing = T)
png("output/0005_prodotti.png", height = 450, width = 800)
par(mar = c(7.1, 4.1, 1, 2.1)) # aumento il margine in basso [3] e riduco quello in basso [1]
barplot(percentuali, las=2, ylim = c(0, 110), space = 0.2, col = c("yellow", "brown", "orange", "black", "purple", "white", "gold", "red", "blue") )
for(i in 1 : length(percentuali)){
	text(percentuali[i], x = (i * 1.2 - 0.6), y = percentuali[i] + 5, )
}
par(mar = c(5.1, 4.1, 4.1, 2.1)) # ripristino i margini di default
dev.off()

colors() #fornisce l'elenco delle 
