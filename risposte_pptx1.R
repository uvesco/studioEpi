data <- as.data.frame(data)
mean(data[, 10], na.rm = T)

#numero alveari monitorati
#aprile
sum(data$`4.	Numero di colonie vive al 1 aprile 2018`, na.rm = T)
mean(data$`4.	Numero di colonie vive al 1 aprile 2018`, na.rm = T)
#ottobre
# numero di colonie possedute
mean(data$`10.	Quante colonie vive aveva il 1 ottobre 2018?`, na.rm = T)
#vive da aprile a ottobre
mean(data$`5.	Relativamente alla domanda 4: Quante colonie, tra queste erano ancora vive il 1 ottobre 2018?`[!is.na(data$`5.	Relativamente alla domanda 4: Quante colonie, tra queste erano ancora vive il 1 ottobre 2018?`)]/data$`4.	Numero di colonie vive al 1 aprile 2018`[!is.na(data$`5.	Relativamente alla domanda 4: Quante colonie, tra queste erano ancora vive il 1 ottobre 2018?`)])
#aprile 19
mean(data$`15. Quante famiglie vive (che hanno superato l'inverno, riunite o acquistate) possedeva al 1 aprile 2019`, na.rm=T)
sum(data$`15. Quante famiglie vive (che hanno superato l'inverno, riunite o acquistate) possedeva al 1 aprile 2019`, na.rm=T)


#2do correlazione mortalità con dimensioni azienda
		