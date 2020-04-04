output$input1 <- renderUI({

	a100 <-
		which(data$`0007-4.	Numero di colonie vive al 1 aprile 2018` < 100)
	a130 <-
		which(
			data$`0007-4.	Numero di colonie vive al 1 aprile 2018` >= 100 &
				data$`0007-4.	Numero di colonie vive al 1 aprile 2018` < 300
		)
	a300 <-
		which(data$`0007-4.	Numero di colonie vive al 1 aprile 2018` >= 300)
	all3 <-
		1:length(data$`0007-4.	Numero di colonie vive al 1 aprile 2018`)
	
		radioButtons(
	"vdom",
	label = "Quesito",
	choices = c(
		"Prodotti",
		"Numero di colonie",
		"Mortalità invernale",
		"Mortalità in stagione attiva",
		"Dimensioni dell'apiario",
		"Perdite invernali accettabili",
		"Confronto perdite anno precedente",
		"Cause di mortalità in inverno",
		"Cause di mortalità in estate"
	),
	selected = "Prodotti"
)
})
	

