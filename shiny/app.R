# librerie ---
library(shiny)

# importazone dati ----
data <-
	readRDS("data.RDS") # file creato dallo script di importazione dal CSV



# Define UI ----------
ui <- navbarPage(title = "Questionario Unaapi",
	tabPanel(
		"Generale",
		# Application title
		# titlePanel("Questionario Unaapi"),
		
		
		sidebarLayout(
			sidebarPanel(
				#helpText("Crea grafici interattivi"),
				
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
				),
				
				radioButtons(
					"vtar",
					label = "Dimensione aziende",
					choices = c(
						"Tutte le aziende",
						"Az. < 100 alveari",
						"Az. 100-300 alveari",
						"As. > 300 alveari"
					),
					selected = "Tutte le aziende"
				)
				
				
				# radioButtons(
				# 	"vmor",
				# 	label = "Mortalità",
				# 	choices = c("Mortalità in stagione attiva",
				# 							"Mortalità invernale"),
				# 	selected = "Mortalità invernale"
				# )#,
				
				#
				# sliderInput("range",
				# 						label = "Range of interest:",
				# 						min = 0, max = 100, value = c(0, 100))
			),
			
			# Show a plot of the generated distribution
			mainPanel(plotOutput("distplot"))
		)
	),
	tabPanel(
		"Confronti",
		sidebarLayout(
			sidebarPanel(
				radioButtons(
					"vmor",
					label = "Mortalità",
					choices = c("Mortalità in stagione attiva",
											"Mortalità invernale"),
					selected = "Mortalità invernale"
				),
				selectInput(
					"vdomsp",
					label = "Quesito",
					choices = c("Impiego di acido ossalico gocciolato",
											"Impiego di acido ossalico sublimato",
											"Impiego di acido formico",
											"Ipm: asportazione di covata da fuco",
											"Ipm: Asportazione totale di covata" ,
											"Ipm: asportazione parziale della covata per la creazione di nuclei", 
											"Ingabbiamento estivo della ape regina",
											# "Ingabbiamento invernale della ape regina" ,
											"Nutrizione: candito zuccherino",
											"Nutrizione: sciroppo zuccherino")
				),
				radioButtons(
					"vtar2",
					label = "Dimensione aziende",
					choices = c(
						"Tutte le aziende",
						"Az. < 100 alveari",
						"Az. 100-300 alveari",
						"As. > 300 alveari"
					),
					selected = "Tutte le aziende"
				)
			),
			mainPanel(plotOutput("bbplot"),
								verbatimTextOutput("nptesto")
								)
		)
		)
	
		
	)


# server --------
server <- function(input, output) {
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
	
	coloriDimApic <- c("#FFD700", "#00FFD7", "#D700FF", "white")
	
	output$distplot <- renderPlot({
		# mortal <- switch(
		# 	input$vmor,
		# 	"Mortalità in stagione attiva" = data$cMest,
		# 	"Mortalità invernale" = data$cMinv
		# )
		rcoloreda <- switch(
			# colore cambia secondo dimensione apicoltore
			input$vtar,
			"Tutte le aziende" = coloriDimApic[4],
			"Az. < 100 alveari" = coloriDimApic[3],
			"Az. 100-300 alveari" = coloriDimApic[2],
			"As. > 300 alveari" = coloriDimApic[1]
		)
		rselect <- switch(
			input$vtar,
			"Tutte le aziende" = all3,
			"Az. < 100 alveari" = a100,
			"Az. 100-300 alveari" = a130,
			"As. > 300 alveari" = a300
		)
		# numeri per tabelle che derivano da apply ----
		NtabApply <- switch(input$vdom,
											 "Prodotti" = "-2",
											 "Numero di colonie" = "-2", #valore dummy
											 "Mortalità invernale" = "-2", #valore dummy
											 "Mortalità in stagione attiva" = "-2", #valore dummy
											 "Dimensioni dell'apiario" = "-2", #valore dummy
											 "Perdite invernali accettabili" = "-2", #valore dummy
											 "Confronto perdite anno precedente" = "-2", #valore dummy
											 "Cause di mortalità in inverno" = "-22",
											 "Cause di mortalità in estate" = "-23"
											 )
		
		NcolExpl <- which(grepl(paste0("^", substr(colnames(data)[ which(grepl(paste0("^....", NtabApply ,"\\.") , names(data)))], 1, 4)), names(data)))[-1]
		tabApply <- apply(data[rselect, NcolExpl], 2, sum, na.rm = T)
		names(tabApply) <- gsub("^.....", "", names(tabApply))
		names(tabApply) <- gsub("-$", "", names(tabApply))
		
		
		tabella <- switch (input$vdom,
			"Prodotti" = tabApply,
			# "Numero di colonie",
			# "Mortalità invernale",
			# "Mortalità in stagione attiva",
			# "Dimensioni dell'apiario",
			"Perdite invernali accettabili" =  table(data[rselect, grepl("^....-20\\." , names(data))]),
			"Confronto perdite anno precedente" =  table(data[rselect, grepl("^....-21\\." , names(data))]),
			"Cause di mortalità in inverno" = tabApply,
			"Cause di mortalità in estate" = tabApply
		)
		dato <- switch( input$vdom,
										"Numero di colonie" = data[, grepl("^....-4\\." , names(data))],
										"Mortalità invernale" = data$cMinv,
										"Mortalità in stagione attiva" = data$cMest,
										"Dimensioni dell'apiario" = data[, grepl("^....-17\\." , names(data))]
										)
		
		if(input$vdom == "Prodotti" | 
			 input$vdom == "Cause di mortalità in inverno" | 
			 input$vdom == "Cause di mortalità in estate" | 
			 input$vdom == "Perdite invernali accettabili" |
			 input$vdom == "Confronto perdite anno precedente"
			 ){
			par(mar = c(8.1, 4.1, 1, 2.1))
			barplot(tabella, col = rcoloreda, las = 2)
		}else{
			hist(dato[rselect], ylab = "Frequenza", xlab = "", main = "", col = rcoloreda)
		}
		

	})		

	
	
	# boxplot ----
	output$bbplot <- renderPlot({
		mortal <- switch(
			input$vmor,
			"Mortalità in stagione attiva" = data$cMest,
			"Mortalità invernale" = data$cMinv
		)
		datoh <- switch(
			input$vdomsp,
			"Impiego di acido ossalico gocciolato" = 
				data[ , which(grepl("^....-60.1 Ha utilizzato prodotti a base di Acido Ossalico - gocciolati" , 
														names(data)))[1]] == "Sì",
			"Impiego di acido ossalico sublimato" =
				data[ , which(grepl("^....-60.1 Ha utilizzato prodotti a base di Acido Ossalico - sublimato", 
														names(data)))[1]] == "Sì",
			"Impiego di acido formico" = 					data[ , which(grepl("^....-60.1 Ha utilizzato prodotti a base di Acido Formico", 
																														names(data)))[1]] == "Sì",
			"Ipm: asportazione di covata da fuco" = data[ , which(grepl("^....-63.1 TRA", 
																																	names(data)))[1]] == "Sì",
			"Ipm: Asportazione totale di covata" = data[ , which(grepl("^....-63.3", 
																																 names(data)))[1]] == "Sì",
			"Ipm: asportazione parziale della covata per la creazione di nuclei" = data[ , which(grepl("^....-63.4", 
																																																 names(data)))[1]] == "Sì", 
			"Ingabbiamento estivo della ape regina" = cut(data[ , which(grepl("^....-63.5", 
																																				names(data)))[1]], breaks = c(0,33,66,100)), 
			# "Ingabbiamento invernale della ape regina" = cut(data[ , which(grepl("^....-63.6", 
			# 																																		 names(data)))[1]], breaks = c(0,33,66,100)),
			"Nutrizione: sciroppo zuccherino" = (data[ , which(grepl(" ha somministrato sciroppo zuccherino industriale ad alcune o a tutte le sue colonie di api\\?", 
																															 names(data)))[1]] == "Sì" |
																					 	data[ , which(grepl(" ha somministrato polline e sciroppo ad alcune o a tutte le sue colonie di api\\?", 
																					 											names(data)))[1]] == "Sì" |
																					 	data[ , which(grepl("sciroppo zuccherino artigianale \\(glucosio, saccarosio ed acqua", 
																					 											names(data)))[1]] == "Sì" ),
			"Nutrizione: candito zuccherino" = (
				data[ , which(grepl("candito zuccherino ad alcune o a tutte le sue colonie di api", 
														names(data)))[1]] == "Sì" )
			
		)
		rcoloreda2 <- switch(
			# colore cambia secondo dimensione apicoltore
			input$vtar2,
			"Tutte le aziende" = coloriDimApic[4],
			"Az. < 100 alveari" = coloriDimApic[3],
			"Az. 100-300 alveari" = coloriDimApic[2],
			"As. > 300 alveari" = coloriDimApic[1]
		)
		rselect2 <- switch(
			input$vtar2,
			"Tutte le aziende" = all3,
			"Az. < 100 alveari" = a100,
			"Az. 100-300 alveari" = a130,
			"As. > 300 alveari" = a300
		)
		
			boxplot(mortal[rselect2]~datoh[rselect2], 
							xlab = "", 
							ylab = "mortalità (%)", 
							col= rcoloreda2,
							varwidth = T)


})
	output$nptesto <- renderPrint({
		
		mortal <- switch(
			input$vmor,
			"Mortalità in stagione attiva" = data$cMest,
			"Mortalità invernale" = data$cMinv
		)
		datoh <- switch(
			input$vdomsp,
			"Impiego di acido ossalico gocciolato" = 
				data[ , which(grepl("^....-60.1 Ha utilizzato prodotti a base di Acido Ossalico - gocciolati" , 
														names(data)))[1]] == "Sì",
			"Impiego di acido ossalico sublimato" =
				data[ , which(grepl("^....-60.1 Ha utilizzato prodotti a base di Acido Ossalico - sublimato", 
														names(data)))[1]] == "Sì",
			"Impiego di acido formico" = 					data[ , which(grepl("^....-60.1 Ha utilizzato prodotti a base di Acido Formico", 
																														names(data)))[1]] == "Sì",
			"Ipm: asportazione di covata da fuco" = data[ , which(grepl("^....-63.1 TRA", 
																																	names(data)))[1]] == "Sì",
			"Ipm: Asportazione totale di covata" = cut(data[ , which(grepl("^....-63.3", 
																																 names(data)))[1]], breaks = c(0,33,66,100)), 
			"Ipm: asportazione parziale della covata per la creazione di nuclei" = cut(data[ , which(grepl("^....-63.4", 
																																																 names(data)))[1]], breaks = c(0,33,66,100)),  
			"Ingabbiamento estivo della ape regina" = cut(data[ , which(grepl("^....-63.5", 
																																				names(data)))[1]], breaks = c(0,33,66,100)), 
			# "Ingabbiamento invernale della ape regina" = cut(data[ , which(grepl("^....-63.6", 
			# 																																		 names(data)))[1]], breaks = c(0,33,66,100)),
			"Nutrizione: sciroppo zuccherino" = (data[ , which(grepl(" ha somministrato sciroppo zuccherino industriale ad alcune o a tutte le sue colonie di api\\?", 
																															 names(data)))[1]] == "Sì" |
																					 	data[ , which(grepl(" ha somministrato polline e sciroppo ad alcune o a tutte le sue colonie di api\\?", 
																					 											names(data)))[1]] == "Sì" |
																					 	data[ , which(grepl("sciroppo zuccherino artigianale \\(glucosio, saccarosio ed acqua", 
																					 											names(data)))[1]] == "Sì" ),
			"Nutrizione: candito zuccherino" = (
				data[ , which(grepl("candito zuccherino ad alcune o a tutte le sue colonie di api", 
														names(data)))[1]] == "Sì" )
			
		)
		rcoloreda2 <- switch(
			# colore cambia secondo dimensione apicoltore
			input$vtar2,
			"Tutte le aziende" = coloriDimApic[4],
			"Az. < 100 alveari" = coloriDimApic[3],
			"Az. 100-300 alveari" = coloriDimApic[2],
			"As. > 300 alveari" = coloriDimApic[1]
		)
		rselect2 <- switch(
			input$vtar2,
			"Tutte le aziende" = all3,
			"Az. < 100 alveari" = a100,
			"Az. 100-300 alveari" = a130,
			"As. > 300 alveari" = a300
		)
		
		if(length(levels(as.factor(datoh[rselect2]))) < 3){# mann-withney solo se 2 livelli
			wilcox.test(mortal[rselect2]~datoh[rselect2])}else{
			
		}
		
	})

}

# Run the application ----
shinyApp(ui = ui, server = server)