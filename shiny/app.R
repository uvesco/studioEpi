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
				)#,
			),
			mainPanel()
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
				input$vquest,
				
			)


})

}

# Run the application ----
shinyApp(ui = ui, server = server)