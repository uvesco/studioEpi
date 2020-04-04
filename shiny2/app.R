# librerie ---
library(shiny)
library(shinythemes)
library(shiny.i18n)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(rgdal)
library(maptools)
library(htmlwidgets)
library(RCurl)

# nella versione definitiva impostare la traduzione di tutto, con bandierine da schiacciare

# importazione dati ----
data <-
	readRDS("data.RDS") # file creato dallo script di importazione dal CSV

# Define UI ----------


ui <- navbarPage(
	title = ("Questionario UNAAPI"),
	theme = shinytheme("united"),
	#shinythemes::themeSelector(),
	# tags$head(
	# 	## Include our custom CSS
	# 	includeCSS("www/preset3.css"),
	# 	includeCSS("www/bootstrap.css"),
	# 	includeCSS("www/template.css")
	# ),
	
	tabPanel("Home",
					 fluidRow(
					 	column(
					 		12,
					 		tags$p(
					 			"In questa sezione c'è un testo che illustra le finalità dell'iniziativa / come leggere i dati"
					 		),
					 		img(src = "img/crt-pau-max.jpg", width = "300px"),
					 		
					 		h1("Informazioni sui dati"),
					 		
					 		tags$p("Poi deve anche presentare i numeri:"),
					 		tags$br(),
					 		tags$ol(
					 			tags$li("numero di record presenti"),
					 			tags$li("numero di record per anno"),
					 			tags$li("versione del dataset"),
					 			tags$li("ultimo aggiornamento")
					 		)
					 	)
					 )),
	navbarMenu(
		"Aziende",
		tabPanel(
			"Distribuzione",
			#tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
			leafletOutput("map", height = "600px"),#, width = "100%", height = "100%")
		),
		tabPanel(
			"Distribuzione apiari",
			tags$p(
				" distribuzione aziende e apiari
				Eventuale Mappa leaflet https://rstudio.github.io/leaflet/shiny.html
						 			 x posizione degli apiari delle aziende intervistate mappa coropletica (https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/)"
			)
		),
		
		tabPanel("Numero di alveari",
						 tags$p("Istogramma")),
		tabPanel("Dimensione apiari",
						 p("istogramma")),
		tabPanel("Produzioni",
						 p("barplot"))
	),
	
	navbarMenu(
		"Mortalità",
		tabPanel(
			"Variazioni del patrimonio (nell'anno)",
			tags$p("Variazione del patrimonio apistico a fine stagione apistica"),
			br(),
			p("boxplot - slide 13 + apporto delle varie componenti (slide 12)")
		),
		tabPanel(
			"Numero di alveari",
			tags$p("Variazione del patrimonio apistico dall'autunno a primavera"),
			br(),
			p("boxplot - slide 14 + apporto delle varie componenti (slide 12)")
		),
		tabPanel(
			"Mortalità media",
			tags$p("Mortalità media (nidificare tabs?) slide 15 in boxplot")
		),
		tabPanel(
			"Fattori nella mortalità",
			p("istogramma - età, estate/inverno, slide 16 e 17"),
			br(),
			p(
				"cause di mortalità in estate e in inverno (separare sono correlazioni e convinzioni dell'apicoltore)"
			)
		),
		tabPanel(
			"Variazione delle perdite invernali (anno precedente)",
			p(
				"slide 18, barplot orizzontale, incrocio con % di perdita (vari boxplot?)"
			)
		),
		tabPanel("Mortalità accettabile",
						 p("slide 19, barplot verticale")),
		tabPanel(
			"Avvelenamenti",
			p(
				"x slide 22: 4 boxplot (uno per stagione sulla % di perdite dichiarate, larghezza proporzionale
						 				 	a numero di osservazioni)"
			),
			br(),
			p(
				"confronto della mortalità tra chi ha dichiarato avelenamenti e chi no"
			),
			br(),
			p(
				"x colture prossime ai fenomeni di avvelenamento (barplot con frequenze)"
			)
		)
		
	),
	
	navbarMenu(
		"Patologie",
		tabPanel(
			"Controlli diagnostici",
			tags$p(
				"    27.  Slide 46: Graficare la frequenza del numero di controlli per la varroa, nei diversi mesi dell’anno;
    28. Idem punto 27 per Nosema spp. e per patologie della covata (domande 56 e 58, rispettivamente) (in parte slide 51);"
			)
		),
		tabPanel(
			"Utilizzo dei trattamenti",
			tags$p(
				"    29.  Slide 47: Graficare la frequenza delle diverse metodiche di monitoraggio della varroa;
    30. Idem punto 29 per Nosema spp. e per patologie della covata (domande 57 e 59, rispettivamente);"
			)
		),
		tabPanel(
			"Acaricidi: periodo di utilizzo",
			tags$p(
				"radarplot Slide 48: Graficare le frequenze di principi attivi somministrati nei diversi mesi dell’anno, per la lotta alla varroa;"
			)
		),
		tabPanel(
			"Trattamenti: frequenza e diffusione",
			tags$p(
				"Slide 49: graficare le frequenze di trattamenti con i diversi principi attivi e la media del patrimonio apistico trattato; -> estrapolare % di alveari trattati?
						 				 da chiarire"
			)
		),
		tabPanel(
			"Lotta biomeccanica",
			tags$p(
				"Slide 50: graficare le frequenze di impiego delle diverse IPM e la % di colonie trattate. Riportare il dato del numero medio di asportazioni di covata da fuco/anno (?) "
			)
		),
		tabPanel(
			"Trattamenti e mortalità",
			tags$p(
				"Correlare la mortalità dei diversi principi attivi e tecniche biomeccaniche con la media degli alveari trattati (?)"
			)
		)
		
		
		
	),
	
	navbarMenu(
		"Alimentazione",
		tabPanel(
			"Finalità",
			"slide 29Il XX% degli apicoltori intervistati utilizza la nutrione
						 				 Perché nutrono? slide 30,
						 				 "
		),
		tabPanel(
			"Raccolti e carenze",
			tags$p(
				"slides 24-26 mesi di raccolta polline, mesi di carenza polline e mesi di carenza nettare (grafico a tela di ragno / radar)
						 				 			 provare a dividere per nord/centro/sud?"
			)
		),
		tabPanel(
			"Vantaggi e svantaggi",
			tags$p("slide 27 e 28, barplot vantaggi e svantaggi con switch")
		),
		tabPanel(
			"Modalità",
			tags$p(
				"slide 31 a radar cambiando la frequenza + slide 32 quante somministrazioni/anno per ogni tipo (boxplot?)"
			)
		),
		tabPanel(
			"Canditi zuccherini",
			p("slide 33 radar e boxplot quantità somministrata/anno")
		),
		tabPanel(
			"Sciroppo industriale",
			p("slide 35 radar e boxplot quantità somministrata/anno")
		),
		tabPanel(
			"Sciroppo autoprodotto",
			p("slide 36 radar e boxplot quantità somministrata/anno")
		),
		#inserire qui altri tipi di nutrizioni con lo stesso schema quando ci fossero, per adesso non attivati (se db relazionale
		#sarà tutto con lo stesso modulo)
		tabPanel("Variazione del consumo",
						 p("slide 37 barplot orizzontale")),
		tabPanel(
			"Alimentazione precoce",
			p(
				"slide 39 ??come possiamo fornire l’output della frequenza di	alimentazione precoce e la durata di tale “trattamento	alimentare”? e Correlare la mortalità con la alimentazione di supporto precoce ( diversificare la mortalità in base al numero di settimane di somministrazione)"
			)
		),
		
		tabPanel(
			"Costi",
			p(
				"Slide 40: graficare la frequenza di spesa che l’apicoltore ha per alimentare i suoi alveari (spesa per alveare/anno
						 				 	suddivisa in (se sei d’accordo) <5 euro/alveare/anno; tra 5 e 10 euro e superiore ai 10 euro. Realizzare un grafico
						 				 	anche per la spesa da pre/probiotici/integratori;"
			)
		),
		tabPanel(
			"Mortalità e alimentazione",
			p(
				"tutti i punti mortalità potrebbero essere unificati.
				Correlare la mortalità estiva con i diversi alimenti somministrati;
    17. Idem punto 15 relativo alla mortalità invernale; POTREBBE ESSERE INTERESSANTE OSSERVARE ANCHE LA MORTALITA’ CONNESSA AL NUMERO DIVERSO DI SOMMINISTRAZIONI ED AL NUMERO DI MESI IN CUI VIENE RIPETUTA LA ALIMENTAZIONE???"
			)
		),
		tabPanel(
			"Mortalità e scorte",
			p(
				"Graficare il numero medio di telaini di scorte negli alveari durante la stagione attiva e non e correlare la mortalità con tale numero medio (DOMANDE RISPETTIVAMENTE 39 E 40 DEL QUESTIONARIO);"
			)
		),
		tabPanel(
			"Mortalità e carenza",
			p(
				"AVEVAMO GIA’ APPUNTATO DI CORRELARE LA MORTALITA’ CON I MESI IN CUI CI FOSSE CARENZA DI POLLINE E CORRELARLA CON I MESI IN CUI SI SIA CARENZA DI NETTARE, GIUSTO? DOMANDE 42 E 43 DEL QUESTIONARIO;"
			)
		),
		tabPanel(
			"Mortalità e monitoraggio polline",
			p(
				"Correlare la mortalità con la tecnica di osservazione della raccolta trofica pollinica posizionando opportune trappole (domanda 44 e 45 per conoscere la percentuale di alveari monitorati rispetto al totale aziendale);"
			)
		),
		tabPanel(
			"Mortalità e uso datalogger",
			p(
				"dem punto 20 con posizionamento datalogger etc (per controllo raccolta nettarifera) (domanda 46 e 47).
						"
			)
		)
		
	),
	navbarMenu(
		"Nomadismo",
		tabPanel(
			"Finalità",
			p(
				"Slide 42: graficare le frequenze delle motivazioni del nomadismo- Barplot verticale, variabili da cambiare:"
			)
		),
		tabPanel("Distanze e ambienti",
						 p("slide 43")),
		tabPanel(
			"Mortalità e nomadismo",
			p(
				"    23. Correlare la mortalità (estiva? Tutta???) con il nomadismo (domanda 48);
    24. Correlare la mortalità invernale (o tutta) con il nomadismo invernale (domanda 49): boxplot e incroci"
			)
		),
		tabPanel(
			"Percezione della normativa",
			p(
				"Graficare la percezione degli apicoltori rispetto alla normativa vigente in merito alla pratica del nomadismo (domanda 52)."
			)
		)
		
		
	),
	
	navbarMenu("Pratiche apistiche",
						 tabPanel(
						 	"Età delle regine",
						 	p("slide 52-54 (boxplot) Mortalità e età delle regine")
						 ))
	
	
)


# server --------
server <- function(input, output, session) {

	province <- table(data[4])
	province <- substr(names(province), 1,3)
	province <- data.frame(COD_PRO = as.integer(province), num = as.vector(tabella))
	prov2008G <-readOGR("./leaflet/prov2008_s.shp")

	colori<-colorRamp(c("yellow", "red")) ## (x) , x in [0,1]
	prov2008G <- merge(prov2008G, province)
	
	cols <- brewer.pal(3, "YlOrRd")
	pal <- colorRampPalette(c("yellow", "red"))(10)
	prov2008G$cols[!is.na(prov2008G$num)] <- rgb(colorRamp(pal)(prov2008G$num[!is.na(prov2008G$num)]/10), max=255)
	
	geo_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # necessario trasformare in L/L WGS84
	prov2008G <- spTransform(prov2008G,geo_proj)

	records <- prov2008G$num
	records[is.na(records)] <- 0
	labels <- sprintf(
		"<strong>Provincia di %s:</strong><br/>%g record",
		prov2008G$NOME_PRO, records
	) %>% lapply(htmltools::HTML)
	
	output$map <- renderLeaflet({
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
		
	})

	
	
}

# Run the application ----
shinyApp(ui = ui, server = server)