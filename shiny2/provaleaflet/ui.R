library(shiny)
library(shinythemes)
library(leaflet)

fluidPage(theme = shinytheme("cerulean"),
					tags$head(
						# Include our custom CSS
						#includeCSS("style.css")
						
					),
					navbarPage("Crime DC",id='nav',
										 tabPanel("Interactive Map",
										 				 fluidRow(
										 				 	column(4,plotOutput("plotOffense",height=300)),
										 				 	column(4,plotOutput("plotDay",height=300)),
										 				 	column(4,plotOutput("plotShift",height=300))
										 				 ),
										 				 leafletOutput("mymap", width = '100%', height = '600px'),
										 				 br(),
										 				 absolutePanel(id = "controls",class = "panel panel-default", fixed = TRUE, draggable = TRUE,
										 				 							top = 500, left = "auto", right = 20, bottom = "auto",
										 				 							width = 330, height = "auto", style = "opacity: .75",
										 				 							
										 				 							h2("Crime DC - Last 30 Days"),
										 				 							paste("Click on cluster or scroll to zoom-in, Click an individual marker for additional detail popup."),
										 				 							a("data source: http://opendata.dc.gov/datasets",href="http://opendata.dc.gov/datasets"),
										 				 							br(),
										 				 							a("author: neil kutty", href="http:/twitter.com/neilkutty"),
										 				 							a("github",href="https://github.com/sampsonsimpson/DC_Crime_Data")
										 				 							
										 				 							
										 				 )
										 ),
										 tabPanel("Data Explorer",
										 				 dataTableOutput("table1")
										 )
					)
)