# pezzi di codice che potrebbero tornare utili

#
#
#
#
output$debugText1 <- renderText({
	paste(
		input$genSelect,
		which(lsgenerali$choices == input$genSelect),
		lsgenerali$inputFiles[which(lsgenerali$choices == input$genSelect)],
		paste0("int/", lsgenerali$inputFiles[which(lsgenerali$choices == input$genSelect)])
	)
})# debug

# reactive({
# 	source(paste0("int/", lsgenerali$inputFiles[which(lsgenerali$choices == input$genSelect)]), verbose = T) ### sperimentale
# })

