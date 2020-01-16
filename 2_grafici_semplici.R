require(ggplot2)
require(hrbrthemes)

ggplot(data) +
	aes(x = `0007-4.	Numero di colonie vive al 1 aprile 2018`) +
	geom_histogram(bins = 30L, fill = "#fdc926") +
	labs(x = "Numero di colonie vive al 1 aprile 2018", y = "conteggio") +
	theme_ipsum()
