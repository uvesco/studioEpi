library(readxl)
province <- read_excel("~/Documenti/importanza_1_sync/unaapi/questionario_BIP_UNAAPI/Elenco-comuni-italiani.xls")[, c(3,11,12)]
province <- unique(province)
province[province[,2] == "-", 2] <- ""
province[province[,3] == "-", 3] <- ""
province <- as.data.frame(province)
province$nome <- paste0(province[,2], province[,3])
province$completo <- paste0(province$`Codice Provincia (1)`, " | ", province$nome)
write.table(province$completo, "province.csv", quote=F, col.names = F, row.names = F)
