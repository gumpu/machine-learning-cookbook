require(xtable)
load("../../RawData/Owls/dataset.rdata")

sniplet <- xtable(summary(dataset))


fileConn<-file("raw_stats.tex")
writeLines(print(sniplet), fileConn)
close(fileConn)


