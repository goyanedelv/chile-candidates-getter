json_writer <- function(data_all, distrito){

setwd("~/Developer/RWorkspace/chile-elections-2017")

library(rjson)

data_distrito_filtered<- subset(data_all,Distrito==distrito)


colnames(data_distrito_filtered) <- c("name","party","votes","percentage2","electo_num","district","list","party2","num","code","percentage","elected")


jsonized<-toJSON(setNames(data_distrito_filtered, colnames(data_distrito_filtered)))
filename<-paste("diputados/candidato",distrito,".json",sep="")



write(jsonized, filename)


	return()
}