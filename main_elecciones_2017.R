#main_elecciones_2017
#Especiales agradecimientos a Javier Sajuria y Alvaro Fuenzalida


setwd("C:/Users/Gonzalo/Desktop/R-json")

source("database_diputados.R")
diputados=database_diputados()

electos=subset(diputados,Electo==1)
electos$Subpacto=as.factor(electos$Subpacto)

tabla_electos=table(electos$Subpacto)
tabla_electos_df=as.data.frame(tabla_electos)
colnames(tabla_electos_df)=c("Partido","Asientos")

tabla_electos_df$Color[tabla_electos_df$Partido =="UDI"]<-"blue4"
tabla_electos_df$Color[tabla_electos_df$Partido =="RN"]<-"blue2"
tabla_electos_df$Color[tabla_electos_df$Partido =="EVOP."]<-"deepskyblue1"
tabla_electos_df$Color[tabla_electos_df$Partido =="PDC"]<-"gold3"

tabla_electos_df$Color[tabla_electos_df$Partido =="PSCH"]<-"darkorange4"
tabla_electos_df$Color[tabla_electos_df$Partido =="PCCH"]<-"brown3"
tabla_electos_df$Color[tabla_electos_df$Partido =="PPD"]<-"darkorange1"
tabla_electos_df$Color[tabla_electos_df$Partido =="PRSD"]<-"darkorange"

tabla_electos_df$Color[tabla_electos_df$Partido =="CIUD."]<-"darkgoldenrod1"

tabla_electos_df$Color[tabla_electos_df$Partido =="LIBER."]<-"coral"

tabla_electos_df$Color[tabla_electos_df$Partido =="RD"]<-"brown4"
tabla_electos_df$Color[tabla_electos_df$Partido =="PH"]<-"brown2"
tabla_electos_df$Color[tabla_electos_df$Partido =="PRO"]<-"brown1"



#	library("googlesheets")
 # Test<- register_ss("https://docs.google.com/spreadsheets/d/1OGN7vZHBcMXZrFJn_qWO8t99ZZIG8B8NxD2WCEuoZY4/edit#gid=0")


source("database_presidente.R")
presidente=database_presidente()




source("plotter_elec.R")
layout = seats(154,5)
result = election(layout, tabla_electos_df$Asientos)#VoteGermany2013$Result) # no overall majority!!!
plot(result$x, result$y, 
    col=tabla_electos_df$Color[result$party], #numeric index
    pch=19, asp=1, 
     frame.plot=FALSE, # gets rid of the surrounding rectangle
    axes="F")

#plot(result$x, result$y, col=result$party,pch=19, asp=1)