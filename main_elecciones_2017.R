#main_elecciones_2017
#Especiales agradecimientos a Javier Sajuria y Alvaro Fuenzalida
library(ggplot2)
library(scales)

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
tabla_electos_df$Color[tabla_electos_df$Partido =="LIBER."]<-"green"
tabla_electos_df$Color[tabla_electos_df$Partido =="RD"]<-"green"
tabla_electos_df$Color[tabla_electos_df$Partido =="PH"]<-"green"
tabla_electos_df$Color[tabla_electos_df$Partido =="PRO"]<-"brown1"
tabla_electos_df$Color[tabla_electos_df$Partido =="AMPLI."]<-"darkgoldenrod1"
tabla_electos_df$Color[tabla_electos_df$Partido =="PAIS"]<-"brown1"

tabla_electos_df$Orient[tabla_electos_df$Partido =="UDI"]<-10
tabla_electos_df$Orient[tabla_electos_df$Partido =="RN"]<-8
tabla_electos_df$Orient[tabla_electos_df$Partido =="EVOP."]<-7
tabla_electos_df$Orient[tabla_electos_df$Partido =="PDC"]<-5
tabla_electos_df$Orient[tabla_electos_df$Partido =="PSCH"]<-3
tabla_electos_df$Orient[tabla_electos_df$Partido =="PCCH"]<-2
tabla_electos_df$Orient[tabla_electos_df$Partido =="PPD"]<-3
tabla_electos_df$Orient[tabla_electos_df$Partido =="PRSD"]<-3
tabla_electos_df$Orient[tabla_electos_df$Partido =="CIUD."]<-6
tabla_electos_df$Orient[tabla_electos_df$Partido =="LIBER."]<-2
tabla_electos_df$Orient[tabla_electos_df$Partido =="RD"]<-1
tabla_electos_df$Orient[tabla_electos_df$Partido =="PH"]<-1
tabla_electos_df$Orient[tabla_electos_df$Partido =="PRO"]<-2
tabla_electos_df$Orient[tabla_electos_df$Partido =="AMPLI."]<-6
tabla_electos_df$Orient[tabla_electos_df$Partido =="PAIS"]<-2
tabla_electos_df<- tabla_electos_df[order(tabla_electos_df$Orient),]
tabla_Coa=table(electos$Coalicion)
tabla_Coa_df=as.data.frame(tabla_Coa)
colnames(tabla_Coa_df)=c("Coalicion","Asientos")
tabla_Coa_df$Coalicion2[tabla_Coa_df$Coalicion=="P"]<-"Chile Vamos"
tabla_Coa_df$Coalicion2[tabla_Coa_df$Coalicion=="N"]<-"Nueva Mayoria"
tabla_Coa_df$Coalicion2[tabla_Coa_df$Coalicion=="O"]<-"Conv. Democratica"
tabla_Coa_df$Coalicion2[tabla_Coa_df$Coalicion=="G"]<-"Frente Amplio"
tabla_Coa_df$Coalicion2[tabla_Coa_df$Coalicion=="H"]<-"Sumemos"
tabla_Coa_df$Coalicion2[tabla_Coa_df$Coalicion=="K"]<-"Regionalista Verde"
tabla_Coa_df$Coalicion2[tabla_Coa_df$Coalicion=="B"]<-"Progresistas"
tabla_Coa_df
tabla_electos_df
source("plotter_elec.R")
layout = seats(sum(tabla_electos_df$Asientos),5)
result = election(layout, tabla_electos_df$Asientos) # no overall majority!!!
plot(result$x, result$y, 
    col=tabla_electos_df$Color[result$party], #numeric index
    pch=19, asp=1, 
     frame.plot=FALSE, # gets rid of the surrounding rectangle
    axes="F",xlab="",ylab="",main="Cámara de Diputados 2018-2022")

#library(SciencesPo)
#asientos<-c(3,3,5,5,7,8,8,8,7,8,6,7,5,6,5,4,7,4,5,8,5,4,7,5,4,5,3,3)


#primario<-dHondt(parties=diputados$Coalicion, votes=diputados$Votos, seats=asientos[b])
#primario_df <- data.frame(Party=primario$Party,Seats=primario$Seats)



#	library("googlesheets")
 # Test<- register_ss("https://docs.google.com/spreadsheets/d/1OGN7vZHBcMXZrFJn_qWO8t99ZZIG8B8NxD2WCEuoZY4/edit#gid=0")

source("database_presidente.R")
presidente=database_presidente()
ggplot(presidente, aes(x=Candidato, y=Votos,fill=Color)) +
  geom_bar(stat="identity")+theme_minimal()+coord_flip()+theme(legend.position="none")+
geom_text(aes(label = paste0(percentage,"%"),y=Votos*(2/3)),size = 4)
presidente


source("plotter_elec.R")
layout = seats(153,8)
result = election(layout, tabla_electos_df$Asientos)#VoteGermany2013$Result) # no overall majority!!!
plot(result$x, result$y, 
    col=tabla_electos_df$Color[result$party], #numeric index
    pch=19, asp=1, 
     frame.plot=FALSE, # gets rid of the surrounding rectangle
    axes="F",
    xlab="",ylab="",main="Cámara de Diputados 2018-2022",sub="www.17-56.cl")
legend(FILL=tabla_electos_df$Color,tabla_electos_df$Partido)


 
