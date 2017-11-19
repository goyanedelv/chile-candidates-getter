
dhondt_Chile <- function(data_all, distrito, asientos_p){

library(SciencesPo)
library(dplyr)

###Iteracion a Coalicion
data_primario<- subset(data_all,Distrito==distrito)
primario<-dHondt(parties=data_primario$Coalicion, votes=data_primario$Votos, seats=asientos_p)
primario_df <- data.frame(Party=primario$Party,Seats=primario$Seats)

###Iteracion a Subpacto
set_Coalicion<-unique(data_primario$Coalicion)
lista_dhondt_output <-list()
t<-1
for (x in 1:length(set_Coalicion)){

	if(primario_df$Seats[primario_df$Party==set_Coalicion[x]]>0){
		data_secundario<- subset(data_all,Distrito==distrito & Coalicion==set_Coalicion[x])
		secundario<-dHondt(parties=data_secundario$Subpacto, votes=data_secundario$Votos,
 					seats=primario_df$Seats[primario_df$Party==set_Coalicion[x]])
		#secundario_df <- data.frame(Party=secundario$Party,Seats=secundario$Seats)
		lista_dhondt_output[[t]] <-data.frame(Party=secundario$Party,Seats=secundario$Seats)#secundario_df
		t<-t+1
	}
}

###Asignación de asientos a nivel de partidos
distribucion_partidos<-plyr::ldply(lista_dhondt_output, data.frame)
distribucion_partidos <- distribucion_partidos[!distribucion_partidos$Seats==0,]


partidos_ganadores<- distribucion_partidos$Party
lista_ganadores<-list()
r<-1
for (w in 1:length(partidos_ganadores)){

	candidatos_electos <- subset(data_primario,Subpacto==partidos_ganadores[w])
	candidatos_electos <- candidatos_electos[order(-candidatos_electos$Votos),]
	candidatos_electos <- head(candidatos_electos,distribucion_partidos$Seats[distribucion_partidos$Party==partidos_ganadores[w]])
	lista_ganadores[[r]]<-candidatos_electos$Candidato
	r<-r+1
}

output_ganadores<-plyr::ldply(lista_ganadores, data.frame)
colnames(output_ganadores) <- c("Ganadores")


lista_output_final<- list()
lista_output_final[[1]] <- distribucion_partidos
lista_output_final[[2]] <- output_ganadores


return(lista_output_final)
}

#data_sub$Votos=round(rnorm(19,1000,100),digits=0)