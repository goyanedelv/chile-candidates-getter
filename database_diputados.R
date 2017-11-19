database_diputados <- function(){


library(jsonlite)
library(dplyr)
setwd("C:/Users/Gonzalo/Desktop/R-json")
#jsonedit(listdata = data$data$sd[5], mode = "tree")
#Distritos
D01 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6001.json"
D02 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6002.json"
D03 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6003.json"
D04 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6004.json"
D05 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6005.json"
D06 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6006.json"
D07 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6007.json"
D08 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6008.json"
D09 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6009.json"
D10 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6010.json"
D11 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6011.json"
D12 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6012.json"
D13 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6013.json"
D14 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6014.json"
D15 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6015.json"
D16 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6016.json"
D17 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6017.json"
D18 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6018.json"
D19 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6019.json"
D20 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6020.json"
D21 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6021.json"
D22 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6022.json"
D23 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6023.json"
D24 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6024.json"
D25 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6025.json"
D26 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6026.json"
D27 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6027.json"
D28 <- "http://servelelecciones.cl/data/elecciones_diputados/computo/distritos/6028.json"

#Circunscripciones
C01 <- "http://servelelecciones.cl/data/elecciones_senadores/computo/circ_senatorial/5001.json"
C02 <- "http://servelelecciones.cl/data/elecciones_senadores/computo/circ_senatorial/5002.json"
C04 <- "http://servelelecciones.cl/data/elecciones_senadores/computo/circ_senatorial/5004.json"
C06 <- "http://servelelecciones.cl/data/elecciones_senadores/computo/circ_senatorial/5006.json"
C09 <- "http://servelelecciones.cl/data/elecciones_senadores/computo/circ_senatorial/5009.json"
C11 <- "http://servelelecciones.cl/data/elecciones_senadores/computo/circ_senatorial/5011.json"
C14 <- "http://servelelecciones.cl/data/elecciones_senadores/computo/circ_senatorial/5014.json"

#Presidencial
P01 <- "http://servelelecciones.cl/data/elecciones_presidente/computo/global/19001.json"
path_diputados=c(D01,D02,D03,D04,D05,D06,D07,D08,D09,D10,D11,D12,D13,D14,D15,D16,D17,D18,D19,D20,
	D21,D22,D23,D24,D25,D26,D27,D28)

data_distritos <-list()
for (g in 1:28){
	data_distritos[[g]]<- fromJSON(path_diputados[g])
}

#data <- fromJSON(D1)

#sd1;	(1) Independientes
#		(2) es Frente Amplio
#		(3) es Fuerza de la Mayoría
#		(4) es Convergencia Democrática
#		(5) es Chile Vamos
#sd2; 	Partidos dentro de pactos.

lista <- list()
k <- 1
for (f in 1:28){
	larguito=nrow(data_distritos[[f]]$data)
	for (j in 1:larguito) {

		largo=nrow(data_distritos[[f]]$data$sd[[j]])

		for (i in 1:largo) {

			 lista[[k]]<-cbind((data_distritos[[f]]$data$sd[[j]]$sd[[i]]),f)
			 k<-k+1
		}
		
		hidden=nrow(data_distritos[[f]]$data)
		if(is.null(nrow(data_distritos[[f]]$data$sd[[hidden]]$sd[[1]]))){

			 lista[[k]]<-cbind((data_distritos[[f]]$data$sd[[hidden]]),f)
			 k<-k+1

		}
	}
}

lista_desarmada <- plyr::ldply(lista, data.frame)##Desarmar la lista
colnames(lista_desarmada) <- c("Candidato", "Partido", "Votos", "Porcentaje", "Candidatos", "Electo","a","Distrito","b")
lista_desarmada <- unique(lista_desarmada)
lista_desarmada <- lista_desarmada[!is.na(lista_desarmada$Candidato),]


lista_desarmada$a=NULL
lista_desarmada$b=NULL
lista_desarmada$Candidatos=NULL

lista_desarmada$Coalicion[lista_desarmada$Partido=="PPD"]<-"N"
lista_desarmada$Coalicion[lista_desarmada$Partido=="PCCH"]<-"N"
lista_desarmada$Coalicion[lista_desarmada$Partido=="PSCH"]<-"N"
lista_desarmada$Coalicion[lista_desarmada$Partido=="PRSD"]<-"N"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-PPD"]<-"N"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-PCCH"]<-"N"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-PSCH"]<-"N"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-PRSD"]<-"N"

lista_desarmada$Coalicion[lista_desarmada$Partido=="UDI"]<-"P"
lista_desarmada$Coalicion[lista_desarmada$Partido=="RN"]<-"P"
lista_desarmada$Coalicion[lista_desarmada$Partido=="PRI"]<-"P"
lista_desarmada$Coalicion[lista_desarmada$Partido=="EVOP."]<-"P"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-UDI"]<-"P"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-RN"]<-"P"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-PRI"]<-"P"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-EVOP."]<-"P"

lista_desarmada$Coalicion[lista_desarmada$Partido=="RD"]<-"G"
lista_desarmada$Coalicion[lista_desarmada$Partido=="PH"]<-"G"
lista_desarmada$Coalicion[lista_desarmada$Partido=="PEV"]<-"G"
lista_desarmada$Coalicion[lista_desarmada$Partido=="LIBER."]<-"G"
lista_desarmada$Coalicion[lista_desarmada$Partido=="PODER"]<-"G"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IGUAL."]<-"G"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-RD"]<-"G"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-PH"]<-"G"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-PEV"]<-"G"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-LIBER."]<-"G"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-PODER"]<-"G"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-IGUAL."]<-"G"

lista_desarmada$Coalicion[lista_desarmada$Partido=="AMPLI."]<-"H"
lista_desarmada$Coalicion[lista_desarmada$Partido=="TODOS"]<-"H"
lista_desarmada$Coalicion[lista_desarmada$Partido=="CIUD."]<-"H"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-AMPLI."]<-"H"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-TODOS"]<-"H"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-CIUD."]<-"H"

lista_desarmada$Coalicion[lista_desarmada$Partido=="FRVS"]<-"K"
lista_desarmada$Coalicion[lista_desarmada$Partido=="DRP"]<-"K"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-FRVS"]<-"K"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-DRP"]<-"K"

lista_desarmada$Coalicion[lista_desarmada$Partido=="UPA"]<-"M"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-UPA"]<-"M"

lista_desarmada$Coalicion[lista_desarmada$Partido=="PDC"]<-"O"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-PDC"]<-"O"
lista_desarmada$Coalicion[lista_desarmada$Partido=="MAS"]<-"O"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-MAS"]<-"O"
lista_desarmada$Coalicion[lista_desarmada$Partido=="ICCH"]<-"O"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-ICCH"]<-"O"

lista_desarmada$Coalicion[lista_desarmada$Partido=="PRO"]<-"B"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-PRO"]<-"B"
lista_desarmada$Coalicion[lista_desarmada$Partido=="PAIS"]<-"B"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-PAIS"]<-"B"

lista_desarmada$Coalicion[lista_desarmada$Partido=="PTR"]<-"D"
lista_desarmada$Coalicion[lista_desarmada$Partido=="IND-PTR"]<-"D"

#Subpacto
lista_desarmada$Subpacto[lista_desarmada$Partido=="PPD"]<-"PPD"
lista_desarmada$Subpacto[lista_desarmada$Partido=="PCCH"]<-"PCCH"
lista_desarmada$Subpacto[lista_desarmada$Partido=="PSCH"]<-"PSCH"
lista_desarmada$Subpacto[lista_desarmada$Partido=="PRSD"]<-"PRSD"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-PPD"]<-"PPD"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-PCCH"]<-"PCCH"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-PSCH"]<-"PSCH"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-PRSD"]<-"PRSD"

lista_desarmada$Subpacto[lista_desarmada$Partido=="UDI"]<-"UDI"
lista_desarmada$Subpacto[lista_desarmada$Partido=="RN"]<-"RN"
lista_desarmada$Subpacto[lista_desarmada$Partido=="PRI"]<-"PRI"
lista_desarmada$Subpacto[lista_desarmada$Partido=="EVOP."]<-"EVOP."
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-UDI"]<-"UDI"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-RN"]<-"RN"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-PRI"]<-"PRI"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-EVOP."]<-"EVOP."

lista_desarmada$Subpacto[lista_desarmada$Partido=="RD"]<-"RD"
lista_desarmada$Subpacto[lista_desarmada$Partido=="PH"]<-"PH"
lista_desarmada$Subpacto[lista_desarmada$Partido=="PEV"]<-"PEV"
lista_desarmada$Subpacto[lista_desarmada$Partido=="LIBER."]<-"LIBER."
lista_desarmada$Subpacto[lista_desarmada$Partido=="PODER"]<-"PODER"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IGUAL."]<-"IGUAL."
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-RD"]<-"RD"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-PH"]<-"PH"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-PEV"]<-"PEV"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-LIBER."]<-"LIBER."
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-PODER"]<-"PODER"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-IGUAL."]<-"IGUAL."

lista_desarmada$Subpacto[lista_desarmada$Partido=="AMPLI."]<-"AMPLI."
lista_desarmada$Subpacto[lista_desarmada$Partido=="TODOS"]<-"TODOS"
lista_desarmada$Subpacto[lista_desarmada$Partido=="CIUD."]<-"CIUD."
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-AMPLI."]<-"AMPLI."
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-TODOS"]<-"TODOS"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-CIUD."]<-"CIUD."

lista_desarmada$Subpacto[lista_desarmada$Partido=="FRVS"]<-"FRVS"
lista_desarmada$Subpacto[lista_desarmada$Partido=="DRP"]<-"DRP"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-FRVS"]<-"FRVS"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-DRP"]<-"DRP"

lista_desarmada$Subpacto[lista_desarmada$Partido=="UPA"]<-"UPA"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-UPA"]<-"UPA"

lista_desarmada$Subpacto[lista_desarmada$Partido=="PDC"]<-"PDC"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-PDC"]<-"PDC"
lista_desarmada$Subpacto[lista_desarmada$Partido=="MAS"]<-"MAS"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-MAS"]<-"MAS"
lista_desarmada$Subpacto[lista_desarmada$Partido=="ICCH"]<-"ICCH"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-ICCH"]<-"ICCH"

lista_desarmada$Subpacto[lista_desarmada$Partido=="PRO"]<-"PRO"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-PRO"]<-"PRO"
lista_desarmada$Subpacto[lista_desarmada$Partido=="PAIS"]<-"PAIS"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-PAIS"]<-"PAIS"

lista_desarmada$Subpacto[lista_desarmada$Partido=="PTR"]<-"PTR"
lista_desarmada$Subpacto[lista_desarmada$Partido=="IND-PTR"]<-"PTR"

lista_desarmada$Numero<-substr(lista_desarmada$Candidato, 1, 2)

lista_desarmada$Coalicion[is.na(lista_desarmada$Coalicion)]<-""

lista_desarmada$Codigo<-paste0(lista_desarmada$Coalicion,lista_desarmada$Numero,sep="")


asientos<-c(3,3,5,5,7,8,8,8,7,8,6,7,5,6,5,4,7,4,5,8,5,4,7,5,4,5,3,3)


source("dhondt_Chile.R")

#####solo para modelación
lista_desarmada$Votos=round(rnorm(nrow(lista_desarmada),1000,100),digits=0)
#####

winners <- list()
q<-1
for(y in 1:28){

	winners[[q]]<-dhondt_Chile(lista_desarmada,y,asientos[y])[2]
	q<-q+1
}

winners_df<-plyr::ldply(winners, data.frame)

lista_desarmada$Candidato<-as.factor(lista_desarmada$Candidato)

for (z in 1:959){
	if(is.element(lista_desarmada$Candidato[z],winners_df$Ganadores)){
		lista_desarmada$Electo[z]<-1
	}	
}

lista_desarmada$Electo<-as.integer(lista_desarmada$Electo)
lista_desarmada$Electo[is.na(lista_desarmada$Electo)]<-0
lista_desarmada$elected[lista_desarmada$Electo==1]<-TRUE
lista_desarmada$elected[lista_desarmada$Electo==0]<-FALSE



source("percentager.R")
for(h in 1:959){
	lista_desarmada$percentage[h]<-percentager(lista_desarmada$Votos[h],lista_desarmada,lista_desarmada$Distrito[h])

}

##check que funciono, tiene q dar 100: sum(lista_desarmada$percentage[lista_desarmada$Distrito==11])

return(lista_desarmada)


}


#write.csv(lista_desarmada,"prueba.csv")
