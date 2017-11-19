database_presidente <- function(){

library(jsonlite)
library(dplyr)
setwd("C:/Users/Gonzalo/Desktop/R-json")

P01 <- "http://servelelecciones.cl/data/elecciones_presidente/computo/global/19001.json"

presidencial=fromJSON(P01)

stat_presidencial = data.frame(Candidato=presidencial$data$a,Votos=strtoi(presidencial$data$c))

stat_presidencial$Votos=lala #por mientras
total=sum(stat_presidencial$Votos)

stat_presidencial$percentage[1] = stat_presidencial$Votos[1]/total
stat_presidencial$percentage[2] = stat_presidencial$Votos[2]/total
stat_presidencial$percentage[3] = stat_presidencial$Votos[3]/total
stat_presidencial$percentage[4] = stat_presidencial$Votos[4]/total
stat_presidencial$percentage[5] = stat_presidencial$Votos[5]/total
stat_presidencial$percentage[6] = stat_presidencial$Votos[6]/total
stat_presidencial$percentage[7] = stat_presidencial$Votos[7]/total
stat_presidencial$percentage[8] = stat_presidencial$Votos[8]/total

stat_presidencial$percentage=stat_presidencial$percentage*100

return(stat_presidencial)
}
