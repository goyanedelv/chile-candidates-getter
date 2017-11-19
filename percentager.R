percentager <- function(x, data_all, distrito){


	calc<- subset(data_all,Distrito==distrito)


	percentage<-100*x/sum(calc$Votos)
	percentage<-round(percentage,digits=2)

		return(percentage)
}