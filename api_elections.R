# Desarrollado por Juan Pablo Hurtado, 2017 para 17-56.cl
#
# Archivo que corre una API que permite hacer consultas a los datos recolectados
#
# Falto mucho por hacer, ademas de los llamados a la API, faltaron utilizar las
# estructuras de dato correctas.
#

library(jsonlite)
source("database_diputados.R")

TOTAL_DISTRITOS = 28
DATA_NUEVOSISTEMA <- read.csv(file="nuevosistema.csv", header=TRUE, sep=",",
                              fileEncoding="ISO-8859-1")

# Output:
# Mensaje de bienvenida. (Esto en realidad debiera mostrar los candidatos
# presidenciales).
#
#* @get /
index <- function() {
    print("API de 17-56 para las elecciones de Chile 2017")
}

# Get Districts
#
# Recibe como parametro un "code"
# /districts?code=<code>
#
# Si es que recibe un codigo > que el TOTAL_DISTRITOS,
# imprime un mensaje de error.
# 
# Si es que no recibe nada (o 0) imprime un mensaje de error.
# Esto hay que cambiarlo para que retorne los candidatos a diputados.
#
# Una mejora para hacer es cargar la base de datos de diputados de una manera
# mas inteligente y eficiente.
#
# Output JSON:
#
#{
#  code: <Number>,
#  communes: [<String>],
#  candidates: [<Candidate>],
#}
#
#* @get /districts
getDistricts <- function(code = 0) {

    if (code > TOTAL_DISTRITOS) {

        paste("Codigo de distrito tiene que ser menor que ", TOTAL_DISTRITOS)

    } else if (code <= 0) {

        print("Codigo de distrito no puede ser igual o menor que 0")

    } else {

        db_diputados = database_diputados()

        subsetPorDistrito = subset(DATA_NUEVOSISTEMA, Distrito == code)
        diputadosPorDistrito = subset(db_diputados, Distrito == code)

        distrito = list(code = code,
                        communes = subsetPorDistrito[['Comuna']],
                        candidates = diputadosPorDistrito[['Candidato']])

        jsonDistrito = toJSON(distrito, auto_unbox = TRUE)

        return(jsonDistrito)

    }

}
