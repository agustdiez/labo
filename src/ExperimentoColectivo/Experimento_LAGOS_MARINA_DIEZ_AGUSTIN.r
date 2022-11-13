# Experimento colectivo
## Integrantes:
### Marina Lagos
### Agustín Diez

# Experimento a resolver
## Traer el semillerío con sus predicciones y hacer un promedio ponderado con otro modelo (Ej. Regresión logística)

## Lectura de dataset a entrenar. Se adopta el ganador de la competencia #3



# Semillerio

#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU
require("data.table")

# Importo archivos .csv de la salida de Tomas Delvechio de 90 semillas

setwd( "D:\\OneDrive\\Personal\\Maestria Data Mining\\economia_finanzas\\labo\\src\\ExperimentoColectivo" )

files_csv_lbgm = list.files(path = "./LGBM", pattern = '.csv', all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# Genero el ID y la foto del mes de un archivo al azar, para luego generar la columna de prob y de rank

dataset_input  <- paste0("./LGBM/ZZ9410_semillerio_841727_resultados.csv" )
dataset  <- fread( dataset_input )

dataset[,c('prob','rank'):=NULL]

#Comienzo a iterar por archivo y agregar columnas

semillas <- 1:90

for( i in semillas)
{
  archivo <- files_csv_lbgm[i]
  dataset_semilla  <- fread(paste0("./LGBM/", archivo ))
  probs <- dataset_semilla[ , .SDcols = 3:4]
  cols_extraer <- names(dataset_semilla)[3:4]
  setnames(probs,cols_extraer,paste0(cols_extraer,i))
  probs[,c('numero_de_cliente','foto_mes'):=NULL]
  dataset <- cbind(dataset, probs)
  
}

nom_pred  <- paste0( "prediccion_LGBM",
                      ".csv"  )

fwrite( dataset,
          file= nom_pred,
          sep= "\t" )


### Mezclo todo (LGBM,logistica y real)




