# Inicio

rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")

#Aqui se debe poner la carpeta de la computadora local
setwd("D:/economia_finanzas")  #Establezco el Working Directory


# Poner sus semillas
semillas <- c(732497,
              681979,
              281887,
              936659,
              692089)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022_fe.csv")
marzo <- dataset[foto_mes == 202103]



#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "KA7240_HIPERPARAMETROS_CURVA_PUBLICO"

PARAM$input$dataset       <- "./datasets/competencia2_2022_fe.csv"
PARAM$input$training      <- c( 202103 )
PARAM$input$future        <- c( 202105 )

PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-      0.00507
PARAM$finalmodel$num_iterations    <-    1000  
PARAM$finalmodel$num_leaves        <-   680  
PARAM$finalmodel$min_data_in_leaf  <-   1480  
PARAM$finalmodel$feature_fraction  <-      0.5785  
PARAM$finalmodel$semilla           <- 732497

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)


#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( paste0("./exp/", PARAM$experimento, "/" ) )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla
                                  )
                    )

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )

#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )


#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 7500, 9500, by=250 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]

  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  PARAM$experimento, "_", envios, ".csv" ),
          sep= "," )
}

#--------------------------------------

quit( save= "no" )

## EVALUACION DE ENVIOS PARA DETERMINAR CUANTOS ASIGNO COMPARANDO ENERO CON MARZO. Esto dará la pauta de "optimizar" la cantidad de envíos o ver en qué entorno es favorable


#Aqui se debe poner la carpeta de la computadora local
setwd("D:/economia_finanzas")  #Establezco el Working Directory

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022_fe.csv")
enero <- dataset[foto_mes == 202101]
marzo <- dataset[foto_mes == 202103]


PARAM$input$training      <- c( 202101 )
PARAM$input$future        <- c( 202103 )

#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)


#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------


#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]



# Armamos el dataset de train para LGBM
#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla
                                  )
                    )



# Prediccion sobre marzo

marzo$pred <- predict(modelo, data.matrix(marzo[, 1:154]))
sum((marzo$pred > 0.025) * ifelse(marzo$clase_ternaria == "BAJA+2", 78000, -2000))


length(marzo$pred)
length(unique(marzo$pred))

#Generacion de leaderboard

# Simulamos un Leaderboard público:
set.seed(semillas)
split <- caret::createDataPartition(marzo$clase_ternaria, p = 0.50, list = FALSE)

# Vemos la cantidad de casos que estaríamos mandando:clase_ternaria
sum(marzo$pred > 0.025) 

# Y obtendríamos una ganancia de
# Privado
sum((marzo$pred[split] > 0.025) * ifelse(marzo$clase_ternaria[split] == "BAJA+2", 78000, -2000)) / 0.5

# Público
sum((marzo$pred[-split] > 0.025) * ifelse(marzo$clase_ternaria[-split] == "BAJA+2", 78000, -2000)) / 0.5


# Ordenamos el dataset segun su probabilidad de forma ascendente
setorder(marzo, cols = -pred)

# PROBAR MULTIPLES VALORES
set.seed(semillas[3])
m <- 500
f <- 2000
t <- 12000

leaderboad <- data.table()
split <- caret::createDataPartition(marzo$clase_ternaria, p = 0.50, list = FALSE)
marzo$board[split] <- "privado"
marzo$board[-split] <- "publico"
for (s in seq(f, t, m)) {
    privado <- marzo[1:s, sum(ifelse(board == "privado",
        ifelse(clase_ternaria == "BAJA+2", 78000, -2000), 0)) / 0.5]
    publico <- marzo[1:s, sum(ifelse(board == "publico",
        ifelse(clase_ternaria == "BAJA+2", 78000, -2000), 0)) / 0.5]
    leaderboad <- rbindlist(list(leaderboad,
                        data.table(envio = s, board = "privado", valor = privado),
                        data.table(envio = s, board = "publico", valor = publico)
                        ))
}
# Graficamos
p <- ggplot(leaderboad, aes(x = envio, y = valor, color = board)) + geom_line()
print(p)





