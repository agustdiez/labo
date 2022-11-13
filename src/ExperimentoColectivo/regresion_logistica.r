#Se corre en la nube
require("data.table")

#Parametros del script
PARAM <- list()
PARAM$experimento <- "REGRESION_LOGISTICA"
PARAM$exp_input <- "HT9420_TRAINING2019202"
PARAM$input$dataset       <- "./datasets/competenciaFINAL_2022.csv.gz"
PARAM$train$test <- c( 202107 )

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa
#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory
base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO



#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
dataset  <- fread( arch_dataset )

#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
dfuture <- fread( arch_future )


#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]
dfuture[ , clase01 := NA ]

campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria") )
print(campos_buenos)

# Modelo de regresión logistica

# modelo de regresión logística 
model_glm <- glm(data = dataset[,campos_buenos, with=FALSE ],formula = clase01 ~ ., family = 'binomial')


#genero la prediccion, Scoring
prediccion  <- predict( model_glm ,
                       dfuture[,campos_buenos, with=FALSE ],type="response" )

tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes ) ]
tb_prediccion[ , prob_glm := prediccion ]


nom_pred  <- paste0( "prediccion_logistica",
                      ".csv"  )

fwrite( tb_prediccion,
          file= nom_pred,
          sep= "\t" )

#Extracción del mes 202107 real

#cargo el dataset de donde extraigo la clase y los IDs
setwd("~/buckets/b1/")

dataset  <- fread( PARAM$input$dataset )

dataset_202107 <- dataset[ foto_mes %in% PARAM$train$test , ]
dataset_202107 <- dataset_202107[,list( numero_de_cliente, clase_ternaria )]


fwrite( dataset_202107,
        file= "dataset_202107.csv.gz",
        logical01= TRUE,
        sep= "," )