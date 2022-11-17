# Compilado de archivos


require("data.table")


# TRaigo todos los datasets

setwd( "D:\\OneDrive\\Personal\\Maestria Data Mining\\economia_finanzas\\" )



scores_lgbm  <- paste0("./datasets/prediccion_LGBM.csv" )
dataset_lgbm  <- fread(scores_lgbm)

scores_regresion  <- paste0("./exp/REGRESION_LOGISTICA/prediccion_logistica.csv")
dataset_regresion  <- fread(scores_regresion,sep = "\t")

scores_reales  <- paste0("./exp/REGRESION_LOGISTICA/dataset_202107.csv.cz")
dataset_reales  <- fread(scores_reales)
