#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("ROCR")
require("caret")
require("rpart.plot")
require("parallel")
require("rlist")
# Librerías necesarias
require("dplyr")


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Poner la carpeta de la materia de SU computadora local
setwd("D:/economia_finanzas")
# Poner sus semillas
semillas <- c(732497,
              681979,
              281887,
              936659,
              692089)


# Cargamos el dataset
dataset <- fread("./datasets/competencia1_2022.csv")
# Nos quedamos solo con el 202101

############# FEATURE ENGINEERING############


## ---------------------------
## Step 8: Un poco de R, como procesar multiples variables con una técnica 
## ---------------------------

# Supongamos que tenemos una lista de variables a las que queremos transformar
mis_variables <- c("ctrx_quarter",
                    "mprestamos_personales",
                    "mcuentas_saldo",
                    "mactivos_margen",
                    "mcaja_ahorro",
                    "mcuenta_corriente")

variables_rankear <- c("mcomisiones",
                        "mactivos_margen",
                        "mpasivos_margen",
                        "mcuenta_corriente_adicional",
                        "mcuenta_corriente",
                        "mcaja_ahorro",
                        "mcaja_ahorro_adicional",
                        "mcaja_ahorro_dolares",
                        "mautoservicio",
                        "mtarjeta_visa_consumo",
                        "mtarjeta_master_consumo",
                        "mprestamos_personales",
                        "mprestamos_prendarios",
                        "mprestamos_hipotecarios",
                        "mplazo_fijo_dolares",
                        "mplazo_fijo_pesos",
                        "minversion1_pesos",
                        "minversion1_dolares",
                        "minversion2",
                        "mpayroll",
                        "mpayroll2",
                        "mcuenta_debitos_automaticos",
                        "mttarjeta_visa_debitos_automaticos",
                        "mttarjeta_master_debitos_automaticos",
                        "mpagodeservicios",
                        "mpagomiscuentas",
                        "mcajeros_propios_descuentos",
                        "mtarjeta_visa_descuentos",
                        "mtarjeta_master_descuentos",
                        "mcomisiones_mantenimiento",
                        "mcomisiones_otras",
                        "mforex_buy",
                        "mforex_sell",
                        "mtransferencias_recibidas",
                        "mtransferencias_emitidas",
                        "mextraccion_autoservicio",
                        "mcheques_depositados",
                        "mcheques_emitidos",
                        "mcheques_depositados_rechazados",
                        "mcheques_emitidos_rechazados",
                        "matm",
                        "matm_other",
                        "Master_mconsumospesos",
                        "Master_mconsumosdolares",
                        "Master_mpagado",
                        "Master_mpagospesos",
                        "Master_mpagosdolares",
                        "Master_mconsumototal",
                        "Master_mpagominimo",
                        "Visa_mconsumospesos",
                        "Visa_mconsumosdolares",
                        "Visa_mpagado",
                        "Visa_mpagospesos",
                        "Visa_mpagosdolares",
                        "Visa_mconsumototal",
                        "Visa_mpagominimo")

# A todas las vamos a rankear

## ---------------------------
## Step 9: Un + poco de R, seleccionar las variables para modelar 
## ---------------------------

# Las mejores más la variables rankeadas
mis_variables_2 <- c("active_quarter",
                    "cprestamos_personales",
                    "ccomisiones_otras",
                    "cdescubierto_preacordado")



prefix <- "r_"
for (var in variables_rankear) {
    dataset[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
}
var_rankeadas <- colnames(dataset[,157:ncol(dataset)])

variables_modelo <- c(var_rankeadas,mis_variables_2)

dapply  <- dataset[foto_mes == 202103 ] # defino donde voy a aplicar el modelo
dataset <- dataset[foto_mes == 202101]  
print(dim(dataset))
print(dim(dapply))


# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  clase_ternaria == "BAJA+2" | clase_ternaria == "BAJA+1",
  "evento",
  "noevento"
)]

print(nrow(dataset[clase_ternaria == "BAJA+1"]))
print(nrow(dataset[clase_ternaria == "BAJA+2"]))
print(nrow(dataset[clase_binaria == "evento"]))
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

# Particionamos de forma estratificada
in_training <- caret::createDataPartition(dataset$clase_binaria,
                     p = 0.70, list = FALSE)
dtrain  <-  dataset[in_training, ]
dtest   <-  dataset[-in_training, ]

calcular_ganancia <- function(modelo, test) {
    pred_testing <- predict(modelo, test, type = "prob")
    sum(
        (pred_testing[, "evento"] >= 0.025) * ifelse(test$clase_binaria == "evento",
                                         78000, -2000) / 0.3
    )
}







campos <- paste( variables_modelo,collapse = " + ")
formula <- paste0( "clase_binaria ~ ", campos )

modelo <- rpart(formula,
                    data = dtrain,
                    xval = 0,
                    cp = -1,
                    minsplit = 300,
                    minbucket = 80,
                    maxdepth = 7)

print(modelo$variable.importance)


pred_training <- predict(modelo, dtrain, type = "prob")
pred_testing <- predict(modelo, dtest, type = "prob")


## Preguntas:
## - ¿Qué tan importante mirar las métricas de train?

## ---------------------------
## Step 3: Mirando la ganancia
## ---------------------------

# Armamos una función que nos calcule la ganancia, usando el punto de corte de
# 0.025
ganancia <- function(probabilidades, clase) {
  return(sum(
    (probabilidades >= 0.025) * ifelse(clase == "evento", 78000, -2000))
  )
}

# La ganancia en testing NORMALIZADA
View(ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3)

n <- 10
resultados_n_gan <- c()
set.seed(semillas[1])
t0 <- Sys.time()
for (i in 1:n) {

    in_training <- caret::createDataPartition(dataset[, get("clase_binaria")],
                            p = 0.70, list = FALSE)
    dtrain  <-  dataset[in_training, ]
    dtest   <-  dataset[-in_training, ]

    prefix <- "r_"
    for (var in variables_rankear) {
        dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
        dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 20)]
    }
    var_rankeadas <- colnames(dtest[,157:ncol(dtest)])

    variables_modelo <- c(var_rankeadas,mis_variables_2)
    modelo <- rpart(formula,
                        data = dtrain,
                        xval = 0,
                        cp = -1,
                        minsplit = 300,
                        minbucket = 80,
                        maxdepth = 7)

    pred_testing <- predict(modelo, dtest, type = "prob")

    gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3

    resultados_n_gan <- c(resultados_n_gan, gan)
}
print(Sys.time() - t0)



# La menor ganancia conseguida en test
print(min(resultados_n_gan))

# La mayor ganancia
print(max(resultados_n_gan))

# La media de la ganancia
print(mean(resultados_n_gan))

# Veamos la dispersión de la ganancia
ggplot() + aes(resultados_n_gan) + geom_density()


# Optimización bayesiana

## ---------------------------
## Step 10: Buscando con una Opt. Bayesiana para 2 parámetros
## ---------------------------


# Armamos una función para modelar con el fin de simplificar el código futuro
modelo_rpart <- function(train, test, cp =  -1, ms = 20, mb = 1, md = 10) {
    modelo <- rpart(clase_binaria ~ ., data = train,
                    xval = 0,
                    cp = cp,
                    minsplit = ms,
                    minbucket = mb,
                    maxdepth = md)

    test_prediccion <- predict(modelo, test, type = "prob")
    roc_pred <-  ROCR::prediction(test_prediccion[, "evento"],
                    test$clase_binaria,
                                  label.ordering = c("noevento", "evento"))
    auc_t <-  ROCR::performance(roc_pred, "auc")

    unlist(auc_t@y.values)
}

# Función para tomar un muestra dejando todos los elementos de la clase BAJA+2
tomar_muestra <- function(datos, resto = 10000) {
      t <- datos$clase_binaria == "evento"
      r <- rep(FALSE, length(datos$clase_binaria))
      r[!t][sample.int(resto, n = (length(t) - sum(t)))] <- TRUE
      t | r
}

# Una función auxiliar para los experimentos
experimento_rpart <- function(ds, semillas, cp = -1, ms = 300, mb = 80, md = 7) {
  auc <- c()
  for (s in semillas) {
    set.seed(s)
    in_training <- caret::createDataPartition(ds$clase_binaria, p = 0.70,
        list = FALSE)
    train  <-  ds[in_training, ]
    test   <-  ds[-in_training, ]
    train_sample <- tomar_muestra(train)
    r <- modelo_rpart(train[train_sample,], test, 
                    cp = cp, ms = ms, mb = mb, md = md)
    auc <- c(auc, r)
  }
  mean(auc)
}

require("DiceKriging")
require("mlrMBO")

set.seed(semillas[1])
obj_fun_md_ms_mb <- function(x) {
  experimento_rpart(dataset[,variables_modelo,with=FALSE], semillas
            , md = x$maxdepth
            , ms = x$minsplit
            , mb = x$minbucket)
}

obj_fun <- makeSingleObjectiveFunction(
  minimize = FALSE,
  fn = obj_fun_md_ms_mb,
  par.set = makeParamSet(
    makeIntegerParam("maxdepth",  lower = 4L, upper = 32L),
    makeIntegerParam("minsplit",  lower = 1L, upper = 500L),
    makeIntegerParam("minbucket",  lower = 1L, upper = 250L)
    # makeNumericParam <- para parámetros continuos
  ),
  # noisy = TRUE,
  has.simple.signature = FALSE
)

ctrl <- makeMBOControl()
ctrl <- setMBOControlTermination(ctrl, iters = 16L)
ctrl <- setMBOControlInfill(
  ctrl,
  crit = makeMBOInfillCritEI(),
  opt = "focussearch",
  # sacar parámetro opt.focussearch.points en próximas ejecuciones
  opt.focussearch.points = 20
)

lrn <- makeMBOLearner(ctrl, obj_fun)

surr_km <- makeLearner("regr.km", predict.type = "se", covtype = "matern3_2")

run_md_ms <- mbo(obj_fun, learner = surr_km, control = ctrl, )
print(run_md_ms)




# Genero prediccion del modelo para Kaggle



# Grid Search

# Complete los valores que se van a combinar para cada parámetro a explorar

for (cp in c(-1, 0.01)) {
for (md in c(5, 10)) {
for (ms in c(1, 50)) {
for (mb in c(1, as.integer(ms / 2))) {

    t0 <- Sys.time()
    gan_semillas <- c()
    for (s in semillas) {
        set.seed(s)
        in_training <- caret::createDataPartition(dataset[,
                        get("clase_binaria")],
                                p = 0.70, list = FALSE)
        dtrain  <-  dataset[in_training, ]
        dtest   <-  dataset[-in_training, ]

        modelo <- rpart(clase_binaria ~ .,
                        data = dtrain,
                        xval = 0,
                        cp = cp,
                        minsplit = ms,
                        minbucket = mb,
                        maxdepth = md)

        pred_testing <- predict(modelo, dtest, type = "prob")
        gan <- ganancia(pred_testing[, "evento"], dtest$clase_binaria) / 0.3

        gan_semillas <- c(gan_semillas, gan)
    }
    tiempo <-  as.numeric(Sys.time() - t0, units = "secs")

    resultados_grid_search <- rbindlist(list(
                                resultados_grid_search,
                                data.table(
                                    tiempo = tiempo,
                                    cp = cp,
                                    mb = mb,
                                    ms = ms,
                                    md = md,
                                    gan = mean(gan_semillas))
                                ))
}
}
}
}

# Visualizo los parámetros de los mejores parámetros
View(resultados_grid_search[gan == max(gan), ])



modelo <- rpart(formula,
                        data = dataset,
                        xval = 0,
                        cp = -1,
                        minsplit = 390,
                        minbucket = 50,
                        maxdepth = 6)

# La siguiente función devuelve todas las hojas (nodos terminales) en una tabla
# para poder analizar mejor nuestro árbol.
tablahojas <- function(arbol, datos, target = "clase_binaria") {
  # Tomamos la columna con el target
  target_vector <- datos[, get(target)]
  # Tomamos las clases de nuestro target
  classes <- unique(target_vector)
  # Tomamos las posicion de las hojas que aplican a los registro de nuestro ds
  row_leaf <- unique(arbol$where)
  leaves <- data.table(row_frame = row_leaf)
  setkey(leaves,row_frame)
  # Relacion target ~ hojas
  leaves_target <- dcast(
    data.table(
      target = target_vector,
      leaf = arbol$where),
    leaf ~ target, length,
    value.var = "target")
  setkey(leaves_target, leaf)
  # Juntamos todo
  leaves_target <- leaves_target[leaves, nomatch = 0]
  # Sumamos algunas columnas calculadas
  colnames(leaves_target[, classes, with = FALSE])[apply(
    leaves_target[, classes, with = FALSE], 1, which.max)]
  # Clase mayoritaria
  leaves_target[, y := colnames(
                    leaves_target[, classes, with = FALSE]
                  )[apply(leaves_target[, classes, with = FALSE],
                   1, which.max)]]
  # Cantidad de elementos de la hoja
  leaves_target[, TOTAL := unlist(Reduce(function(a, b) Map(`+`, a, b), .SD)),
                 .SDcols = classes]
  leaves_target
}

# Ejecutamos la función sobre nuestro modelo, con nuestros datos
hojas <- tablahojas(modelo, dataset)
print(hojas)

# Transformamos las hojas a una tabla
hojasbinario <- tablahojas(modelo, dataset, "clase_binaria")

# Y agregamos la ganancia de cada hoja
hojasbinario[, ganancia := evento * 78000 - 2000 * noevento]
print(hojasbinario)
# Por último sumarizamos
print(hojasbinario[ganancia > 0,
 .(ganancia = sum(ganancia), enviados = sum(TOTAL), sevan = sum(evento))])

## Pregunta
## - ¿Considera que la agrupación de clases fue positiva para la  ganancia?

## ---------------------------
## Step 6: Salida probabilísticas
## ---------------------------

# Calculamos la probabilidad de evento en cada hoja
hojasbinario[, p_evento := evento / (evento + noevento)]

# Ordenamos de forma descendiente las probabilidades, ya que nos interesan
# ante todo las probabilidades más altas
hojasordenadas <- hojasbinario[order(-p_evento),]

# Calculamos la ganancia acumulada, desde con la probabilidad desde la primera
# fila con probabilidad más alta hasta la fila N, para cada fila.
hojasordenadas[, gan_acum := cumsum(ganancia)]

print(hojasordenadas)

####################################

prediccion  <- predict( modelo, dapply, type = "prob")[,"evento"]

entrega  <-  as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente,
                                  "Predicted"= as.integer(  prediccion > 0.025 ) ) )

fwrite( entrega, paste0( "./TareasHogar/kaggle/Entrega_binario_comparativa_ms300mb80md7_varrankeadas.csv"), sep="," )



print('Hola')

