##
## Sobre Ensambles
##
## ---------------------------
## Step 1: Conceptos
## ---------------------------
##
##  Hay quienes pasan por el bosque y sólo ven leña para el fuego
## --- León Tolstoi

## Preguntas
## - ¿Qué es un ensamble de modelos?
## - ¿Cómo tienen que ser los modelos dentro de un ensamble?
## - ¿Qué técnicas conoce para ensamblar modelos?
## - ¿Por qué funcionan mejor los ensambles?


# Los ensambles pueden partir de modelos ya desarrollados, o de modelos que se
# creen especialmente para ser ensamblados.

# Sobre los segundos, veremos son los llamados Bagging (bootstrap aggregating).
# - Hacer **N** nuevos conjunto de entrenamiento usando boostraping, o sea,
#   reemplazar nuestro dataset por elementos aleatorios con reemplazo.
# - Para este cada nuevo dataset obtener un modelo.
# - Promediar (o votar) las salidas de los modelos.

# El espíritu detrás de este algoritmo, puede entenderse en que cada modelo es
# una especialista de sólo una parte, y la suma de muchos especialistas
# consiguen un buen modelo.

# El algoritmo de **bagging** más conocido es el **random forest**.

# Bagging:
# Bootstrapp aggregating: muestreo con repetición



## ---------------------------
## Step 2: Primer RF
## ---------------------------

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("ranger")
require("randomForest")
require("lightgbm")

# Poner la carpeta de la materia de SU computadora local
setwd("D:/economia_finanzas")
# Poner sus semillas
semillas <- c(732497,
              681979,
              281887,
              936659,
              692089)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022.csv.gz")
enero <- dataset[foto_mes == 202101]
marzo <- dataset[foto_mes == 202103]

# Importante que la clase sea factor
enero[, clase_binaria1 := factor(ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            ))]
enero$clase_ternaria <- NULL
in_training <- caret::createDataPartition(enero$clase_binaria1,
                     p = 0.70, list = FALSE)

dtrain  <-  enero[in_training, ]
dtest   <-  enero[-in_training, ]

# ranger no soporta, como lo hacen otras librerías, los missing values
dtrain <-  na.roughfix(dtrain)
dtest <-  na.roughfix(dtest)

## "ranger" es una adaptación del algoritmo original y no soporta missing values.

# Cantidad de variables que abren por cada hoja
n_variables <- round(sqrt(dim(dtrain)[2] - 1))

t0 <- Sys.time()
modelo_rf_1 <- ranger(clase_binaria1 ~ ., data = dtrain,
                  probability = TRUE,
                  num.trees = 100,
                  min.node.size=10, #Con esta variable se está cortando por tamaño mínimo del nodo
                  mtry = n_variables,
                  splitrule = "gini", #como se decide cortar
                  sample.fraction = 0.66,
                  importance = "impurity",
                  verbose = TRUE)
t1 <- Sys.time()
as.numeric(t1 - t0, units = "secs")

#El RF tiene la cant. de arboles como parámetro más importante y la cantidad de variables es el siguiente. En realidad, es el más importante
#

## ---------------------------
## Step 3: Midiendo el primero RF
## ---------------------------

pred_train <- predict(modelo_rf_1, dtrain)
## No se predice sobre train.
## Encontró todas las instancias, por lo que nunca se mira la ganancia en train. Overfitting

pred_test <- predict(modelo_rf_1, dtest)



# Ganancia en dtrain
print(sum((pred_train$predictions[, "evento"] >= 0.025) * ifelse(
                    dtrain$clase_binaria1 == "evento",
                    78000, -2000) / 0.7))
# Ganancia en dtest
print(sum((pred_test$predictions[, "evento"] >= 0.025) * ifelse(
                    dtest$clase_binaria1 == "evento",
                    78000, -2000) / 0.3))

## Preguntas
## - ¿Qué paso en `train`?
## - ¿Se veía esa diferencia en los árboles?

# En los árboles, este fenomeno no se veia tanto

## ---------------------------
## Step 4: Importancia de variables
## ---------------------------

importancia <- as.data.table(modelo_rf_1$variable.importance,
                    keep.rownames = TRUE)
colnames(importancia) <- c("variable", "importancia")
setorder(importancia, -importancia)
importancia

## Preguntas
## - ¿Qué significa que una variable sea más importante que otra?
# 
## - ¿Qué significa que una variable tenga 0 importancia?
# No entró en ningún lado la variable en juego
## - ¿Con el **RF** es suficiente como para descartarlas?
## - ¿Qué una variable tenga algo de importancia es suficiente como para
## - entender que da valor?

# Mientras más árboles se pongan, la importancia puede subir porque hay mayor probabiliaad de que aparezcan.


## ---------------------------
## Step 5: Un experimento con pollitos
## ---------------------------

dtrain$pollito <- runif(nrow(dtrain))

modelo_rf_2 <- ranger(clase_binaria1 ~ ., data = dtrain,
                  probability = TRUE,
                  num.trees = 150,
                  min.node.size = 10, # <---------
                  mtry = n_variables,
                  splitrule = "gini",
                  importance = "impurity",
                  verbose = TRUE)

importancia2 <- as.data.table(modelo_rf_2$variable.importance,
                    keep.rownames = TRUE)
colnames(importancia2) <- c("variable", "importancia")
setorder(importancia2, -importancia)
importancia2
which(importancia2$variable == "pollito")

## En el RF se está aplicando la técnica del pollito y está quedando tercero en la importancia.
## No es correcto esto

## Active learning o a llorar a la iglesia.

## ---------------------------
## Step 5.1: Hablando de los Extra Trees
## ---------------------------

# ### Se baja la profundidad y se tira aleatoriadad en cada nodo. QUe los puntos de corte sean aleatorio
# ayuda mucho a evitar que aparezcan las variables canario.

modelo_rf_3 <- ranger(clase_binaria1 ~ ., data = dtrain,
                  probability = TRUE,
                  num.trees = 150,
                  min.node.size = 1000, # <---------
                  mtry = n_variables,
                  splitrule = "extratrees", # <---------
                  num.random.splits = 10, # <---------
                  importance = "impurity",
                  verbose = TRUE)

importancia3 <- as.data.table(modelo_rf_3$variable.importance,
                    keep.rownames = TRUE)
colnames(importancia3) <- c("variable", "importancia")
setorder(importancia3, -importancia)
importancia3
which(importancia3$variable == "pollito")

## ---------------------------
## Step 6: Boosting, la navaja suiza de los modelos - Conceptos
## ---------------------------

# Estos se construyen de forma serial.
# Primero se parte de un modelo (que puede ser un valor constante) y se
# complementa con un modelo que busca mejorar al anterior.

# Hay dos algoritmos muy conocidos de este tipo:

# **Adaboost**: Que cada nuevo modelo va mejorando a los anteriores poniendo un
# peso mayor en los casos donde la clasificación es incorrecta

# **Gradient Boosting**: Que cada nuevo modelo va mejorando los anteriores,
# tratando de corregir los residuos, buscando estos últimos con el gradiente
# de una función de perdida.

# Este último se empezó a hacer muy popular por la excelente pieza de tecnología
# que es su implementación **xgboost**, superado luego por el LightGBM.

### Adaboost corrige con la ponderación de pesos de errores
### Gradient Boosting hace algo similar pero con los residuos


## ---------------------------
## Step 7: LightGBM
## ---------------------------

# Cargamos todo para tener un código limpio
dataset <- fread("./datasets/competencia2_2022.csv.gz")
enero <- dataset[foto_mes == 202101]
marzo <- dataset[foto_mes == 202103]
rm(dataset)

clase_binaria <- ifelse(enero$clase_ternaria == "BAJA+2", 1, 0)
enero$clase_ternaria <- NULL

dtrain  <- lgb.Dataset(data = data.matrix(enero), label = clase_binaria)

ganancia_lgb <- function(probs, datos) {
  return(list("name" = "ganancia",
                "value" =  sum( (probs > 0.025) *
                    ifelse(getinfo(datos, "label") == 1, 78000, -2000)) / 0.2,
                "higher_better" = TRUE))
}

set.seed(semillas[1])
# LightGBM, al igual que XGB traen su implementación del CV
# Los parámetros los iremos viendo en profundidad la siguiente clase.

## Se dispone de un crossvalidation interno que valide con una función personalziada.

model_lgbm_cv <- lgb.cv(data = dtrain,
         eval = ganancia_lgb,
         stratified = TRUE,
         nfold = 5,
         param = list(objective = "binary",
                       max_bin = 15,
                       min_data_in_leaf = 4000,
                       learning_rate = 0.05
                       )
      )

# Mejor iteración
model_lgbm_cv$best_iter

## La mejor iteración

# Ganancia de la mejor iteración
unlist(model_lgbm_cv$record_evals$valid$ganancia$eval)[model_lgbm_cv$best_iter]

# Una vez que elegimos los parámetros tenemos que entrenar con todos.
model_lgm <- lightgbm(data = dtrain,
            nrounds = model_lgbm_cv$best_iter, # <--- OJO! Double Descent alert
            params = list(objective = "binary",
                            max_bin = 15,
                            min_data_in_leaf = 4000,
                            learning_rate = 0.05),
             verbose = -1)

# También tiene su importancia de variables
lgb.importance(model_lgm, percentage = TRUE)

## ---------------------------
## Step 8: En Marzo
## ---------------------------

marzo$pred <- predict(model_lgm, data.matrix(marzo[, 1:154]))

# TOTAL
sum((marzo$pred > 0.025) *
            ifelse(marzo$clase_ternaria == "BAJA+2", 78000, -2000))

# Sobre 100 LB
leaderboad <- data.table()
set.seed(semillas[1])
for (i in 1:100) {
  split <- caret::createDataPartition(marzo$clase_ternaria,
                     p = 0.70, list = FALSE)
  privado <- sum((marzo$pred[split] > 0.025) *
        ifelse(marzo$clase_ternaria[split] == "BAJA+2", 78000, -2000)) / 0.7
  publico <- sum((marzo$pred[-split] > 0.025) *
        ifelse(marzo$clase_ternaria[-split] == "BAJA+2", 78000, -2000)) / 0.3
  leaderboad <- rbindlist(list(leaderboad,
                data.table(privado = privado, publico = publico)))
}

# Comparar con la salida del árbol
summary(leaderboad)

## Bienvenido al mundo de los ensambles

