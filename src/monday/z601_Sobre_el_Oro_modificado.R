##
## Sobre el Oro
##
## ---------------------------
## Step 1: Armando un modelo para usar.
## ---------------------------
##
## All that gliters is not gold
## --- William Shakespeare

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")

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

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022.csv.gz")
enero <- dataset[foto_mes == 202101]
marzo <- dataset[foto_mes == 202103]

# Borramos el dataset para liberar memoria.
rm(dataset)

# Armamos diferentes clases binarias:
# Sólo es evento las clase BAJA+2
enero[, clase_binaria1 := ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]

# Entrenamos en Enero para ver como funciona nuestro modelo en Marzo.
parametros <- list(cp = -1, minsplit = 1073, minbucket = 278, maxdepth = 9)
modelo <- rpart(clase_binaria1 ~ . - clase_ternaria,
                data = enero,
                xval = 0,
                control = parametros)

## ---------------------------
## Step 2: Aplicando ese modelo a los datos de Marzo
## ---------------------------

# Predigo la probabilidad de marzo.
marzo$pred <- predict(modelo, marzo, type = "prob")[, "evento"]

# Marzo entero
marzo[, sum(ifelse(pred > 0.025,
                ifelse(clase_ternaria == "BAJA+2", 78000, -2000)
            , 0))]



## ---------------------------
## Step 3: Creando 100 leaderboards
## ---------------------------

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
#Se guardan para una partición creada en el for y se ve cómo afecta la separación en el score entre público y privado
# Todos los modelos compiten contra el mismo leaderboard. Todos están frente a la mismo leaderboard
# EN función de como se parten los datos, se observa
# El que se queda con todo lo que es facil de predecir, se lleva más ganancia.



leaderboad$r_privado <- frank(leaderboad$privado)
leaderboad$r_publico <- frank(leaderboad$publico)

leaderboad

# Guardar la salida para comparar más adelante
summary(leaderboad)

## Preguntas
## ¿Qué conclusiones saca al ver los valores?
## - Respecto al valor real
## - Respecto a la relación entre el **público** y el **privado**

## AL alterar las proporciones, se sigue sin resolver el problema.


## ---------------------------
## Step 4: Graficando leaderboads
## ---------------------------

df <- melt(leaderboad, measure.vars =  c("privado", "publico"))
ggplot(df, aes(x = value, color = variable)) + geom_density()

## Observaciones?

## La dispersión entre leaderboard público y privado es distinta.

## ---------------------------
## Step 5: Compitiendo entre dos modelos
## ---------------------------

# Sumamos un modelo básico


parametros2 <- list(cp = -1, minsplit = 2, minbucket = 1, maxdepth = 5)
modelo2 <- rpart(clase_binaria1 ~ . - clase_ternaria,
                data = enero,
                xval = 0,
                control = parametros2)

marzo$pred2 <- predict(modelo2, marzo, type = "prob")[, "evento"]

# Marzo entero
marzo[, sum(ifelse(pred2 >= 0.025,
                ifelse(clase_ternaria == "BAJA+2", 78000, -2000)
            , 0))]

## Preguntas
## Abriendo la caja de pandora, ¿Cúal de los dos modelos era mejor?

## Los árboles más simples son más generalizables a nuevas predicciones. La unica variable
# que corta es el maxdepth del árbol.

## ---------------------------
## Step 6: Compitiendo entre dos modelos, ahora en los leaderboards
## ---------------------------

leaderboad2 <- data.table()
set.seed(semillas[1])
for (i in 1:100) {
  split <- caret::createDataPartition(marzo$clase_ternaria,
                     p = 0.70, list = FALSE)
    # Privado y publico es el primer modelo en muchos leaderboard
  privado <- sum((marzo$pred[split] > 0.025) *
        ifelse(marzo$clase_ternaria[split] == "BAJA+2", 78000, -2000)) / 0.7
  publico <- sum((marzo$pred[-split] > 0.025) *
        ifelse(marzo$clase_ternaria[-split] == "BAJA+2", 78000, -2000)) / 0.3
    # privado2 y publico2 es el segundo modelo en muchos leaderboard
  privado2 <- sum((marzo$pred2[split] > 0.025) *
        ifelse(marzo$clase_ternaria[split] == "BAJA+2", 78000, -2000)) / 0.7
  publico2 <- sum((marzo$pred2[-split] > 0.025) *
        ifelse(marzo$clase_ternaria[-split] == "BAJA+2", 78000, -2000)) / 0.3

  leaderboad2 <- rbindlist(list(leaderboad2,
                data.table(privado = privado,
                           publico = publico,
                           privado2 = privado2,
                           publico2 = publico2)))
}

leaderboad2


## Preguntas
## Viendo la tabla anterior, ¿En cuántos leaderboard hubiera elegido el modelo
## correcto usando el público?

# Es bastante consistente el modelo 2
# Aun asi, para un leaderboard puede ocurrir que exista en ciertos puntos
# que un modelo resulte mejor que otro

## ---------------------------
## Step 7: Compitiendo entre dos modelos, las curvas!
## ---------------------------

df2 <- melt(leaderboad2, measure.vars =  c("publico", "publico2"))
ggplot(df2, aes(x = value, color = variable)) + geom_density()

df3 <- melt(leaderboad2, measure.vars =  c("privado", "privado2"))
ggplot(df3, aes(x = value, color = variable)) + geom_density()

## Active learning ... entender que pasa.

## Se observa que los dos modelos son distintos.
## Se debe lograr poder separar los modelos en las curvas, de forma tal
# de garantizar que un modelo es efectivamente mejor que otro.

# Ahora que se tienen 202101 y 202103 se puede hacer lo siguiente:

### 1
# 202101: Montercarlo CrossValidation
# 202103: test
# - Hago modelo de training con 01,02 y 03

### 2
# MontercarloCV con 01 y 02.
# Test de 03

### 3
# USar la estructura futura para entrenar. Usar validación a futuro
# WalkForward validation