##
## Sobre Campos
##
## ---------------------------
## Step 1: Cargando los datos y las librerías
## ---------------------------
##
## Genius is one percent inspiration and 99 percent perspiration
## --- ~~Thomas Edison~~ Kate Sanborn

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("dplyr")

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
dataset <- dataset[foto_mes == 202101]

# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]
# Borramos el target viejo
dataset[, clase_ternaria := NULL]

set.seed(semillas[1])

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

## ---------------------------
## Step 2: Importancia de variables
## ---------------------------

# Antes de empezar vamos a ver la importancia de variables
modelo <- rpart(clase_binaria ~ .,
                data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = 20,
                minbucket = 10,
                maxdepth = 5)

calcular_ganancia(modelo, dtest)

print(modelo$variable.importance)


## Preguntas
## - ¿Cuáles son las variables más importantes para el modelo?
## - ¿Cómo calcula RPART la importancia de una variable?
## - ¿Es la única forma de calcular la importancia de una variable?

## No todos los modelos tienen las formas de obtener importancia de variables.
## No todos los arboles tienen la misma importancia de variables.
## Cuenta el número de cortes que se usó una variable para cortar, y eso es la importancia de variables.

## Se observa el monto de prestamos, la cantidad de prestamos como variables importantes del árbol.
## Para variables con poca cardinalidad, esta métrica ignora si es importante o no porque aparecería poco

## La variable ID aparece y termina impactando en el modelo. En general no deben entrar en los modelos, por la fuerza que tiene
## normalmente en los modelos.

## ---------------------------
## Step 2: Datos nulos
## ---------------------------

# En el summary del modelo buscamos un corte donde la primera variable
# tenga missing.

summary(modelo)

## Preguntas
## - ¿Cómo operó con la variable nula?
## - ¿Hace falta imputar las variables para que el árbol abra?

## RPART con las variables missing trata a los datos como variables subrogadas.
## tienen una estructura de corte muy similar a otras variables. Si las variables que abren bien
## no se pueden escribir tienen primary splits y subrogate splits (buscar variables que cortan parecido a variables primarias)
## Con missings, los datos faltantes usan la variable subrrogada si van a derecha o izquierda en ese nodo.


## Una variable subrogada se comparta similar a la variable principal, que mantendría la estructura de cortes.

## No es imprescindible hacerlo.

## ---------------------------
## Step 3: Datos nulos - Metiendo mano
## ---------------------------

## Las redes neuronales no soportan datos faltantes. Los métodos NN tampoco.
## Los árboles soportan la existencia de nulos.


# Numero de nulos en variable Visa_fechaalta
print(sum(is.na(dtrain$Visa_fechaalta)))

# Imputamos los nulos de nuestra variable con ceros
dtrain[, Visa_fechaalta_2 := ifelse(is.na(Visa_fechaalta), 
            0,
            Visa_fechaalta)] 

# Chequeamos el número de nulos de la nueva variable
print(sum(is.na(dtrain$Visa_fechaalta_2)))

# Comparamos las estadísticas de ambas variables
summary(dtrain$Visa_fechaalta)
summary(dtrain$Visa_fechaalta_2)

# Hacemos un modelo sin la variable vieja
modelo2 <- rpart(clase_binaria ~ . - Visa_fechaalta,
                data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = 20,
                minbucket = 10,
                maxdepth = 5)

print(modelo2$variable.importance)

# Para calcular la ganancia hay que agregar la variable a test
dtest[, Visa_fechaalta_2 := ifelse(is.na(Visa_fechaalta), 
            0,
            Visa_fechaalta)] 

calcular_ganancia(modelo2, dtest)
calcular_ganancia(modelo,dtest)

## Preguntas
## - ¿Desde el punto de vista de la importancia de variable, después que se 
##   imputo, pasó a ser más o menos importante?


## En este caso pasó a ser menos importante

## ---------------------------
## Step 4: Datos nulos - Metiendo mano, una vez más
## ---------------------------

## Problema de imputar con la media:

### - Hacés más marcada la distribución
### - Es sensible a outliers.
### - Se debe usar el entrenamiento para sacar una media a imputar. De alguna forma se tiene que tener
### un parámetro más

mean_Visa_fechaalta <- mean(dtrain$Visa_fechaalta, na.rm = T)
# Imputamos los nulos de nuestra variable con la media
dtrain[, Visa_fechaalta_3 := ifelse(is.na(Visa_fechaalta), 
            mean_Visa_fechaalta,
            Visa_fechaalta)] 

dtest[, Visa_fechaalta_3 := ifelse(is.na(Visa_fechaalta), 
            mean_Visa_fechaalta,
            Visa_fechaalta)] 

# Hacemos un modelo sin la variable vieja
modelo3 <- rpart(clase_binaria ~ . - Visa_fechaalta - Visa_fechaalta_2,
                data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = 20,
                minbucket = 10,
                maxdepth = 5)

print(modelo3$variable.importance)
calcular_ganancia(modelo3, dtest)

### Hay que hacer un experimento para ver si la media efectivamente mejora o no el modelo
### como imputador

## Preguntas
## - ¿Son muchos los casos nulos?

### Se trata de un porcentaje bajo, por lo que la imputación tiene un efecto menor

## - En mi caso aparenta una mejora, con más casos cree que esa mejora se
##   mantendría 
## - ¿Existe otro valor mejor que la media para imputar?

## ---------------------------
## Step 5: Datos nulos - Midiendo bien
## ---------------------------

## Actividad para medir bien la influencia de la media en de esa variable, 
## escriba una función de experimento que refleje la transformación  


### Para cada conjunto de train y cada semilla, calcular su media, imponerla, medirla
### y en función de ello medirla

### Los parámetros que aparezcan en la imputación agregan un grado de complejidad para hacer el 
### tuning de los mismos.

experimento <- function() {
    gan <- c()
    for (s in semillas) {
        set.seed(s)
        in_training <- caret::createDataPartition(dataset$clase_binaria, p = 0.70,
            list = FALSE)
        train  <-  dataset[in_training, ]
        test   <-  dataset[-in_training, ]

        mean_Visa_fechaalta <- mean(train$Visa_fechaalta, na.rm = T)
        # Imputamos los nulos de nuestra variable con la media
        train[, Visa_fechaalta_3 := ifelse(is.na(Visa_fechaalta), 
                                            mean_Visa_fechaalta,
                                            Visa_fechaalta)] 
        
        test[, Visa_fechaalta_3 := ifelse(is.na(Visa_fechaalta), 
                                           mean_Visa_fechaalta,
                                           Visa_fechaalta)] 
        
        r <- rpart(clase_binaria ~ .,
                    data = train,
                    xval = 0,
                    cp = -1,
                    minsplit = 20,
                    minbucket = 10,
                    maxdepth = 5)

        gan <- c(gan, calcular_ganancia(r, test))
    }
    mean(gan)
}

experimento()

## 

# Veamos la 
## Preguntas
## - ¿Qué sucede si una transformación que depende del dataset no se aplica de
##   esta manera?
## - A como funciona el rpart ¿Qué decisión toma sobre esta variable?

## ---------------------------
## Step 6: Correlaciones
## ---------------------------

# Veamos la correlación entre las dos variables previas construidas
cor(dtrain$Visa_fechaalta_2,dtrain$Visa_fechaalta_3)

# Varios modelos en los que entren dos variables muy correlacionadas se 
# romperían. Veamos que pasa con los árboles

## Matrices mal condicionadas: generan números extraños (los beta generados no son adecuads)



modelo4 <- rpart(clase_binaria ~ . ,
                data = dtrain,
                xval = 0,
                cp = -1,
                minsplit = 20,
                minbucket = 10,
                maxdepth = 5)
calcular_ganancia(modelo4, dtest)


## Preguntas
## - ¿Por qué no empeora el modelo cuándo metemos variables correlacionadas?

### Los árboles trata a las variables correlacionadas sin alterar resultadas. lo que ocurre es que el árbol se abre
### por la que esté un poco mejor y se come los efectos de la variable correlacionada, ya que no se ve afectada por la misma.



## ---------------------------
## Step 5: Outliers
## ---------------------------

# Veamos el boxplot de una variable muy importante según nuestro árbol
ggplot(dtrain, aes(x=ctrx_quarter)) + geom_boxplot()

# Vemos la distribución de los deciles
quantile(dtrain$ctrx_quarter, probs = c(0,0.5, 0.75, 0.9, 0.95, 0.99, 1))

## Preguntas
## - ¿Qué tan frecuentes considera estas dispersiones en los datasets?

### LOs modelos basados en comportamiento humanos funcionan bien con datos del 
### desorden de la generación de datos. Los árboles funcionan muy bien con este tipo de datos

## ---------------------------
## Step 6: Outliers - Luchando 
## ---------------------------

### La regresión logística y regresión lineal se ven afectadas por outliers.
### En árboles, los outliers

# Reduzcamos la enorme dispersión usando un logaritmo
dtrain[, ctrx_quarter_2 := log(ctrx_quarter + 1)]
dtest[, ctrx_quarter_2 := log(ctrx_quarter + 1)]

quantile(dtrain$ctrx_quarter_2, probs = c(0,0.5, 0.75, 0.9, 0.95, 0.99, 1))

# Comparemos dos splits
modelo_cq_1 <- rpart(clase_binaria ~ ctrx_quarter,
                    data = dtrain,
                    xval = 0,
                    cp = -1,
                    maxdepth = 1)
modelo_cq_2 <- rpart(clase_binaria ~ ctrx_quarter_2,
                    data = dtrain,
                    xval = 0,
                    cp = -1,
                    maxdepth = 1)

print(modelo_cq_1)
print(modelo_cq_2)

## Preguntas
## - Mirando los puntos de corte de los dos modelos ¿Existe una relación
##   matermática entre ellos?
## - ¿Es útil una transformación monótona en los árboles de decisión?

### Los puntos de corte no interesan qué tan lejos están. Lo que interesa es el orden de los datos
### Los árboles miran únicamente el orden.

## ---------------------------
## Step 7: Outliers - Una más y no jodemos más 
## ---------------------------

dtrain[, r_ctrx_quarter := ntile(ctrx_quarter, 10)]
dtest[, r_ctrx_quarter := ntile(ctrx_quarter, 10)]

modelo_cq_4 <- rpart(clase_binaria ~ . - ctrx_quarter - ctrx_quarter_2 - Visa_fechaalta_2 - Visa_fechaalta_3,
                    data = dtrain,
                    xval = 0,
                    cp = -1,
                    minsplit = 20,
                    minbucket = 10,
                    maxdepth = 5)

calcular_ganancia(modelo_cq_4, dtest)

### Binnear los datos es ventjoso para reducir la granularidad de los datos, 
### y además sirve para poder correlacionar variables de hoy con una prediccción que capaz se haga a futuro.
### Esto se llama "DATA DRIFTING", que es el nombre que se le da al fenómeno de cuando la variable
### se aparta de esto.

### No tiene sentido binarizar todas las variables del dataset

## Actividad: Para mi semilla, esta estrategia es buena, hacer un experimento
## donde no quede al azar este resultado.

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

# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
    dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
    dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
}

## ---------------------------
## Step 9: Un + poco de R, seleccionar las variables para modelar 
## ---------------------------

# Las mejores más la variables rankeadas
mis_variables_2 <- c("r_ctrx_quarter",
                    "active_quarter",
                    "r_mprestamos_personales",
                    "cprestamos_personales",
                    "r_mactivos_margen",
                    "r_mcuentas_saldo",
                    "ccomisiones_otras",
                    "r_mcuenta_corriente",
                    "cdescubierto_preacordado") 

campos <- paste(mis_variables_2, collapse = " + ")
formula <- paste0( "clase_binaria ~ ", campos )

modelo5 <- rpart(formula,
                    data = dtrain,
                    xval = 0,
                    cp = -1,
                    minsplit = 20,
                    minbucket = 10,
                    maxdepth = 5)

print(modelo5$variable.importance)

## ---------------------------
## Step 10: Embeddings (Caseros)
## ---------------------------

# Hagamos interactuar algunas variables para ver si conseguimos alguna mejor

### PCA: reducción de dimensionalidad.
## Buscar TSNE

#Interacciones: se toma una lista de variables y se las hace interactuar entre sí.
# EN este caso, se las multiplica aunque se podrían dividir.


nuevas <- c()
for (var1 in mis_variables_2) {
    for (var2 in mis_variables_2) {
        if (var1 != var2) {
            nueva <- paste(var1, var2, sep = "___")
            dtrain[, (nueva) := get(var1) * get(var2)]
            dtest[, (nueva) := get(var1) * get(var2)]
            nuevas <- c(nuevas, nueva)
        }
    }
}

mis_variables_3 <- c(nuevas, mis_variables_2) 

campos2 <- paste(mis_variables_3, collapse = " + ")
formula2 <- paste0( "clase_binaria ~ ", campos2 )

modelo6 <- rpart(formula2,
                    data = dtrain,
                    xval = 0,
                    cp = -1,
                    minsplit = 20,
                    minbucket = 10,
                    maxdepth = 5)

print(modelo6$variable.importance)

# Importante: Que una modelo tenga otras variables importantes no implicar que
# sea mejor, ni peor. Eso se debe evaluar con los experimentos

### No considerar la variable más importante para entrenar es una buena práctica para
### ver si el modelo es robusto por fuera de las variables más importantes.

### La construcción de nuevas variables debe realizarse no únicamente por aplicar interacciones en el espacio


##
## TAREA: Multiples experimentos. Un script por cada uno que debe incluir:
## - Feature engineering correctamente aplicado
## - Opt Bayesiana para el dataset que se incluya nuevas variables
## - Scorear en los datos de marzo y subir a kaggle el score.


