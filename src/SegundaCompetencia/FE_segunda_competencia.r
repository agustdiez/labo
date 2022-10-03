# Se deberá hacer FE con las variables que presenten data drifting y elimino todas las variables que presente canarios



#Limpieza de memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

#Librerías requeridas
require("data.table")
require("xgboost")
require("Hmisc")

#Aqui se debe poner la carpeta de la computadora local
setwd("D:/economia_finanzas")  #Establezco el Working Directory


# Poner sus semillas
semillas <- c(732497,
              681979,
              281887,
              936659,
              692089)


# Cargamos el dataset
dataset <- fread("./datasets/competencia2_2022.csv.gz")



#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

#Columnas principales

print(colnames(dataset))

## DATA DRIFTING 

# Se identifican las siguientes variables con data drifting con montos en pesos asociados al mes

variables_drift_pesos <- c("mrentabilidad","mrentabilidad_annual","mcomisiones","mactivos_margen","mpasivos_margen","cproductos",
                        "tcuentas","ccuenta_corriente","mcuenta_corriente_adicional","mcuenta_corriente","ccaja_ahorro","mcaja_ahorro","mcaja_ahorro_adicional",
                        "mcaja_ahorro_dolares","cdescubierto_preacordado","mcuentas_saldo","ctarjeta_debito","ctarjeta_debito_transacciones","mautoservicio",
                        "ctarjeta_visa","ctarjeta_visa_transacciones","mtarjeta_visa_consumo","ctarjeta_master","ctarjeta_master_transacciones","mtarjeta_master_consumo",
                        "cprestamos_personales","mprestamos_personales","cprestamos_prendarios","mprestamos_prendarios","cprestamos_hipotecarios","mprestamos_hipotecarios",
                        "cplazo_fijo","mplazo_fijo_dolares","mplazo_fijo_pesos","cinversion1","minversion1_pesos","minversion1_dolares","cinversion2","minversion2",
                        "cseguro_vida","cseguro_auto","cseguro_vivienda","cseguro_accidentes_personales","ccaja_seguridad","cpayroll_trx","mpayroll","mpayroll2","cpayroll2_trx",
                        "ccuenta_debitos_automaticos","mcuenta_debitos_automaticos","ctarjeta_visa_debitos_automaticos","mttarjeta_visa_debitos_automaticos",
                        "ctarjeta_master_debitos_automaticos","mttarjeta_master_debitos_automaticos","cpagodeservicios","mpagodeservicios","cpagomiscuentas","mpagomiscuentas",
                        "ccajeros_propios_descuentos","mcajeros_propios_descuentos","ctarjeta_visa_descuentos","mtarjeta_visa_descuentos","ctarjeta_master_descuentos",
                        "mtarjeta_master_descuentos","ccomisiones_mantenimiento","mcomisiones_mantenimiento","ccomisiones_otras","mcomisiones_otras","cforex","cforex_buy",
                        "mforex_buy","cforex_sell","mforex_sell","ctransferencias_recibidas","mtransferencias_recibidas","ctransferencias_emitidas","mtransferencias_emitidas",
                        "cextraccion_autoservicio","mextraccion_autoservicio","ccheques_depositados","mcheques_depositados","ccheques_emitidos","mcheques_emitidos",
                        "ccheques_depositados_rechazados","mcheques_depositados_rechazados","ccheques_emitidos_rechazados","mcheques_emitidos_rechazados","tcallcenter",
                        "ccallcenter_transacciones","thomebanking","chomebanking_transacciones","ccajas_transacciones","ccajas_consultas","ccajas_depositos",
                        "ccajas_extracciones","ccajas_otras","catm_trx","matm","catm_trx_other","matm_other","ctrx_quarter","tmobile_app","cmobile_app_trx",
                        "Master_delinquency","Master_status","Master_mfinanciacion_limite","Master_Fvencimiento","Master_Finiciomora","Master_msaldototal", # nolint
                        "Master_msaldopesos","Master_msaldodolares","Master_mconsumospesos","Master_mconsumosdolares","Master_mlimitecompra","Master_madelantopesos",
                        "Master_madelantodolares","Master_fultimo_cierre","Master_mpagado","Master_mpagospesos","Master_mpagosdolares","Master_fechaalta",
                        "Master_mconsumototal","Master_cconsumos","Master_cadelantosefectivo","Master_mpagominimo","Visa_delinquency","Visa_status",
                        "Visa_mfinanciacion_limite","Visa_Fvencimiento","Visa_Finiciomora","Visa_msaldototal","Visa_msaldopesos","Visa_msaldodolares",
                        "Visa_mconsumospesos","Visa_mconsumosdolares","Visa_mlimitecompra","Visa_madelantopesos","Visa_madelantodolares","Visa_fultimo_cierre",
                        "Visa_mpagado","Visa_mpagospesos","Visa_mpagosdolares","Visa_fechaalta","Visa_mconsumototal","Visa_cconsumos",
                        "Visa_cadelantosefectivo","Visa_mpagominimo")



#Normalizo variables asociadas a Drifting

marzo <- subset(dataset, foto_mes %in% 202103)
mayo <- subset(dataset, foto_mes %in% 202105)
nrow(marzo)
nrow(mayo)

# Creacion de listas de clasificacion de variables
negativos_marzo <- c()
negativos_mayo <- c()
variables_marzo <- c()
variables_mayo <- c()

otros <- c()

for (campo in variables_drift_pesos) {
      if (min(marzo[, get(campo)]) >= 0) {
        marzo[, paste0("r__", campo, sep = "") := (frankv(marzo, cols = campo) - 1) / (length(marzo[, get(campo)]) - 1)] # rankeo entre 0 y 1
        marzo[, paste0(campo) := NULL] # elimino la variable original
        variables_marzo <- c(variables_marzo, campo)
      } 
      else {
        negativos_marzo <- c(negativos_marzo, campo)
      }
    }

for (campo in variables_drift_pesos) {
      if (min(mayo[, get(campo)]) >= 0) {
        mayo[, paste0("r__", campo, sep = "") := (frankv(mayo, cols = campo) - 1) / (length(mayo[, get(campo)]) - 1)] # rankeo entre 0 y 1
        mayo[, paste0(campo) := NULL] # elimino la variable original
        variables_mayo <- c(variables_mayo, campo)
        } 
      else {
        negativos_mayo <- c(negativos_mayo, campo)
      }
    }

print(negativos_marzo)

vars_utilizar <- intersect(variables_drift_pesos,intersect(variables_marzo,variables_mayo))

#En esta etapa se descartan las variables con valores negativos para el rankeo.

#### FEATURE ENGINEERING FINAL CON VARIABLES EN COMUN

# Cargamos el dataset
dataset <- fread("./datasets/competencia2_2022.csv.gz")

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

print(unique(dataset$foto_mes))

enero <- subset(dataset, foto_mes == 202101)
marzo <- subset(dataset, foto_mes == 202103)
mayo <- subset(dataset, foto_mes == 202105)
nrow(marzo)
nrow(mayo)
negativos_enero <- c()
negativos_marzo <- c()
negativos_mayo <- c()
variables_enero <- c()
variables_marzo <- c()
variables_mayo <- c()

for (campo in vars_utilizar) {
      if (min(marzo[, get(campo)]) >= 0) {
        marzo[, paste0("r__", campo, sep = "") := (frankv(marzo, cols = campo) - 1) / (length(marzo[, get(campo)]) - 1)] # rankeo entre 0 y 1
        marzo[, paste0(campo) := NULL] # elimino la variable original
        variables_marzo <- c(variables_marzo, campo)
      } 
      else {
        negativos_marzo <- c(negativos_marzo, campo)
      }
    }

for (campo in vars_utilizar) {
      if (min(mayo[, get(campo)]) >= 0) {
        mayo[, paste0("r__", campo, sep = "") := (frankv(mayo, cols = campo) - 1) / (length(mayo[, get(campo)]) - 1)] # rankeo entre 0 y 1
        mayo[, paste0(campo) := NULL] # elimino la variable original
        variables_mayo <- c(variables_mayo, campo)
        } 
      else {
        negativos_mayo <- c(negativos_mayo, campo)
      }
    }

for (campo in vars_utilizar) {
      if (min(enero[, get(campo)]) >= 0) {
        enero[, paste0("r__", campo, sep = "") := (frankv(enero, cols = campo) - 1) / (length(enero[, get(campo)]) - 1)] # rankeo entre 0 y 1
        enero[, paste0(campo) := NULL] # elimino la variable original
        variables_enero <- c(variables_enero, campo)
        } 
      else {
        negativos_enero <- c(negativos_enero, campo)
      }
    }

dataset <- rbind(marzo,mayo,enero)

unique(dataset$foto_mes)


print(colnames(dataset))
write.csv(dataset,"./datasets/competencia2_2022_fe.csv", row.names = FALSE)


