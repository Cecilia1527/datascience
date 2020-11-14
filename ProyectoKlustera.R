# limpiar 
rm(list=ls())


#Instalaci髇 de paquetes


library(C50)
library(gmodels)
library(dplyr)
library(ggplot2)
require(class)
require(gmodels)
require(dplyr)
library(tidyverse)

#directorio de ruta de BD
dir1 <- "C:\\Users\\Dis22442\\Documents\\DREVF\\klustera\\klustera"
tempo1 <- read.csv(paste(dir1, "e.csv", sep="/"), stringsAsFactors = TRUE)



str(tempo1)
tempo1 <- tempo1[-1]
str(tempo1)

#sum(is.na(tempo1))

#vis_miss(tempo1)

#delete.na <- function(tempo1, n=" ") {
 # tempo1[rowSums(is.na(tempo1)) <= n,]
#}

#tabla con visitor
#tempo1$visitor <- as.character(tempo1$visitor)
#str(tempo1$visitor)

#Codificaci髇 de visitor como "factor, el objetivo debe de ser un factor

tempo1$visitor <- factor(tempo1$visitor, levels = c("true", "false"), 
                         labels = c("visitante", "NOvisitante"))

# Tabla a porcentajes
round(prop.table(table(tempo1$visitor)) * 100, digits = 1)

summary(tempo1[c("tiempodeses", "day_tz", "hour_tz")])


# Determinamos que se ocupa normalizar los valores
# Funcion para normalizar los valores de 0 a 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#normalize(c(2:6))
#quitar na
#tempo1$visitor <- ifelse(is.na(tempo1$visitor, tempo1$device_mac, tempo1$branch_office,
 #                              tempo1$month_tz, tempo1$day_tz, tempo1$day_of_week_tz), ave_age, empo1$visitor, tempo1$device_mac, tempo1$branch_office,
  #                       tempo1$month_tz, tempo1$day_tz, tempo1$day_of_week_tz)


tempo1_n <- as.data.frame(lapply(tempo1[c(4,6,8)], normalize)) #list apply
head(tempo1_n)

#separaci髇 de BD entrenamiento vs prueba
nfilas <- floor(nrow(tempo1_n)) * .80
set.seed(123)
index <- sample(1:nrow(tempo1_n), nfilas) # 80% #SAMPLE HACE UNA SECUENCIA DE NU. ALEATORIOS
tempo1_train <- tempo1_n[index, ] #tenia tempo1_n ?? # Obtener solo las muestras
tempo1_test <- tempo1_n[-index, ] # Todo menos las muestras

#nombramos los tempo1 index
tempo1_train_labels <- tempo1[index, 7]
tempo1_test_labels <- tempo1[-index,7]
str(tempo1_train_labels)


## ---------- Cargamos los datos a la funci贸n knn
# cl = factor de clasificaci贸n
# k = n煤mero de vecinos a usar 
# - preferentemente usar un n煤mero impar para romper empates
# - La raiz cuadrada de las 249556 es 499.55, tomamos k = 499
tempo1_test_pred <- knn(train = tempo1_train, test = tempo1_test, cl = tempo1_train_labels, k = 200)  #modelo KNN
#tempo1_model <- C5.0(tempo1_train, tempo1_train_labels)

## ----------- Evaluamos los resultados del modelo 
# Creamos una tabla para compara predicciones vs real
CrossTable(x = tempo1_test_labels, y = tempo1_test_pred, prop.r=FALSE, prop.chisq = FALSE)

#################
#
# Comparativo de la BD v. csv  agregar columna de visitantes
#
#################
dir2 <- "C:\\Users\\Dis22442\\Documents\\DREVF\\klustera\\klustera"
tempo2 <- read.csv(paste(dir2, "v.csv", sep="/"), stringsAsFactors = TRUE)

str(tempo2)
#quitando la columana primera
tempo2 <- tempo2[-1]
str(tempo2)

tempo2$visitor <- visitor
#sum(is.na(tempo1))
#tempo3 <- c(tempo1[8],tempo2)
#str(tempo3)

tempo2 <- tempo2[-1]
str(tempo2)
tempo2=cbind(tempo1, visitor =rep("visitor", 249556))
str(tempo2)

#tempo2$visitor 


summary(tempo2[c("tiempodeses", "day_tz", "hour_tz")])

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

tempo2_n <- as.data.frame(lapply(tempo2[c(4,6,8)], normalize)) #list apply
head(tempo2_n)

#separaci髇 de BD entrenamiento vs prueba
nfilas <- floor(nrow(tempo2_n)) * .80
set.seed(123)
index <- sample(1:nrow(tempo2_n), nfilas) # 80% #SAMPLE HACE UNA SECUENCIA DE NU. ALEATORIOS
tempo2_train <- tempo2_n[1:8100 , -7] #tenia tempo1_n ?? # Obtener solo las muestras
tempo2_test <- tempo2_n[81001:90000, -7 ] # Todo menos las muestras

#nombramos los index

tempo2_train_labels <- tempo2[index, 7]
tempo2_test_labels <- tempo2[-index,7]
str(tempo2_train_labels)

# Cargamos los datos a la funci贸n knn
# cl = factor de clasificaci贸n
# k = n煤mero de vecinos a usar 
# - preferentemente usar un n煤mero impar para romper empates
# - La raiz cuadrada de las 249556 es 499.55, tomamos k = 499
tempo2_test_pred <- knn(train = tempo2_train, test = tempo2_test, cl = tempo2_train_labels, k = 200)  #modelo KNN
#tempo1_model <- C5.0(tempo1_train, tempo1_train_labels)

## ----------- Evaluamos los resultados del modelo 
# Creamos una tabla para compara predicciones vs real
CrossTable(x = tempo2_test_labels, y = tempo2_test_pred, prop.r=FALSE, prop.chisq = FALSE)

tempo2$visitor <- tempo2_test_pred

write.csv(tempo2, paste(dir1, "new_v.csv", sep = "/"), fileEncoding = "UTF-8")
