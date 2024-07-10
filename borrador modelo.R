datos <- read_excel(here("data","receptivo_fecha.xlsx"))

library(here)
library(tidyverse)
library(randomForest)
library(modelr)
library(readxl)
library(rpart)
library(mice)

variablesElegidas <- c("GastoTotal_porPersona", "Motivo", "Estudio", "Alojamiento", "Ocupacion", "TransporteLocal")
datos2 <- datos[variablesElegidas]

set.seed(33) 

entrenamiento <- sample(1:nrow(datos2), 0.7 * nrow(datos2))  
desarrollo <- datos2[entrenamiento, ]
testeo <- datos2[-entrenamiento, ]

desarrollo <- na.omit(desarrollo)
testeo <- na.omit(testeo)

#arbol

arbol <- rpart(GastoTotal_porPersona ~ ., data = desarrollo, method = "anova")

arbol_predict <- predict(arbol, testeo)
t_arbol <- table(testeo$GastoTotal_porPersona, arbol_predict)


#bosque

bosque <- randomForest(GastoTotal_porPersona ~ ., data = desarrollo)

bosque_predict <- predict(bosque, testeo)
t_bosque <- table(testeo$GastoTotal_porPersona, bosque_predict)

importancia_variables <- importance(bosque)
importancia_variables 

#evaluacion del modelo
#EMC (error medio cuadratico)

mean_gasto <- mean(datos2$GastoTotal_porPersona, na.rm = TRUE)


emc_arbol <- mean((testeo$GastoTotal_porPersona - arbol_predict)^2)
emc_bosque <- mean((testeo$GastoTotal_porPersona - bosque_predict)^2)

cat("EMC Árbol de Regresión: ", emc_arbol, "\n")
cat("EMC Bosque Aleatorio: ", emc_bosque, "\n")

################################################################################################

importancia_variables <- importance(bosque)
importancia_variables 

#Probamos modelo solo con alojamiento y motivo


variablesb <- c("GastoTotal_porPersona", "Motivo", "Alojamiento")
datos2b <- datos[variablesb]

set.seed(66) 

entrenamientob <- sample(1:nrow(datos2b), 0.7 * nrow(datos2b))  
desarrollob <- datos2b[entrenamientob, ]
testeob <- datos2b[-entrenamientob, ]

desarrollob <- na.omit(desarrollob)
testeob <- na.omit(testeob)

#arbolb

arbolb <- rpart(GastoTotal_porPersona ~ ., data = desarrollob, method = "anova")

arbol_predictb <- predict(arbolb, testeob)
t_arbolb <- table(testeob$GastoTotal_porPersona, arbol_predictb)


#bosqueb

bosqueb <- randomForest(GastoTotal_porPersona ~ ., data = desarrollob)

bosque_predictb <- predict(bosqueb, testeob)
t_bosqueb <- table(testeob$GastoTotal_porPersona, bosque_predictb)

#evaluacion del modelo
#EMC (error medio cuadratico)


emc_arbolb <- mean((testeob$GastoTotal_porPersona - arbol_predictb)^2)
emc_bosqueb <- mean((testeob$GastoTotal_porPersona - bosque_predictb)^2)

cat("EMC Árbol de Regresión: ", emc_arbolb, "\n")
cat("EMC Bosque Aleatorio: ", emc_bosqueb, "\n")









