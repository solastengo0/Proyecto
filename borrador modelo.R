


library(here)
library(tidyverse)
library(randomForest)
library(modelr)
library(readxl)
library(rpart)
library(mice)

datos <- read_excel(here("data","receptivo_fecha.xlsx"))
datos$GastoTotal_porPersona = datos$GastoTotal

variablesElegidas <- c("GastoTotal_porPersona", "Motivo", "Estudio", "Alojamiento", "Ocupacion", "TransporteLocal")
datos2 <- datos[variablesElegidas]
datos2[is.na(datos2$Estudio),'Estudio'] = 'Sin informacion'

set.seed(33) 

entrenamiento <- sample(1:nrow(datos2), 0.7 * nrow(datos2))  
desarrollo <- datos2[entrenamiento, ]
testeo <- datos2[-entrenamiento, ]

#desarrollo <- na.omit(desarrollo)
#testeo <- na.omit(testeo)

#arbol

arbol <- rpart(GastoTotal_porPersona ~ ., data = desarrollo)

# Entrenamiento

arbol_predict_training = predict(arbol, desarrollo)
error_training = sqrt(mean((arbol_predict_training-desarrollo$GastoTotal_porPersona)^2))

# En términos de la escala no parece muygrande

df = data.frame(
  predicciones = arbol_predict_training,
  valores_reales = desarrollo$GastoTotal_porPersona
)


# Vemos que el modelo subestima el gasto!!!!
ggplot(
  df,
  aes(
    x = predicciones,
    y = valores_reales
  )
) + 
geom_point()



## El árbol no esta captando las relaciones de variables



# Revisar test

arbol_predict <- predict(arbol, testeo)
error_test = sqrt(mean((arbol_predict-testeo$GastoTotal_porPersona)^2))

df = data.frame(
  predicciones = arbol_predict,
  valores_reales = testeo$GastoTotal_porPersona
)


# pasa lo mismo en test

ggplot(
  df,
  aes(
    x = predicciones,
    y = valores_reales
  )
) + 
  geom_point()


# No es un problema de clásificación no se debe hacer un table!!

#bosque

#bosque <- randomForest(GastoTotal_porPersona ~ ., data = desarrollo)

bosque = ranger::ranger(GastoTotal_porPersona ~ ., data = desarrollo)

# Muy pooco ajuste (ver r2)

## Vamos a ver si con un tuneo sirve (aunque es entendible, con variables categorias podria ser muy dificil)

tune_grid <- expand.grid(
  mtry = c(2, 3, 4, 5),  # Número de variables consideradas en cada división
  splitrule = "variance", 
  min.node.size = c(1, 5, 10)  # Tamaño mínimo del nodo
)

library(caret)
library(ranger)

# Configuración de control de entrenamiento (lo dejo como ultima instancia)
control <- trainControl(
  method = "cv",
  number = 5,
  search = "grid"
)



modelo_tuneado <- train(
  GastoTotal_porPersona ~ .,
  data = desarrollo,
  method = "ranger",
  trControl = control,
  tuneGrid = tune_grid,
  metric = "RMSE"
)


# El ajuste sigue dando mal... esto puede ser una conclusión también expliquen los pasos que hicieron
# e interpreten xq el modelo esta dando mal (que subestima los valores del gasto) posibles mejoras que si 
# tuvieran X datos podrian mejorar, etc.

bosque_predict <- predict(bosque, testeo)

error_test = sum(bosque_predict)

#t_bosque <- table(testeo$GastoTotal_porPersona, bosque_predict) (No es clasificación)

importancia_variables <- importance(bosque)
importancia_variables 

#evaluacion del modelo





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


##ejemplo

prueba <- data.frame(Motivo = "Ocio y vacaciones", Estudio = "Secundaria completa", Alojamiento = "Vivienda arrendada", Ocupacion = "Estudiante", TransporteLocal = "Taxi - Bus")
arbol_predictp <- predict(arbol, newdata = prueba)

print(arbol_predictp)



