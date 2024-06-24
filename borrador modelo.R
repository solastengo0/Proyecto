datos <- read_excel(here("data","receptivo_fecha.xlsx"))

library(here)
library(tidyverse)
library(randomForest)
library(modelr)
library(readxl)

modelo <- datos %>%
  select(Motivo, Pais, Estudio, Ocupacion, `Transporte Internacional de Ingreso`, `Transporte Internacional de Egreso`, `Lugar Ingreso`, `Lugar Egreso`, Localidad, Alojamiento, TransporteLocal, Destino, GastoTotal_porPersona ) %>%
  mutate()


intrain <- sample(x = 1:nrow(modelo), size = nrow(modelo)*.7)
training <- modelo[intrain,]
testing <- modelo[-intrain,]

rf <- randomForest(GastoTotal_porPersona~ .,data= training)

rmse(rf, testing)

