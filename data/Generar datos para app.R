library(readxl)
receptivo_fecha <- read_excel("data/receptivo_fecha.xlsx")
library(tidyverse)
datos <- receptivo_fecha %>% 
  select(GastoAlojamiento_porPersona, 
         GastoAlimentacion_porPersona, 
         GastoTransporte_porPersona, 
         GastoCultural_porPersona, 
         GastoTours_porPersona,
         GastoCompras_porPersona,
         GastoOtros_porPersona,
         Motivo,
         Pais,
         Estudio,
         Ocupacion,
         `Transporte Internacional de Ingreso`,
         `Transporte Internacional de Egreso`)

install.packages("writexl")
library(writexl)

write_xlsx(datos, path = "datos_filtrados.xlsx")
