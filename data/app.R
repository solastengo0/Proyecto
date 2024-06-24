library(shiny)
library(tidyverse)
library(readxl)
library(here)

receptivo_fecha <- read_excel(here("data/receptivo_fecha.xlsx"))

columnas_gasto <- c("GastoAlojamiento_porPersona", "GastoAlimentacion_porPersona", 
                    "GastoTransporte_porPersona", "GastoCultural_porPersona", 
                    "GastoTours_porPersona", "GastoCompras_porPersona", 
                    "GastoOtros_porPersona")

ui <- fluidPage(
  titlePanel("Gastos por Persona según Motivo"),
  sidebarLayout(
    sidebarPanel(
      selectInput('motivo', 'Seleccionar Motivo', unique(receptivo_fecha$Motivo))
    ),
    mainPanel(
      h2("Gastos por Persona según Motivo", align = "center"),
      plotOutput("gastosPlot")
    )
  )
)

server <- function(input, output) {
  output$gastosPlot <- renderPlot({
    datos_filtrados <- receptivo_fecha %>% 
      filter(Motivo == input$motivo) %>%
      select(Motivo, all_of(columnas_gasto))
    
    datos_gathered <- datos_filtrados %>%
      gather(key = "TipoGasto", value = "Gasto", -Motivo)
    
    ggplot(data = datos_gathered, aes(x = reorder(TipoGasto, Gasto), y = Gasto, fill = TipoGasto)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Tipo de Gasto", y = "Gasto por Persona", 
           title = paste("Gastos por Persona para el Motivo:", input$motivo),
           legend = "Gasto en Alojamiento", "Gasto en Alimentación", 
                      "Gasto en Transporte", "Gasto en Cultural", 
                      "Gasto en Tours", "Gasto en Compras", 
                      "Otros Gastos") +
      theme(axis.text.x = element_blank())
      
  })
}

shinyApp(ui, server) 
