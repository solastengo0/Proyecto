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
  titlePanel("Gastos por Persona según ..."),
  tabsetPanel(
    tabPanel( "Motivo",
      sidebarLayout(
        sidebarPanel(
          selectInput('motivo', 'Seleccionar Motivo', unique(receptivo_fecha$Motivo))
          ),
        mainPanel(
          h2("Gastos por Persona según Motivo", align = "center"),
          plotOutput("gastosPlot")
          )
        )
      ),
  tabPanel("Nacionalidad", 
   sidebarLayout(
    sidebarPanel(
      selectInput('nacionalidad', 'Seleccionar nacionalidad', unique(receptivo_fecha$Pais))
    ),
    mainPanel(
      h2("Gastos por Persona según nacionalidad", align = "center"),
      plotOutput("gastosPlotn")
    )
  )
           ),
  tabPanel("Nivel de Educacion",
   sidebarLayout(
    sidebarPanel(
       selectInput('Educacion', 'Seleccionar nivel de educacion', unique(receptivo_fecha$Estudio))
     ),
    mainPanel(
      h2("Gastos por Persona según nivel de educacion", align = "center"),
       plotOutput("gastosPlote")
     )
   )
           ),
  tabPanel("Ocupacion",
   sidebarLayout(
    sidebarPanel(
       selectInput('ocupacion', 'Seleccionar ocupacion', unique(receptivo_fecha$Ocupacion))
   ),
   mainPanel(
     h2("Gastos por Persona según ocupacion", align = "center"),
      plotOutput("gastosPloto")
     )
   )
           ),
  tabPanel("Transporte de Ingreso",
   sidebarLayout(
    sidebarPanel(
       selectInput('ingreso', 'Seleccionar transporte de ingreso', unique(receptivo_fecha$`Transporte Internacional de Ingreso`))
   ),
  mainPanel(
    h2("Gastos por Persona según metodo de ingreso", align = "center"),
      plotOutput("gastosPloti")
     )
   )
           ),
  tabPanel("Transporte de Egreso",
   sidebarLayout(
    sidebarPanel(
      selectInput('egreso', 'Seleccionar metodo de egreso', unique(receptivo_fecha$`Transporte Internacional de Egreso`))
   ),
   mainPanel(
     h2("Gastos por Persona según metodo de ingreso", align = "center"),
        plotOutput("gastosPloteg")
             )
    ))
  )
)

server <- function(input, output) {
  output$gastosPlot <- renderPlot({
    datos_filtrados <- receptivo_fecha %>%    ##motivo
      filter(Motivo == input$motivo) %>%
      select(Motivo, all_of(columnas_gasto))
    
    datos_gathered <- datos_filtrados %>%
      gather(key = "TipoGasto", value = "Gasto", -Motivo) %>% 
      mutate(TipoGasto = case_match(
        TipoGasto,
        "GastoAlojamiento_porPersona" ~ "Gasto en Alojamiento",
        "GastoAlimentacion_porPersona" ~ "Gasto en Alimentacion",
        "GastoTransporte_porPersona" ~ "Gasto en Transporte", 
        "GastoCultural_porPersona" ~ "Gasto Cultural", 
        "GastoTours_porPersona" ~ "Gasto en Tours", 
        "GastoCompras_porPersona" ~ "Gasto en Compras", 
        "GastoOtros_porPersona" ~ "Otros Gastos"
      ))
    
    ggplot(data = datos_gathered, aes(x = reorder(TipoGasto, Gasto), y = Gasto, fill = TipoGasto)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Tipo de Gasto", y = "Gasto por Persona", 
           title = paste("Gastos por Persona para el Motivo:", input$motivo)) +
      theme(axis.text.x = element_blank())
      
  })
  
  output$gastosPlotn <- renderPlot({
    datos_filtrados <- receptivo_fecha %>% 
      filter(Pais == input$nacionalidad) %>%     ##nacionalidad
      select(Pais, all_of(columnas_gasto))
    
    datos_gathered <- datos_filtrados %>%
      gather(key = "TipoGasto", value = "Gasto", -Pais) %>% 
      mutate(TipoGasto = case_match(
        TipoGasto,
        "GastoAlojamiento_porPersona" ~ "Gasto en Alojamiento",
        "GastoAlimentacion_porPersona" ~ "Gasto en Alimentacion",
        "GastoTransporte_porPersona" ~ "Gasto en Transporte", 
        "GastoCultural_porPersona" ~ "Gasto Cultural", 
        "GastoTours_porPersona" ~ "Gasto en Tours", 
        "GastoCompras_porPersona" ~ "Gasto en Compras", 
        "GastoOtros_porPersona" ~ "Otros Gastos"
      ))
    
    ggplot(data = datos_gathered, aes(x = reorder(TipoGasto, Gasto), y = Gasto, fill = TipoGasto)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Tipo de Gasto", y = "Gasto por Persona", 
           title = paste("Gastos por Persona para la nacionalidad:", input$nacionalidad)) +
      theme(axis.text.x = element_blank())
  })
  
  output$gastosPlote <- renderPlot({
    datos_filtrados <- receptivo_fecha %>% 
      filter(Estudio == input$Educacion) %>%        ##educacion
      select(Estudio, all_of(columnas_gasto))
    
    datos_gathered <- datos_filtrados %>%
      gather(key = "TipoGasto", value = "Gasto", -Estudio) %>% 
      mutate(TipoGasto = case_match(
        TipoGasto,
        "GastoAlojamiento_porPersona" ~ "Gasto en Alojamiento",
        "GastoAlimentacion_porPersona" ~ "Gasto en Alimentacion",
        "GastoTransporte_porPersona" ~ "Gasto en Transporte", 
        "GastoCultural_porPersona" ~ "Gasto Cultural", 
        "GastoTours_porPersona" ~ "Gasto en Tours", 
        "GastoCompras_porPersona" ~ "Gasto en Compras", 
        "GastoOtros_porPersona" ~ "Otros Gastos"
      ))
    
    ggplot(data = datos_gathered, aes(x = reorder(TipoGasto, Gasto), y = Gasto, fill = TipoGasto)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Tipo de Gasto", y = "Gasto por Persona", 
           title = paste("Gastos por Persona para nivel de estudio:", input$Educacion)) +
      theme(axis.text.x = element_blank())
  })
  
  output$gastosPloto <- renderPlot({
    datos_filtrados <- receptivo_fecha %>% 
      filter(Ocupacion == input$ocupacion) %>%        ##ocupacion
      select(Ocupacion, all_of(columnas_gasto))
    
    datos_gathered <- datos_filtrados %>%
      gather(key = "TipoGasto", value = "Gasto", -Ocupacion) %>% 
      mutate(TipoGasto = case_match(
        TipoGasto,
        "GastoAlojamiento_porPersona" ~ "Gasto en Alojamiento",
        "GastoAlimentacion_porPersona" ~ "Gasto en Alimentacion",
        "GastoTransporte_porPersona" ~ "Gasto en Transporte", 
        "GastoCultural_porPersona" ~ "Gasto Cultural", 
        "GastoTours_porPersona" ~ "Gasto en Tours", 
        "GastoCompras_porPersona" ~ "Gasto en Compras", 
        "GastoOtros_porPersona" ~ "Otros Gastos"
      ))
    
    ggplot(data = datos_gathered, aes(x = reorder(TipoGasto, Gasto), y = Gasto, fill = TipoGasto)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Tipo de Gasto", y = "Gasto por Persona", 
           title = paste("Gastos por Persona para nivel de estudio:", input$ocupacion)) +
      theme(axis.text.x = element_blank())
  })
  
  output$gastosPloti <- renderPlot({
    datos_filtrados <- receptivo_fecha %>% 
      filter(`Transporte Internacional de Ingreso` == input$ingreso) %>%        ##ingreso
      select(`Transporte Internacional de Ingreso`, all_of(columnas_gasto))
    
    datos_gathered <- datos_filtrados %>%
      gather(key = "TipoGasto", value = "Gasto", -`Transporte Internacional de Ingreso`) %>% 
      mutate(TipoGasto = case_match(
        TipoGasto,
        "GastoAlojamiento_porPersona" ~ "Gasto en Alojamiento",
        "GastoAlimentacion_porPersona" ~ "Gasto en Alimentacion",
        "GastoTransporte_porPersona" ~ "Gasto en Transporte", 
        "GastoCultural_porPersona" ~ "Gasto Cultural", 
        "GastoTours_porPersona" ~ "Gasto en Tours", 
        "GastoCompras_porPersona" ~ "Gasto en Compras", 
        "GastoOtros_porPersona" ~ "Otros Gastos"
      ))
    
    ggplot(data = datos_gathered, aes(x = reorder(TipoGasto, Gasto), y = Gasto, fill = TipoGasto)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Tipo de Gasto", y = "Gasto por Persona", 
           title = paste("Gastos por Persona para nivel de estudio:", input$ingreso)) +
      theme(axis.text.x = element_blank())
  })
  
  output$gastosPloteg <- renderPlot({
    datos_filtrados <- receptivo_fecha %>% 
      filter(`Transporte Internacional de Egreso` == input$egreso) %>%        ##egreso
      select(`Transporte Internacional de Egreso`, all_of(columnas_gasto))
    
    datos_gathered <- datos_filtrados %>%
      gather(key = "TipoGasto", value = "Gasto", -`Transporte Internacional de Egreso`) %>% 
      mutate(TipoGasto = case_match(
        TipoGasto,
        "GastoAlojamiento_porPersona" ~ "Gasto en Alojamiento",
        "GastoAlimentacion_porPersona" ~ "Gasto en Alimentacion",
        "GastoTransporte_porPersona" ~ "Gasto en Transporte", 
        "GastoCultural_porPersona" ~ "Gasto Cultural", 
        "GastoTours_porPersona" ~ "Gasto en Tours", 
        "GastoCompras_porPersona" ~ "Gasto en Compras", 
        "GastoOtros_porPersona" ~ "Otros Gastos"
      ))
    
    ggplot(data = datos_gathered, aes(x = reorder(TipoGasto, Gasto), y = Gasto, fill = TipoGasto)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Tipo de Gasto", y = "Gasto por Persona", 
           title = paste("Gastos por Persona para nivel de estudio:", input$egreso)) +
      theme(axis.text.x = element_blank())
  })
}

shinyApp(ui, server) 
