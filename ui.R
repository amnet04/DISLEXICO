
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyUI(navbarPage("DialectALEC Vresión 0.0 (Dialectología basada en DIStribución LÉXica en COlombia, DISLEXICO)",
  tabPanel("Distribuciónes dialectales",
    fluidPage(    
      fluidRow(
         column( width=2,
          
        selectInput("Layer", label = "Elija el mapa de fondo", 
                    choices = list(
                      "Open Street Map, transport" = "http://a.tile.thunderforest.com/transport/{z}/{x}/{y}.png", 
                      "MapQuest" = "http://otile4.mqcdn.com/tiles/1.0.0/osm/{z}/{x}/{y}.png",
                      "Ninguno" = "nada"), 
                    selected = 2),
        
        selectInput("Clusters", label = "Elija el número de agrupaciones", 
                    choices = c(
                      "2" = 2, 
                      "3" = 3,
                      "4" = 4,
                      "5" = 5, 
                      "6" = 6,
                      "7" = 7,
                      "8" = 8, 
                      "9" = 9,
                      "10" = 10,
                      "11" = 11, 
                      "12" = 12                      
                    ), 
                    selected = 12),
        
        selectInput("Layer", label = "Elija el tipo de análisis", 
                    choices = list(
                      "Diferencias léxicas" = "diferencias", 
                      "Similaridades léxicas" = "similaridades",
                      "Identidad relativa" = "identidad"                     
                    ), 
                    selected = 12)
        ),
        
        column( width=6,
          tabsetPanel(
            tabPanel("Mapa", 
                     mapOutput('map_container'),
                     absolutePanel(
                       id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
                       top = 150, left = "auto", right = 617, bottom = "auto",
                       width = 90, height = "auto",
                       HTML('<i style="float: left; height: 18px ;margin-right: 8px;
                            width: 18px; background:'),
                       RColorBrewer::brewer.pal(12, 'Paired')[1],
                       HTML('"></i>')
                     )
                     
            ),
            
            tabPanel("Matriz de diferencias", tableOutput('diferencias')),
            tabPanel("Matriz de similaridad"),
            tabPanel("Indice general de identidad")
          )       
        ),
        
        column(width=4, 
              tags$h3("Dendrograma"), 
              HTML('<div id="dendrograma" style="width:100%; height:560px; overflow: scroll;">'), 
              plotOutput('dendrograma', width = "100%"),
              HTML('</div>')
        )


      )
     
      
    ) 
  
  )

))



