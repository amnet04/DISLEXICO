
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


shinyUI(
  navbarPage("DialectALEC ",
    tabPanel("Mapas",
             
      tabsetPanel(
        
        tabPanel("Mapas de distribución general",
          fluidPage(    
            fluidRow(
              column( width=2,
                
                selectInput("Layer", label = "Elija el mapa de fondo", 
                  choices = list(
                    "Open Street Map, transport" = "http://a.tile.thunderforest.com/transport/{z}/{x}/{y}.png", 
                    "MapQuest" = "http://otile4.mqcdn.com/tiles/1.0.0/osm/{z}/{x}/{y}.png",
                    "Ninguno" = "nada"), 
                      selected = 2
                  ),
                                                   
                selectInput("Clusters", label = "Elija el número de agrupaciones", 
                  choices = kgroups_choices, 
                  selected = 13
                ),
                                                   
                selectInput("Matriz", label = "Elija el tipo de análisis", 
                  choices = list(
                    "Diferencias léxicas" = "diferencias", 
                    "Similaridades léxicas" = "similaridades",
                    "Identidad relativa" = "IIR"
                  ), 
                  selected = "diferencias"
                )
              ),
                                           
              column( 
                width=6,
                chartOutput('map_container', 'leaflet')
              ),
                                           
              column(
                width=4, 
                tags$h3("Dendrograma"), 
                HTML('<div id="dendrograma" style="width:100%; height:560px; overflow: scroll;">'), 
                HTML('</div>')
              )   
            )
          ) 
        ),
                            
        tabPanel("Mapas de comparacion por lugar",
          fluidPage(    
            fluidRow(
              column( width=2,
                           
                selectInput("LayerLoc", label = "Elija el mapa de fondo", 
                  choices = list(
                  "Open Street Map, transport" = "http://a.tile.thunderforest.com/transport/{z}/{x}/{y}.png", 
                  "MapQuest" = "http://otile4.mqcdn.com/tiles/1.0.0/osm/{z}/{x}/{y}.png",
                  "Ninguno" = "nada"
                  ), 
                  selected = 2
                ),
                           
                selectInput("ClustersLoc", label = "Elija el número de agrupaciones", 
                  choices = kgroups_choices, 
                  selected = 13
                ),
                           
                selectInput("MatrizLoc", label = "Elija el tipo de análisis", 
                  choices = list(
                    "Diferencias léxicas" = "diferencias", 
                    "Similaridades léxicas" = "similaridades",
                    "Identidad relativa" = "IIR"
                  ), 
                selected = "diferencias"
                )
              ),
                   
              column( 
                width=6,
                chartOutput('map_container_loc', 'leaflet')
              ),
                   
              column(
                width=4, 
                tags$h3("Histograma"), 
                HTML('<div id="dendrograma" style="width:100%; height:560px; overflow: scroll;">'), 
                HTML('</div>')
               )   
              )
            ) 
          )
        )
      ),
                   
      tabPanel(
        "Tablas",  
        tabsetPanel(
          tabPanel("Matriz de diferencias", dataTableOutput('diferencias')),
          tabPanel("Matriz de similaridad", dataTableOutput('similaridades')),
          tabPanel("Indice general de identidad",dataTableOutput('IIR')),
          tabPanel("Vacios por localidad",dataTableOutput('vacios')),
          tabPanel("Referencia de vacios comunes",dataTableOutput('referenciaVacios'))
      )       
    )
  )
)


