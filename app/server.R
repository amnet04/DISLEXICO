
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(rCharts)
library(dendextend)
library(dendextendRcpp)

options(shiny.maxRequestSize = 9*1024^2)


shinyServer(function(input, output, session){
  
  output$diferencias <-  renderDataTable({   
    cbind(Lugar = rownames(diferencias), diferencias)
  })
 
  output$similaridades <-  renderDataTable({   
    cbind(Lugar = rownames(similaridades), similaridades)
  })

  output$IIR <-  renderDataTable({   
    cbind(Lugar = rownames(IIR), IIR)
  })
  
  output$vacios <-  renderDataTable({   
    vacios
  })
  
  output$referenciaVacios <-  renderDataTable({
    cbind(Mapa = rownames(comVoidMaps), comVoidMaps)  
  })
  
  output$map_container <- renderMap({
    Layer=input$Layer
    Clusters=as.numeric(input$Clusters)
    if (input$Matriz=="diferencias"){
     dibujarMapa(tileLayer=Layer, clusters=Clusters, matriz=diferencias)
    }
    else if (input$Matriz=="similaridades"){
      dibujarMapa(tileLayer=Layer, clusters=Clusters, matriz=1-similaridades)
    }
    else if (input$Matriz=="IIR"){
      dibujarMapa(tileLayer=Layer, clusters=Clusters, matriz=IIR)
    }
  }) 
  
})

