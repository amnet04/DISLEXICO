
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
  
  output$diferencias <-  renderTable({   
    consolidado_diferencias
  })
 
  output$map_container <- renderMap({
    Layer=input$Layer
    print(input$Clusters)
    Clusters=as.numeric(input$Clusters)
    mapaInicial(tileLayer=Layer, clusters=Clusters)
  }) 
  
  
  output$dendrograma <- renderPlot({
    par(mar=c(2,0.5,0.5,2))
    r=as.dendrogram(cluster) %>% set("branches_lwd", 6)
    color_branches(r, 
                   k=as.numeric(input$Clusters), 
                   col=RColorBrewer::brewer.pal(12, 'Paired')[round(seq(from = 1, to = 12, by = 11/(as.numeric(input$Clusters)-1)))],
                   groupLabels = TRUE
                   )%>% plot(horiz=TRUE)
    rect.dendrogram(r, k=as.numeric(input$Clusters), horiz=TRUE)
    }, 
    height = 2500, width = 400
  ) 
  
})

