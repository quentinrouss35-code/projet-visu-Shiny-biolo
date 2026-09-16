library(shiny)
library(rAmCharts)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  #output$distPlot <- renderPlot({
  
  # generate bins based on input$bins from ui.R
  #x    <- faithful[, input$var] 
  #bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  # draw the histogram with the specified number of bins
  #hist(x, breaks = bins, col = input$color, border = 'white', main = input$titre)
  
  
  #})
  data <- reactive({
    faithful[, input$var]
  })
  output$distPlot<-renderAmCharts({
    input$go_graph
    isolate({
      x <- data()
      bins<-round(seq(min(x), max(x),length.out=input$bins + 1),2)
      
      #useamHist
      amHist(x=x, control_hist=list(breaks=bins),
             col=input$color,main = input$titre,
             export=TRUE,zoom=TRUE)
    })
  })
  
  # summary
  output$summary <- renderPrint({
    summary(faithful)
  })
  
  # table
  output$table <- renderDataTable({
    faithful
  })
  
  # nombre de classe
  output$n_bins <- renderText({
    paste("Nombre de classes : ", input$bins)
  })
  
  #output$boxPlot <- renderPlot({
  #x <- faithful[, input$var]
  #boxplot(x, col=input$color)
  
  #})
  
  output$boxPlot <- renderAmCharts({
    input$go_graph
    isolate({
      x <- data()
      amBoxplot(x, col=input$color)
    })
  })
  
  output$nuage_points <- renderPlotly({
    input$go_graph
    isolate({
      plot_ly(data=faithful, x=~waiting, y=~eruptions, 
              marker = list(color=input$color, size=10))%>%
        layout(title = 'Manually Specified Labels', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Attente'), 
               yaxis = list(title = 'Eruptions'))
    })
  })
  observeEvent(input$go_graph,{
    updateTabsetPanel(session,inputId="viz",selected="Histogramme")
    
    observeEvent(input$show, {
      showModal(modalDialog(
        title = "Important message",
        "This is an important message!"
      ))
    })
  })
})
