library(shiny)
library(rAmCharts)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  metals_bees <- read_csv2("C:/Users/Quent/master2/projet-visu-Shiny-biolo/data/metals_bees.csv")
  metals_bees <- metals_bees |> 
    mutate(Date = as.Date(Date, format="%d/%m/%Y"))
  
  pest_bees <- read_csv2("C:/Users/Quent/master2/projet-visu-Shiny-biolo/data/pesticide_bees.csv")
  pest_bees <- pest_bees |> 
    mutate(Date = as.Date(Date, format="%d/%m/%Y"))
  pest_bees[is.na(pest_bees)] <- 0
  
  soil_occup <- read_csv2("C:/Users/Quent/master2/projet-visu-Shiny-biolo/data/soil.occupation.3km.csv")
  
  treat_pest <- read_csv2("C:/Users/Quent/master2/projet-visu-Shiny-biolo/data/treatments_pesticides.csv")
  treat_pest <- treat_pest |> 
    mutate(Date = as.Date(Date, format="%d/%m/%Y"))
  
  
  
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
  
  # table
  output$table <- renderDataTable({
    metals_bees
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
