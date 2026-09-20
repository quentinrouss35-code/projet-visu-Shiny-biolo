library(shiny)
library(rAmCharts)
library(plotly)
library(tidyverse)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  metals_bees <- read_csv2("../data/metals_bees.csv")
  metals_bees <- metals_bees |> 
    mutate(Date = as.Date(Date, format="%d/%m/%Y"),
           Location = as.factor(Location), 
           Hive = as.factor(Hive)) |> 
    mutate(`Aluminium mg/kg` = as.numeric(ifelse(grepl(c("^<"), `Aluminium mg/kg`), round(as.numeric(gsub(",", ".", substring(`Aluminium mg/kg`, 3)))/3,2), gsub(",", ".", `Aluminium mg/kg`)))) |>
    
    mutate(`Arsenic mg/kg` = as.numeric(ifelse(grepl(c("^<"), `Arsenic mg/kg`), round(as.numeric(gsub(",", ".", substring(`Arsenic mg/kg`, 3)))/3,2), gsub(",", ".", `Arsenic mg/kg`)))) |> 
    
    mutate(`Cadmium mg/kg` = as.double(ifelse(grepl(c("^<"), `Cadmium mg/kg`), round(as.numeric(gsub(",", ".", substring(`Cadmium mg/kg`, 3)))/3,2), gsub(",", ".", `Cadmium mg/kg`)))) |>
    
    mutate(`Chromium mg/kg` = as.double(ifelse(grepl(c("^<"), `Chromium mg/kg`), round(as.numeric(gsub(",", ".", substring(`Chromium mg/kg`, 3)))/3,2), gsub(",", ".", `Chromium mg/kg`)))) |>
    
    mutate(`Cobalt mg/kg` = as.double(ifelse(grepl(c("^<"), `Cobalt mg/kg`), round(as.numeric(gsub(",", ".", substring(`Cobalt mg/kg`, 3)))/3,2), gsub(",", ".", `Cobalt mg/kg`)))) |> 
    
    mutate(`Lead mg/kg` = as.double(ifelse(grepl(c("^<"), `Lead mg/kg`), round(as.numeric(gsub(",", ".", substring(`Lead mg/kg`, 3)))/3,2), gsub(",", ".", `Lead mg/kg`))))
  
  pest_bees <- read_csv2("../data/pesticide_bees.csv")
  pest_bees <- pest_bees |> 
    mutate(Date = as.Date(Date, format="%d/%m/%Y"),
           Location = as.factor(Location), 
           Hive = as.factor(Hive))
  pest_bees[is.na(pest_bees)] <- 0
  
  soil_occup <- read_csv2("../data/soil.occupation.3km.csv")
  soil_occup <- soil_occup |> 
    mutate(Location = as.factor(Location))
  
  treat_pest <- read_csv2("../data/treatments_pesticides.csv")
  treat_pest <- treat_pest |> 
    mutate(Date = as.Date(Date, format="%d/%m/%Y"),
           Molecule = as.factor(Molecule))
  
  
  # Analyse descriptive
  
  ## Analyse exploratoire
  palette_compar <- reactive({
    if (input$choix_compar_metal=="Location"){
      c(
        "Forest" = "green", 
        "Suburban" = "blue", 
        "Urban"  = "grey", 
        "Vineyard" = "purple"
      )
    } else if (input$choix_compar_metal=="Hive"){
      c(
        "B1" = "darkgreen",
        "N1" = "lightgreen",
        
        "B20" = "darkblue",
        "C17" = "lightblue",
        
        "C2" = "darkgrey",
        "C21" = "lightgrey",
        
        "B" = "purple4",
        "N10" = "purple1"
      )
    }
  })
  
  output$boxPlot_Metal <- renderAmCharts({
    input$maj_g_explo
    
    isolate({
      
      metals_bees <- metals_bees |> 
        rename(Metal = .data[[input$choix_metal]], Var_Compar = .data[[input$choix_compar_metal]]) |> 
        select(Metal, Var_Compar)
      
      ## Boxplot des taux du métal choisi selon le milieu
      amBoxplot(Metal~Var_Compar, data=metals_bees, col=palette_compar())
      
    })
  })
  
  output$Tempo_Metal <- renderPlotly({
    input$maj_g_explo
    isolate({
      
      metals_bees <- metals_bees |> 
        rename(Metal = .data[[input$choix_metal]], Var_Compar = .data[[input$choix_compar_metal]]) |> 
        select(Var_Compar, Metal, Date)
      
      plot_ly(metals_bees, 
              x = ~Date, 
              y = ~Metal, 
              color = ~Var_Compar,
              type = 'scatter', 
              mode = 'lines+markers',
              colors=palette_compar()) |> 
        layout(
          yaxis = list(title = input$choix_metal), 
          xaxis = list(title = ""),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.2)
        )
    })
  })
  
  ## Sous-onglet :  
  data_tél <- reactive({
    choix <- input$choix_tab
    if(choix == "métaux"){
      metals_bees
    } else if (choix == "pesticides") {
      pest_bees
    } else if (choix == "occupation du sol") {
      soil_occup
    } else {
      treat_pest
    }
  })

  
  ### Table
  output$table <- renderDataTable({
    datatable(data_tél())
  })
  
  output$tél_tab <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data_tél(), file)
    }
  )
})
