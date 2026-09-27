library(shiny)
library(rAmCharts)
library(plotly)
library(tidyverse)
library(DT)
#commentaire
library(FactoMineR)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  metals_bees <- read_csv2("../data/metals_bees.csv")
  metals_bees <- metals_bees |> 
    mutate(Date = as.Date(Date, format="%d/%m/%Y"),
           Location = as.factor(Location), 
           Hive = factor(Hive, levels=c("B1", "N1", "B20", "C17", "C2", "C21", "B3", "N10"))) |> 
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
  
  ### Graphiques sur le dataset des taux de concentration de métaux dans les ruches
  palette_compar <- reactive({
    if (input$choix_compar_metal=="Location"){
      c(
        "Forest" = "#39A62B", 
        "Suburban" = "#1F2BF2", 
        "Urban"  = "#898AA3", 
        "Vineyard" = "#9719E6"
      )
    } else if (input$choix_compar_metal=="Hive"){
      c(
        "B1" = "#46CB34",
        "N1" = "#2D8221",
        
        "B20" = "#3943F3",
        "C17" = "#0C15C6",
        
        "C2" = "#A4A4B7",
        "C21" = "#70718F",
        
        "B3" = "#AA43EA",
        "N10" = "#7C15BC"
      )
    }
  })
  
  
  
  
  output$boxPlot_Metal <- renderPlotly({
    input$maj_g_explo
    metals_bees <- metals_bees |> 
      filter(Location %in% input$cases_fonciers)
      
    isolate({
      metals_bees <- metals_bees |> 
        arrange(Location) |> 
        rename(Metal = .data[[input$choix_metal]], Var_Compar = .data[[input$choix_compar_metal]]) |> 
        select(Metal, Var_Compar)
      
      ## Boxplot des taux du métal choisi selon le milieu
      plot_ly(metals_bees, 
              x = ~Var_Compar, 
              y = ~Metal, 
              color = ~Var_Compar,
              type = 'box', 
              colors=palette_compar()) |> 
        layout(
          title = paste("Boîte à moustaches de la concentration de", input$choix_metal, "selon", ifelse(input$choix_compar_metal=="Location", "le foncier", "les ruches d'abeilles")),
          yaxis = list(title = input$choix_metal), 
          xaxis = list(title = input$choix_compar_metal),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.2)
        )
      
    })
  })
  
  output$Tempo_Metal <- renderPlotly({
    input$maj_g_explo
    metals_bees <- metals_bees |> 
      filter(Location %in% input$cases_fonciers)
    
    isolate({
      metals_bees <- metals_bees |>
        arrange(Location) |> 
        rename(Metal = .data[[input$choix_metal]], Var_Compar = .data[[input$choix_compar_metal]]) |>
        group_by(Var_Compar, Date) |>
        summarize(Metal = mean(Metal)) |> 
        select(Var_Compar, Metal, Date)
      
      plot_ly(metals_bees, 
              x = ~Date, 
              y = ~Metal, 
              color = ~Var_Compar,
              type = 'scatter', 
              mode = 'lines+markers',
              colors=palette_compar()) |> 
        layout(
          title = paste("Evolution temporelle de la concentration de", input$choix_metal, "selon", ifelse(input$choix_compar_metal=="Location", "le foncier", "les ruches d'abeilles")),
          yaxis = list(title = input$choix_metal), 
          xaxis = list(title = "Date"),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.2)
        )
    })
  })
  
  
  
  ### Affichage des tableaux de données brutes
  output$table_Metaux <- renderDataTable({
    datatable(metals_bees)
  })
  
  output$table_Pest <- renderDataTable({
    # Affichage seulement des lignes ne contenant pas que des 0 dans jeu de données pesticides
    if (input$filtre_zero_pest){
      cols_num <- sapply(pest_bees, is.numeric)
      lignes_a_garder <- rowSums(pest_bees[,cols_num], na.rm = TRUE)>0
      pest_bees <- pest_bees[lignes_a_garder,]
    }
    datatable(pest_bees)
  })
  
  output$table_Sol <- renderDataTable({
    datatable(soil_occup)
  })
  
  output$table_Trait <- renderDataTable({
    datatable(treat_pest)
  })
})
