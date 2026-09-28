

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
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
    datatable(pest_bees)
  })
  
  output$table_Sol <- renderDataTable({
    datatable(soil_occup)
  })
  
  output$table_Trait <- renderDataTable({
    datatable(treat_pest)
  })
})

#Kenavo