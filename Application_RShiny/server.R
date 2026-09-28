

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
    # Affichage seulement des lignes ne contenant pas que des 0 dans jeu de données pesticides
    if (input$filtre_zero_pest){
      cols_num <- sapply(pest_bees, is.numeric)
      lignes_a_garder <- rowSums(pest_bees[,cols_num], na.rm = TRUE)>0
      pest_bees <- pest_bees[lignes_a_garder,]
    }
    datatable(pest_bees)
  })
  
  #Output du tableau de données des sols
  output$data_table_soil <- renderDataTable({
    soil_occup_output <- soil_occup |> 
      filter(Location %in% input$varloc1)
  })
  
  #Téléchargement des données sol 
  output$downloadData_soil <- downloadHandler(
    filename =('soil.occupation.3km.csv'),
    content = function(con) {
      write.csv(soil_occup, con)
    })
  
  #Affichage des pourcentages d'occupation des sols
  output$squarePie <- renderPlot({
    df_plot <- soil_occup |> 
      filter(Location %in% input$varloc2) |> 
      pivot_longer(
        cols = -Location,
        names_to = "Category",
        values_to = "Value"
      ) |> 
      mutate(Category = str_replace_all(Category, "[_.]", " ")) |> 
      mutate(Label_text = (paste(Category,"\n", round(Value), "%")))
    
    vec_fill=c("darkgrey", "lightgreen", "darkgreen", "darkred", "lightblue", "gold")
    names(vec_fill)=c("Urban fabric", "Grassland", "Forest", "Vineyards", "Water", "Crops Oilseed Grain")
    
    ggplot(df_plot, mapping=aes(area = Value , fill = Category, label = Label_text)) +
      # Dessine les rectangles de catégories
      geom_treemap()+
      # Ajoute les étiquettes de texte
      geom_treemap_text(colour = "white", place = "centre", size=20, grow= F) +
      scale_fill_manual(values = vec_fill) +
      theme(
        plot.title = element_text(face = "bold",
                                  size=18,
                                  hjust=0.5)
      ) +
      labs(title = "Poucentages d'occupation des sols pour")+
      facet_wrap(~ Location) 
    
  })
  
  #Affichage du jeu de données traitement
  output$data_table_treat <- renderDataTable({
    if (input$vartreat=="Date"){
      treat_pest_output <-  treat_pest |> 
        arrange(Date)
    }else{
      treat_pest_output <-  treat_pest |> 
        arrange(Molecule)
    }
    treat_pest_output
  })
  
  #Téléchargement des données 
  output$downloadtreat_pest <- downloadHandler(
    filename =('treatments_pesticides.csv'),
    content = function(con) {
      write.csv(treat_pest, con)
    })
  
  output$table_Test <- renderDataTable({
    datatable(data_consolidé)
  })
})

#Kenavo