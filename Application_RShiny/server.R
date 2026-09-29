

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
  
  
  
  ### Choix de l'affichage des métaux 
  data_mod_metaux <- reactive({
    validate(need(length(input$varmet1) > 0, "Sélectionner au moins un métal."))
    
    if('Tous' %in% input$varmet1){
      validate(need(length(input$varmet1) < 2, "Décocher 'Tous' pour sélectionner des métaux."))
      metals_bees
    }
    else{
      metals_output <- metals_bees |> 
        select(all_of(input$varmet1))}
    
  })
  
  #Affichage des métaux
  output$table_Metaux <- renderDataTable({
    data_mod_metaux()
  })
  
  #Téléchargement des métaux
  output$downloadData_metal <- downloadHandler(
    filename =('residus_de_metaux.csv'),
    content = function(con) {
      d <- data_mod_metaux()
      write.csv(d, con)
    })
  
  ##Partie des résidus de pesticides
  # Affichage seulement des lignes ne contenant pas que des 0 dans jeu de données pesticides
  data_mod_pest <- reactive({
    if (input$filtre_zero_pest){
      cols_num <- sapply(pest_bees, is.numeric)
      lignes_a_garder <- rowSums(pest_bees[,cols_num], na.rm = TRUE)>0
      pest_bees <- pest_bees[lignes_a_garder,]
    }else{
      pest_bees
    }
  })
  
  #Affichage du tableau de données résidus de pesticide
  output$table_Pest <- renderDataTable({
    data_mod_pest()
  })
  
  #Téléchargement des données résidus de pesticide
  output$downloadData_pest <- downloadHandler(
    filename =('residus_de_pesticides.csv'),
    content = function(con) {
      d <- data_mod_pest()
      write.csv(d, con)
    })
  
  ##Partie données de sol 
  #Modif utilisateur du tableau de données des sols
  data_mod_sol <- reactive({
    validate(need(length(input$varloc1) > 0, "Sélectionner au moins une localisation."))
    
    soil_occup_output <- soil_occup |> 
      filter(Location %in% input$varloc1)     # locations gardée
  })
  
  #Output du tableau de données des sols
  output$data_table_soil <- renderDataTable({
    data_mod_sol()
  })
  
  #Téléchargement des données sol 
  output$downloadData_soil <- downloadHandler(
    filename =('occupation_sols_loc.csv'),
    content = function(con) {
      d <- data_mod_sol()
      write.csv(d, con)
    })
  
  #Affichage des pourcentages d'occupation des sols
  df_plot_sol <- reactive({
    validate(need(length(input$varloc2) > 0, "Sélectionner au moins une variable."))
    df_plot_sol <- soil_occup |> 
      filter(Location %in% input$varloc2) |> 
      pivot_longer(
        cols = -Location,
        names_to = "Category",
        values_to = "Value"
      ) |> 
      mutate(Category = str_replace_all(Category, "[_.]", " ")) |> 
      mutate(Label_text = (paste(Category,"\n", round(Value), "%")))
    
  })
  
  output$squarePie <- renderPlot({
    
    vec_fill=c("darkgrey", "lightgreen", "darkgreen", "darkred", "lightblue", "gold")
    names(vec_fill)=c("Urban fabric", "Grassland", "Forest", "Vineyards", "Water", "Crops Oilseed Grain")
    
    ggplot(df_plot_sol(), mapping=aes(area = Value , fill = Category, label = Label_text)) +
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
  
  ##Partie traitmement pesticide 
  #Modif utilisateur du tableau de données des traitements
  data_mod_treat <- reactive({
    validate(need(length(input$vartreat) > 0, "Sélectionner au moins une variable."))
    
    if (input$vartreat=="Date"){
      treat_pest_output <-  treat_pest |> 
        arrange(Date)
    }else{
      treat_pest_output <-  treat_pest |> 
        arrange(Molecule)
    }     
  })
  
  #Affichage du jeu de données traitement
  output$data_table_treat <- renderDataTable({
    data_mod_treat()
  })
  
  #Téléchargement des données traitement
  output$downloadTreat_pest <- downloadHandler(
    filename =('traitements_pesticide.csv'),
    content = function(con) {
      d <- data_mod_treat()
      write.csv(d, con)
    })
  
  output$table_Test <- renderDataTable({
    datatable(data_consolidé)
  })
})

#Kenavo