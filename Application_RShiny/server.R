# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # Analyse descriptive
  
  ## Analyse exploratoire
  
  ##Map des sites
  output$Carte <- renderLeaflet({
    url <- "https://geo.api.gouv.fr/departements/33/communes?format=geojson&geometry=contour"
    communes <- st_read(url)
    
    loc <- c("Saint-Magne-de-Castillon", "Le Porge", "Villenave-d'Ornon", "Gujan-Mestras")
    
    communes_bees <- communes[communes$nom %in% loc,]
    
    communes_bees <- communes_bees |>
      mutate(
        type_localisation = case_when(
          nom == "Saint-Magne-de-Castillon" ~ "Vignoble",
          nom == "Le Porge"                 ~ "Forêt",
          nom == "Villenave-d'Ornon"        ~ "Urbain",
          nom == "Gujan-Mestras"            ~ "Péri-urbain"
        ),
        nom_ruche = case_when(
          nom == "Saint-Magne-de-Castillon" ~ "B3 et N10",
          nom == "Le Porge"                 ~ "B1 et N1",
          nom == "Villenave-d'Ornon"        ~ "C2 et C21",
          nom == "Gujan-Mestras"            ~ "B20 et C17"
        )
      )
    
    couleurs <- c("Forêt"       = "#39A62B",
                  "Péri-urbain" = "#1F2BF2",
                  "Urbain"      = "#898AA3",
                  "Vignoble"    = "#9719E6")
    
    pts <- communes_bees|>
      st_transform(2154) |>
      st_point_on_surface() |>
      st_transform(4326)
    
    pts$couleurs <- couleurs[pts$type_localisation]
    
    leaflet(pts) |>
      addTiles() |>
      addMarkers(
        label = ~lapply(
          paste0("<div style='background-color:", couleurs, ";",
                 "color:white; font-size:16px; padding:6px 10px;",
                 "border-radius:4px;'>",
                 "<b>Site :</b> ", type_localisation,
                 "<br>",
                 "<b>Ruches :</b> ", nom_ruche),
          htmltools::HTML
        )
      )
    
    
  })
  
  ### Graphiques sur le dataset des taux de concentration de métaux dans les ruches
  
  #### Définition des palettes de couleur pour le foncier et les ruches
  palette_foncier <- c(
    "Forest" = "#39A62B", 
    "Suburban" = "#1F2BF2", 
    "Urban"  = "#898AA3", 
    "Vineyard" = "#9719E6"
  )
  
  palette_ruche <- c(
    "B1" = "#46CB34",
    "N1" = "#2D8221",
    
    "B20" = "#3943F3",
    "C17" = "#0C15C6",
    
    "C2" = "#A4A4B7",
    "C21" = "#70718F",
    
    "B3" = "#AA43EA",
    "N10" = "#7C15BC"
  )
  
  palette_compar <- reactive({
    if (input$choix_compar_metal=="Location"){
      palette_foncier
    } else if (input$choix_compar_metal=="Hive"){
      palette_ruche
    }
  })
  
  metals_bees_explo <- reactive({
    df <- metals_bees |> 
      filter(Location %in% input$cases_fonciers,
             Date >= input$Explo_selectDate[1], Date <= input$Explo_selectDate[2]) |> 
      arrange(Location) |> 
      rename(Metal = .data[[input$choix_metal]], Var_Compar = .data[[input$choix_compar_metal]])
      
    df
  })
  
  
  output$titre_boxPlotMetal <- renderText({
    metal <- tolower(str_split(input$choix_metal, " ")[[1]][1])
    conc <- str_split(input$choix_metal, " ")[[1]][2]
    paste('Boîte à moustaches de la concentration de', metal, '(', conc, ')', 'selon', ifelse(input$choix_compar_metal=='Location', 'le foncier', 'les ruches d\'abeilles'))
  })
  output$boxPlot_Metal <- renderPlotly({
    #input$maj_g_explo
    #df <- ifelse(input$maj_g_explo==T, metals_bees_explo(), metals_bees |> filter(Location %in% input$cases_fonciers))
    df <- metals_bees_explo()
      
    isolate({
      metals_bees <- df |> 
        select(Metal, Var_Compar)
      
      ## Boxplot des taux du métal choisi selon le milieu
      plot_ly(metals_bees, 
              x = ~Var_Compar, 
              y = ~Metal, 
              color = ~Var_Compar,
              type = 'box', 
              colors=palette_compar()) |> 
        layout(
          #title = paste("Boîte à moustaches de la concentration de", input$choix_metal, "selon", ifelse(input$choix_compar_metal=="Location", "le foncier", "les ruches d'abeilles")),
          yaxis = list(title = input$choix_metal), 
          xaxis = list(title = input$choix_compar_metal),
          #hovermode = "x unified",
          legend = list(orientation = "h", y = -0.2)
        )
      
    })
  })
  
  output$titre_TempoMetal <- renderText({
    metal <- tolower(str_split(input$choix_metal, " ")[[1]][1])
    conc <- str_split(input$choix_metal, " ")[[1]][2]
    paste('Evolution temporelle de la concentration de', metal, '(', conc, ')', 'selon', ifelse(input$choix_compar_metal=='Location', 'le foncier', 'les ruches d\'abeilles'))
  })
  output$Tempo_Metal <- renderPlotly({
    input$maj_g_explo
    df <- metals_bees_explo()
    
    isolate({
      #metals_bees <- metals_bees |>
        #arrange(Location) |> 
        #rename(Metal = .data[[input$choix_metal]], Var_Compar = .data[[input$choix_compar_metal]]) |>
        #group_by(Var_Compar, Date) |>
        #summarize(Metal = mean(Metal)) |> 
        #select(Var_Compar, Metal, Date)
      
      metals_bees <- df |> 
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
          #title = paste("Evolution temporelle de la concentration de", input$choix_metal, "selon", ifelse(input$choix_compar_metal=="Location", "le foncier", "les ruches d'abeilles")),
          yaxis = list(title = input$choix_metal), 
          xaxis = list(title = "Date of measurement"),
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
      metals_output <- metals_bees
    }
    else{
      metals_output <- metals_bees |> 
        select(1:3,all_of(input$varmet1))}
    
    metals_output <- metals_output |> 
      filter(Date >= input$metals_selectDate[1], Date <= input$metals_selectDate[2])
    
    metals_output <- metals_output |>
      filter(Location %in% input$selectlocationmetals)
    
    if ('Toutes' %in% input$selecthivemetals){
      validate(need(length(input$selecthivemetals) < 2, "Décocher 'Toutes' pour sélectionner des ruches."))
      metals_output
    }else{
      metals_output <- metals_output |>
        filter(Hive %in% input$selecthivemetals)
    }
    
    
    
    
    metals_output
    
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
  data_mod_pest <- reactive({
    
    pest_bees <- pest_bees |> 
      rename_with(~ str_remove(.x, "^Pest_"))
    
    if ('Toutes' %in% input$selecthivepest){
      validate(need(length(input$selecthivepest) < 2, "Décocher 'Toutes' pour sélectionner des ruches."))
      pest_bees
    }else{
      pest_bees <- pest_bees |>
        filter(Hive %in% input$selecthivepest)
    }
    
    pest_bees <- pest_bees |> 
      filter(Date >= input$pest_selectDate[1], Date <= input$pest_selectDate[2])
    
    pest_bees <- pest_bees |>
      filter(Location %in% input$selectlocationpest)
    
    if('Tous' %in% input$appliedpest){
      pest_bees 
    }
    else if ("Traitements appliqués dans la parcelle adjacente" %in% input$appliedpest){
      pest_bees <- pest_bees |> 
        select(1:3, all_of(treat_pest$Molecule))
    }
    
    if (input$filtre_zero_pest){
      cols_num <- sapply(pest_bees, is.numeric)
      lignes_a_garder <- rowSums(pest_bees[,cols_num], na.rm = TRUE)>0
      pest_bees <- pest_bees[lignes_a_garder,]
    }else{
      pest_bees
    }
    
    pest_bees
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
                                  hjust=0.5),
        strip.text = element_text(size = 24, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm")
      ) +
      labs(title = "Pourcentages d'occupation des sols pour")+
      facet_wrap(~ Location) 
    
  })
  
  ##Partie traitmement pesticide 
  #Modif utilisateur du tableau de données des traitements
  data_mod_treat <- reactive({
    validate(need(length(input$moleculeTrait) > 0, "Sélectionner au moins une variable."))
    out <- treat_pest |> 
      filter(Date >= input$treat_selectDate[1], Date <= input$treat_selectDate[2])
    
    if ("Toutes" %in% input$moleculeTrait){
      validate(need(length(input$moleculeTrait) < 2, "Décocher 'Toutes'."))
    }else{
      out <- out |> 
        filter( Molecule %in% input$moleculeTrait)}
    
    if (input$vartreat=="Date"){
      out <-  out |> 
        arrange(Date)
    }else{
      out <-  out |> 
        arrange(Molecule)}
    out
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
  
  
  
#--------------------------------------------------------------------------------    
  
  ## ACP sur les concentrations de métaux lourds
  observeEvent(input$ACP_selectMetal, {
    updateCheckboxGroupInput(
      session,
      inputId = "ACP_selectMetal_Sup",
      choices = setdiff(colnames(metals_bees[, 4:20]), input$ACP_selectMetal)
    )
  })
  
  observeEvent(input$ACP_selectSolOccup, {
    updateCheckboxGroupInput(
      session,
      inputId = "ACP_selectSolOccup_Sup",
      choices = setdiff(colnames(soil_occup[, 2:7]), input$ACP_selectSolOccup)
    )
  })
  
  observeEvent(input$ACP_selectRuche, {
    updateCheckboxGroupInput(
      session,
      inputId = "ACP_selectIndiv_Sup",
      choices = setdiff(unique(metals_bees$Hive), input$ACP_selectRuche)
    )
  })
  
  
  res_acp <- eventReactive(input$ACP_maj, {
    donnees <- metals_bees |> 
        inner_join(soil_occup, by='Location') |> 
        filter(Hive %in% c(input$ACP_selectRuche, input$ACP_selectIndiv_Sup),
               Date >= input$ACP_selectDate[1], Date <= input$ACP_selectDate[2]) |> 
        select(1:3, input$ACP_selectMetal, input$ACP_selectSolOccup, input$ACP_selectMetal_Sup, input$ACP_selectSolOccup_Sup)
    
    
    ind_sup <- which(donnees$Hive%in%input$ACP_selectIndiv_Sup)
    args_pca <- list(
      X = donnees,
      scale.unit = TRUE,
      quanti.sup = c(input$ACP_selectMetal_Sup,input$ACP_selectSolOccup_Sup),
      quali.sup = 1:3,
      graph = F
    )
    if (length(ind_sup) > 0) {
      args_pca$ind.sup <- ind_sup
    }
    
    res_acp <- do.call(PCA, args_pca)
    
  })
  
  output$ACP_Indic_IndivSup <- renderText({
    input$ACP_maj
    isolate({
      "Les individus supplémentaires sont colorisés en rouge"
    })
  })
  
  
  #### Récupération du nombre de ruche pour le conditionalPanel qui affiche ou non les cases à cocher sur les groupes d'individus supplémentaires
  output$nb_hives <- reactive({
    length(input$ACP_selectRuche)
  })
  outputOptions(output, "nb_hives", suspendWhenHidden = FALSE)
  
  output$Test <- renderTable({input$ACP_selectIndiv_Sup})
  
  
  
  ### Identification et sélection des axes
  output$ACP_graphDecompo <-renderPlot({
    res <- res_acp()
    barplot(res$eig[, 2], main="Décomposition de l'inertie")
  })
  output$ACP_tabEigen <- renderTable({
    res <- res_acp()
    res$eig
  })
  
  
  palette_quali <- eventReactive(input$ACP_selectHabillage,{
    if (input$ACP_selectHabillage=="Location"){
      palette_foncier
    } else {
      palette_ruche
    }
  })
  
  #### Mise à jour de l'argument max du sliderInput de sélection de la contribution des individus
  observeEvent(input$ACP_maj, {
    res <- res_acp()
    nb_indiv <- nrow(res$ind$coord)
    
    updateSliderInput(
      session,
      inputId = "ACP_selectIndValue_Contrib",
      min = 1,
      max = nb_indiv,
      value = nb_indiv
    )
    
  })
  
  output$ACP_graphInd <- renderPlot({
    res <- res_acp()
    
    select_ind <- switch(
      input$ACP_selectInd,
      # Aucun filtre
      "none" = NULL,
      # Cos²
      "cos2" = list(
        cos2 = input$ACP_selectIndValue_Cos2
      ),
      # Contribution
      "contrib" = list(
        contrib = input$ACP_selectIndValue_Contrib
      )
    )
    
    fviz_pca_ind(
      res,
      axes=c(input$ACP_Axe1, input$ACP_Axe2),
      habillage = ifelse(input$ACP_selectHabillage=="Pas d'habillage", "none", ifelse(input$ACP_selectHabillage=="Hive", 2, 3)),
      palette = palette_quali(),
      addEllipses = input$ACP_ellispe,
      select.ind = select_ind,
      add.ind.sup=T,
      col.ind.sup = "red",
      title = "Graphique des individus"
    )
  })
  
  #### Mise à jour de l'argument max du sliderInput de sélection de la contribution des variables
  observeEvent(input$ACP_maj, {
    res <- res_acp()
    nb_vars <- nrow(res$var$coord)
    
    updateSliderInput(
      session,
      inputId = "ACP_selectVarValue_Contrib",
      min = 1,
      max = nb_vars,
      value = nb_vars
    )
    
  })
  #### Construction du graphique des corrélations
  output$ACP_graphVar <- renderPlot({
    res <- res_acp()
    
    select_var <- switch(
      input$ACP_selectVar,
      # Aucun filtre
      "none" = NULL,
      # Cos²
      "cos2" = list(
        cos2 = input$ACP_selectVarValue_Cos2
      ),
      # Contribution
      "contrib" = list(
        contrib = input$ACP_selectVarValue_Contrib
      )
    )

    fviz_pca_var(
      res,
      axes=c(input$ACP_Axe1, input$ACP_Axe2),
      select.var = select_var,
      title = "Graphique des corrélations"
    )
  })
  
  
  #### Affichage des lignes de code
  ##### Formulation de l'ACP
  output$ACP_codePCA <- renderText({
    
    # Utiliser les valeurs validées lors du dernier clic
    req(res_acp())
    
    # Indices des variables supplémentaires quantitatives
    donnees <- metals_bees |>
      inner_join(soil_occup, by = "Location") |>
      filter(
        Hive %in% c(
          input$ACP_selectRuche,
          input$ACP_selectIndiv_Sup
        ),
        Date >= input$ACP_selectDate[1],
        Date <= input$ACP_selectDate[2]
      ) |>
      select(
        1:3,
        input$ACP_selectMetal,
        input$ACP_selectSolOccup,
        input$ACP_selectMetal_Sup,
        input$ACP_selectSolOccup_Sup
      )
    
    ind_sup <- which(donnees$Hive %in% input$ACP_selectIndiv_Sup)
    
    # Formule PCA
    code <- paste0(
      "PCA(\n",
      "  X = donnees,\n",
      "  scale.unit = TRUE,\n",
      "  quanti.sup = c(",
      paste(
        deparse(input$ACP_selectMetal_Sup),
        deparse(input$ACP_selectSolOccup_Sup),
        sep = ", "
      ),
      "),\n",
      "  quali.sup = 1:3,\n"
    )
    
    if (length(ind_sup) > 0) {
      code <- paste0(
        code,
        "  ind.sup = c(",
        paste(ind_sup, collapse = ", "),
        "),\n"
      )
    }
    
    code <- paste0(
      code,
      "  graph = FALSE\n",
      ")"
    )
    
    code
  })
  
  ##### Graphique des individus
  output$ACP_codeGraphInd <- renderText({
    
    # Axes
    axe1 <- input$ACP_Axe1
    axe2 <- input$ACP_Axe2
    
    # Sélection des individus
    code_select_ind <- switch(
      input$ACP_selectInd,
      
      "none" = "select.ind = NULL",
      
      "cos2" = paste0(
        "select.ind = list(cos2 = ",
        deparse(input$ACP_selectIndValue_Cos2),
        ")"
      ),
      
      "contrib" = paste0(
        "select.ind = list(contrib = ",
        deparse(input$ACP_selectIndValue_Contrib),
        ")"
      )
    )
    
    # Habillage
    habillage <- switch(
      input$ACP_selectHabillage,
      "Pas d'habillage" = "\"none\"",
      "Hive" = "2",
      "Sexe" = "3"
    )
    
    # Ellipses
    ellipses <- if (isTRUE(input$ACP_ellispe)) "TRUE" else "FALSE"
    
    # Palette
    palette_code <- paste(
      deparse(palette_quali()),
      collapse = "\n"
    )
    
    # Code final
    paste0(
      "fviz_pca_ind(\n",
      "  res,\n",
      "  axes = c(", axe1, ", ", axe2, "),\n",
      "  habillage = ", habillage, ",\n",
      "  palette = ", palette_code, ",\n",
      "  addEllipses = ", ellipses, ",\n",
      "  ", code_select_ind, ",\n",
      "  add.ind.sup = TRUE,\n",
      "  col.ind.sup = \"red\",\n",
      "  title = \"Graphique des individus\"\n",
      ")"
    )
  })
  
  ##### Graphique des corrélations
  output$ACP_codeGraphCorr <- renderText({
    
    # Récupération des valeurs actuelles
    axe1 <- input$ACP_Axe1
    axe2 <- input$ACP_Axe2
    type_selection <- input$ACP_selectVar
    
    # Construction de l'argument select.var
    code_select_var <- switch(
      type_selection,
      
      "none" = "select.var = NULL",
      
      "cos2" = paste0(
        "select.var = list(cos2 = ",
        deparse(input$ACP_selectVarValue_Cos2),
        ")"
      ),
      
      "contrib" = paste0(
        "select.var = list(contrib = ",
        deparse(input$ACP_selectVarValue_Contrib),
        ")"
      )
    )
    
    # Code final
    paste0(
      "fviz_pca_var(\n",
      "  res,\n",
      "  axes = c(", axe1, ", ", axe2, "),\n",
      "  ", code_select_var, ",\n",
      "  title = \"Graphique des corrélations\"\n",
      ")"
    )
  })
  
  
  
  
  ### Sous-onglet Tableaux de résultats
  observeEvent(
      input$ACP_maj,
    {
      # Choix toujours disponibles
      choix <- c(
        "Tableau des individus" = "1",
        "Tableau des variables" = "3",
        "Tableau des variables qualitatives supplémentaires" = "5"
      )
      
      # Ajouter le tableau des individus supplémentaires
      if (length(input$ACP_selectIndiv_Sup) > 0) {
        choix <- c(
          "Tableau des individus" = "1",
          "Tableau des individus supplémentaires" = "2",
          "Tableau des variables" = "3",
          "Tableau des variables qualitatives supplémentaires" = "5"
        )
      }
      
      # Ajouter le tableau des variables quantitatives supplémentaires
      if (
        length(input$ACP_selectMetal_Sup) > 0 ||
        length(input$ACP_selectSolOccup_Sup) > 0
      ) {
        choix <- c(
          choix[names(choix) !=
                  "Tableau des variables qualitatives supplémentaires"],
          "Tableau des variables quantitatives supplémentaires" = "4",
          "Tableau des variables qualitatives supplémentaires" = "5"
        )
      }
      
      # Conserver le choix courant s'il est toujours disponible
      selection <- input$ACP_Tab_Select
      
      if (is.null(selection) || !selection %in% unname(choix)) {
        selection <- unname(choix)[1]
      }
      
      updateRadioButtons(
        session,
        inputId = "ACP_Tab_Select",
        choices = choix,
        selected = selection
      )
    },
    ignoreInit = FALSE
  )
  
  output$ACP_Tab_CoordInd <- renderTable({
    res <- res_acp()
    res$ind$coord
  }, rownames=T)
  
  output$ACP_Tab_CoordIndSup <- renderTable({
    res <- res_acp()
    if (is.null(res$ind.sup)) return(NULL)
    res$ind.sup$coord
  }, rownames = T)
  
  output$ACP_Tab_CoordVar <- renderTable({
    res <- res_acp()
    res$var$coord
  }, rownames = T)
  
  output$ACP_Tab_CoordQuantiSup <- renderTable({
    res <- res_acp()
    if (is.null(res$quanti.sup)) return(NULL)
    res$quanti.sup$coord
  }, rownames = T)
  
  output$ACP_Tab_CoordQualiSup <- renderTable({
    res <- res_acp()
    if (is.null(res$quali.sup)) return(NULL)
    res$quali.sup$coord
  }, rownames = T)
  
#--------------------------------------------------------------------------------  
  
  
  
  
  
  
  
  
  
  
  
  #Tests de student intra site 
  output$tTestIntraSite <- renderPlotly({
    
    df_plot <- res_t_test_intra_loc
    df_plot$Signif <- ifelse(df_plot$p < 0.05, "p < 0.05", "p ≥ 0.05")
    
    g <- ggplot(df_plot, aes(x = estimate, y = Metal)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      #barre 0
      geom_errorbarh(aes(xmin = lower, xmax = upper), width = 0.2, color = "grey50") +
      #barre d'erreur horizontale
      geom_point(aes(color = Signif), size = 2.5) +
      scale_color_manual(
        values = c("p ≥ 0.05" = "#7A8793", "p < 0.05" = "#D1495B"),
        name = NULL
      ) +
      facet_wrap(~Location) +
      labs(x = "Log différence entre ruches intra-site", y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        strip.text         = element_text(face = "bold", size = 13),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey92"),
        axis.title.x       = element_text(margin = margin(t = 10)),
        panel.spacing      = unit(1.2, "lines")
      )
    
    ggplotly(g, tooltip = "text", height = 650) %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.12)) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"))
  })
  
  #Tests posthoc inter site 
  
  output$TestInterSite <- renderPlotly({
    
    g <- ggplot(results_all, aes(x = estimate, y = Metal)) +
      geom_point(aes(color = Significant), size = 3) +
      geom_errorbarh(aes(xmin = estimate - 1.96 * SE, xmax = estimate + 1.96 * SE),
                     width = 0.15, color = "grey50") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
      scale_color_manual(
        values = c("TRUE" = "#D1495B", "FALSE" = "#7A8793"), # Harmonisé avec ton autre graphique
        labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
        name = NULL
      ) +
      facet_wrap(~ Contrast_clean, ncol = 3, scales = "free_x") +
      labs(x = "Log différence inter-sites", y = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 12),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey92"),
        axis.title.x = element_text(margin = margin(t = 10)),
        panel.spacing = unit(1.2, "lines")
      )
    
    ggplotly(g, tooltip = "text", height = 750) %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15)) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"))
  })
  
  output$plots_metaux_trait <- renderUI({
    req(input$tr_metaux)
    plotlyOutput("plot_trait_all", height = paste0(320 * length(input$tr_metaux), "px"))
  })
  
  
  output$plot_trait_all <- renderPlotly({
    
    req(input$tr_metaux)
    
    trait  <- treat_clean |> filter(Molecule %in% input$tr_mols)
    mols_i <- as.character(trait$Molecule)
    n <- length(input$tr_metaux)
    
    # Un graphique par métal, empilés avec un axe des dates partagé 
    plots <- lapply(seq_along(input$tr_metaux), function(k) {
      metal <- input$tr_metaux[k]
      
      # Une seule courbe pour le site : moyenne des ruches à chaque date
      d <- metals_bees |>
        filter(Location == "Vineyard") |>
        rename(Metal = all_of(metal)) |>
        group_by(Date) |>
        summarize(Metal = mean(Metal, na.rm = TRUE), .groups = "drop")
      
      y_min <- min(d$Metal, na.rm = TRUE)
      y_max <- max(d$Metal, na.rm = TRUE)
      
      p <- plot_ly(d, x = ~Date, y = ~Metal,
                   type = "scatter", mode = "lines+markers",
                   line = list(color = "#9719E6"), marker = list(color = "#9719E6"),
                   name = "Vignoble (moyenne des ruches)",
                   legendgroup = "site", showlegend = (k == 1),
                   hoverinfo = "none")
      
      # Une ligne verticale par traitement coché
      for (i in seq_len(nrow(trait))) {
        mol <- mols_i[i]
        p <- p |> add_trace(
          x = rep(trait$Date[i], 2), y = c(y_min, y_max),
          type = "scatter", mode = "lines", inherit = FALSE,
          line = list(color = col_mols[[mol]], dash = "dash", width = 2),
          name = mol, legendgroup = mol,
          showlegend = (k == 1) && !(mol %in% mols_i[seq_len(i - 1)]),  
          text = paste0(mol, "<br>Dose : ", trait$Dose[i], " g/ha"),
          hoverinfo = "text"
        )
      }
      
      p |> layout(yaxis = list(title = paste("Concentrations en", metal)))
    })
    
    marge <- 0.04
    h     <- (1 - marge * (n - 1)) / n          
    etiquettes <- lapply(seq_len(n), function(k) {
      list(
        text = paste0("<b>", input$tr_metaux[k], "</b>"),
        xref = "paper", yref = "paper",
        x = 0.99, y = 1 - (k - 1) * (h + marge),  
        xanchor = "right", yanchor = "top",  
        showarrow = FALSE,
        font = list(size = 18, color = "#151617"),
        bgcolor = "#FFD000",
        bordercolor = "#151617", borderwidth = 1, borderpad = 6
      )
    })
    
    subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE, margin = 0.04) |>
      layout(
        title = NULL,
        xaxis = list(title = "Date"),
        annotations = etiquettes,
        hovermode = "closest",
        legend = list(orientation = "h", y = -0.08)
      )
  })
  output$tr_pesti <- renderPlot({
    ggplot(df_pesti, aes(x = delai, y = event)) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "grey30") +
      geom_point(aes(fill = detecte, colour = detecte), shape = 21, size = 5, stroke = 1) +
      scale_fill_manual(name = NULL,
                        values = c("TRUE" = "orange", "FALSE" = "white"),
                        labels = c("TRUE" = "Positif", "FALSE" = "Négatif")) +
      scale_colour_manual(name = NULL,
                          values = c("TRUE" = "orange", "FALSE" = "grey50"),
                          labels = c("TRUE" = "Positif", "FALSE" = "Négatif")) +
      labs(title = "Détection de résidus de pesticides chez les abeilles après traitement sur le site du vignoble",
           x = "Nombre de jours après l'application", y = NULL) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.x = element_line(colour = "grey85", linewidth = 0.4),
            panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.4),
            panel.grid.minor   = element_blank(),
            legend.position    = "top")
  })
  
  #Résumés 
  output$SummaryTreat <- renderPrint(
    print(summary(treat_pest)), width = 100
  )
  output$SummaryMetal <- renderPrint(
    print(summary(metals_bees)), width = 100
  )
  output$SummaryPest <- renderPrint(
    print(summary(pest_bees)), width = 100
  )
})