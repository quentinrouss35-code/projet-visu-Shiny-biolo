

theme <- bs_theme(
  # Controls the default grayscale palette
  bg = "#202123", fg = "white",
  # Controls the accent (e.g., hyperlink, button, etc) colors
  primary = "#FFD000", secondary = "yellow",
  base_font = c("Grandstander", "sans-serif"),
  code_font = c("Courier", "monospace"),
  heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  # Can also add lower-level customization
  "input-border-color" = "#FFD000"
)



# Define UI for application that draws a histogram
shinyUI(
  # navbarPage
  ## pbm : Est-ce qu’on peut utiliser les abeilles comme des bio-indicateurs pour détecter et évaluer la contamination de différents milieux environnementaux ?
  navbarPage("", theme=theme,
             tabPanel("Présentation de l'étude",
                      fluidRow(
                        column(width = 3,
                               value_box(
                                 title = "Nombre de ruches",
                                 value = n_distinct(metals_bees$Hive),
                                 showcase = tags$span("🐝", style = "font-size: 4rem;"),
                                 theme = value_box_theme(bg = "Black", fg = "Yellow")
                               )),
                        column(width = 3,
                               value_box(
                                 title = "Nombre de sites différents",
                                 value = n_distinct(metals_bees$Location),
                                 showcase = tags$span("📍️",style = "font-size: 4rem;"),
                                 theme = value_box_theme(bg = "Black", fg = "Yellow")
                               )),
                        column(width = 3,
                               value_box(
                                 title = "Nombre de métaux analysés",
                                 value = ncol(metals_bees) - 3,
                                 showcase = tags$span("💎️",style = "font-size: 4rem;"),
                                 theme = value_box_theme(bg = "Black", fg = "Yellow")
                               )),
                        column(width = 3,
                               value_box(
                                 title = "Nombre de pesticides analysés",
                                 value = ncol(pest_bees) - 3,
                                 showcase = tags$span("🧪️",style = "font-size: 4rem;"),
                                 theme = value_box_theme(bg = "Black", fg = "Yellow")
                               ))
                        
                      )
                      
             ),
             
             
             # Onglet d'analyse descriptive des différents jeux de données de l'étude
             tabPanel("Analyse Descriptive",
                      navlistPanel(
                        "Choix du tableau de données", widths = c(2, 10),
                        tabPanel("Occupation des sols sur un rayon de 3km",
                                 tabsetPanel(id = "Onglet_OccupSol",
                                             tabPanel("Tableau de données",
                                                      fluidRow(
                                                        column(width = 9, checkboxGroupInput(inputId = "varloc1", label = "Variable : ", choices = unique(soil_occup$Location), selected="Urban", inline=TRUE)), 
                                                        column(width = 3, downloadButton("downloadData_soil", "Télécharger les données")),
                                                        column(width = 12, dataTableOutput("data_table_soil")))
                                             ),
                                             tabPanel("Graphiques",
                                                      fluidRow(
                                                        div(h3("Visualisation", style = "color : white"), align="center"),
                                                        column(width = 2, checkboxGroupInput(inputId = "varloc2", label = "Variable : ", choices = unique(soil_occup$Location), selected="Urban")),
                                                        column(width = 10, div(
                                                          style = "margin-bottom: 30px;",
                                                          plotOutput("squarePie"))))
                                             )
                                 )
                        ),
                        tabPanel("Traitement phytosanitaire dans les vignobles",
                                 tabsetPanel(id = "Onglet_PestTrait",
                                             tabPanel("Tableau de données",
                                                      fluidRow(
                                                        div(h2("Données de traitement pesticides", style = "color : white"),align="center"),
                                                        column(width = 9, radioButtons(inputId = "vartreat", label = "Voir par : ", choices = unique(colnames(treat_pest[,1:2])), selected="Date", inline=TRUE)), 
                                                        column(width = 3, downloadButton("downloadtreat_pest", "Télécharger les données")),
                                                        dataTableOutput("data_table_treat")
                                                      )
                                             )
                                 )
                        ),
                        tabPanel("Pesticides détectés",
                                 tabsetPanel(id = "Onglet_pest",
                                             tabPanel("Tableau de données",
                                                      checkboxInput("filtre_zero_pest",
                                                                    "Enlever les lignes ne contenant que des 0",
                                                                    value = FALSE, width='100%'),
                                                      dataTableOutput("table_Pest")
                                             )
                                 )
                        ),
                        tabPanel("Métaux détectés",
                                 tabsetPanel(id = "Onglet_metaux",
                                             tabPanel("Tableau de données",
                                                      dataTableOutput("table_Metaux")
                                             ),
                                             tabPanel("Graphiques",
                                                      fluidRow(
                                                        column(width=5,
                                                               wellPanel(
                                                                 selectInput(inputId = "choix_metal", label = "Choisissez le métal à représenter",
                                                                             choices = l_metaux,
                                                                             multiple=F
                                                                 )
                                                               )
                                                        ),
                                                        column(width=5,
                                                               wellPanel(
                                                                 selectInput(inputId = "choix_compar_metal", label = "Choisissez la strate (donner meilleur nom)",
                                                                             choices = c("Location", "Hive"),
                                                                             multiple=F
                                                                 )                                         
                                                               )
                                                        ),
                                                        column(width=2,
                                                               br(),
                                                               actionButton("maj_g_explo", "Mettre à jour")
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(width=1,
                                                               checkboxGroupInput(inputId = "cases_fonciers", label = "Sélectionnez un(des) foncier(s) :", selected = c("Forest", "Suburban", "Urban", "Vineyard"),
                                                                                  choices = c("Forêt" = "Forest", "Banlieue" = "Suburban", "Urbain" = "Urban", "Vignoble" = "Vineyard"))),
                                                        column(width=5,
                                                               plotlyOutput("boxPlot_Metal")
                                                        ),
                                                        column(width=6,
                                                               plotlyOutput("Tempo_Metal")
                                                        )
                                                      )
                                             )
                                 )
                                 
                        ),
                      )
             ), 
             
             # second onglet Visualisation
             tabPanel("ACP", 
                      # Sidebar with a slider input for number of bins 
                      fluidRow(
                        dataTableOutput("table_Test")
                      )
             )
  )
)