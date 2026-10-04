

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
                        column(12,
                               layout_column_wrap(
                                 width = 1/2,
                                 style = "margin-bottom: 1rem;",
                                 card(
                                   style = "background-color: black; color: yellow;",
                                   card_header("Introduction"),
                                   card_body(
                                     p("Les abeilles domestiques sont exposées à de nombreux contaminants, notamment avec l'usage croissant des pesticides et l'augmentation des teneurs en métaux d'origine humaine, qui menacent la biodiversité et les services écosystémiques comme la pollinisation. Grâce à son large rayon de butinage et à sa capacité de stockage, l'abeille domestique constitue un outil précieux pour évaluer les risques environnementaux, son profil d'exposition pouvant fournir des signaux d'alerte précoce d'impacts écologiques plus larges.")
                                   )
                                 ),
                                 card(
                                   style = "background-color: black; color: yellow;",
                                   card_header("Problématique"),
                                   card_body(
                                     p("Les abeilles domestiques peuvent-elles servir de biosentinelles pour détecter et comparer l'exposition aux pesticides et aux métaux dans des milieux contrastés ?")
                                   )
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               layout_column_wrap(
                                 width = 1/5,
                                 value_box(
                                   title = "Nombre de ruches",
                                   value = n_distinct(metals_bees$Hive),
                                   showcase = tags$span("🐝", style = "font-size: 4rem;"),
                                   theme = value_box_theme(bg = "Black", fg = "Yellow")
                                 ),
                                 value_box(
                                   title = "Nombre de sites différents",
                                   value = n_distinct(metals_bees$Location),
                                   showcase = tags$span("📍", style = "font-size: 4rem;"),
                                   theme = value_box_theme(bg = "Black", fg = "Yellow")
                                 ),
                                 value_box(
                                   title = "Nombre de métaux analysés",
                                   value = ncol(metals_bees) - 3,
                                   showcase = tags$span("💎", style = "font-size: 4rem;"),
                                   theme = value_box_theme(bg = "Black", fg = "Yellow")
                                 ),
                                 value_box(
                                   title = "Nombre de pesticides analysés",
                                   value = ncol(pest_bees) - 3,
                                   showcase = tags$span("🧪", style = "font-size: 4rem;"),
                                   theme = value_box_theme(bg = "Black", fg = "Yellow")
                                 ),
                                 value_box(
                                   title = "Période d'échantillonnage",
                                   value = "3 mai - 20 octobre 2021",
                                   showcase = tags$span("🗓️", style = "font-size: 4rem;"),
                                   theme = value_box_theme(bg = "Black", fg = "Yellow")
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(6,
                               layout_column_wrap(
                                 width = 1/2,          
                                 height = "600px",     
                                 value_box(
                                   title = "",
                                   value = "Teneur en métaux",
                                   "Concentrations en métaux (g/kg ou mg/kg) mesurées par site, par ruche et par date.",
                                   theme = value_box_theme(bg = "Black", fg = "Yellow")
                                 ),
                                 value_box(
                                   title = "",
                                   value = "Traitements pesticides appliqués",
                                   "Dose (g/ha) appliquée par date et par molécule au site du vignoble.",
                                   theme = value_box_theme(bg = "Black", fg = "Yellow")
                                 ),
                                 value_box(
                                   title = "",
                                   value = "Résidus de pesticides mesurés",
                                   "Concentrations en pesticides (ng/g) mesurées par site, par ruche et par date.",
                                   theme = value_box_theme(bg = "Black", fg = "Yellow")
                                 ),
                                 value_box(
                                   title = "",
                                   value = "Caractérisation des sites",
                                   "Part (%) de chaque type d'occupation du sol (urbain, périurbain, forêt et vignoble) autour des 4 sites d'étude.",
                                   theme = value_box_theme(bg = "Black", fg = "Yellow")
                                 )
                               )
                        ),
                        column(6,
                               leafletOutput("Carte", height = "600px")
                        )
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
                                                        column(width = 12, checkboxGroupInput(inputId = "varloc1",
                                                                                              label = "Variable : ", 
                                                                                              choices = unique(soil_occup$Location), 
                                                                                              selected="Urban", inline=TRUE)),
                                                        column(width = 12, dataTableOutput("data_table_soil"))),
                                                      column(width = 3, downloadButton("downloadData_soil", "Télécharger les données"))
                                             ),
                                             tabPanel("Graphiques",
                                                      fluidRow(
                                                        div(h3("Visualisation", style = "color : white"), align="center"),
                                                        column(width = 2, checkboxGroupInput(inputId = "varloc2", label = "Variable : ", 
                                                                                             choices = unique(soil_occup$Location), 
                                                                                             selected="Urban")),
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
                                                        column(width = 9, radioButtons(inputId = "vartreat", label = "Voir par : ", 
                                                                                       choices = unique(colnames(treat_pest[,1:2])), 
                                                                                       selected="Date", inline=TRUE)),
                                                        dataTableOutput("data_table_treat"),
                                                        column(width = 3, downloadButton("downloadTreat_pest", "Télécharger les données"))
                                                      )
                                             ),
                                             tabPanel("Frise",
                                                      div(h2("Données de traitement pesticides", style = "color : white"),align="center"),
                                                      column(width = 12,div(img(src="Frise_treatment.png", title="Popup", width = "80%")), align="center",
                                                             style = "margin-bottom: 30px;")
                                             )
                                 )
                        ),
                        tabPanel("Pesticides détectés",
                                 tabsetPanel(id = "Onglet_pest",
                                             tabPanel("Tableau de données",
                                                      checkboxInput("filtre_zero_pest",
                                                                    "Enlever les lignes ne contenant que des 0",
                                                                    value = FALSE, width='100%'),
                                                      dataTableOutput("table_Pest"),
                                                      column(width = 12, downloadButton("downloadData_pest", "Télécharger les données"))
                                             )
                                 )
                        ),
                        tabPanel("Métaux détectés",
                                 tabsetPanel(id = "Onglet_metaux",
                                             tabPanel("Tableau de données",
                                                              column(width = 12, selectInput(inputId = "varmet1",
                                                                                             label = "Métaux : ", 
                                                                                             choices = c("Tous",l_metaux), 
                                                                                             selected= "Tous", multiple=T)),
                                                              dataTableOutput("table_Metaux"),
                                                              column(width = 12, downloadButton("downloadData_metal", "Télécharger les données"))
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
             tabPanel("Effet des traitements phytosanitaires",
                         
               
             ),
             
             # second onglet Visualisation
             tabPanel("Profils de contamination", 
                      # Sidebar with a slider input for number of bins 
                      fluidRow(
                        dataTableOutput("table_Test")
                      )
             ),
             tabPanel("Variabilité et comparaisons")
  )
)