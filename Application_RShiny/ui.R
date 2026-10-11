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
                                   style = "background-color: #565A61; color: #FFD000;",
                                   card_header("INTRODUCTION"),
                                   card_body(
                                     p("Les abeilles domestiques sont exposées à de nombreux contaminants, notamment avec l'usage croissant des pesticides et l'augmentation des teneurs en métaux d'origine humaine, qui menacent la biodiversité et les services écosystémiques comme la pollinisation. Grâce à son large rayon de butinage et à sa capacité de stockage, l'abeille domestique constitue un outil précieux pour évaluer les risques environnementaux, son profil d'exposition pouvant fournir des signaux d'alerte précoce d'impacts écologiques plus larges.")
                                   )
                                 ),
                                 card(
                                   style = "background-color: #565A61; color: #FFD000;",
                                   card_header("PROBLÉMATIQUE"),
                                   card_body(
                                     p("Les abeilles domestiques peuvent-elles servir de biosentinelles pour détecter et comparer l'exposition aux pesticides et aux métaux dans des milieux contrastés ?")
                                   )
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               h3("Les chiffres clefs",
                                  style = "color: #FFD000; font-weight: bold; margin: 1rem 0;"),
                               layout_column_wrap(
                                 width = 1/5,
                                 value_box(
                                   title = "Nombre de ruches",
                                   value = n_distinct(metals_bees$Hive),
                                   showcase = tags$span("🐝", style = "font-size: 4rem;"),
                                   theme = value_box_theme(bg = "#2D2F33", fg = "#FFD000")
                                 ),
                                 value_box(
                                   title = "Nombre de sites différents",
                                   value = n_distinct(metals_bees$Location),
                                   showcase = tags$span("📍", style = "font-size: 4rem;"),
                                   theme = value_box_theme(bg = "#2D2F33", fg = "#FFD000")
                                 ),
                                 value_box(
                                   title = "Nombre de métaux analysés",
                                   value = ncol(metals_bees) - 3,
                                   showcase = tags$span("💎", style = "font-size: 4rem;"),
                                   theme = value_box_theme(bg = "#2D2F33", fg = "#FFD000")
                                 ),
                                 value_box(
                                   title = "Nombre de pesticides analysés",
                                   value = ncol(pest_bees) - 3,
                                   showcase = tags$span("🧪", style = "font-size: 4rem;"),
                                   theme = value_box_theme(bg = "#2D2F33", fg = "#FFD000")
                                 ),
                                 value_box(
                                   title = "Période d'échantillonnage",
                                   value = "3 mai - 20 octobre 2021",
                                   showcase = tags$span("🗓️", style = "font-size: 4rem;"),
                                   theme = value_box_theme(bg = "#2D2F33", fg = "#FFD000")
                                 )
                               )
                        )
                      ),
                      fluidRow(
                        column(6,
                               h3("Présentation des 4 jeux de données",
                                  style = "color: #FFD000; font-weight: bold; margin: 1rem 0;"),
                               layout_column_wrap(
                                 width = 1/2,          
                                 height = "600px",     
                                 value_box(
                                   title = "",
                                   value = "Teneur en métaux",
                                   "Concentrations en métaux (g/kg ou mg/kg) mesurées par site, par ruche et par date.",
                                   theme = value_box_theme(bg = "#151617", fg = "#FFD000")
                                 ),
                                 value_box(
                                   title = "",
                                   value = "Traitements pesticides appliqués",
                                   "Dose (g/ha) appliquée par date et par molécule au site du vignoble.",
                                   theme = value_box_theme(bg = "#151617", fg = "#FFD000")
                                 ),
                                 value_box(
                                   title = "",
                                   value = "Résidus de pesticides mesurés",
                                   "Concentrations en pesticides (ng/g) mesurées par site, par ruche et par date.",
                                   theme = value_box_theme(bg = "#151617", fg = "#FFD000")
                                 ),
                                 value_box(
                                   title = "",
                                   value = "Caractérisation des sites",
                                   "Part (%) de chaque type d'occupation du sol (urbain, périurbain, forêt et vignoble) autour des 4 sites d'étude.",
                                   theme = value_box_theme(bg = "#151617", fg = "#FFD000")
                                 )
                               )
                        ),
                        column(6,
                               h3("Localisation des 4 sites",
                                  style = "color: #FFD000; font-weight: bold; margin: 1rem 0;"),
                               leafletOutput("Carte", height = "600px")
                        )
                      ),
                      fluidRow(
                        column(12,
                               h5("Bibliographie",
                                  style = "color: #C0C3C8; font-weight: bold; margin: 1rem 0;"),
                               p("Tison, L., et al. (2026). Vineyard footprint revealed by honey bees used as sentinels for organic and inorganic contaminants in contrasted environments. ",
                                 em("Peer Community Journal"),
                                 ", 6, e85. ",
                                 a("https://doi.org/10.24072/pcjournal.774",
                                   href = "https://doi.org/10.24072/pcjournal.774",
                                   target = "_blank",
                                   style = "color: #C0C3C8;"),
                                 style = "color: #C0C3C8;"))
             )),
             
             
             # Onglet d'analyse descriptive des différents jeux de données de l'étude
             tabPanel("Analyse Descriptive",
                      navlistPanel(
                        "Choix du tableau de données", widths = c(2, 10),
                        tabPanel("Occupation des sols sur un rayon de 3km",
                                 tabsetPanel(id = "Onglet_OccupSol",
                                             tabPanel("Tableau de données",
                                                      wellPanel(
                                                      fluidRow(
                                                        column(width = 12, checkboxGroupInput(inputId = "varloc1",
                                                                                              label = "Variable : ", 
                                                                                              choices = unique(soil_occup$Location), 
                                                                                              selected=unique(soil_occup$Location), inline=TRUE)))),
                                                        column(width = 12, dataTableOutput("data_table_soil")),
                                                      column(width = 3, downloadButton("downloadData_soil", "Télécharger le tableau"))),
                                             tabPanel("Graphiques",
                                                      wellPanel(
                                                      fluidRow(
                                                        column(width = 2, checkboxGroupInput(inputId = "varloc2", label = "Variable : ", 
                                                                                             choices = unique(soil_occup$Location), 
                                                                                             selected="Urban")))),
                                                        column(width = 10, div(
                                                          style = "margin-bottom: 30px;",
                                                          plotOutput("squarePie")))))
                                 ),
                                             
                        tabPanel("Traitement phytosanitaire dans les vignobles",
                                 tabsetPanel(id = "Onglet_PestTrait",
                                             tabPanel("Tableau de données",
                                                      wellPanel(
                                                      fluidRow(
                                                        column(width = 2, radioButtons(inputId = "vartreat", label = "Voir par : ", 
                                                                                       choices = unique(colnames(treat_pest[,1:2])), 
                                                                                       selected="Date", inline=TRUE)),
                                                        column(width=3, offset=1,
                                                               dateRangeInput(inputId = "treat_selectDate", label = ("Sélectionnez une période :"),
                                                                              start = min(treat_pest$Date), end = max(treat_pest$Date), 
                                                                              min = min(treat_pest$Date), max = max(treat_pest$Date), 
                                                                              format = "dd-mm-yyyy", language = "fr", separator = " à ")),
                                                        column(width=3, offset=1,
                                                               selectInput(inputId="moleculeTrait", label=("Selectionnez une molécule :"),
                                                                           choices=c(as.character(l_molecules),"Toutes"), selected="Toutes", multiple=T)))),
                                                        dataTableOutput("data_table_treat"),
                                                        column(width = 3, downloadButton("downloadTreat_pest", "Télécharger le tableau"))),
                                             tabPanel("Frise",
                                                      column(width = 12,div(img(src="Frise_treatment.png", width = "80%")), align="center",
                                                             style = "margin-bottom: 30px;")
                                             ),
                                             tabPanel("Résumé",
                                                      verbatimTextOutput("SummaryTreat"))
                                 )
                        ),
                        tabPanel("Pesticides détectés",
                                 tabsetPanel(id = "Onglet_pest",
                                             tabPanel("Tableau de données",
                                                      wellPanel(
                                                      fluidRow(
                                                        column(width=4,checkboxInput("filtre_zero_pest",
                                                                                     "Enlever les lignes ne contenant que des 0",
                                                                                     value = FALSE, width='100%')),
                                                        column(width=5, offset=3, radioButtons(inputId="appliedpest", label="",
                                                                                               choices=c("Tous", "Traitements appliqués dans la parcelle adjacente"), 
                                                                                               inline=TRUE)),
                                                        column(width=4,selectInput(inputId="selecthivepest",
                                                                                   label="Sélectionnez les ruches",
                                                                                   choices=c("Toutes",as.character(pest_bees$Hive)),
                                                                                   selected="Toutes",
                                                                                   multiple=T
                                                        )),
                                                        column(width=4,dateRangeInput(inputId = "pest_selectDate", label = ("Sélectionnez une période :"),
                                                                                      start = min(pest_bees$Date), end = max(pest_bees$Date), 
                                                                                      min = min(pest_bees$Date), max = max(pest_bees$Date), 
                                                                                      format = "dd-mm-yyyy", language = "fr", separator = " à ")),
                                                        column(width=4,checkboxGroupInput(inputId="selectlocationpest",
                                                                                          label="Sélectionnez les location",
                                                                                          choices=unique(pest_bees$Location),
                                                                                          selected=unique(pest_bees$Location)
                                                        )))),
                                                        dataTableOutput("table_Pest"),
                                                        column(width = 12, downloadButton("downloadData_pest", "Télécharger le tableau"))
                                                      ),
                                             tabPanel("Résumé",
                                                      verbatimTextOutput("SummaryPest"))
                                 )
                        ),
                        tabPanel("Métaux détectés",
                                 tabsetPanel(id = "Onglet_metaux",
                                             tabPanel("Tableau de données",
                                                      wellPanel(
                                                      fluidRow(
                                                        column(width = 6, selectInput(inputId = "varmet1",
                                                                                      label = "Métaux : ", 
                                                                                      choices = c("Tous",l_metaux), 
                                                                                      selected= "Tous", multiple=T))
                                                        ,
                                                        column(width=6,selectInput(inputId="selecthivemetals",
                                                                                   label="Sélectionnez les ruches",
                                                                                   choices=c("Toutes",as.character(metals_bees$Hive)),
                                                                                   selected="Toutes",
                                                                                   multiple=T
                                                        )),
                                                        column(width=6,dateRangeInput(inputId = "metals_selectDate", label = ("Sélectionnez une période :"),
                                                                                      start = min(metals_bees$Date), end = max(metals_bees$Date), 
                                                                                      min = min(metals_bees$Date), max = max(metals_bees$Date), 
                                                                                      format = "dd-mm-yyyy", language = "fr", separator = " à ")),
                                                        
                                                        column(width=6,checkboxGroupInput(inputId="selectlocationmetals",
                                                                                          label="Sélectionnez les location",
                                                                                          choices=unique(metals_bees$Location),
                                                                                          selected=unique(metals_bees$Location)
                                                        )))),
                                                        dataTableOutput("table_Metaux"),
                                                        column(width = 12, downloadButton("downloadData_metal", "Télécharger le tableau"))
                                             ),
                                             tabPanel("Graphiques",
                                                      wellPanel(
                                                        fluidRow(
                                                          column(width=3,
                                                                 selectInput(inputId = "choix_metal", label = "Choisissez le métal :",
                                                                             choices = l_metaux,
                                                                             multiple=F
                                                                 )
                                                          ),
                                                          column(width=3,
                                                                 selectInput(inputId = "choix_compar_metal", label = "Choisissez la strate :",
                                                                             choices = c("Location", "Hive"),
                                                                             multiple=F
                                                                             
                                                                 )
                                                          ),
                                                          column(width=3,
                                                                 dateRangeInput(inputId = "Explo_selectDate", label = "Choisissez une période :",
                                                                                start = min(metals_bees$Date), end = max(metals_bees$Date), 
                                                                                min = min(metals_bees$Date), max = max(metals_bees$Date), 
                                                                                format = "dd-mm-yyyy", language = "fr", separator = " à "),
                                                          ),
                                                          column(width=3,
                                                                 checkboxGroupInput(inputId = "cases_fonciers", label = "Sélectionnez un(des) foncier(s) :", selected = unique(metals_bees$Location),
                                                                                    choices = unique(metals_bees$Location), inline=T)
                                                          )
                                                        ),
                                                        #tags$hr(),
                                                        #fluidRow(
                                                          #actionButton("maj_g_explo", "Mettre à jour")
                                                        #)
                                                        
                                                      ),
                                                      tags$br(),
                                                      fluidRow(
                                                        column(width=5,
                                                               tabsetPanel(
                                                                 tabPanel(h4(textOutput("titre_boxPlotMetal"), style = "color: yellow; text-align: center;"),
                                                                          plotlyOutput("boxPlot_Metal")
                                                                 )
                                                               )
                                                            
                                                        ),
                                                        column(width=7,
                                                               tabsetPanel(
                                                                 tabPanel(h4(textOutput("titre_TempoMetal"), style = "color: yellow; text-align: center;"),
                                                                          plotlyOutput("Tempo_Metal")
                                                                 )
                                                               )
                                                               
                                                        )
                                                      )
                                             ),
                                             tabPanel("Résumé",
                                                      verbatimTextOutput("SummaryMetal"))
                                 )
                                 
                        )
                      )
             ), 
             tabPanel("Variabilité et comparaisons",
                      navlistPanel( widths = c(2, 10),
                                    tabPanel("Variabilité intra site",
                                             fluidRow(
                                               column(width=12, div(plotlyOutput("tTestIntraSite"
                                               ),style = "margin-bottom: 275px;")),
                                               column(width=12, card(
                                                 style = "background-color: black; color: white;",
                                                 card_header("Tests T intra sites"),
                                                 card_body(
                                                   p("Différence en log entre les deux ruches d'un même site, avec intervalle de confiance à 95 %. Un intervalle qui croise 0 indique l'absence de différence significative (p ≥ 0,05, non corrigée pour les tests multiples).")
                                                 )))
                                             )),
                                    tabPanel("Variabilité inter site",
                                             fluidRow(
                                               column(width=12, 
                                                      div(plotlyOutput("TestInterSite"),
                                                          style = "margin-bottom: 350px;")),
                                               column(width=12, card(
                                                 style = "background-color: black; color: white;",
                                                 card_header("Tests de comparaison inter sites"),
                                                 card_body(
                                                   p("Différence en log entre les deux sites différents (1er site - 2e site), avec intervalle de confiance à 95 %. Un intervalle qui croise 0 indique l'absence de différence significative (p ≥ 0,05).")
                                                 ))
                                               ),
                                               column(width=12, card(
                                                 style = "background-color: black; color: white;",
                                                 card_header("Modèle utilisé avec lmer : "),
                                                 card_body(withMathJax(
                                                   helpText("$$\\log(\\text{Concentration}) \\sim \\text{Metal} \\times \\text{Location} + (1 \\mid \\text{Hive})$$")
                                                 )))
                                                 
                                               )
                                             )
                                    )      
                      )
             ),
             tabPanel("Effet des traitements phytosanitaires",
                               tabsetPanel(
                                 tabPanel("Sur les teneurs en métaux",
                                          fluidRow(
                                            column(6, wellPanel(
                                              selectInput("tr_metaux", "Métaux à représenter :",
                                                          choices = l_metaux, selected = l_metaux[1], multiple = TRUE))),
                                            column(6, wellPanel(
                                              checkboxGroupInput("tr_mols", "Molécules de traitement à afficher :",
                                                                 choices = sort(unique(trimws(treat_pest$Molecule))),
                                                                 selected = sort(unique(trimws(treat_pest$Molecule))))))
                                          ),
                                          uiOutput("plots_metaux_trait")
                                 ),
                                 tabPanel("Sur les résidus de pesticides",
                                          plotOutput("tr_pesti")
                                 )
                               )
             ),
             
             # second onglet Visualisation
             tabPanel("Profils de contamination", 
                      tags$style(HTML("
                        .multicol {
                          column-count: 3;       /* Nombre de colonnes */
                          column-gap: 20px;      /* Espace entre colonnes */
                        }
                        .multicol .checkbox {
                          margin: 0;             /* Supprime marges inutiles */
                        }
                      ")),
                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(
                          checkboxInput(inputId = "ACP_PersoData", label = "Structuration de l'ACP", value=F),
                          conditionalPanel(
                            condition = "input.ACP_PersoData == true",
                            wellPanel(
                              p("Sélectionnez les métaux lourds que vous voulez analyser :"),
                              div(class="multicol", 
                                  checkboxGroupInput(inputId = "ACP_selectVar", label=NULL, selected = colnames(metals_bees[, -c(1:3)]), inline=T,
                                                     choices = colnames(metals_bees[, -c(1:3)]))
                              ),
                              tags$hr(),
                              p("Sélectionnez les ruches que vous voulez analyser :"),
                              div(class="multicol", 
                                  checkboxGroupInput(inputId = "ACP_selectRuche", label=NULL, selected = unique(metals_bees$Hive), inline=T,
                                                     choices = unique(metals_bees$Hive))
                              ),
                              tags$hr(),
                              p("Sélectionnez une période :"),
                              dateRangeInput(inputId = "ACP_selectDate", label = NULL,
                                             start = min(metals_bees$Date), end = max(metals_bees$Date), 
                                             min = min(metals_bees$Date), max = max(metals_bees$Date), 
                                             format = "dd-mm-yyyy", language = "fr", separator = " à "),
                              tags$hr(),
                              p("Sélectionnez les variables quantitatives supplémentaires :"),
                              div(class="multicol",
                                  checkboxGroupInput(inputId = "ACP_selectQuantiSup", label=NULL, inline=T,
                                                     choices = NULL)
                              ),
                              tags$hr(),
                              p("Sélectionnez les variables qualitatives supplémentaires :"),
                              div(class="multicol",
                                  checkboxGroupInput(inputId = "ACP_selectQualiSup", label=NULL, inline=T,
                                                     choices = colnames(metals_bees[, 1:3]))
                              ),
                              tags$hr(),
                              fluidRow(
                                column(width=12, align="center",
                                       actionButton("maj_ACP", "Mettre à jour")
                                )
                              )
                              
                            )
                            
                          ),
                          br(),
                          checkboxInput(inputId = "ACP_PersoGraph", label = "Personnalisation graphique de l'ACP", value=F),
                          
                          conditionalPanel(
                            condition = "input.ACP_PersoGraph == true",
                            wellPanel(
                              p("Sélectionnez le graphique à personnaliser : "),
                              radioButtons("ACP_selectGraphPerso", label=NULL, choices=c("individus", "variables"), selected="individus", inline=T),
                              conditionalPanel(
                                condition = "input.ACP_selectGraphPerso == 'individus'",
                                wellPanel(
                                  p("Représenter les ellipses :"),
                                  checkboxInput(inputId="ACP_ellispe", label="Ellipse ?"),
                                  tags$br(),
                                  p("Représenter l'habillage :"),
                                  radioButtons(inputId="ACP_selectHabillage", label=NULL, inline=T, choices=c("Habillage", "Pas d'habillage"), selected="Pas d'habillage")
                                  #checkboxInput(inputId="ACP_Habillage", label="Habillage ?")
                                  
                                )
                              )
                            )
                          ),
                        ),
                        
                        # Graphiques de l'ACP
                        mainPanel(
                          fluidRow(
                            column(width=6,
                                   plotOutput("ACP_graphDecompo")
                            ),
                            
                            column(width=6,
                                   conditionalPanel(
                                     condition = "input.maj_ACP > 0",
                                     wellPanel(
                                       h4("Sélectionnez les deux axes à représenter"),
                                       numericInput(inputId = "ACP_Axe1", label = NULL, value = 1, min=1, max=5),
                                       numericInput(inputId = "ACP_Axe2", label = NULL, value = 2, min=1, max=5)
                                     )
                                   )
                            )
                            
                          ),
                          tags$hr(),
                          fluidRow(
                            column(width=6,
                                   plotOutput("ACP_graphInd")
                            ),
                            column(width=6,
                                   plotOutput("ACP_graphVar")
                            ),
                            #######tableOutput("Test")
                          )
                          
                        )
                      )
                      
             )
  )
)