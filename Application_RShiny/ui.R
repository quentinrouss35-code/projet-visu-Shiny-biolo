library(shiny)
library(colourpicker)
library(bslib)
library(rAmCharts)
library(plotly)
library(tidyverse)

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

# Importation des jeux de données
metals_bees <- read_csv2("../data/metals_bees.csv")
l_metaux <- metals_bees |> 
  select(!Date:Location) |>
  colnames()

#gsub(c(" mg/kg"), "", names(metals_bees))


# Define UI for application that draws a histogram
shinyUI(
  # navbarPage
  navbarPage("Est-ce qu’on peut utiliser les abeilles comme des bio-indicateurs pour détecter et évaluer la contamination de différents milieux environnementaux ?", theme=theme,
             tabPanel("Présentation de l'étude",
                      
             ),
             
             # Onglet d'analyse descriptive des différents jeux de données de l'étude
             tabPanel("Analyse Descriptive",
                      navlistPanel(
                        "Choix du tableau de données", widths = c(2, 10),
                        tabPanel("Occupation des sols sur un rayon de 3km",
                                 tabsetPanel(id = "Onglet_OccupSol",
                                             tabPanel("Tableau de données",
                                                      dataTableOutput("table_Sol")
                                             ),
                                             tabPanel("Graphiques",
                                             )
                                 )
                        ),
                        tabPanel("Traitement phytosanitaire dans les vignognes",
                                 tabsetPanel(id = "Onglet_PestTrait",
                                             tabPanel("Tableau de données",
                                                      dataTableOutput("table_Trait")
                                             ),
                                             tabPanel("Graphiques",
                                             )
                                 )
                        ),
                        tabPanel("Pesticides détectés",
                                 tabsetPanel(id = "Onglet_pest",
                                             tabPanel("Tableau de données",
                                                      checkboxInput("filtre_zero_pest",
                                                                    "Enlever les lignes ne contenant que des 0",
                                                                    value = FALSE),
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
                                                        column(width=4,
                                                               wellPanel(
                                                                 selectInput(inputId = "choix_metal", label = "Choisissez le métal à représenter",
                                                                             choices = l_metaux,
                                                                             multiple=F
                                                                 )
                                                               )
                                                        ),
                                                        column(width=1),
                                                        column(width=2,
                                                               br(),
                                                               actionButton("maj_g_explo", "Mettre à jour")
                                                        ),
                                                        column(width=1),
                                                        column(width=4,
                                                               wellPanel(
                                                                 selectInput(inputId = "choix_compar_metal", label = "Choisissez la strate (donner meilleur nom)",
                                                                             choices = c("Location", "Hive"),
                                                                             multiple=F
                                                                 )                                         
                                                               )
                                                        )
                                                      ),
                                                      fluidRow(
                                                        column(width=5,
                                                               plotlyOutput("boxPlot_Metal")
                                                        ),
                                                        column(width=1,
                                                               checkboxGroupInput(inputId = "cases_fonciers", label = "Sélectionnez un(des) foncier(s) :", selected = c("Forest", "Suburban", "Urban", "Vineyard"),
                                                                                  choices = c("Forêt" = "Forest", "Banlieue" = "Suburban", "Urbain" = "Urban", "Vignoble" = "Vineyard"))),
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
                      )
             )
  )
)