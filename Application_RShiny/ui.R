library(shiny)
library(colourpicker)
library(bslib)
library(rAmCharts)
library(plotly)

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
  navbarPage("Est-ce qu’on peut utiliser les abeilles comme des bio-indicateurs pour détecter et évaluer la contamination de différents milieux environnementaux ?", theme=theme,
             # premier onglet Data
             tabPanel("Analyse Descriptive",
                      navlistPanel(
                        "Choix de la représentation du dataset", widths = c(3, 9),
                        tabPanel("Description Globale",
                                 
                        ),
                        tabPanel("Analyse exploratoire",
                        ),
                        tabPanel("Tableau de données",
                                 fluidRow(
                                   column(width=6,
                                          wellPanel(
                                            selectInput(inputId = "choix_tab", label = "Dataset au choix",
                                                        choices = c("métaux", "pesticides", "occupation du sol", "traitement pesticides")
                                            )
                                          )
                                   ),
                                   column(width=6,
                                        downloadButton("tél_tab", "Télécharger le jeu de données")  
                                   )
                                 ),
                                 dataTableOutput("table")
                        )
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