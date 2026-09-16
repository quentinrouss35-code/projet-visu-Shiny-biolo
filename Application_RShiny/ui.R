library(shiny)
library(colourpicker)
library(bslib)
library(rAmCharts)
library(plotly)

theme_test <- bs_theme(
  # Controls the default grayscale palette
  bg = "#202123", fg = "white",
  # Controls the accent (e.g., hyperlink, button, etc) colors
  primary = "#FFD000", secondary = "#664312",
  base_font = c("Grandstander", "sans-serif"),
  code_font = c("Courier", "monospace"),
  heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  # Can also add lower-level customization
  "input-border-color" = "#FFD000"
)


# Define UI for application that draws a histogram
shinyUI(
  # navbarPage
  navbarPage("Premiers pas avec shiny", theme=theme_test,
             # premier onglet Data
             tabPanel("Data",
                      navlistPanel(
                        "Choix de la représentation du dataset",
                        tabPanel("dataTable",
                                 # table
                                 h1("Tableau de données faithful", style="color: #FFD000;"),
                                 dataTableOutput("table")),
                        tabPanel("Summary",
                                 # summary
                                 verbatimTextOutput("summary"))
                      )
             ), 
             
             # second onglet Visualisation
             tabPanel("Visualisation", 
                      
                      # Sidebar with a slider input for number of bins 
                      fluidRow(
                        column(width=4,
                               sliderInput("bins",
                                           "Number of bins:",
                                           min = 1,
                                           max = 50,
                                           value = 30),
                               
                               # input pour la couleur
                               colourInput(inputId = "color", label = "Couleur :", value = "#FFD000"),
                               
                               # titre du graphique
                               textInput(inputId = "titre", label = "Titre :", value = "Histogramme"),
                               
                               # selection de la colonne
                               radioButtons(inputId = "var", label = "Variable : ", choices = colnames(faithful)),
                               actionButton("go_graph", "Update !")
                        ),
                        column(width=8,
                               tabsetPanel(id = "viz",
                                           tabPanel("Histogramme",
                                                    # Show a plot of the generated distribution
                                                    #plotOutput("distPlot"),
                                                    amChartsOutput("distPlot"),
                                                    # classes (div centrée)
                                                    div(textOutput("n_bins"), align = "center")),
                                           tabPanel("Boxplot",
                                                    #plotOutput("boxPlot"))
                                                    amChartsOutput("boxPlot")
                                           ),
                                           tabPanel("Nuage de points",
                                                    plotlyOutput("nuage_points")
                                           )
                                           
                               )
                        )
                      )
             )
  )
)