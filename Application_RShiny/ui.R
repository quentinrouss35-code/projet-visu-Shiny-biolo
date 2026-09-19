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
             tabPanel("Analyse Descriptive",
                      navlistPanel(
                        "Choix de la représentation du dataset", widths = c(3, 9),
                        tabPanel("Description Globale",
                                 
                        ),
                        tabPanel("Analyse exploratoire",
                        ),
                        tabPanel("Tableau de données",
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