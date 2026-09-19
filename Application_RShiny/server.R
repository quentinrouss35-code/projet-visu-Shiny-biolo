library(shiny)
library(rAmCharts)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  metals_bees <- read_csv2("../data/metals_bees.csv")
  metals_bees <- metals_bees |> 
    mutate(Date = as.Date(Date, format="%d/%m/%Y"),
           Location = as.factor(Location), 
           Hive = as.factor(Hive)) |> 
    mutate(`Aluminium mg/kg` = as.numeric(ifelse(grepl(c("^<"), `Aluminium mg/kg`), round(as.numeric(gsub(",", ".", substring(`Arsenic mg/kg`, 3)))/3,2), gsub(",", ".", `Aluminium mg/kg`)))) |>
    
    mutate(`Arsenic mg/kg` = as.numeric(ifelse(grepl(c("^<"), `Arsenic mg/kg`), round(as.numeric(gsub(",", ".", substring(`Arsenic mg/kg`, 3)))/3,2), gsub(",", ".", `Arsenic mg/kg`)))) |> 
    
    mutate(`Cadmium mg/kg` = as.double(ifelse(grepl(c("^<"), `Cadmium mg/kg`), round(as.numeric(gsub(",", ".", substring(`Cadmium mg/kg`, 3)))/3,2), gsub(",", ".", `Cadmium mg/kg`)))) |>
    
    mutate(`Chromium mg/kg` = as.double(ifelse(grepl(c("^<"), `Chromium mg/kg`), round(as.numeric(gsub(",", ".", substring(`Chromium mg/kg`, 3)))/3,2), gsub(",", ".", `Chromium mg/kg`)))) |>
    
    mutate(`Cobalt mg/kg` = as.double(ifelse(grepl(c("^<"), `Cobalt mg/kg`), round(as.numeric(gsub(",", ".", substring(`Cobalt mg/kg`, 3)))/3,2), gsub(",", ".", `Cobalt mg/kg`)))) |> 
    
    mutate(`Lead mg/kg` = as.double(ifelse(grepl(c("^<"), `Lead mg/kg`), round(as.numeric(gsub(",", ".", substring(`Lead mg/kg`, 3)))/3,2), gsub(",", ".", `Lead mg/kg`))))
  
  pest_bees <- read_csv2("../data/pesticide_bees.csv")
  pest_bees <- pest_bees |> 
    mutate(Date = as.Date(Date, format="%d/%m/%Y"),
           Location = as.factor(Location), 
           Hive = as.factor(Hive))
  pest_bees[is.na(pest_bees)] <- 0
  
  soil_occup <- read_csv2("../data/soil.occupation.3km.csv")
  soil_occup <- soil_occup |> 
    mutate(Location = as.factor(Location))
  
  treat_pest <- read_csv2("../data/treatments_pesticides.csv")
  treat_pest <- treat_pest |> 
    mutate(Date = as.Date(Date, format="%d/%m/%Y"),
           Molecule = as.factor(Molecule))
  
  
  
  
  # table
  output$table <- renderDataTable({
    metals_bees
  })
})
