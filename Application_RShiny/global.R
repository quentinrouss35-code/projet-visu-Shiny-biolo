#Chargement des librairies
library(shiny)
library(colourpicker)
library(bslib)
library(rAmCharts)
library(plotly)
library(tidyverse)
library(DT)
library(FactoMineR)

# Importation des jeux de données

  #Métaux
metals_bees <- read_csv2("../data/metals_bees.csv")
metals_bees <- metals_bees |> 
  mutate(Date = as.Date(Date, format="%d/%m/%Y"),
         Location = as.factor(Location), 
         Hive = factor(Hive, levels=c("B1", "N1", "B20", "C17", "C2", "C21", "B3", "N10"))) |> 
  mutate(`Aluminium mg/kg` = as.numeric(ifelse(grepl(c("^<"), `Aluminium mg/kg`), round(as.numeric(gsub(",", ".", substring(`Aluminium mg/kg`, 3)))/3,2), gsub(",", ".", `Aluminium mg/kg`)))) |>
  
  mutate(`Arsenic mg/kg` = as.numeric(ifelse(grepl(c("^<"), `Arsenic mg/kg`), round(as.numeric(gsub(",", ".", substring(`Arsenic mg/kg`, 3)))/3,2), gsub(",", ".", `Arsenic mg/kg`)))) |> 
  
  mutate(`Cadmium mg/kg` = as.double(ifelse(grepl(c("^<"), `Cadmium mg/kg`), round(as.numeric(gsub(",", ".", substring(`Cadmium mg/kg`, 3)))/3,2), gsub(",", ".", `Cadmium mg/kg`)))) |>
  
  mutate(`Chromium mg/kg` = as.double(ifelse(grepl(c("^<"), `Chromium mg/kg`), round(as.numeric(gsub(",", ".", substring(`Chromium mg/kg`, 3)))/3,2), gsub(",", ".", `Chromium mg/kg`)))) |>
  
  mutate(`Cobalt mg/kg` = as.double(ifelse(grepl(c("^<"), `Cobalt mg/kg`), round(as.numeric(gsub(",", ".", substring(`Cobalt mg/kg`, 3)))/3,2), gsub(",", ".", `Cobalt mg/kg`)))) |> 
  
  mutate(`Lead mg/kg` = as.double(ifelse(grepl(c("^<"), `Lead mg/kg`), round(as.numeric(gsub(",", ".", substring(`Lead mg/kg`, 3)))/3,2), gsub(",", ".", `Lead mg/kg`))))

  #Pesticides
pest_bees <- read_csv2("../data/pesticide_bees.csv")
pest_bees <- pest_bees |> 
  mutate(Date = as.Date(Date, format="%d/%m/%Y"),
         Location = as.factor(Location), 
         Hive = as.factor(Hive))
pest_bees[is.na(pest_bees)] <- 0

  #Occupation des sols
soil_occup <- read_csv2("../data/soil.occupation.3km.csv")
soil_occup <- soil_occup |> 
  mutate(Location = as.factor(Location))

  #Traitement des pesticides
treat_pest <- read_csv2("../data/treatments_pesticides.csv")
treat_pest <- treat_pest |> 
  mutate(Date = as.Date(Date, format="%d/%m/%Y"),
         Molecule = as.factor(Molecule))


  #
data_consolidé <- read_csv2("../data/pesticide_trait_bees.csv")
data_consolidé <- data_consolidé |>
  mutate(Date = as.Date(Date, format="%d/%m/%Y"),
         Location = as.factor(Location), 
         Hive = as.factor(Hive)) |> 
  full_join(metals_bees, by=c("Date", "Location", "Hive"))
data_consolidé[is.na(data_consolidé)] <- 0




#Récupération des noms de métaux
l_metaux <- metals_bees |> 
  select(!Date:Location) |>
  colnames()

message("global.R est exécuté, wd = ", getwd())