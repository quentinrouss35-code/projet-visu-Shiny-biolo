#Chargement des librairies
if (!require(treemapify)) install.packages("treemapify", dependencies = TRUE)
if (!require(rclipboard)) install.packages("rclipboard", dependencies = TRUE)
if (!require(factoextra)) install.packages("factoextra", dependencies = TRUE)
library(shiny)
library(colourpicker)
library(bslib)
library(rAmCharts)
library(plotly)
library(tidyverse)
library(DT)
library(FactoMineR)
library(treemapify)
library(sf)
library(ggplot2)
library(leaflet)
library(dplyr)
library(factoextra)
library(shinydashboard)
library(rclipboard)

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

#Nettoyage du df metal et t-test
df_metals_clean <- as.data.frame(metals_bees)

format_metal_label <- function(metal_name) {
  metal_name %>%
    str_remove("\\.g\\.kg|\\.mg\\.kg") %>%
    str_replace_all("\\.", " ") %>%
    str_to_title()
}

df_metals_long <- df_metals_clean %>%
  pivot_longer(cols = -c(Date, Hive, Location),
               names_to = "Metal_raw", values_to = "Concentration") %>%
  mutate(Metal = format_metal_label(Metal_raw))
#Pivot du data frame pour avoir une colonne "métal" et une colonne "concentration"

df_metals_long$Location <- as.factor(df_metals_long$Location)
levels(df_metals_long$Location) <- gsub("[()]", "", levels(df_metals_long$Location))
levels(df_metals_long$Location) <- gsub(" ", "_", levels(df_metals_long$Location))
#supprime toute parenthèse et remplace chaque espace par un underscore

df_metals_long <- df_metals_long %>%
  mutate(LogConcentration = log(Concentration + 1e-6))

res_t_test_intra_hive <- data.frame()

for (loc in unique(as.character(df_metals_long$Location))){
  for (m in unique(df_metals_long$Metal)){
    d <- subset(df_metals_long, Location==loc & Metal==m)
    d$Hive <- droplevels(factor(d$Hive))
    t <- t.test(LogConcentration ~ Hive, data=d)
    res_t_test_intra_hive <- rbind(res_t_test_intra_hive, data.frame(
      Location = loc,
      Metal = m,
      estimate = unname(t$estimate[2]-t$estimate[1]),
      lower    = -t$conf.int[2],
      upper    = -t$conf.int[1],
      p        = t$p.value
    ))
  }
}
res_t_test_intra_hive$Significant <- res_t_test_intra_hive$p < 0.05
res_t_test_intra_hive$Metal <- factor(res_t_test_intra_hive$Metal, levels = rev(sort(unique(res_t_test_intra_hive$Metal))))
res_t_test_intra_hive <- res_t_test_intra_hive %>%
  mutate(Signif = factor(ifelse(p < 0.05, "p < 0.05", "p ≥ 0.05"),
                         levels = c("p ≥ 0.05", "p < 0.05")))




#Récupération des noms de métaux
l_metaux <- metals_bees |> 
  select(!Date:Location) |>
  colnames()

#Préparation pour graphes effet du traitement sur les résidus de pesticides mesurés
to_date <- function(x) if (inherits(x, "Date")) x else as.Date(x, format = "%d/%m/%Y")

