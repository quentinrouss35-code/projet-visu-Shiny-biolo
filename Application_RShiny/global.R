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
library(emmeans)
library(lme4)


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

#Nettoyage du df metal et tests variabilité métaux inter intra
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

#intra site 
res_t_test_intra_loc <- data.frame()

for (loc in unique(as.character(df_metals_long$Location))) {
  for (m in unique(df_metals_long$Metal)) {
    d <- df_metals_long %>%
      filter(Location == loc, Metal == m) %>%
      select(Date, Hive, LogConcentration) %>%
      pivot_wider(names_from = Hive, values_from = LogConcentration) %>%
      drop_na()
    
    t <- t.test(d[[3]], d[[2]], paired = TRUE)   # ruche 2 - ruche 1
    res_t_test_intra_loc <- rbind(res_t_test_intra_loc, data.frame(
      Location = loc, Metal = m,
      estimate = unname(t$estimate),
      lower = t$conf.int[1], upper = t$conf.int[2],
      p = t$p.value
    ))
  }
}

#inter site 
modele_mixte <- lmer(LogConcentration ~ Metal * Location + (1 | Hive),
                     data = df_metals_long)

str(df_metals_long)

contrast_vin <- list(
  "Vineyard vs Forest"   = c(-1, 0, 0, 1),
  "Vineyard vs Suburban" = c(0, -1, 0, 1),
  "Vineyard vs Urban"    = c(0, 0, -1, 1)
)
contrast_other <- list(
  "Urban vs Suburban"  = c(0, -1, 1, 0),
  "Urban vs Forest"    = c(-1, 0, 1, 0),
  "Suburban vs Forest" = c(-1, 1, 0, 0)
)
metaux <- unique(df_metals_long$Metal)

results_cmp <- lapply(metaux, function(m) {
  emm_metal <- emmeans(modele_mixte, ~ Location, at = list(Metal = m))
  contr <- contrast(emm_metal, method = contrast_vin, adjust = "none")
  dfc <- as.data.frame(contr)
  dfc$Metal <- m
  return(dfc)
}) %>% bind_rows() %>%
  mutate(Significant = p.value < 0.05,
         Metal = factor(Metal, levels = rev(sort(unique(Metal)))))

results_extra_cmp <- lapply(metaux, function(m){
  emm_metal <- emmeans(modele_mixte, ~ Location, at = list(Metal = m))
  contr <- contrast(emm_metal, method = contrast_other, adjust = "none")
  dfc <- as.data.frame(contr)
  dfc$Metal <- m
  return(dfc)
}) %>% bind_rows() %>%
  mutate(Significant = p.value < 0.05,
         Metal = factor(Metal, levels = rev(sort(unique(Metal)))))

results_all <- bind_rows(results_cmp, results_extra_cmp) %>%
  mutate(
    Contrast_clean = factor(
      contrast,
      levels = c(
        "Vineyard vs Forest",
        "Vineyard vs Suburban",
        "Vineyard vs Urban",
        "Urban vs Suburban",
        "Urban vs Forest",
        "Suburban vs Forest"
      )
    )
  )

#Récupération des noms de métaux
l_metaux <- metals_bees |> 
  select(!Date:Location) |>
  colnames()

#Préparation pour graphes effet du traitement sur les métaux 
to_date <- function(x) if (inherits(x, "Date")) x else as.Date(x, format = "%d/%m/%Y")

treat_clean <- treat_pest |>
  mutate(Date = to_date(Date), Molecule = trimws(Molecule))

mols_all <- sort(unique(treat_clean$Molecule))
col_mols <- setNames(hcl.colors(length(mols_all), "Dark 3"), mols_all) 

#prétraitement données effet du traitement sur les résidus de pesticides mesurés
SITE    <- "Vineyard"   # "Vineyard", "Suburban", "Urban" ou "Forest"
MAX_LAG <- 60           # fenêtre après traitement (jours)

residus <- pest_bees |>
  filter(Location == SITE) |>
  pivot_longer(-c(Hive, Location, Date),
               names_to = "Molecule", values_to = "conc") |>
  mutate(Molecule = tolower(trimws(str_remove(Molecule, "^Pest_")))) |>  # "Pest_dimethomorph " -> "dimethomorph"
  group_by(Molecule, Date) |>
  summarise(conc = max(conc, na.rm = TRUE), .groups = "drop")

traitements <- treat_pest |>
  mutate(Molecule = tolower(trimws(as.character(Molecule)))) |>
  distinct(Molecule, Date) |>
  rename(Date_trait = Date)

df_pesti <- inner_join(traitements, residus, by = "Molecule",
                 relationship = "many-to-many") |>
  mutate(delai = as.numeric(Date - Date_trait)) |>
  filter(delai >= 0, delai <= MAX_LAG) |>
  mutate(event = paste0(Molecule, " (", format(Date_trait, "%d/%m"), ")"),
         event = fct_reorder(event, Date_trait, .desc = TRUE))  # ancien en haut

df_pesti <- df_pesti |> mutate(detecte = conc > 0)

