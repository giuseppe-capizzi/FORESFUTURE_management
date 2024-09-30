library(medfateland)
library(dplyr)

#Read IFN3 sf
IFN3_sf <- readRDS("~/OneDrive/mcaceres_work/model_initialisation/medfate_initialisation/IFN/Products/IFN3/Catalunya/IFN3_cat_final_ETRS89H31.rds")

#Subset Plot types : A1, A3E, A4E, NN
forestfuture_sf <- IFN3_sf |>
  filter(IDCLASE %in% c("A1", "A3E", "NN"))

#Determine dominant tree species
forestfuture_sf$dominant_tree_species <- sapply(forestfuture_sf$forest, stand_dominantTreeSpecies, SpParamsMED)

# Isolate tree data for FORMES
forestfuture_sf$tree_data_formes <- lapply(forestfuture_sf$forest, function(x){
  if(!is.null(x)) {
    td <- x$treeData
    if(nrow(td)>0){
      return(td[td$DBH>=7.5, c("IFNcode","Species", "DBH", "Height", "N"), drop=FALSE])
    }
  }
  return(data.frame("IFNcode"=character(0), "Species" = character(0), "DBH" = numeric(0), "Height" = numeric(0), "N"=numeric(0)))
})


# Get estratos (what to do with estrato 0?)
# Barcelona
plot_estrato_08 <- read.csv("~/OneDrive/EMF_datasets/ForestInventories/IFN/Sources/IFN3/BBDD-Sig/Sig3p08/Parcpoly.csv", sep="\t")
plot_estrato_08 |>
  as_tibble() |>
  select(Estrato, Parcela, Cla, SubClase) |>
  mutate(IDPARCELA = paste0("0", as.character(8*10000+Parcela)),
         IDCLASE = paste0(Cla, SubClase),
         id = paste0(IDPARCELA,"_", IDCLASE)) |>
  filter(IDCLASE %in% c("A1", "A3E", "NN")) -> plot_estrato_08 
nplots_estrato_08 <- plot_estrato_08 |>
  group_by(Estrato) |>
  summarise(Npar = n())
polygon_08 <- read.csv("~/OneDrive/EMF_datasets/ForestInventories/IFN/Sources/IFN3/BBDD-Sig/Sig3p08/Poligon.csv", sep="\t")
polygon_08 |>
  group_by(Estrato) |>
  summarise(area_estrato = sum(SUPERFICIE)) |>
  right_join(nplots_estrato_08, by = "Estrato") |>
  mutate(area = area_estrato/Npar) |> #Area per parcela
  select(Estrato, area) -> estratos_08
estratos_08$area[estratos_08$Estrato==0] <- mean(estratos_08$area[estratos_08$Estrato!=0]) # Set the area represented by 0 to mean of other strata
plot_estrato_08 |>
  right_join(estratos_08, by = "Estrato") -> plot_estrato_08

# Girona
plot_estrato_17 <- read.csv("~/OneDrive/EMF_datasets/ForestInventories/IFN/Sources/IFN3/BBDD-Sig/Sig3p17/Parcpoly.csv", sep="\t")
plot_estrato_17 |>
  as_tibble() |>
  select(Estrato, Parcela, Cla, SubClase) |>
  mutate(IDPARCELA = paste0("", as.character(17*10000+Parcela)),
         IDCLASE = paste0(Cla, SubClase),
         id = paste0(IDPARCELA,"_", IDCLASE)) |>
  filter(IDCLASE %in% c("A1", "A3E", "NN")) -> plot_estrato_17 
nplots_estrato_17 <- plot_estrato_17 |>
  group_by(Estrato) |>
  summarise(Npar = n())
polygon_17 <- read.csv("~/OneDrive/EMF_datasets/ForestInventories/IFN/Sources/IFN3/BBDD-Sig/Sig3p17/Poligon.csv", sep="\t")
polygon_17 |>
  group_by(Estrato) |>
  summarise(area_estrato = sum(SUPERFICIE)) |>
  right_join(nplots_estrato_17, by = "Estrato") |>
  mutate(area = area_estrato/Npar) |> #Area per parcela
  select(Estrato, area) -> estratos_17
estratos_17$area[estratos_17$Estrato==0] <- mean(estratos_17$area[estratos_17$Estrato!=0]) # Set the area represented by 0 to mean of other strata
plot_estrato_17 |>
  right_join(estratos_17, by = "Estrato") -> plot_estrato_17

plot_estrato_25 <- read.csv("~/OneDrive/EMF_datasets/ForestInventories/IFN/Sources/IFN3/BBDD-Sig/Sig3p25/Parcpoly.csv", sep="\t")
plot_estrato_25 |>
  as_tibble() |>
  select(Estrato, Parcela, Cla, SubClase) |>
  mutate(IDPARCELA = paste0("", as.character(25*10000+Parcela)),
         IDCLASE = paste0(Cla, SubClase),
         id = paste0(IDPARCELA,"_", IDCLASE)) |>
  filter(IDCLASE %in% c("A1", "A3E", "NN")) -> plot_estrato_25 
nplots_estrato_25 <- plot_estrato_25 |>
  group_by(Estrato) |>
  summarise(Npar = n())
polygon_25 <- read.csv("~/OneDrive/EMF_datasets/ForestInventories/IFN/Sources/IFN3/BBDD-Sig/Sig3p25/Poligon.csv", sep="\t")
polygon_25 |>
  group_by(Estrato) |>
  summarise(area_estrato = sum(SUPERFICIE)) |>
  right_join(nplots_estrato_25, by = "Estrato") |>
  mutate(area = area_estrato/Npar) |> #Area per parcela
  select(Estrato, area) -> estratos_25
estratos_25$area[estratos_25$Estrato==0] <- mean(estratos_25$area[estratos_25$Estrato!=0]) # Set the area represented by 0 to mean of other strata
plot_estrato_25 |>
  right_join(estratos_25, by = "Estrato") -> plot_estrato_25

plot_estrato_43 <- read.csv("~/OneDrive/EMF_datasets/ForestInventories/IFN/Sources/IFN3/BBDD-Sig/Sig3p43/Parcpoly.csv", sep="\t")
plot_estrato_43 |>
  as_tibble() |>
  select(Estrato, Parcela, Cla, SubClase) |>
  mutate(IDPARCELA = paste0("", as.character(43*10000+Parcela)),
         IDCLASE = paste0(Cla, SubClase),
         id = paste0(IDPARCELA,"_", IDCLASE)) |>
  filter(IDCLASE %in% c("A1", "A3E", "NN")) -> plot_estrato_43 
nplots_estrato_43 <- plot_estrato_43 |>
  group_by(Estrato) |>
  summarise(Npar = n())
polygon_43 <- read.csv("~/OneDrive/EMF_datasets/ForestInventories/IFN/Sources/IFN3/BBDD-Sig/Sig3p43/Poligon.csv", sep="\t")
polygon_43 |>
  group_by(Estrato) |>
  summarise(area_estrato = sum(SUPERFICIE)) |>
  right_join(nplots_estrato_43, by = "Estrato") |>
  mutate(area = area_estrato/Npar) |> #Area per parcela
  select(Estrato, area) -> estratos_43
estratos_43$area[estratos_43$Estrato==0] <- mean(estratos_43$area[estratos_43$Estrato!=0]) # Set the area represented by 0 to mean of other strata
plot_estrato_43 |>
  right_join(estratos_43, by = "Estrato") -> plot_estrato_43

plot_estrato <- bind_rows(plot_estrato_08, plot_estrato_17, plot_estrato_25, plot_estrato_43)

forestfuture_sf |>
  right_join(plot_estrato[,c("Estrato", "id", "area")], by="id") -> forestfuture_sf

sum(is.na(forestfuture_sf$Estrato))
sum(is.na(forestfuture_sf$area))
sum(forestfuture_sf$area) # 1678493 ha
table(forestfuture_sf$Estrato)

# Generate input for FORMES
df <- sf::st_drop_geometry(forestfuture_sf)[,c("id", "tree_data_formes")]
forestfuture_tree_data <-tidyr::unnest(df, cols = c("id","tree_data_formes"))

# Store RDS
saveRDS(forestfuture_sf, "Rdata/forestfuture_sf.rds")
saveRDS(forestfuture_tree_data, "Rdata/forestfuture_tree_data.rds")
