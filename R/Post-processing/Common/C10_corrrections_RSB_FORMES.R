# Fix the RSB for WB ES FORMES

rm(list=ls())
library(sp)
library(sf)
library(tidyverse)
library(FORMES)
library(FORMESutils)
library(readxl)
options(dplyr.summarise.inform = FALSE)

## A2.1 Build initial plotData ----------------------------------------------------------------------
nfiplot = readRDS("Rdata/nfiplot.rds")
nfiplot$IDPARCELA <- sub("^0+", "", nfiplot$IDPARCELA)
exclude <- st_drop_geometry(nfiplot[,c("IDPARCELA", "prior_agri", "prior_pasture")])
exclude_1 <- exclude[(exclude$prior_agri>0 & exclude$prior_agri< 11) | (exclude$prior_pasture>0 & exclude$prior_pasture< 11),]$IDPARCELA
exclude_2 <- exclude[exclude$prior_agri>10 | exclude$prior_pasture>10,]$IDPARCELA

# Load spatial data -------------------------------------------------------
comarques <- sf::read_sf("Data/Comarques/comarques.shp")
cat <- sf::st_union(comarques)

# Load calculated ES and select common IFN plots -------------------------------------
# ES_period_FORMES <- readRDS("Rdata/ES/ES_period_FORMES.rds")
ES_period_FORMES <- readRDS("Rdata/ES/ES_period_FORMES_0209.rds")

ids_formes <- unique(ES_period_FORMES$id)
n_plots_period <- length(ids_formes)
ES_period_FORDYN  <- readRDS("Rdata/ES/ES_period_MEDFATE_0209.rds")|>
  mutate(id = as.character(as.numeric(substr(id, 1,6)))) |>
  filter(id %in% ids_formes)
ES_period <- dplyr::bind_rows(ES_period_FORDYN, ES_period_FORMES) 
rm(ES_period_FORDYN)
rm(ES_period_FORMES)
gc()



# Replace FORMES RSB of exclude 1 and exclude 2 
exclude_1 # 1 filter RSB (2021-2030)
exclude_2 # 2 filter RSB (2031-2040)
head(ES_period)

# PERIOD ----
# replacement 1 
rep_1 <- ES_period |> filter(Management=="RSB", Period %in% c("2021-2030", "2031-2040","2031-2050",
                                                     "2041-2050", "2051-2060", "2081-2100",
                                                     "2061-2070", "2071-2080",
                                                     "2081-2090", "2091-2100"),
                    id %in% exclude_1) |> filter(Model =="FORDYN") |> select(Climate:Model, ES3_LAI:ES4_ErosionMitigation)

# rep_1 # dati da sostituire...
ES_period <- ES_period |> mutate(mask_1 = Management=="RSB" & Period %in% c("2021-2030", "2031-2040","2031-2050",
                                                            "2041-2050", "2051-2060", "2081-2100",
                                                            "2061-2070", "2071-2080",
                                                            "2081-2090", "2091-2100") &
                    id %in% exclude_1 & Model =="FORMES")


ES_period[ES_period$mask_1,"ES3_LAI"] <- rep_1$ES3_LAI
ES_period[ES_period$mask_1,"ES3_BlueWater"] <- rep_1$ES3_BlueWater
ES_period[ES_period$mask_1,"ES3_Precipitation"] <- rep_1$ES3_Precipitation
ES_period[ES_period$mask_1,"ES3_RunoffCoefficient"] <- rep_1$ES3_RunoffCoefficient
ES_period[ES_period$mask_1,"ES4_RainfallErosivity"] <- rep_1$ES4_RainfallErosivity
ES_period[ES_period$mask_1,"ES4_StructuralImpact"] <- rep_1$ES4_StructuralImpact
ES_period[ES_period$mask_1,"ES4_ErosionMitigation"] <- rep_1$ES4_ErosionMitigation
aa <- ES_period |> filter(mask_1) |> select(Climate:Model, ES3_LAI:ES4_ErosionMitigation)

all.equal(rep_1,aa)


# replacement 2 (start from next decade)
rep_2 <- ES_period |> filter(Management=="RSB", Period %in% c("2031-2040","2031-2050", "2081-2100",
                                                              "2041-2050", "2051-2060",
                                                              "2061-2070", "2071-2080",
                                                              "2081-2090", "2091-2100"),
                             id %in% exclude_2) |> filter(Model =="FORDYN") |> select(Climate:Model, ES3_LAI:ES4_ErosionMitigation)

# rep_2 # dati da sostituire...
ES_period <- ES_period |> mutate(mask_2 = Management=="RSB" & Period %in% c("2031-2040","2031-2050", "2081-2100",
                                                                            "2041-2050", "2051-2060",
                                                                            "2061-2070", "2071-2080",
                                                                            "2081-2090", "2091-2100") &
                                   id %in% exclude_2 & Model =="FORMES")


ES_period[ES_period$mask_2,"ES3_LAI"] <- rep_2$ES3_LAI
ES_period[ES_period$mask_2,"ES3_BlueWater"] <- rep_2$ES3_BlueWater
ES_period[ES_period$mask_2,"ES3_Precipitation"] <- rep_2$ES3_Precipitation
ES_period[ES_period$mask_2,"ES3_RunoffCoefficient"] <- rep_2$ES3_RunoffCoefficient
ES_period[ES_period$mask_2,"ES4_RainfallErosivity"] <- rep_2$ES4_RainfallErosivity
ES_period[ES_period$mask_2,"ES4_StructuralImpact"] <- rep_2$ES4_StructuralImpact
ES_period[ES_period$mask_2,"ES4_ErosionMitigation"] <- rep_2$ES4_ErosionMitigation
aa <- ES_period |> filter(mask_2) |> select(Climate:Model, ES3_LAI:ES4_ErosionMitigation)

all.equal(rep_2,aa)


# remove masks and save 
ES_period <- ES_period |> select(-mask_1, -mask_2)
saveRDS(ES_period, "Rdata/ES_Miquel/ES_period_0209.rds")
