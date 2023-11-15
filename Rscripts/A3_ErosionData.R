# Copy files from data server (EMFtoolbox could also be used)
# scp -r mcaceres@65.109.109.162:/srv/emf_data/emf/datasets/Soils/Sources/Europe/Erosion_ESDAC .
library(terra)

nfiplot <- dplyr::bind_rows(readRDS(paste0("Rdata/nfiplot.rds")))
nfi_ErosionData_sf <- nfiplot[,"id"]


# L-S factor
LS_spain <-terra::rast("Data/Erosion_ESDAC/LSFactor_ES/Spain.tif")
nfi_geom_sf <- sf::st_transform(sf::st_geometry(nfiplot), crs(LS_spain))
nfi_LS <- terra::extract(LS_spain,vect(nfi_geom_sf))
nfi_ErosionData_sf$LS <- nfi_LS$Spain

# K factor
K <-terra::rast("Data/Erosion_ESDAC/KFactor/K_new_crop.tif")
nfi_geom_sf <- sf::st_transform(sf::st_geometry(nfiplot), crs(K))
nfi_K <- terra::extract(K,vect(nfi_geom_sf))
nfi_ErosionData_sf$K <- nfi_K$K_new_crop

# Kst factor
Kst <-terra::rast("Data/Erosion_ESDAC/Kst_Factor/Kst_correct_noneg.tif")
nfi_geom_sf <- sf::st_transform(sf::st_geometry(nfiplot), crs(Kst))
nfi_Kst <- terra::extract(Kst,vect(nfi_geom_sf))
nfi_ErosionData_sf$Kst <- nfi_Kst$Kst_correct_noneg

sf::write_sf(nfi_ErosionData_sf, "Data/ErosionData.gpkg")
