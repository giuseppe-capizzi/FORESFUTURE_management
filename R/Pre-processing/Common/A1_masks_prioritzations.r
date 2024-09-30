rm(list=ls())
library(sf)
library(tidyverse)


## NFI plots
forestfuture_sf = readRDS("Rdata/forestfuture_lq_sf.rds") # 10.261 plots
st_crs(forestfuture_sf)

## Mask with the protected areaa ------------------------------------------------------------------------------------

## Layer of ENPE: Espais Naturals de Protecció Especial
enpe_sf = st_read("Sources/EspaisNaturals/ESPAISNATURALS_ENPE.shp")

## Keep only the type of enpe:
# rnp = Reserva natural parcial 
# pnat = Parc natural
# pnin = paratge natural d'interés nacional
# pnac = parc naciona
# rni = resrves naturals integras
enpe_sf = enpe_sf %>% select(CODI_ESPAI)

## Intersect with NFI plots and create the mask 1 = management, 0 - no management for each scenario
## according to the enpe type
nfiplot_enpe_sf = st_join(forestfuture_sf, enpe_sf) 
nfiplot_enpe_sf$management_BAU = ifelse(is.na(nfiplot_enpe_sf$CODI_ESPAI) | nfiplot_enpe_sf$CODI_ESPAI %in% c("pnat", "pnin", "zpnat"),1,0)
nfiplot_enpe_sf$managment_AMF = nfiplot_enpe_sf$management_BAU 
nfiplot_enpe_sf$managment_RSB = nfiplot_enpe_sf$management_BAU
nfiplot_enpe_sf$managment_ASEA = ifelse(is.na(nfiplot_enpe_sf$CODI_ESPAI),1,0)
nfiplot_enpe_sf$managment_ACG = ifelse(is.na(nfiplot_enpe_sf$CODI_ESPAI) | nfiplot_enpe_sf$CODI_ESPAI %in% c("pnat", "pnin", "zpnat", "rnp", "pnac", "zpnac"),1,0)
saveRDS(nfiplot_enpe_sf, file= "Rdata/nfiplot_enpe.rds")
  # nfiplot_enpe_sf = readRDS("Rdata/nfiplot_enpe.rds")


## Prioritizations according to MCSC1993 ------------------------------------------------------------------------------------

## Raster with distance to Conreus/Prats of MCSC93, 100m in 31N ETRS89
dist_mcsc = stars::read_stars("Sources/MCSC93/DistConreusPrats_MCSC93_31N-ETRS89.tif") 
dist_mcsc_sf = st_as_sf(dist_mcsc)
st_crs(dist_mcsc_sf) = st_crs(forestfuture_sf)
nfiplot_dist_sf = st_join(nfiplot_enpe_sf, dist_mcsc_sf)  # 11.089 plots
## Search for duplicates when joining the distances and remove the extra ones. 
## Take the distance of the first "copy" with lossing much precision
dupli = duplicated(nfiplot_dist_sf$IDPARCELA)
id_dupli = which(dupli); head(id_dupli)
nfiplot_dist_sf  = nfiplot_dist_sf[-id_dupli,] 
names(nfiplot_dist_sf)[ncol(nfiplot_dist_sf)] = "dist_agripast"
saveRDS(nfiplot_dist_sf, file= "Rdata/nfiplot_dist.rds")
  
## First mark and select plots that could be converted to agricultural land:
## Parcel·les amb pendent < 25%  =~ 14º
## Excloent orientació nord
## Priorització per distància a zones agrícoles o pastures al 1993. 
nfiplot_sf = readRDS("Rdata/nfiplot_dist.rds")
nfiplot_sf$agri_conver = nfiplot_sf$slope <= 14 & (nfiplot_sf$aspect>45 & nfiplot_sf$aspect<=315)
table(nfiplot_sf$agri_conver)
plot_to_agri = nfiplot_sf[nfiplot_sf$agri_conver, c("IDPARCELA", "area", "dist_agripast")] 
plot_to_agri = plot_to_agri[order(plot_to_agri$dist_agripast),]  # sorted by distance, but also by ID
plot_to_agri$cumarea = cumsum(plot_to_agri$area)
target = 5700 # ha / year to be converted to agricultural land
plot_to_agri$prior_agri = 0
lower = 1
for(year in 1:20){
  upper = which(plot_to_agri$cumarea>=target*year)[1]
  plot_to_agri$prior_agri[lower:(upper-1)] = year  # selected by distance, but also ID, will it be great influence on forest dynamics? 
  lower = upper
}
id_to_agri = plot_to_agri$IDPARCELA[plot_to_agri$prior_agri!=0]  # 700
## Transfer prioritzation to nfiplot
nfiplot_sf$prior_agri = 0
for(id in id_to_agri){
  nfiplot_sf$prior_agri[nfiplot_sf$IDPARCELA == id] = 
    plot_to_agri$prior_agri[plot_to_agri$IDPARCELA == id]
}


## Second mark and select plots that could be converted to pastures:
## Parcel·les amb pendent < 40%  =~ 22º
## Priorització per distància a zones agrícoles o pastures al 1993. 
nfiplot_sf$pasture_conver = nfiplot_sf$slope <= 22 & !(nfiplot_sf$IDPARCELA %in% id_to_agri)
table(nfiplot_sf$pasture_conver)
plot_to_pasture = nfiplot_sf[nfiplot_sf$pasture_conver, c("IDPARCELA", "area", "dist_agripast")] 
plot_to_pasture = plot_to_pasture[order(plot_to_pasture$dist_agripast),]  # sorted by distance, but also by ID
plot_to_pasture$cumarea = cumsum(plot_to_pasture$area)
target = 3000 # ha / year to be converted to agricultural land
plot_to_pasture$prior_pasture = 0
lower = 1
for(year in 1:20){
  upper = which(plot_to_pasture$cumarea>=target*year)[1]
  plot_to_pasture$prior_pasture[lower:(upper-1)] = year  # selected by distance, but also ID, will it be great influence on forest dynamics? 
  lower = upper
}
id_to_pasture = plot_to_pasture$IDPARCELA[plot_to_pasture$prior_pasture!=0]  # 362
## Transfer prioritzation to nfiplot
nfiplot_sf$prior_pasture = 0
for(id in id_to_pasture){
  nfiplot_sf$prior_pasture[nfiplot_sf$IDPARCELA == id] = 
    plot_to_pasture$prior_pasture[plot_to_pasture$IDPARCELA == id]
}

## Remove unnecessary data
nfiplot_sf = nfiplot_sf %>% select(-"dist_agripast", -"agri_conver", -"pasture_conver")
saveRDS(nfiplot_sf, file = "Rdata/nfiplot.rds")
