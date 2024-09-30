#########################################################################################################
##
## Land cover changes map
##
#########################################################################################################

rm(list=ls())
library(sp)
library(sf)
library(cowplot)
library(ggthemes)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)

## Load NFI plots 
nfiplot = readRDS("Rdata/nfiplot.rds")
nfiplot$lcc = ifelse(nfiplot$prior_agri>0, "Agrícola", ifelse(nfiplot$prior_pasture>0, "Pastura", NA))
nfiplot$prior = ifelse(nfiplot$prior_agri>0, nfiplot$prior_agri, 
                       ifelse(nfiplot$prior_pasture>0, nfiplot$prior_pasture, NA)) + 2020
nfiplot_lcc = nfiplot[!is.na(nfiplot$lcc),]

## Load limit
limit_cat = st_read(paste0(dirname(dirname(getwd())), 
                           "/CARTO-DATA/LIMITS/Catalunya/bm5mv21sh0tpp1_20200601_0.shp"), quiet=T)

## Plot it
p1 = ggplot() + geom_sf(data=st_geometry(nfiplot_lcc), 
                        aes(shape=nfiplot_lcc$lcc, color=nfiplot_lcc$prior), cex=2) +
  geom_sf(data=st_geometry(limit_cat), color="black", linewidth=1, fill=NA) +
  theme_map() + theme(legend.position="right", legend.key.size = unit(1, 'cm'),
                      legend.title=element_text(size=24), legend.text = element_text(size=18)) +
  ggspatial::annotation_scale(location = "bl", height = unit(0.25, "cm"), text_cex = 0.9) +
  labs(color="Any de transició", shape="Ús del sòl") + scale_color_viridis_c(option = "D")
p1
ggsave(p1, file=paste0("Plots/LandCoverChangeMap_OCCC.png"), width = 14, height = 7)  
