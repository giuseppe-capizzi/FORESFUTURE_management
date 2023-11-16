library(ggplot2)
library(cowplot)
library(sf)
library(tidyverse)
library(tidyterra)


# Load spatial data -------------------------------------------------------
nfiplot <- dplyr::bind_rows(readRDS(paste0("Rdata/nfiplot.rds")))
comarques <- sf::read_sf("Data/Comarques/comarques.shp")
cat <- sf::st_union(comarques)

# Load calculated ES and select common IFN plots -------------------------------------
ES_ALL_MEDFATE_sf  <- readRDS("Rdata/ES_MEDFATE.rds")|>
  mutate(id = as.character(as.numeric(substr(id, 1,6))))
ES_ALL_FORMES_sf <- readRDS("Rdata/ES_FORMES.rds")
ids_formes <- unique(ES_ALL_sf$id[ES_ALL_sf$Model=="FORMES"])
ES_ALL_MEDFATE_sf <- ES_ALL_MEDFATE_sf |>
  filter(id %in% ids_formes)
ES_ALL_sf <- dplyr::bind_rows(ES_ALL_MEDFATE_sf,
                              ES_ALL_FORMES_sf)
ES_ALL <- sf::st_drop_geometry(ES_ALL_sf)


# Functions to generate plots/maps --------------------------------------------------------
plot_ES <- function(ES_all, var, ylab, ylim, outlier = Inf, add_formes = FALSE) {
  ES_sum <- ES_all |>
    filter(!(Management %in% c("NOG", "NOGEST"))) |>
    filter({{var}} < outlier) |>
    group_by(Climate, Management, Period, MidYear, Model) |>
    summarise(ES = mean({{var}}, na.rm=TRUE), 
              ES_se = sd({{var}}, na.rm=TRUE)/sqrt(n()),
              ES_q25 = quantile({{var}}, probs=0.25, na.rm = TRUE),
              ES_q75 = quantile({{var}}, probs=0.75, na.rm = TRUE),
              .groups = "drop")
  p1<-ggplot(ES_sum[ES_sum$Climate=="RCP45" & ES_sum$Model =="MEDFATE",])+
    geom_point(aes(x = MidYear, y = ES, col = Management))+
    geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
    geom_line(aes(x = MidYear, y = ES, col = Management))+
    scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                       labels = unique(ES_sum$Period))+
    scale_fill_discrete("Escenari")+
    scale_color_discrete("Escenari")+
    ylab(ylab)+ ylim(ylim)+labs(title = "MEDFATE / RCP 4.5")+theme_bw()
  l <- get_legend(p1)
  p2<-ggplot(ES_sum[ES_sum$Climate=="RCP85" & ES_sum$Model =="MEDFATE",])+
    geom_point(aes(x = MidYear, y = ES, col = Management))+
    geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
    geom_line(aes(x = MidYear, y = ES, col = Management))+
    scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                       labels = unique(ES_sum$Period))+
    ylab("")+ ylim(ylim)+labs(title = "MEDFATE / RCP 8.5")+ theme_bw()
  pA <- plot_grid(p1 + theme(legend.position = "none"), 
                  p2+ theme(legend.position = "none"), nrow = 1)
  if(add_formes) {
    p3<-ggplot(ES_sum[ES_sum$Climate=="RCP45" & ES_sum$Model =="FORMES",])+
      geom_point(aes(x = MidYear, y = ES, col = Management))+
      geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
      geom_line(aes(x = MidYear, y = ES, col = Management))+
      scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                         labels = unique(ES_sum$Period))+
      ylab(ylab)+ ylim(ylim)+labs(title = "FORMES / RCP 4.5")+theme_bw()
    l <- get_legend(p1)
    p4<-ggplot(ES_sum[ES_sum$Climate=="RCP85"& ES_sum$Model =="FORMES",])+
      geom_point(aes(x = MidYear, y = ES, col = Management))+
      geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
      geom_line(aes(x = MidYear, y = ES, col = Management))+
      scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                         labels = unique(ES_sum$Period))+
      ylab("")+ ylim(ylim)+labs(title = "FORMES / RCP 8.5")+ theme_bw()
    pB <- plot_grid(p3 + theme(legend.position = "none"), 
                    p4+ theme(legend.position = "none"), nrow = 1)
    pALL <- plot_grid(pA,pB, nrow = 2)
  } else {
    pALL <- pA
  }
  return(plot_grid(pALL, l, rel_widths = c(1,0.2)))
}

sf::st_bbox(nfiplot)
xmin <- 260000
xmax <- 520000
ymin <- 4495000
ymax <- 4750000
res = 5000

map_scenario<-function(sf_ALL, var, climate_scen, breaks, breaks_diff, units, type = "div", palette = "YlGnBu") {
  r <-terra::rast(resolution  = c(res,res), xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
  limits <- c(min(breaks), max(breaks))
  limits_diff <- c(min(breaks_diff), max(breaks_diff))
  leg_pos <- c(0.85,0.2)
  
  sf_1 <- sf_ALL |>
    filter(Period == "2001-2020", Climate == climate_scen, Management == "BAU") |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var)
  
  sf_2 <- sf_ALL |>
    filter(Period == "2041-2060", Climate == climate_scen) |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var) |>
    mutate(diff_AMF = AMF - BAU,
           diff_RSB = RSB - BAU,
           diff_ASEA = ASEA - BAU,
           diff_ACG = ACG - BAU) 
  
  sf_3 <- sf_ALL |>
    filter(Period == "2081-2100", Climate == climate_scen) |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var) |>
    mutate(diff_AMF = AMF - BAU,
           diff_RSB = RSB - BAU,
           diff_ASEA = ASEA - BAU,
           diff_ACG = ACG - BAU)
  
  raster_01<-terra::rasterize(terra::vect(sf_1),r, "BAU", fun = mean, na.rm = TRUE)
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "BAU", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "BAU", fun = mean, na.rm = TRUE)
  names(raster_01) <- "m1"
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g1<-ggplot()+
    geom_spatraster(aes(fill=m1), data = raster_01)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "BAU", subtitle = "2011")+
    theme_bw()
  g2<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2051")+
    theme_bw()
  g3<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2091")+
    theme_bw()
  ## ASEA
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_ASEA", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_ASEA", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g22<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "ASEA Diff.", subtitle = "2051")+
    theme_bw()
  g23<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2091")+
    theme_bw()
  ## ACG
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_ACG", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_ACG", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g32<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "ACG Diff.", subtitle = "2051")+
    theme_bw()
  g33<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2091")+
    theme_bw()
  ## AMF
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_AMF", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_AMF", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g42<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "AMF Diff.", subtitle = "2051")+
    theme_bw()
  g43<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2091")+
    theme_bw()
  ## RSB
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_RSB", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_RSB", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g52<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "RSB Diff.", subtitle = "2051")+
    theme_bw()
  g53<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2091")+
    theme_bw()
  ll <- plot_grid(get_legend(g1), get_legend(g22), nrow = 1)
  g<-plot_grid(g1+theme(legend.position = "none"),
               g2+theme(legend.position = "none"),
               g3+theme(legend.position =  "none"), 
               ll,
               g22+theme(legend.position = "none"), 
               g23+theme(legend.position = "none"),
               NULL,
               g32+theme(legend.position = "none"), 
               g33+theme(legend.position = "none"),
               NULL,
               g42+theme(legend.position = "none"), 
               g43+theme(legend.position = "none"),
               NULL,
               g52+theme(legend.position = "none"), 
               g53+theme(legend.position = "none"),
               nrow = 5, ncol = 3)
  return(g)
}

# ES1_VolumeStructure -----------------------------------------------------
d_ES <- plot_ES(ES_ALL, ES1_VolumeStructure, "Stock fusta estructural (m3/ha)", c(0,250), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES1_VolumeStructure.png",d_ES, width = 10, height = 8, bg = "white")
breaks = seq(0,200, by=25)
breaks_diff = c(-100,-50, -25, -5, 5, 25, 50, 100)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_VolumeStructure", climate_scen = "RCP45", 
                        breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
ggsave2("Plots/ES_maps/ES1_VolumeStructure_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_VolumeStructure", climate_scen = "RCP45",
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
ggsave2("Plots/ES_maps/ES1_VolumeStructure_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_VolumeStructure", climate_scen = "RCP85", 
                        breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
ggsave2("Plots/ES_maps/ES1_VolumeStructure_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_VolumeStructure", climate_scen = "RCP85",
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
ggsave2("Plots/ES_maps/ES1_VolumeStructure_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES1_VolumeAdultFirewood -------------------------------------------------
d_ES <- plot_ES(ES_ALL, ES1_VolumeAdultFirewood, "Stock llenyes (m3/ha)", c(0,100), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES1_VolumeAdultFirewood.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES1_VolumeAdultFirewood)
breaks = seq(0,200, by=25)
breaks_diff = c(-100,-50, -25, -5, 5, 25, 50, 100)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_VolumeAdultFirewood", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
ggsave2("Plots/ES_maps/ES1_VolumeAdultFirewood_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_VolumeAdultFirewood", climate_scen = "RCP45",
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
ggsave2("Plots/ES_maps/ES1_VolumeAdultFirewood_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_VolumeAdultFirewood", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
ggsave2("Plots/ES_maps/ES1_VolumeAdultFirewood_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_VolumeAdultFirewood", climate_scen = "RCP85",
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
ggsave2("Plots/ES_maps/ES1_VolumeAdultFirewood_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES1_CutStructure --------------------------------------------------------
d_ES <- plot_ES(ES_ALL, ES1_CutStructure, "Provisió de fusta estructural (m3/ha/any)", c(0,4), outlier = 25, add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES1_CutStructure.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES1_CutStructure)
summary(ES_ALL_FORMES_sf$ES1_CutStructure)
breaks = c(0,0.1,0.5,1,2,5,10, 50)
breaks_diff = c(-50, -10,-5, -2, -1,1, 2,5, 10, 50)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_CutStructure", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
ggsave2("Plots/ES_maps/ES1_CutStructure_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_CutStructure", climate_scen = "RCP45",
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
ggsave2("Plots/ES_maps/ES1_CutStructure_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_CutStructure", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
ggsave2("Plots/ES_maps/ES1_CutStructure_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_CutStructure", climate_scen = "RCP85",
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
ggsave2("Plots/ES_maps/ES1_CutStructure_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES1_CutAdultFirewood ----------------------------------------------------
d_ES <- plot_ES(ES_ALL, ES1_CutAdultFirewood, ylab= "Provisió de llenyes (m3/ha/any)", 
                ylim = c(0,5), outlier = 20, add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES1_CutAdultFirewood.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES1_CutAdultFirewood)
summary(ES_ALL_FORMES_sf$ES1_CutAdultFirewood)
breaks = c(0,0.1,0.2, 0.5,1,2,5,20)
breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_CutAdultFirewood", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
ggsave2("Plots/ES_maps/ES1_CutAdultFirewood_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_CutAdultFirewood", climate_scen = "RCP45",
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
ggsave2("Plots/ES_maps/ES1_CutAdultFirewood_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_CutAdultFirewood", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
ggsave2("Plots/ES_maps/ES1_CutAdultFirewood_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_CutAdultFirewood", climate_scen = "RCP85",
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
ggsave2("Plots/ES_maps/ES1_CutAdultFirewood_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")

# ES2_AdultTreeBiomass --------------------------------------------
d_ES <- plot_ES(ES_ALL, ES2_AdultTreeBiomass, ylab = "Stock de carboni arbres (Mg C/ha)", ylim = c(0,600), outlier = 1000, add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES2_AdultTreeBiomass.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES2_AdultTreeBiomass)
summary(ES_ALL_FORMES_sf$ES2_AdultTreeBiomass)
breaks = c(0,25,50,100,200, 500, 1000, 3000)
breaks_diff = c(-1000, -500, -200, -100, -50, -25, 25, 50, 100 ,200, 500, 1000)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_AdultTreeBiomass", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomass_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_AdultTreeBiomass", climate_scen = "RCP45",
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomass_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_AdultTreeBiomass", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomass_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_AdultTreeBiomass", climate_scen = "RCP85",
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomass_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")

# ES2_AdultTreeBiomassChange --------------------------------------------
d_ES <- plot_ES(ES_ALL, ES2_AdultTreeBiomassChange, ylab = "Embornal de carboni arbres (Mg C/ha/any)", 
                outlier = 50, ylim = c(-1,10), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES2_AdultTreeBiomassChange.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES2_AdultTreeBiomassChange)
summary(ES_ALL_FORMES_sf$ES2_AdultTreeBiomassChange)
breaks = c(-10,-0.5, 0.5,1,2,5,10,50)
breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_AdultTreeBiomassChange", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassChange_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_AdultTreeBiomassChange", climate_scen = "RCP45",
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassChange_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_AdultTreeBiomassChange", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassChange_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_AdultTreeBiomassChange", climate_scen = "RCP85",
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassChange_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES2_CutBiomassStructure --------------------------------------------
d_ES <- plot_ES(ES_ALL, ES2_CutBiomassStructure, ylab = "Embornal de carboni fusta estructural (Mg C/ha/any)", 
                outlier = 80, ylim = c(0,30), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES2_CutBiomassStructure.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES2_CutBiomassStructure)
summary(ES_ALL_FORMES_sf$ES2_CutBiomassStructure)
breaks = c(-10,-0.5, 0.5,1,2,5,10,50)
breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_CutBiomassStructure", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_CutBiomassStructure_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_CutBiomassStructure", climate_scen = "RCP45",
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_CutBiomassStructure_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_CutBiomassStructure", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_CutBiomassStructure_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_CutBiomassStructure", climate_scen = "RCP85",
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_CutBiomassStructure_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES2_AdultTreeBiomassSequestr --------------------------------------------
d_ES <- plot_ES(ES_ALL, ES2_AdultTreeBiomassSequestr, ylab = "Embornal de carboni arbres+fusta (Mg C/ha/any)", 
                outlier = 80, ylim = c(-5,30), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES2_AdultTreeBiomassSequestr.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES2_AdultTreeBiomassSequestr)
summary(ES_ALL_FORMES_sf$ES2_AdultTreeBiomassSequestr)
breaks = c(-10,-0.5, 0.5,1,2,5,10,50)
breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_AdultTreeBiomassSequestr", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassSequestr_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_AdultTreeBiomassSequestr", climate_scen = "RCP45",
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassSequestr_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_AdultTreeBiomassSequestr", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassSequestr_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_AdultTreeBiomassSequestr", climate_scen = "RCP85",
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassSequestr_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES2_LiveBiomassSequestr --------------------------------------------
d_ES <- plot_ES(ES_ALL, ES2_LiveBiomassSequestr, ylab = "Embornal de carboni total (Mg C/ha/any)", ylim = c(0,8), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES2_LiveBiomassSequestr.png",d_ES, width = 10, height = 4, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES2_LiveBiomassSequestr)
summary(ES_ALL_FORMES_sf$ES2_LiveBiomassSequestr)
breaks = c(-10,-0.5, 0.5,1,2,5,10,50)
breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_LiveBiomassSequestr", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_LiveBiomassSequestr_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_LiveBiomassSequestr", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_LiveBiomassSequestr_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES3_BlueWater -----------------------------------------------------------
d_ES <- plot_ES(ES_ALL, ES3_BlueWater, ylab = "Aigua blava (mm/any)", ylim = c(150,300), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES3_BlueWater.png",d_ES, width = 10, height = 4, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES3_BlueWater)
breaks = seq(0,320, by=40)
breaks_diff = seq(-200,200, by = 40)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES3_BlueWater", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "mm/any")
ggsave2("Plots/ES_maps/ES3_BlueWater_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES3_BlueWater", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "mm/any")
ggsave2("Plots/ES_maps/ES3_BlueWater_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES3_RunoffCoefficient ---------------------------------------------------
d_ES <- plot_ES(ES_ALL, ES3_RunoffCoefficient, ylab = "Coeficient d'escolament [%]", ylim = c(23,45), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES3_RunoffCoefficient.png",d_ES, width = 10, height = 4, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES3_RunoffCoefficient)
breaks = c(0,5,10,20,40,60,80, 100)
breaks_diff = c(-50,-10,-20,-5, -2,2,5,10,20,50)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES3_RunoffCoefficient", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "%")
ggsave2("Plots/ES_maps/ES3_RunoffCoefficient_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES3_RunoffCoefficient", climate_scen = "RCP85", 
             breaks = breaks, breaks_diff = breaks_diff, units = "%")
ggsave2("Plots/ES_maps/ES3_RunoffCoefficient_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES4_ErosionMitigation ---------------------------------------------------
d_ES <- plot_ES(ES_ALL, ES4_ErosionMitigation, ylab = "Mitigació de l'erosió (Mg/ha/any)", ylim = c(100,150), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES4_ErosionMitigation.png",d_ES, width = 10, height = 4, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES4_ErosionMitigation)
breaks = c(0,25, 50, 100, 150, 200, 300, 4000)
breaks_diff = c(-100, -50,-10,-5, 5,10,50, 100)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES4_ErosionMitigation", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "Mg/ha/any")
ggsave2("Plots/ES_maps/ES4_ErosionMitigation_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES4_ErosionMitigation", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "Mg/ha/any")
ggsave2("Plots/ES_maps/ES4_ErosionMitigation_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES5_RecreationalValue ---------------------------------------------------
d_ES <- plot_ES(ES_ALL, ES5_RecreationalValue, ylab = "Valor recreatiu [0-1]", ylim = c(0.4,0.55), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES5_RecreationalValue.png",d_ES, width = 10, height = 4, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES5_RecreationalValue)
breaks = c(0,0.05,0.1,0.2,0.4,0.6,0.8, 1)
breaks_diff = c(-0.5,-0.3, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.5)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES5_RecreationalValue", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "[0-1]")
ggsave2("Plots/ES_maps/ES5_RecreationalValue_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES5_RecreationalValue", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "[0-1]")
ggsave2("Plots/ES_maps/ES5_RecreationalValue_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES6_SurfaceFirePotential  ---------------------------------------------------
d_ES <- plot_ES(ES_ALL, ES6_SurfaceFirePotential, ylab = "Risk d'incendi de superficie [0-9]", ylim = c(7,9), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES6_SurfaceFirePotential.png",d_ES, width = 10, height = 4, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES6_SurfaceFirePotential)
breaks = seq(0,9, by=1)
breaks_diff = c(-5,-4,-3,-2,-1,1,2,3,4,5)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES6_SurfaceFirePotential", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "[0-1]")
ggsave2("Plots/ES_maps/ES6_SurfaceFirePotential_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES6_SurfaceFirePotential", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "[0-1]")
ggsave2("Plots/ES_maps/ES6_SurfaceFirePotential_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")

# ES6_CrownFirePotential  ---------------------------------------------------
d_ES <- plot_ES(ES_ALL, ES6_CrownFirePotential, ylab = "Risk d'incendi de capçada [0-9]", ylim = c(4,7), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES6_CrownFirePotential.png",d_ES, width = 10, height = 4, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES6_CrownFirePotential)
breaks = seq(0,9, by=1)
breaks_diff = c(-5,-4,-3,-2,-1,1,2,3,4,5)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES6_CrownFirePotential", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "[0-1]")
ggsave2("Plots/ES_maps/ES6_CrownFirePotential_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES6_CrownFirePotential", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "[0-1]")
ggsave2("Plots/ES_maps/ES6_CrownFirePotential_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


