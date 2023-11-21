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
ES_state_FORMES <- readRDS("Rdata/ES_state_FORMES.rds")
ids_formes <- unique(ES_state_FORMES$id)
n_plots_state <- length(ids_formes)
ES_state_MEDFATE  <- readRDS("Rdata/ES_state_MEDFATE.rds")|>
  mutate(id = as.character(as.numeric(substr(id, 1,6))))|>
  filter(id %in% ids_formes)
ES_state <- dplyr::bind_rows(ES_state_MEDFATE, ES_state_FORMES) 
rm(ES_state_MEDFATE)
rm(ES_state_FORMES)

ES_period_FORMES <- readRDS("Rdata/ES_period_FORMES.rds")
ids_formes <- unique(ES_period_FORMES$id)
n_plots_period <- length(ids_formes)
ES_period_MEDFATE  <- readRDS("Rdata/ES_period_MEDFATE.rds")|>
  mutate(id = as.character(as.numeric(substr(id, 1,6)))) |>
  filter(id %in% ids_formes)
ES_period <- dplyr::bind_rows(ES_period_MEDFATE, ES_period_FORMES) 
rm(ES_period_MEDFATE)
rm(ES_period_FORMES)
gc()

# Functions to generate plots --------------------------------------------------------
plot_ES_state <- function(ES_state, var, ylab, ylim, outliers = c(-Inf,Inf), add_formes = FALSE) {
  ES_sum <- ES_state |>
    filter(!(Management %in% c("NOG", "NOGEST")),
           {{var}} > outliers[1],
           {{var}} < outliers[2]) |>
    group_by(Climate, Management, Year, Model) |>
    summarise(ES = mean({{var}}, na.rm=TRUE), 
              ES_se = sd({{var}}, na.rm=TRUE)/sqrt(sum(!is.na({{var}}))),
              ES_q25 = quantile({{var}}, probs=0.25, na.rm = TRUE),
              ES_q75 = quantile({{var}}, probs=0.75, na.rm = TRUE),
              .groups = "drop")
  p1<-ggplot(ES_sum[ES_sum$Climate=="RCP45" & ES_sum$Model =="MEDFATE",])+
    geom_point(aes(x = Year, y = ES, col = Management))+
    geom_ribbon(aes(x = Year, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
    geom_line(aes(x = Year, y = ES, col = Management))+
    geom_vline(xintercept = 2020, linetype="dashed")+
    annotate("rect", xmin = 1995, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
    scale_x_continuous("",limits = c(1995,2105), expand = c(0,0))+
    scale_fill_discrete("Escenari")+
    scale_color_discrete("Escenari")+
    ylab(ylab)+ ylim(ylim)+labs(title = "MEDFATE / RCP 4.5")+theme_bw()
  l <- get_legend(p1)
  p2<-ggplot(ES_sum[ES_sum$Climate=="RCP85" & ES_sum$Model =="MEDFATE",])+
    geom_point(aes(x = Year, y = ES, col = Management))+
    geom_ribbon(aes(x = Year, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
    geom_line(aes(x = Year, y = ES, col = Management))+
    geom_vline(xintercept = 2020, linetype="dashed")+
    annotate("rect", xmin = 1995, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
    scale_x_continuous("",limits = c(1995,2105), expand = c(0,0))+
    ylab("")+ ylim(ylim)+labs(title = "MEDFATE / RCP 8.5")+ theme_bw()
  pA <- plot_grid(p1 + theme(legend.position = "none"), 
                  p2+ theme(legend.position = "none"), nrow = 1)
  if(add_formes) {
    p3<-ggplot(ES_sum[ES_sum$Climate=="RCP45" & ES_sum$Model =="FORMES",])+
      geom_point(aes(x = Year, y = ES, col = Management))+
      geom_ribbon(aes(x = Year, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
      geom_line(aes(x = Year, y = ES, col = Management))+
      geom_vline(xintercept = 2020, linetype="dashed")+
      annotate("rect", xmin = 1995, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_x_continuous("",limits = c(1995,2105), expand = c(0,0))+
      ylab(ylab)+ ylim(ylim)+labs(title = "FORMES / RCP 4.5")+theme_bw()
    l <- get_legend(p1)
    p4<-ggplot(ES_sum[ES_sum$Climate=="RCP85"& ES_sum$Model =="FORMES",])+
      geom_point(aes(x = Year, y = ES, col = Management))+
      geom_ribbon(aes(x = Year, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
      geom_line(aes(x = Year, y = ES, col = Management))+
      geom_vline(xintercept = 2020, linetype="dashed")+
      annotate("rect", xmin = 1995, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_x_continuous("",limits = c(1995,2105), expand = c(0,0))+
      ylab("")+ ylim(ylim)+labs(title = "FORMES / RCP 8.5")+ theme_bw()
    pB <- plot_grid(p3 + theme(legend.position = "none"), 
                    p4+ theme(legend.position = "none"), nrow = 1)
    pALL <- plot_grid(pA,pB, nrow = 2)
  } else {
    pALL <- pA
  }
  return(plot_grid(pALL, l, rel_widths = c(1,0.2)))
}
plot_ES_period <- function(ES_period, var, ylab, ylim, outliers = c(-Inf,Inf), add_formes = FALSE) {
  ES_sum <- ES_period |>
    filter(Period %in% paste0(seq(2001,2091, by=10), "-",seq(2010,2100, by=10)),
           !(Management %in% c("NOG", "NOGEST")),
           {{var}} > outliers[1],
           {{var}} < outliers[2]) |>
    group_by(Climate, Management, Period, MidYear, Model) |>
    summarise(ES = mean({{var}}, na.rm=TRUE), 
              ES_se = sd({{var}}, na.rm=TRUE)/sqrt(sum(!is.na({{var}}))),
              ES_q25 = quantile({{var}}, probs=0.25, na.rm = TRUE),
              ES_q75 = quantile({{var}}, probs=0.75, na.rm = TRUE),
              .groups = "drop")
  p1<-ggplot(ES_sum[ES_sum$Climate=="RCP45" & ES_sum$Model =="MEDFATE",])+
    geom_point(aes(x = MidYear, y = ES, col = Management))+
    geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
    geom_line(aes(x = MidYear, y = ES, col = Management))+
    geom_vline(xintercept = 2020, linetype="dashed")+
    annotate("rect", xmin = 2000, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
    scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                       labels = unique(ES_sum$Period), limits = c(2000,2101), expand = c(0,0))+    
    scale_fill_discrete("Escenari")+
    scale_color_discrete("Escenari")+
    ylab(ylab)+ ylim(ylim)+labs(title = "MEDFATE / RCP 4.5")+theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  l <- get_legend(p1)
  p2<-ggplot(ES_sum[ES_sum$Climate=="RCP85" & ES_sum$Model =="MEDFATE",])+
    geom_point(aes(x = MidYear, y = ES, col = Management))+
    geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
    geom_line(aes(x = MidYear, y = ES, col = Management))+
    geom_vline(xintercept = 2020, linetype="dashed")+
    annotate("rect", xmin = 2000, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
    scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                       labels = unique(ES_sum$Period), limits = c(2000,2101), expand = c(0,0))+
    ylab("")+ ylim(ylim)+labs(title = "MEDFATE / RCP 8.5")+ theme_bw()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  pA <- plot_grid(p1 + theme(legend.position = "none"), 
                  p2+ theme(legend.position = "none"), nrow = 1)
  if(add_formes) {
    p3<-ggplot(ES_sum[ES_sum$Climate=="RCP45" & ES_sum$Model =="FORMES",])+
      geom_point(aes(x = MidYear, y = ES, col = Management))+
      geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
      geom_line(aes(x = MidYear, y = ES, col = Management))+
      geom_vline(xintercept = 2020, linetype="dashed")+
      annotate("rect", xmin = 2000, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                         labels = unique(ES_sum$Period), limits = c(2000,2101), expand = c(0,0))+
      ylab(ylab)+ ylim(ylim)+labs(title = "FORMES / RCP 4.5")+theme_bw()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    l <- get_legend(p1)
    p4<-ggplot(ES_sum[ES_sum$Climate=="RCP85"& ES_sum$Model =="FORMES",])+
      geom_point(aes(x = MidYear, y = ES, col = Management))+
      geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
      geom_line(aes(x = MidYear, y = ES, col = Management))+
      geom_vline(xintercept = 2020, linetype="dashed")+
      annotate("rect", xmin = 2000, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_x_continuous("",breaks = unique(ES_sum$MidYear), limits = c(2000,2101), expand = c(0,0),
                         labels = unique(ES_sum$Period))+
      ylab("")+ ylim(ylim)+labs(title = "FORMES / RCP 8.5")+ theme_bw()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    pB <- plot_grid(p3 + theme(legend.position = "none"), 
                    p4+ theme(legend.position = "none"), nrow = 1)
    pALL <- plot_grid(pA,pB, nrow = 2)
  } else {
    pALL <- pA
  }
  return(plot_grid(pALL, l, rel_widths = c(1,0.2)))
}

# Functions to generate maps --------------------------------------------------------
sf::st_bbox(nfiplot)
xmin <- 260000
xmax <- 520000
ymin <- 4495000
ymax <- 4750000
res = 5000

map_scenario_state<-function(sf_ALL, var, model, climate_scen, breaks, breaks_diff, units, type = "div", palette = "YlGnBu") {
  r <-terra::rast(resolution  = c(res,res), xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
  limits <- c(min(breaks), max(breaks))
  limits_diff <- c(min(breaks_diff), max(breaks_diff))
  leg_pos <- c(0.85,0.2)
  
  sf_1 <- sf_ALL |>
    filter(Year == 2010, Climate == climate_scen, Management == "BAU") |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var)
  sf_2 <- sf_ALL |>
    filter(Year == 2050, Climate == climate_scen) |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var) |>
    mutate(diff_AMF = AMF - BAU,
           diff_RSB = RSB - BAU,
           diff_ASEA = ASEA - BAU,
           diff_ACG = ACG - BAU) 
  
  sf_3 <- sf_ALL |>
    filter(Year == 2090, Climate == climate_scen) |>
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
    labs(fill=units, title = paste(model, climate_scen, "BAU", sep=" / "), subtitle = "2010")+
    theme_bw()
  g2<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2050")+
    theme_bw()
  g3<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2090")+
    theme_bw()
  ## ASEA
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_ASEA" , fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_ASEA", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g22<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = paste(model, climate_scen, "ASEA Diff.", sep=" / "), subtitle = "2050")+
    theme_bw()
  g23<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2090")+
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
    labs(fill=units, title = paste(model, climate_scen, "ACG Diff.", sep=" / "), subtitle = "2050")+
    theme_bw()
  g33<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2090")+
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
    labs(fill=units, title = paste(model, climate_scen, "AMF Diff.", sep=" / "), subtitle = "2050")+
    theme_bw()
  g43<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2090")+
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
    labs(fill=units, title = paste(model, climate_scen, "RSB Diff.", sep=" / "), subtitle = "2050")+
    theme_bw()
  g53<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2090")+
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

map_scenario_states<-function(ES_state, var, breaks, breaks_diff, units, type = "div", palette = "YlGnBu", draw_formes = TRUE) {
  m_ES <- map_scenario_state(ES_state[ES_state$Model=="MEDFATE",], model = "MEDFATE", var = var, climate_scen = "RCP45", 
                             breaks = breaks, breaks_diff = breaks_diff, units = units, type = type, palette = palette)
  ggsave2(paste0("Plots/ES_maps/", var, "_medfate_rcp45.png"),m_ES, width = 13, height = 22, bg = "white")
  m_ES <- map_scenario_state(ES_state[ES_state$Model=="MEDFATE",], model = "MEDFATE",var = var, climate_scen = "RCP85", 
                             breaks = breaks, breaks_diff = breaks_diff, units = units)
  ggsave2(paste0("Plots/ES_maps/",var,"_medfate_rcp85.png"),m_ES, width = 13, height = 22, bg = "white")
  if(draw_formes) {
    m_ES <- map_scenario_state(ES_state[ES_state$Model=="FORMES",], model = "FORMES",var = var, climate_scen = "RCP45",
                               breaks = breaks, breaks_diff = breaks_diff, units = units)
    ggsave2(paste0("Plots/ES_maps/",var,"_formes_rcp45.png"),m_ES, width = 13, height = 22, bg = "white")
    m_ES <- map_scenario_state(ES_state[ES_state$Model=="FORMES",], model = "FORMES", var = var, climate_scen = "RCP85",
                               breaks = breaks, breaks_diff = breaks_diff, units = units)
    ggsave2(paste0("Plots/ES_maps/",var,"_formes_rcp85.png"),m_ES, width = 13, height = 22, bg = "white")
  }
}

map_scenario_period<-function(sf_ALL, var, model, climate_scen, breaks, breaks_diff, units, type = "div", palette = "YlGnBu") {
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
    labs(fill=units, title = paste(model, climate_scen, "BAU", sep=" / "), subtitle = "2001-2020")+
    theme_bw()
  g2<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2041-2060")+
    theme_bw()
  g3<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2081-2100")+
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
    labs(fill=units, title = paste(model, climate_scen, "ASEA Diff.", sep=" / "), subtitle = "2041-2060")+
    theme_bw()
  g23<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2081-2100")+
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
    labs(fill=units, title = paste(model, climate_scen, "ACG Diff.", sep=" / "), subtitle = "2041-2060")+
    theme_bw()
  g33<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2081-2100")+
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
    labs(fill=units, title = paste(model, climate_scen, "AMF Diff.", sep=" / "), subtitle = "2041-2060")+
    theme_bw()
  g43<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2081-2100")+
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
    labs(fill=units, title = paste(model, climate_scen,"RSB Diff.", sep=" / "), subtitle = "2041-2060")+
    theme_bw()
  g53<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2081-2100")+
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
map_scenario_periods<-function(ES_period, var, breaks, breaks_diff, units, type = "div", palette = "YlGnBu", draw_formes = TRUE) {
  m_ES <- map_scenario_period(ES_period[ES_period$Model=="MEDFATE",], model = "MEDFATE", var = var, climate_scen = "RCP45", 
                             breaks = breaks, breaks_diff = breaks_diff, units = units, type = type, palette = palette)
  ggsave2(paste0("Plots/ES_maps/", var, "_medfate_rcp45.png"),m_ES, width = 13, height = 22, bg = "white")
  m_ES <- map_scenario_period(ES_period[ES_period$Model=="MEDFATE",], model = "MEDFATE",var = var, climate_scen = "RCP85", 
                             breaks = breaks, breaks_diff = breaks_diff, units = units)
  ggsave2(paste0("Plots/ES_maps/",var,"_medfate_rcp85.png"),m_ES, width = 13, height = 22, bg = "white")
  if(draw_formes) {
    m_ES <- map_scenario_period(ES_period[ES_period$Model=="FORMES",], model = "FORMES",var = var, climate_scen = "RCP45",
                                breaks = breaks, breaks_diff = breaks_diff, units = units)
    ggsave2(paste0("Plots/ES_maps/",var,"_formes_rcp45.png"),m_ES, width = 13, height = 22, bg = "white")
    m_ES <- map_scenario_period(ES_period[ES_period$Model=="FORMES",], model = "FORMES", var = var, climate_scen = "RCP85",
                                breaks = breaks, breaks_diff = breaks_diff, units = units)
    ggsave2(paste0("Plots/ES_maps/",var,"_formes_rcp85.png"),m_ES, width = 13, height = 22, bg = "white")
  }
}


# ES1_VolumeStructure -----------------------------------------------------
d_ES <- plot_ES_state(ES_state, ES1_VolumeStructure, "Stock fusta estructural (m3/ha)", c(0,250), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES1_VolumeStructure.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_state$ES1_VolumeStructure)
map_scenario_states(ES_state, var = "ES1_VolumeStructure", 
                    breaks = c(0,10, 25,50,100,150, 200, 300,500, 1000), 
                    breaks_diff = c(-500, -200, -100,-50, -25, -10, 10, 25, 50, 100, 200, 500), 
                    units = "m3/ha")

# ES1_VolumeAdultFirewood -------------------------------------------------
d_ES <- plot_ES_state(ES_state, ES1_VolumeAdultFirewood, "Stock llenyes (m3/ha)", c(0,100), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES1_VolumeAdultFirewood.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_state$ES1_VolumeAdultFirewood)
map_scenario_states(ES_state, var = "ES1_VolumeAdultFirewood", 
                    breaks = c(0,5, 10, 25, 50, 75, 100, 125, 150, 200), 
                    breaks_diff = c(-100,-75,-50, -25,-10, -5, 5, 10, 25, 50, 75, 100), 
                    units = "m3/ha")

# ES1_CutStructure --------------------------------------------------------
d_ES <- plot_ES_period(ES_period, ES1_CutStructure, "Provisió de fusta estructural (m3/ha/any)", ylim = c(0,3), 
                       outliers = c(-1,25), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES1_CutStructure.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period$ES1_CutStructure)
map_scenario_periods(ES_period, var = "ES1_CutStructure", 
                    breaks = c(0,0.1,0.5,1,2,5,10, 20, 50), 
                    breaks_diff = c(-50, -20,-10,-5, -2, -1,1, 2,5, 10, 20, 50), 
                    units = "m3/ha/any")

# ES1_CutAdultFirewood ----------------------------------------------------
d_ES <- plot_ES_period(ES_period, ES1_CutAdultFirewood, ylab= "Provisió de llenyes (m3/ha/any)", 
                ylim = c(0,2), outliers = c(-2,20), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES1_CutAdultFirewood.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period$ES1_CutAdultFirewood)
map_scenario_periods(ES_period, var = "ES1_CutAdultFirewood", 
                     breaks = c(0,0.1,0.2, 0.5,1,2,5,10, 20), 
                     breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20), 
                     units = "m3/ha/any")

# ES2_AdultTreeBiomass --------------------------------------------
d_ES <- plot_ES_state(ES_state, ES2_AdultTreeBiomass, ylab = "Stock de carboni arbres (Mg C/ha)", ylim = c(0,700), 
                outliers = c(-1, 1000), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES2_AdultTreeBiomass.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_state$ES2_AdultTreeBiomass)
map_scenario_states(ES_state, var = "ES2_AdultTreeBiomass", 
                    breaks = c(0,10,20, 50,100,200, 500, 1000, 2000), 
                    breaks_diff = c(-1000, -500, -200, -100, -50, -10, 10, 50, 100 ,200, 500, 1000), 
                    units = "MgC/ha")

# ES2_AdultTreeBiomassChange --------------------------------------------
d_ES <- plot_ES_period(ES_period, ES2_AdultTreeBiomassChange, ylab = "Embornal de carboni arbres (Mg C/ha/any)", 
                outliers = c(-10,10), ylim = c(-1,6), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES2_AdultTreeBiomassChange.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period$ES2_AdultTreeBiomassChange)
map_scenario_periods(ES_period, var = "ES2_AdultTreeBiomassChange",
                     breaks = c(-10,-0.5, 0.5,1,2,5,10,20, 50), 
                     breaks_diff = c(-20, -10, -5, -2, -1,-0.5, 0.5, 1, 2,5, 10, 20), 
                     units = "MgC/ha/any")

# ES2_CutBiomassStructure --------------------------------------------
d_ES <- plot_ES_period(ES_period, ES2_CutBiomassStructure, ylab = "Embornal de carboni fusta estructural (Mg C/ha/any)", 
                outlier = c(-1,50), ylim = c(0,5), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES2_CutBiomassStructure.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period$ES2_CutBiomassStructure)
map_scenario_periods(ES_period, var = "ES2_CutBiomassStructure",
                     breaks = c(0.0,0.1, 0.2, 0.5,1,2,5,10,20, 50), 
                     breaks_diff = c(-50, -20, -5, -2, -1,-0.5, 0.5, 1, 2,5, 20,50), 
                     units = "MgC/ha/any")

# ES2_AdultTreeBiomassSequestr --------------------------------------------
d_ES <- plot_ES_period(ES_period, ES2_AdultTreeBiomassSequestr, ylab = "Embornal de carboni arbres+fusta (Mg C/ha/any)", 
                outlier = c(-50,50), ylim = c(-1,8), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES2_AdultTreeBiomassSequestr.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period$ES2_AdultTreeBiomassSequestr)
map_scenario_periods(ES_period, var = "ES2_AdultTreeBiomassSequestr",
                     breaks = c(-10,-0.5, 0.5,1,2,5,10,20,50), 
                     breaks_diff = c(-50, -20, -5, -2, -1,-0.5, 0.5, 1, 2,5, 20,50), 
                     units = "MgC/ha/any")

# ES2_LiveBiomassSequestr --------------------------------------------
d_ES <- plot_ES_period(ES_period, ES2_LiveBiomassSequestr, ylab = "Embornal de carboni total (Mg C/ha/any)", 
                       outliers = c(-50,50), ylim = c(-1,6), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES2_LiveBiomassSequestr.png",d_ES, width = 10, height = 4, bg = "white")
summary(ES_period$ES2_LiveBiomassSequestr)
map_scenario_periods(ES_period, var = "ES2_LiveBiomassSequestr",
                     breaks = c(-10,-0.5, 0.5,1,2,5,10,20,50), 
                     breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20), 
                     units = "MgC/ha/any", draw_formes = FALSE)

# ES3_LAI -------------------------------------------------------
d_ES <- plot_ES_period(ES_period, ES3_LAI, ylab = "LAI", ylim = c(2,4.5), add_formes = T)
ggsave2("Plots/ES_dynamics/ES3_LAI.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period$ES3_LAI)
map_scenario_periods(ES_period, var = "ES3_LAI", 
                     breaks = seq(0,4.5, by=0.5), 
                     breaks_diff = c(-2,-1.5,-1.0,-0.5,-0.25,0.25,0.5,1.0,1.5,2), 
                     units = "m2/m2", draw_formes = TRUE)

# ES3_BlueWater -----------------------------------------------------------
# d_ES <- plot_ES_period(ES_period, ES3_Precipitation, ylab = "Precipitacio (mm/any)", ylim = c(0,1200), add_formes = T)
d_ES <- plot_ES_period(ES_period, ES3_BlueWater, ylab = "Aigua blava (mm/any)", ylim = c(80,300), add_formes = T)
ggsave2("Plots/ES_dynamics/ES3_BlueWater.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period$ES3_BlueWater)
map_scenario_periods(ES_period, var = "ES3_BlueWater", 
                     breaks = seq(0,320, by=40), 
                     breaks_diff = seq(-200,200, by = 40), 
                     units = "mm/any", draw_formes = TRUE)

# ES3_RunoffCoefficient ---------------------------------------------------
d_ES <- plot_ES_period(ES_period, ES3_RunoffCoefficient, ylab = "Coeficient d'escolament [%]", ylim = c(10,40), add_formes = T)
ggsave2("Plots/ES_dynamics/ES3_RunoffCoefficient.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period$ES3_RunoffCoefficient)
map_scenario_periods(ES_period, var = "ES3_RunoffCoefficient", 
                     breaks = c(0,5,10,20,40,60,80, 100), 
                     breaks_diff = c(-50,-10,-20,-5, -2,2,5,10,20,50), 
                     units = "%", draw_formes = TRUE)

# ES4_ErosionMitigation ---------------------------------------------------
d_ES <- plot_ES_period(ES_period, ES4_ErosionMitigation, ylab = "Mitigació de l'erosió (Mg/ha/any)", ylim = c(100,175), 
                outlier = 3000, add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES4_ErosionMitigation.png",d_ES, width = 10, height = 4, bg = "white")
summary(ES_period$ES4_ErosionMitigation)
map_scenario_periods(ES_period, var = "ES4_ErosionMitigation", 
                     breaks = c(0,25, 50, 100, 150, 200, 300, 1000, 4000), 
                     breaks_diff = c(-100, -50,-20, -10,-5, 5,10,20, 50, 100), 
                     units = "Mg/ha/any", draw_formes = FALSE)


# ES5_RecreationalValue ---------------------------------------------------
d_ES <- plot_ES_state(ES_state, ES5_RecreationalValue, ylab = "Valor recreatiu [0-1]", ylim = c(0.0,1), add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES5_RecreationalValue.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_state$ES5_RecreationalValue)
map_scenario_states(ES_state, var = "ES5_RecreationalValue", 
                     breaks = c(0.1,0.2,0.4,0.5,0.6,0.7,0.8, 0.9, 1), 
                    breaks_diff = c(-0.5,-0.3, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.3, 0.5),
                    units = "[0-1]", draw_formes = TRUE)


# ES6_SurfaceFirePotential  ---------------------------------------------------
d_ES <- plot_ES_period(ES_period, ES6_SurfaceFirePotential, ylab = "Risk d'incendi de superficie [0-9]", ylim = c(5,9), 
                       add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES6_SurfaceFirePotential.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period$ES6_SurfaceFirePotential)
map_scenario_periods(ES_period, var = "ES6_SurfaceFirePotential", 
                    breaks = seq(0,9, by=1), 
                    breaks_diff = c(-5,-4,-3,-2,-1,1,2,3,4,5), 
                    units = "[0-1]", draw_formes = TRUE)

# ES6_CrownFirePotential  ---------------------------------------------------
d_ES <- plot_ES_period(ES_period, ES6_CrownFirePotential, ylab = "Risk d'incendi de capçada [0-9]", ylim = c(2,7), 
                       add_formes = TRUE)
ggsave2("Plots/ES_dynamics/ES6_CrownFirePotential.png",d_ES, width = 10, height = 8, bg = "white")
summary(ES_period$ES6_CrownFirePotential)
map_scenario_periods(ES_period, var = "ES6_CrownFirePotential", 
                     breaks = seq(0,9, by=1), 
                     breaks_diff = c(-5,-4,-3,-2,-1,1,2,3,4,5), 
                     units = "[0-1]", draw_formes = TRUE)


