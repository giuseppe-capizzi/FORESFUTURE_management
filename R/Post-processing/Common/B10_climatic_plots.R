# Create plots for climatic series 
library(dplyr)
library(ggplot2)
library(cowplot)

# Retrieve and organize data -----
# HISTORIC
decade_2001_2010 <- readRDS("../FORMESdata/Rdata/Interpolated_climate_historic/decade/decade_2001_2010.rds")
decade_2011_2020 <- readRDS("../FORMESdata/Rdata/Interpolated_climate_historic/decade/decade_2011_2020.rds")

dec_1 <- decade_2001_2010 %>% select(ID, MeanTemperature, Precipitation, PET) %>%
  mutate(decade = "2000-2010")
dec_2 <- decade_2011_2020 %>% select(ID, MeanTemperature, Precipitation, PET) %>%
  mutate(decade = "2010-2020")

hist_dec <- rbind(dec_1,dec_2)

# PROJECTIONS
decades_vector <- c("2021_2030",
                    "2031_2040",
                    "2041_2050",
                    "2051_2060",
                    "2061_2070",
                    "2071_2080",
                    "2081_2090",
                    "2091_2100")


pattern <- paste(decades_vector, collapse = "|")

# List files that match the pattern
files_45 <- list.files(path = "../FORMESdata/Rdata/Interpolated_climate_projections/decade/", pattern = paste0("rcp_45_(",pattern,")"), full.names = T)
files_85 <- list.files(path = "../FORMESdata/Rdata/Interpolated_climate_projections/decade/", pattern = paste0("rcp_85_(",pattern,")"), full.names = T)

data_clim_45 <- hist_dec

for(f in files_45){
  # browser()
  clim <- readRDS(f)
  clim <- clim %>% select(ID, MeanTemperature, Precipitation, PET) %>%
    mutate(decade = gsub("_", "-",substr(f, 80,88)))
  data_clim_45 <- rbind(data_clim_45,clim)
  
  }


data_clim_85 <- hist_dec
for(f in files_85){
  clim <- readRDS(f)
  clim <- clim %>% select(ID, MeanTemperature, Precipitation, PET) %>%
    mutate(decade = gsub("_", "-",substr(f, 80,88)))
  data_clim_85 <- rbind(data_clim_85,clim)
  
}

saveRDS(data_clim_45, "Rdata/climate_45.rds")
saveRDS(data_clim_85, "Rdata/climate_85.rds")
rm(list = ls())


# Generate plots -------

climate_45 <- readRDS("Rdata/climate_45.rds")
climate_85 <- readRDS("Rdata/climate_85.rds")

means_45 <- climate_45 %>%
  group_by(decade) %>%
  summarize(Mean_temp = mean(MeanTemperature),
            Mean_prec = mean(Precipitation),
            Mean_PET = mean(PET),
            ES_se_temp = sd(MeanTemperature)/sqrt(sum(!is.na(MeanTemperature))),
            ES_se_prec = sd(Precipitation)/sqrt(sum(!is.na(Precipitation))),
            ES_se_PET = sd(PET)/sqrt(sum(!is.na(PET)))) %>%
  mutate(clim_scn = "RCP 4.5")


sd(climate_45[climate_45$decade == "2000-2010",]$Precipitation)/
sqrt(sum(!is.na(climate_45[climate_45$decade == "2000-2010",]$Precipitation)))

means_85 <- climate_85 %>%
  group_by(decade) %>%
  summarize(Mean_temp = mean(MeanTemperature),
            Mean_prec = mean(Precipitation),
            Mean_PET = mean(PET),
            ES_se_temp = sd(MeanTemperature)/sqrt(sum(!is.na(MeanTemperature))),
            ES_se_prec = sd(Precipitation)/sqrt(sum(!is.na(Precipitation))),
            ES_se_PET = sd(PET)/sqrt(sum(!is.na(PET)))) %>%
  mutate(clim_scn = "RCP 8.5")


means_ALL <- bind_rows(means_45, means_85)


# Temperature
plot_temp<- ggplot(means_ALL)+
  geom_point(aes(x = decade, y = Mean_temp, col = clim_scn), size = 3)+
  geom_ribbon(aes(x = decade, ymin = Mean_temp - ES_se_temp*1.96, ymax = Mean_temp + ES_se_temp*1.96, fill = clim_scn, group = clim_scn), alpha = 0.3)+
  geom_line(aes(x = decade, y = Mean_temp, group = clim_scn, col = clim_scn), linewidth=1.5) + 
  scale_fill_brewer("Escenari climatic",palette = "Set2")+
  scale_color_brewer("Escenari climatic", palette = "Set2")+
  theme_bw() +
  ylim(13,18.5) + 
  ylab("Temperatura Mitjana (°C)") + xlab("Dècada") + 
  labs(title = "") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=16),
        legend.title = element_text(size=20),
        legend.key.size = unit(0.6, 'cm'))

plot_temp

ggsave2("Plots/climatic_series/Temperature.png",plot_temp, width = 12, height = 8, bg = "white")

  
# Precipitation
plot_prec <- ggplot(means_ALL)+
  geom_point(aes(x = decade, y = Mean_prec, col = clim_scn), size = 3)+
  geom_ribbon(aes(x = decade, ymin = Mean_prec - ES_se_prec*1.96, ymax = Mean_prec + ES_se_prec*1.96, fill = clim_scn, group = clim_scn), alpha = 0.3)+
  geom_line(aes(x = decade, y = Mean_prec, group = clim_scn, col = clim_scn), linewidth=1.5) + 
  scale_fill_brewer("Escenari climatic",palette = "Set2")+
  scale_color_brewer("Escenari climatic", palette = "Set2")+
  theme_bw() +
  ylim(600,825) +
  ylab("Precipitació anual (mm/any)") + xlab("Dècada") + 
  labs(title = "") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=16),
        legend.title = element_text(size=20),
        legend.key.size = unit(0.6, 'cm'))
plot_prec

ggsave2("Plots/climatic_series/Precipitation.png",plot_prec, width = 12, height = 8, bg = "white")


# PET
plot_PET <- ggplot(means_ALL)+
  geom_point(aes(x = decade, y = Mean_PET, col = clim_scn))+
  geom_ribbon(aes(x = decade, ymin = Mean_PET - ES_se_PET*1.96, ymax = Mean_PET + ES_se_PET*1.96, fill = clim_scn, group = clim_scn), alpha = 0.3)+
  geom_line(aes(x = decade, y = Mean_PET, group = clim_scn, col = clim_scn)) + 
  scale_fill_brewer("Escenari climatic",palette = "Set2")+
  scale_color_brewer("Escenari climatic", palette = "Set2")+
  theme_bw() +
  ylab("PET") + xlab("Dècada") + 
  labs(title = "") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        legend.key.size = unit(0.6, 'cm'))
plot_PET

ggsave2("Plots/climatic_series/PET.png",plot_PET, width = 10, height = 8, bg = "white")



