library(ggplot2)
library(cowplot)
res_opt <- data.frame()
for(block in 1:11) {
  res_opt_i <- read_delim(paste0("res_opt_", block,".csv"), delim = "\t", 
                      escape_double = FALSE, trim_ws = TRUE)
  res_opt <- rbind(res_opt, res_opt_i)
}

ifn3_climate <- sf::read_sf("Rdata/ifn3_cat_climate_1976_2020.gpkg")

res_all <- res_opt |> 
  dplyr::left_join(forestfuture_lq_sf[,c("id","dominant_tree_species")], by="id") |>
  dplyr::left_join(ifn3_climate, by=c("id" = "ID")) |>
  dplyr::mutate(MI = pmin(2.0, MAP/MAPET), MIsummer = MAPsummer/MAPETsummer)

g1<-ggplot2::ggplot(res_all, aes(x=lai, y=sew_fin))+ 
  geom_jitter(aes(col=MI), height = 0, width = 0.1, size = 0.5)+
  geom_smooth(col="black")+
  scale_color_gradient2(low = "black", mid="yellow", high="blue", midpoint = 1)+
  ylim(c(0,850))+ xlim(0,8)+
  xlab("LAI")+ ylab("TAW (mm)")+
  theme_bw()
# g2<-ggplot2::ggplot(res_all, aes(x=lai, y=sew_fin))+ 
#   geom_point(aes(col=dominant_tree_species))+
#   geom_smooth(col="black")+
#   ylim(c(0,850))
tds <- table(res_all$dominant_tree_species)
spp <- names(tds)[tds>25]
res_all$dominant_tree_species[is.na(res_all$dominant_tree_species)] <- "Other spp."
res_all$dominant_tree_species[!(res_all$dominant_tree_species %in% spp)] <- "Other spp."
g2 <- ggplot2::ggplot(res_all, aes(x =dominant_tree_species, y=sew_fin))+
  geom_violin(fill="light blue", col="light blue")+
  scale_x_discrete(limits = rev)+
  coord_flip()+
  geom_jitter(height = 0, width = 0.1, alpha = 0.5, cex = 0.1)+
  # geom_jitter(aes(col=MI), height = 0, width = 0.1, alpha = 0.5, cex = 0.1)+
  # scale_color_gradient2(low = "black", mid="yellow", high="blue", midpoint = 1)+
  ylab("TAW (mm)")+xlab("Dominant species")+
  theme_bw()
# g2.3<-ggplot2::ggplot(res_all, aes(x=sew_ini, y=sew_fin))+
#   geom_point()+
#   geom_abline(slope=1, intercept=0)
plot_grid(g2,g1, nrow = 1)
