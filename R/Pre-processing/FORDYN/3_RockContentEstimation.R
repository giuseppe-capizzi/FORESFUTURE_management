### 
# TRY TO ESTIMATE ROCK FRAGMENT CONTENT
###
library(medfate)
library(medfateland)
library(meteoland)
library(sf)

block = 11

forestfuture_lq_sf <- readRDS("Rdata/forestfuture_lq_sf.rds")

climate_base = "emf/datasets/Climate/"


cli::cli_li(paste0("Block ", block))

i_ini <- (block-1)*1000 + 1
i_fin <- min(nrow(forestfuture_lq_sf), block*1000)
sf_init <- forestfuture_lq_sf[i_ini:i_fin,]
nplots <- nrow(sf_init)

cli::cli_li(paste0("Target plots: ", nplots))

# Get latitude
longlat = sf::st_transform(sf::st_geometry(sf_init), crs = 4326)
lat <- sf::st_coordinates(longlat)[,2]

years <- 2001:2020
cli::cli_li(paste0("Loading interpolators for ", length(years), " years"))
interpolators <- vector("list", length(years))
for(iy in 1:length(years)) {
  interpolator_file <- EMFdatautils::download_emfdata(climate_base, 
                                                      paste0("Products/InterpolationData/Catalunya/Historic/calibrated_2.0/interpolator_", years[iy],"_calibrated.nc"))
  interpolators[[iy]] <- meteoland::read_interpolator(interpolator_file)
  file.remove(interpolator_file)
}


cli::cli_li("Processing SEW estimation")
res_df <- data.frame()
for(ip in 1:nplots) {
  id_i <- sf_init$id[ip]
  cat(paste0(ip, " ", id_i))
  forest_i <- medfate::forest_mergeTrees(sf_init$forest[[ip]])
  soil_i <- sf_init$soil[[ip]]
  lai_i <- medfate::stand_LAI(forest_i, SpParamsMED)
  ba_i <- medfate::stand_basalArea(forest_i)
  sew_ini_i <- sum(soil_waterExtractable(soil(soil_i), "VG"))
  cat(paste0(" LAI: ", round(lai_i,2)))
  cat(paste0(" BA: ", round(ba_i,2)))
  cat(paste0(" initial SEW: ", round(sew_ini_i,2)))
  x <- medfate::forest2spwbInput(forest_i, soil(soil_i), SpParamsMED, defaultControl())
  met_df <- data.frame()
  cat(" [weather] ")
  for(iy in 1:length(years)) {
    met_iy <- interpolate_data(sf_init[ip, ],interpolators[[iy]], verbose= FALSE)
    met_df_iy <- as.data.frame(met_iy$interpolated_data[[1]])
    row.names(met_df_iy) <- as.character(met_df_iy$dates)
    met_df_iy$dates <- NULL
    met_df <- rbind(met_df, met_df_iy)
  }
  cat(" [opt] ")
  
  rfc_final_i <- rep(NA, 4)
  sew_fin_i <- NA
  tryCatch({
    res <- medfateutils::spwb_rockOptimization(x, met_df, latitude = lat[ip], 
                                               elevation = sf_init$elevation[ip],
                                               slope = sf_init$slope[ip],
                                               aspect = sf_init$aspect[ip])
    rfc_final_i <- res$RFC
    soil_i$rfc <- rfc_final_i
    sew_fin_i <- sum(soil_waterExtractable(soil(soil_i), "VG"))
    cat(paste0(" final SEW:  ", round(sew_fin_i,2),  "\n"))
  }, error=function(cond) {cat(" error\n")})
  res_df <- rbind(res_df,
                  data.frame(id = id_i,
                             lai = lai_i,
                             ba = ba_i,
                             sew_ini = sew_ini_i,
                             sew_fin = sew_fin_i,
                             rfc_1 = rfc_final_i[1],
                             rfc_2 = rfc_final_i[2],
                             rfc_3 = rfc_final_i[3],
                             rfc_4 = rfc_final_i[4]))
  if((ip%%5==0) || (ip==nplots)) {
    write.table(res_df,file = paste0("res_opt_", block, ".csv"), sep="\t", row.names=FALSE)
  }
}
