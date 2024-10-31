library(medfateland)

forestfuture_sf <- readRDS("Rdata/forestfuture_sf.rds")

IFN_base <- "emf/datasets/ForestInventories/IFN/"
Climate_base <- "emf/datasets/Climate/"

# Fetch rocosity
f <- EMFdatautils::download_emfdata(IFN_base, "Products/IFN3/plotDataIFN3_Catalunya.csv")
df<-read.csv2(f, sep = "\t")
df_rock <- df[, c("ID", "Rocosid")]
file.remove(f)
df_rock$Rocosid[is.na(df_rock$Rocosid)] <- 1 # Assume that missing rock content is 1

# Fetch annual precipitation
f <- EMFdatautils::download_emfdata(Climate_base, "Products/IFNplots/Catalunya/Historic/Climate/ifn3_cat_climate_1976_2020.gpkg")
ifn3_cat_climate<- sf::st_read(f)
ifn3_MAP <- sf::st_drop_geometry(ifn3_cat_climate)[,c("ID", "MAP")]
file.remove(f)

# Join data to sf
# Define low quality (MAP < 400 or Rock content = 4 o 5)
forestfuture_lq_sf  <- forestfuture_sf |> 
  dplyr::left_join(df_rock, by="ID") |>
  dplyr::left_join(ifn3_MAP, by="ID") |>
  dplyr::mutate(LowQuality = (Rocosid %in% c(4,5)) | (MAP < 400)) 

table(forestfuture_lq_sf$LowQuality)

# Store RDS
saveRDS(forestfuture_lq_sf, "Rdata/forestfuture_lq_sf.rds")

