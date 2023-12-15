# --------------- Data for PRV ---------------
# library(tidyverse)
# library(medfate)
# library(medfateland)
# library(Metrics)
# library(raster)
# library(sf)
# library(gridExtra)


# # --------------- 1a. Set-up NFI4 data - using Pipo's file of plot sampled in the three NFI
# goodid <- read.delim("data/databaseForESTime.txt")
# IDPIPO <- goodid$Plot
# length(IDPIPO) #OK, 3417
# length(goodid[,1])

# 
# # -- section i - select the 3417 plot from NFI4
# nfi4 <- readRDS("data/forestlist_roots_IFN4_Catalunya.rds")
# nfi4_3417id <- nfi4[as.character(IDPIPO)]
# nfi4_3417id <- nfi4_3417id[order(as.numeric(names(nfi4_3417id)))] #Sort
# any(sapply(nfi4_3417id$plot_ID, is.null)) #No null
# any(sapply(nfi4_3417id, is.null)) #No null
# length(nfi4_3417id) #Check length
# 
# nfi4_xxx <- nfi4_3417id


# --------------- 1b. Set-up NFI4 data - using all available plots in NFI4
# I use the old forest obj. It is ok, just without the final edits by Miquel
nfi4 <- readRDS("data/forest_obj/old/forestlist_roots_IFN4_Catalunya.rds")
nfi4 <- nfi4[order(as.numeric(names(nfi4)))] #Sort
any(sapply(names(nfi4), is.null)) #No null
any(sapply(nfi4, is.null)) #No null
length(nfi4)


# --------------- 2. Loop with all info and save - Stand level
nfi4_xxx <- nfi4

nomi <- c("PlotID","Max_DBH_t","Max_Cover_s","Richness_t","Richness_ts",
          "CV_N_t","CV_DBH_t","CV_H_t","CV_Cover_s","CV_H_s","LAI_t","LAI_ts")
df_prv <- data.frame(matrix(nrow = length(nfi4_xxx), ncol = length(nomi)))
colnames(df_prv) <- nomi

for (i in 1:length(nfi4_xxx)) {
  
  aa <- nfi4_xxx[[i]]
  
  df_prv$PlotID[i] <- aa$ID
  
  df_prv$Max_DBH_t[i] <- max(aa$treeData$DBH)
  
  df_prv$Max_Cover_s[i] <- max(aa$shrubData$Cover)
  
  df_prv$Richness_t[i] <- length(unique(aa$treeData$Species))
  df_prv$Richness_ts[i] <- length(unique(aa$treeData$Species))+length(unique(aa$shrubData$Species))
  
  df_prv$CV_N_t[i] <- sd(aa$treeData$N)/mean(aa$treeData$N)
  df_prv$CV_DBH_t[i] <- sd(aa$treeData$DBH)/mean(aa$treeData$DBH)
  df_prv$CV_H_t[i] <- sd(aa$treeData$H)/mean(aa$treeData$H)
  df_prv$CV_Cover_s[i] <- sd(aa$shrubData$Cover)/mean(aa$shrubData$Cover)
  df_prv$CV_H_s[i] <- sd(aa$shrubData$Height)/mean(aa$shrubData$Height)
  
  dbh <- aa$treeData$DBH
  LAI_x <- plant_LAI(aa,SpParamsMED)
  LAI_x <- LAI_x[1:length(dbh)]
  df_prv$LAI_t[i] <- sum(LAI_x)
  df_prv$LAI_ts[i] <- sum(plant_LAI(aa,SpParamsMED))
  
}

df_prv <- replace(df_prv, df_prv==-Inf, NA)
saveRDS(df_prv, "data/PRV/PotRecrValue_table_stand.rds")
head(df_prv, n=10)


# --------------- 3. Range and histograms
df_prv <- readRDS("data/PRV/PotRecrValue_table_stand.rds")

range(df_prv$Max_DBH_t, na.rm = T)
hist(df_prv$Max_DBH_t)

range(df_prv$CV_DBH_t, na.rm = T)
hist(df_prv$CV_DBH_t)
max(df_prv$CV_DBH_t, na.rm = T)

hist(df_prv$LAI_t)
hist(df_prv$LAI_ts)
range(df_prv$LAI_ts)

hist(df_prv$CV_Cover_s)
hist(df_prv$Max_Cover_s)
range(df_prv$Max_Cover_s, na.rm = T)

range(df_prv$Richness_t, na.rm = T)
range(df_prv$Richness_ts, na.rm = T)

# --------------- 4. Loop with all info and save - Landscape level

# ----Section 0 - prepare the data
fo_nfi4_id <- names(nfi4) #get NFI4 plot id

topo_inf4 <- readRDS("data/topo/IFN4_spt_cat_ETRS89H31.rds") #adjust topo to get coordinates of NFI4
length(topo_inf4) #5502 (more than the 5441)

topo_inf4_df <- as.data.frame(topo_inf4) #get the topo to 5441
topo_inf4_df$plot_ID <- rownames(topo_inf4_df)
rownames(topo_inf4_df) <- NULL
topo_inf4_df <- topo_inf4_df[, c(6,4,5)] #just keep x and y
length(topo_inf4_df[,1])

topo_inf4_clean <- topo_inf4_df[topo_inf4_df$plot_ID %in% fo_nfi4_id,] #filter topo for the same plot ID of NFI4
length(topo_inf4_clean[,1]) #ok, 5441 plots
# setequal(fo_nfi4_id, topo_inf4_clean$plot_ID)
# setequal(tail(names(nfi4), n=60),tail(topo_inf4_clean$plot_ID, n=60))
# write.csv(topo_inf4_clean, "NFI4_5441_plotID.csv")

nfi4_clean <- nfi4[as.character(fo_nfi4_id)] #clean the NFI objects
# setequal(fo_nfi4_id, names(nfi4_clean))

pts_sf <- sp_to_sf(examplepointslandscape) #sf from GitHub for crs

# ----Section i - loop to get N. plot and forest object within 25km
nomi1 <- c("PlotID","N_Plot25km")
df_prv_1 <- data.frame(matrix(nrow = length(topo_inf4_clean[,1]), ncol = length(nomi1)))
colnames(df_prv_1) <- nomi1

forest_25km <- list()

for (i in 1:length(topo_inf4_clean[,1])) {
  
  bb <- topo_inf4_clean[i,]
  
  print(i)
  # print(bb$plot_ID)
  
  sdf_bb <- st_as_sf(bb, coords = c("x", "y"), crs=st_crs(pts_sf))
  sdf_all <- st_as_sf(topo_inf4_clean, coords = c("x", "y"), crs=st_crs(pts_sf))
  
  buffer <- st_buffer(sdf_bb,25000)
  intr <- as.data.frame(st_intersects(sdf_all, buffer, sparse=F))
  intr$Plot_ID <- topo_inf4_clean$plot_ID
  intr <- intr[intr$V1 == TRUE, ]
  # length(intr[,1])
  
  intr_id <- intr$Plot_ID #get ID of plot within 25km from bb
  fo_25km <- nfi4_clean[as.character(intr_id)] #select NFI within 25km
  # length(fo_25km)
  # setequal(names(fo_25km), intr_id)
  
  df_prv_1$N_Plot25km[i] <- length(intr_id)
  df_prv_1$PlotID[i] <- bb$plot_ID
  head(df_prv_1)
  
  forest_25km[[i]] <- fo_25km
  names(forest_25km)[i] <- bb$plot_ID
  
}

head(df_prv_1, n=20)
max(df_prv_1$N_Plot25km) #max plot within 25 km is 623
min(df_prv_1$N_Plot25km) #min is 11 (PlotID 254080)

length(forest_25km)
length(forest_25km[[4444]])
names(forest_25km[2548])

saveRDS(df_prv_1, "data/PRV/N_Plots_within_25km.rds")
saveRDS(forest_25km, "data/PRV/For_Obj_within_25km.rds")

# ----Section 2 - loop to get the landscape level info for PRV table
nomi <- c("PlotID","VBS_1","VBS_min_2","VBS_max_2","VBS_3","Uniq","Abs_Uniq")
df_prv_lnd <- data.frame(matrix(nrow = length(forest_25km), ncol = length(nomi)))
colnames(df_prv_lnd) <- nomi

# VBS_1:            CV of DBH of all plots within 25km, get the mean, and range at the end
# VBS_2 (min, max): CV of DBH of all plots within 25km, get the range, and mean (min, max) at the end
# VBS_3:            mean DBH of all plots within 25 km, get the CV, and get the range at the end (I used this)

for (i in 1:length(forest_25km)) {
  
  print(paste0("i = ", i))
  fo <- forest_25km[[i]]
  
  nomicvdbh <- c("cvdbh","meandbh")
  df_cvdbh <- data.frame(matrix(nrow = length(fo), ncol = length(nomicvdbh)))
  colnames(df_cvdbh) <- nomicvdbh
  
  for (j in 1:length(fo)) {
    
    # print(paste0("This is j = ", j))
    cc <- fo[[j]]
    df_cvdbh$cvdbh[j] <- sd(cc$treeData$DBH)/mean(cc$treeData$DBH)
    df_cvdbh$meandbh[j] <- mean(cc$treeData$DBH)
    
    head(df_cvdbh)
  }
  
  df_prv_lnd$PlotID[i] <- names(forest_25km[i])
  df_prv_lnd$VBS_1[i] <- mean(df_cvdbh$cvdbh, na.rm=T)
  df_prv_lnd$VBS_min_2[i] <- min(df_cvdbh$cvdbh, na.rm=T)
  df_prv_lnd$VBS_max_2[i] <- max(df_cvdbh$cvdbh, na.rm=T)
  df_prv_lnd$VBS_3[i] <- sd(df_cvdbh$meandbh, na.rm=T)/mean(df_cvdbh$meandbh, na.rm=T) #I used this
  df_prv_lnd$Uniq[i] <- (1 - (length(fo)/623))
  df_prv_lnd$Abs_Uniq[i] <- length(fo)/623
  
}

saveRDS(df_prv_lnd, "data/PRV/PotRecrValue_table_landscape.rds")

head(df_prv_lnd, n=20)
length(df_prv_lnd[,1])

range(df_prv_lnd$VBS_1)
hist(df_prv_lnd$VBS_1)

mean(df_prv_lnd$VBS_min_2)
mean(df_prv_lnd$VBS_max_2)

range(df_prv_lnd$VBS_3) #I used this
hist(df_prv_lnd$VBS_3)

range(df_prv_lnd$Uniq)
hist(df_prv_lnd$Uniq)

# --------------- 5. Look at the table (if needed)
prv_table <- readRDS("data/PRV/PotRecrValue_table_landscape.rds")
hist(prv_table$VBS_max)
hist(prv_table$VBS_min)