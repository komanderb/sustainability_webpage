##################################################
## Project: Webpage Sustainability
## Script purpose: Prepare the data
## Date: 10.01.2023
## Author: Bjoern Komander
##################################################

library(tidyverse)
library(ggthemes)
library(cmocean)
library(cowplot)
library(raster)


# read in Data ---

source("C:/Users/lenovo/Documents/GitHub/sustainability_dashboard/other_files/helper_functions.R", local = TRUE)

df <- readRDS("C:/Users/lenovo/Documents/GitHub/sustainability_dashboard/dashboard_leaflet/all_rcp.rds")
# drop rcp4p5
df <- df[-c(grep("rcp4p5", names(df)))]

# make Absolute bins ----
breaks <- c(0, 0.05, 0.1, 0.3, 0.5, 1)
# we need to modify changecal_rcp2p6
df$changecal_rcp2p6 <- df$changecal_rcp2p6 *-1
source_col <- c("changecal_rcp2p6", "changecal_rcp6p0", "changecal_rcp8p5")
target_col <- c("bin_change_rcp2p6", "bin_change_rcp6p0", "bin_change_rcp8p5")

for (i in seq_along(source_col)){
  df[[target_col[i]]] <- new_bins(df[[source_col[i]]], breaks)
}

df$bin_change_rcp2p6 <- df$bin_change_rcp2p6 *-1
# relative change ----
growth_cols <- names(df)[grep("growth_rate", names(df))]

for (i in seq_along(growth_cols)){
  # remove outliers and infinity
  col = growth_cols[i]
  df[[col]][is.infinite(df[[col]])] <- 0
  df[[col]][df[[col]] > 1000] <- NA
  bin_name <- paste0("bin_", col)
  df[[bin_name]] <- 0
  
  pos_bool <- df[[col]] > 0 & !is.na(df[[col]])
  neg_bool <- df[[col]] < 0 & !is.na(df[[col]])
  
  df[[bin_name]][pos_bool] <- bin_maker(df[[col]][pos_bool], breaks)
  df[[bin_name]][neg_bool] <- bin_maker(df[[col]][neg_bool], breaks)
}

# creating raster files for leaflet ----
# assign new target cols 
target_cols <- names(df)[grep("bin", names(df))]
# there is one problem -> rcp6p0 is negatively skewed but max values are positive
# assign the highest bin (its only 5 or so values)
df$bin_change_rcp6p0[is.na(df$bin_change_rcp6p0)] <- 19099.02

# make empty raster
r <- raster(ncol = 4320, nrow = 2160,
            xmn= -180, xmx= 180, ymn = -90, ymx = 90,
            crs = "EPSG:4326")


for (i in seq_along(target_cols)){
  x <- rasterize(df[, 1:2], r, df[[target_cols[i]]])
  if (grepl("growth", target_cols[i] )){
    values(x) <- round(values(x), 3)
  }
  else{
    values(x) <- round(values(x))
  }
  x <- setMinMax(x)
  x <- projectRaster(x, crs = CRS('+init=EPSG:3857'),method = 'ngb')
  x <- setMinMax(x)
  file_name <- paste0(target_cols[i], ".tif")
  writeRaster(x, file_name,options=c('TFW=YES'))
}

# some weird stuff happens
# also save the RDs with the bins 
saveRDS(df, file = "bin_data.rds")

# for the plots where the distribution is across all 
all_change <- c(df$changecal_rcp2p6, df$changecal_rcp6p0, df$changecal_rcp8p5)
all_growth <- c(df$growth_ratecal_rcp2p6, df$growth_ratecal_rcp6p0, df$growth_ratecal_rcp8p5)

df_all <- as.data.frame(cbind(all_change, all_growth))

df_all$bin_change <- new_bins(df_all$all_change, breaks)

# assign back to df
df$compare_rcp2p6 <- df_all$bin_change[1:2277587]
df$compare_rcp6p0 <- df_all$bin_change[2277588: 4555174]
df$compare_rcp8p5 <- df_all$bin_change[4555175:6832761]

target_cols <- c("compare_rcp2p6", "compare_rcp6p0", "compare_rcp8p5")

for (i in seq_along(target_cols)){
  x <- rasterize(df[, 1:2], r, df[[target_cols[i]]])
  values(x) <- round(values(x))
  x <- setMinMax(x)
  x <- projectRaster(x, crs = CRS('+init=EPSG:3857'),method = 'ngb')
  x <- setMinMax(x)
  file_name <- paste0(target_cols[i], ".tif")
  writeRaster(x, file_name,options=c('TFW=YES'))
}
