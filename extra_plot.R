
source("leaflet.R", local = TRUE)

# other libraries

library(tidyverse)
library(ggplot2)
library(cmocean)
library(ggthemes)
library(cowplot)

#---- load tifs ---- 

source("C:/Users/lenovo/Documents/GitHub/sustainability_dashboard/other_files/helper_functions.R", local = TRUE)

df <- readRDS("bin_data.rds")
breaks <- c(0, 0.05, 0.1, 0.3, 0.5, 1)
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

growth_cols <- c("bin_growth_ratecal_rcp2p6",
                 "bin_growth_ratecal_rcp6p0",
                 "bin_growth_ratecal_rcp8p5")
#---- prepare plot ----
plotter <- function(df, target_col){
  if(grepl("growth", target_col)){
    df[[target_col]] <- round(df[[target_col]], 3)
  } else {
    df[[target_col]] <- round(df[[target_col]])}
  sort_values <- sort(unique(df[[target_col]]))
  my_color <- cmocean(name = 'curl', direction = -1, clip = 0.1)(11)
  names(my_color) <- as.character(sort_values)
  
  
  out_plot <- ggplot(df, aes(get("x"), get("y"))) +
    geom_tile(aes(fill =as.character(get(target_col)))) +
    xlim(-180, 180) +
    ylim(-90, 90) +
    coord_equal() + # maybe I need but idk
    
    scale_fill_manual(values = my_color, name = "Bin Mean",
                      labels = sort_values)  +
    theme_map() +
    theme(legend.direction = 'vertical',
          legend.position = c(.05, .25),
          legend.spacing.y = unit(0, "pt"),
          legend.title = element_text(size = 9, color = "#4e4d47"),
          legend.text = element_text(size = 7, color = "#4e4d47"),
          plot.caption = element_text(size = 6, 
                                      hjust = 0.92, 
                                      margin = margin(t = 0.2, 
                                                      b = 0, 
                                                      unit = "cm"), 
                                      color = "#939184"),
          plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
          plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47"),
          panel.background = element_rect(fill = "white")) +
    labs(x = NULL, 
         y = NULL, 
         caption = "Author: Bjoern Komander. More information ... ") 
  
  
  
  #ggsave(paste0(target_col, ".png"), out_plot, dpi = 300)
  save_plot(paste0(target_col, ".png"), out_plot, base_height = 7,dpi = "print")

}



