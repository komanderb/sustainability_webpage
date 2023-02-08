# check leaflet again
library(rgdal)
library(raster)
library(cmocean)
library(leaflet)

absolute_tifs <- c("bin_change_rcp2p6.tif", "bin_change_rcp6p0.tif", 
                   "bin_change_rcp8p5.tif" )
relative_tifs <- c("bin_growth_ratecal_rcp2p6.tif", 
                   "bin_growth_ratecal_rcp6p0.tif",
                   "bin_growth_ratecal_rcp8p5.tif")

compare_tifs <- c("compare_rcp2p6.tif", "compare_rcp6p0.tif", 
                  "compare_rcp8p5.tif")

leaflet_function <- function(file_name, title){
  x <- raster(file_name)
  if(grepl("growth", file_name)){
    values(x) <- round(values(x), 3)
  }
  pal = colorFactor(cmocean(name = 'curl', direction = -1, clip = 0.1)(11), sort(unique(values(x))),
                    ordered = TRUE, 
                    na.color = "transparent")
  out_plot <- leaflet() %>% 
    addRasterImage(x = x , 
                   colors = pal, 
                   opacity = 1,
                   project = FALSE) %>%
    addLegend(pal = pal, values = values(x),
              title = title) %>%
    setView(lng = 0, lat = 10, zoom = 1.5)
  
  return(out_plot)
}




