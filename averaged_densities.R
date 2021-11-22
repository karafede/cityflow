


library(readr)
library(sp)
library(raster)
library(gstat)
library(rgdal)
library(RNetCDF)
library(ncdf4)
library(stringr)
library(htmlwidgets)
library(webshot)
library(leaflet)
library(rgeos)
library(ggvoronoi)
library(dplyr)
library(deldir)
library(gridGraphics)
library(sf)
library(sp)
library(raster)
library(gstat)
library(lubridate)
library(scales)
library(viridis)  # better colors for everyone
library(ggthemes) # theme_map()
library(inlmisc)
library(tidyr)
# install.packages("NISTunits", dependencies = TRUE)
library(NISTunits)

rm(list = ls())

setwd("D:/Federico/CityFLows/output_files/")
folder_trajectories <- "D:/Federico/CityFLows/output_files/trajectories/"

name_camera <- "duca_aosta"
# name_camera <- "lato_taxi"
# name_camera <- "lato_biciclette"


if (name_camera == "duca_aosta") {
  ### ----> Piazza Duca d'Aosta
  # vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_densities_duca_aosta_a_new.csv")
  vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_densities_duca_aosta_17h20_to_18h20_10Oct2021.csv")
  # max_dens <- 0.32
}


if (name_camera == "lato_taxi") {
vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_densities_lato_taxi_a.csv")
# max_dens <- 0.21
}

if (name_camera == "lato_biciclette") {
vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_densities_lato_biciclette_a.csv")
}

vec_all <- vec_all %>%
  filter(Ds < 5,
         density_vI < 5,
         density_v < 5)


## remove rows with NAs
vec_all <- vec_all[complete.cases(vec_all), ]
vec_all <- vec_all %>%
  dplyr::select(-timedate)
vec_all <- vec_all %>% 
  filter_all(all_vars(!is.infinite(.)))


####--setup customized borders to make plos of the same size --####
xmin = min(vec_all$X)
ymax = max(vec_all$Y)
ymin = min(vec_all$Y)
xmax = max(vec_all$X)


# if (name_camera == "duca_aosta") {
#   vec_all <- vec_all %>%
#     filter(Y <= 80,
#            X >= 22)
#   xmin = min(vec_all$X)
#   ymax = max(vec_all$Y)
# }


### save files for ANDREEA
# write.csv(vec_all, file = paste0("D:/Federico/CityFLows/output_files/all_ID_densities_", name_camera, "_AD.csv"), row.names=FALSE)


DF_sf <- st_as_sf(x = vec_all,                         
                  coords = c("X", "Y")) %>%
  st_set_crs(32632)  

extent = st_sfc(st_polygon(list(rbind(c(xmin,ymin), c(xmax,ymin), c(xmax,ymax),
                                      c(xmin, ymax), c(xmin,ymin)))))

## set CRS
extent <- extent %>% 
  st_set_crs(st_crs(DF_sf))


## define a regular grid of 2 meter resolution and add an Id to each cell
GRID <- st_make_grid(extent, cellsize = 2, square = T) %>% 
  st_as_sf() %>%
  mutate(cell_ID = row_number())

GRID_density <- st_intersection(DF_sf, GRID) 

GRID_density <- as.data.frame(GRID_density[, c("Ds", "density_v", "density_vI", "cell_ID")])
GRID_density <- as.data.frame(GRID_density[, c("cell_ID", "Ds", "density_v", "density_vI")])
## join SPEED values to the GRID
GRID_A <- GRID %>%
  left_join(GRID_density, by = c("cell_ID"))
GRID_A <- GRID_A %>%
  group_by(cell_ID) %>%
  summarise(mean_Ds = mean(Ds, na.rm = T),
            mean_density_v = mean(density_v, na.rm = T),
            mean_density_vI = mean(density_vI, na.rm = T))


# GRID_A <- GRID_A %>%
#   filter(mean_Ds < 0.8)

######################################
###-- Mean Standard Density ---- #####

  plot_mean_density_Ds <- (GRID_A[, c("x", "mean_Ds")]) %>% 
    st_transform("+proj=omerc  +lat_0=0 +lonc=0 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=0") %>%
    ggplot(aes()) +
    theme_bw() +
    geom_sf(aes(fill = mean_Ds)) + 
    scale_fill_gradient2(low = "blue", mid = "white", high = "red4", midpoint = .005, trans = "sqrt",
                          name =expression( paste("[#/", m^2, "]")), na.value = 'white') +    #limits=c(0,0.5)   # limits=c(0, max_dens)
    # scale_fill_gradient2(low = "grey", mid = "white", high = "blue", name = "[m/s]") +
    coord_sf(datum = st_crs(32632))  
plot_mean_density_Ds
  ## save plot
  dpi = 96
  ggsave(filename=paste0(folder_trajectories, "Standard_Density_mean_", name_camera, ".png"),
         width = 700 / dpi, height = 650 / dpi,
         dpi = dpi)
  # ggsave(filename=paste0(folder_trajectories, "Standard_Density_mean_17h20_18h20_10Oct2021_", name_camera, ".png"),
  #        width = 700 / dpi, height = 650 / dpi,
  #        dpi = dpi)


  
######################################
###-- Mean Voronoi Density ---- ######
  
  # plot_mean_density_v <- (GRID_A[, c("x", "mean_density_v")]) %>% 
  #   st_transform("+proj=omerc  +lat_0=0 +lonc=0 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=0") %>%
  #   ggplot(aes()) +
  #   theme_bw() +
  #   geom_sf(aes(fill = mean_density_v)) + 
  #   scale_fill_gradient2(low = "blue", mid = "white", high = "red4", midpoint = .005, trans = "sqrt",
  #                        name =expression( paste("[#/", m^2, "]")), na.value = 'white' ) +    #limits=c(0,0.5)
  #   # scale_fill_gradient2(low = "grey", mid = "white", high = "blue", name = "[m/s]") +
  #   coord_sf(datum = st_crs(32632))  
  # plot_mean_density_v
  # ## save plot
  # dpi = 96
  # ggsave(filename=paste0(folder_trajectories, "Density_v_mean_", name_camera, ".png"),
  #        width = 700 / dpi, height = 650 / dpi,
  #        dpi = dpi)

  
  
  
  ########################################
  ###-- Mean Voronoi_I Density ---- ######
  
  
# GRID_A_voronoi <- GRID_A %>%
#   filter(mean_density_vI <= max_density)
  
  plot_mean_density_v <- (GRID_A[, c("x", "mean_density_vI")]) %>% 
    st_transform("+proj=omerc  +lat_0=0 +lonc=0 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=0") %>%
    ggplot(aes()) +
    theme_bw() +
    geom_sf(aes(fill = mean_density_vI)) + 
    scale_fill_gradient2(low = "blue", mid = "white", high = "red4", midpoint = .005, trans = "sqrt",
                         name =expression( paste("[#/", m^2, "]")), na.value = 'white') +    # limits=c(0, max_dens)
    # scale_fill_gradient2(low = "grey", mid = "white", high = "blue", name = "[m/s]") +
    coord_sf(datum = st_crs(32632))  
  plot_mean_density_v
  ## save plot
  dpi = 96
  ggsave(filename=paste0(folder_trajectories, "Density_vI_mean_", name_camera, ".png"),
         width = 700 / dpi, height = 650 / dpi,
         dpi = dpi)
  # ggsave(filename=paste0(folder_trajectories, "Density_vI_mean_17h20_18h20_10Oct2021_", name_camera, ".png"),
  #        width = 700 / dpi, height = 650 / dpi,
  #        dpi = dpi)


