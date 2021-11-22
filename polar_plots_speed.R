

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

name_camera <- "duca_aosta"
# name_camera <- "lato_taxi"
# name_camera <- "lato_biciclette"


# time_of_day <- "morning"   ## from 06:00 to 10:00
time_of_day <- "evening"  ## from 17:00 to 19:00


setwd("D:/Federico/CityFLows/output_files/")
folder_trajectories <- "D:/Federico/CityFLows/output_files/trajectories/"

## set forward motion speed
speed_FWD_motion <- 0.3  ## units [m/s]


if (name_camera == "duca_aosta") {
  # vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles.csv")
  # vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_4Nov_50ms.csv")
  vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_lato_taxi_angles__17h20_to_18h20_10Oct2021.csv")
}


if (name_camera == "lato_taxi") {
  vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_lato_taxi_angles.csv")
  # vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_lato_taxi_angles_4Nov_50ms.csv")
}


if (name_camera == "lato_biciclette") {
  vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_lato_biciclette_angles.csv")
}



## remove rows with NAs
vec_all <- vec_all[complete.cases(vec_all), ] 
vec_all <- vec_all %>% 
  filter_all(all_vars(!is.infinite(.)))


# if (name_camera == "duca_aosta") {
#   vec_all <- vec_all %>%
#     filter(Y <= 80,
#            X >= 20)
# }



####--setup customized borders to make plos of the same size --####
xmin = min(vec_all$X)
ymin = min(vec_all$Y)-2
xmax = max(vec_all$X)+2
ymax = max(vec_all$Y)

vec_all <- vec_all %>%
  filter(speed >= speed_FWD_motion & speed <= 2.5)

vec_all <- vec_all %>%
  mutate(timedate = ymd_hms(timedate, tz = "UTC"))



### save files for ANDREEA
# write.csv(vec_all, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_", name_camera, "_AD.csv"), row.names=FALSE)


vec_all <- as.vector(vec_all)
vec_all$id <- 1:nrow(vec_all)

coordinates(vec_all) <- c("X", "Y")
df <- as.data.frame(vec_all)

#######################################################################################
#######################################################################################
###### SPEED ##########################################################################

#Line parameters
line.length <- 2 #length of polylines representing wind in the map (meters)
arrow.length <- 0.7 #length of arrowhead leg (meters)
arrow.angle <- 120 #angle of arrowhead leg (degrees azimuth)

#Generate data frame with auxiliary coordinates
end.xy.df <- data.frame(end.x=NA,end.y=NA,end.arrow.x=NA,end.arrow.y=NA)

# i <- 10

for (i in c(1:nrow(df))){
  
  #coordinates of end points for wind lines (the initial points are the ones where data was observed)
  if (df$angle_deg[i] <= 90) {
    end.x <- df$X[i] + (cos((90 - df$angle_deg[i]) * 0.0174532925) * 2*df$speed[i])      #### df$speed == line.length
  } else if (df$angle_deg[i] > 90 & df$angle_deg[i] <= 180) {
    end.x <- df$X[i] + (cos((df$angle_deg[i] - 90) * 0.0174532925) * 2*df$speed[i])
  } else if (df$angle_deg[i] > 180 & df$angle_deg[i] <= 270) {
    end.x <- df$X[i] - (cos((270 - df$angle_deg[i]) * 0.0174532925) * 2*df$speed[i])
  } else {end.x <- df$X[i] - (cos((df$angle_deg[i] - 270) * 0.0174532925) * 2*df$speed[i])}
  
  if (df$angle_deg[i] <= 90) {
    end.y <- df$Y[i] + (sin((90 - df$angle_deg[i]) * 0.0174532925) * 2*df$speed[i])
  } else if (df$angle_deg[i] > 90 & df$angle_deg[i] <= 180) {
    end.y <- df$Y[i] - (sin((df$angle_deg[i] - 90) * 0.0174532925) * 2*df$speed[i])
  } else if (df$angle_deg[i] > 180 & df$angle_deg[i] <= 270) {
    end.y <- df$Y[i] - (sin((270 - df$angle_deg[i]) * 0.0174532925) * 2*df$speed[i])
  } else {end.y <- df$Y[i] + (sin((df$angle_deg[i] - 270) * 0.0174532925) * 2*df$speed[i])}
  
  #coordinates of end points for arrowhead leg lines (the initial points are the previous end points)
  end.arrow.x <- end.x + (cos((df$angle_deg[i] + arrow.angle) * 0.0174532925) * arrow.length)
  end.arrow.y <- end.y - (sin((df$angle_deg[i] + arrow.angle) * 0.0174532925) * arrow.length)
  
  end.xy.df <- rbind(end.xy.df,c(end.x,end.y,end.arrow.x,end.arrow.y)) 
}

end.xy <- end.xy.df[-1,]
df <- data.frame(df,end.xy) #df with observed and auxiliary variables
# head(df,3)

#------------------------------
#Step 3 - Create an object of class `SpatialLinesDataFrame` 

lines <- data.frame(cbind(lng=c(df$X, df$end.x, df$end.arrow.x),
                          lat=c(df$Y, df$end.y, df$end.arrow.y),
                          id=c(rep(df$id,3))))

lines.list <- list()
library(sp)

for (i in c(1:max(lines$id))){
  line <- subset(lines,lines$id==i)
  line <- as.matrix(line[,c(1:2)])
  line <- Line(line) #object of class 'Line'
  lines.list[[i]] <- Lines(list(line), ID = i) #list of 'objects'Lines' 
}

sp.lines <- SpatialLines(lines.list) #object of class 'SpatialLines'
proj4string(sp.lines) <- CRS("+init=epsg:32632") #define CRS
sp.lines <- spTransform(sp.lines, CRS("+init=epsg:32632"))


rownames(df) = df$id
## Join SPEED variables (id, speed, direction and date) to object of class 'SpatialLines'
sp.lines.df <- SpatialLinesDataFrame(sp.lines, df[,c(1,6:7,2)]) #object of class 'SpatialLinesDataFrame'
# plot(sp.lines.df)

#### ---- Build GRID of 2x2 square meters....
cell_SIZE <- 2   ## (UNITS --> [m])
extent = st_sfc(st_polygon(list(rbind(c(xmin,ymin), c(xmax,ymin), c(xmax,ymax),
                                      c(xmin, ymax), c(xmin,ymin)))))

## set CRS
extent <- extent %>% 
  st_set_crs(st_crs(sp.lines.df))

## define a regular grid of 2 meter resolution and add an Id to each cell
GRID <- st_make_grid(extent, cellsize = 2, square = T) %>% 
  st_as_sf() %>%
  mutate(cell_ID = row_number())

# plot(sp.lines.df)
# plot(st_make_grid(GRID, cellsize = 2, square = T), add = TRUE)


########################################################################################################
######### ------- plot AVERAGE SPEEDs and directions per CELL ------------- ############################

###################################
#### MORNING & EVENING HOURS ######
###################################

df <- df %>%
  mutate(hour = hour(timedate))

# df_time <- df

if (time_of_day == "morning") {
  df_time <- df %>%
    filter(hour < 10 & hour >= 6)
} else if (time_of_day == "evening") {
  df_time <- df %>%
    filter(hour <19 & hour >= 17)
}


df_time$X_copy <- df_time$X
df_time$Y_copy <- df_time$Y

DF_sf <- st_as_sf(x = df_time,
                  coords = c("X", "Y")) %>%
  st_set_crs(32632)


GRID_direction <- st_intersection(DF_sf, GRID) 
GRID_direction <- as.data.frame(GRID_direction[, c("angle_deg", "end.x", "end.y","X_copy", "Y_copy", 
                                                   "end.arrow.x", "end.arrow.y", "speed", "cell_ID")])
GRID_direction <- as.data.frame(GRID_direction[, c("cell_ID", "speed", "angle_deg", "end.x", "end.y", 
                                                   "X_copy", "Y_copy", "end.arrow.x", "end.arrow.y")])
## join SPEED values to the GRID
GRID_A <- GRID %>%
  left_join(GRID_direction, by = c("cell_ID"))
GRID_A <- GRID_A %>%
  group_by(cell_ID) %>%
  summarise(mean_speed = mean(speed, na.rm = T),
            mean_angle = mean(angle_deg, na.rm = T),
            inv_sd_angle = 1/sd(angle_deg, na.rm = T),
            mean_end_x = mean(end.x, na.rm = T),
            mean_end_y = mean(end.y, na.rm = T),
            mean_end_arrow_x = mean(end.arrow.x, na.rm = T),
            mean_end_arrow_y = mean(end.arrow.y, na.rm = T),
            mean_X = mean(X_copy, na.rm = T),
            mean_Y = mean(Y_copy, na.rm = T))

## remove rows with inf values
new_df <- as.data.frame(GRID_A)
new_df <-  new_df[, !names(new_df) %in% c("x")]
new_df <- new_df %>% 
  filter_all(all_vars(!is.infinite(.)))
### replace NAs values with 0
new_df <- new_df %>%
  mutate_at(vars(mean_speed), ~replace_na(., 0))


##### data cleaning including the AVERAGES values of the angles ####
# new_df <- new_df %>%
#   filter(mean_angle > 0)
# new_df$inv_sd_angle <- 0.5
# new_df <- new_df[complete.cases(new_df), ]


#########################################################
new_df <- new_df %>%
  filter(mean_speed <= 2.5,
         inv_sd_angle < 0.2)   #5
#########################################################

#Step 3 - Create an object of class `SpatialLinesDataFrame` 

# new_df$id <- 1:nrow(new_df)
# lines <- data.frame(cbind(lng=c(new_df$mean_X, new_df$mean_end_x, new_df$mean_end_arrow_x),
#                           lat=c(new_df$mean_Y, new_df$mean_end_y, new_df$mean_end_arrow_y),
#                           id=c(rep(new_df$id,3))))

#Line parameters
arrow.length <- 1 #length of arrowhead leg (meters)
arrow.angle <- 120 #angle of arrowhead leg (degrees azimuth)

#Generate data frame with auxiliary coordinates
end.xy.df <- data.frame(end.x=NA,end.y=NA,end.arrow.x=NA,end.arrow.y=NA)



for (i in (1:nrow(new_df))){
  print(i)
  if (new_df$mean_angle[i] > 0 & new_df$mean_angle[i] < 90) {
    new_df$mean_angle[i] <- 90 - new_df$mean_angle[i]
  } else if (new_df$mean_angle[i] > 90 & new_df$mean_angle[i] < 180) {
    new_df$mean_angle[i] <- 180 + new_df$mean_angle[i]
  } else if  (new_df$mean_angle[i] > 180 & new_df$mean_angle[i] < 270) {
    new_df$mean_angle[i] <- 90 - new_df$mean_angle[i]
  } else if (new_df$mean_angle[i] > 270 & new_df$mean_angle[i] < 360) {
    diff <-  abs(270 - new_df$mean_angle[i])
    new_df$mean_angle[i] <-   270 + new_df$mean_angle[i] - 2*diff
  }
}

for (i in c(1:nrow(new_df))){
  
  #coordinates of end points for wind lines (the initial points are the ones where data was observed)
  if (new_df$mean_angle[i] <= 90) {
    end.x <- new_df$mean_X[i] + (cos((90 - new_df$mean_angle[i]) * 0.0174532925) * 30*new_df$inv_sd_angle[i])    
  } else if (new_df$mean_angle[i] > 90 & new_df$mean_angle[i] <= 180) {
    end.x <- new_df$mean_X[i] + (cos((new_df$mean_angle[i] - 90) * 0.0174532925) * 30*new_df$inv_sd_angle[i])
  } else if (new_df$mean_angle[i] > 180 & new_df$mean_angle[i] <= 270) {
    end.x <- new_df$mean_X[i] - (cos((270 - new_df$mean_angle[i]) * 0.0174532925) * 30*new_df$inv_sd_angle[i])
  } else {end.x <- new_df$mean_X[i] - (cos((new_df$mean_angle[i] - 270) * 0.0174532925) * 30*new_df$inv_sd_angle[i])}
  
  if (new_df$mean_angle[i] <= 90) {
    end.y <- new_df$mean_Y[i] + (sin((90 - new_df$mean_angle[i]) * 0.0174532925) * 30*new_df$inv_sd_angle[i])
  } else if (new_df$mean_angle[i] > 90 & new_df$mean_angle[i] <= 180) {
    end.y <- new_df$mean_Y[i] - (sin((new_df$mean_angle[i] - 90) * 0.0174532925) * 30*new_df$inv_sd_angle[i])
  } else if (new_df$mean_angle[i] > 180 & new_df$mean_angle[i] <= 270) {
    end.y <- new_df$mean_Y[i] - (sin((270 - new_df$mean_angle[i]) * 0.0174532925) * 30*new_df$inv_sd_angle[i])
  } else {end.y <- new_df$mean_Y[i] + (sin((new_df$mean_angle[i] - 270) * 0.0174532925) * 30*new_df$inv_sd_angle[i])}
  
  #coordinates of end points for arrowhead leg lines (the initial points are the previous end points)
  end.arrow.x <- end.x + (cos((new_df$mean_angle[i] + arrow.angle) * 0.0174532925) * arrow.length)
  end.arrow.y <- end.y - (sin((new_df$mean_angle[i] + arrow.angle) * 0.0174532925) * arrow.length)
  
  end.xy.df <- rbind(end.xy.df,c(end.x,end.y,end.arrow.x,end.arrow.y)) 
}

end.xy <- end.xy.df[-1,]
df_new <- data.frame(new_df,end.xy) #df with observed and auxiliary variables
# head(df,3)

#------------------------------
#Step 3 - Create an object of class `SpatialLinesDataFrame` 

df_new$id <- 1:nrow(df_new)
lines <- data.frame(cbind(lng=c(df_new$mean_X, df_new$end.x, df_new$end.arrow.x),
                          lat=c(df_new$mean_Y, df_new$end.y, df_new$end.arrow.y),
                          id=c(rep(df_new$id,3))))


lines.list <- list()
library(sp)

for (i in c(1:max(lines$id))){
  line <- subset(lines,lines$id==i)
  line <- as.matrix(line[,c(1:2)])
  line <- Line(line) #object of class 'Line'
  lines.list[[i]] <- Lines(list(line), ID = i) #list of 'objects'Lines' 
}

mean_arrows <- SpatialLines(lines.list) #object of class 'SpatialLines'
proj4string(mean_arrows) <- CRS("+init=epsg:32632") #define CRS
mean_arrows <- spTransform(mean_arrows, CRS("+init=epsg:32632"))
plot(mean_arrows)


jpeg(filename=paste0(folder_trajectories, "all_trajectory_directions__17h20_18h20_10Oct2021_", name_camera, "_", time_of_day, ".jpg"),width = 800, height = 800, res = 1)
plot(mean_arrows)
dev.off()


######################################
######################################
######################################
######################################
######----->>>>>>>>>>>>>>>>>>>>>>>>>>>
######----->>>>>>>>>>>>>>>>>>>>>>>>>>>
######----->>>>>>>>>>>>>>>>>>>>>>>>>>>


#### ---- Build GRID of 2x2 square meters....
cell_SIZE <- 2   ## (UNITS --> [m])
extent = st_sfc(st_polygon(list(rbind(c(xmin,ymin), c(xmax,ymin), c(xmax,ymax),
                                      c(xmin, ymax), c(xmin,ymin)))))

## set CRS
extent <- extent %>% 
  st_set_crs(st_crs(mean_arrows))

## define a regular grid of 2 meter resolution and add an Id to each cell
GRID <- st_make_grid(extent, cellsize = 2, square = T) %>% 
  st_as_sf() %>%
  mutate(cell_ID = row_number())

 
## save pics
# png(filename=paste0(folder_trajectories, "all_trajectory_directions_duca_aosta_GRID.png"),width = 800, height = 800, res = 1)
# png(filename=paste0(folder_trajectories, "all_trajectory_directions_lato_taxi_GRID.png"),width = 800, height = 800, res = 1)
# png(filename=paste0(folder_trajectories, "all_trajectory_directions_lato_biciclette_GRID.png"), width = 800, height = 800, res = 1)
# plot(mean_arrows)
# plot(st_make_grid(GRID, cellsize = 2, square = T), add = TRUE)  
# dev.off()


# png(filename=paste0(folder_trajectories, "all_trajectory_directions_duca_aosta.png"),width = 800, height = 800, res = 1)
# png(filename=paste0(folder_trajectories, "all_trajectory_directions_lato_taxi.png"),width = 800, height = 800, res = 1)
# png(filename=paste0(folder_trajectories, "all_trajectory_directions_lato_biciclette.png"), width = 800, height = 800, res = 1)
# plot(mean_arrows)
# dev.off()


#########################################
### ---> ploar mean speed by CELL ---####
### replace NAs values with 0

GRID_B <- GRID_A %>%
  mutate_at(vars(mean_speed), ~replace_na(., 0))
GRID_B <- as.data.frame(GRID_B)
GRID_B[, "mean_speed"][GRID_B[, "mean_speed"] == 0] <- NA
## make an sf object again....
GRID_B <- GRID_B %>% 
  st_as_sf() 



plot_speed <- (GRID_B[, c("x", "mean_speed")]) %>% 
  st_transform("+proj=omerc  +lat_0=0 +lonc=0 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=0") %>%
  ggplot(aes()) +
  theme_bw() +
  geom_sf(aes(fill = mean_speed)) +
  scale_fill_gradient2(low = "grey", mid = "white", high = "blue", midpoint = 0.3, trans = "sqrt",   # limits=c(0.3,2),   
                       name = "[m/s]", na.value = 'white') +
  # scale_fill_gradient2(low = "grey", mid = "white", high = "blue", name = "[m/s]") +
  coord_sf(datum = st_crs(32632)) 
plot_speed
## save plot
dpi = 96
ggsave(filename=paste0(folder_trajectories, "all_mean_speed__17h20_18h20_10Oct2021_", name_camera, "_", time_of_day, ".png"),
       width = 700 / dpi, height = 650 / dpi,
       dpi = dpi)



################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
#### -------------------------------------------- ##############
#### draw clustered directions and speed #######################
#### -------------------------------------------- ##############
#### -------------------------------------------- ##############
#### -------------------------------------------- ##############

# time_of_day <- "morning"   ## from 06:00 to 10:00
time_of_day <- "evening"  ## from 17:00 to 19:00

###---> load clustered speed and directions

if (time_of_day == "morning") {
  cluster_direction <- read.csv("D:/Federico/CityFLows/output_files/morning_clustered_directions_from_06_to_24_Oct2021.csv")[-1]
  cluster_speeds <- read.csv("D:/Federico/CityFLows/output_files/morning_clustered_speeds_from_06_to_24_Oct2021.csv")[-1]
  cluster_directions_speed <- cluster_speeds %>%
    left_join(cluster_direction, by = c("ID", "counts"))
} else if (time_of_day == "evening") {
  cluster_direction <- read.csv("D:/Federico/CityFLows/output_files/evening_clustered_directions_from_06_to_24_Oct2021.csv")[-1]
  cluster_speeds <- read.csv("D:/Federico/CityFLows/output_files/evening_clustered_speeds_from_06_to_24_Oct2021.csv")[-1]
  cluster_directions_speed <- cluster_speeds %>%
    left_join(cluster_direction, by = c("ID", "counts"))
}



### https://aurelienmadouasse.wordpress.com/2012/07/12/r-code-draw-an-arrow-using-origin-length-and-angle/

### load clustered speed and directions
# cluster_direction <- read.csv("D:/Federico/CityFLows/output_files/clustered_directions_from_06_to_24_Oct2021.csv")[-1]
# cluster_speeds <- read.csv("D:/Federico/CityFLows/output_files/clustered_speeds_from_06_to_24_Oct2021.csv")[-1]
# cluster_directions_speed <- cluster_speeds %>%
#   left_join(cluster_direction, by = c("ID", "counts"))


###--- function to draw an arrow taking the coordinates of the origin and end point of the arrow as arguments -- ####
arrows.1 <- function(x0, y0, length.ar, angle.ar, ...){
  
  ab <- cos(angle.ar) * length.ar
  bc <- sign(sin(angle.ar)) * sqrt(length.ar^2 - ab^2)
  
  x1 <- x0 + ab
  y1 <- y0 + bc
  
  arrows(x0, y0, x1, y1, ...)
  outputs <- list(x1, y1)
  return(outputs)
}


## Circle function
circle <- function(xorig, yorig, radius, add, ...){
  
  x <- seq(-radius, radius, length.out = 1000)
  y <- sapply(x, function(x) sqrt(radius^2 - x^2))
  
  if(add == TRUE){
    
    lines(xorig + c(x, rev(x)), c(yorig + y, yorig + rev(-y)), type = "l", ...)
    
  } else {
    
    plot(xorig + c(x, rev(x)), c(yorig + y, yorig + rev(-y)),
         xlab=" ", ylab=" ",
         main=paste0("Piazza Duca d'Aosta\n clustered directions and speeds\n",time_of_day) ,
         type = "l", ...)
 
  }
  
}



library(NISTunits)

x <- c(cluster_directions_speed$angle_deg )
## convert degrees to radians
cluster_directions_speed$angle_rad <- NISTdegTOradian(x)
## normalize counts
cluster_directions_speed$norm_counts <- (cluster_directions_speed$counts)/ max(cluster_directions_speed$counts)


### plot polar diagram with clustered directions
png(filename = paste0("D:/Federico/CityFLows/output_files/trajectories/clustered_directions_", time_of_day, ".png"))

circle(1, 1, 1, add = FALSE)
for(i in  1: nrow(cluster_directions_speed)) {
  arrow <- arrows.1(x0 = 1, y0 = 1, length.ar = cluster_directions_speed$norm_counts[i], 
           angle.ar = cluster_directions_speed$angle_rad[i])
  x1 <- unlist(arrow[1])
  y1 <- unlist(arrow[2])
  text(x1+0.1,y1+0.13, paste0(c(round(cluster_directions_speed$speed[i], digits = 1) ), " m/s")  , cex=1.2)
}

dev.off()

