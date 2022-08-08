

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


time_of_day <- "morning"   ## from 06:00 to 10:00
# time_of_day <- "evening"  ## from 17:00 to 19:00


setwd("D:/Federico/CityFLows/output_files/")
folder_trajectories <- "D:/Federico/CityFLows/output_files/trajectories/"

## set forward motion speed
speed_FWD_motion <- 0.3  ## units [m/s]


if (name_camera == "duca_aosta") {
  # vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_26Nov2021_FK.csv")
  vec_all_a <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_01April2022.csv")
  vec_all_b <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_02April2022.csv")
  vec_all_c <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_03April2022.csv")
  vec_all_d <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_04April2022.csv")
  vec_all_e <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_05April2022.csv")
  vec_all_f <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_06April2022.csv")
  vec_all_g <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_07April2022.csv")
  vec_all_h <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_08April2022.csv")
  vec_all_i <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_19April2022.csv")
  vec_all_l <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_20April2022.csv")
  vec_all_m <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_21April2022.csv")
  vec_all_n <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_22April2022.csv")
  vec_all_o <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_23April2022.csv")
  
  
  ## ----------- get 20% of representative data
  vec_all_a <- vec_all_a %>%
    sample_frac(0.05)
  vec_all_b <- vec_all_b %>%
    sample_frac(0.05)
  vec_all_c <- vec_all_c %>%
    sample_frac(0.05)
  vec_all_d <- vec_all_d %>%
    sample_frac(0.05)
  vec_all_e <- vec_all_e %>%
    sample_frac(0.05)
  vec_all_f <- vec_all_f %>%
    sample_frac(0.05)
  vec_all_g <- vec_all_g %>%
    sample_frac(0.05)
  vec_all_h <- vec_all_h %>%
    sample_frac(0.05)
  vec_all_i <- vec_all_i %>%
    sample_frac(0.05)
  vec_all_l <- vec_all_l %>%
    sample_frac(0.05)
  vec_all_m <- vec_all_m %>%
    sample_frac(0.05)
  vec_all_n <- vec_all_n %>%
    sample_frac(0.05)
  vec_all_o <- vec_all_o %>%
    sample_frac(0.05)
  
  vec_all <- rbind(vec_all_a,
                   vec_all_b,
                   vec_all_c,
                   vec_all_d,
                   vec_all_e,
                   vec_all_f,
                   vec_all_g,
                   vec_all_h,
                   vec_all_i,
                   vec_all_l,
                   vec_all_m,
                   vec_all_n,
                   vec_all_o
                   )
  
}


rm(vec_all_a)
rm(vec_all_b)
rm(vec_all_c)
rm(vec_all_d)
rm(vec_all_e)
rm(vec_all_f)
rm(vec_all_g)
rm(vec_all_h)
rm(vec_all_i)
rm(vec_all_l)
rm(vec_all_m)
rm(vec_all_n)
rm(vec_all_o)

# write.csv(vec_all, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_April2022.csv"), row.names=FALSE)

if (name_camera == "lato_taxi") {
  vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_lato_taxi_angles.csv")
  # vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_lato_taxi_angles_4Nov_50ms.csv")
}


if (name_camera == "lato_biciclette") {
  vec_all <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_lato_biciclette_angles.csv")
}


# TAG <- "01April2022" 
# TAG <- "02April2022"
# TAG <- "03April2022"
# TAG <- "04April2022"
# TAG <- "05April2022"
# TAG <- "06April2022"
# TAG <- "07April2022"
# TAG <- "08April2022"
# TAG <- "19April2022"
# TAG <- "20April2022"
# TAG <- "21April2022"
TAG <- "April2022"

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
# ymin = min(vec_all$Y)-2
# xmax = max(vec_all$X)+2
ymin = min(vec_all$Y)
xmax = max(vec_all$X)
ymax = max(vec_all$Y)


box = c(xmin = xmin, ymin= ymin, xmax= xmax, ymax= ymax)
piazza = st_sfc(st_polygon(list(rbind(c(xmin,ymin), c(xmax,ymin), c(xmax,ymax),
                                      c(xmin, ymax), c(xmin,ymin)))))

piazza <- piazza %>% 
  st_set_crs(st_crs(32632))
plot(piazza)

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
  mutate(hour = hour(timedate),
         minute = minute(timedate),
         second = second(timedate))


if (time_of_day == "morning") {
  df_time_ALL <- df %>%
    filter(hour < 10 & hour >= 6)
} else if (time_of_day == "evening") {
  df_time_ALL <- df %>%
    filter(hour <19 & hour >= 17)
}

# df_time_ALL <- df

##### show distribution of angles
p <- ggplot(df_time_ALL, aes(angle_deg, fill = "blue")) +
  geom_histogram() +  
  guides(fill=FALSE) +
  theme_bw() +
  theme( strip.text = element_text(size =20)) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=20,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                  # Remove x-axis label
  ylab("counts") +            # Set y-axis label
  xlab(expression(paste("angle (degrees)"))) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=20),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=20, hjust = 0.5)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  ggtitle(paste0("directions_", TAG)) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=22))
p

dpi = 96
ggsave(filename=paste0(folder_trajectories,"directions_", TAG , "_", ".png"),
       width = 700 / dpi, height = 650 / dpi,
       dpi = dpi)



##### show distribution of speeds
p <- ggplot(df_time_ALL, aes(speed, fill = "blue")) +
  geom_histogram() +  
  guides(fill=FALSE) +
  theme_bw() +
  theme( strip.text = element_text(size =20)) +
  theme(axis.text.x=element_text(angle=0,hjust=1,vjust=0.5)) +
  theme(axis.text.x=element_text(size=20,face="bold", colour = "black")) +
  theme(axis.title.x = element_blank()) +                  # Remove x-axis label
  ylab("counts") +            # Set y-axis label
  xlab(expression(paste("speed(m/s)"))) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=20),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=20, hjust = 0.5)) + 
  theme(axis.title.y = element_text(face="bold", colour="black", size=20),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=20)) +
  ggtitle(paste0("speeds_", TAG)) +
  theme(plot.title = element_text(lineheight=.8, face="bold", size=22))
p

dpi = 96
ggsave(filename=paste0(folder_trajectories,"speeds_", TAG , "_", ".png"),
       width = 700 / dpi, height = 650 / dpi,
       dpi = dpi)



### loop every 1 minute -----------------------------------------
####-------------------------------------------------------------

# AAA <- df_time %>%
#   group_by(ID) %>%
#   count(ID)

# AAA <- df_time_ALL %>%
#   mutate(timePeriod_15sec = floor_date(timedate, "15seconds"))
#     group_by(timePeriod_15sec) %>%
#     mutate(flag = paste0("gotta_", j))

### ----->>>>>> ###### -------------------------------------------------------------------------------------------
### ----->>>>>> ###### -------------------------------------------------------------------------------------------
df_time_ALL <- df_time_ALL %>%
  dplyr::mutate(time = format(ymd_hms(as.character(df_time_ALL$timedate, tz = "CET")),'%H:%M:%S'))
## order by time
df_time_ALL <- df_time_ALL[order(df_time_ALL$time),]
# df_time_ALL <- df_time_ALL[order(df_time_ALL$time, df_time_ALL$X, df_time_ALL$Y),]
df_time_ALL <- df_time_ALL[!duplicated(df_time_ALL[c("timedate","ID", "X", "Y")]),]
df_time_ALL$FLAG <- 0


### ---> remove consecutive points that are more than 1 meters far from each other.
# for (k in 1:nrow(df_time_ALL))  {
#   dist <- sqrt( ((df_time_ALL[k+1, "X"] - df_time_ALL[k, "X"])^2) +
#                   ((df_time_ALL[k+1, "Y"] - df_time_ALL[k, "Y"])^2) )
#   ang_diff <- abs(df_time_ALL$angle_deg[k+1] - df_time_ALL$angle_deg[k])
#   if (k != nrow(df_time_ALL) &
#       # df_time_ALL$ID[k] == df_time_ALL$ID[k+1] |
#       df_time_ALL$ID[k] != df_time_ALL$ID[k+1] &
#       # df_time_ALL$time[k] == df_time_ALL$time[k+1] &
#       ang_diff > 90) {  #dist < 1.5    #best choice -----> 90 degrees  
#     # print(dist)
#     # print(paste0("k:",k))
#     # print(df_time_ALL[k,"ID"])
#     print(ang_diff)
#     df_time_ALL$FLAG[k] <- -999
#   }
# }
# 
# df_time_ALL <- df_time_ALL[!(df_time_ALL$FLAG == -999), ]



##----PREVALENT directions OBSERVED from DISTRIBUTION of angles--------------

df_time_S <- df_time_ALL %>%
  filter(angle_deg >= 250 & angle_deg <= 280)
df_time_N <- df_time_ALL %>%
  filter(angle_deg >= 80 & angle_deg <= 120)
# df_time_ALL <- rbind(df_time_S,
#                      df_time_N)
df_time_ALL <- df_time_S

### ----->>>>>> ###### -------------------------------------------------------------------------------------------
### ----->>>>>> ###### -------------------------------------------------------------------------------------------

df_time_ALL <- df_time_ALL %>%
  mutate(timePeriod_15sec = floor_date(timedate, "30seconds"))
df_time_ALL <- df_time_ALL %>%
  mutate(floor = second(timePeriod_15sec))
list_floor <- unique(as.list(df_time_ALL$floor))

# df_time_ALL <- df_time_ALL[1:10000, ]
list_hours <- unique(as.list(df_time_ALL$hour))
list_minutes <- unique(as.list(df_time_ALL$minute))

plot(piazza)

for (h in 1:length(list_hours)) {
  for (j in 1:length(list_minutes)) {
    for (f in 1:length(list_floor)) {
    print(paste0("hour:", list_hours[h], " @ ","minute:", j, " @ ","second:", list_floor[f]))
    df_time <- df_time_ALL %>%
      filter(hour == list_hours[h] & minute == list_minutes[j] & floor == list_floor[f])
    

# df_time <- df_time_ALL
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
            mean_angle = median(angle_deg, na.rm = T),
            inv_sd_angle = 1/sd(angle_deg, na.rm = T),
            mean_end_x = median(end.x, na.rm = T),
            mean_end_y = median(end.y, na.rm = T),
            mean_end_arrow_x = median(end.arrow.x, na.rm = T),
            mean_end_arrow_y = median(end.arrow.y, na.rm = T),
            mean_X = median(X_copy, na.rm = T),
            mean_Y = median(Y_copy, na.rm = T))

## remove rows with inf values
new_df <- as.data.frame(GRID_A)
new_df <-  new_df[, !names(new_df) %in% c("x")]
new_df <- new_df %>% 
  filter_all(all_vars(!is.infinite(.)))
### replace NAs values with 0
new_df <- new_df %>%
  mutate_at(vars(mean_speed), ~replace_na(., 0))


# new_df_S <- new_df %>%
#   filter(mean_angle >= 250 & mean_angle <= 280)
# new_df_N <- new_df %>%
#   filter(mean_angle >= 80 & mean_angle <= 120)
# new_df <- rbind(new_df_S,
#                 new_df_N)

#########################################################
new_df <- new_df %>%
  filter(mean_speed <= 2.5,
         inv_sd_angle < 0.2)  ## best value --> 0.3 - 0.4
#########################################################


#Step 3 - Create an object of class `SpatialLinesDataFrame` 

# new_df$id <- 1:nrow(new_df)
# lines <- data.frame(cbind(lng=c(new_df$mean_X, new_df$mean_end_x, new_df$mean_end_arrow_x),
#                           lat=c(new_df$mean_Y, new_df$mean_end_y, new_df$mean_end_arrow_y),
#                           id=c(rep(new_df$id,3))))

#Line parameters
arrow.length <- 0.5 #length of arrowhead leg (meters)
arrow.angle <- 120 #angle of arrowhead leg (degrees azimuth)
length <- 0.05*1.25   #0.05

#Generate data frame with auxiliary coordinates
end.xy.df <- data.frame(end.x=NA,end.y=NA,end.arrow.x=NA,end.arrow.y=NA)


if (nrow(new_df) > 2) {
  
  for (i in (1:nrow(new_df))){
    
    
    # print(i)
    if (new_df$mean_angle[i] > 0 & new_df$mean_angle[i] < 90) {
      new_df$mean_angle[i] <- 90 - new_df$mean_angle[i]
    } else if (new_df$mean_angle[i] > 90 & new_df$mean_angle[i] < 180) {
      new_df$mean_angle[i] <- 90 - new_df$mean_angle[i]    ##################### <--------
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
      end.x <- new_df$mean_X[i] + (cos((90 - new_df$mean_angle[i]) * 0.0174532925) * 30*length)    
    } else if (new_df$mean_angle[i] > 90 & new_df$mean_angle[i] <= 180) {
      end.x <- new_df$mean_X[i] + (cos((new_df$mean_angle[i] - 90) * 0.0174532925) * 30*length)
    } else if (new_df$mean_angle[i] > 180 & new_df$mean_angle[i] <= 270) {
      end.x <- new_df$mean_X[i] - (cos((270 - new_df$mean_angle[i]) * 0.0174532925) * 30*length)
    } else {end.x <- new_df$mean_X[i] - (cos((new_df$mean_angle[i] - 270) * 0.0174532925) * 30*length)}
    
    if (new_df$mean_angle[i] <= 90) {
      end.y <- new_df$mean_Y[i] + (sin((90 - new_df$mean_angle[i]) * 0.0174532925) * 30*length)
    } else if (new_df$mean_angle[i] > 90 & new_df$mean_angle[i] <= 180) {
      end.y <- new_df$mean_Y[i] - (sin((new_df$mean_angle[i] - 90) * 0.0174532925) * 30*length)
    } else if (new_df$mean_angle[i] > 180 & new_df$mean_angle[i] <= 270) {
      end.y <- new_df$mean_Y[i] - (sin((270 - new_df$mean_angle[i]) * 0.0174532925) * 30*length)
    } else {end.y <- new_df$mean_Y[i] + (sin((new_df$mean_angle[i] - 270) * 0.0174532925) * 30*length)}
    
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
  # plot(mean_arrows)
  
  # box = c(xmin = xmin, ymin= ymin, xmax= xmax, ymax= ymax)
  # piazza = st_sfc(st_polygon(list(rbind(c(xmin,ymin), c(xmax,ymin), c(xmax,ymax),
  #                                       c(xmin, ymax), c(xmin,ymin)))))
  # 
  # ## set CRS
  # piazza <- piazza %>%
  #   st_set_crs(st_crs(mean_arrows))
  # plot(piazza)
  
  
  
  plot(mean_arrows, add = T, pch = 16, cex = .01, lwd=2.0)   # plot(x, y, type="l", lwd=5.0, col="blue")
  # jpeg(filename=paste0(folder_trajectories, list_hours[h],"h_", j, "min_", list_floor[f],"secs_27Nov2021_", name_camera, "_",
  #                      time_of_day, ".jpg"),width = 800, height = 800, res = 1)
  # plot(piazza)
  # plot(mean_arrows, add = T, pch = 16, cex = .01)
  # dev.off()
  
  
#      }
#     }
#   }
# }


# jpeg(filename=paste0(folder_trajectories, "all_trajectory_directions__17h20_18h20_10Oct2021_", name_camera, "_", time_of_day, ".jpg"),width = 800, height = 800, res = 1)
# jpeg(filename=paste0(folder_trajectories, "all_trajectory_directions__27Nov2021_", name_camera, "_", time_of_day, ".jpg"),width = 800, height = 800, res = 1)
# jpeg(filename=paste0(folder_trajectories, "all_trajectory_directions__25Nov2021_evening_", name_camera, "_", time_of_day, ".jpg"),width = 800, height = 800, res = 1)
# jpeg(filename=paste0(folder_trajectories, "all_trajectory_directions__28Nov2021", name_camera, "_", time_of_day, ".jpg"),width = 800, height = 800, res = 1)
# jpeg(filename=paste0(folder_trajectories, "all_trajectory_directions__25Nov2021_subway_entrace_right_side", name_camera, "_", time_of_day, ".jpg"),width = 800, height = 800, res = 1)
# plot(mean_arrows)
# dev.off()


######----->>>>>>>>>>>>>>>>>>>>>>>>>>>
######----->>>>>>>>>>>>>>>>>>>>>>>>>>>
#### ---------------  MEAN SPEED ------------------------- #####################################
################################################################################################

#### ---- Build GRID of 2x2 square meters....
cell_SIZE <- 2   ## (UNITS --> [m])
extent = st_sfc(st_polygon(list(rbind(c(xmin,ymin), c(xmax,ymin), c(xmax,ymax),
                                      c(xmin, ymax), c(xmin,ymin)))))

## set CRS
# extent <- extent %>% 
#   st_set_crs(st_crs(mean_arrows))

extent <- extent %>% 
  st_set_crs(st_crs(32632))

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
### ---> polar mean speed by CELL ---####
### replace NAs values with 0

GRID_B <- GRID_A %>%
  mutate_at(vars(mean_speed), ~replace_na(., 0))
GRID_B <- as.data.frame(GRID_B)
GRID_B[, "mean_speed"][GRID_B[, "mean_speed"] == 0] <- NA
GRID_B[, "mean_speed"][GRID_B[, "mean_speed"] < 0.8] <- NA
# GRID_B[, "inv_sd_angle"][GRID_B[, "inv_sd_angle"] < 0.2] <- NA
## make an sf object again....
GRID_B <- GRID_B %>% 
  st_as_sf() 



# plot(extent)
library(scales)
# cols <- cut(GRID_B$mean_speed, 6, labels = c("pink", "red", "yellow", "blue", "green", ))
# cols <- cut(GRID_B$mean_speed, 2, labels = c("purple", "blue"))
cols <- cut(GRID_B$mean_speed, 2, labels = c("lightskyblue", "purple"))
plot((GRID_B[, c("x", "mean_speed")]), add = T, pch = 16, cex = .01, col = alpha(cols, 0.2))

# plot_speed <- (GRID_B[, c("x", "mean_speed")]) %>% 
#   st_transform("+proj=omerc  +lat_0=0 +lonc=0 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=0") %>%
#   ggplot(aes()) +
#   theme_bw() +
#   geom_sf(aes(fill = mean_speed)) +
#   scale_fill_gradient2(low = "grey", mid = "white", high = "blue", midpoint = 0.3, trans = "sqrt",   # limits=c(0.3,2),   
#                        name = "[m/s]", na.value = 'white') +
#   # scale_fill_gradient2(low = "grey", mid = "white", high = "blue", name = "[m/s]") +
#   coord_sf(datum = st_crs(32632)) 
# plot_speed

## save plot
dpi = 96
# ggsave(filename=paste0(folder_trajectories, "all_mean_speed_", name_camera, "_", time_of_day, ".png"),
#        width = 700 / dpi, height = 650 / dpi,
#        dpi = dpi)
# ggsave(filename=paste0(folder_trajectories, list_hours[h],"h_", j, "min_", list_floor[f],"secs_mean_speed_25Nov2021_", name_camera, "_",
#                        time_of_day, ".png"),
#        width = 700 / dpi, height = 650 / dpi,
#        dpi = dpi)

      }
    }
  }
}



##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
##################################################################################################################################################################
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


### https://aurelienmadouasse.wordpress.com/2012/07/12/r-code-draw-an-arrow-using-origin-length-and-angle/


# time_of_day <- "morning"   ## from 06:00 to 10:00
time_of_day <- "evening"  ## from 17:00 to 19:00
# time_of_day <- "all day"

###---> load clustered speed and directions

### ----> from_17h20_to_18h20_10Oct2021 (for Andreea)
# cluster_direction <- read.csv("D:/Federico/CityFLows/output_files/all_day_clustered_directions_from_17h20_to_18h20_10Oct2021.csv")[-1]
# cluster_speeds <- read.csv("D:/Federico/CityFLows/output_files/all_day_clustered_speeds_from_17h20_to_18h20_10Oct2021.csv")[-1]
# cluster_directions_speed <- cluster_speeds %>%
#   left_join(cluster_direction, by = c("ID", "counts"))

if (time_of_day == "morning") {
  cluster_direction <- read.csv("D:/Federico/CityFLows/output_files/morning_clustered_directions_April2022.csv")[-1]
  cluster_speeds <- read.csv("D:/Federico/CityFLows/output_files/morning_clustered_speeds_April2022.csv")[-1]
  cluster_directions_speed <- cluster_speeds %>%
    left_join(cluster_direction, by = c("ID", "counts"))
} else if (time_of_day == "evening") {
  cluster_direction <- read.csv("D:/Federico/CityFLows/output_files/evening_clustered_directions_April2022.csv")[-1]
  cluster_speeds <- read.csv("D:/Federico/CityFLows/output_files/evening_clustered_speeds_April2022.csv")[-1]
  cluster_directions_speed <- cluster_speeds %>%
    left_join(cluster_direction, by = c("ID", "counts"))
}


# if (time_of_day == "morning") {
#   cluster_direction <- read.csv("D:/Federico/CityFLows/output_files/morning_clustered_directions_28Nov2021.csv")[-1]
#   cluster_speeds <- read.csv("D:/Federico/CityFLows/output_files/morning_clustered_speeds_28Nov2021.csv")[-1]
#   cluster_directions_speed <- cluster_speeds %>%
#     left_join(cluster_direction, by = c("ID", "counts"))
# } else if (time_of_day == "evening") {
#   cluster_direction <- read.csv("D:/Federico/CityFLows/output_files/evening_clustered_directions_28Nov2021.csv")[-1]
#   cluster_speeds <- read.csv("D:/Federico/CityFLows/output_files/evening_clustered_speeds_28Nov2021.csv")[-1]
#   cluster_directions_speed <- cluster_speeds %>%
#     left_join(cluster_direction, by = c("ID", "counts"))
# }



# ### ----> 27Nov2021 (for Andreea)
# cluster_direction <- read.csv("D:/Federico/CityFLows/output_files/all_day_clustered_directions_27Nov2021.csv")[-1]
# cluster_speeds <- read.csv("D:/Federico/CityFLows/output_files/all_day_clustered_speeds_27Nov2021.csv")[-1]
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
         main=paste0("Piazza Duca d'Aosta\n clustered directions and speeds\n","(",time_of_day,")") ,
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
png(filename = paste0("D:/Federico/CityFLows/output_files/trajectories/clustered_directions_",
                       time_of_day, ".png"), width = 12, height = 12, units = "cm", res = 500)

# png(filename = paste0("D:/Federico/CityFLows/output_files/trajectories/clustered_directions_from_17h20_to_18h20_10Oct2021.png"))
# png(filename = paste0("D:/Federico/CityFLows/output_files/trajectories/clustered_directions_27Nov2021.png"))
# png(filename = paste0("D:/Federico/CityFLows/output_files/trajectories/clustered_directions_28Nov2021", time_of_day, ".png"))

circle(1, 1, 1, add = FALSE)
for(i in  1: nrow(cluster_directions_speed)) {
  arrow <- arrows.1(x0 = 1, y0 = 1, length.ar = cluster_directions_speed$norm_counts[i], 
           angle.ar = cluster_directions_speed$angle_rad[i])
  x1 <- unlist(arrow[1])
  y1 <- unlist(arrow[2])
  # text(x1+0.1,y1+0.13, paste0(c(round(cluster_directions_speed$speed[i], digits = 1) ), " m/s")  , cex=1.2)
  text(x1-0.22,y1+0.12, paste0(c(round(cluster_directions_speed$speed[i], digits = 1) ), " m/s")  , cex=1.2)
}

dev.off()

# cluster_directions_speed <- cluster_directions_speed %>%
#   dplyr::select(ID,
#                 counts,
#                 speed,
#                 angle_deg)
# write.csv(cluster_directions_speed, "cluster_directions_speed_from_17h20_to_18h20_10Oct2021.csv")


cluster_directions_speed <- cluster_directions_speed %>%
  dplyr::select(ID,
                counts,
                speed,
                angle_deg)
# write.csv(cluster_directions_speed, "cluster_directions_speed_27Nov2021.csv")

