

rm(list = ls())

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


setwd("D:/Federico/CityFLows/output_files/")
folder_speed_duca_aosta <- "D:/Federico/CityFLows/output_files/speed_plots_Duca_Aosta/"

## set forward motion speed
speed_FWD_motion <- 0.2  ## units [m/s]

###-----------------****** IDs generated from FEDERICO KARAGULIAN from video processing with OpenCV -----******########
# DF <- read.csv('D:/Federico/CityFLows/objectdetection/IDs_meters_Piazza_Duca_Aosta_26Nov2021.csv')[-1]
DF_a <- read.csv('D:/Federico/CityFLows/objectdetection/01April2022_PiazzaDucaAosta_meters.csv')[-1]
DF_a <- DF_a %>%
  dplyr::select(X_m, Y_m, ID, timestamp)
DF_b <- read.csv('D:/Federico/CityFLows/objectdetection/02April2022_PiazzaDucaAosta_meters.csv')[-1]
DF_b <- DF_b %>%
  dplyr::select(X_m, Y_m, ID, timestamp)
DF_c <- read.csv('D:/Federico/CityFLows/objectdetection/03April2022_PiazzaDucaAosta_meters.csv')[-1]
DF_c <- DF_c %>%
  dplyr::select(X_m, Y_m, ID, timestamp)
DF_d <- read.csv('D:/Federico/CityFLows/objectdetection/04April2022_PiazzaDucaAosta_meters.csv')[-1]
DF_d <- DF_d %>%
  dplyr::select(X_m, Y_m, ID, timestamp)
DF_e <- read.csv('D:/Federico/CityFLows/objectdetection/05April2022_PiazzaDucaAosta_meters.csv')[-1]
DF_e <- DF_e %>%
  dplyr::select(X_m, Y_m, ID, timestamp)
DF_f <- read.csv('D:/Federico/CityFLows/objectdetection/06April2022_PiazzaDucaAosta_meters.csv')[-1]
DF_f <- DF_f %>%
  dplyr::select(X_m, Y_m, ID, timestamp)
DF_g <- read.csv('D:/Federico/CityFLows/objectdetection/07April2022_PiazzaDucaAosta_meters.csv')[-1]
DF_g <- DF_g %>%
  dplyr::select(X_m, Y_m, ID, timestamp)
DF_h <- read.csv('D:/Federico/CityFLows/objectdetection/08April2022_PiazzaDucaAosta_meters.csv')[-1]
DF_h <- DF_h %>%
  dplyr::select(X_m, Y_m, ID, timestamp)
DF_i <- read.csv('D:/Federico/CityFLows/objectdetection/19April2022_PiazzaDucaAosta_meters.csv')[-1]
DF_i <- DF_i %>%
  dplyr::select(X_m, Y_m, ID, timestamp)
DF_l <- read.csv('D:/Federico/CityFLows/objectdetection/20April2022_PiazzaDucaAosta_meters.csv')[-1]
DF_l <- DF_l %>%
  dplyr::select(X_m, Y_m, ID, timestamp)
DF_m <- read.csv('D:/Federico/CityFLows/objectdetection/21April2022_PiazzaDucaAosta_meters.csv')[-1]
DF_m <- DF_m %>%
  dplyr::select(X_m, Y_m, ID, timestamp)
DF_n <- read.csv('D:/Federico/CityFLows/objectdetection/22April2022_PiazzaDucaAosta_meters.csv')[-1]
DF_n <- DF_n %>%
  dplyr::select(X_m, Y_m, ID, timestamp)
DF_o <- read.csv('D:/Federico/CityFLows/objectdetection/23April2022_PiazzaDucaAosta_meters.csv')[-1]
DF_o <- DF_o %>%
  dplyr::select(X_m, Y_m, ID, timestamp)


## ----------- get 20% of representative data
DF_a <- DF_a %>%
  sample_frac(0.10)
DF_b <- DF_b %>%
  sample_frac(0.10)
DF_c <- DF_c %>%
  sample_frac(0.10)
DF_d <- DF_d %>%
  sample_frac(0.10)
DF_e <- DF_e %>%
  sample_frac(0.10)
DF_f <- DF_f %>%
  sample_frac(0.1)
DF_g <- DF_g %>%
  sample_frac(0.1)
DF_h <- DF_h %>%
  sample_frac(0.1)
DF_i <- DF_i %>%
  sample_frac(0.1)
DF_l <- DF_l %>%
  sample_frac(0.1)
DF_m <- DF_m %>%
  sample_frac(0.1)
DF_n <- DF_n %>%
  sample_frac(0.1)
DF_o <- DF_o %>%
  sample_frac(0.1)

DF <- rbind(DF_a,
            DF_b,
            DF_c,
            DF_d,
            DF_e,
            DF_f,
            DF_g,
            DF_h,
            DF_i,
            DF_l,
            DF_m,
            DF_n,
            DF_o
)



rm(DF_a)
rm(DF_b)
rm(DF_c)
rm(DF_d)
rm(DF_e)
rm(DF_f)
rm(DF_g)
rm(DF_h)
rm(DF_i)
rm(DF_l)
rm(DF_m)
rm(DF_n)
rm(DF_o)


DF <- DF %>%
  mutate(hour = hour(timestamp),
         minute = minute(timestamp),
         second = second(timestamp))

time_of_day <- "morning"
# time_of_day <- "evening"


if (time_of_day == "morning") {
  DF <- DF %>%
    filter(hour < 10 & hour >= 6)
} else if (time_of_day == "evening") {
  DF <- DF %>%
    filter(hour <19 & hour >= 17)
}



DF <- DF %>%
  dplyr::select(X_m, Y_m, ID, timestamp)

names(DF) <- c("X", "Y", "ID", "timedate")



# remove duplicates rows
DF <- DF[!duplicated(DF), ]

### APPROXIMATION to transform PIXELS into METERS
### Piazza DUCA Aosta Milano
# DF$X <- (((DF$X)/10)/2)*0.15   
# DF$Y <- ((DF$Y)/10)*1.1  # 0.75

DF$X <- (((DF$X)/10)/2)*0.09   # 0.15
DF$Y <- ((DF$Y)/10)*0.7  # 0.75   # 1.1


## count recurrency of IDs
COUNTS_ID <- DF %>%
  group_by(ID) %>%
  summarise(count_IDs = length(ID))


DF <- DF %>%
  merge(COUNTS_ID[,c("ID")], by = "ID")


####--setup customized borders to make plots of the same size --####
xmin = min(DF$X)
ymin = min(DF$Y)
xmax = max(DF$X)
ymax = max(DF$Y)


DF <- DF %>%
  mutate(year = year(timedate),
         month = month(timedate),
         day = day(timedate),
         hour = hour(timedate),
         minute = minute(timedate))

day_list <- as.list(DF$day)
day_list <- unique(day_list)


if (time_of_day == "morning") {
  DF <- DF %>%
    dplyr::filter(hour <= 10 & hour >= 6)
} else if (time_of_day == "evening") {
  DF <- DF %>%
    dplyr::filter(hour <= 19 & hour >= 17)
}


        
## select unique ID by  timestamp and DIST
DF_people <- DF

## build a new data frame DF_people including speeds associated to each position
DF_people_speed = NULL


## get list of all the IDs in the current timeslot
list_IDs <- unique(DF_people$ID)
# i <- 1
  
for (i in list_IDs) {
    # print(paste0("ID: ",i))
    distance <- 0
    DF_people_speed_ID = NULL
    
    
    ## select one trajectory
    DF_a <- DF_people %>%
      filter(ID == i)
    
    ## remove duplicated X, Y
    DF_a <- DF_a[!duplicated(DF_a[c("X", "Y")]),]
    DF_a <- DF_a %>%
      group_by(Y) %>%
      summarise(X = mean(X, na.rm = T),
                ID = ID,
                timedate= timedate,
                day = day,
                hour = hour,
                minute = minute, .groups = 'drop')
    DF_a <- as.data.frame(DF_a)
      
    
    ## sort DF_a by timedate
    # DF_a <- DF_a[order(DF_a$timedate),]
    

    p_t <- ggplot(DF_a, aes(x = X, y = Y, colour = ID, label=ID, group = ID)) +
      theme_bw() +
      geom_point(aes(X,Y,color=NULL, fill = ID), size = 2) +
      geom_path(aes(X,Y,color=NULL, fill = ID)) +
      # geom_text(aes(label=  paste0("ID: ", ID, "  -->",  round( minute(timedate), 0), ":", round( second(timedate), 0))     )   ,hjust=0, vjust=0) +
      geom_text(aes(label=  paste0(round( minute(timedate), 0), ":", round( second(timedate), 0))     )   ,hjust=0, vjust=0) +
      # scale_y_reverse() +
      # scale_x_reverse() +
      theme(legend.position = "none")
    # coord_flip()
    p_t
    
  
    
    # DF_a$timedate <- as.POSIXct(DF_a$timedate)
    ## add row number
    DF_a$index <- 1:nrow(DF_a)
    
    DF_a <- DF_a %>%
      dplyr::mutate(time = format(ymd_hms(as.character(DF_a$timedate, tz = "CET")),'%H:%M:%S'))
    DF_a <- as.data.frame(DF_a)
    

    if (nrow(DF_a) > 1) {
      
      ## compute speed without curvature (UNITS --> [m/s]), principal movement speed
      max_timedate <- max(strptime(DF_a[,"time"],format="%H:%M:%S"))
      min_timedate <-  min(strptime(DF_a[,"time"],format="%H:%M:%S"))
      # max_timedate <- max(strptime(DF_a[,"timedate"], format = "%Y-%m-%d %H:%M:%OS"))
      # min_timedate <- min(strptime(DF_a[,"timedate"], format = "%Y-%m-%d %H:%M:%OS"))
      op <- options(digits.secs=3)
      # options(op) #reset options
      
      
      duration_trajectory <- difftime(max_timedate,min_timedate)
      ## set units
      units(duration_trajectory) <- "secs"
      
      
      for (j in 1:nrow(DF_a))  {
        if (j != nrow(DF_a)) {
          ##---distance between two consecutive points (in meters)
          distance_j <- sqrt( ((DF_a[j+1, "X"] - DF_a[j, "X"])^2) +
                                ((DF_a[j+1, "Y"] - DF_a[j, "Y"])^2) )
          # print(paste0("distance: ", distance_j))
          distance = distance + distance_j
          
          
          ## speed [m/s]
          time1a <- strptime(DF_a[j+1, "time"],format="%H:%M:%S")
          time2a <- strptime(DF_a[j, "time"],format="%H:%M:%S")
          
          # ## calculate the speed along the single trajectory of each ID
          # ###-->> this is also the AVERAGE SPEEED OVER the QUARTER of HOUR --####
          DF_speed <- data.frame(timedate = DF_a[j, "timedate"],
                                 ID = i,
                                 X = DF_a[j, "X"],
                                 Y = DF_a[j, "Y"],
                                 speed = 0)

          names(DF_speed) <- c("timedate", "ID", "X", "Y", "speed")
          DF_people_speed_ID = rbind(DF_people_speed_ID, DF_speed)
        }
      }
      
      ################################################################
      ##### filter out erroneous points ##############################
      
      
      ## ---> remove consecutive points that are more than 1 meters far from each other.
      for (j in 1:nrow(DF_people_speed_ID))  {
        if (j != nrow(DF_people_speed_ID)) {

          # j <- 42

          DF_people_speed_ID <- DF_people_speed_ID %>%
            dplyr::mutate(time = format(ymd_hms(as.character(DF_people_speed_ID$timedate, tz = "CET")),'%H:%M:%S'))
          DF_people_speed_ID <- as.data.frame(DF_people_speed_ID)
          time1a <- strptime(DF_people_speed_ID[j+1, "time"],format="%H:%M:%S")
          time2a <- strptime(DF_people_speed_ID[j, "time"],format="%H:%M:%S")
          duration <- difftime(time1a,time2a)
          ## remove "time" field
          DF_people_speed_ID <- DF_people_speed_ID[ , -which(names(DF_people_speed_ID) %in% c("time")) ]
          ## set units
          units(duration) <- "secs"

          if(time1a != time2a) {

            distance_jdx <- sqrt( ((DF_people_speed_ID[j+1, "X"] - DF_people_speed_ID[j, "X"])^2) +
                                    ((DF_people_speed_ID[j+1, "Y"] - DF_people_speed_ID[j, "Y"])^2) )

            if (j > 1) { ##---distance between two consecutive points (in meters)
              distance_jsx <- sqrt( ((DF_people_speed_ID[j-1, "X"] - DF_people_speed_ID[j, "X"])^2) +
                                      ((DF_people_speed_ID[j-1, "Y"] - DF_people_speed_ID[j, "Y"])^2) )
            } else {distance_jsx <- 0}

            if (j+2 < nrow(DF_people_speed_ID)) { ##---distance between two consecutive points (in meters)
              distance_jddx <- sqrt( ((DF_people_speed_ID[j+2, "X"] - DF_people_speed_ID[j, "X"])^2) +
                                       ((DF_people_speed_ID[j+2, "Y"] - DF_people_speed_ID[j, "Y"])^2) )
            } else {distance_jddx <- 0}


            speed_jdx <- distance_jdx / as.numeric(duration) # time in seconds
            speed_jdx <- abs(speed_jdx)

            ## the value "1" indicates 1 meter distance while 2 indicates a distance of 2 meters.
            if( as.numeric(difftime(time1a,time2a))<=1 & distance_jdx >=1 &
                distance_jsx <=1  & distance_jddx <=1 ) {
              (DF_people_speed_ID[j+1, "speed"] <-0 )
            } else  {DF_people_speed_ID[j+1, "speed"] = speed_jdx}
          } else  {DF_people_speed_ID[j+1, "speed"] = NA}
        }
      }
      
      
      ################################################################
      ################################################################
      ################################################################
 
      ## calculate the speed alone the singe trajectory of each ID
      ###-->> this is also the AVERAGE SPEEED OVER the QUARTER of HOUR --####
      speed_trajectory_quarter <- distance / as.numeric(duration_trajectory)  ## m/secs
      # speed_trajectory_quarter <- mean(DF_people_speed_ID[(DF_people_speed_ID$speed)>0, c("speed")] )
      DF_people_speed_ID$speed <- speed_trajectory_quarter
      
      
      ### ----> get angle (in degrees) from ORIGIN --> DESTINATION
      len_path <- nrow(DF_people_speed_ID)
      
      ## sort data by timedate
      DF_people_speed_ID <- DF_people_speed_ID[order(DF_people_speed_ID$timedate),]
      
      DY <- DF_people_speed_ID[len_path ,"Y"] - DF_people_speed_ID[1 ,"Y"]
      DX <- DF_people_speed_ID[len_path ,"X"] - DF_people_speed_ID[1 ,"X"]
    
      slope <- DY/DX
      ## get angle from arcTg(slope)
      angle_rad <- atan((slope))  ## this is in radians
      angle_deg <- NISTradianTOdeg(angle_rad)   ## this in degree
      # print(paste0("----------------------------  ", angle_rad))
      
      if (angle_deg <= 0 & DX < 0 & !is.na(angle_deg)) {
        angle_deg = angle_deg + 180   
      } else if (DX < 0 &  DY < 0 & !is.na(angle_deg)) {
        angle_deg = angle_deg + 180  
      } else if (DX > 0 & DY <= 0 & !is.na(angle_deg)) {
        angle_deg = angle_deg + 360
      } else if (DX <= 0 & DY <= 0 & !is.na(angle_deg)) {
        angle_deg = angle_deg + 360
      }
      
      # print(paste0("----------------------------  ", angle_deg))
      DF_people_speed_ID$angle_deg <- angle_deg
    
      DF_people_speed_ID <- DF_people_speed_ID[!is.na(DF_people_speed_ID$speed),]
      DF_people_speed_ID <- DF_people_speed_ID[ (DF_people_speed_ID$speed > speed_FWD_motion), ]
      DF_people_speed_ID <- DF_people_speed_ID[ (DF_people_speed_ID$speed < 2.5), ]
      speed_trajectory_quarter <- mean(DF_people_speed_ID[(DF_people_speed_ID$speed)> 0, c("speed")] )
  
      
      len_path <- nrow(DF_people_speed_ID)
      
      DY <- DF_people_speed_ID[len_path ,"Y"] - DF_people_speed_ID[1 ,"Y"]
      DX <- DF_people_speed_ID[len_path ,"X"] - DF_people_speed_ID[1 ,"X"]
      
      slope <- DY/DX
      ## get angle from arcTg(slope)
      angle_rad <- atan((slope))  ## this is in radians
      angle_deg <- NISTradianTOdeg(angle_rad)   ## this in degree
      # print(paste0("----------------------------  ", angle_rad))
      
      tryCatch({
      
      if (angle_deg <= 0 & DX < 0 & !is.na(angle_deg)) {
        angle_deg = angle_deg + 180   
      } else if (DX < 0 &  DY < 0 & !is.na(angle_deg)) {
        angle_deg = angle_deg + 180  
      } else if (DX > 0 & DY <= 0 & !is.na(angle_deg)) {
        angle_deg = angle_deg + 360
      } else if (DX <= 0 & DY <= 0 & !is.na(angle_deg)) {
        angle_deg = angle_deg + 360
      }
        
      }, error= function(err) {print("not enough data_1--------->")
        
      }, finally = {
        
      })

      DF_people_speed_ID$angle_deg <- angle_deg
      
    } else {print("not enough data_2--------------->>>")}
    
    DF_people_speed <- rbind(DF_people_speed, DF_people_speed_ID) 

}   ### this closes the for loop of the "list_IDs"

  
# TAG <-as.character(paste0(DF$year, "_", DF$month, "_", DF$day, "_", time_of_day)[1])
TAG <-as.character(paste0(DF$year, "_", DF$month, "_", time_of_day)[1])


#####################################################
#####################################################
#####################################################
###--- SPEED plot with the right orientation---######
## convert data into a SpatialPolygon dataframe----##

cell_SIZE <- 2   ## (UNITS --> [m])
DF_people_speed$X <- (DF_people_speed$X)*7.2


if  (is.data.frame(DF_people_speed) == FALSE) {
  DF_people_speed =data.frame(X = 0,
                              Y = 0,
                              speed = 0)
} else {DF_c <- DF_people_speed %>%
  dplyr::select(X,Y, speed) }


DF_people_speed[is.na(DF_people_speed)] <- 0

## fill NA values with 0
DF_c[is.na(DF_c)] <- 0
## remove NAs in the speed column
DF_c <- DF_c[!is.na(DF_c$speed),]

#####-----> ####################################################
#####-----> ####################################################
### Make a Spatial Frame

DF_sf <- st_as_sf(x = DF_people_speed,                         
               coords = c("X", "Y")) %>%
  st_set_crs(32632)  
  
xmax = max(DF_people_speed$X)
extent = st_sfc(st_polygon(list(rbind(c(xmin,ymin), c(xmax,ymin), c(xmax,ymax),
                                      c(xmin, ymax), c(xmin,ymin)))))

## set CRS
extent <- extent %>% 
  st_set_crs(st_crs(DF_sf))


## define a regular grid of 2 meter resolution and add an Id to each cell
GRID <- st_make_grid(extent, cellsize = 2, square = T) %>% 
  st_as_sf() %>%
  mutate(cell_ID = row_number())

GRID_speed <- st_intersection(DF_sf, GRID) 

GRID_speed <- as.data.frame(GRID_speed[, c("speed", "cell_ID")])
GRID_speed <- as.data.frame(GRID_speed[, c("cell_ID", "speed")])
## join SPEED values to the GRID
GRID_A <- GRID %>%
  left_join(GRID_speed, by = c("cell_ID"))
GRID_A <- GRID_A %>%
  group_by(cell_ID) %>%
  summarise(mean_speed = mean(speed, na.rm =T))

### replace NAs values with 0
GRID_A <- GRID_A %>%
  mutate_at(vars(mean_speed), ~replace_na(., 0))


if (mean(GRID_A$mean_speed, na.rm = T) > 0) {
  
plot_speed <- (GRID_A[, c("x", "mean_speed")]) %>% 
  st_transform("+proj=omerc  +lat_0=0 +lonc=0 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=0") %>%
  ggplot(aes()) +
  theme_bw() +
  geom_sf(aes(fill = mean_speed)) + 
  scale_fill_gradient2(low = "lightskyblue", mid = "white", high = "purple", midpoint = .02, 
                       trans = "sqrt", name = "[m/s]") +    
  # scale_fill_gradient2(low = "grey", mid = "white", high = "blue", name = "[m/s]") +
  ggtitle(paste0("mean speed: ", TAG)) + 
  coord_sf(datum = st_crs(32632)) 
  # scale_y_reverse() 
plot_speed

## save plot
dpi = 96
ggsave(filename=paste0(folder_speed_duca_aosta,TAG, ".png"),
       width = 700 / dpi, height = 650 / dpi,
       dpi = dpi)

}




