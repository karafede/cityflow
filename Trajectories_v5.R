

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
folder_speed_duca_aosta <- "D:/Federico/CityFLows/output_files/speed_plots_Duca_Aosta/23April2022/"

## set forward motion speed
speed_FWD_motion <- 0.2  ## units [m/s]

###-----------------****** IDs generated from FEDERICO KARAGULIAN from video processing with OpenCV -----******########
# DF <- read.csv('D:/Federico/CityFLows/objectdetection/IDs_meters_Piazza_Duca_Aosta_26Nov2021.csv')[-1]
# DF_a <- read.csv('D:/Federico/CityFLows/objectdetection/01April2022_PiazzaDucaAosta_meters.csv')[-1]
# DF_b <- read.csv('D:/Federico/CityFLows/objectdetection/02April2022_PiazzaDucaAosta_meters.csv')[-1]
# DF_c <- read.csv('D:/Federico/CityFLows/objectdetection/03April2022_PiazzaDucaAosta_meters.csv')[-1]
# DF_d <- read.csv('D:/Federico/CityFLows/objectdetection/04April2022_PiazzaDucaAosta_meters.csv')[-1]
# DF_e <- read.csv('D:/Federico/CityFLows/objectdetection/05April2022_PiazzaDucaAosta_meters.csv')[-1]
# DF_f <- read.csv('D:/Federico/CityFLows/objectdetection/06April2022_PiazzaDucaAosta_meters.csv')[-1]
# DF_g <- read.csv('D:/Federico/CityFLows/objectdetection/07April2022_PiazzaDucaAosta_meters.csv')[-1]
# DF_h <- read.csv('D:/Federico/CityFLows/objectdetection/08April2022_PiazzaDucaAosta_meters.csv')[-1]
# DF_i <- read.csv('D:/Federico/CityFLows/objectdetection/19April2022_PiazzaDucaAosta_meters.csv')[-1]
# DF_l <- read.csv('D:/Federico/CityFLows/objectdetection/20April2022_PiazzaDucaAosta_meters.csv')[-1]
# DF_m <- read.csv('D:/Federico/CityFLows/objectdetection/21April2022_PiazzaDucaAosta_meters.csv')[-1]
# DF_n <- read.csv('D:/Federico/CityFLows/objectdetection/22April2022_PiazzaDucaAosta_meters.csv')[-1]
DF <- read.csv('D:/Federico/CityFLows/objectdetection/23April2022_PiazzaDucaAosta_meters.csv')[-1]



DF <- DF %>%
  dplyr::select(X_m, Y_m, ID, timestamp)

names(DF) <- c("X", "Y", "ID", "timedate")


# remove duplicates rows
DF <- DF[!duplicated(DF), ]

### APPROXIMATION to transform PIXELS into METERS
### Piazza DUCA Aosta Milano
DF$X <- (((DF$X)/10)/2)*0.09   # 0.15
DF$Y <- ((DF$Y)/10)*0.7  # 0.75   # 1.1


## count recurrency of IDs
COUNTS_ID <- DF %>%
  group_by(ID) %>%
  summarise(count_IDs = length(ID))

# COUNTS_ID <- COUNTS_ID %>%
#   filter( count_IDs > 10 &
#            count_IDs < 500)

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

####----> loop over days
all_ID_speed = NULL



for (d in 1:length(day_list)) {
  print(paste0("day: ", day_list[d]))
  DF_day <- DF %>%
    filter(day == day_list[d])

    ### ---- set list of quarter within I can do the averages of the densities
    quarters <- list(0, 15, 30, 45, 60)
    ## initialize an empty data frame to gather all IDs and their mean speed

    
    for (q in 1:length(quarters)) {
      print(q)
      if (q < length(quarters)) { 
        DF_people_quarter <- DF_day %>%
          group_by(day, hour, minute) %>%
          filter(minute >= quarters[q] & minute <= quarters[q+1])
      } else {
        DF_people_quarter <- DF_day %>%
          group_by(day, hour, minute) %>%
          filter(minute >= quarters[q-1] & minute <= quarters[q])
      } 
      
      
      DF_summary <- DF_people_quarter %>%
        group_by(year, month, day, hour, minute) %>%
        summarise(minute = unique(minute), .groups = 'drop')
      
      
      year_list_quarter <- as.list(DF_summary$year)
      month_list_quarter <- as.list(DF_summary$month)
      day_list_quarter <- as.list(DF_summary$day)
      hour_list_quarter <- as.list(DF_summary$hour)
      minute_list_quarter <- as.list(DF_summary$minute)
      
      list_hours <- unique(hour_list_quarter)
      
    
      for (h in 1:length(list_hours)) {
        print(list_hours[h])
        DF_people_quarter_a <- DF_people_quarter %>%
          filter(hour == list_hours[h])
        
        DF_summary_a <- DF_people_quarter_a %>%
          group_by(year, month, day, hour, minute) %>%
          summarise(minute = unique(minute), .groups = 'drop')
        
        year_list_quarter_a <- as.list(DF_summary_a$year)
        month_list_quarter_a <- as.list(DF_summary_a$month)
        day_list_quarter_a <- as.list(DF_summary_a$day)
        hour_list_quarter_a <- as.list(DF_summary_a$hour)
        minute_list_quarter_a <- as.list(DF_summary_a$minute)
       
        
        ### loop over the first slot of 15 minute data
        for (k in 1:length(unique(minute_list_quarter_a))) {
          print(k)
          DF_people <- DF_people_quarter_a %>%
            filter(day == day_list_quarter_a[k] && minute == minute_list_quarter_a[k])
          TAG <- paste0(year_list_quarter_a[k], "_", month_list_quarter_a[k], "_", day_list_quarter_a[k],"__", list_hours[h],"_",
                        minute_list_quarter_a[k])
          print(TAG)
          
          ## select unique ID by  timestamp and DIST
          DF_people <- distinct(DF_people, timedate, ID, X, Y)
          
          
          
          ## build a new data frame DF_people including speeds associated to each position
          DF_people_speed = NULL
          
          # DF_people <- DF
          
          ## get list of all the IDs in the current timeslot
          list_IDs <- unique(DF_people$ID)
          # i <- 5
          
        for (i in list_IDs) {
            # print(paste0("ID: ",i))
            distance <- 0
            DF_people_speed_ID = NULL
            
            
            ## select one trajectory
            DF_a <- DF_people %>%
              filter(ID == i)
            
            ## ID = 1771
            ## ID 849
            ## ID = 1811
            
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
            ## keep y always increasing....
            
            DF_a_rectified <- NULL
            for (j in 1:nrow(DF_a))  {
              if (j != nrow(DF_a)) {
              if (DF_a$Y[j+1] > DF_a$Y[j]) {
                print("yes") 
                DF_a[j, ]
                DF_a_rectified = rbind(DF_a_rectified, DF_a[j, ])
                } else {print("no")}
              } 
            }
              
            
            DF_a_rectified <- DF_a_rectified[order(DF_a_rectified$timedate),]  
            DF_a_rectified$X <- (DF_a_rectified$X)*7.2
            
            ## remove 3 minutes.....
            DF_a_rectified$timedate <- ymd_hms((DF_a_rectified$timedate))
            DF_a_rectified$timedate <- DF_a_rectified$timedate-60*3
            
            p_t <- ggplot(DF_a_rectified, aes(x = X, y = Y, colour = ID, label=ID, group = ID)) +
              theme_bw() +
              geom_point(aes(X,Y,color=NULL, fill = ID), size = 2) +
              geom_path(aes(X,Y,color=NULL, fill = ID)) +
              geom_text(aes(label=  paste0("0",round(hour(timedate), 2), ":", 
                                           paste0("0",round(minute(timedate), 0)), ":", 
                                           round( second(timedate), 2))),
                                           hjust=0, vjust=0, size = 5, colour="black") +
              theme(axis.title.y = element_text(face="bold", colour="black", size=18),
                    axis.text.y  = element_text(angle=0, vjust=0.5, size=18)) +
              theme(axis.title.x = element_text(face="bold", colour="black", size=18),
                    axis.text.x  = element_text(angle=0, vjust=0.5, size=18)) +
              theme(legend.position = "none")
            p_t
            
            dpi = 96
            ggsave(filename=paste0("D:/Federico/CityFLows/output_files/trajectories/ID_1771_23April2022.png"),
                   width = 1200 / dpi, height = 650 / dpi,
                   dpi = dpi)
            
            
            
      
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
            ## build a dataframe with speed for each ID
            all_ID_speed <- rbind(all_ID_speed, DF_people_speed_ID)
            
        }   ### this closes the for loop of the "list_IDs"

          
      }  
        
        TAG <- paste0(year_list_quarter_a[k], "_", month_list_quarter_a[k], "_", day_list_quarter_a[k],"__", list_hours[h],
                      "h", quarters[q], "min")
        print(paste0("TAG picture ---------: ", TAG))
        
        
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
          scale_fill_gradient2(low = "grey", mid = "white", high = "blue", midpoint = .02, 
                               trans = "sqrt", name = "[m/s]") +    
          # scale_fill_gradient2(low = "grey", mid = "white", high = "blue", name = "[m/s]") +
          coord_sf(datum = st_crs(32632)) 
          # scale_y_reverse() 
        plot_speed
        
        ## save plot
        dpi = 96
        ggsave(filename=paste0(folder_speed_duca_aosta,TAG, ".png"),
               width = 700 / dpi, height = 650 / dpi,
               dpi = dpi)
        
        }

      }
      
    }  
}
    

#####---> save all speed values
all_ID_speed$X <- (all_ID_speed$X)*7.2
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_01April2022.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_02April2022.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_03April2022.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_04April2022.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_05April2022.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_06April2022.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_07April2022.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_08April2022.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_19April2022.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_20April2022.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_21April2022.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_22April2022.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_23April2022.csv"), row.names=FALSE)


# all_ID_speed <- read.csv("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_angles_02April2022.csv")

