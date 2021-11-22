

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

## https://cran.r-project.org/web/packages/ggvoronoi/vignettes/ggvoronoi.html
# https://r-charts.com/part-whole/voronoi-diagram/

setwd("D:/Federico/CityFLows/output_files/")
folder_speed_duca_aosta <- "D:/Federico/CityFLows/output_files/speed_plots_Duca_Aosta/"
folder_speed_lato_taxi <- "D:/Federico/CityFLows/output_files/speed_plots_lato_taxi/"
folder_speed_lato_biciclette <- "D:/Federico/CityFLows/output_files/speed_plots_lato_biciclette/"


## load walking people data:
### Piazza Duca d'Aosta Milano St. Centrale
DF <- read.csv('D:/Federico/CityFLows/output_files/MILANOCENTRALE3_from_2021-10-06__14h0min_to_2021-10-12__23h59min.txt')[-1]
DF_NEW <- read.csv('D:/Federico/CityFLows/output_files/MILANO_3_from_2021-10-15__3h0min_to_2021-10-21__12h59min.txt')[-1]


### Milano St. Centrale, Piazza Lato Taxi
# DF <- read.csv('D:/Federico/CityFLows/output_files/MILANOCENTRALE2_from_2021-10-06__13h0min_to_2021-10-12__22h41min.txt')[-1]
# DF_NEW <- read.csv('D:/Federico/CityFLows/output_files/MILANO_2_TAXI_from_2021-10-15__16h0min_to_2021-10-21__0h30min.txt')[-1]


### Milano St. Centrale, Piazza Lato Biciclette
# DF <- read.csv('D:/Federico/CityFLows/output_files/MILANOCENTRALE1_from_2021-10-06__14h0min_to_2021-10-12__23h59min.txt')[-1]
# DF_NEW <- read.csv('D:/Federico/CityFLows/output_files/MILANO_1_BICI_from_2021-10-15__3h0min_to_2021-10-21__12h59min.txt')[-1]


### concatenate the two datasets and remove duplicates
DF <- rbind(DF, DF_NEW)
# remove duplicates rows
DF <- DF[!duplicated(DF), ]

### transform cm into meters
DF$X <- (DF$X)/100
DF$Y <- (DF$Y)/100

####--setup customized borders to make plos of the same size --####
xmin = min(DF$X)
ymin = min(DF$Y)
xmax = max(DF$X)
ymax = max(DF$Y)


##---> make an offset of all the coordinates(no negatives!!)
DF$X <- DF$X - xmin
DF$Y <- DF$Y - ymin

xmin = min(DF$X)
ymin = min(DF$Y)
xmax = max(DF$X)
ymax = max(DF$Y)
ymax = 80 ## only for Piazza Duca Aosta

# people <- DF[, c("timedate", "ID", "X", "Y")]
## change datetime format
# DF_people$timedate <- as.POSIXct(DF_people$timedate)

remove(DF_NEW)

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
    ## initialize an empty dataframe to gather all IDs and their mean speed

    
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
          
          
  #       }
  #     }
  #   }
  # }
          
          ## select unique ID by  timestamp and DIST
          DF_people <- distinct(DF_people, timedate, ID, X, Y)
          
          ## get list of all the IDs in the current timeslot
          list_IDs <- unique(DF_people$ID)
          
          
          ## build a new dataframe DF_people including speeds associated to each position
          DF_people_speed = NULL
     
          
          for (i in list_IDs) {
            # print(paste0("ID: ",i))
            distance <- 0
            DF_people_speed_ID = NULL
            
            ## select one trajectory
            DF_a <- DF_people %>%
              filter(ID == i)
            
            ## quick plot
            p_t <- ggplot(DF_a) +
              geom_point(aes(X,Y,color=NULL)) 
              # scale_y_reverse()
            p_t
            
            
            DF_a$timedate <- as.POSIXct(DF_a$timedate)
            ## add row number
            DF_a$index <- 1:nrow(DF_a)
            
            DF_a <- DF_a %>%
              dplyr::mutate(time = format(ymd_hms(as.character(DF_a$timedate, tz = "CET")),'%H:%M:%S'))
            DF_a <- as.data.frame(DF_a)
    
            if (nrow(DF_a) > 1) {
              
              ## compute speed without curvature (UNITS --> [m/s]), principal movement speed
              max_timedate <- max(strptime(DF_a[,"time"],format="%H:%M:%S"))
              min_timedate <-  min(strptime(DF_a[,"time"],format="%H:%M:%S"))
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
                  
                  # ## calculate the speed alone the singe trajectory of each ID
                  # ###-->> this is also the AVERAGE SPEEED OVER the QUARTER of HOUR --####
                  # speed_trajectory_quarter <- distance / as.numeric(duration_trajectory)  
                  DF_speed <- data.frame(timedate = DF_a[j, "timedate"],
                                         ID = i,
                                         X = DF_a[j, "X"],
                                         Y = DF_a[j, "Y"],
                                         speed = 0)
                  
                  names(DF_speed) <- c("timedate", "ID", "X", "Y", "speed")
                  DF_people_speed_ID = rbind(DF_people_speed_ID, DF_speed)
                }
              }
              ## calculate the speed alone the singe trajectory of each ID
              ###-->> this is also the AVERAGE SPEEED OVER the QUARTER of HOUR --####
              speed_trajectory_quarter <- distance / as.numeric(duration_trajectory)
              DF_people_speed_ID$speed <- speed_trajectory_quarter
     
            } else {print("not enough data")}
            
            DF_people_speed <- rbind(DF_people_speed, DF_people_speed_ID) 
            ## build a dataframe with speed for each ID
            ID_speed <- data.frame(timedate = DF_a[j, "timedate"],
                                   ID = i,
                                   speed = speed_trajectory_quarter)
            all_ID_speed <- rbind(all_ID_speed, ID_speed)
          
          }
          
          ### ------------------------------------ ###########################
          # make for all trajectories and then make a SPATIAL INTERPOLATION ##
          ####################################################################
          
          
          
          tryCatch({
            ### stat speed [m/s]
            speed_STATS <- DF_people_speed %>%
              # group_by(ID) %>%
              summarise(mean_speed = mean(speed, na.rm =T),
                        max_speed = max(speed, na.rm =T),
                        stdv_speed = sd(speed, na.rm =T))
          }, error= function(err) {print("not enough data")
    
          }, finally = {
    
          })
          
          
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
        
        
        
       
       
        if  (is.data.frame(DF_people_speed) == FALSE) {
          DF_people_speed =data.frame(X = 0,
                                      Y = 0,
                                      speed = 0) 
        } else {DF_c <- DF_people_speed %>%
          dplyr::select(X,Y, speed) }
          
        
        
        ## fill NA values with 0
        DF_c[is.na(DF_c)] <- 0
        ## remove NAs in the speed column
        # DF_c <- DF_c[!is.na(DF_c$speed),]
    
        #####-----> ####################################################
        #####-----> ####################################################
        ### Make a Spatial Frame
        
        DF_sf <- st_as_sf(x = DF_c,                         
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
    
        GRID_speed <- st_intersection(DF_sf, GRID) 
        
        GRID_speed <- as.data.frame(GRID_speed[, c("speed", "cell_ID")])
        GRID_speed <- as.data.frame(GRID_speed[, c("cell_ID", "speed")])
        ## join SPEED values to the GRID
        GRID_A <- GRID %>%
          left_join(GRID_speed, by = c("cell_ID"))
        GRID_A <- GRID_A %>%
          group_by(cell_ID) %>%
          summarise(mean_speed = mean(speed))
        
        ### replace NAs values with 0
        GRID_A <- GRID_A %>%
          mutate_at(vars(mean_speed), ~replace_na(., 0))
        
        
        if (mean(GRID_A$mean_speed, na.rm = T) > 0) {
          
        plot_speed <- (GRID_A[, c("x", "mean_speed")]) %>% 
          st_transform("+proj=omerc  +lat_0=0 +lonc=0 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=0") %>%
          ggplot(aes()) +
          theme_bw() +
          geom_sf(aes(fill = mean_speed)) + 
          scale_fill_gradient2(low = "grey", mid = "white", high = "blue", midpoint = .02, trans = "sqrt", limits=c(0,1),
                               name = "[m/s]") +
          # scale_fill_gradient2(low = "grey", mid = "white", high = "blue", name = "[m/s]") +
          coord_sf(datum = st_crs(32632)) 
        plot_speed
        ## save plot
        dpi = 96
        ggsave(filename=paste0(folder_speed_duca_aosta,TAG, ".png"),
               width = 700 / dpi, height = 650 / dpi,
               dpi = dpi)
        # ggsave(filename=paste0(folder_speed_lato_taxi, TAG, ".png"),
        #        width = 700 / dpi, height = 650 / dpi,
        #        dpi = dpi)
        # ggsave(filename=paste0(folder_speed_lato_biciclette,TAG, ".png"),
        #        width = 700 / dpi, height = 650 / dpi,
        #        dpi = dpi)
        
        }
    
      }
      
    }
}
    
    

###---> save all speed values
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_duca_aosta_a.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_lato_taxi_a.csv"), row.names=FALSE)
# write.csv(all_ID_speed, file = paste0("D:/Federico/CityFLows/output_files/all_ID_speeds_lato_biciclette_a.csv"), row.names=FALSE)


