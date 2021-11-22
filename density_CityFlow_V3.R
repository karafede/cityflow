


rm(list = ls())

library(ggvoronoi)
library(dplyr)
library(deldir)
library(gridGraphics)
library(sf)
library(lubridate)
library(ggplot2)
library(tidyr)
library(lwgeom)
library(scales)


options(warn=-1)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

## https://cran.r-project.org/web/packages/ggvoronoi/vignettes/ggvoronoi.html
# https://r-charts.com/part-whole/voronoi-diagram/

setwd("D:/Federico/CityFLows/output_files/")
folder_density_standard_duca_aosta <- "D:/Federico/CityFLows/output_files/standard_density_plots_duca_aosta/"
folder_density_voronoi_duca_aosta <- "D:/Federico/CityFLows/output_files/voronoi_density_plots_duca_aosta/"
folder_density_voronoi_I_duca_aosta <- "D:/Federico/CityFLows/output_files/voronoi_I_density_plots_duca_aosta/"
folder_density_Number_duca_aosta <- "D:/Federico/CityFLows/output_files/Cumulative_Number_plots_duca_aosta/"


folder_density_standard_lato_taxi <- "D:/Federico/CityFLows/output_files/standard_density_plots_lato_taxi/"
folder_density_voronoi_lato_taxi <- "D:/Federico/CityFLows/output_files/voronoi_density_plots_lato_taxi/"
folder_density_voronoi_I_lato_taxi <- "D:/Federico/CityFLows/output_files/voronoi_I_density_plots_lato_taxi/"
folder_density_Number_lato_taxi <- "D:/Federico/CityFLows/output_files/Cumulative_Number_plots_lato_taxi/"


folder_density_standard_lato_biciclette <- "D:/Federico/CityFLows/output_files/standard_density_plots_lato_biciclette/"
folder_density_voronoi_lato_biciclette <- "D:/Federico/CityFLows/output_files/voronoi_density_plots_lato_biciclette/"
folder_density_voronoi_I_lato_biciclette <- "D:/Federico/CityFLows/output_files/voronoi_I_density_plots_lato_biciclette/"
folder_density_Number_lato_biciclette <- "D:/Federico/CityFLows/output_files/Cumulative_Number_plots_lato_biciclette/"


## load walking people data:
### Piazza Duca d'Aosta Milano St. Centrale
DF <- read.csv('D:/Federico/CityFLows/output_files/MILANOCENTRALE3_from_2021-10-06__14h0min_to_2021-10-12__23h59min.txt')[-1]
DF_NEW <- read.csv('D:/Federico/CityFLows/output_files/MILANO_3_from_2021-10-15__3h0min_to_2021-10-24__11h59min.txt')[-1]

# DF <- read.csv('D:/Federico/CityFLows/output_files/MILANO_3_from_2021-10-10__17h0min_to_2021-10-10__18h59min.txt')[-1]


### Milano St. Centrale, Piazza Lato Taxi
# DF <- read.csv('D:/Federico/CityFLows/output_files/MILANOCENTRALE2_from_2021-10-06__13h0min_to_2021-10-12__22h41min.txt')[-1]
# DF_NEW <- read.csv('D:/Federico/CityFLows/output_files/MILANO_2_TAXI_from_2021-10-15__16h0min_to_2021-10-24__7h59min.txt')[-1]


### Milano St. Centrale, Piazza Lato Biciclette
# DF <- read.csv('D:/Federico/CityFLows/output_files/MILANOCENTRALE1_from_2021-10-06__14h0min_to_2021-10-12__23h59min.txt')[-1]
# DF_NEW <- read.csv('D:/Federico/CityFLows/output_files/MILANO_1_BICI_from_2021-10-15__3h0min_to_2021-10-24__4h59min.txt')[-1]


### concatenate the two datasets and remove duplicates
DF <- rbind(DF, DF_NEW)
# remove duplicates rows
DF <- DF[!duplicated(DF), ]

remove(DF_NEW)

DF <- DF[, c("timedate", "ID", "X", "Y")]
## change datetime format
DF$timedate <- as.POSIXct(DF$timedate)
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


DF <- DF %>%
  mutate(year = year(timedate),
         month = month(timedate),
         day = day(timedate),
         hour = hour(timedate),
         minute = minute(timedate))


day_list <- as.list(DF$day)
day_list <- unique(day_list)


####----> loop over days
## initialize an empty dataframe to gather all IDs and their different type of densities
all_ID_density = NULL


for (d in 1:length(day_list)) {
  print(paste0("day: ", day_list[d]))
  DF_day <- DF %>%
    filter(day == day_list[d])


      ### ---- set list of quarter within I can do the averages of the densities
      quarters <- list(0, 15, 30, 45, 60)

       
      ##---- get slots of data every 15 minutes and make an AVERAGE of DENSITY by m^2
      ##----- initialize empty vectors for each type of density
      # SUM_N <- 0
      
      # i <- 5
      
      
      for (i in 1:length(quarters)) {
        print(i)
        if (i < length(quarters)) { 
          DF_people_quarter <- DF_day %>%
            group_by(day, hour, minute) %>%
            filter(minute >= quarters[i] & minute <= quarters[i+1])
        } else {
          DF_people_quarter <- DF_day %>%
            group_by(day, hour, minute) %>%
            filter(minute >= quarters[i-1] & minute <= quarters[i])
        } 
        
        ##----- get slots of 15 minutes data 
        # DF_people_quarter <- DF %>%
        #   group_by(day, hour, minute) %>%
        #   filter(minute >= 0 & minute <=first_quarter)
        
        DF_summary <- DF_people_quarter %>%
          group_by(year, month, day, hour, minute) %>%
          summarise(minute = unique(minute), .groups = 'drop')
        
        
        year_list_quarter <- as.list(DF_summary$year)
        month_list_quarter <- as.list(DF_summary$month)
        day_list_quarter <- as.list(DF_summary$day)
        hour_list_quarter <- as.list(DF_summary$hour)
        minute_list_quarter <- as.list(DF_summary$minute)
        
        list_hours <- unique(hour_list_quarter)
        
        tryCatch({
          
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
          
          ##----- initialize empty vectors for each type of density
          SUM_Dv <- 0
          SUM_DvI <- 0
          SUM_Ds <- 0
          SUM_N <- 0
      
          ### loop over the first slot of 15 minute data
            for (j in 1:length(unique(minute_list_quarter_a))) {
              print(j)
              DF_people <- DF_people_quarter_a %>%
                filter(day == day_list_quarter_a[j] & minute == minute_list_quarter_a[j])
              TAG <- paste0(year_list_quarter_a[j], "_", month_list_quarter_a[j], "_", day_list_quarter_a[j],"__", list_hours[h],"_",
                            minute_list_quarter_a[j])
              print(TAG)
              
#       }
#     }
#   }
# }
     
              ## select unique ID by  timestamp and DIST
              DF_people <- distinct(DF_people, timedate, ID, X, Y)
              
              ## get list of all the IDs in the current timeslot
              list_IDs <- unique(DF_people$ID)
              
             
              ## get unique ID at a fixed time t0
              DF_people_t0 <- DF_people %>%
                group_by(ID) %>%
                summarise(timedate = min(timedate))
              
                
              if (nrow(DF_people_t0) > 2) {  
                ## join X, Y
                DF_people_t0 <- DF_people_t0 %>%
                  merge(DF_people, by = c("ID", "timedate"))
                ## remove duplicated
                DF_people_t0 <- DF_people_t0[!duplicated(DF_people_t0), ]
                
                
                ## remove duplicate values of X,Y
                DF_people_t0 <- DF_people_t0 %>%
                  distinct(timedate, X, Y,.keep_all = T)
                
                # TAG_HOUR <- unique(format(DF_people_t0$timedate,format="%Y_%m_%d, %H"))
                
                # ## quick plot....for each fixed time t0....
                # p_t0 <- ggplot(DF_people_t0) +
                #   geom_point(aes(X,Y,color=NULL)) +
                #   # scale_y_reverse() +
                #   theme(legend.position = "none") 
                # # coord_flip()
                # p_t0
                
                
                # p_t0 <- ggplot(DF_people_t0,aes(X,Y)) +
                #   stat_voronoi(geom="path") +
                #   geom_point(size = 0.7) 
                # # scale_y_reverse() 
                # #  coord_flip()
                # p_t0
                
                # ## save plot
                # png(paste0("map_points.jpg"),
                # width = 1800, height = 1050, units = "px", pointsize = 30,
                # bg = "white", res = 150)
                # print(p_t0)
                # dev.off()
                
                
                DF_people_t0 <- as.data.frame(DF_people_t0)
                
                pts <- tibble(datetime = DF_people_t0$timedate, longitude = DF_people_t0$X, latitude = DF_people_t0$Y, ID = DF_people_t0$ID) %>%
                  st_as_sf(coords = c('longitude', 'latitude')) %>%
                  st_set_crs(32632)
                
                
                vor <- st_voronoi(st_combine(pts))
                # voronoi of pts
                vor_poly <- st_collection_extract(vor)
                # plot(pts, col = 'blue', pch = 16, cex = .01)
                # plot(vor_poly, add = T, fill = NA)
                
                # png(filename="raw_point_voroni.png")
                # plot(pts, col = 'blue', pch = 16, cex = .01)
                # plot(vor_poly, add = T, fill = NA)
                # dev.off()
                
                # bbox <- st_bbox(pts)
                # xmin = bbox[1]
                # ymin = bbox[2]
                # xmax = bbox[3]
                # ymax = bbox[4]
                
                
                ##--------- Overlay a rectangular GRID ----###########################
                ######################################################################
                
                ## design a square around the extent of the point and crop sfc_POLYGON 
                box = c(xmin = xmin, ymin= ymin, xmax= xmax, ymax= ymax)
                
                # box = bbox
                # plot(st_crop(st_make_valid(vor_poly), box))
                # # plot(st_crop(vor_poly, box))
                # plot(pts, add = T, pch = 16, cex = .01)
                
                ###---> crop the polyhon over the box!!!!
                vor_poly <- st_crop(st_make_valid(vor_poly), box)
                
                # png(filename="raw_point_voroni_cropped.png")
                # plot(st_crop(st_make_valid(vor_poly), box))
                # plot(pts, add = T, pch = 16, cex = .01)
                # dev.off()
                
                extent = st_sfc(st_polygon(list(rbind(c(xmin,ymin), c(xmax,ymin), c(xmax,ymax),
                                                      c(xmin, ymax), c(xmin,ymin)))))
                
                ## set CRS
                extent <- extent %>% 
                  st_set_crs(st_crs(vor_poly))
                
                ## define a regular grid of 2 meter resolution and add an Id to each cell
                GRID <- st_make_grid(extent, cellsize = 2, square = T) %>% 
                  st_as_sf() %>%
                  mutate(cell_ID = row_number())
                
                
                ## add and ID for each Voronoi cell
                vor_poly <- vor_poly %>% 
                  st_as_sf() %>%
                  mutate(vor_ID = row_number())
                
                
                # plot(st_crop(st_make_valid(vor_poly), box),add = TRUE, col = 'white')
                # plot(pts, add = TRUE, pch = 16, cex = .01)
                # plot(st_make_grid(GRID, cellsize = 2, square = T), add = TRUE)
                
                cell_AREA <- 2*2 ## m^2
                
                ## get number of point within each GRID cell.....
                N_people <- st_intersection(GRID ,pts) 
                N_people <- as.data.frame(N_people)
                
                ## remove same ID appearing multiple times within the same cell 
                # N_people <- N_people %>%
                #   distinct(cell_ID,  datetime, ID)
                
                #### ---------------> #######################################
                ## count how many time frames we have in each cell_ID
                grouped_frames <- N_people %>%
                  group_by(cell_ID) %>%
                  summarise(N_frames_cell = length(datetime),
                            timedate = min(datetime))


                ## count ho many ID we have in each cell_ID
                grouped_ID <- N_people %>%
                  group_by(cell_ID,
                           ID) %>%
                  summarise(N_ID = length(ID),
                            timedate = min(datetime))

                ## merge number of frames with number of ID for each cell_ID
                grouped_frames <- grouped_frames %>%
                  left_join(grouped_ID, by = c("cell_ID", "timedate"))

                ## weight each ID for its number of recurrencies in each cell_ID
                grouped_frames$N <- (grouped_frames$N_ID)/(grouped_frames$N_frames_cell)
                N_people <- grouped_frames %>%
                  group_by(cell_ID) %>%
                  summarise(N = sum(N),
                            timedate = min(timedate))
                N_people <- as.data.frame(N_people)
                #### <--------------- #######################################
                
                
                # grouped_frames <- N_people %>%
                #   group_by(cell_ID,
                #            ID) %>%
                #   summarise(N_ID_cell_frame = length(datetime),
                #             timedate = min(datetime))
                # N_people <- grouped_frames %>%
                #   group_by(cell_ID) %>%
                #   summarise(N = 0.5*length(cell_ID),
                #             timedate = min(timedate))
                # N_people <- as.data.frame(N_people)
                
                
              
                # N_people <- N_people %>%
                #   group_by(cell_ID) %>%
                #   summarise(N = length(cell_ID),
                #             timedate = min(datetime))
                # N_people <- as.data.frame(N_people)
                
                
             
                ## get area of each Voronoi ID
                area_vor_cell <- st_intersection(GRID ,st_make_valid(vor_poly)) %>% 
                  group_by(vor_ID) %>%  
                  summarise(geom = st_union(x)) %>%
                  mutate(geom = st_sfc(geom),
                         area = st_area(geom))
                
                
                ## intersect "extent" with "Voronoi polygon"
                intersect_ID <- st_intersection(GRID, st_make_valid(vor_poly))
                intersect_ID <- as.data.frame(intersect_ID)
                
                
                ## get all geometries of the Voronoi cells assigned to each GRID cell
                intersect_ID <- intersect_ID %>%
                  dplyr::select(vor_ID,
                                cell_ID)
                intersect_ID <- intersect_ID %>%
                  left_join(area_vor_cell, by = c("vor_ID"))
                
                
                ################################################
                ###------ DENSITY CALCULATIONS----##############
                ################################################
                
                ##########################
                ###-- FIRST METHOD----####
                
                ## define a DENSITY DISTRIBUTION for all person falling in each GRID cell (cell_ID)
                ## each person is associated to a Voronoi cell
      
                
                # ### if AREA of Voroni cell > 2*cell_AREA then use cell_AREA
                # density_distr <- intersect_ID %>%
                #   group_by(cell_ID) %>%
                #   summarise( if(as.numeric(intersect_ID$area) <= cell_AREA) {
                #     density_dist = sum(1/area)
                #   } else {density_dist = sum(1/cell_AREA)})
                # names(density_distr) <- c("cell_ID", "density_dist")
                
                
                density_distr <- intersect_ID %>%
                  group_by(cell_ID) %>%
                  summarise(density_dist = sum(1/area))
                
                density_distr <- density_distr %>%
                  left_join(N_people, by = c("cell_ID"))
                # density_distr$N[is.na(density_distr$N)] <- 1
                Dv <- density_distr %>%
                  # mutate(density_v = (as.numeric(density_dist)/N)/cell_AREA)
                  mutate(density_v = (as.numeric(density_dist)*N)/cell_AREA)
                
                
                ### find Voronoi Geometries falling inside each grid CELL
                Voronoi_GRID <- Dv %>%
                  left_join(intersect_ID, by = c("cell_ID")) %>%
                  group_by(cell_ID) %>%
                  summarise(x = st_union(geom)) %>%
                  mutate(x = st_sfc(x)) %>%
                  st_as_sf()
                
                ### get the union of all VORONOI geometries ovelaying the grid CELLS
                Voronoi_Dv <- Voronoi_GRID %>%
                  left_join(Dv[, c("cell_ID", "density_v")], by = c("cell_ID"))
                
                
                ###---> sum all densities over 15 minutes
                # SUM_Dv <- Voronoi_Dv$density_v + SUM_Dv
                
                
                ####---- SECOND METHOD----############################
                ## get number of people in each GRID cell (cell_ID)
                
                intersect_ID_DvI <- intersect_ID %>%
                  left_join(N_people[, c("cell_ID", "N")], by = c("cell_ID"))
                ## remove row with NA values in the column "N"
                intersect_ID_DvI <- intersect_ID_DvI %>% drop_na(N)
                
                DvI <- intersect_ID_DvI %>%
                  group_by(cell_ID) %>%
                  summarise(total_area_vor_cell = sum(area)) 
                
                ## join number of people with DvI
                DvI <- DvI %>%
                  left_join(N_people[, c("cell_ID", "N", "timedate")], by = c("cell_ID"))
                
                
                DvI <- DvI %>%
                  mutate(density_vI = as.numeric(N/total_area_vor_cell))
                
                
                ## calculate "classic" measure of density (Number of people / GRID CELL AREA)
                DvI$Ds <- DvI$N/cell_AREA
                
                
                ## join density values to the GRID
                GRID_A <- GRID %>%
                  left_join(DvI, by = c("cell_ID"))
                
                
                Dv <- as.data.frame(Dv)
                GRID_B <- GRID_A %>%
                  left_join( Dv[, c("cell_ID", "density_v")], by = c("cell_ID"))
                
                GRID_B <- GRID_B %>%
                  mutate_at(vars(N, density_v, density_vI,  Ds), ~replace_na(., 0))
                
                GRID_B <- GRID_B %>%
                  mutate(density_vI = as.numeric(GRID_B$density_vI))
                
                
                ###---> sum all densities over 15 minutes
                SUM_DvI <- GRID_B$density_vI + SUM_DvI
                SUM_Dv <- GRID_B$density_v + SUM_Dv
                SUM_Ds <- GRID_B$Ds + SUM_Ds
                ##---> cumulative number of people
                SUM_N <- GRID_B$N + SUM_N
              } else {
                
                print("not enough data")
                SUM_DvI <- SUM_DvI
                SUM_Dv <- SUM_Dv
                SUM_Ds <- SUM_Ds
                SUM_N <- SUM_N} 
                
                
            }
       
          ####----- replace all SUMMED "densities" with the average value by dividing by the numbers of loops
          # Voronoi_Dv$density_v <-  SUM_Dv/length(minute_list_quarter_a)
          # GRID_B$density_v <- SUM_Dv/length(minute_list_quarter_a)
          # GRID_B$density_vI <- SUM_DvI/length(minute_list_quarter_a)
          # GRID_B$Ds <- SUM_Ds/length(minute_list_quarter_a)
          GRID_B$density_v <- SUM_Dv/16
          GRID_B$density_vI <- SUM_DvI/16
          GRID_B$Ds <- SUM_Ds/16
          GRID_B$N <- SUM_N
          
          TAG <- paste0(year_list_quarter_a[j], "_", month_list_quarter_a[j], "_", day_list_quarter_a[j],"__", list_hours[h],
                        "h", quarters[i], "min")
      
          print(paste0("TAG picture ------->: ", TAG))
          
          #####################################################
          #####################################################
          #####################################################
          ###--- density plot with the right orientation---####
          
          GRID_B$density_v <- (GRID_B$density_v)/ mean((GRID_B$density_v)/(GRID_B$Ds), na.rm = T)
          GRID_B$density_vI <- (GRID_B$density_vI)/ mean((GRID_B$density_vI)/(GRID_B$Ds), na.rm = T)
          GRID_B$density_vI[is.na(GRID_B$density_vI)] <- 0
          GRID_B$density_v[is.na(GRID_B$density_v)] <- 0
          
          
          GRID_B$centroid <- st_centroid(GRID_B$x)
          XY <- st_coordinates(GRID_B$centroid)
          GRID_B <- cbind(GRID_B, XY)
          density <- as.data.frame(GRID_B)
          density <- density %>%
            dplyr::select(N,
                   density_vI,
                   density_v,
                   Ds,
                   timedate,
                   X, Y)
          all_ID_density <- rbind(density, all_ID_density)
          ## remove rows with NAs
          # all_ID_density <- all_ID_density[complete.cases(all_ID_density), ]
          
          
          if (mean(GRID_B$Ds, na.rm = T) > 0) {
            
          
          ## --> Standard_Density
          Standard_density_plot <- (GRID_B[, c("x", "Ds")]) %>% 
            # st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-10 +lat_0=40") %>%
            st_transform("+proj=omerc  +lat_0=0 +lonc=0 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=0") %>%
            ggplot(aes()) +
            theme_bw() +
            geom_sf(aes(fill = Ds)) + 
            scale_fill_gradient2(low = "blue", mid = "white", high = "red4", midpoint = .005, trans = "sqrt",
                                  name =expression( paste("[#/", m^2, "]")) ) +  # limits=c(0,0.5)
            coord_sf(datum = st_crs(32632)) 
          # theme(legend.position = "none") 
          Standard_density_plot
          ## save plot
          dpi = 96
          ggsave(filename=paste0(folder_density_standard_duca_aosta, TAG, ".png"),
                 width = 700 / dpi, height = 650 / dpi,
                 dpi = dpi)
          # ggsave(filename=paste0(folder_density_standard_lato_taxi,TAG, ".png"),
          #        width = 700 / dpi, height = 650 / dpi,
          #        dpi = dpi)
          # ggsave(filename=paste0(folder_density_standard_lato_biciclette,TAG, ".png"),
          #        width = 700 / dpi, height = 650 / dpi,
          #        dpi = dpi)
          
          }
          
          ####---> colors should follow Voronoi cell shapes---#######################
          # DV_plot <- (Voronoi_Dv[, c("x", "density_v")]) %>% 
          #   st_transform("+proj=omerc  +lat_0=0 +lonc=0 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=0") %>%
          #   ggplot(aes()) +
          #   theme_bw() +
          #   geom_sf(aes(fill = density_v)) + 
          #   scale_fill_gradient2(low = "grey", mid = "white", high = "brown", midpoint = .02, trans = "sqrt") +
          #   coord_sf(datum = st_crs(32632)) 
          # # theme(legend.position = "none") 
          # DV_plot
          # ## save plot
          # dpi = 96
          # ggsave(filename=paste0(folder_density_standard,"Voronoi_Density_",TAG, ".png"),
          #        width = 700 / dpi, height = 650 / dpi,
          #        dpi = dpi)
          
          if (mean(GRID_B$density_v, na.rm = T) > 0) {
            
          ### --> Voronoi_Density
          Voronoi_Density_plot <- (GRID_B[, c("x", "density_v")]) %>%
            # st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-10 +lat_0=40") %>%
            st_transform("+proj=omerc  +lat_0=0 +lonc=0 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=0") %>%
            ggplot(aes()) +
            theme_bw() +
            geom_sf(aes(fill = density_v)) +
            scale_fill_gradient2(low = "blue", mid = "white", high = "red4", midpoint = .005, trans = "sqrt",
                                 name =expression( paste("[#/", m^2, "]"))) +   #limits=c(0,0.5)
            # scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits=c(0,1)) +
            coord_sf(datum = st_crs(32632))
          Voronoi_Density_plot
          ## save plot
          dpi = 96
          ggsave(filename=paste0(folder_density_voronoi_duca_aosta,TAG, ".png"),
                 width = 700 / dpi, height = 650 / dpi,
                 dpi = dpi)
          # ggsave(filename=paste0(folder_density_voronoi_lato_taxi,TAG, ".png"),
          #        width = 700 / dpi, height = 650 / dpi,
          #        dpi = dpi)
          # ggsave(filename=paste0(folder_density_voronoi_lato_biciclette,TAG, ".png"),
          #        width = 700 / dpi, height = 650 / dpi,
          #        dpi = dpi)
          
          }
          
          
          if (mean(GRID_B$density_vI, na.rm = T) > 0) {
          
          ## --> Voronoi_Density_I
          Voronoi_Density_I_plot <- (GRID_B[, c("x", "density_vI")]) %>% 
            # st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-10 +lat_0=40") %>%
            st_transform("+proj=omerc  +lat_0=0 +lonc=0 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=0") %>%
            ggplot(aes()) +
            theme_bw() +
            geom_sf(aes(fill = density_vI)) + 
            scale_fill_gradient2(low = "blue", mid = "white", high = "red4", midpoint = .005, trans = "sqrt",
                                 name =expression( paste("[#/", m^2, "]"))) +    #limits=c(0,0.5)
            # scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits=c(0,1)) +
            coord_sf(datum = st_crs(32632)) 
          Voronoi_Density_I_plot
          ## save plot
          dpi = 96
          ggsave(filename=paste0(folder_density_voronoi_I_duca_aosta, TAG, ".png"),
                 width = 700 / dpi, height = 650 / dpi,
                 dpi = dpi)
          # ggsave(filename=paste0(folder_density_voronoi_I_lato_taxi,TAG, ".png"),
          #        width = 700 / dpi, height = 650 / dpi,
          #        dpi = dpi)
          # ggsave(filename=paste0(folder_density_voronoi_I_lato_biciclette,TAG, ".png"),
          #        width = 700 / dpi, height = 650 / dpi,
          #       dpi = dpi)
          
          } 
          
          
          if (mean(GRID_B$N, na.rm = T) > 0) {
          
            
          ### --> Cumulative_Number_people
          Cumulative_Number_plot <- (GRID_B[, c("x", "N")]) %>% 
            # st_transform("+proj=laea +x_0=0 +y_0=0 +lon_0=-10 +lat_0=40") %>%
            st_transform("+proj=omerc  +lat_0=0 +lonc=0 +alpha=0 +k_0=.7 +datum=WGS84 +units=m +no_defs +gamma=0") %>%
            ggplot(aes()) +
            theme_bw() +
            geom_sf(aes(fill = N)) + 
            scale_fill_gradient2(low = "grey", mid = "white", high = "brown", midpoint = .02, trans = "sqrt",
                                 name =expression( paste("[num.people]"))) +
            # scale_fill_gradient2(low = "grey", mid = "white", high = "brown") +
            coord_sf(datum = st_crs(32632)) 
          Cumulative_Number_plot
          ## save plot
          dpi = 96
          ggsave(filename=paste0(folder_density_Number_duca_aosta,TAG, ".png"),
                 width = 700 / dpi, height = 650 / dpi,
                 dpi = dpi)
          # ggsave(filename=paste0(folder_density_Number_lato_taxi,TAG, ".png"),
          #        width = 700 / dpi, height = 650 / dpi,
          #        dpi = dpi)
          # ggsave(filename=paste0(folder_density_Number_lato_biciclette,TAG, ".png"),
          #        width = 700 / dpi, height = 650 / dpi,
          #        dpi = dpi)
          
          }
        } 
          
        }, error= function(err) {print("not enough data DF_people_t0")
          
        }, finally = {
          
        })
          
      }
} 


###---> save all densities values
# write.csv(all_ID_density, file = paste0("D:/Federico/CityFLows/output_files/all_ID_densities_duca_aosta_17h20_to_18h20_10Oct2021.csv"), row.names=FALSE)

write.csv(all_ID_density, file = paste0("D:/Federico/CityFLows/output_files/all_ID_densities_duca_aosta_b.csv"), row.names=FALSE)
# write.csv(all_ID_density, file = paste0("D:/Federico/CityFLows/output_files/all_ID_densities_lato_taxi_b.csv"), row.names=FALSE)
# write.csv(all_ID_density, file = paste0("D:/Federico/CityFLows/output_files/all_ID_densities_lato_biciclette_b.csv"), row.names=FALSE)

