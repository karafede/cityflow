



rm(list = ls())

library(ggvoronoi)
library(dplyr)
library(deldir)
library(gridGraphics)
library(sf)
library(sp)
library(raster)
library(gstat)

## https://cran.r-project.org/web/packages/ggvoronoi/vignettes/ggvoronoi.html
# https://r-charts.com/part-whole/voronoi-diagram/

setwd("D:/Federico/CityFLows/output_files/")

## load walking people data:
# DF_people <- read.csv('DF_people_ALTRAN_2021-09-03.txt')[-1]
DF_people <- read.csv('DF_people_ALTRAN_2021-09-01.txt')[-1]
DF_people <- read.csv('D:/Federico/CityFLows/DF_people_ALTRAN_07July2021.txt')[-1]
DF_people <- DF_people[, c("timedate", "ID", "X", "Y")]
### transform cm into meters
DF_people$X <- (DF_people$X)/10
DF_people$Y <- (DF_people$Y)/10

## select unique ID by  timestamp and DIST
DF_people <- distinct(DF_people, timedate, ID, X, Y)

list_IDs <- unique(DF_people$ID)

i <- 1


## build a new dataframe DF_people including speeds associated to each position
DF_people_speed = NULL


for (i in list_IDs) {
  print(paste0("ID: ",i))
  
  
  ## select one trajectory
  DF <- DF_people %>%
    filter(ID == i)
  
  ## quick plot
  p_t <- ggplot(DF) +
    geom_point(aes(X,Y,color=NULL)) +
    scale_y_reverse()
  p_t
  
  
  DF$timedate <- as.POSIXct(DF$timedate)
  ## add row number
  DF$index <- 1:nrow(DF)
  
  
  ## compute speed without curvature (UNITS --> [m/s]), principal movement speed
  for (j in 1:nrow(DF))  {
    print(j)
    
    if (j != 49) {
    ##---distance between two consecutive points
    distance_j <- sqrt( ((DF[j+1, "X"] - DF[j, "X"])^2) +
                        ((DF[j+1, "Y"] - DF[j, "Y"])^2) )
    print(paste0("distance: ", distance_j))
    
    ## speed [m/s]
    speed_j <- distance_j / as.numeric(DF[j+1, "timedate"] - DF[j, "timedate"])
    speed_j <- abs(speed_j)
    print(paste0("speed: ", speed_j))
    
    DF_speed <- data.frame(timedate = DF[j, "timedate"],
                      ID = i,
                      X = DF[j, "X"],
                      Y = DF[j, "Y"],
                      speed = speed_j)
    DF_people_speed = rbind(DF_people_speed, DF_speed)
    
    }
      
  }
}


### ------------------------------------ ###########################
# make for all trajectories and then make a SPATIAL INTERPOLATION ##
####################################################################


# convert data into a spatialpolygon dataframe------------------------------------

cell_SIZE <- 2   ## (UNITS --> [m^2])

DF <- DF_people_speed %>%
  dplyr::select(X,Y, speed)

## fill NA values with 0
DF[is.na(DF)] <- 0
# DF <- DF[!is.na(DF$speed),]



DF$x <- DF$X
DF$y <- DF$Y

coordinates(DF) = ~x + y  ## Set spatial coordinates to create a Spatial object:

# make a variogram----------------------------------------------------------------

vargram_speed <- variogram(speed ~ 1, DF) # calculates sample variogram values 

##--- fit the variogram
vargram_speed_fit  <- fit.variogram(vargram_speed, fit.ranges = FALSE, fit.sills = FALSE,
                                  vgm(1, c("Sph", "Exp", "Mat")), fit.kappa = TRUE)

##--plot the sample values, along with the fit model
plot(vargram_speed, vargram_speed_fit) 


# make a regular empty grid
x.range <- as.numeric(c(min(DF$X), max(DF$X)))  # min/max longitude of the interpolation area
y.range <- as.numeric(c(min(DF$Y), max(DF$Y)))  # min/max latitude of the interpolation area

## grid at 10km resolution
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = cell_SIZE),
                   y = seq(from = y.range[1], to = y.range[2], by = cell_SIZE))  # expand points to grid
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE
plot(grd)


###------ perform KRIEGING
dat.krg <- gstat::krige(speed ~ 1, DF, grd, vargram_speed_fit, nmax = 1)
r <- raster(dat.krg)
projection(r) <- CRS("+proj=utm +zone=32 +datum=WGS84")  
# plot(r)




###---- perform interpolation (INVERSE DISTANCE WEIGHTING method)
idw <- idw(formula = speed ~ 1, locations = DF,
           newdata = grd)  # apply idw model for the data

r <- raster(idw)
### add CRS
crs(r) <- "+proj=utm +zone=32 +datum=WGS84" 
plot(r)

bbox(r)

DF_speed = as.data.frame(idw)  # output is defined as a data table
DF_speed <- DF_speed[, c("x", "y", "var1.pred")]
names(DF_speed) <- c("X", "Y", "mean_speed")  # give names to the modelled variables


write.csv(DF_speed, file = "DF_speed.csv", row.names=FALSE)
plot(r$var1.pred)

