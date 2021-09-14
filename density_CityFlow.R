

rm(list = ls())

library(ggvoronoi)
library(dplyr)
library(deldir)
library(gridGraphics)
library(sf)

## https://cran.r-project.org/web/packages/ggvoronoi/vignettes/ggvoronoi.html
# https://r-charts.com/part-whole/voronoi-diagram/

setwd("D:/Federico/CityFLows/output_files/")

## load walking people data:
# DF_people <- read.csv('DF_people_ALTRAN_2021-09-03.txt')[-1]
# DF_people <- read.csv('DF_people_ALTRAN_2021-09-01.txt')[-1]
# DF_people <- read.csv('D:/Federico/CityFLows/output_files/DF_people_ALTRAN_07July2021.txt')[-1]
DF_people <- read.csv('D:/Federico/CityFLows/output_files/DF_people_ALTRAN_2021-09-03.txt')[-1]
DF_people <- DF_people[, c("timedate", "ID", "X", "Y")]
### transform cm into meters
DF_people$X <- (DF_people$X)/10
DF_people$Y <- (DF_people$Y)/10

## select unique ID by  timestamp and DIST
DF_people <- distinct(DF_people, timedate, ID, X, Y)

# DF_people <- DF_people[complete.cases(DF_people), ] 
# DF_people <- DF_people[!duplicated(DF_people[ ,c('timedate', 'X', 'Y', 'DIST')]), ]


# DF_people <- DF_people %>%
#   group_by(timedate, ID) %>%
#   summarise(ID)



## select unique ID, X, Y at the minimum timestamp (t0) "starting point"
# DF_people <- DF_people %>%
#   group_by(X, Y, ID, timedate) %>%
#   summarise(min(timedate))

DF_people_t0 <- DF_people %>%
  group_by(ID) %>%
  summarise(timedate = min(timedate))

## join X, Y
DF_people_t0 <- DF_people_t0 %>%
  merge(DF_people, by = c("ID", "timedate"))
## remove duplicated
DF_people_t0 <- DF_people_t0[!duplicated(DF_people_t0), ]


## remove duplicate values of X,Y
DF_people_t0 <- DF_people_t0 %>%
  distinct(X, Y,.keep_all = T)

## quick plot
p_t0 <- ggplot(DF_people_t0) +
  geom_point(aes(X,Y,color=NULL)) +
  scale_y_reverse()
p_t0


# p <- ggplot(DF_people) +
#   geom_point(aes(X,Y,color=DIST)) +
#   scale_y_reverse()
# p


# ## plot a Voronoi diagram
# ggplot(DF_people) +
#   geom_voronoi(aes(X,Y,fill = DIST)) + 
#   scale_y_reverse()


p_t0 <- ggplot(DF_people_t0,aes(X,Y)) +
        stat_voronoi(geom="path") +
        geom_point(size = 0.7) 
        #scale_y_reverse()
p_t0


DF_people_t0 <- as.data.frame(DF_people_t0)




# voronoi_diagram <- voronoi_polygon(DF_people_t0, x="X", y="Y")
# vor_df <- fortify_voronoi(voronoi_diagram)
# ggplot(vor_df) +
#   geom_polygon(aes(x=X,y=Y))

pts <- tibble(longitude = DF_people_t0$X, latitude = DF_people_t0$Y) %>%
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(32632)

vor <- st_voronoi(st_combine(pts))
# voronoi of pts
head(vor)
vor_poly <- st_collection_extract(vor)
head(vor_poly)
plot(pts, col = 'blue', pch = 16, cex = .01)
plot(vor_poly, add = T, fill = NA)

bbox <- st_bbox(pts)
xmin = bbox[1]
ymin = bbox[2]
xmax = bbox[3]
ymax = bbox[4]

##--------- Overlay a rectangular GRID ----###########################
######################################################################

## design a square around the extent of the point and crop sfc_POLYGON 
# box = c(xmin = 0.5, ymin= 13.3, xmax= 58.5, ymax= 31.3)
library(lwgeom)
box = bbox
plot(st_crop(st_make_valid(vor_poly), box))
# plot(st_crop(vor_poly, box))
plot(pts, add = T, pch = 16, cex = .01)


## define a polygon for the border extent
# extent = st_sfc(st_polygon(list(rbind(c(0.5,13.3), c(58.5,13.3), c(58.5,31.3), 
#                                    c(0.5, 31.3), c(0.5,13.3)))))

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


plot(st_crop(st_make_valid(vor_poly), box),add = TRUE, col = 'white')
plot(pts, add = TRUE, pch = 16, cex = .01)
plot(st_make_grid(GRID, cellsize = 2, square = T), add = TRUE)


cell_AREA <- 2*2 ## m^2

## get number of point within each GRID cell.....
N_people <- st_intersection(GRID ,pts) 
N_people <- N_people %>% 
  group_by(cell_ID) %>%
  summarise(N = length(x))
N_people <- as.data.frame(N_people)


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
  select(vor_ID,
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
density_distr <- intersect_ID %>%
  group_by(cell_ID) %>%
  summarise(density_dist = sum(1/area))

density_distr <- density_distr %>%
  left_join(N_people, by = c("cell_ID"))
density_distr$N[is.na(density_distr$N)] <- 1
Dv <- density_distr %>%
  mutate(density_v = (density_dist/N)/cell_AREA)
 


###-- SECOND METHOD----####
## get number of people in each GRID cell (cell_ID)

intersect_ID_DvI <- intersect_ID %>%
  left_join(N_people[, c("cell_ID", "N")], by = c("cell_ID"))
## remove row with NA values in the column "N"
library(tidyr)
intersect_ID_DvI <- intersect_ID_DvI %>% drop_na(N)

DvI <- intersect_ID_DvI %>%
  group_by(cell_ID) %>%
  summarise(total_area_vor_cell = sum(area)) 

## join number of people with DvI
DvI <- DvI %>%
  left_join(N_people[, c("cell_ID", "N")], by = c("cell_ID"))


DvI <- DvI %>%
  mutate(density_vI = N/total_area_vor_cell)

# DENSITIES <- DvI %>%
#   left_join(Dv, by =c("cell_ID"))

## calculate "classic" measure of density (Number of people / GRID CELL AREA)
DvI$Ds <- DvI$N/cell_AREA

## join density values to the GRID
GRID <- GRID %>%
  left_join(DvI, by = c("cell_ID"))

Dv <- as.data.frame(Dv)
GRID <- GRID %>%
  left_join( Dv[, c("cell_ID", "density_v")], by = c("cell_ID"))


plot(GRID[, c("x", "density_v")])
plot(GRID[, c("x", "density_vI")])
## standard density
plot(GRID[, c("x", "Ds")])

## load speed data
DF_speed <- read.csv("D:\\Federico\\CityFLows\\output_files\\DF_speed.csv")
GRID <- GRID %>%
  left_join( DF_speed[, c("cell_ID", "density_v")], by = c("cell_ID"))



# ## compute the Voronoi diagram for each position for each ID (assign cells to each ID)
# voronoi_t0 <- deldir(DF_people_t0$X, DF_people_t0$Y)
# tiles <- tile.list(voronoi_t0)
# plot(tiles, pch = 19)
# 
# ## get area of each Voronoi cell (this is also called Dirichlet area surrounding the point)
# ## "dir.area" 
# voronoi_area <- voronoi_t0$summary$dir.area





