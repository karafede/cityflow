

library(magick)
library(stringr)
# remotes::install_github("rstudio/chromote")
# remotes::install_github("rstudio/webshot2")
library(webshot2)
library(lubridate)
library(gtools)
library(animation)

rm(list = ls())
# setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/voronoi_I_density_plots_duca_aosta")
# setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/voronoi_density_plots_duca_aosta")
# setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/standard_density_plots_duca_aosta")
# setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/standard_density_plots_lato_taxi")
setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/standard_density_plots_lato_biciclette")
# setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/speed_plots_Duca_Aosta")
# setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/speed_plots_lato_taxi")
# setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/speed_plots_lato_biciclette")


# dir.create("C:/Users/Federico/ownCloud/CityFLows/output_files/standard_density_plots_duca_aosta/movies")
# output_dir <- "C:/Users/Federico/ownCloud/CityFLows/output_files/standard_density_plots_duca_aosta/movies/"

# dir.create("C:/Users/Federico/ownCloud/CityFLows/output_files/standard_density_plots_lato_taxi/movies")
# output_dir <- "C:/Users/Federico/ownCloud/CityFLows/output_files/standard_density_plots_lato_taxi/movies/"

dir.create("C:/Users/Federico/ownCloud/CityFLows/output_files/standard_density_plots_lato_biciclette/movies")
output_dir <- "C:/Users/Federico/ownCloud/CityFLows/output_files/standard_density_plots_lato_biciclette/movies/"

# dir.create("C:/Users/Federico/ownCloud/CityFLows/output_files/voronoi_I_density_plots_duca_aosta/movies")
# output_dir <- "C:/Users/Federico/ownCloud/CityFLows/output_files/voronoi_I_density_plots_duca_aosta/movies/"


# dir.create("C:/Users/Federico/ownCloud/CityFLows/output_files/speed_plots_Duca_Aosta/movies")
# output_dir <- "C:/Users/Federico/ownCloud/CityFLows/output_files/speed_plots_Duca_Aosta/movies/"

# dir.create("C:/Users/Federico/ownCloud/CityFLows/output_files/speed_plots_lato_taxi/movies")
# output_dir <- "C:/Users/Federico/ownCloud/CityFLows/output_files/speed_plots_lato_taxi/movies/"

# dir.create("C:/Users/Federico/ownCloud/CityFLows/output_files/speed_plots_lato_biciclette/movies")
# output_dir <- "C:/Users/Federico/ownCloud/CityFLows/output_files/speed_plots_lato_biciclette/movies/"



images <- list.files(pattern = ".png")
##---> sort files in alphabetical order....
images <- mixedsort(images)


#### DAILY IMAGES with labels 
for (i in 1:length(images)) {
  print(i)
  ## open .png images
  img <- image_read(images[i])
  # plot(img)
  ## add LABEL with DATE and TIME
  # date <- str_sub(images[i], start = 0, end = -15)
  # time <- str_sub(images[i], start = 13, end = -5)
  # label <- paste0(date, "    ", time)
  label <- str_sub(images[i], start = 0, end = -5)
  img <- image_annotate(img, label, size = 20, color = "black", boxcolor = "transparent",
                        degrees = 0, location = "+300-5")   ## +300+7 
  plot(img)  
  image_write(img, paste0(output_dir, label, ".png"), format = "png")
  
}


# setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/speed_plots_Duca_Aosta/movies")
# setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/speed_plots_lato_taxi/movies")
# setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/speed_plots_lato_biciclette/movies")
# setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/standard_density_plots_duca_aosta/movies")
# setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/standard_density_plots_lato_taxi/movies")
setwd("C:/Users/Federico/ownCloud/CityFLows/output_files/standard_density_plots_lato_biciclette/movies")



imgs <- list.files(pattern="*.png")
##---> sort files in alphabetical order....

imgs <- mixedsort(imgs)
saveVideo({
  for(img in imgs){
    im <- magick::image_read(img)
    plot(as.raster(im))
  }  
})

  


###########################################################################
###########################################################################
###########################################################################

# to make a movie.......
# to use with ImageMagik using the commanad line cmd in windows
# cd into the directory where there are the png files



## sort by name.....
# magick -delay 50 -loop 0 *.png MOVIE_density_Voronoi_I_Duca_AOSTA.gif
# magick -delay 50 -loop 0 *.png MOVIE_MEAN_SPEED_Duca_AOSTA.gif

###########################################################################
###########################################################################
###########################################################################






