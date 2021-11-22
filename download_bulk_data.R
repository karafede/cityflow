


rm(list = ls())

setwd("D:\\Federico\\CityFLows")
source("download_ALTRAN.R")


start_time <- Sys.time()


##--- download 60 minutes of data---#####

for (i in 1:1000) {
  print(i)
  ## download last 60 seconds of data
  download_data(last_seconds = 20000)
  Sys.sleep(7200)  ## 120 minutes
  
}


## download last 60 seconds of data???
# data <- download_data(last_seconds = 60)
# 
# run_time <- Sys.time() - start_time
# print(run_time)
