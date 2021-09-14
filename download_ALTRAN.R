

## download camera data from ALTRAN (Capgemini)

## last_seconds is the duration of the time range where data are downloaded 
## from 10 to 60 seconds

download_data <- function(last_seconds = last_last_seconds) {


rm(list = ls())
## https://stackoverflow.com/questions/2617600/importing-data-from-a-json-file-into-r
library(httr)
library(jsonlite)
library(stringr)
library(RJSONIO)
library(lubridate)


### ignore SSL certificate
httr::set_config(httr::config(ssl_verifypeer = 0L))
setwd("D:\\Federico\\CityFLows\\input_files")


## get current date
current_date <- str_sub(Sys.time(), start = 1, end = -10)

x <- ymd_hms(Sys.time())

## get time interval
## start time
ts_start <- paste0(hour(x)-1, ":30:00")
## end time (+ 1 hour)
ts_to <- paste0(hour(x), ":30:00") 

last_seconds <- 60


if (last_seconds > 0) {
  json_ALTRAN = paste0("http://ec2-34-248-140-70.eu-west-1.compute.amazonaws.com:7777/api/get_report_people_distance_at_interval?ts_key=1234$5678*9&ts_start=",current_date,
                       "%20",ts_start,"&ts_to=",current_date,"%20",ts_to, "&last_seconds=", last_seconds)
  
  # fetch ALL data
  resp <- GET(json_ALTRAN)
  report <- content(resp)
  
  ## get report (.json data)
  exportJson <- toJSON(report)
  write(exportJson, paste0(format(x,format="%Y_%m_%d"), "_",format(x,format="%H_%M")  ,".json"))

} else {
  
  
  ## get the URL
  # json_ALTRAN = "http://ec2-34-248-140-70.eu-west-1.compute.amazonaws.com:7777/api/get_report_people_distance_at_interval?ts_key=1234$5678*9&ts_start=2021-08-26%2020:30:00&ts_to=2021-08-26%2021:30:00"
  json_ALTRAN = paste0("https://ec2-34-248-140-70.eu-west-1.compute.amazonaws.com:7777/api/get_report_people_distance_at_interval?ts_key=1234$5678*9&ts_start=",current_date,
                       "%20",ts_start,"&ts_to=",current_date,"%20",ts_to, "&last_seconds=0")
  
  
  # fetch ALL data
  resp <- GET(json_ALTRAN)
  report <- content(resp)

  ## get report (.json data)
  exportJson <- toJSON(report)
  write(exportJson, paste0(current_date, "_", hour(x)-1, "_30_to_",format(x,format="%H_%M"),".json"))

  }

}




