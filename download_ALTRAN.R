

## download camera data from ALTRAN (Capgemini)

## last_seconds is the duration of the time range where data are downloaded 
## from 10 to 60 seconds

download_data <- function(last_seconds = last_last_seconds) {


rm(list = ls())

## clear memory
gc()
## https://stackoverflow.com/questions/2617600/importing-data-from-a-json-file-into-r
library(httr)
library(jsonlite)
library(stringr)
library(RJSONIO)
library(lubridate)


### ignore SSL certificate
httr::set_config(httr::config(ssl_verifypeer = 0L))
setwd("D:\\Federico\\CityFLows\\new_input_files")


## get current date
current_date <- str_sub(Sys.time(), start = 1, end = -10)

x <- ymd_hms(Sys.time())

## get time interval
## start time
ts_start <- paste0(hour(x)-1, ":30:00")
## end time (+ 1 hour)
ts_to <- paste0(hour(x), ":30:00") 

last_seconds <- 20000
flag_report <- 0


if (last_seconds > 0) {
  # json_ALTRAN = paste0("http://ec2-34-248-140-70.eu-west-1.compute.amazonaws.com:7777/api/get_report_people_distance_at_interval?ts_key=1234$5678*9&ts_start=",current_date,
  #                      "%20",ts_start,"&ts_to=",current_date,"%20",ts_to, "&last_seconds=", last_seconds)
  
  
  
  ################
  ### CAMERA 2 ###
  ################
  
  tryCatch({
    
    ##---lato sinistro stazione con bus e taxi
    json_ALTRAN_Camera2 = paste0("http://ec2-34-248-140-70.eu-west-1.compute.amazonaws.com:7777/api/get_report_people_distance_at_interval_v2?ts_key=1234$5678*9&last_second=",
                                 last_seconds, "&flag_report=", flag_report)
    
    # fetch ALL data
    resp_Camera2 <- GET(json_ALTRAN_Camera2)
    report_Camera2 <- content(resp_Camera2)
    
    ## get report (.json data)
    exportJson_Camera2 <- toJSON(report_Camera2)
    # write(exportJson, paste0(format(x,format="%Y_%m_%d"), "_",format(x,format="%H_%M")  ,".json"))
    write(exportJson_Camera2, paste0("Camera2_from_xxxx_to_",format(x,format="%Y_%m_%d"), "__",format(x,format="%H_%M"), ".json"))
    # write(exportJson, paste0(current_date, "_from_11am_to_2pm.json"))
    
  }, error= function(err) {print("server not working - Milano Centrale Lato Taxi")
    
  }, finally = {
    
  })
  

  ################
  ### CAMERA 3 ###
  ################
  
  tryCatch({
    
    ##----Piazza Duca d'Aosta
    json_ALTRAN_Camera3 = paste0("http://ec2-34-248-140-70.eu-west-1.compute.amazonaws.com:7003/api/get_report_people_distance_at_interval_v2?ts_key=1234$5678*9&last_second=",
                                 last_seconds, "&flag_report=", flag_report)
    # fetch ALL data
    resp_Camera3 <- GET(json_ALTRAN_Camera3)
    report_Camera3 <- content(resp_Camera3)
    
    ## get report (.json data)
    exportJson_Camera3 <- toJSON(report_Camera3)
    write(exportJson_Camera3, paste0("Camera3_from_xxxx_to_",format(x,format="%Y_%m_%d"), "__",format(x,format="%H_%M"), ".json"))
    
  }, error= function(err) {print("server not working - Milano Centrale P.zza Duca AOSTA")
    
  }, finally = {
    
  })
  

  ################
  ### CAMERA 1 ###
  ################
  
  
  tryCatch({
    
    ##----lato biciclette
    json_ALTRAN_Camera1 = paste0("http://ec2-34-248-140-70.eu-west-1.compute.amazonaws.com:7000/api/get_report_people_distance_at_interval_v2?ts_key=1234$5678*9&last_second=",
                                 last_seconds, "&flag_report=", flag_report)
    # fetch ALL data
    resp_Camera1 <- GET(json_ALTRAN_Camera1)
    report_Camera1 <- content(resp_Camera1)
    
    ## get report (.json data)
    exportJson_Camera1 <- toJSON(report_Camera1)
    write(exportJson_Camera1, paste0("Camera1_from_xxxx_to_",format(x,format="%Y_%m_%d"), "__",format(x,format="%H_%M"), ".json"))
    
    
  }, error= function(err) {print("server not working - Milano Centrale Lato Biciclette")
    
  }, finally = {
    
  })
  

} else {
  
  
  ## get the URL
  # json_ALTRAN = "http://ec2-34-248-140-70.eu-west-1.compute.amazonaws.com:7777/api/get_report_people_distance_at_interval?ts_key=1234$5678*9&ts_start=2021-08-26%2020:30:00&ts_to=2021-08-26%2021:30:00"
  # json_ALTRAN = paste0("https://ec2-34-248-140-70.eu-west-1.compute.amazonaws.com:7777/api/get_report_people_distance_at_interval?ts_key=1234$5678*9&ts_start=",current_date,
  #                      "%20",ts_start,"&ts_to=",current_date,"%20",ts_to, "&last_seconds=0")
  json_ALTRAN = paste0("http://ec2-34-248-140-70.eu-west-1.compute.amazonaws.com:7777/api/get_report_people_distance_at_interval_v2?ts_key=1234$5678*9", 
                       "&last_second=0", "&flag_report=0")
  
  
  # fetch ALL data
  resp <- GET(json_ALTRAN)
  report <- content(resp)

  ## get report (.json data)
  exportJson <- toJSON(report)
  write(exportJson, paste0(current_date, "_", hour(x)-1, "_30_to_",format(x,format="%H_%M"),".json"))

  }

}




