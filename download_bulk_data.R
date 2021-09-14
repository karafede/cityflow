

source("download_ALTRAN.R")


start_time <- Sys.time()


##--- download 60 minutes of data---#####

for (i in 1:60) {
  print(i)
  ## download last 60 seconds of data
  data <- download_data(last_seconds = 60)
  
}


## download last 60 seconds of data
data <- download_data(last_seconds = 60)

run_time <- Sys.time() - start_time
print(run_time)
