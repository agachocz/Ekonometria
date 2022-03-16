library(tidyverse)
library(plyr)

data_files <- list.files(path = "data", full.names = TRUE)

data_list <- lapply(data_files, function(file){
  read.csv(file)
})

# Some data are noted daily, some annually

daily_data <- join_all(data_list[1:3], by = c("Entity", "Code", "Day"), type = 'full')
annual_data <- join_all(data_list[4:11], by = c("Entity", "Code", "Year"), type = 'full')
