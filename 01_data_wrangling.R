library(tidyverse)
library(lubridate)
library(ggthemes)
library(purrr)
library(patchwork)

# loading custom function
source("functions/merge_files.R")




# Temperature -------------------------------------------------------------


list_of_files <- list.files(path = "data/temp/", full.names = T)

temp <- merge_files(list_of_files)

region_names <- str_match(temp$file_name, "temp/\\s*(.*?)\\s*_t.csv") 

temp$region <- region_names[,2]


# NDVI --------------------------------------------------------------------


list_of_files <- list.files(path = "data/ndvi/landsat/", full.names = T)

ndvi <- merge_files(list_of_files) 

region_names <- str_match(ndvi$file_name, "landsat/\\s*(.*?)\\s*_nl.csv") 

ndvi$region <- region_names[,2]



# NDWI --------------------------------------------------------------------

list_of_files <- list.files(path = "data/ndwi/", full.names = T)

ndwi <- merge_files(list_of_files) 

region_names <- str_match(ndwi$file_name, "ndwi/\\s*(.*?)\\s*_wl.csv") 

ndwi$region <- region_names[,2]



## eliminating superfluous of objects
rm(region_names, list_of_files, merge_files)

## END OF SCRIPT --------------
