##avoid source
no_function()

##read data
setwd(r4projects::get_project_wd())
library(tidyverse)
rm(list = ls())

setwd("3_data_analysis/data_20200511/fiber/")
fiber_data <- readxl::read_xlsx("External-Internal Samples and Dates 19-11-06.xlsx")
