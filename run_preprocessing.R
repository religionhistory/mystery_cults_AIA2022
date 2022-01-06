rm(list = ls())

# Load packages and functions
source("./project_support.R")

# Clean data
make.dir("./01_make_nexus/input")
files <- c("./data/drh_data.csv", "./data/questions.csv")
file.copy(files, "./01_make_nexus/input", overwrite = TRUE)
setwd("./01_make_nexus/")
source("make_nexus.R")
setwd("..")
