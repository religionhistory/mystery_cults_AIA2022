rm(list = ls())

# Load packages and functions
source("./project_support.R")

# Make figures folder
make.dir("figures")

# Visualize tree
make.dir("./02_visualization/input")
files <- c( "./01_make_nexus/output/mystery_cults.csv", "./data/BEAST/mystery_cults.tree")
file.copy(files, "./02_visualization/input", overwrite = TRUE)
setwd("./02_visualization/")
source("visualization.R")
setwd("..")

# Find questions that discriminate between clusters
make.dir("./03_clusters/input")
files <- c("./01_make_nexus/output/mystery_cults.csv", "./01_make_nexus/output/question_dict.csv", "./data/BEAST/mystery_cults.tree")
file.copy(files, "./03_clusters/input", overwrite = TRUE)
setwd("./03_clusters/")
source("clusters.r")
setwd("..")