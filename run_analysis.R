rm(list = ls())

# Load packages and functions
source("./project_support.R")

# Make figures folder
make.dir("figures")

# Find questions that discriminate between clusters
make.dir("./02_clusters/input")
files <- c("./01_make_nexus/output/mystery_cults.csv", "./01_make_nexus/output/question_dict.csv", "./data/BEAST/mystery_cults.tree")
file.copy(files, "./02_clusters/input", overwrite = TRUE)
setwd("./02_clusters/")
source("clusters.R")
setwd("..")

# Visualize tree
make.dir("./03_visualization/input")
files <- c( "./02_clusters/output/data_cluster.csv", "./data/BEAST/mystery_cults.tree")
file.copy(files, "./03_visualization/input", overwrite = TRUE)
setwd("./03_visualization/")
source("visualization.R")
setwd("..")

