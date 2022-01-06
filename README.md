# Mystery Cults - AIA2022

## Data 

Data were downloaded from the [Database of Religious History (DRH)](https://religiondatabase.org/landing/) on 6<sup>th</sup> January 2022 and consists of 37 entries with 8786 unique answers. 

## Data Analysis Software

All data preprocessing was performed using [R version 4.0.5](https://cran.r-project.org/index.html). The following packages were used:
  - [tidyverse version 1.3.1](https://cran.r-project.org/web/packages/tidyverse/index.html)
  - [data.table version 1.14.0](https://cran.r-project.org/web/packages/data.table/index.html)
  - [naniar version 0.6.1](https://cran.r-project.org/web/packages/naniar/index.html)
  - [ape version 5.5](https://cran.r-project.org/web/packages/ape/index.html)

The data derived taxonomy was then produced using [BEAST2 version 2.6.6](https://www.beast2.org/). 

Susequent analysis was performed using [R version 4.0.5](https://cran.r-project.org/index.html), with the following packages:
  - [tidyverse version 1.3.1](https://cran.r-project.org/web/packages/tidyverse/index.html)
  - [ape version 5.5](https://cran.r-project.org/web/packages/ape/index.html)
  - [ggtree version 2.99.0](https://bioconductor.org/packages/release/bioc/html/ggtree.html)

All required packaged will be automatically installed by running the analysis code. 

## Run Analysis 

To run the analysis use: 
1. Use ```setwd()``` or the RStudio file selector to set the working directory to the folder that contains the analysis code.
2. ```source("run_preprocessing.R")```
3. ```source("run_analysis.R")```
