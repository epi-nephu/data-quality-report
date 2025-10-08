# This script is used to check for packages that are required and install them if they are not in the package library

#.libPaths("M:/NEPHU/Restrict/Data Intelligence and Epidemiology/R_Libraries")

# Package names required
packages <- c("tidyverse", "readxl", "lubridate", "ggpubr", "ggthemes", "here", "summarytools", "magrittr", "flextable", "officer", "janitor", 
              "cowplot", "apyramid", "tibble", "rmarkdown", "knitr", "sf", "plyr", "gt", "rio")

# Install packages not yet installed
install.packages(setdiff(packages, rownames(installed.packages)), dep=TRUE)

# Packages loading
#invisible(lapply(packages, library, character.only = TRUE))