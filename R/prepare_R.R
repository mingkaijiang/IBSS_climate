#### prepare R scripts

#### all functions
if(!require(pacman))install.packages("pacman")
pacman::p_load(reshape, reshape2, lubridate, eeptools, hyfo,
               dplyr) # add other packages needed to this list


# Sourcing all R files in the modules subdirectory
sourcefiles <- dir("modules", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)