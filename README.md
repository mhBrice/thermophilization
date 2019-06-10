# thermophilization


This repository includes the data and R scripts to reproduce analyses and figures found in the article *Disturbances amplify tree community responses to climate change in the temperate-boreal ecotone* by Brice et al. in GEB.

All data used for the analyses can be found in the data folder.
Several steps required for data preparation were performed using scripts available in the [Quebec_data repository](https://github.com/mhBrice/Quebec_data). Scripts 1 and 2 format the data for the analyses performed in the scripts 3, 4 and 5. The figures produced are in the folder ms/figures.

## Packages required to run the Scripts

library(dplyr)
library(reshape2)
library(stringr)

library(sf)
library(sp)
library(raster)

library(vegan)
library(FD)
library(adespatial)
library(car)
library(gtools)
library(zoo)
library(effects)

library(RColorBrewer)
library(scales)
library(graphicsutils)

library(knitr)
library(kableExtra)
