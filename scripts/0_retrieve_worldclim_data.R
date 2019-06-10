# Download average temperature form WorldClim
# WorldClim version 2 - average temperature
# NB this takes some time as the file to be downloaded is >1.Go

# Create a directory
dir.create("data/worldclim")

bioclim_url <- "http://biogeo.ucdavis.edu/data/worldclim/v2.0/tif/base/wc2.0_30s_bio.zip"
download.file(bioclim_url, destfile = "data/worldclim/wc2.0_30s_bio.zip")
unzip("wc2.0_30s_bio.zip",  exdir = "data/worldclim/wc2.0_30s_bio")

# get all the file names inside the folder
clim_filenames <- list.files(path = "data/worldclim/wc2.0_30s_bio", pattern = "*.tif", full.names = TRUE)

# Remove unwanted bioclim files
file.remove(clim_filenames[c(2:19)])
