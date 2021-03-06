#### Community Temperature and Shade Index ####

### PACKAGES ####
library(sf)
library(sp)
library(dplyr)
library(stringr)
library(raster)
library(vegan)
library(FD)


### Extract North America tree species occurence from QUICC-FOR
# FIA, Quebec Forest inventory, Ontario forest inventory

# Species matrix (produced in 1_dataFormatting.R)

sp_mat <- readRDS("data/sp_mat.rds")
sp_mat1 <- sp_mat %>% group_by(plot_id) %>% arrange(year_measured) %>% slice(1)
sp_mat2 <- sp_mat %>% group_by(plot_id) %>% arrange(year_measured) %>% slice(n())

MySpecies <- colnames(sp_mat)[-c(1:4)]

# Occurence data
# Tree occurrence dataset from QUICCFOR project
load("data/treedatasp.rda")
tree_sf <- st_as_sf(treedatasp)

# Trait (Paquette et al)

tree_trait <- read.csv2("data/tree_trait.csv", sep=";")
tree_trait <- subset(tree_trait, Code %in% MySpecies, select = c(Code, TolS))
tree_trait$TolS <- as.numeric(as.character(tree_trait$TolS))

# Species code
sps_code <- read.csv2("data/ref_spCode.csv")
vect_names <- as.character(sps_code$CODE[sps_code$spCode %in% c(MySpecies, "SORDEC", "SORAME")])
vect_names <- na.omit(vect_names)

# Match species code

colnames(tree_sf) <- str_replace(colnames(tree_sf), "X", "")
colnames(tree_sf) <- str_replace_all(colnames(tree_sf), "\\.", "-")

# Correct some code

tree_sf$`19242-CAR-OVA` <- (rowSums(cbind(tree_sf$`19242-CAR-OVA`, tree_sf$`NA-CAR-OVA`))>0)*1

tree_sf$`183319-PIN-BAN` <- (rowSums(cbind(tree_sf$`183319-PIN-BAN`, tree_sf$`NA-PIN-BAN`))>0)*1

### Select species ####

tree_sf <- tree_sf %>%
  dplyr::select(plot_id:year_measured, vect_names)


### Get worldclim data ####

## NB you need to run `0_retrieve_worldclim_data.R`
## You need to execute the lines above the
clim_filenames <- list.files(path = "data/worldclim/wc2.0_30s_bio", pattern = "*.tif", full.names = TRUE)

# Get annual mean temperature
bioclimTP <- raster(clim_filenames)

## Crop bioclim raster
bioclimTP <- crop(bioclimTP, as.vector(st_bbox(tree_sf))[c(1, 3, 2, 4)])

## Extract bioclim data from points
bioclimTP_pts <- extract(bioclimTP, as(tree_sf, "Spatial"))

tree <- tree_sf %>%
  st_set_geometry(NULL) %>%
  mutate(TP = bioclimTP_pts)

# Change species code
x <- which(colnames(tree) %in% c("plot_id", "year_measured", "TP"))
colnames(tree)[-x] <- as.character(sps_code$spCode[match(colnames(tree)[-x], sps_code$CODE)])

tree <- tree %>%
  mutate(SORSP = SORAME + SORDEC) %>%
  dplyr::select(-SORAME, -SORDEC)

### PART 1 - Mean CTI ####

### Compute Species Temperature Index using mean TP of occurrence ####
STI <- data.frame(species = MySpecies,
                  STI = numeric(length(MySpecies)),
                  STI_med = numeric(length(MySpecies)))


for(sp in MySpecies) {
  tmp <- tree[which(tree[,sp]==1),]
  STI[which(MySpecies==sp), "STI"] <- mean(tmp$TP, na.rm = T)
  STI[which(MySpecies==sp), "STI_med"] <- median(tmp$TP, na.rm = T)
}

# Join Shade tolerance with STI
tree_trait <- left_join(STI, tree_trait, by = c("species" = "Code"))

#### Compute mean community trait ####
tree_mat <- as.matrix(tree_trait[,-1])
rownames(tree_mat) <- MySpecies

CTI_mean1 <- functcomp(tree_mat, as.matrix(sp_mat1[,MySpecies]))
CTI_mean1 <- cbind.data.frame(sp_mat1[,1:4], CTI_mean1)
sum(is.na(CTI_mean1))

CTI_mean2 <- functcomp(tree_mat, as.matrix(sp_mat2[,MySpecies]))
CTI_mean2 <- cbind.data.frame(sp_mat2[,1:4], CTI_mean2)
sum(is.na(CTI_mean2))

### PART 2 - Warm and cold end of CT distribution ####

### Compute Community Temperature Index from Species Temperature Distribution ####
STD2 <- STD1 <- data.frame(plot_id = sp_mat1$plot_id,
                           STq5 = numeric(length(sp_mat1$plot_id)),
                           STq10 = numeric(length(sp_mat1$plot_id)),
                           STq90 = numeric(length(sp_mat1$plot_id)),
                           STq95 = numeric(length(sp_mat1$plot_id)))

for(id in sp_mat1$plot_id){
  comm1 <- subset(sp_mat1, plot_id==id, select = MySpecies)
  comm2 <- subset(sp_mat2, plot_id==id, select = MySpecies)

  comm1 <- comm1[,which(comm1>0)]
  comm2 <- comm2[,which(comm2>0)]

  tp1 <- c()
  for(sp in names(comm1)) {
    tp1 <- c(tp1, sample(tree[which(tree[,sp] == 1), ]$TP, 1000*comm1[[sp]], replace = T))
  }

  tp2 <- c()
  for(sp in names(comm2)) {
    tp2 <- c(tp2, sample(tree[which(tree[,sp] == 1), ]$TP, 1000*comm2[[sp]], replace = T))
  }

  STD1[STD1$plot_id==id, 2:5] <- quantile(tp1, c(.05,.1,.9,.95), na.rm = T)

  STD2[STD2$plot_id==id, 2:5] <- quantile(tp2, c(.05,.1,.9,.95), na.rm = T)
}


# Join mean trait to STD

Comm_trait1 <- left_join(CTI_mean1, STD1, by = "plot_id")
Comm_trait2 <- left_join(CTI_mean2, STD2, by = "plot_id")

saveRDS(Comm_trait1, "data/Comm_trait1.RDS")
saveRDS(Comm_trait2, "data/Comm_trait2.RDS")


### Format trait ####

tree_trait$Vernacular <- as.character(sps_code$VERNACULAR[match(tree_trait$species, sps_code$spCode)])

tree_trait$Latin <- as.character(sps_code$complete.name[match(tree_trait$species, sps_code$spCode)])

tree_trait <- tree_trait %>%
  dplyr::select(Code=species, Latin, Vernacular, STI, STI_med, TolS)

saveRDS(tree_trait, "data/tree_trait_sti.RDS")
