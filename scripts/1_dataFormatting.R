### DATA FORMATTING FOR BETA DIVERSITY ####

### PACKAGES ####

require(dplyr)
require(reshape2)
require(sf)

source('functions/temperature.trend.R')

### DATA ####

# Species

sp_mat <- readRDS("../Quebec_data/data/sp_mat_abun_jul2018.RDS")

# Ecoregion

ecoreg_df <- readRDS("../Quebec_data/data/ecoreg_df.RDS")

# combine sugar maple-hickory with sugar maple-basswood
ecoreg_df$ecoreg5 <- ecoreg_df$ecoreg6

ecoreg_df$ecoreg5[ecoreg_df$ecoreg5 == "Sugar maple-bitternut hickory"] = "Sugar maple-basswood"

ecoreg_df$ecoreg5 <- droplevels(ecoreg_df$ecoreg5)

# Spatial

xy <- st_read("../Quebec_data/data/plot_xy32198_may2018.gpkg")
st_crs(xy) <- 32198


# Climate
bioclim10 <- readRDS("../Quebec_data/data/bioclim10_mat.RDS")
bioclim_all <- readRDS("../Quebec_data/data/bioclim_corrected.RDS")

# disturbance
env_data <- readRDS("../Quebec_data/data/env_data_may2018.RDS")


### FORMATTING SPECIES ####

### Keep only plots that have been sampled first before 1980 and last after 2000

sp_mat <- sp_mat %>%
  filter(!(PICABI>0 | PINSYL>0)) %>% # planted tree species
  mutate(TOTAL = rowSums(select(.,-ID_PE,-ID_PE_MES, -plot_id, -year_measured))) %>%
  group_by(plot_id) %>% arrange(year_measured) %>%
  filter(first(year_measured)<=1980 & last(year_measured)>=2000) %>% # before 1980 & after 2000
  filter(first(TOTAL)>0 | last(TOTAL)>0) %>% # remove if first AND last rows = 0
  slice(c(1, n()))  %>%  # only first and last inventory to have +/- constant time interval
  select(-TOTAL)

# Species selection
# Combine Sorbus americana and Sorbus decora as forest technicians are not able to distinguish these species
# Remove Malus sp (introduced, difficult to interpret)
# Keep all other species
# conservative treshold because 1. all species contribute to beta diversity and 2. rare species doesn't affect our analysis...
sp_mat <- sp_mat %>%
  mutate(SORSP = SORAME + SORDEC) %>%
  select(-MALSP, -SORAME, -SORDEC)

MySpecies <- sort(names(which(colSums(sp_mat[,-c(1:4)])>1)))
sp_mat$TOTAL <- rowSums(sp_mat[,MySpecies])

sp_mat <- sp_mat %>%
  group_by(plot_id) %>% arrange(year_measured) %>%
  filter(first(TOTAL)>0 | last(TOTAL)>0) %>%
  select(ID_PE, ID_PE_MES, plot_id, year_measured, MySpecies)

### FORMATTING SPATIAL ####

xy <- subset(xy, plot_id %in% sp_mat$plot_id)

### FORMATTING CLIMATE ####

bioclim_all$year <- as.numeric(bioclim_all$year)
bioclim_all <- bioclim_all %>% subset(ID_PE %in% sp_mat$ID_PE & year >= 1970)

# sg_15 <- mean temperature for period 3 (growing season)
# sg_06 <- total precipitation for period 3 (growing season)
# cmi <- climate moisture index
# bio_06 <- min temperature of coldest period


####################


slope_TP <- temperature.trend(bioclim_all$sg_15, (bioclim_all$year-1970),
                              bioclim_all$ID_PE)[,1:2]
slope_PP <- temperature.trend(bioclim_all$sg_06, (bioclim_all$year-1970),
                              bioclim_all$ID_PE)[,1:2]
slope_CMI <- temperature.trend(bioclim_all$cmi, (bioclim_all$year-1970),
                              bioclim_all$ID_PE)[,1:2]

bioclim_all <- bioclim_all %>%
  group_by(ID_PE) %>%
  mutate(CMI_min = mean(cmi)-min(cmi),
         TP_max = max(bio_05)-mean(bio_05),
         TP_min = mean(bio_06)-min(bio_06)) %>%
  select(ID_PE, CMI_min, TP_max, TP_min) %>% distinct()

bioclim10 <- bioclim10 %>%
  subset(ID_PE_MES %in% sp_mat$ID_PE_MES) %>%
  select(plot_id, year_measured, sg_15, sg_06) %>%
  rename(TP = sg_15, PP = sg_06) %>%
  left_join(bioclim_all, by = 'ID_PE')


delta_clim <- bioclim10 %>%
  group_by(plot_id) %>%
  arrange(year_measured)  %>%
  mutate(year1 = first(year_measured)) %>%
  mutate(year2 = lead(year_measured, 1L)) %>%
  mutate(time_interv = year2 - year1) %>%
  mutate(year_measured = NULL) %>%
  filter(!is.na(year2)) %>% distinct() %>%
  left_join(slope_TP, by = c("ID_PE" = "plot_id")) %>% rename(slope_TP = slope) %>%
  left_join(slope_PP, by = c("ID_PE" = "plot_id")) %>% rename(slope_PP = slope) %>%
  left_join(slope_CMI, by = c("ID_PE" = "plot_id")) %>% rename(slope_CMI = slope) %>%
  select(ID_PE, plot_id, year1:time_interv, TP:TP_min, slope_TP:slope_CMI)


saveRDS(delta_clim, "data/delta_clim.rds")
delta_clim <- readRDS("data/delta_clim.rds")

### FORMATTING DISTURBANCES ####

# harvest

#tree_data <- readRDS("../Quebec_data/data/tree_data_may2018.RDS")

# mortalityTable <- dcast(tree_data, plot_id + year_measured ~ state,
#                         fun.aggregate = length)
#
#
#
# harvest <- mortalityTable %>%
#   subset(plot_id %in% sp_mat$plot_id) %>%
#   group_by(plot_id) %>%
#   mutate(harvest_100 = harvested/lag(alive,1L)) %>%
#   tidyr::replace_na(list(harvest_100 = 0)) %>%
#   mutate(harvest_100 = ifelse(harvest_100>1, 1, harvest_100)) %>%
#   select("plot_id", "year_measured", "harvest_100", "harvested", "dead")



# Disturbance
env_data <- env_data %>%
  subset(ID_PE %in% sp_mat$ID_PE)

major_disturb <- as.data.frame.matrix(table(env_data$ID_PE_MES, env_data$ORIGINE2)) %>%
  tibble::rownames_to_column(var = "ID_PE_MES")

minor_disturb <- as.data.frame.matrix(table(env_data$ID_PE_MES, env_data$PERTURB2)) %>%
  tibble::rownames_to_column(var = "ID_PE_MES")


disturb <- left_join(major_disturb, minor_disturb, by = "ID_PE_MES") %>%
  mutate(outbreak = 2*severe_outbreak + light_outbreak,
         burn = 2*burn + partial_burn,
         logging = 2*logging + partial_logging,
         windfall = 2*windfall + partial_windfall) %>%
  mutate_if(is.numeric, funs(ifelse(.>2,2,.))) %>%
  select(ID_PE_MES, outbreak, burn, logging, windfall)


# Natural disturbances
planting <- env_data[which(env_data$ORIGINE2=="plantation"),]$plot_id

env_data2 <- env_data %>% filter(!(plot_id %in% planting)) %>%
  mutate(major_disturb = ifelse(is.na(ORIGINE2), 0, 1),
         minor_disturb = ifelse(is.na(PERTURB2), 0, 1)) %>%
  mutate(disturb = 2*major_disturb + minor_disturb) %>%
  mutate(major_nat_disturb = ifelse(major_disturb==0 | ORIGINE2=="logging", 0, 1)) %>%
  mutate(minor_nat_disturb = ifelse(is.na(PERTURB2) | PERTURB2=="partial_logging", 0, 1)) %>%
  mutate(nat_disturb = 2*major_nat_disturb + minor_nat_disturb) %>%
  mutate_at(vars(disturb, nat_disturb), funs(ifelse(.>2,2,.))) %>%
  select(ID_PE:year_measured, age_mean, disturb, nat_disturb)

# Join everything

env_data2 <- env_data2 %>%
  arrange(plot_id, year_measured) %>%
  left_join(disturb, by = "ID_PE_MES")

# df of disturbances

env_df <- env_data2 %>%
  arrange(plot_id, year_measured) %>%
  group_by(plot_id) %>%
  mutate_at(vars(disturb:windfall), cummax) %>% # all disturbances
  slice(n()) %>%
  select(-ID_PE_MES, -year_measured)

env_recent <- env_data2 %>%
  arrange(plot_id, year_measured) %>%
  group_by(plot_id) %>%
  slice(-1) %>% #only recent disturbances
  mutate_at(vars(disturb:nat_disturb, logging), cummax) %>%
  select(ID_PE, plot_id, rec_disturb=disturb, rec_nat_disturb=nat_disturb, rec_logging=logging) %>%
  slice(n())

env_old <- env_data2 %>%
  arrange(plot_id, year_measured) %>%
  group_by(plot_id) %>%
  slice(1) %>% #only old disturbances
  mutate_at(vars(disturb:nat_disturb, logging), cummax) %>%
  select(ID_PE,plot_id, old_disturb=disturb, old_nat_disturb=nat_disturb, old_logging=logging) %>%
  slice(n())

# age not evaluated in recently harvested forest
env_df[is.na(env_df$age_mean),]$age_mean <- 15

env_df <- env_df %>%
  left_join(env_old) %>%
  left_join(env_recent)

# Final df of all environmental variables

lm_df <- delta_clim %>%
  right_join(env_df) %>%
  left_join(xy) %>%
  left_join(ecoreg_df) %>%
  ungroup() %>%
  mutate_at(vars(disturb:rec_logging), funs(as.factor(as.character(.))))

# Disturbance level
table(lm_df$disturb)

saveRDS(lm_df, file = "data/lm_df.rds")

# remove plantation from sp_mat and sp_ba

sp_mat <- filter(sp_mat, plot_id %in% lm_df$plot_id)

saveRDS(sp_mat, file = "data/sp_mat.rds")
