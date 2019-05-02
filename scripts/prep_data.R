#### Data preparation for other scripts ####

### PACKAGES ####
require(dplyr)
require(reshape2)
require(sf)

source("functions/SCTBD.R")

### DATA ####

# Species

sp_mat <- readRDS("data/sp_mat_tbi.rds")

MySpecies <- colnames(sp_mat)[-c(1:4)]

# CWM
Comm_trait1 <- readRDS("data/Comm_trait1.RDS")
Comm_trait2 <- readRDS("data/Comm_trait2.RDS")

# Environmental variables

lm_df <- readRDS("data/lm_df_tbi.rds")


# Ecoregion map
ecoregion <- st_read("../Quebec_data/data/ecoregion_simple.gpkg", quiet = T)
# ecoregion <- st_transform(ecoregion, 32198)

ecoregion$SOUS_DOM6 <- factor(ecoregion$SOUS_DOM6, c("Sugar maple-bitternut hickory",
                                                     "Sugar maple-basswood",
                                                     "Sugar maple-yellow birch",
                                                     "Balsam fir-yellow birch",
                                                     "Balsam fir-white birch",
                                                     "Spruce-moss"))



xy <- st_read("../Quebec_data/data/plot_xy32198_may2018.gpkg")

xy <- st_transform(xy, st_crs(ecoregion))

xy <- subset(xy, plot_id %in% sp_mat$plot_id)


# create a species matrice with historical (1) and contemporary period (2)
sp_mat1 <- sp_mat %>% group_by(plot_id) %>% arrange(year_measured) %>% slice(1)
sp_mat2 <- sp_mat %>% group_by(plot_id) %>% arrange(year_measured) %>% slice(n())

# quantile(sp_mat2$year_measured-sp_mat1$year_measured)
#
# sp_mat12 <- rbind(cbind("time" = rep("t1", nrow(sp_mat1)),
#                         sp_mat1[,c("plot_id", "year_measured", MySpecies)]),
#                   cbind("time" = rep("t2", nrow(sp_mat1)),
#                         sp_mat2[,c("plot_id", "year_measured", MySpecies)]))



############################
### TBI ####
############################

sp_contrib <- SCTBD(mat1 = sp_mat1[,MySpecies], mat2 = sp_mat2[,MySpecies], pa.tr = F)

tbi <- apply(sp_contrib$SCTBD.b.den, 1, sum) + apply(sp_contrib$SCTBD.c.den, 1, sum)
b <- rowSums(sp_contrib$SCTBD.b.den)
c <- rowSums(sp_contrib$SCTBD.c.den)
a <- rowSums(sp_contrib$SCTBD.a.den)

BCD <- data.frame(tbi = rowSums(sp_contrib$SCTBD.b.den) + rowSums(sp_contrib$SCTBD.c.den),
                  losses = rowSums(sp_contrib$SCTBD.b.den),
                  gains = rowSums(sp_contrib$SCTBD.c.den),
                  similarity = rowSums(sp_contrib$SCTBD.a.den))


### DATA FRAME OF BCD ####

BCDdf <- cbind(plot_id = sp_mat1$plot_id, BCD) %>%
  left_join(lm_df, by = "plot_id")

### DEFINE SPECIES GROUPS ####

pioneer <- c("BETPAP", "BETPOP",
             "POPBAL", "POPDEL", "POPGRA", "POPTRE", "PRUPEN", "SALSP", "SORSP")

temperate <- c("ACEPEN", "ACERIN", "ACERUB", "ACESAC", "ACESPI", "AMESP", "BETALL",
               "CARCAR", "CARCOR", "FAGGRA", "FRAAME", "FRANIG", "FRAPEN", "JUGCIN",
               "OSTVIR",
               "PICRUB", "PINRES", "PINSTR",
               "PRUSER", "PRUVIR",
               "QUEALB", "QUEBIC", "QUEMAC", "QUERUB", "THUOCC",
               "TILAME", "TSUCAN", "ULMAME", "ULMRUB", "ULMTHO")

boreal <- c("ABIBAL","ALNRUG", "LARLAR", "PICGLA", "PICMAR",
            "PINBAN")

spgr <- c(boreal, pioneer, temperate)

sctbi.a <- sp_contrib$SCTBD.a.den
sctbi.b <- sp_contrib$SCTBD.b.den
sctbi.c <- sp_contrib$SCTBD.c.den
sctbi.d <- sp_contrib$SCTBD.d.den

BCD_boreal <- cbind(b.stable = apply(sctbi.a[,boreal],1,sum),
                    b.losses = apply(sctbi.b[,boreal],1,sum),
                    b.gains = apply(sctbi.c[,boreal],1,sum),
                    b.tbi = apply(sctbi.d[,boreal],1,sum))
BCD_temperate <- cbind(t.stable = apply(sctbi.a[,temperate],1,sum),
                       t.losses = apply(sctbi.b[,temperate],1,sum),
                       t.gains = apply(sctbi.c[,temperate],1,sum),
                       t.tbi = apply(sctbi.d[,temperate],1,sum))
BCD_pioneer <- cbind(p.stable = apply(sctbi.a[,pioneer],1,sum),
                     p.losses = apply(sctbi.b[,pioneer],1,sum),
                     p.gains = apply(sctbi.c[,pioneer],1,sum),
                     p.tbi = apply(sctbi.d[,pioneer],1,sum))
