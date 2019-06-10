## Table S1. List of studied species and their traits
library(knitr)
library(kableExtra)
library(dplyr)

tree_trait <- readRDS("data/tree_trait_sti.RDS")
sp_mat <- readRDS("data/sp_mat.rds")
MySpecies <- colnames(sp_mat)[-c(1:4)]

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

tree_trait <- tree_trait %>% filter(Code %in% MySpecies) %>%
  mutate(Group = case_when(Code %in% pioneer ~ "Pioneer",
                           Code %in% boreal ~ "Boreal",
                           Code %in% temperate ~ "Temperate"))

tree_trait <- tree_trait %>%
  dplyr::select(Latin, Vernacular, Group, TolS, STI) %>%
  arrange(Latin) %>%
  rename("Species name" = Latin, "Vernacular name" = Vernacular,
         "Shade tolerance" = TolS, "Temperature preference" = STI)


kable(tree_trait, digits = 2, format = 'markdown') %>%
  cat(file = "ms/figures/TableS1.md", sep = "/n")
