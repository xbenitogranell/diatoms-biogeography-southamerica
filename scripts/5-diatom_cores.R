###############################################
## Tropical South American Diatom Database
###############################################

###############################################
#contact email: xavier.benito.granell@gmail.com 
###############################################

#clear workspace
rm(list=ls(all=TRUE))
dev.off()

#unload all loaded packages
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

#load packages for functions used
library(tidyverse)
library(ggplot2)
library(sf)
library(mapview)
library(neotoma2)
library(maps)
#library(rwordlmap)
library(ggpubr)
library(adespatial)
library(mgcv)
library(openxlsx)


#read diatom core datasets
mergedCores <- read.csv("data/diatom-cores/mergedCores_diatomcounts.csv")[-1] #this is a dataframe with absolute counts containing all the spp

agedepth <- mergedCores[, names(mergedCores) %in% c("depth", "upper_age", "lower_age", "lake")]
diat <- mergedCores[, !names(mergedCores) %in% c("depth", "upper_age", "lower_age", "lake")]
diat[is.na(diat)] <- 0

diatoms_save <- cbind(agedepth, diat)

changes <- read.csv("data/diatom-cores/old_new_nms_cores_counts.csv", stringsAsFactors = FALSE)
#new1: ecological groups
#new2: harmonized taxonomic names

#this is to transform to tidy format, calculate % and subset more common species
new <- diatoms_save %>% 
  gather(key = taxa, value = count, -depth, -upper_age, -lower_age, -lake) %>%
  mutate(taxa = plyr::mapvalues(taxa, from = changes$old, to = changes$new_2)) %>%
  group_by(depth, taxa, lake, upper_age, lower_age) %>%
  summarise(count = sum(count)) %>%
  dplyr::filter(!count == "0" ) %>% #this is to remove empty samples (rows)
  dplyr::filter(!upper_age==0.0) %>% #this is to drop extraneous ages
  ungroup() %>%
  dplyr::group_by(depth, lake) %>%
  mutate(relative_abundance_percent = count / sum(count) * 100) %>%
  ungroup()

#make it wide
core_counts_wide <- new %>%
  dplyr::select(depth, lake, upper_age, lower_age, taxa, count) %>%
  spread(key = taxa, value = count) 

## split cores by lakes and reassemble
coresList <- split(core_counts_wide, core_counts_wide$lake)

# function to calculate time series of LCBD indices and Spearman rho between indices
core_function <- function(i, cores, ...) {
  core <- cores[[i]]
  core <- core[ , -which(names(core) %in% c("depth","upper_age", "lower_age", "lake", "AgeCE"))] # drop year & depths vars
  core[is.na(core)] <- 0
  core <- core[, colSums(core) > 0] #select only present species
  depth <- coresList[[i]]$depth
  upper_age <- coresList[[i]]$upper_age
  lower_age <- coresList[[i]]$lower_age
  age <- cbind(upper_age, lower_age)
  cbind.data.frame(depth,age,core) #combine extracted columns and remove first row to match with scd
}

## apply function to each core
diat_cores_list <- lapply(seq_along(coresList), core_function, cores=coresList)
names(diat_cores_list) <- names(coresList)

## extract diatom cores and write as csv separately
nms <- names(diat_cores_list)
setwd(paste0(getwd(), "/data/diatom-cores"))
for (i in seq_along(diat_cores_list)) {
  assign(paste0(nms[i]), diat_cores_list[[i]])
  filenamexlsx=paste(nms[i],".xlsx")
  write.xlsx(diat_cores_list[[i]], filenamexlsx)
}

