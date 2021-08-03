###############################################
## Tropical South American Diatom Database
###############################################

###############################################
#contact email: xavier.benito.granell@gmail.com 
###############################################

#load packages
library(tidyverse)
library(ggplot2)

#Read in assembled diatom datasets and Regions
combined <- read.csv("data/assembledspp_new.csv", row.names=1)
lake_regions <- read.csv("data/regions_new.csv", row.names = 1, sep=";")

##Merge diatom datasets and regions datasets
modern_lakes <- merge(combined, lake_regions, by="row.names")

#transform dataframe to tidy format
df_thin <- modern_lakes %>%
  gather(key = taxa, value = count, -Row.names, -region)#don't gather region

#import dataframe wiht old and new names to group
changes_training <- read.csv("data/old_new_nms_trainingset.csv", sep=";", stringsAsFactors = FALSE)

#spread
diatomRegions <- df_thin %>%
  mutate(taxa = plyr::mapvalues(taxa, from = changes_training$old, to = changes_training$new_1)) %>%
  mutate(region=str_replace(region, "Colombia-Andes-Central", "Colombia-Andes"))%>%
  mutate(region=str_replace(region, "Colombia-Andes-Eastern", "Colombia-Andes"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-North", "Colombia-Lowlands"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-Eastern", "Colombia-Lowlands"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-Western", "Colombia-Lowlands")) %>%
  group_by(region, Row.names, taxa) %>%
  summarise(count = sum(count)) %>%
  filter(!count == 0) %>% #this is to remove empty samples (rows)
  spread(key = taxa, value = count) %>%
  as.data.frame()

row.names(diatomRegions) <- diatomRegions$Row.names

# See how many unique samples 
length(unique(diatomRegions$Row.names)) #437 samples w/diatom data

## split data by regions and reassemble
diatomRegionsList <- split(diatomRegions, diatomRegions$region)

#drop empty regions from the list
diatomRegionsList$`Lauca Basin` <- NULL
diatomRegionsList$`Tierra del Fuego` <- NULL

nms <- names(diatomRegionsList)

#Remove empty spp resulting from merging dataframes
remove <- function(i, cores, ...) {
  core <- cores[[i]]
  rownms <- diatomRegionsList[[i]][["Row.names"]]
  core <- core[, -which(names(core) %in% c("region", "Row.names"))] # drop year & depths vars
  core[is.na(core)] <- 0
  core <- core[, colSums(core) > 0] #select only present species
  rownames(core) <- rownms
  return(core)
}

diatomRegionsList <- lapply(seq_along(diatomRegionsList), remove, cores=diatomRegionsList)
names(diatomRegionsList) <- nms

# check the length of the list
length(diatomRegionsList)

##extract diatom datasets and write as csv separately
nams <- names(diatomRegionsList)
for (i in seq_along(diatomRegionsList)) {
  assign(paste0("", nams[i]), diatomRegionsList[[i]])
  setwd("~/R/diatoms-biogeography-southamerica/data/diatom-datasets")
  filename=paste(nams[i],".csv")
  write.csv(diatomRegionsList[[i]], filename)
}


## Sites
sitesDB <- read.csv("data/biogeographySites_new.csv", sep=";", stringsAsFactors = FALSE) %>%
  dplyr::select(CollectionName, Country, Collector.Analyst, Year, SiteName, SampleType, Habitat, Substrate,
                code, region, Lat.DD.S, Long.DD.W) %>%
  mutate(Lat.DD.S=as.numeric(gsub(",", ".", gsub("\\.", "", Lat.DD.S)))) %>%
  mutate(Long.DD.W=as.numeric(gsub(",", ".", gsub("\\.", "", Long.DD.W)))) %>%
  mutate(region=str_replace(region, "Colombia-Andes-Central", "Colombia-Andes"))%>%
  mutate(region=str_replace(region, "Colombia-Andes-Eastern", "Colombia-Andes"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-North", "Colombia-Lowlands"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-Eastern", "Colombia-Lowlands"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-Western", "Colombia-Lowlands"))


head(sitesDB)
rownames(sitesDB) <- sitesDB$code

str(sitesDB)
unique(sitesDB$SiteName) #338 sites
length(unique(sitesDB$region)) #29

#remove extra-tropical regions
sitesDB <- sitesDB %>% filter(!region %in% c("Tierra del Fuego", "", "Lauca Basin"))

unique(sitesDB$SiteName) #326 sites
length(unique(sitesDB$region)) #26 regions

## split data by regions and reassemble
sitesDBList <- split(sitesDB, sitesDB$region)
sitesDBList$`Tierra del Fuego` <- NULL
sitesDBList$`Lauca Basin` <- NULL


nams <- names(sitesDBList)
for (i in seq_along(sitesDBList)) {
  assign(paste0("", nams[i]), sitesDBList[[i]])
  setwd("~/R/diatoms-biogeography-southamerica/data/sites-datasets")
  filename=paste(nams[i],".csv")
  write.csv(sitesDBList[[i]], filename)
}

#write.csv(unique(sitesDB$region), "~/R/diatoms-biogeography-southamerica/data/all_regions_new.csv")

######
#Environmental datasets
environmental_data_lakes <- read.csv("data/environmental_data_lakes.csv") %>%
  mutate(lake_depth_ratio=Lake_area/Depth_avg) %>%
  mutate(lake_catch_ratio=Lake_area/Wshd_area) %>%
  mutate(catch_vol_ratio=Wshd_area/Vol_total)

rownames(environmental_data_lakes) <- environmental_data_lakes$ï..code
names(environmental_data_lakes)

environmental_data_lakes_regions <- merge(environmental_data_lakes, lake_regions, by="row.names") %>%
  select(!Row.names) %>%
  mutate(region=str_replace(region, "Colombia-Andes-Central", "Colombia-Andes"))%>%
  mutate(region=str_replace(region, "Colombia-Andes-Eastern", "Colombia-Andes"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-North", "Colombia-Lowlands"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-Eastern", "Colombia-Lowlands"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-Western", "Colombia-Lowlands"))

#
length(unique(environmental_data_lakes_regions$ï..code)) #647 samples with environmental data
length(unique(environmental_data_lakes_regions$region)) #28 regions

## split data by regions and reassemble
ENVRegionsList <- split(environmental_data_lakes_regions, environmental_data_lakes_regions$region)
ENVRegionsList$`Tierra del Fuego` <- NULL
ENVRegionsList[[1]] <- NULL #remove Lauca Basin

# check the length of the list
length(ENVRegionsList)


nams <- names(ENVRegionsList)
for (i in seq_along(ENVRegionsList)) {
  assign(paste0("", nams[i]), ENVRegionsList[[i]])
  setwd("~/R/diatoms-biogeography-southamerica/data/region-datasets")
  filename=paste(nams[i],".csv")
  write.csv(ENVRegionsList[[i]], filename)
}

## Create the TSADB.RData list
TSADBList <- list(sitesDBList, ENVRegionsList, diatomRegionsList)
names(TSADBList) <- c("sites", "environment", "diatoms")

## Save the R list object
saveRDS(TSADBList, "data/TSADB.Rdata")
