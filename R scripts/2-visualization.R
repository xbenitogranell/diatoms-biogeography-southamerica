#clear workspace
rm(list=ls(all=TRUE))
dev.off()
#unload all loaded packages
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

#load packages
library(tidyverse)
library(ggplot2)

#Read in assembled diatom datasets and Regions
combined <- read.csv("data/assembledspp.csv", row.names=1)
lake_regions <- read.csv("data/regions.csv", row.names = 1)

##Merge diatom datasets and regions datasets
modern_lakes <- merge(combined, lake_regions, by="row.names")

#transform dataframe to tidy format
df_thin <- modern_lakes %>%
  gather(key = taxa, value = count, -Row.names, -region)#don't gather region

#import dataframe wiht old and new names to group
changes_training <- read.csv("data/old_new_nms_trainingset.csv", stringsAsFactors = FALSE)

#spread
diatomRegions <- df_thin %>%
  mutate(taxa = plyr::mapvalues(taxa, from = changes_training$old, to = changes_training$new_1)) %>%
  group_by(region, Row.names, taxa) %>%
  summarise(count = sum(count)) %>%
  filter(!count == 0) %>% #this is to remove empty samples (rows)
  spread(key = taxa, value = count) %>%
  as.data.frame()

# Overview of number of diatom taxa
genera_overview <- df_thin %>%   
  separate(taxa, into = c("genera", "sp"), convert = TRUE, remove = FALSE) %>%  
  mutate(genera_f=factor(genera)) %>%
  mutate(genera_f=str_replace(genera_f, "Ammphora", "Amphora"))%>% #fix typos
  mutate(genera_f=str_replace(genera_f, "Achanthes", "Achnanthes")) %>%
  filter(!count==0) %>%
  filter(!taxa=="Auxospores") %>%
  group_by(genera_f) %>%
  summarise(n_spp = n_distinct(taxa)) %>%
  mutate(genera=fct_reorder(genera_f, n_spp)) #reorder n_spp

sum(genera_overview$n_spp) #1762 species
length(genera_overview$genera_f) #129 genera

ggplot(genera_overview, aes(x=genera, y=n_spp)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()

spp_mostoccurrence <- df_thin %>%
  group_by(taxa) %>%
  filter(!count==0) %>%
  count(taxa, sort = TRUE) %>%
  ungroup()

# not important stuff
test2 <- df_thin %>%
  filter(str_detect(taxa, "Mastogloia")) %>%
  filter(!count==0) %>%
  summarise(Unique_Elements = n_distinct(taxa))


## Sites
sitesDB <- read.csv("data/biogeographySites.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(CollectionName, Country, Collector.Analyst, Year, SiteName, SampleType, Habitat, Substrate,
                code, region, Lat.DD.S, Long.DD.W) %>%
  mutate(region=str_replace(region, "Colombia-Andes-Central", "Colombia-Andes"))%>%
  mutate(region=str_replace(region, "Colombia-Andes-Eastern", "Colombia-Andes"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-North", "Colombia-Lowlands"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-Eastern", "Colombia-Lowlands"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-Western", "Colombia-Lowlands")) %>%
  rename(Row.names=code) 

# Here is to create a bubble chart showing diatom most common species across regions and habitats
diatoms_habitat <- df_thin %>%
  mutate(taxa = plyr::mapvalues(taxa, from = changes_training$old, to = changes_training$new_1)) %>%
  group_by(region, Row.names, taxa) %>%
  summarise(count = sum(count)) %>%
  filter(count > 30) %>% #this is to remove empty samples (rows)
  spread(key = taxa, value = count) %>%
  as.data.frame() %>%
  left_join(sitesDB, by="Row.names") %>% 
  select(-c(CollectionName, Country, Collector.Analyst, Year, SiteName, region.y, Row.names, SampleType)) %>%
  gather(key = taxa, value = abund, -Habitat, -Lat.DD.S, -Long.DD.W, -region.x, -Substrate) %>%
  mutate(taxa=factor(taxa)) %>%
  mutate(Habitat=factor(Habitat)) %>%
  filter(!Habitat=="channel") %>%
  #ecological grouping
  mutate(taxa_traits = plyr::mapvalues(taxa, from = changes_training$old, to = changes_training$new_2)) %>%
  filter(!taxa_traits=="Auxospores")

# Make a bubble chart
plt <- ggplot(diatoms_habitat, aes(x = reorder(region.x, -Lat.DD.S), y = fct_rev(taxa))) + 
  geom_point(aes(size = abund, color = Habitat, shape=taxa_traits)) +
  scale_color_viridis_d(option = "D")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7))+
  ylab("Diatom taxa") + xlab("Regions")
plt

ggsave("plots/diatoms_regions_traits.png", plot = last_plot(),
       height=8, width=10,units="in",
       dpi = 300)

## Environmental data
diatom_environment <- read.csv("data/environmental_data_lakes.csv") %>%
  mutate(lake_depth_ratio=Lake_area/Depth_avg) %>%
  mutate(lake_catch_ratio=Lake_area/Wshd_area) %>%
  mutate(catch_vol_ratio=Wshd_area/Vol_total) %>%
  rename(Row.names=code) %>%
  left_join(diatomRegions, by="Row.names") #here join with diatom data






