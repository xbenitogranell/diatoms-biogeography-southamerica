#clear workspace
rm(list=ls(all=TRUE))
dev.off()
#unload all loaded packages
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)

#load packages
library(tidyverse)
library(ggplot2)

#Read in assembled diatom datasets and Regions
#combined <- read.csv("data/assembledspp.csv", row.names=1)
combined <- read.csv("data/assembledspp_new.csv", row.names=1)

#lake_regions <- read.csv("data/regions.csv", row.names = 1)
lake_regions <- read.csv("data/regions_new.csv", row.names = 1, sep=";") 

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
  mutate(region=str_replace(region, "Colombia-Lowlands-Western", "Colombia-Lowlands")) %>%
  rename(Row.names=code) 


## Read in lake environmental data
diatom_environment <- read.csv("data/environmental_data_lakes.csv") %>%
  mutate(lake_depth_ratio=Lake_area/Depth_avg) %>%
  mutate(lake_catch_ratio=Lake_area/Wshd_area) %>%
  mutate(catch_vol_ratio=Wshd_area/Vol_total) %>%
  filter(!lat<= -40) %>% #filter out Tierra del Fuego sites
  rename(Row.names=ï..code) %>%
  magrittr::set_rownames(.$Row.names) %>%
  left_join(sitesDB, by="Row.names") #join with site's information

  
# This is to create a bubble chart showing the most common diatom species across regions and habitats
diatoms_habitat <- df_thin %>%
  mutate(taxa = plyr::mapvalues(taxa, from = changes_training$old, to = changes_training$new_1)) %>%
  group_by(region, Row.names, taxa) %>%
  summarise(count = sum(count)) %>%
  filter(count > 30) %>% # uncomment this line to make the following plot (P/A data will not be plotted)
  spread(key = taxa, value = count) %>%
  as.data.frame() %>%
  left_join(sitesDB, by="Row.names") %>% 
  select(-c(CollectionName, Country, Collector.Analyst, region.x, SiteName, Row.names)) %>%
  gather(key = taxa, value = abund, -Habitat, -Lat.DD.S, -Long.DD.W, -region.y, -Substrate, -Year, -SampleType) %>%
  mutate(taxa=factor(taxa)) %>%
  filter(!Habitat=="channel") %>%
  mutate(Habitat=factor(Habitat)) %>%
  #ecological grouping
  mutate(taxa_traits = plyr::mapvalues(taxa, from = changes_training$old, to = changes_training$new_2)) %>%
  filter(!taxa_traits=="Auxospores") 


# Make a bubble chart
plt <- ggplot(diatoms_habitat, aes(x = reorder(region.y, -Lat.DD.S), y = fct_rev(taxa))) + #arrange regions by latitude
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

## Summarize metadata of regions
data_summ <- df_thin %>%
  mutate(taxa = plyr::mapvalues(taxa, from = changes_training$old, to = changes_training$new_1)) %>%
  group_by(region, Row.names, taxa) %>%
  summarise(count = sum(count)) %>%
  spread(key = taxa, value = count) %>%
  as.data.frame() %>%
  left_join(sitesDB, by="Row.names") %>% 
  # comment the next two lines for making the buuble plot
  left_join(diatom_environment[,c(1,4:27, 50)], by="Row.names") %>% #here join with certain env variables
  select(-c(CollectionName, Country, Collector.Analyst, region.y, Row.names)) %>%
  gather(key = taxa, value = abund, -Habitat, -Lat.DD.S, -Long.DD.W, -region.x, -Substrate, -Year, -SampleType, -SiteName,
         -pH, -Water.T, -Cond, -Turb, -Chl, -Secchi, -Alkalinity, -Ca, -Mg, -K, -Na, -Si, -Cl, -NO2, 
         -NO3, -SO4, -PO4, -TN, -TP, -DO..,-DO,-Carbonate, -Silicate, -DOC, -Ecoregion) %>%
  mutate(taxa=factor(taxa)) %>%
  filter(!Habitat=="channel") %>%
  mutate(Habitat=factor(Habitat)) %>%
  #ecological grouping
  mutate(taxa_traits = plyr::mapvalues(taxa, from = changes_training$old, to = changes_training$new_2)) %>%
  filter(!taxa_traits=="Auxospores") 
  


library(skimr)
metadata <- data_summ %>%
  mutate(Year=factor(Year)) %>%
  mutate(SampleType=factor(SampleType)) %>%
  mutate(Substrate=factor(Substrate)) %>%
  mutate(SiteName=factor(SiteName)) %>%
  group_by(region.x) %>%
  filter(!abund==0) %>%
  skim() 

write.csv(metadata, "regions_metadata.csv")


## Make the same without filtering spp for obtaining a list of diatom spp with coordinates
diatoms_list <- df_thin %>%
  mutate(taxa = plyr::mapvalues(taxa, from = changes_training$old, to = changes_training$new_1)) %>%
  group_by(region, Row.names, taxa) %>%
  summarise(count = sum(count)) %>%
  spread(key = taxa, value = count) %>%
  as.data.frame() %>%
  left_join(sitesDB, by="Row.names") %>% 
  select(-c(CollectionName, Country, Collector.Analyst, region.y, Row.names)) %>%
  gather(key = taxa, value = abund, -Habitat, -Lat.DD.S, -Long.DD.W, -region.x, -Substrate, -Year, -SampleType, -SiteName) %>%
  filter(!taxa=="Auxospores") %>%
  filter(!Habitat=="channel") %>%
  mutate(taxa=factor(taxa)) %>%
  #assign presence/absence column
  mutate(pres_abs=ifelse(abund>0.5, 1,0)) %>% #the shiny collapses if abund<0.5
  #create cut levels of abundance
  mutate(abund_lvl=cut(abund, 
                       c(0,.5,1,2,3,5,100), include.lowest = T,
                       labels = c('<0.5%' ,'0.5-1%', '1-2%', '2-3%', '3-5%','>5%')))

# then assign a palette to this using colorFactor
abundColour <- colorFactor(palette = 'RdYlGn', diatoms_list$abund_lvl)



# would like to make a pie chart plot showing the number of sites per habitat
habitats_plt <- diatoms_list %>% count(Habitat) %>%
  ggplot(aes(x="", y=n, fill=Habitat)) +
  geom_bar(stat="identity", color="black")+
  coord_polar("y", start=0) +
  labs(x = NULL, y = NULL, fill = NULL, title = "")+
  scale_fill_viridis_d()+
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# save plots
ggsave(plot=habitats_plt, "plots/habitats.png",
       height=8, width=10,units="in",
       dpi = 300)

# create a series of plots showing the number of sites per habitat,sampleType and years
habitats_plt <- diatoms_list %>% count(Habitat) %>%
  ggplot(aes(x="", y=n, fill=Habitat)) +
  geom_bar(stat="identity", color="black")+
  coord_polar("y", start=0) +
  labs(x = NULL, y = NULL, fill = NULL, title = "")+
  scale_fill_viridis_d()+
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 14))

sampletype_plt <- diatoms_list %>% count(SampleType) %>%
  ggplot(aes(x="", y=n, fill=SampleType)) +
  geom_bar(stat="identity", color="black")+
  coord_polar("y", start=0) +
  labs(x = NULL, y = NULL, fill = NULL, title = "")+
  scale_fill_viridis_d()+
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 14))

Year_plt <- diatoms_list %>% count(Year) %>%
  ggplot(aes(x="", y=n, fill=Year)) +
  geom_bar(stat="identity")+
  coord_polar("y", start=0) +
  labs(x = NULL, y = NULL, fill = NULL, title = "")+
  scale_fill_viridis_c()+
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 14))


#save plots
ggsave("plots/habitats.png", plot = last_plot(),
       height=8, width=10,units="in",
       dpi = 300)

#plot modern lake database with marginal histograms
library(maps)
library(rwordlmap)
library(ggpubr)

world <- map_data("world")

interest <- c("Colombia", "Ecuador", "Peru", "Bolivia", "Chile", "Argentina")
countries <- world %>% filter(str_detect(region, interest))

sites_map <- read.csv("data/biogeographySites_new.csv", sep=";", stringsAsFactors = FALSE) %>% 
  filter(!region=="Tierra del Fuego" & !Habitat=="channel") %>%
  mutate(Lat.DD.S=as.numeric(gsub(",", ".", gsub("\\.", "", Lat.DD.S)))) %>%
  mutate(Long.DD.W=as.numeric(gsub(",", ".", gsub("\\.", "", Long.DD.W))))
  

southamerica <- ggplot() +
  geom_polygon(data=world, aes(x=long, y = lat, group =group), fill="lightgrey") +
  geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, colour="black")+
  #geom_point(data=sites_map, aes(x=Long.DD.W, y=Lat.DD.S, col=Habitat), shape=20, size=4)+
  geom_point(data=sites_map, aes(x=Long.DD.W, y=Lat.DD.S), shape=20, size=4)+
  scale_color_viridis_d()+
  coord_equal(ylim=c(-45,15), xlim=c(-92,-40))+
  #coord_map("albers", parameters = c(-100, -100),  ylim=c(-40,15), xlim=c(-82,-40)) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw()
southamerica

# southamerica <- ggplot() +
#   geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="lightgrey") +
#   geom_point(data=diatom_environment, aes(x=long, y=lat), colour="blue")+
#   theme(legend.position = "right")+
#   coord_equal(ylim=c(-45,15), xlim=c(-82,-40))+
#   xlab("Longitude") + ylab("Latitude") +
#   theme_bw()
# southamerica

xbp <- gghistogram(
    sites_map$Long.DD.W,
    fill = "orange1",
    binwidth = 4,
    size = 0.1) +
  theme_transparent()

ybp <- gghistogram(
    sites_map$Lat.DD.S,
    fill = "orange1",
    binwidth = 4,
    size = 0.1,) +
  ggpubr::rotate() +
  theme_transparent()

xbp_grob <-  ggplotGrob(xbp)
ybp_grob <-  ggplotGrob(ybp)


my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
my.min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)

xmin <-  my.min(sites_map$Long.DD.W)
xmax <-  my.max(sites_map$Long.DD.W)
ymin <-  my.min(sites_map$Lat.DD.S)
ymax <-  my.max(sites_map$Lat.DD.S)


map_hist_plt <- 
  southamerica + #create a South America map
  annotation_custom(
    grob = xbp_grob,
    xmin = xmin,
    xmax = xmax,
    ymin = -41,
    ymax = -49.3) +
  annotation_custom(
    grob = ybp_grob,
    xmin = -96,
    xmax = -88,
    ymin = ymin,
    ymax = ymax)
map_hist_plt

#save plots
ggsave("plots/map_sites_histograms_B&W.png", plot = last_plot(),
       height=8, width=10,units="in",
       dpi = 300)

ggsave(plot=southamerica, "plots/sites.png",
       height=8, width=10,units="in",
       dpi = 300)

