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
library(cowplot)

#Read in assembled diatom datasets (relative abundances and )
combined <- read.csv("data/assembledspp.csv", row.names=1)
#combined2 <- read.csv("data/assembledspp_new.csv", row.names=1)

# Read in lake regions id
lake_regions <- read.csv("data/regions.csv", row.names = 1, sep=";")
#lake_regions2 <- read.csv("data/regions_new.csv", row.names = 1, sep=";") 

##Merge diatom datasets and regions datasets
modern_lakes <- merge(combined, lake_regions, by="row.names")

#transform dataframe to tidy format
df_thin <- modern_lakes %>%
  gather(key = taxa, value = count, -Row.names, -region)

#import dataframe wiht old and new names to group
changes_nms <- read.csv("data/old_new_nms_trainingset.csv", sep=";", stringsAsFactors = FALSE)
changes_nms <- read.csv("data/old_new_nms_master.csv", sep=";", stringsAsFactors = FALSE)

# Overview of number of diatom taxa
genera_overview <- df_thin %>%   
  mutate(taxa = plyr::mapvalues(taxa, from = changes_nms[,1], to = changes_nms$new_1)) %>%
  separate(taxa, into = c("genera", "sp"), convert = TRUE, remove = FALSE) %>%  
  mutate(genera_f=factor(genera)) %>%
  mutate(genera_f=str_replace(genera_f, "Ammphora", "Amphora"))%>% #fix typos
  mutate(genera_f=str_replace(genera_f, "Achanthes", "Achnanthes")) %>% #fix typos
  mutate(genera_f=str_replace(genera_f, "Discotella", "Discostella")) %>% #fix typos
  mutate(genera_f=str_replace(genera_f, "Encyononema", "Encyonema")) %>% #fix typos
  mutate(genera_f=str_replace(genera_f, "FragilarIa", "Fragilaria")) %>% #fix typos
  mutate(genera_f=str_replace(genera_f, "Trybionella", "Tryblionella")) %>% #fix typos
  mutate(genera_f=str_replace(genera_f, "Eutnota", "Eunotia"))%>%
  filter(!count==0) %>%
  filter(!taxa=="Auxospores") %>%
  group_by(genera_f) %>%
  summarise(n_spp = n_distinct(taxa)) %>%
  mutate(genera=fct_reorder(genera_f, n_spp)) %>% #reorder n_spp
  filter(n_spp>2) #filter out genera with less than 2 species

# How many species?
sum(genera_overview$n_spp) 
length(genera_overview$genera_f) 

# plot genera frequency
genera_plt <- ggplot(genera_overview, aes(x=genera, y=n_spp)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_classic()+
  theme(text = element_text(size=9)) +
  xlab ("Genera") + ylab("Species number")

ggsave("plots/genera_plt.png", plot = genera_plt,
       height=8, width=10,units="in",
       dpi = 300)

# Overview of number of diatom taxa and proportion identified as cf, aff, sp.
spp_overview <- df_thin %>%   
  mutate(taxa = plyr::mapvalues(taxa, from = changes_nms[,1], to = changes_nms$new_1)) %>%
  separate(taxa, into = c("genera", "sp"), convert = TRUE, remove = FALSE) %>%  
  mutate(genera_f=factor(genera)) %>%
  mutate(genera_f=str_replace(genera_f, "Ammphora", "Amphora"))%>% #fix typos
  mutate(genera_f=str_replace(genera_f, "Achanthes", "Achnanthes")) %>% #fix typos
  mutate(genera_f=str_replace(genera_f, "Discotella", "Discostella")) %>% #fix typos
  mutate(genera_f=str_replace(genera_f, "Encyononema", "Encyonema")) %>% #fix typos
  mutate(genera_f=str_replace(genera_f, "FragilarIa", "Fragilaria")) %>% #fix typos
  mutate(genera_f=str_replace(genera_f, "Trybionella", "Tryblionella")) %>% #fix typos
  mutate(genera_f=str_replace(genera_f, "Eutnota", "Eunotia"))%>%
  filter(!count==0) %>%
  filter(!taxa=="Auxospores") %>%
  group_by(genera_f, sp) %>%
  filter(sp %in% c("cf", "aff", "sp")) %>%
  summarise(n_notid = n_distinct(taxa)) %>%
  ungroup() %>%
  group_by(genera_f) %>%
  summarise(n_taxa=sum(n_notid)) %>%
  left_join(genera_overview, by="genera_f") %>%
  mutate(n_spp = replace_na(n_spp, 1)) %>%
  rename(not_identified=n_taxa) %>%
  rename(identified=n_spp)
str(spp_overview)

taxon_spp <- spp_overview %>% gather(taxon,count,identified:not_identified) %>%
  mutate(genera_f=fct_reorder(genera_f, count))
head(taxon_spp)

ggplot(taxon_spp, aes(fill=taxon, y=count, x=genera_f)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T) +
  coord_flip() +
  theme(text = element_text(size=8)) +
  xlab ("Genera") + ylab("Species number")+
  theme_classic()

ggsave("plots/proportion_spp_identified_plt.png", plot = last_plot(),
       height=8, width=10,units="in",
       dpi = 300)

# ## double.check the above step
# test <- df_thin %>% filter(str_detect(taxa,"Microcostatus"))
# levels(factor(test$taxa))

# Diatom taxa with more occurrences
spp_mostoccurrence <- df_thin %>%
  group_by(taxa) %>%
  filter(!count==0) %>%
  count(taxa, sort = TRUE) %>%
  ungroup()


## Read in site's descriptors
sitesDB <- read.csv("data/biogeographySites.csv", sep=";", stringsAsFactors = FALSE) %>%
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


## Read in environmental data
diatom_environment <- read.csv("data/environmental_data.csv", sep=";") %>%
  mutate(lake_depth_ratio=Lake_area/Depth_avg) %>%
  mutate(lake_catch_ratio=Lake_area/Wshd_area) %>%
  mutate(catch_vol_ratio=Wshd_area/Vol_total) %>%
  filter(!lat<= -40) %>% #filter out Tierra del Fuego sites
  rename(Row.names=ï..code) %>%
  magrittr::set_rownames(.$Row.names) %>%
  left_join(sitesDB, by="Row.names") #join with site's information

  
# This is to create a bubble chart showing the most common diatom species across regions and habitats
diatoms_habitat <- df_thin %>%
  mutate(taxa = plyr::mapvalues(taxa, from = changes_nms[,1], to = changes_nms$new_1)) %>%
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
  mutate(taxa_traits = plyr::mapvalues(taxa, from = changes_nms[,1], to = changes_nms$new_2)) %>%
  filter(!taxa_traits=="Auxospores") 


# Make a bubble chart
spp_plt <- ggplot(diatoms_habitat, aes(x = reorder(region.y, -Lat.DD.S), y = fct_rev(taxa))) + #arrange regions by latitude
  geom_point(aes(size = abund, color = Habitat, shape=taxa_traits)) +
  scale_color_viridis_d(option = "D")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 7))+
  ylab("Diatom taxa") + xlab("Regions")
spp_plt

# ggsave("plots/diatoms_regions_traits.png", plot = last_plot(),
#        height=8, width=10,units="in",
#        dpi = 300)


## Summarize metadata of regions
data_summ <- df_thin %>%
  mutate(taxa = plyr::mapvalues(taxa, from = changes_nms[,1], to = changes_nms$new_1)) %>%
  group_by(region, Row.names, taxa) %>%
  summarise(count = sum(count)) %>%
  spread(key = taxa, value = count) %>%
  as.data.frame() %>%
  left_join(sitesDB, by="Row.names") %>% 
  # comment the next two lines for making the bubble plot
  left_join(diatom_environment[,c(1,4:27, 41, 50)], by="Row.names") %>% #here join with certain env variables
  select(-c(CollectionName, Country, Collector.Analyst, region.y, Row.names)) %>%
  gather(key = taxa, value = abund, -Habitat, -Lat.DD.S, -Long.DD.W, -region.x, -Substrate, -Year, -SampleType, -SiteName,
         -pH, -Water.T, -Cond, -Turb, -Chl, -Secchi, -Alkalinity, -Ca, -Mg, -K, -Na, -Si, -Cl, -NO2, 
         -NO3, -SO4, -PO4, -TN, -TP, -DO..,-DO,-Carbonate, -Silicate, -DOC, -Elevation, -Ecoregion) %>%
  mutate(taxa=factor(taxa)) %>%
  filter(!Habitat=="channel") %>% #remove extraneous habitat
  mutate(Habitat=factor(Habitat)) %>%
  #ecological grouping
  mutate(taxa_traits = plyr::mapvalues(taxa, from = changes_nms[,1], to = changes_nms$new_2)) %>%
  filter(!taxa_traits=="Auxospores") #remove extraneous traits

# Here create an elevation/latitude-richness plot
elevation_latitude_plt <- data_summ %>% 
  group_by(SiteName) %>% 
  mutate(Richness = sum(abund > 0)) %>%
  ungroup() %>%
  select(region.x, SiteName, Lat.DD.S, Elevation, Ecoregion, Richness) %>%  
  rename(Region = region.x) %>% #rename region.x column to make it nicer
  mutate(Region=str_replace(Region, "Colombia-Andes-Central", "Colombia-Andes"))%>%
  mutate(Region=str_replace(Region, "Colombia-Andes-Eastern", "Colombia-Andes"))%>%
  mutate(Region=str_replace(Region, "Colombia-Lowlands-North", "Colombia-Lowlands"))%>%
  mutate(Region=str_replace(Region, "Colombia-Lowlands-Eastern", "Colombia-Lowlands"))%>%
  mutate(Region=str_replace(Region, "Colombia-Lowlands-Western", "Colombia-Lowlands")) %>%
  ggplot(aes(x=Lat.DD.S, y=Elevation, colour=Region)) +
  geom_point(aes(size=Richness)) +
  #scale_colour_viridis_d()+
  #scale_colour_brewer(palette = "Set1")+
  theme_classic() +
  xlab("Latitude") + ylab("Elevation (m)")
elevation_latitude_plt

# ggsave("plots/elev_lat_richness.png", plot = last_plot(),
#        height=8, width=10,units="in",
#        dpi = 300)
  
#summarize metadata for each region
library(skimr)
metadata <- data_summ %>%
  mutate(Year=factor(Year)) %>%
  mutate(SampleType=factor(SampleType)) %>%
  mutate(Substrate=factor(Substrate)) %>%
  mutate(SiteName=factor(SiteName)) %>%
  group_by(region.x) %>%
  filter(!abund==0) %>%
  skim() 

#write.csv(metadata, "regions_metadata.csv")

## Make the same without filtering spp for obtaining a list of diatom spp with coordinates
diatoms_list <- df_thin %>%
  mutate(taxa = plyr::mapvalues(taxa, from = changes_nms[,1], to = changes_nms$new_1)) %>%
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



# create a series of plots showing the number of sites per habitat,sampleType and years
# habitats_plt <- diatoms_list %>% 
#   ggplot(aes(x=Habitat)) +
#   geom_bar(stat="count")+
#   #coord_polar("y", start=0) +
#   #labs(x = NULL, y = NULL, fill = NULL, title = "")+
#   scale_y_continuous("count", breaks = 0:12)+
#   scale_fill_viridis_c()+
#   theme_classic()+
#   ggtitle("Habitats")+
#   theme(axis.line = element_blank(),
#         axis.text = element_blank(),
#         legend.text = element_text(size = 10))

# create a series of plots showing the number of sites per habitat,sampleType and years
habitats_plt <- diatoms_list %>% 
  group_by(Habitat) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  ggplot(aes(x=reorder(Habitat,-freq),y=freq)) +
  geom_bar(stat="identity") +
  scale_fill_viridis_d()+
  theme_classic()+
  ggtitle("Habitats") + xlab("") + ylab("Proportion")
  

sampletype_plt <- diatoms_list %>% count(SampleType) %>%
  ggplot(aes(x="", y=n, fill=SampleType)) +
  geom_bar(stat="identity", color="black")+
  coord_polar("y", start=0) +
  labs(x = NULL, y = NULL, fill = NULL, title = "")+
  scale_fill_viridis_d()+
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 10))+
  ggtitle("Sample type")

sampletype_summary <- diatoms_list %>% 
  group_by(SampleType) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  ggplot(aes(x=reorder(SampleType,-freq),y=freq)) +
  geom_bar(stat="identity") +
  scale_fill_viridis_d()+
  theme_classic()+
  ggtitle("Sample Type") + xlab("") + ylab("Proportion")


Year_plt <- diatoms_list %>% count(Year) %>%
  ggplot(aes(x="", y=n, fill=Year)) +
  geom_bar(stat="identity")+
  coord_polar("y", start=0) +
  labs(x = NULL, y = NULL, fill = NULL, title = "")+
  scale_fill_viridis_c()+
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 10))+
  ggtitle("Year of sampling")

Year_summary <- diatoms_list %>% 
  group_by(Year) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100) %>%
  ggplot(aes(x=reorder(Year,-freq),y=freq)) +
  geom_bar(stat="identity") +
  scale_fill_viridis_d()+
  theme_classic()+
  ggtitle("Year") + xlab("") + ylab("Proportion")

## intend to make an area chart showing habitat types per year
df_summary <- sitesDB %>% 
  filter(!region=="Tierra del Fuego" & !Habitat=="channel") %>%
  group_by(Year, Habitat, SampleType) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)*100)

data3 <- arrange(df_summary, Habitat,SampleType, Year)

ggplot(data = data3, aes(x=Year, y=n, fill=Habitat)) +
  geom_bar(stat="identity", width = 1) +
  facet_wrap(~SampleType, scales = "free_y") +
  scale_fill_viridis_d()+
  theme_bw()+
  xlab("Year") + ylab("Number of samples") 

ggsave("plots/sites_plts_summary_new.png", plot = last_plot(),
       height=8, width=10,units="in",
       dpi = 300)

#Arrange multiple plots
plot_grid(habitats_plt, sampletype_plt, Year_plt,
          nrow = 2, ncol = 2,
          labels = "",
          label_size = 12,
          align = "hv")

ggsave("plots/sites_plts_summary.png", plot = last_plot(),
       height=8, width=10,units="in",
       dpi = 300)

#plot modern lake database with marginal histograms and diatom paleolimnological sites
library(maps)
library(rwordlmap)
library(ggpubr)
library(raster)

# Increase current storage capacity
memory.limit(size=56000)

# Get some basemap data
world <- map_data("world")
interest <- c("Colombia", "Ecuador", "Peru", "Bolivia", "Chile", "Argentina")
countries <- world %>% filter(str_detect(region, interest))

# Read in modern TSABD sites
sites_map <- read.csv("data/biogeographySites_new.csv", sep=";", stringsAsFactors = FALSE) %>% 
  filter(!region=="Tierra del Fuego" & !Habitat=="channel") %>%
  mutate(Lat.DD.S=as.numeric(gsub(",", ".", gsub("\\.", "", Lat.DD.S)))) %>%
  mutate(Long.DD.W=as.numeric(gsub(",", ".", gsub("\\.", "", Long.DD.W))))

# Read DEM
DEM <- raster("data/DEM/dem2.bil")
ext<-extent(-92,-30,-45,15)
altmod<-crop(DEM,ext)

#convert the raster to points for plotting
map.p <- rasterToPoints(altmod)

#Make the points a dataframe for ggplot
df <- data.frame(map.p)

#Make appropriate column headings
colnames(df) <- c("long", "lat", "Elevation")


#Read in the paleolimnological sites from 3-paleolimnological_records.R
diatom_paleo_review_df <- read.csv("data/diatom_paleorecords_review.csv", row.names = 1)
diatom_neotoma_df <- read.csv("data/diatom_neotoma.csv", row.names = 1)

# Plot
shapes <- c("JOPL review"=17,"Neotoma"=18)

southamerica <- ggplot(data=df, aes(y=lat, x=long)) +
  geom_raster(aes(fill=Elevation))+
  scale_fill_gradientn(colours = terrain.colors(7)) +
  geom_polygon(data=world, aes(x=long, y = lat, group=group), fill=NA) +
  #geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, colour="black")+
  geom_point(data=sites_map, aes(x=Long.DD.W, y=Lat.DD.S, col=Habitat), shape=20, size=4)+
  scale_color_viridis_d()+
  geom_point(data=diatom_neotoma_df, aes(x=long, y=lat, shape="Neotoma"), colour="red", size=3)+
  geom_point(data=diatom_paleo_review_df, aes(x=long, y=lat, shape="JOPL review"), colour="red", size=3)+
  coord_equal(ylim=c(-45,15), xlim=c(-92,-40))+
  xlab("Longitude (deg)")+
  ylab("Latitude (deg)")+
  guides(fill = guide_colourbar(title="Elevation (m)")) +
  scale_shape_manual(name="Diatom paleorecords",values = shapes) +
  theme_bw()
southamerica

# Create histograms of site distribution to add then into the margins
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
  southamerica + #
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
ggsave("plots/map_sites_histograms_colour.png", plot = last_plot(),
       height=8, width=10,units="in",
       dpi = 300)


