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
library(measurements)
library(sf)
library(mapview)
library(neotoma)
library(maps)
library(rwordlmap)
library(ggpubr)


###########################
### Paleolimnological sites
###########################

## read in all published papers in JOPL between 1997-2015 
#(Escobar et al 2020 JOPL https://doi.org/10.1007/s10933-020-00120-6)
jopl_all <- read.csv("data/paleosites/jopl_all.csv", sep = ";") %>%
  drop_na(Latdd) %>%
  mutate(Lat=case_when(Hemisf=="S" ~ Latdd*-1,
                       Hemisf=="N" ~ Latdd*1)) %>%
  mutate(Lat=case_when(Lat>0 ~ paste0("+", Latdd*1),
                       TRUE ~ paste0("-", Latdd))) %>%
  mutate(Long=case_when(Hemisf.1=="W" ~ Longdd*-1,
                        Hemisf.1=="E" ~ Longdd*1)) %>%
  mutate(Long=case_when(Long>0 ~ paste0("+", Longdd*1),
                        TRUE ~ paste0("-", Longdd))) %>%
  unite(Latitude, Lat, Latmm, sep = " ") %>%
  unite(Longitude, Long, Longmm, sep = " ") %>%
  mutate(Latitude = gsub(',', '.', jopl_all$Latitude)) %>%
  mutate(Longitude = gsub(',', '.', jopl_all$Longitude)) %>%
  select(-c(1,22,23,24)) %>%
  filter(str_detect(proxies, "diatom")) %>% #select diatom proxies
  select(number.total.published..number.ID., Title, Latitude, Longitude) %>%
  rename(ID=number.total.published..number.ID.)

# convert from decimal minutes to decimal degrees and transform to numeric
jopl_all$lat <- measurements::conv_unit(jopl_all$Latitude, from = 'deg_dec_min', to = 'dec_deg')
jopl_all$lat <- as.numeric(jopl_all$lat) 
jopl_all$long <- measurements::conv_unit(jopl_all$Longitude, from = 'deg_dec_min', to = 'dec_deg') 
jopl_all$long <- as.numeric(jopl_all$long)

#convert to spatial object
jopl_all_sf <- st_as_sf(jopl_all, coords = c("long", "lat"), 
                        crs = 4326, agr = "constant")

# plot JOPL papers locations
mapview(jopl_all_sf)

## Read in Holocene published papers between 2013-2015 (Escobar et al 2020 JOPL https://doi.org/10.1007/s10933-020-00120-6)
holocene_all <- read.csv("data/paleosites/holocene_all.csv", sep = ";") %>%
  drop_na(Number.tropical) %>%
  separate(Latitude, into = c("latdd", "latmm", "hemisf"), sep = " ", convert = TRUE) %>%
  separate(longitude, into = c("longdd", "longmm", "hemisf.1"), sep = " ", convert = TRUE) %>%
  mutate(Lat=case_when(hemisf=="S" ~ latdd*-1,
                       hemisf=="N" ~ latdd*1)) %>%
  mutate(Lat=case_when(Lat>0 ~ paste0("+", latdd*1),
                       TRUE ~ paste0("-", latdd))) %>%
  mutate(Long=case_when(hemisf.1=="W" ~ longdd*-1,
                        hemisf.1=="E" ~ longdd*1)) %>%
  mutate(Long=case_when(Long>0 ~ paste0("+", longdd*1),
                        TRUE ~ paste0("-", longdd))) %>%
  unite(Latitude, Lat, latmm, sep = " ") %>%
  unite(Longitude, Long, longmm, sep = " ") %>%
  mutate(Latitude = gsub(',', '.', holocene_all$Latitude)) %>%
  mutate(Longitude = gsub(',', '.', holocene_all$Longitude)) %>%
  filter(str_detect(proxies, "diat")) %>% #select diatom proxies
  select(Number.tropical, Title, Latitude, Longitude) %>%
  rename(ID=Number.tropical)
  
  
# convert from decimal minutes to decimal degrees and transform to numeric
holocene_all$lat <- measurements::conv_unit(holocene_all$Latitude, from = 'deg_dec_min', to = 'dec_deg')
holocene_all$lat <- as.numeric(holocene_all$lat) 
holocene_all$long <- measurements::conv_unit(holocene_all$Longitude, from = 'deg_dec_min', to = 'dec_deg') 
holocene_all$long <- as.numeric(holocene_all$long)

#convert to spatial object
holocene_all_sf <- st_as_sf(holocene_all, coords = c("long", "lat"), 
                        crs = 4326, agr = "constant")

#Merge JOPL and Holocene diatom records
diatom_tropics <- rbind(jopl_all, holocene_all) %>%
  filter(between(lat, -40, 40)) #filter to tropical regions
#write.csv(diatom_tropics, "data/diatom_paleorecords_review.csv")


#Merge JOPL and Holocene diatom records

diatom_all_sf <- rbind(jopl_all_sf, holocene_all_sf) 
diatom_all_sf <- st_as_sf(diatom_all, coords = c("long", "lat"), 
                          crs = 4326, agr = "constant")

# plot JOPL and Holocene tropical diatom papers locations
mapview(diatom_all_sf) 


## Get diatom samples from Neotoma paleoecological database 
diatom_datasets <- get_dataset(loc=c(-95, -45, -45, 20), datasettype = "diatom")
#browse(diatom_datasets)

sites <- lapply(diatom_datasets, `[`, "site.data")
sites_neotoma_df <- plyr::ldply(sites, data.frame)
colnames(sites_neotoma_df)[4] <- "long"
colnames(sites_neotoma_df)[5] <- "lat"

# There are two site not returned by the function: Umayo (27539) and Lauca Basin (27498)
umayo <- data.frame("27539", "49470", "Lago Umayo", "-70.178", "-15.742", NA, NA, NA, NA)
names(umayo) <- colnames(sites_neotoma_df)

laucabasin <- data.frame("27498", "49371", "Lauca Basin", "-69.160", "-18.579", "4065", 
                         "A single, 31-m thick-sedimentary section from the Lauca Basin (northern Chilean Altiplano), which contains variably fluvial and lacustrine sedimentary sequences spanning the interval from 8.7 to 2.3 Ma.", NA, NA)
names(laucabasin) <- colnames(sites_neotoma_df)

# rbind
sites_neotoma_df <- rbind(sites_neotoma_df, umayo, laucabasin)

# save dataframe
#write.csv(sites_neotoma_df, "data/diatom_neotoma.csv")

# create a common dataframe for all diatom paleolimnological sites
diatom_paleo_review <- diatom_tropics[,names(diatom_tropics) %in% c("Title", "lat", "long")]
diatom_paleo_review$review <- "Escobaetal2020JOPL"

sites_neotoma_df <- sites_neotoma_df[,3:5]
colnames(sites_neotoma_df)[1] <- "Title"
sites_neotoma_df[1,1] <- "Paleolimnologically inferred eutrophication of a shallow, tropical, urban reservoir in southeast Brazil"
sites_neotoma_df[2,1] <- "Holocene multidecadal- to millennial-scale hydrologic variability on the South American Altiplano"
sites_neotoma_df[3,1] <- "Quaternary glaciation and hydrologic variation in the South American tropics as reconstructed from the Lake Titicaca drilling project"
sites_neotoma_df[4,1] <- "Hydrologic variation during the last 170,000 years in the southern hemisphere tropics of South America"
sites_neotoma_df[5,1] <- "A 700-year record of climate and environmental change from a high Andean lake: Laguna del Maule, central Chile (36°S)"
sites_neotoma_df[6,1] <- "A ~6000 yr diatom record of mid- to late Holocene fluctuations in the level of Lago Winaymarca, Lake Titicaca (Pery/Bolivia)"
sites_neotoma_df[7,1] <- "A ~6000 yr diatom record of mid- to late Holocene fluctuations in the level of Lago Winaymarca, Lake Titicaca (Pery/Bolivia)"
sites_neotoma_df[8,1] <- "A ~6000 yr diatom record of mid- to late Holocene fluctuations in the level of Lago Winaymarca, Lake Titicaca (Pery/Bolivia)"
sites_neotoma_df[9,1] <- "Holocene multidecadal- to millennial-scale hydrologic variability on the South American Altiplano"
sites_neotoma_df[10,1] <- "Paleoclimate of the subtropical Andes during the latest Miocene, Lauca Basin, Chile"
sites_neotoma_df$review <- "Neotoma"
sites_neotoma_df_2 <- sites_neotoma_df[, c(1,3,2,4)] #rearrange columns so that lat goes first than long

df <- rbind(diatom_paleo_review, sites_neotoma_df_2)
write.xlsx(df, "data/diatom_paleolimnologicalsites_tropics.xlsx")
##

#plot paleolimnological sites
world <- map_data("world")

interest <- c("Colombia", "Ecuador", "Peru", "Bolivia", "Chile", "Argentina")
countries <- world %>% filter(str_detect(region, interest))

# Read in contemporary diatom sites from TSADB
sites_map <- read.csv("data/biogeographySites_new.csv", sep=";", stringsAsFactors = FALSE) %>% 
  filter(!region=="Tierra del Fuego" & !Habitat=="channel") %>%
  mutate(Lat.DD.S=as.numeric(gsub(",", ".", gsub("\\.", "", Lat.DD.S)))) %>%
  mutate(Long.DD.W=as.numeric(gsub(",", ".", gsub("\\.", "", Long.DD.W))))

southamerica <- ggplot() +
  geom_polygon(data=world, aes(x=long, y = lat, group =group), fill="lightgrey") +
  geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, colour="black")+
  geom_point(data=sites_map, aes(x=Long.DD.W, y=Lat.DD.S, col=Habitat), shape=20, size=4)+
  #geom_point(data=sites_neotoma_df, aes(x=long, y=lat), shape=17, size=3, colour="green")+
  geom_point(data=df, aes(x=long, y=lat, colour=""), shape=17, size=3, colour="red")+
  scale_color_viridis_d()+
  coord_equal(ylim=c(-45,15), xlim=c(-92,-40))+
  #coord_map("albers", parameters = c(-100, -100),  ylim=c(-40,15), xlim=c(-82,-40)) +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw()
southamerica

