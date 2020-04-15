

#Import datasets

#Diatom data of individual datasets
cusco <- read.csv("Cusco.csv", row.names = 1) #counts
ecuKan <- read.csv("Ecuador-Kannan.csv", row.names=1) #pres-abs
ecuMic <- read.csv("Ecuador-Micheluti.csv", row.names=1) #relative abund
junin <- read.csv("JuninPlain.csv", row.names=1) #counts
titicpp <- read.csv("TiticacaPP.csv", row.names=1) #counts
titicss <- read.csv("TiticacaSS.csv", row.names=1) #counts
titicws <- read.csv("TiticacaWS.csv", row.names=1) #relative abund
amazonlw <- read.csv("AmazonLW.csv", row.names=1)  #counts
sehuencas <- read.csv("sehuencas.csv", row.names=1)  #counts
sorata <- read.csv("sorata.csv", row.names=1) #pres-abs
lipezVil <- read.csv("Vildary-SudLipez.csv", row.names=1) #relative abund

#Calculate relative abund (when necessary)

#Ecuador Steinitz-Kannan
ecuKan[is.na(ecuKan)] <- 0
rowSums(ecuKan)

#Ecuador Smol-Michelutti
ecuMic[is.na(ecuMic)] <- 0
rowSums(ecuMic)

#Cusco
cusco[is.na(cusco)] <- 0
cusco.num <- data.frame(sapply(cusco, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
total <- apply(cusco, 1, sum)
cusco <- cusco / total * 100

#Junin
junin[is.na(junin)] <- 0
junin.num <- data.frame(sapply(junin, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
row.names(junin.num) <- row.names(junin)
total <- apply(junin, 1, sum)
junin <- junin / total * 100

#Lake Titicaca phytoplankton
titicpp[is.na(titicpp)] <- 0 #replace NAs by 0
titicpp.num <- data.frame(sapply(titicpp, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
row.names(titicpp.num) <- row.names(titicpp)
total <- apply(titicpp.num, 1, sum)
titicpp <- titicpp.num / total * 100

#Lake Titicaca sediment surface
titicss[is.na(titicss)] <- 0 #replace NAs by 0
titicss.num <- data.frame(sapply(titicss, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
row.names(titicss.num) <- row.names(titicss)
total <- apply(titicss.num, 1, sum)
titicss <- titicss.num / total * 100

#Amazon lowlands
amazonlw[is.na(amazonlw)] <- 0
amazonlw.num <- data.frame(sapply(amazonlw, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
row.names(amazonlw.num) <- row.names(amazonlw)
amazonlw <- amazonlw.num
total <- apply(amazonlw, 1, sum)
amazonlw <- amazonlw / total * 100

#Sehuencas streams
sehuencas <- read.csv("sehuencas.csv", row.names=1)  #counts
sehuencas[is.na(sehuencas)] <- 0
sehuencas.num <- data.frame(sapply(sehuencas, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
row.names(sehuencas.num) <- row.names(sehuencas)
total <- apply(sehuencas.num, 1, sum)
sehuencas <- sehuencas.num / total * 100

#Sorata streams
sorata <- read.csv("sorata.csv", row.names=1) #pres-abs

#Colombia Andes Central
ClAndC <- read.csv("ColombiaAndesCentral.csv", row.names=1) #counts
ClAndC[is.na(ClAndC)] <- 0
data.num <- data.frame(sapply(ClAndC, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
row.names(data.num) <- row.names(ClAndC)
total <- apply(data.num, 1, sum)
ClAndC <- data.num / total * 100

#Colombia Andes eastern
ClAndEast <- read.csv("ColombiaAndesEastern.csv", row.names=1) #counts
ClAndEast[is.na(ClAndEast)] <- 0
data.num <- data.frame(sapply(ClAndEast, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
row.names(data.num) <- row.names(ClAndEast)
total <- apply(data.num, 1, sum)
ClAndEast <- data.num / total * 100

#Colombia Lowlands North
ClLowNorth <- read.csv("ColombiaLowlandsNorth.csv", row.names=1) #counts
ClLowNorth[is.na(ClLowNorth)] <- 0
data.num <- data.frame(sapply(ClLowNorth, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
row.names(data.num) <- row.names(ClLowNorth)
total <- apply(data.num, 1, sum)
ClLowNorth <- data.num / total * 100

#Colombia Lowlands Eastern
ClLowEast <- read.csv("ColombiaLowlandsEastern.csv", row.names=1) #counts
ClLowEast[is.na(ClLowEast)] <- 0
data.num <- data.frame(sapply(ClLowEast, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
row.names(data.num) <- row.names(ClLowEast)
total <- apply(data.num, 1, sum)
ClLowEast <- data.num / total * 100

#Colombia Lowlands Western
ClLowWeast <- read.csv("ColombiaLowlandsWeastern.csv", row.names=1) #counts
ClLowWeast[is.na(ClLowWeast)] <- 0
data.num <- data.frame(sapply(ClLowWeast, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
row.names(data.num) <- row.names(ClLowWeast)
total <- apply(data.num, 1, sum)
ClLowWeast <- data.num / total * 100

#GoslingPeru
GoslingPeru <- read.csv("GoslingBird-Peru.csv", row.names=1) #counts
GoslingPeru[is.na(GoslingPeru)] <- 0
GoslingPeru.num <- data.frame(sapply(GoslingPeru, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
row.names(GoslingPeru.num) <- row.names(GoslingPeru)
GoslingPeru <- GoslingPeru.num
total <- apply(GoslingPeru, 1, sum)
GoslingPeru <- GoslingPeru / total * 100

#Bush Peru
BushPeru <- read.csv("BushGuido-Peru.csv", row.names=1) #counts
BushPeru[is.na(BushPeru)] <- 0
total <- apply(BushPeru, 1, sum)
BushPeru <- BushPeru / total * 100

#Bradbury Altiplano
BradAltiplano <- read.csv("Bradbury-SAltiplano.csv", row.names=1) #counts
BradAltiplano[is.na(BradAltiplano)] <- 0
BradAltiplano.num <- data.frame(sapply(BradAltiplano, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
row.names(BradAltiplano.num) <- row.names(BradAltiplano)
BradAltiplano <- BradAltiplano.num
total <- apply(BradAltiplano, 1, sum)
BradAltiplano <- BradAltiplano / total * 100

#Fritz Altiplano
FritzAltiplano <- read.csv("Fritz-Altiplano.csv", row.names=1) #counts
FritzAltiplano[is.na(FritzAltiplano)] <- 0
total <- apply(FritzAltiplano, 1, sum)
FritzAltiplano <- FritzAltiplano / total * 100

#Mayle Pantanal
MaylePantanal <- read.csv("Mayle-Pantanal.csv", row.names=1) #counts
MaylePantanal[is.na(MaylePantanal)] <- 0
total <- apply(MaylePantanal, 1, sum)
MaylePantanal <- MaylePantanal / total * 100

#Challacaba
Challacaba <- read.csv("Challacaba-Bolivia.csv", row.names=1) #counts
total <- apply(Challacaba, 1, sum)
Challacaba <- Challacaba / total * 100

#LacCore samples
LacCore <- read.csv("LacCore-ChileArgentina.csv", row.names=1) #counts
LacCore[is.na(LacCore)] <- 0
data.num <- data.frame(sapply(LacCore, function(x) as.numeric(as.character(x)))) #transform as.numeric dataframe
row.names(data.num) <- row.names(LacCore)
total <- apply(data.num, 1, sum)
LacCore <- data.num / total * 100

#McGlue Brazil
McGlueBrazil <- read.csv("McGlue-Brazil.csv", row.names=1) #counts
McGlueBrazil[is.na(McGlueBrazil)] <- 0
total <- apply(McGlueBrazil, 1, sum)
McGlueBrazil <- McGlueBrazil / total * 100

#Chile Carrevedo
ChileCarrevedo <- read.csv("chile-carrevedo.csv", row.names=1) #relative abundance
ChileCarrevedo[is.na(ChileCarrevedo)] <- 0

#Ecuador NatGeo project
ecuador <- read.csv("ecuador.csv", row.names=1) #counts
ecuador[is.na(ecuador)] <- 0
total <- apply(ecuador, 1, sum)
ecuador_natgeo <- ecuador / total * 100

#read regions ID
lake_regions <- read.csv("data/regions.csv", row.names = 1)

##Merging datasets by unique columns (spp) 
####using rioja's Merge function 
combined <- rioja::Merge(amazonlw, BradAltiplano, BushPeru, Challacaba, ClAndC, ClAndEast, ClLowEast, ClLowNorth, ClLowWeast,
                  cusco, ecuKan, ecuMic, FritzAltiplano, GoslingPeru, junin, LacCore, lipezVil, MaylePantanal, McGlueBrazil, 
                  sehuencas, sorata, titicpp, titicss, titicws,ChileCarrevedo, ecuador_natgeo, split = FALSE) #Argument split=FALSE create dataframe with a common set of merged columns

rowSums(combined)
combined <- combined[rowSums(combined)!=0, ] 

#export dataset
write.csv(combined, "assembledspp.csv") #mix of presence/absence and relative abundances


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

levels(diatomRegions$region)
row.names(diatomRegions) <- diatomRegions$Row.names

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

##extract diatom datasets and write as csv separately
nams <- names(diatomRegionsList)
for (i in seq_along(diatomRegionsList)) {
  assign(paste0("", nams[i]), diatomRegionsList[[i]])
  setwd("~/diatoms-biogeography-southamerica/data/diatom-datasets")
  filename=paste(nams[i],".csv")
  write.csv(diatomRegionsList[[i]], filename)
}


## Sites
sitesDB <- read.csv("data/biogeographySites.csv", stringsAsFactors = FALSE) %>%
  dplyr::select(CollectionName, Country, Collector.Analyst, Year, SiteName, SampleType, Habitat, Substrate,
         code, region, Lat.DD.S, Long.DD.W) %>%
  mutate(region=str_replace(region, "Colombia-Andes-Central", "Colombia-Andes"))%>%
  mutate(region=str_replace(region, "Colombia-Andes-Eastern", "Colombia-Andes"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-North", "Colombia-Lowlands"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-Eastern", "Colombia-Lowlands"))%>%
  mutate(region=str_replace(region, "Colombia-Lowlands-Western", "Colombia-Lowlands"))
  
  

head(sitesDB)
rownames(sitesDB) <- sitesDB$code

str(sitesDB)
unique(sitesDB$SiteName) #317 sites
length(unique(sitesDB$region)) #28 regions

## split data by regions and reassemble
sitesDBList <- split(sitesDB, sitesDB$region)
sitesDBList[[1]] <- NULL
sitesDBList$`Tierra del Fuego` <- NULL

nams <- names(sitesDBList)
for (i in seq_along(sitesDBList)) {
  assign(paste0("", nams[i]), sitesDBList[[i]])
  setwd("~/diatoms-biogeography-southamerica/data/sites-datasets")
  filename=paste(nams[i],".csv")
  write.csv(sitesDBList[[i]], filename)
}

write.csv(unique(sitesDB$region), "~/diatoms-biogeography-southamerica/data/all_regions.csv")

######
#Environmental datasets
environmental_data_lakes <- read.csv("data/environmental_data_lakes.csv") %>%
  mutate(lake_depth_ratio=Lake_area/Depth_avg) %>%
  mutate(lake_catch_ratio=Lake_area/Wshd_area) %>%
  mutate(catch_vol_ratio=Wshd_area/Vol_total)

rownames(environmental_data_lakes) <- environmental_data_lakes$code
names(environmental_data_lakes)

environmental_data_lakes_regions <- merge(environmental_data_lakes, lake_regions, by="row.names") %>%
  select(!Row.names)

## split data by regions and reassemble
ENVRegionsList <- split(environmental_data_lakes_regions, environmental_data_lakes_regions$region)
ENVRegionsList$`Tierra del Fuego` <- NULL

nams <- names(ENVRegionsList)
for (i in seq_along(ENVRegionsList)) {
  assign(paste0("", nams[i]), ENVRegionsList[[i]])
  setwd("~/diatoms-biogeography-southamerica/data/region-datasets")
  filename=paste(nams[i],".csv")
  write.csv(ENVRegionsList[[i]], filename)
}

