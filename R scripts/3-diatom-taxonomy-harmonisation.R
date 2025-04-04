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

#load packages
library(tidyverse)
library(stringr)
library(qdapTools)
library(algaeClassify) #something is wrong with this package as it doesn't realese anything
library(worrms)
library(xlsx)

#####################
## Functions start ##
#####################

#Function modified from biodata_check() by Ian Bishop 2017; Date: 29 May 2021
diatom_authority <- function(list_filename="", 
                               data_filename="", 
                               diatnames_filename="", 
                               authority_list="",
                               omnidia="") {
  
  diat_names <- diat_master
  user_list <- taxa_names
  
  #Add user list to new conversion dataframe
  conversion_df <- user_list
  names(conversion_df)[1] <- "UserTaxa"
  
  #Compare user names to Master list taxon harmonised, 
  conversion_df$CurrentDiatomSAname <- lookup_e(conversion_df[,1], diat_names[,c("species_original", "species_harmonised")])
  
  #Add column for change notes
  conversion_df$ConversionNotes <- NA
  conversion_df[is.na(conversion_df$CurrentDiatomSAname)==FALSE,]$ConversionNotes <- "Name Updated to Diatom SA Master List"
  conversion_df[is.na(conversion_df$CurrentDiatomSAname)==TRUE,]$ConversionNotes <- "Name Not Found In Diatom SA Master List"
  conversion_df$ConversionNotes[as.character(conversion_df$CurrentDiatomSAname)==as.character(conversion_df$UserTaxa)] <- "Name Already Current"
  
  #Replace NAs in CurrentDiatomSAname column with original user name.
  conversion_df[is.na(conversion_df$CurrentDiatomSAname)==TRUE,]$CurrentDiatomSAname <- conversion_df[is.na(conversion_df$CurrentDiatomSAname)==TRUE,]$UserTaxa
  
  #create new column with updated names matching to algaebase online database
  # algaebase <- spp_list_algaebase(conversion_df, phyto.name="CurrentDiatomSAname",long=FALSE,write=FALSE)
  # conversion_df$algaebase_synonyms <- algaebase$synonyms
  # conversion_df$algaebase_match <- algaebase$match.name
  # 
  #Create new column with spp authorities from Biodata
  #Replace var. for var to lookup
  diat_authorities_biodata <- BioData
  conversion_df$CurrentDiatomSAname <- gsub("var", "var.", conversion_df$CurrentDiatomSAname, fixed=TRUE)
  conversion_df$biodata_name <- lookup_e(conversion_df$CurrentDiatomSAname, diat_authorities_biodata[,c("ï..BenchTaxonName","BiodataTaxonName")])
  conversion_df$authority_biodata <- lookup_e(conversion_df$biodata_name, diat_authorities_biodata[,c("ï..BenchTaxonName","PublishedTaxonAuthority")])
  
  # 
  # #Replace NAs in CurrentDiatomSAname column with original user name.
  conversion_df[is.na(conversion_df$authority_biodata)==TRUE,]$authority_biodata <- conversion_df[is.na(conversion_df$authority_biodata)==TRUE,]$UserTaxa
  
  # 
  # # #Compare updated names (from Master list taxon) to algaebase and get the authority from Biodata
  # conversion_df$authority_algaebase_biodata <- lookup_e(conversion_df$algaebase_match, diat_authorities_biodata[,c("BiodataTaxonName","PublishedTaxonAuthority")])
  # conversion_df[is.na(conversion_df$authority_algaebase_biodata)==TRUE,]$authority_algaebase_biodata <- conversion_df[is.na(conversion_df$authority_algaebase_biodata)==TRUE,]$UserTaxa
  # # 
  
  # #Create new column with spp authorities from Omnidia
  diat_authorities_omnidia <- Omnidia2015_database
  conversion_df$CurrentDiatomSAname <- gsub("var", "var.", conversion_df$CurrentDiatomSAname, fixed=TRUE)
  conversion_df$authority_omnidia <- lookup_e(conversion_df$CurrentDiatomSAname, diat_authorities_omnidia[,c("DENOM3","DENOM")])
  
  #Replace NAs in CurrentDiatomSAname column with original user name.
  conversion_df[is.na(conversion_df$authority_omnidia)==TRUE,]$authority_omnidia <- conversion_df[is.na(conversion_df$authority_omnidia)==TRUE,]$UserTaxa
  
  #Compare updated names (from Master list taxon) to algaebase and get the authority from Omnidia
  # conversion_df$authority_algaebase_omnidia <- lookup_e(conversion_df$algaebase_match, diat_authorities_omnidia[,c("DENOM3","DENOM")])
  # conversion_df[is.na(conversion_df$authority_algaebase_omnidia)==TRUE,]$authority_algaebase_omnidia <- conversion_df[is.na(conversion_df$authority_algaebase_omnidia)==TRUE,]$UserTaxa
  
  #Get taxa information from the worms package
  d <- seq_along(conversion_df$CurrentDiatomSAname) #create an index for each taxa name
  d2 <- split(conversion_df$CurrentDiatomSAname, d) #make a list--one element for each spp
  results <- list() #initialize the list where to store the results
  #

  # make a loop using try() to skip errors (i.e., those taxa not found)
  for (i in seq_along(d2)) {
    x <- d2[[i]]
    results[i] <- try(wm_records_names(x, fuzzy=FALSE), silent = TRUE)
  }

  # extract the results from the list
  worms <- do.call(rbind, results) 

  # Create table with variables needed 
  dtmBase <- select(worms, "AphiaID", "scientificname", "authority","status","citation", "rank", "genus", "family", "valid_AphiaID", "valid_name", "valid_authority")
  
  # Combine worms table with conversion_df table
  conversion_df <- left_join(conversion_df, dtmBase, by = c("CurrentDiatomSAname" = "valid_name"))
  
  #Convert conversion_df to dataframe.
  conversion_df <- data.frame(lapply(conversion_df, as.character), stringsAsFactors=FALSE)
  
  # Create an Excel file
  write.xlsx(conversion_df, "data/diatom_master_taxon_list_authorities.xlsx", row.names = TRUE)

  return(conversion_df)
  
}

# This function removes all text after the specific epithet or variety or form, etc.
truncAuthor <- function(x){
  first  <- str_split_fixed(x, " ", n=6)[1] #extract first word
  second <- str_split_fixed(x, " ", n=6)[2] #extract second word
  third  <- str_split_fixed(x, " ", n=6)[3] #extract third word
  fourth <- str_split_fixed(x, " ", n=6)[4] #extract fourth word
  fifth  <- str_split_fixed(x, " ", n=6)[5] #extract fifth word
  
  if (grepl(" var. ", x) == TRUE) {
    paste(first, second, third, fourth)
  }
  else if (grepl(" fo. ", x) == TRUE) {
    paste(first, second, third, fourth)
  }
  else if (grepl(" sp. . .", x) == TRUE) {
    paste(first, second, third, fourth, fifth)
  }
  else {paste(first, second)}
}

###################
## Functions End ##
###################

#Read diatom tropical South America master Taxon List
diat_master <- read.csv("data/Diatomspp_MasterList_June2021.csv", sep = ";")  

### Prepare data to translate diatom names list with taxa authorities
# Read in OMNIDIA 2015 DB (first 4 columns) -- this dataset can't be shared in a public repository; contact me for a copy
Omnidia2015_database <- read.csv("data/Omnidia2015_database.csv", sep = ";")[,1:4]

# Read in Biodata complete taxonomy
BioData <- read.csv("data/BioData_taxonlist.csv", sep = ";")

# prepare Omnidia database to split diatom names and authorities
Omnidia2015_database$DENOM2 <- sapply(Omnidia2015_database$DENOM, truncAuthor)
Omnidia2015_database$DENOM3 <- sapply(Omnidia2015_database$DENOM2, truncAuthor) #iteration to remove left authorities

#Read in dataset(s) to be harmonized
diat <- read.csv("data/galapagos.csv", row.names = 1, sep = ";")
#diat <- read.csv("data/assembledspp_new.csv", row.names=1)

#transform dataframe to tidy format
df_thin <- diat %>%
  gather(key = taxa, value = count)#don't gather region

#import dataframe wiht old and new names to group
#changes_nms <- read.csv("data/old_new_nms_master.csv", sep=";", stringsAsFactors = FALSE)
changes_nms <- read.csv("data/old_new_nms_master_revised.csv", sep=";", stringsAsFactors = FALSE)


#spread & fix some taxonomic errors with updated revised harmonized taxonomy
diat <- df_thin %>%
  mutate(taxa = plyr::mapvalues(taxa, from = changes_nms[,1], to = changes_nms$revised_harmonized_taxon_name))%>%
  mutate(taxa=str_replace(taxa, "Bacteriastrumÿhyalinum", "Bacteriastrum.hyalinum"))%>%
  mutate(taxa=str_replace(taxa, "Cyclostephanos.sp.1.ENCAÃ.ADO", "Cyclostephanos.sp.1.ENCANADO"))%>%
  #mutate(taxa = plyr::mapvalues(taxa, from = changes_nms[,2], to = changes_nms$new_1))%>%
  mutate(taxa=factor(taxa))

# assign taxa names to be updated
taxa_names <- data.frame(levels(diat$taxa))

#replace points by space for running diatomAuthority function
#taxa_names <- data.frame(gsub("f.", "fo.", taxa_names[,1], fixed=TRUE), stringsAsFactors = FALSE) 
taxa_names <- data.frame(gsub(".", " ", taxa_names[,1], fixed=TRUE), stringsAsFactors = FALSE) 
taxa_names <- data.frame(gsub("  ", " ", taxa_names[,1], fixed=TRUE), stringsAsFactors = FALSE) 

colnames(taxa_names) <- c("diat_name")
#taxa_names <- taxa_names %>% top_n(40) #test the first 10

# Apply diatom_authority()
list <- diatom_authority(list_filename="user_list", 
                         diatnames_filename="taxa_names", 
                         authority_list="Biodata",
                         omnidia="Omnidia2015_database")


#Replace diatom taxa updated names and write csv
#colnames(diat) <- list[,2]
#write.xlsx(list, "data/diatom_master_taxon_list_authorities.xlsx", row.names = TRUE)

