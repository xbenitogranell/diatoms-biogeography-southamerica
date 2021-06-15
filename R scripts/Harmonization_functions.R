#Function modified from biodata_check() by Ian Bishop 2017
#Date: 29 May 2021

diatomTaxa_check <- function(list_filename="", 
                          data_filename="", 
                          diatnames_filename="", 
                          authority_list="",
                          omnidia="") {

  #Libraries
  library(qdapTools)
  library(reshape2)
  library(tidyverse)
  
  #setwd("~/R/diatoms-biogeography-southamerica")
  
  data_dir <- "~/R/diatoms-biogeography-southamerica/data"
  
  #Import diatom taxonomy tropical South America complete taxonomy  
  #diat_names <- readr::read_csv(file = glue::glue("{data_dir}/{diatnames_filename}.csv"))
  
  diat_names <- diat_master
  

    #Import user list of taxa
    #user_list <- readr::read_csv(file = glue::glue("{data_dir}/{list_filename}.csv"))
  
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
  
    #Create new column with spp authorities from Biodata
    diat_authorities_biodata <- BioData
    #diat_authorities <- readr::read_csv(file = glue::glue("{data_dir}/{authority_list}.csv"))
    conversion_df$CurrentDiatomSAname <- gsub("var", "var.", conversion_df$CurrentDiatomSAname, fixed=TRUE)
    
    conversion_df$authority_biodata <- lookup_e(conversion_df$CurrentDiatomSAname, diat_authorities_biodata[,c("BiodataTaxonName","PublishedTaxonAuthority")])
    conversion_df[is.na(conversion_df$authority_biodata)==TRUE,]$authority_biodata <- conversion_df[is.na(conversion_df$authority_biodata)==TRUE,]$UserTaxa

    conversion_df$authority_biodata_original <- lookup_e(conversion_df$UserTaxa, diat_authorities_biodata[,c("BiodataTaxonName","PublishedTaxonAuthority")])
    conversion_df$test <- paste(conversion_df$CurrentDiatomSAname, conversion_df$authority_biodata_original)
    conversion_df$test <- gsub("NA", "", conversion_df$test, fixed=TRUE)
    
    
    #Create new column with spp authorities from Omnidia
    diat_authorities_omnidia <- Omnidia2015_database
    #diat_authorities_omnidia <- readr::read_csv(file = glue::glue("{data_dir}/{omnidia}.csv"))
    
    conversion_df$authority_omnidia <- lookup_e(conversion_df$CurrentDiatomSAname, diat_authorities_omnidia[,c("DENOM3","DENOM")])
    conversion_df[is.na(conversion_df$authority_omnidia)==TRUE,]$authority_omnidia <- conversion_df[is.na(conversion_df$authority_omnidia)==TRUE,]$UserTaxa
    

    #Convert conversion_df to dataframe.
    conversion_df <- data.frame(lapply(conversion_df, as.character), stringsAsFactors=FALSE)
  
    #Export conversion_df to .csv
    # write.csv(conversion_df, "OUTPUT_converted_names.csv", row.names = FALSE)
    
    #Replace diatom taxa updated names and write csv
    colnames(diat) <- conversion_df[,2]
    write.csv(diat, "data/updated_dataset.csv", row.names = TRUE)
    
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

truncAuthor_mod <- function(x){
  gsub("  ", " ", x, fixed=TRUE) #remove double space
  first  <- str_split_fixed(x, " ", n=6)[1] #extract first word
  second <- str_split_fixed(x, " ", n=6)[2] #extract second word
  #third  <- str_split_fixed(x, " ", n=6)[3] #extract third word
  # fourth  <- str_split_fixed(x, " ", n=6)[4] #extract fourth word

  if (grepl("var", x) == TRUE) {
    paste(first, second)
  }
  else if (grepl("f", x) == TRUE) {
    paste(first, second)
  }

  else {paste(first, second)}
  
}
