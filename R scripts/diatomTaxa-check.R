#Function modified from biodata_check() by Ian Bishop 2017
#Date: 29 May 2021

diatomTaxa_check <- function(list_filename="", 
                          data_filename="", 
                          diatnames_filename="", 
                          data_type=c("List", "Dataframe")) {

  #set WD
  #setwd("~/R/diatoms-biogeography-southamerica/data")
  
  #Libraries
  library(qdapTools)
  library(reshape2)
  
  #Import diatom taxonomy tropical South America complete taxonomy  
  #diat_names <- read.csv(paste(diatnames_filename, ".csv", sep=";"), header=TRUE, stringsAsFactors = FALSE)
  
  diat_names <- diat_master
  
  if (data_type=="List") {

    #Import user list of taxa
    #user_list <- read.csv(paste(list_filename, ".csv", sep=";"), header=TRUE, stringsAsFactors = FALSE)
    
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
  
   
    #Convert conversion_df to dataframe.
    conversion_df <- data.frame(lapply(conversion_df, as.character), stringsAsFactors=FALSE)
  
    #Export conversion_df to .csv
    # write.csv(conversion_df, "OUTPUT_converted_names.csv", row.names = FALSE)
    
    return(conversion_df)
  
  } else if(data_type=="Dataframe") {
    
    #Import user wide format tidy dataset  
    user_data <- read.csv(paste(data_filename, ".csv", sep=""), check.names = FALSE, stringsAsFactors=FALSE, header=TRUE, fileEncoding="UTF-8-BOM")
    
    #Create local variable for imported data
    working_wide <- user_data
    
    #Convert working_wide to long format
    working_wide <- melt(working_wide, value.name="Count", variable.name="Taxon", id.vars="Sample")
    
    #Add BiodataTaxon column of current Biodata names
    working_wide$BiodataTaxon <- lookup_e(working_wide$Taxon, diat_names[,c("species_original", "species_harmonised")])
    
    
    
    #Name translation sheet
    working_translation <- working_wide[,c("Taxon", "BiodataTaxon")]
    working_translation <- unique(working_translation)
    
    #Add column for change notes
    working_translation$ConversionNotes <- NA
    working_translation[is.na(working_translation$BiodataTaxon)==FALSE,]$ConversionNotes <- "Name Updated to Diatom SA Master List"
    working_translation[is.na(working_translation$BiodataTaxon)==TRUE,]$ConversionNotes <- "Name Not Found In Diatom SA Master List"
    working_translation$ConversionNotes[as.character(working_translation$BiodataTaxon)==as.character(working_translation$Taxon)] <- "Name Already Current"
    
    #Fill NAs in working_translation$BiodataTaxon with Taxon
    working_translation$BiodataTaxon[is.na(working_translation$BiodataTaxon)] <- as.character(working_translation$Taxon[is.na(working_translation$BiodataTaxon)])
    
    
    
    #Fill in BiodataTaxon NAs with original taxon.
    working_wide$BiodataTaxon[is.na(working_wide$BiodataTaxon)] <- as.character(working_wide$Taxon[is.na(working_wide$BiodataTaxon)])
    
    #Collected list of names that have been changed
    changed_names <- as.data.frame(unique(working_wide[working_wide$Taxon != working_wide$BiodataTaxon,]$Taxon))
    colnames(changed_names) <- "Changed Names"
    
    
    
    #Replace NAs in BiodataTaxon column with original Taxon.
    working_wide$Taxon <- working_wide$BiodataTaxon
    #Remove BiodataTaxon column
    working_wide$BiodataTaxon <- NULL
    
    #Collapse any species values that are now synonymized
    working_wide <- aggregate(Count ~ Sample + Taxon, FUN = sum, data=working_wide)
    
    #Convert from long format back to wide format
    working_wide <- dcast(working_wide, Sample ~ Taxon)
    
    #Export working_wide to .csv
    # write.csv(working_wide, "OUTPUT_renamed_data.csv", row.names = FALSE)
    # #export changed name list
    # write.csv(changed_names, "OUTPUT_name_changes.csv", row.names = FALSE)
    
    output<-list(UpdatedDataset = working_wide, ChangedNames = changed_names, NameTranslationSheet=working_translation)
    return(output)
    
    #Return error message if data_type string is misspelled
  } else { return(print("ERROR: Please choose either 'List' or 'Dataframe' for data_type"))}
}
