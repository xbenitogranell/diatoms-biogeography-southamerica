###############################################
#contact email: xavier.benito.granell@gmail.com 
###############################################

#Function modified from biodata_check() by Ian Bishop 2017; Date: 29 May 2021
diatom_dictionary <- function(diatnames="", 
                             biodata="",
                             omnidia="",
                             eddi="",
                             neotoma="") {
  
  library(tidyverse)
  library(stringr)
  library(qdapTools)
  #library(algaeClassify) #something is wrong with this package as it doesn't realese anything
  library(worrms)
  #library(xlsx)
  library(writexl)
  

  #Add user list to new conversion dataframe
  conversion_df <- user_list
  #names(conversion_df) <- "user_taxa"
  names(conversion_df) <- c("user_taxa")
  
  #Compare user names to Omnidia taxon list
  diat_authorities_omnidia <- Omnidia2015_noAuthorities

  # make sure var. and forma. is written correctly: var. and fo.
  conversion_df$user_taxa_lev1 <- gsub(conversion_df[,1], pattern=" var ", replacement=" var. ", fixed = TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" cf ", replacement=" cf. ", fixed=TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" for. ", replacement=" fo. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" sp ", replacement=" sp. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern="spp(?!\\.)", replacement="spp.", perl =TRUE)
  conversion_df$user_taxa_lev1 <- gsub(conversion_df$user_taxa_lev1, pattern=" aff ", replacement=" aff. ", fixed =TRUE)
  conversion_df$user_taxa_lev1 <- trimws(conversion_df$user_taxa_lev1)
  
  # 
  conversion_df$omnidia_names_lev1 <- lookup_e(conversion_df$user_taxa_lev1, diat_authorities_omnidia[,c("DENOM_lev1","DENOM")])
  
  # this is OMNIDIA names without authority
  conversion_df$omnidia_names_lev2 <- lookup_e(conversion_df$omnidia_names_lev1, diat_authorities_omnidia[,c("DENOM","DENOM_lev1")])
  
  #Compare if user names have OMINIDA codes
  conversion_df$omnidia_code <- lookup_e(conversion_df$omnidia_names_lev1, diat_authorities_omnidia[,c("DENOM","CODE")])

  # here add latest accepted syn and then others
  conversion_df$omnidia_latest_syn <- lookup_e(conversion_df$omnidia_names_lev1, diat_authorities_omnidia[,c("DENOM","MOST_RECENT_SYN")])
  conversion_df$name_OMNIDIA_SYN <- lookup_e(conversion_df$omnidia_latest_syn, diat_authorities_omnidia[,c("CODE","DENOM")]) 
  #conversion_df$name_OMNIDIA_SYN <- lookup_e(conversion_df$name_OMNIDIA_SYN, diat_authorities_omnidia[,c("CODE","DENOM")])
  
  conversion_df$other_synonims_OMINIDIA <- lookup_e(conversion_df$omnidia_names_lev1, diat_authorities_omnidia[,c("DENOM","SYN")])
  
  ## work with other synonyms in OMNIDIA
  for (i in 1:length(conversion_df$other_synonims_OMINIDIA)) {
    
    # The regular expression pattern found in the column SYN of the OMNIDIA db
    pattern <- "(?<==)[A-Z]{4}\\b"
    
    # Extract all matches
    extracted_syn <- unlist(str_extract_all(conversion_df$other_synonims_OMINIDIA[i], pattern))
    
    # Join the words with a slash
    result_syn <- str_c(extracted_syn, collapse = "/")
    
    # Print the result
    print(result_syn)
    
    # combine with the main df
    conversion_df$other_synonims_OMINIDIA[i] <- result_syn
    
  }

  
  #Create new column with spp authorities from Biodata
  diat_authorities_biodata <- BioData
    
  #Replace var for var. to lookup at Biodata
  conversion_df$biodata_name <- lookup_e(conversion_df$user_taxa_lev1, diat_authorities_biodata[,c("BenchTaxonName","BiodataTaxonName")])
  conversion_df$authority_biodata <- lookup_e(conversion_df$biodata_name, diat_authorities_biodata[,c("BenchTaxonName","PublishedTaxonAuthority")])
  
  #Create new column with spp info from EDDI
  diat_edi <- eddi_diatoms
  
  #Create new column with spp found in EDDI
  conversion_df$eddi_code <- lookup_e(conversion_df$user_taxa_lev1, diat_edi[,c("TaxonName","TaxonId")])
  conversion_df$eddi_name <- lookup_e(conversion_df$eddi_code, diat_edi[,c("TaxonId","TaxonName")])
  
  #Create new column with spp info from Neotoma diatom master list
  diat_neotoma <- Neotoma
  
  #look up at Neotoma and add names and codes
  conversion_df$neotoma_code <- lookup_e(conversion_df$user_taxa_lev1, diat_neotoma[,c("taxonname","taxoncode")])
  conversion_df$neotoma_name <- lookup_e(conversion_df$neotoma_code, diat_neotoma[,c("taxoncode","taxonname")])
  
  
  # Flag all conversions that are NA
  #conversion_df$notFound <- NA
  #conversion_df[is.na(conversion_df$names_ominidia_lev1)==TRUE & is.na(conversion_df$names_ominidia_lev2)==TRUE & is.na(conversion_df$biodata_name)==TRUE & is.na(conversion_df$AphiaID)==TRUE,]$notFound <- "Name not Found in any database"
  #conversion_df[is.na(conversion_df$names_ominidia_lev1)==TRUE & is.na(conversion_df$names_ominidia_lev2)==TRUE & is.na(conversion_df$biodata_name)==TRUE,]  <- "Name not Found in any database"
  
  #Convert conversion_df to dataframe.
  conversion_df <- data.frame(lapply(conversion_df, as.character), stringsAsFactors=FALSE)
  #conversion_df$code_OMNIDIA_lev1 <- coalesce(conversion_df$code_OMNIDIA_lev1, conversion_df$code_BiodataxOmnidia)
  
  #Replace NAs in code_OMNIDIA_lev1 column with original user name.
  conversion_df[is.na(conversion_df$omnidia_code)==TRUE,]$omnidia_code <- conversion_df[is.na(conversion_df$omnidia_code)==TRUE,]$user_taxa_lev1

  # Create a new column changing user taxon names: post-harmonization (aka after the upload)
  #  agg and aff. elevate to species
  conversion_df$harmoniz_lev1 <- gsub(conversion_df$omnidia_names_lev2, pattern="(aff\\.).*", replacement="\\1") #there is no aff. in EDDI taxon list
  conversion_df$harmoniz_lev1 <- gsub(conversion_df$harmoniz_lev1, pattern="\\(agg\\.)", replacement="") #remove the pattern (agg.) and everything afterwards
  conversion_df$harmoniz_lev1 <- gsub(conversion_df$harmoniz_lev1, pattern="\\(.*?\\)", replacement="")
  conversion_df$harmoniz_lev1 <- gsub(conversion_df$harmoniz_lev1, pattern="\\+.*", replacement="\\1")
  conversion_df$harmoniz_lev1 <- gsub(conversion_df$harmoniz_lev1, pattern="  ", replacement=" ", fixed = TRUE)
  
  # 
  # removes whitespace from start and end of string
  conversion_df$harmoniz_lev1 <- str_squish(conversion_df$harmoniz_lev1)
  
  #Replace NAs in harmoniz_lev1 column with names onmidia lev2
  conversion_df[is.na(conversion_df$harmoniz_lev1)==TRUE,]$harmoniz_lev1 <- conversion_df[is.na(conversion_df$harmoniz_lev1)==TRUE,]$omnidia_code
  
  # now spp and genera look-up WORMMS
  conversion_df$harmoniz_lev2_worrms <- conversion_df$harmoniz_lev1

  conversion_df$harmoniz_lev2_worrms <- gsub(conversion_df$harmoniz_lev2_worrms, pattern="cf.", replacement=" ",)
  conversion_df$harmoniz_lev2_worrms <- gsub(conversion_df$harmoniz_lev2_worrms, pattern="  ", replacement="",)
  conversion_df$harmoniz_lev2_worrms <- gsub(conversion_df$harmoniz_lev2_worrms, pattern="var\\..*", replacement="", fixed = TRUE) #remove the pattern var. and everything afterwards
  conversion_df$harmoniz_lev2_worrms <- gsub(conversion_df$harmoniz_lev2_worrms, pattern="fo\\..*", replacement="", fixed = TRUE) #remove the pattern fo. and everything afterwards
  conversion_df$harmoniz_lev2_worrms <- gsub(conversion_df$harmoniz_lev2_worrms, pattern="sp\\..*", replacement="", fixed = TRUE) #remove the pattern sp. and everything afterwards
  conversion_df$harmoniz_lev2_worrms <- gsub(conversion_df$harmoniz_lev2_worrms, pattern="sp.\\..*", replacement="", fixed=TRUE) #remove the pattern sp. and everything afterwards
  conversion_df$harmoniz_lev2_worrms <- gsub(conversion_df$harmoniz_lev2_worrms, pattern="sp\\. .*", replacement="") #remove the pattern sp. and everything afterwards
  conversion_df$harmoniz_lev2_worrms <- sub(conversion_df$harmoniz_lev2_worrms, pattern="spp.*", replacement="") #remove the pattern spp. and everything afterwards
  #conversion_df$harmoniz_lev2_worrms <- sub(conversion_df$harmoniz_lev2_worrms, pattern="sp.*", replacement="") #remove the pattern sp. and everything afterwards
  
  # Keep only the first two words so we only have species level (or genera)
  conversion_df$harmoniz_lev2_worrms <- word(conversion_df$harmoniz_lev2_worrms, 1,2) #to ensure only the first two sentences are kept (if any)

  conversion_df$harmoniz_lev2_worrms <- trimws(conversion_df$harmoniz_lev2_worrms)
  
  # #Get taxa information from the worms package
  ## IMportant: remove all inconsistencies and var. fo. first

  # WORRMS loop
  # d <- seq_along(conversion_df$harmoniz_lev2_worrms) #create an index for each taxa name
  # d2 <- split(conversion_df$harmoniz_lev2_worrms, d) #make a list--one element for each spp
  # results <- list() #initialize the list where to store the results
  # # #
  # #
  # # # make a loop using try() to skip errors (i.e., those taxa not found)
  # for (i in seq_along(d2)) {
  #   x <- d2[[i]]
  #   results[[i]]<- try(wm_records_name(x, fuzzy=TRUE,marine_only=FALSE), silent = TRUE) #return varieties
  #   #results[i] <- try(wm_records_names(x, marine_only=FALSE), silent = TRUE)
  #   #results[i] <- try(wm_records_taxamatch(x, marine_only=FALSE), silent = TRUE)
  #   #fuzzy=TRUE  include varietes etc. only work with wm_records_name
  # }
  # #
  # # # extract the results from the list
  # worms <- do.call(rbind, results)
  # worms <- worms[!grepl("Error", worms$AphiaID),]
  # worms$AphiaID <- as.integer(worms$AphiaID)
  # # # #
  # # # # #
  # # # # # # Create table with variables needed--> valid names
  # worms <- as.data.frame(worms)
  # dtmBase <- worms %>%
  #   filter(status=="accepted") %>%
  #   dplyr::select("AphiaID", "scientificname", "valid_name", "valid_authority")
  # 
  # # # # #
  # # # Get source information for the set of AphiaIDs created in the  code chunk above (from Don Charles)
  # dtmBase_source <- wm_sources_(id = dtmBase$AphiaID)
  # # # Returns sources for several variables, one of which is specified as "original description" in the "use" column
  # # # Create an object with source for "original description" only
  # dtm_Base_origDes <- filter(dtmBase_source, use == "original description")
  # # # Limit variables / columns to those most relevant
  # dtm_Base_origDes1 <- dplyr::select(dtm_Base_origDes, id ,reference, page, doi)
  # # # Change "id" column from character to integer type
  # dtm_Base_origDes1$id <- as.integer(dtm_Base_origDes1$id)
  # # #
  # # # Combine main taxa table (dtmBase_out) and source table (dtm_Base_origDes1)
  # dtm_Base_comb <- left_join(dtmBase, dtm_Base_origDes1, by = c("AphiaID" = "id"))
  # 
  # # # # # # Combine worms table with the original conversion_df table
  # conversion_df <- left_join(conversion_df, dtm_Base_comb, by = c("harmoniz_lev2_worrms" = "valid_name"))

  # #
  # #

  # add dataset name
  #conversion_df$dataset <- paste0(Id,"_", Author,"_",Site)

  # # Create an Excel file
  #write_xlsx(conversion_df, paste0("harmonized_list_",datasetId, ".xlsx"))
  # 
  return(conversion_df)
  
  
}

# This function removes all text after the specific epithet or variety or form, etc.
truncAuthor <- function(x){
  first  <- str_split_fixed(x, " ", n=7)[1] #extract first word
  second <- str_split_fixed(x, " ", n=7)[2] #extract second word
  third  <- str_split_fixed(x, " ", n=7)[3] #extract third word
  fourth <- str_split_fixed(x, " ", n=7)[4] #extract fourth word
  fifth  <- str_split_fixed(x, " ", n=7)[5] #extract fifth word
  sixth <-  str_split_fixed(x, " ", n=7)[6]
  seventh <-  str_split_fixed(x, " ", n=7)[7]
  
  if (grepl(" var. ", x) == TRUE) {
    paste(first, second, third, fourth)
  }
  else if (grepl(" fo. ", x) == TRUE) {
    paste(first, second, third, fourth)
  }
  else if (grepl(" sp. . .", x) == TRUE) {
    paste(first, second, third, fourth, fifth)
  }
  
  else if (grepl(" cf. ", x) == TRUE) {
    paste(first, second, third)
  }

  else if (grepl(" aff. ", x) == TRUE) {
    paste(first, second, third)
  }
  
  else {paste(first, second)}
}


# Species case function
capitalize <- function(x){
  first <- toupper(substr(x, start=1, stop=1)) ## capitalize first letter
  rest <- tolower(substr(x, start=2, stop=nchar(x)))   ## everything else lowercase
  paste0(first, rest)
}

