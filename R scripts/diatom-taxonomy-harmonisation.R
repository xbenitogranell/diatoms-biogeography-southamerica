#Load function to update diatom taxon names
source("~/R/diatoms-biogeography-southamerica/R scripts/Harmonization_functions.R") #Benito's adapted function from Bishop and Lee

#Read diatom tropical South America master Taxon List
diat_master <- read.csv("data/Diatomspp_MasterList_June2021.csv", sep = ";")  

### Prepare data to translate diatom names list with taxa authorities
# Read in OMNIDIA 2015 DB (first 4 columns)
Omnidia2015_database <- read.csv("data/Omnidia2015_database.csv", sep = ";")[,1:4]

# Read in Biodata complete taxonomy
BioData <- read.csv("data/BioData_taxonlist.csv", sep = ",")

# Create a new column that removes author names 
Omnidia2015_database$DENOM2 <- sapply(Omnidia2015_database$DENOM, truncAuthor)
Omnidia2015_database$DENOM3 <- sapply(Omnidia2015_database$DENOM2, truncAuthor) #iteration to remove left authorities

#Read in dataset to be harmonized
diat <- read.csv("data/galapagos.csv", row.names = 1, sep = ";")

# assign taxa names to be updated
taxa_names <- data.frame(colnames(diat))

#replace points by space for running diatomTaxa-check function
taxa_names <- data.frame(gsub(".", " ", taxa_names[,1], fixed=TRUE), stringsAsFactors = FALSE) 
taxa_names <- data.frame(gsub("  ", " ", taxa_names[,1], fixed=TRUE), stringsAsFactors = FALSE) 

colnames(taxa_names) <- c("My taxa")
  

# apply diatomTaxa_check()
list <- diatomTaxa_check(list_filename="user_list", diatnames_filename="taxa_names", 
                          authority_list="Biodata",
                          omnidia="Omnidia2015_database")


#Replace diatom taxa updated names and write csv
colnames(diat) <- list[,2]
write.csv(diat, "data/galapagos_updated.csv", row.names = TRUE)

