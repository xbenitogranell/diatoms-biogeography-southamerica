

#Load function to update diatom taxon names
source("~/R/diatoms-biogeography-southamerica/R scripts/diatomTaxa-check.R") #Benito's function from Bishop


#Read in dataset to be harmonized
diat <- read.csv("data/galapagos.csv", row.names = 1, sep = ";")  

#Read diatom tropical South America complete taxonomy  
diat_master <- read.csv("data/Diatomspp_MasterList_June2021.csv", sep = ";")  


#Save diatom names for taxonomy harmonisation and write csv
taxa_names <- data.frame(colnames(diat))
  
taxa_names <- data.frame(gsub(".", " ", taxa_names[,1], fixed=TRUE), stringsAsFactors = FALSE) #replace points by space for running diatomTaxa-check function
colnames(taxa_names) <- c("My taxa")
  

#function modified from Bishop's biodata_check to accomodate Diatom Master List of South America
#input as list of names
check <- diatomTaxa_check(list_filename="user_list", 
                          diatnames_filename="", 
                          data_type ="List")

  
#Replace diatom taxa updated names and write csv
colnames(diat) <- check[,2]

write.csv(diat, "data/galapagos_updated.csv", row.names = TRUE)

#



