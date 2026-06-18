library(tidyverse)
library(openxlsx)

# Set the paths of the repository
scriptpath <- dirname(rstudioapi::getSourceEditorContext()$path) # path of current script file
source(paste0(scriptpath,"/nomenclature_harmonisation_functions.R"))
path_repo_root <- dirname(scriptpath) # path to the cloned repo
L0datapath <- file.path(path_repo_root, "taxonomic_lists")

# read external diatom lists
setwd(L0datapath)
# Read in Biodata complete taxonomy
BioData <- read.csv("BioData_taxonlist.csv", sep = ";")
# Read in Mertens et al. 2025 Indicator Values Dutch diatoms
#Mertens <- read.csv("Mertens2025_indval.csv", sep = ";")
# read in Omnidida 2015 database (cleaned)
Omnidia2015_noAuthorities <- read.csv("Omnidia2015_noAuthorities.csv", sep = ",", row.names = 1) %>%
  dplyr::rename(MOST_RECENT_SYN=NOUV..APPE)
# read full synonyms OMNIDIA table
Omnidia_full_synonyms_table <- read.csv("Omnidia_full_synonyms_table.csv")
# read in Neotoma diatom master list (Simon Goring May 2024)
Neotoma <- read.csv("neotoma_diatoms_20240528.csv", sep = ",", row.names = 1)
# read in EDDI diatom master list
eddi_diatoms <- read.csv("eddi_taxon_list.csv", sep = ";")

## Read in from the clipboard; most direct way of I have the excel file open
user_list <- read.table("clipboard", sep = "\t")
print(user_list)

user_list <- user_list %>%
  mutate(user_taxa=gsub(".", " ", V1, fixed = TRUE)) %>%
  select(-1)

# Apply diatom_dictionary()
# conversion_table will be the look up table to harmonize 
conversion_table <- diatom_dictionary(diatnames="user_list", 
                                      biodata="BioData",
                                      omnidia="Omnidia2015_noAuthorities",
                                      eddi="eddi_diatoms",
                                      neotoma="Neotoma")

# save the conversion table
write.xlsx(conversion_table, "conversion_table.xlsx")
