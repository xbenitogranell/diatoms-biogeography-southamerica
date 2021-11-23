---
output:
  word_document: default
  pdf_document: default
---
Database contribution guide
================

## How to share your data for the Tropical South American Diatom Database? 
*Note: this guide is under development and might suffer changes*

The TSADB is intended to be dynamic and constantly evolving with contributions from authors. We are eager to ease the process of sharing diatom datasets and therefore only two minimum criteria are needed to contribute to the TSADB:


### Eligibility
The dataset has to include the following 4 sources of data. **IMPORTANT**: all sites must have a common identifier, named **code**, which are reported in the first column of each of data matrix: diatoms, sites, and environment:

1. <b>Diatoms</b>: a matrix of diatom community data. Accepted formats are relative abundance, absolute counts or presence/absence. Priority is given to absolute counts.
  
2. <b>Sites</b>: a matrix of site information. This must include site name, country, year of sampling, habitat type, substrate, and latitude and longitude coordinates.
  
3. <b>Environment</b>: a matrix of environmental variables, including any type of abiotic characteristics relevant to the diatom community matrix. Priority is given to water physical-chemistry variables (e.g. pH, Conductivity, nutrients, water temperature). Geo-climatic variables can be further extracted using GIS layers as described in <i>Benito et al., under review</i>
  
4. <b>Metadata</b>: a table summarizing sources and descriptions of environmental variables and any type of information relevant to the dataset (e.g. publication/document/report that refers the dataset to)



### Formatting
1. **Format** of the three data matrices: 
      a) Diatoms: species in columns, and sites in rows
      b) Sites: variables in columns, and sites in rows. Aquatic habitats can include lakes, rivers, streams, lagoons, ponds, or springs.
      c) Environment: variables in columns, and sites in rows. Latitude and Longitude must be **WGS84 Geographical Coordinate System**
      d) Metadata: at least three columns must be included, which refers to the environment matrix: variable, unit, and source.  
      

2. **Species list**. We encourage an species list is provided along with the submission which should include all the diatom taxa recorded in the **diatoms** matrix. During the screening of the dataset, a taxonomic harmonization process consisting in steps from `3-diatom-taxonomy-harmonization.R` will be performed to synchronize the new datasets entries with the taxon master list, while reflecting current taxonomy in [**BioData**](https://apps.usgs.gov/biodata/), and [**Omnidia 2015**](https://omnidia.fr/en/).

3. **File formats**. Different formats are accepted:
      a) Excel spreadsheet (.ods, .xls, .xlsx)
      b) Comma or tab separated file (.csv, .txt)
      c) R objects (e.g. list, tibble)


### Sharing your data
The dataset will be screened according to the TSADB's requirements and if it fulfills them, it will be considered for integration to the TSADB. Please send your file(s) + your affiliation + email of contact to xavier.benito.granell[at]gmail.com 


### Connection with Neotoma database
The TSADB is a current database constituent of [**Neotoma**](https://www.neotomadb.org), a global community-curated database by regional experts for multiple types of paleoecological data. The dataset contributor is welcome to use this guide as a preliminary step to make sure their diatom counts and taxonomy meet the standards of data reusability, sharing and reproducibility for the larger diatom community. 

### Contact
Please write us an email at xavier.benito.granell[at]gmail.com for any questions or comments.
