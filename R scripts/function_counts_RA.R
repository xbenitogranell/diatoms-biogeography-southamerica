


#extract core clusters
nams <- names(diatomRegionsList)
for (i in seq_along(diatomRegionsList)) {
  assign(paste0(nams[i]), diatomRegionsList[[i]])
}

# it works
## ULL! there are regions with mix of presence/absence and relative abundance
# pre-process mixed regions
df <- diatomRegionsList[[13]][1:15,]

prop <- apply(diatomRegionsList[[27]], 1, FUN = function(x) {min(x[x > 0])})
prop
fc <- 1/prop
fc
#counts <- data.frame(t(apply(diatomRegionsList[[7]], 2, FUN = function(x) {round(x*fc,0)}))) #only when 1 single observation
counts <- data.frame(apply(diatomRegionsList[[26]], 2, FUN = function(x) {round(x*fc,0)})) #when >1 observations
rowSums(counts)
str(counts)
#row.names(counts) <- row.names(diatomRegionsList[[7]])
#counts2 <- rbind(diatomRegionsList[[15]][1:12,], counts)

##
diatomRegionsList_counts <- list()
diatomRegionsList_counts[[26]] <- counts
diatomRegionsList_counts[[23]] <- diatomRegionsList[[23]]
names(diatomRegionsList_counts) <- nms

rownames(diatomRegionsList_counts[[1]]) <- row.names(diatomRegionsList[[1]])
rownames(diatomRegionsList_counts[[2]]) <- row.names(diatomRegionsList[[2]])
##

#
for (i in seq_along(diatomRegionsList_counts)) {
  assign(paste0("", nams[i]), diatomRegionsList_counts[[i]])
  setwd("~/R/diatoms-biogeography-southamerica/data/diatom-datasets/counts")
  filenamecsv=paste(nams[i],".csv")
  write.csv(diatomRegionsList_counts[[i]], filenamecsv)
  setwd("~/R/diatoms-biogeography-southamerica/data/diatom-datasets/excel/counts")
  filenamexlsx=paste(nams[i],".xlsx")
  write.xlsx(diatomRegionsList_counts[[i]], filenamexlsx)
}

save(diatomRegionsList_counts, file="data/diatomRegionsList_counts.Rdata")

## apply the function to each dataframe in the list
prop <- lapply(diatomRegionsList, FUN = function(x) {min(x[x > 0])})
prop
fc <- lapply(prop, FUN = function(x) {1/x})

convert <- function(i, regions, ...) {
  region <- diatomRegionsList[[i]]
  prop <- min(region[region>0])
  fc <- 1/prop
  # counts <- region*fc
}

counts <- lapply(seq_along(diatomRegionsList), convert, regions=diatomRegionsList)

##
