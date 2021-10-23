

df <- read.csv("data/TiticacaWS.csv", row.names=1)

# convert <- function(x) {
#   Small(sum(x == 0, na.rm=TRUE) + 1)
#   return(x)
# }

# it works
prop <- apply(df, 1, FUN = function(x) {min(x[x > 0])})
prop
fc <- 1/prop
counts <- apply(df, 2, FUN = function(x) {x*fc})
rowSums(counts)
#

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

prop <- apply(df, 2, FUN = function(x) {x*(1/min(x[x > 0]))})
rowSums(prop)


convert <- function(x) {
 1/(min(x[x > 0]))
}
  
counts <- apply(df,1,convert)
rowSums(counts)

  