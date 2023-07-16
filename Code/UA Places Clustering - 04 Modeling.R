# -------------------------------------------------------------------------
# 2023 04 Urban Area Places Clustering
# - Run PCA analysis
# -------------------------------------------------------------------------


# Libraries and paths -----------------------------------------------------
library(tidyverse)
options(scipen = 9999)

path <- "2023 04 City Roles In Urban Areas"


# Functions ---------------------------------------------------------------
pcalooksee <- function(tmpPca, numComponents = 6, componentLoadingThreshold = 0.18)
{
  plot(tmpPca, main = "PCA Scree Plot")
  # summary(tmpPca)
  
  # Check if there are enough variables to match the [numComponents] parameter, must be greater than, or equal to, length(tmpPca)
  if(numComponents > dim(tmpPca$loadings)[1] ) {
    print("WARNGING: Too many components requested given number of input variables")
    print(noquote(""))
    
    numComponents <- dim(tmpPca$loadings)[1] # Change [numComponents] to max possible with given dataset
  }
  
  # Print first [numComponents = 6]  principal components with loadings over [componentLoadingThreshold = 0.18]
  for(p in 1:numComponents) {
    
    tmpLoadingsOrder1 <- sort(tmpPca$loadings[,p])
    tmpLoadingsOrder2 <- tmpLoadingsOrder1[which(abs(tmpLoadingsOrder1) > componentLoadingThreshold)]
    tmpLoadingsOrder3 <- matrix(tmpLoadingsOrder2)
    row.names(tmpLoadingsOrder3) <- names(tmpLoadingsOrder2)
    tmpLoadingsOrder3 <- as.data.frame(tmpLoadingsOrder3)
    names(tmpLoadingsOrder3)[1] <- paste0("Component: ", p)
    
    print(
      round(
        tmpLoadingsOrder3
        , 2)
    )
    
    print(noquote(""))
    
  }
  
  # Print percent of variance explained
  print(
      round(
        cumsum(
          (tmpPca$sdev^2) / sum((tmpPca$sdev^2))
      
          )
    , 3)
    )
  
  print(noquote(""))
  
}


# Load data ----------------------------------------------------------
load(file = paste0(path, "/Data/Cleaned/FINAL_DATA.Rda"))

dim(dat5) # 6967 x 47
head(dat5)

# t(dat5 %>% filter(place_geoid == 1712385) %>% clipr::write_clip())
# t(dat5 %>% filter(urbanarea_geoid == 74179) %>% clipr::write_clip())

# for(x in 1:dim(dat5)[2])
# {
#   print(paste0(names(dat5)[x], ": ", max(dat5[,x], na.rm = T)))
# }
# 
# # Explore all values, excluding ids, national ratios, and percents of total
# foo <- unlist(dat5[,c(12:34)])
# length(which(foo > 3)) / length(foo)
# bar <- foo[which(foo < 3)]
# hist(bar)
# 


# -------------------------------------------------------------------------
dat <- dat5
# -------------------------------------------------------------------------


# Export correlations -----------------------------------------------------
# corMat <- cor(dat[, -c(1:8)])
# 
# corMat[upper.tri(corMat)] <- -999
# diag(corMat) <- -999
# 
# clipr::write_clip(corMat)

# PCA ---------------------------------------------------------------------

names(dat)

## Notes
# pca1 -> 
# pca2 -> 
# pca -> 
# pca -> 

# Starting with all vars but:
#     [1:8] IDs
#     [9] total pop
#     [11] housing perc of UA total (cor with pop percent 99.4%)
#     [13] housing density (cor with pop density 91.1%)
#     [34] keeping grad/undergrad students separate; dropping college students
#             - (cor undergrad:college students 96.0%)

droplist_1 <- c(1:8, 9, 11, 13, 34)
pca1 <- princomp(dat[, -droplist_1], cor = F)

pcalooksee(pca1)

# pca1$scores[,1] %>% summary()
# dat6$place_cityname[pca1$scores[,1] <= -1] %>% length
# 864/dim(dat6)[1] # 12.4% more than 1 std dev away from mean (0)
# dat6$place_cityname[pca1$scores[,1] <= -1] %>% head
# 
# dat6$place_geoid[pca1$scores[,1] <= -1] %>% clipr::write_clip()


# PCA - 2
#     [15] I'm keeping median age, but highly correlated with 65+ [17] (82.3%)
#     NOTE: 
#          - all other vars are less than 80% correlated with each other in absolute terms (+/-)
#          - either need to choose median age [15] or 65+ [17] for next analyses

droplist_2 <- c(droplist_1, 15)
pca2 <- princomp(dat[, -droplist_2], cor = F)

pcalooksee(pca2, numComponents = 8)


# PCA - 3
# Can we find commuter  cities?
# Should have:
#      - lower population [10]
#      - lower housing costs [29, 31]
#      - lower density [12]
#      - lower 'worked in place of residence' [18]
#      - higher employment [26]
#      - higher commute times [19]
# I also want to include:
#      - income
#      - popPerHH_rua

keeplist_3 <- c(10, 12, 18, 19, 26, 29, 31, 14, 25)
pca3 <- princomp(dat[, keeplist_3], cor = F)

pcalooksee(pca3, numComponents = 8)
