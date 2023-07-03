
# -------------------------------------------------------------------------
# Urban Area Places Clustering
# 01 - Code to collate and process US Census data
# Author - Ben Claassen
# -------------------------------------------------------------------------


# Libraries and paths -----------------------------------------------------
library(tidyverse)

path <- "2023 04 Urban Area Places Clustering/"
acsPath <- "/Data/ACS 2020 5 yr"


# -------------------------------------------------------------------------
# Read in PLACE data ------------------------------------------------------

# Read in list of Place IDs to keep ---------------------------------------
ids <- read.csv( paste0(path, "Data/Place IDs/US Places Intersecting UrbanAreas()x FINAL.csv") )
dim(ids)
head(ids)

ids %>% select(place_geoid) %>% duplicated %>% sum # Should be 0
ids %>% count(place_geoid) %>% filter(n>1) # Should be 0


# Population and ages [S0101] ---------------------------------------------
## S0101_C01_001E - total
## S0101_C01_022E - under 18
## S0101_C01_030E - 65 and over
## S0101_C01_032E - median age

pop <- read.csv( paste0(path, acsPath, "/Places/Age and Sex/ACSST5Y2020.S0101-Data.csv") )
pop <- pop %>% select( # Select vars
  GEO_ID
  , NAME
  , S0101_C01_001E
  , S0101_C01_022E
  , S0101_C01_030E
  , S0101_C01_032E
)

names(pop) <- c( # Set shorthand names
  "place_fullgeoid"
  , "CityName"
  , "totalPop"
  , "Under18"
  , "x65andOver"
  ,"MedianAge"
)

head(pop) # Compare shorthand/long-form names
pop <- pop[-1,] # Remove Census ACS long-form names
head(pop)

sapply(pop, class) # Check var class
pop[,c(3:6)] <- sapply(pop[,c(3:6)], as.numeric) # Convert vars to numeric
summary(pop)

sapply(pop, class) # Confirm var class
head(pop)

pop$percUnder18 <- pop$Under18 / pop$totalPop # Convert to percent
pop$perc65andOver <- pop$x65andOver / pop$totalPop # Convert to percent

summary(pop)
head(pop)

pop <- pop %>% select( # Select final vars
  place_fullgeoid
  , CityName
  , totalPop
  , percUnder18
  , perc65andOver
  , MedianAge
  )

head(pop)


# Median income [S1903] ---------------------------------------------------
## S1903_C01_001E - Income base figure
## S1903_C03_001E - Household Median Income
inc <- read.csv( paste0(path, acsPath, "/Places/Median Income/ACSST5Y2020.S1903-Data.csv") )
inc <- inc %>% select( # Select vars
  GEO_ID
  , NAME
  , S1903_C01_001E
  , S1903_C03_001E
)

names(inc) <- c( # Set shorthand names
  "place_fullgeoid"
  , "CityName"
  , "place_incomebase"
  , "HHMedianIncome"
)

head(inc) # Compare shorthand/long-form names
inc <- inc[-1,] # Remove Census ACS long-form names
head(inc)

sapply(inc, class)
inc$place_incomebase <- as.numeric(inc$place_incomebase) # Convert vars to numeric
inc$HHMedianIncome <- as.numeric(inc$HHMedianIncome)
sapply(inc, class)

summary(inc)
head(inc)


# Commuting characteristics [S0801] ----------------------------------------
## S0801_C01_001E - commuting base figure
## S0801_C01_019E - work in place of residence
## S0801_C01_046E - average (mean) commute time
commuting <- read.csv( paste0(path, acsPath, "/Places/Commuting/ACSST5Y2020.S0801-Data.csv") )
commuting <- commuting %>% select( # Select vars
  GEO_ID
  , NAME
  , S0801_C01_001E
  , S0801_C01_019E
  , S0801_C01_046E
)

names(commuting) <- c( # Set shorthand names
  "place_fullgeoid"
  , "CityName"
  , "place_commutingbase"
  , "PercWorkedInPlaceOfResidence"
  , "MeanCommuteTimeMins"
)

head(commuting) # Compare shorthand/long-form names
commuting <- commuting[-1,] # Remove Census ACS long-form names
head(commuting)


sapply(commuting, class) # Check var class
commuting[,c(3:5)] <- sapply(commuting[,c(3:5)], as.numeric) # Convert vars to numeric
head(commuting)
sapply(commuting, class) # Confirm var class

summary(commuting)

commuting$PercWorkedInPlaceOfResidence <- commuting$PercWorkedInPlaceOfResidence / 100 # Convert %s to [0,1]

head(commuting)
summary(commuting)


# Housing costs [DP04] ----------------------------------------------------
## DP04_0001E - housing costs base figure
## DP04_0047PE - % renter occupied
## DP04_0134E - median rent
## DP04_0091PE - % owner-occupied houses with a mortgage
## DP04_0101E - median mortgage for houses that have a mortgage

housing <- read.csv( paste0(path, acsPath, "/Places/Housing Costs/ACSDP5Y2020.DP04-Data.csv") )
housing <- housing %>% select( # Select vars
  GEO_ID
  , NAME
  , DP04_0001E
  , DP04_0047PE
  , DP04_0134E
  , DP04_0091PE
  , DP04_0101E
)

names(housing) <- c( # Set shorthand names
  "place_fullgeoid"
  , "CityName"
  , "place_housingbase"
  , "percRenters"
  , "MedianRent"
  , "percWithMortgage"
  , "MedianMortgage"
)

head(housing) # Compare shorthand/long-form names
housing <- housing[-1,] # Remove Census ACS long-form names
head(housing)


sapply(housing, class) # Check var class
housing[,c(3:7)] <- sapply(housing[,c(3:7)], as.numeric) # Convert vars to numeric
sapply(housing, class) # Confirm var class
head(housing)

summary(housing)


housing$percRenters <- housing$percRenters / 100 # Convert %s to [0,1]
housing$percWithMortgage <- housing$percWithMortgage / 100 # Convert %s to [0,1]

head(housing)

housing <- housing %>% select( # Select final vars
  place_fullgeoid
  , CityName
  , place_housingbase
  , percRenters
  , MedianRent
  , percWithMortgage
  , MedianMortgage
  )

head(housing)
summary(housing)


# Employment [DP03] -------------------------------------------------------
## DP03_0001E - civilian employment base figure
## DP03_0004PE - % civilian employed
## - industries
###    - DP03_0033PE - Agriculture, forestry, fishing and hunting, and mining
###    - DP03_0034PE - Construction
###    - DP03_0035PE - Manufacturing
###    - DP03_0036PE - Wholesale trade
###    - DP03_0037PE - Retail trade
###    - DP03_0038PE - Transportation and warehousing, and utilities
###    - DP03_0039PE - Information
###    - DP03_0040PE - Finance and insurance, and real estate and rental and leasing
###    - DP03_0041PE - Professional, scientific, and management, and administrative and waste management services
###    - DP03_0042PE - Educational services, and health care and social assistance
###    - DP03_0043PE - Arts, entertainment, and recreation, and accommodation and food services
###    - DP03_0044PE - Other services, except public administration
###    - DP03_0045PE - Public administration
## DP03_0006PE - armed forces


employment <- read.csv( paste0(path, acsPath, "/Places/Employment Rate and Industry/ACSDP5Y2020.DP03-Data.csv") )
employment <- employment %>% select( # Select vars
  GEO_ID
  , NAME
  , DP03_0001E
  , DP03_0004PE
  , DP03_0033PE
  , DP03_0034PE
  , DP03_0035PE
  , DP03_0036PE
  , DP03_0037PE
  , DP03_0038PE
  , DP03_0039PE
  , DP03_0040PE
  , DP03_0041PE
  , DP03_0042PE
  , DP03_0043PE
  , DP03_0044PE
  , DP03_0045PE
  , DP03_0006PE
)

names(employment) <- c( # Set shorthand names
  "place_fullgeoid"
  , "CityName"
  , "place_employmentbase"
  , 'percCivilianEmployed'
  , 'percResourceExtraction'
  , 'percConstrctn'
  , 'percManufctrng'
  , 'percWholesale'
  , 'percRetail'
  , 'percTransportationUtilities'
  , 'percInformation'
  , 'percFinance'
  , 'percProfessionalAndOtherServices'
  , 'percEducationalAndHealthcare'
  , 'percEntertainmentAndHospitality'
  , 'percOtherServices'
  , 'percPublicAdmin'
  , 'percArmedForces'
)

head(employment) # Compare shorthand/long-form names
employment <- employment[-1,] # Remove Census ACS long-form names
head(employment)

employment[, c(3:18)] <- sapply(employment[, c(3:18)], as.numeric) # Convert all vars to numeric except [place_fullgeoid], [CityName]
head(employment)
sapply(employment, class) # Check var classes

# employment$civIndustryTotalSum <- rowSums(employment[, -c(1:4, 18)]) # Sum across all vars except [place_fullgeoid], [CityName], [percCivilianEmployed], [percArmedForces]
# summary(employment$civIndustryTotalSum)

## Test calcs for data converted to NAs ##
# tmpdat1 <- left_join(ids, pop, by = "place_fullgeoid")
# tmpdat1 <- left_join(tmpdat1, employment, by = "place_fullgeoid")
# tmpdat1 %>% # Copy all NAs to clipboard
#   filter(is.na(civIndustryTotalSum)) %>% 
#   select(urbanarea_cityname, place_cityname, totalPop, percCivilianEmployed, civIndustryTotalSum) %>% 
#   clipr::write_clip()
# tmpdat1 %>% summary

# for(x in 5:18) # Explore cities with more than 99% employment in a single industry, incl. Armed Forces
# {
#   print(employment[which(employment[,x] >= 99), c(1:4,x)])
# }


summary(employment)
head(employment)

employment[, c(4:18)] <- apply(employment[, c(4:18)], MARGIN = 2, FUN = function(x) {x / 100}) # Convert %s to [0,1]

summary(employment)
head(employment)


# Education [S1501] -------------------------------------------------------
## S1501_C01_006E - education base figure (all vars for population 25+)
## S1501_C02_007E - lessThan9th
## S1501_C02_008E - 9to12_noHS
## S1501_C02_009E - HS
## S1501_C02_010E - SomeCollege
## S1501_C02_011E - AssocDegree
## S1501_C02_012E - BachelorsDegree
## S1501_C02_013E - GraduateDegree


education <- read.csv( paste0(path, acsPath, "/Places/Education/ACSST5Y2020.S1501-Data.csv") )
education <- education %>% select( # Select vars
  GEO_ID
  , NAME
  , S1501_C01_006E
  , S1501_C02_007E
  , S1501_C02_008E
  , S1501_C02_009E
  , S1501_C02_010E
  , S1501_C02_011E
  , S1501_C02_012E
  , S1501_C02_013E
)

names(education) <- c( # Set shorthand names
  "place_fullgeoid"
  , "CityName"
  , "place_educationbase"
  , "lessThan9th"
  , "x9to12_noHS"
  , "percHS"
  , "SomeCollege"
  , "AssocDegree"
  , "percBachelorsDegree"
  , "percGraduateDegree"
)

head(education) # Compare shorthand/long-form names
education <- education[-1,] # Remove Census ACS long-form names
head(education)

sapply(education, class) # Check var class
education[,c(3:10)] <- sapply(education[,c(3:10)], as.numeric) # Convert vars to numeric
summary(education)
head(education)
sapply(education, class) # Confirm var class

summary(rowSums(education[, -c(1:3)])) # Should all be near 100%

head(education)

# Combine education levels
education$percLessThanHS <- education$lessThan9th + education$x9to12_noHS 
education$percSomeCollege <- education$SomeCollege + education$AssocDegree
  

education <- education %>% select( # Select final vars
  place_fullgeoid
  , CityName
  , place_educationbase
  , percLessThanHS
  , percHS
  , percSomeCollege
  , percBachelorsDegree
  , percGraduateDegree
  )

head(education)

summary(rowSums(education[,-c(1:3)])) # Should all be near 100%

education[, c(4:8)] <- apply(education[, c(4:8)], MARGIN = 2, FUN = function(x) {x / 100}) # Convert %s to [0,1]

head(education)
summary(education)


# Number of Households [B25001] -------------------------------------------
## B25001_001E - Total Number of Households
numHouseholds <- read.csv( paste0(path, acsPath, "/Places/Housing Units/ACSDT5Y2020.B25001-Data.csv") )
numHouseholds <- numHouseholds %>% select( # Select vars
  GEO_ID
  , NAME
  , B25001_001E
)

names(numHouseholds) <- c( # Set shorthand names
  "place_fullgeoid"
  , "CityName"
  , "TotalHouseholds"
)

head(numHouseholds) # Compare shorthand/long-form names
numHouseholds <- numHouseholds[-1,] # Remove Census ACS long-form names
head(numHouseholds)

sapply(numHouseholds, class)

numHouseholds$TotalHouseholds <- as.numeric(numHouseholds$TotalHouseholds)

sapply(numHouseholds, class)
head(numHouseholds)

summary(numHouseholds)


# Number of students [B14002] ---------------------------------------------
## B14002_002E - Male_Total Enrollment
## B14002_019E - Male_Undergrad Enrollment
## B14002_022E - Male_Grad Enrollment
## B14002_026E - Female_Total Enrollment
## B14002_043E - Female_Undergrad Enrollment
## B14002_046E - Female_Grad Enrollment

students <- read.csv( paste0(path, acsPath, "/Places/Students/ACSDT5Y2020.B14002-Data.csv") )
students <- students %>% select( # Select vars
  GEO_ID
  , NAME
  , B14002_002E
  , B14002_019E
  , B14002_022E
  , B14002_026E
  , B14002_043E
  , B14002_046E
)

names(students) <- c( # Set shorthand names
  "place_fullgeoid"
  , "CityName"
  , "Male_Total"
  , "Male_Undergrad"
  , "Male_Grad"
  , "Female_Total"
  , "Female_Undergrad"
  , "Female_Grad"
)

head(students) # Compare shorthand/long-form names
students <- students[-1,] # Remove Census ACS long-form names
head(students)

sapply(students, class) # Check var class
students[,c(3:8)] <- sapply(students[,c(3:8)], as.numeric) # Convert vars to numeric
head(students)
sapply(students, class) # Confirm var class

# Combine sex-specific vars to get total vars
students$TotalPop_Students <- students$Male_Total + students$Female_Total
students$UndergradStudents <- students$Male_Undergrad + students$Female_Undergrad
students$GraduateStudents <- students$Male_Grad + students$Female_Grad

head(students)

# Create percent vars
students$percUndergradStudents <- students$UndergradStudents / students$TotalPop_Students
students$percGraduateStudents <- students$GraduateStudents / students$TotalPop_Students

head(students)

students$place_studentsbase <- students$TotalPop_Students # Save total as place_base

students <- students %>% select( # Select final vars
  place_fullgeoid
  , CityName
  , place_studentsbase
  , percUndergradStudents
  , percGraduateStudents
  )

summary(students)
head(students)


# -------------------------------------------------------------------------
# Join PLACE data ---------------------------------------------------------

dat1 <- left_join(ids, pop[, -2], by = "place_fullgeoid") # Join IDs and population data sets
head(dat1)
dim(dat1)


dat1 <- left_join(dat1, commuting[, -2], by = "place_fullgeoid") # Join commuting to main data set
head(dat1)
dim(dat1)
dat1 <- left_join(dat1, education[, -2], by = "place_fullgeoid") # Join education
dat1 <- left_join(dat1, employment[, -2], by = "place_fullgeoid") # Join employment
dat1 <- left_join(dat1, housing[, -2], by = "place_fullgeoid") # Join housing
dat1 <- left_join(dat1, inc[, -2], by = "place_fullgeoid") # Join income
dat1 <- left_join(dat1, numHouseholds[, -2], by = "place_fullgeoid") # Join number of households
dat1 <- left_join(dat1, students[, -2], by = "place_fullgeoid") # Join students

head(dat1)
summary(dat1)
dim(dat1)

# write.csv(dat1, file = paste0(path, "/Data/Cleaned/AllPlaceData.csv"), row.names = F)
# save(dat1, file = paste0(path, "/Data/Cleaned/AllPlacesInUAsDat_1.Rda")) # Save to external file




# -------------------------------------------------------------------------
# Read in national data ---------------------------------------------------

# Industry [DP03]
##  - DP03_0033PE - Agriculture, forestry, fishing and hunting, and mining
##  - DP03_0034PE - Construction
##  - DP03_0035PE - Manufacturing
##  - DP03_0036PE - Wholesale trade
##  - DP03_0037PE - Retail trade
##  - DP03_0038PE - Transportation and warehousing, and utilities
##  - DP03_0039PE - Information
##  - DP03_0040PE - Finance and insurance, and real estate and rental and leasing
##  - DP03_0041PE - Professional, scientific, and management, and administrative and waste management services
##  - DP03_0042PE - Educational services, and health care and social assistance
##  - DP03_0043PE - Arts, entertainment, and recreation, and accommodation and food services
##  - DP03_0044PE - Other services, except public administration
##  - DP03_0045PE - Public administration


national_industry <- read.csv( paste0(path, acsPath, "/National/Industry/ACSDP5Y2020.DP03-Data.csv") )
national_industry <- national_industry %>% select( # Select vars
  GEO_ID
  , DP03_0033PE
  , DP03_0034PE
  , DP03_0035PE
  , DP03_0036PE
  , DP03_0037PE
  , DP03_0038PE
  , DP03_0039PE
  , DP03_0040PE
  , DP03_0041PE
  , DP03_0042PE
  , DP03_0043PE
  , DP03_0044PE
  , DP03_0045PE
)

names(national_industry) <- c( # Set shorthand names
  "natl_geoid"
  , 'natl_percResourceExtraction'
  , 'natl_percConstrctn'
  , 'natl_percManufctrng'
  , 'natl_percWholesale'
  , 'natl_percRetail'
  , 'natl_percTransportationUtilities'
  , 'natl_percInformation'
  , 'natl_percFinance'
  , 'natl_percProfessionalAndOtherServices'
  , 'natl_percEducationalAndHealthcare'
  , 'natl_percEntertainmentAndHospitality'
  , 'natl_percOtherServices'
  , 'natl_percPublicAdmin'
)


head(national_industry) # Compare shorthand/long-form names
national_industry <- national_industry[-1,] # Remove Census ACS long-form names
head(national_industry)

sapply(national_industry, class) # Check var classes
national_industry[, c(2:14)] <- sapply(national_industry[, c(2:14)], as.numeric) # Convert all vars to numeric except [place_fullgeoid], [CityName]
head(national_industry)
sapply(national_industry, class) # Check var classes
# national_industry$industryTotalSum <- rowSums(national_industry[, -c(1:4, 18)]) # Add all vars except [place_fullgeoid], [CityName], [percCivilianEmployed], [percArmedForces]

rowSums(national_industry[, c(2:14)]) # Check that row sum adds to 100% => 99.9%

national_industry[, c(2:14)] <- apply(national_industry[, c(2:14)], MARGIN = 2, FUN = function(x) {x / 100}) # Convert %s to [0,1]

head(national_industry)

# save(national_industry, file = paste0(path, "/Data/Cleaned/NationalIndustryDat_1.Rda")) # Save to external file
