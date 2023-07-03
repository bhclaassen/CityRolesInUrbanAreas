
# -------------------------------------------------------------------------
# Urban Area Places Clustering
# - Create Place-level vars with UA bases
#       - This is so all obs are comparable across the country, i.e if an Urban 
#            Area has higher commute times overall, that shouldn't obscure
#            the discovery of a commuter town
# - Output to data file ready for analysis
# Author - Ben Claassen
# -------------------------------------------------------------------------


# Libraries and paths -----------------------------------------------------
library(tidyverse)

path <- "2023 04 Urban Area Places Clustering"

# Load all pre-processed data ---------------------------------------------
load(file = paste0(path, "/Data/Cleaned/AllPreProcessedData_1.Rda"))


# Create Place/UA-based variables -----------------------------------------
dat4 <- dat3

# Variable suffix definitions
## 'pua' - percent of UA
## 'rua' - ratio of UA figure
## 'rnatl' - ratio of national figure

# General
dat4$pop_pua = dat4$totalPop / dat4$ua_pop # Population - percent of UA
dat4$popDensity_rua = dat4$popDensityPerSqKm / dat4$ua_popDensityPerSqKm # Population:Square km - ratio of UA figure
dat4$hh_pua = dat4$TotalHouseholds / dat4$ua_hh # Households - percent of UA
dat4$hhDensity_rua = dat4$householdsPerSqKm / dat4$ua_householdsPerSqKm # Household:Square km - ratio of UA figure
dat4$popPerHH_rua = dat4$peoplePerHH / dat4$ua_peoplePerHH # People:Household - ratio of UA figure

# Ages
dat4$medianAge_rua = dat4$MedianAge / dat4$ua_w_medianage
dat4$under18_rua = dat4$percUnder18 / dat4$ua_w_children
dat4$x65andOver_rua = dat4$perc65andOver / dat4$ua_w_retirees

# Commuting
dat4$workedInPlaceOfRes_rua = dat4$PercWorkedInPlaceOfResidence / dat4$ua_w_workInPlace # Worked in Place of residence
dat4$commuteTime_rua = dat4$MeanCommuteTimeMins / dat4$ua_w_commutetime # Mean commute time (minutes)

# Educational attainment
dat4$lessThanHighschool_rua = dat4$percLessThanHS / dat4$ua_w_lessthanhighschool
dat4$highschool_rua = dat4$percHS / dat4$ua_w_highschool
dat4$someCollege_rua = dat4$percSomeCollege / dat4$ua_w_somecollege
dat4$bachelorsDegree_rua = dat4$percBachelorsDegree / dat4$ua_w_bachelors
dat4$graduateDegree_rua = dat4$percGraduateDegree / dat4$ua_w_graduatedegrees

# General employment
dat4$hhMedianIncome_rua = dat4$HHMedianIncome / dat4$ua_w_inc # Median household income ($)
dat4$civilianEmployed_rua = dat4$percCivilianEmployed / dat4$ua_w_employmentrate
dat4$armedForces_rua = dat4$percArmedForces / dat4$ua_w_armedforces

# Industry
dat4$resourceExtraction_jobs_rnatl = dat4$percResorceExtraction / dat4$natl_percResourceExtraction # Jobs: Agriculture, forestry, fishing and hunting, and mining - ratio of national figure
dat4$construction_jobs_rnatl = dat4$percConstrctn / dat4$natl_percConstrctn # Jobs: Construction
dat4$manufacturing_jobs_rnatl = dat4$percManufctrng / dat4$natl_percManufctrng # Jobs: Manufacturing
dat4$wholesale_jobs_rnatl = dat4$percWholesale / dat4$natl_percWholesale # Jobs: Wholesale trade
dat4$retail_jobs_rnatl = dat4$percRetail / dat4$natl_percRetail # Jobs: Retail trade
dat4$transportationUtilities_jobs_rnatl = dat4$percTransportationUtilities / dat4$natl_percTransportationUtilities # Jobs: Transportation and warehousing, and utilities
dat4$information_jobs_rnatl = dat4$percInformation / dat4$natl_percInformation # Jobs: Information
dat4$finance_jobs_rnatl = dat4$percFinance / dat4$natl_percFinance # Jobs: Finance and insurance, and real estate and rental and leasing
dat4$professionalAndOtherServices_jobs_rnatl = dat4$percProfessionalAndOtherServices / dat4$natl_percProfessionalAndOtherServices # Jobs: Professional, scientific, and management, and administrative and waste management services
dat4$educationAndHealthcare_jobs_rnatl = dat4$percEducationalAndHealthcare / dat4$natl_percEducationalAndHealthcare # Jobs: Educational services, and health care and social assistance
dat4$entertainmentAndHospitality_jobs_rnatl = dat4$percEntertainmentAndHospitality / dat4$natl_percEntertainmentAndHospitality # Jobs: Arts, entertainment, and recreation, and accommodation and food services
dat4$publicAdmin_jobs_rnatl = dat4$percPublicAdmin / dat4$natl_percPublicAdmin # Jobs: Public administration
dat4$otherServices_jobs_rnatl = dat4$percOtherServices / dat4$natl_percOtherServices # Jobs: Other services, except public administration

# Housing costs
dat4$renters_rua = dat4$percRenters / dat4$ua_w_percrenters # Renters
dat4$medianRent_rua = dat4$MedianRent / dat4$ua_w_medianrent # Median rent ($)
dat4$ownerWithMortgage_rua = dat4$percWithMortgage / dat4$ua_w_percmortgage # Owners with a mortgage
dat4$medianMortgage_rua = dat4$MedianMortgage / dat4$ua_w_medianmortgage # Median mortgage ($)

# College students
dat4$undergradStudents_rua = dat4$percUndergradStudents / dat4$ua_w_undergradstudents # Undergraduate students
dat4$graduateStudents_rua = dat4$percGraduateStudents / dat4$ua_w_graduatestudents # Graduate students
dat4$collegeStudents_rua = dat4$percCollegeStudents / dat4$ua_w_collegestudents # All college students


# Keep final vars ---------------------------------------------------------
dat5 <- dat4 %>% select(
  place_geoid
  , place_fullgeoid
  , place_cityname
  , place_state
  , place_fullarea
  , urbanarea_geoid
  , urbanarea_cityname
  , ua_placecount
  , totalPop
  , pop_pua
  , hh_pua
  , popDensity_rua
  , hhDensity_rua
  , popPerHH_rua
  , medianAge_rua
  , under18_rua
  , x65andOver_rua
  , workedInPlaceOfRes_rua
  , commuteTime_rua
  , lessThanHighschool_rua
  , highschool_rua
  , someCollege_rua
  , bachelorsDegree_rua
  , graduateDegree_rua
  , hhMedianIncome_rua
  , civilianEmployed_rua
  , armedForces_rua
  , renters_rua
  , medianRent_rua
  , ownerWithMortgage_rua
  , medianMortgage_rua
  , undergradStudents_rua
  , graduateStudents_rua
  , collegeStudents_rua
  , resourceExtraction_jobs_rnatl
  , construction_jobs_rnatl
  , manufacturing_jobs_rnatl
  , wholesale_jobs_rnatl
  , retail_jobs_rnatl
  , transportationUtilities_jobs_rnatl
  , information_jobs_rnatl
  , finance_jobs_rnatl
  , professionalAndOtherServices_jobs_rnatl
  , educationAndHealthcare_jobs_rnatl
  , entertainmentAndHospitality_jobs_rnatl
  , publicAdmin_jobs_rnatl
  , otherServices_jobs_rnatl
)

dim(dat5) # 8918 x 47
head(dat5)
summary(dat5)


# Scale all vars to {mean = 0} and {sd = 1} -------------------------------

for(x in 10:47){
  dat5[,x] <- scale(dat5[,x])
}

round(
  sapply(dat5[,10:47], mean, na.rm = T)
  , 1) == 0 # Must all be zero/TRUE

round(
  sapply(dat5[10:47], sd, na.rm = T)
  , 1) == 1 # Must all be one/TRUE

# -------------------------------------------------------------------------

# save(dat5, file = paste0(path, "/Data/Cleaned/FINAL_DATA.Rda")) # Save to external file

# -------------------------------------------------------------------------
# FINAL DATA SET ----------------------------------------------------------
load(file = paste0(path, "/Data/Cleaned/FINAL_DATA.Rda"))

dim(dat5) # 8918 x 47
head(dat5)

t(dat5 %>% filter(place_geoid == 1712385) %>% clipr::write_clip()) # Pull and example city
t(dat5 %>% filter(urbanarea_geoid == 74179) %>% clipr::write_clip()) # Pull an example urban area

for(x in 1:dim(dat5)[2]) # Check maximum std devs from zero
{
  print(paste0(names(dat5)[x], ": ", max(dat5[,x], na.rm = T)))
}

for(x in 1:dim(dat5)[2]) # Check minimum std devs from zero 
{
  print(paste0(names(dat5)[x], ": ", min(dat5[,x], na.rm = T)))
}

# Explore all values, excluding ids, national ratios, and percents of total
foo <- unlist(dat5[,c(12:34)])
length(which(abs(foo) > 3)) / length(foo) # How many obs more than 3 std devs from mean? 

bar <- foo[which(abs(foo) < 3)] # What is the distribution of obs within 3 std devs of mean?
hist(bar)


