
# -------------------------------------------------------------------------
# 2023 04 Urban Area Places Clustering
# - Combine Place-level estimates up to UA-level
# Author - Ben Claassen
# -------------------------------------------------------------------------


# Libraries and paths -----------------------------------------------------
library(tidyverse)

path <- "2023 04 Urban Area Places Clustering"

# -------------------------------------------------------------------------
# Read in collated data [dat1] -------------------------------------------
load(file = paste0(path, "/Data/Cleaned/AllPlacesInUAsDat_1.Rda"))
dim(dat1)
names(dat1)
head(dat1)


# Drop obs with no people for total population ----------------------------
dat1 %>% filter(totalPop == 0) %>% count(place_geoid) # 17
dat1 <- dat1 %>% filter(totalPop > 0)
dim(dat1)

# Drop obs with no households for total households ------------------------
dat1 %>% filter(TotalHouseholds == 0) %>% count(place_geoid) # 9
dat1 <- dat1 %>% filter(TotalHouseholds > 0)

dim(dat1) # 8918 x 48 -> 17 + 9 + {74 with no merged data} = 9018 - 100 = 8918

# Create cross-dataset Place vars -----------------------------------------
dat2 <- dat1
dat2$popDensityPerSqKm <- dat2$totalPop / dat2$place_fullarea
dat2$peoplePerHH <- dat2$totalPop / dat2$TotalHouseholds
dat2$householdsPerSqKm <- dat2$TotalHouseholds / dat2$place_fullarea

dat2$percCollegeStudents <- dat2$percUndergradStudents + dat2$percGraduateStudents

summary(dat2$percUndergradStudents)
summary(dat2$percGraduateStudents)
summary(dat2$percCollegeStudents)

summary(dat2 %>% select(popDensityPerSqKm, peoplePerHH, householdsPerSqKm))
names(dat2)

# save(dat2, file = paste0(path, "/Data/Cleaned/AllPlacesInUAsDat_2.Rda")) # Save to external file


# Read in Place data (2) --------------------------------------------------
load(file = paste0(path, "/Data/Cleaned/AllPlacesInUAsDat_2.Rda"))
dim(dat2) # 8918 x 52
head(dat2)

# -------------------------------------------------------------------------
# Aggregate Place data by UA ----------------------------------------------

# Create UA total counts --------------------------------------------------
uas_pop <- dat2 %>% group_by(urbanarea_geoid) %>% summarize(ua_pop = sum(totalPop)) # Sum population by UA geoid
uas_area <- dat2 %>% group_by(urbanarea_geoid) %>% summarize(ua_area = sum(place_fullarea)) # Sum areas by UA geoid
uas_hh <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_hh = sum(TotalHouseholds)) # Sum households by UA geoid



# -------------------------------------------------------------------------
# Create UA weighted sums -------------------------------------------------

# Weighted sum  of median households income by UA geoid
uas_inc <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_inc = weighted.mean(HHMedianIncome, place_incomebase, na.rm = T) )


# Weighted sum of percent of workers working in their Place of residence
uas_workInPlaceRes <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_workInPlace = weighted.mean(PercWorkedInPlaceOfResidence, place_commutingbase, na.rm = T) )

head(uas_workInPlaceRes)
summary(uas_workInPlaceRes)
summary(dat2$PercWorkedInPlaceOfResidence)


# Weighted sum of mean commute times
uas_commutetime <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_commutetime = weighted.mean(MeanCommuteTimeMins, place_commutingbase, na.rm = T) )

head(uas_commutetime)
summary(uas_commutetime)
summary(dat2$MeanCommuteTimeMins)


# Weighted sum of % renters
uas_percrenters <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_percrenters = weighted.mean(percRenters, place_housingbase, na.rm = T ))

head(uas_percrenters)
summary(uas_percrenters)
summary(dat2$percRenters)


# Weighted sum of median rent
uas_medianrent <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_medianrent = weighted.mean(MedianRent, place_housingbase, na.rm = T ))

head(uas_medianrent)
summary(uas_medianrent)
summary(dat2$MedianRent)


# Weighted sum of % with mortgage
uas_percmortgage <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_percmortgage = weighted.mean(percWithMortgage, place_housingbase, na.rm = T ))

head(uas_percmortgage)
summary(uas_percmortgage)
summary(dat2$percWithMortgage)


# Weighted sum of median mortgage
uas_medianmortgage <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_medianmortgage = weighted.mean(MedianMortgage, place_housingbase, na.rm = T ))

head(uas_medianmortgage)
summary(uas_medianmortgage)
summary(dat2$MedianMortgage)


# Weighted sum of employment rate
uas_employmentrate <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_employmentrate = weighted.mean(percCivilianEmployed, place_employmentbase, na.rm = T ))

head(uas_employmentrate)
summary(uas_employmentrate)
summary(dat2$percCivilianEmployed)

# Weighted sum of armed forces
uas_armedforces <- dat2 %>%
  group_by(urbanarea_geoid) %>%
  summarize(ua_w_armedforces = weighted.mean(percArmedForces, place_employmentbase, na.rm = T ))

head(uas_armedforces)
summary(uas_armedforces)
summary(dat2$percArmedForces)


# Weighted sum of less than high school
uas_perclessthanhighschool <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_lessthanhighschool = weighted.mean(percLessThanHS, place_educationbase, na.rm = T ))

head(uas_perclessthanhighschool)
summary(uas_perclessthanhighschool)
summary(dat2$percLessThanHS)


# Weighted sum of high school
uas_perchighschool <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_highschool = weighted.mean(percHS, place_educationbase, na.rm = T ))

head(uas_perchighschool)
summary(uas_perchighschool)
summary(dat2$percHS)


# Weighted sum of some college
uas_percsomecollege <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_somecollege = weighted.mean(percSomeCollege, place_educationbase, na.rm = T ))

head(uas_percsomecollege)
summary(uas_percsomecollege)
summary(dat2$percSomeCollege)

# Weighted sum of bachelors degrees
uas_percbachelors <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_bachelors = weighted.mean(percBachelorsDegree, place_educationbase, na.rm = T ))

head(uas_percbachelors)
summary(uas_percbachelors)
summary(dat2$percBachelorsDegree)

# Weighted sum of graduate degrees
uas_percgraduatedegrees <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_graduatedegrees = weighted.mean(percGraduateDegree, place_educationbase, na.rm = T ))

head(uas_percgraduatedegrees)
summary(uas_percgraduatedegrees)
summary(dat2$percGraduateDegree)


# Weighted sum of median age
uas_medianage <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_medianage = weighted.mean(MedianAge, totalPop, na.rm = T ))

head(uas_medianage)
summary(uas_medianage)
summary(dat2$MedianAge)


# Weighted sum of children (age < 18)
uas_percchildren <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_children = weighted.mean(percUnder18, totalPop, na.rm = T ))

head(uas_percchildren)
summary(uas_percchildren)
summary(dat2$percUnder18)

# Weighted sum of retirees age (age >= 65)
uas_percretirees <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_retirees = weighted.mean(perc65andOver, totalPop, na.rm = T ))

head(uas_percretirees)
summary(uas_percretirees)
summary(dat2$perc65andOver)


# Weighted sum of undergraduate students
uas_percundergradstudents <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_undergradstudents = weighted.mean(percUndergradStudents, place_studentsbase, na.rm = T ))

head(uas_percundergradstudents)
summary(uas_percundergradstudents)
summary(dat2$percUndergradStudents)


# Weighted sum of graduate students
uas_percgraduatestudents <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_graduatestudents = weighted.mean(percGraduateStudents, place_studentsbase, na.rm = T ))

head(uas_percgraduatestudents)
summary(uas_percgraduatestudents)
summary(dat2$percGraduateStudents)


# Weighted sum of college students
uas_perccollegestudents <- dat2 %>% 
  group_by(urbanarea_geoid) %>% 
  summarize(ua_w_collegestudents = weighted.mean(percCollegeStudents, place_studentsbase, na.rm = T ))

head(uas_perccollegestudents)
summary(uas_perccollegestudents)
summary(dat2$percCollegeStudents)


# Join all UA data --------------------------------------------------------
uasdat <- full_join(uas_pop, uas_area, by = "urbanarea_geoid") # Join population and area data for Urban Areas
uasdat <- full_join(uasdat, uas_hh, by = "urbanarea_geoid") # Join households to UA dataset

uasdat <- full_join(uasdat, uas_inc, by = "urbanarea_geoid") # Join income
uasdat <- full_join(uasdat, uas_employmentrate, by = "urbanarea_geoid") # Join employment rate
uasdat <- full_join(uasdat, uas_commutetime, by = "urbanarea_geoid") # Join commute time
uasdat <- full_join(uasdat, uas_armedforces, by = "urbanarea_geoid") # Join armed forces
uasdat <- full_join(uasdat, uas_workInPlaceRes, by = "urbanarea_geoid") # Join % work in Place of res

uasdat <- full_join(uasdat, uas_medianage, by = "urbanarea_geoid") # Join median age
uasdat <- full_join(uasdat, uas_percchildren, by = "urbanarea_geoid") # Join children/age under 18
uasdat <- full_join(uasdat, uas_percretirees, by = "urbanarea_geoid") # Join age 65+/retirees

uasdat <- full_join(uasdat, uas_percrenters, by = "urbanarea_geoid") # Join % renters
uasdat <- full_join(uasdat, uas_medianrent, by = "urbanarea_geoid") # Join median rent
uasdat <- full_join(uasdat, uas_percmortgage, by = "urbanarea_geoid") # Join % with a mortgage
uasdat <- full_join(uasdat, uas_medianmortgage, by = "urbanarea_geoid") # Join median mortgage

uasdat <- full_join(uasdat, uas_perclessthanhighschool, by = "urbanarea_geoid") # Join less than high school
uasdat <- full_join(uasdat, uas_perchighschool, by = "urbanarea_geoid") # Join high school
uasdat <- full_join(uasdat, uas_percsomecollege, by = "urbanarea_geoid") # Join some college
uasdat <- full_join(uasdat, uas_percbachelors, by = "urbanarea_geoid") # Join bachelors degrees
uasdat <- full_join(uasdat, uas_percgraduatedegrees, by = "urbanarea_geoid") # Join graduate degrees

uasdat <- full_join(uasdat, uas_percundergradstudents, by = "urbanarea_geoid") # Join graduate students
uasdat <- full_join(uasdat, uas_percgraduatestudents, by = "urbanarea_geoid") # Join undergraduate students
uasdat <- full_join(uasdat, uas_perccollegestudents, by = "urbanarea_geoid") # Join college students



dim(uasdat) # 591 x 24
head(uasdat)


# Create UA calcs ---------------------------------------------------------
uasdat$ua_popDensityPerSqKm <- uasdat$ua_pop / uasdat$ua_area # Calculate UA population density
uasdat$ua_peoplePerHH <- uasdat$ua_pop / uasdat$ua_hh # Calculate UA population per household
uasdat$ua_householdsPerSqKm <- uasdat$ua_hh / uasdat$ua_area # Calculate UA household density

dim(uasdat) # 591 x 27
head(uasdat)
summary(uasdat)



# Attach National industry data -------------------------------------------
load(file = paste0(path, "/Data/Cleaned/NationalIndustryDat_1.Rda"))
head(national_industry)

# dim(
#   cbind(uasdat, national_industry[, -1])
# )
# 
# head(
#   cbind(uasdat, national_industry[,-1])
# )

uasdat <- cbind(uasdat, national_industry[,-1])

# save(uasdat, file = paste0(path, "/Data/Cleaned/UAsDat_1.Rda")) # Save to external file

# Load UAs data -----------------------------------------------------------
load(file = paste0(path, "/Data/Cleaned/UAsDat_1.Rda"))
dim(uasdat) # 591 x 40
summary(uasdat)

# -------------------------------------------------------------------------
# Create Place-UA variables -----------------------------------------------
dat3 <- left_join(dat2, uasdat, by = "urbanarea_geoid")
dim(dat3) # 8918 x 91
head(dat3)

# save(dat3, file = paste0(path, "/Data/Cleaned/AllPreProcessedData_1.Rda"))



# -------------------------------------------------------------------------
# Load all pre-processed data ---------------------------------------------
load(file = paste0(path, "/Data/Cleaned/AllPreProcessedData_1.Rda"))


