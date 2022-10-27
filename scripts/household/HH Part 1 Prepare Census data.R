################################
#
# Simulating crime data
#
# Part I: Prepare Census data
#
################################

rm(list=ls())

#load dplyr package
library(dplyr)
library(here)

#For each - generate binary indicator (mean) and also save numerator and denominator

###NB Approx 121 households in first area (use to check...)

##Accommodation type - terraced (but could also look at flats?)
# load accomoation data we need to replicate.
Accommodation_by_OA <- read.csv(here("data", "QS402EWDATA.csv"))

# load list of output areas.
OA <- read.csv(here("data", "SAM_OA_DEC_2011_EW.csv"))

# remove all geographies that are not OAs.
Accommodation_by_OA <- Accommodation_by_OA %>%
  rename(OA11CD = GeographyCode) %>%
  semi_join(OA, by = "OA11CD")

# renaming, dropping missings pop and arranging OAs
accommodation_mean <-  Accommodation_by_OA %>% 
  rename(HH_accom = `QS402EW0001`,
         detached = `QS402EW0004`,
         semi = `QS402EW0005`,
         terraced = `QS402EW0006`, 
         flat = ) %>% 
  mutate(mean_terraced = terraced/HH_accom) %>%
  select(OA11CD, HH_accom, terraced, mean_terraced)

write.csv(accommodation_mean, here("data", "Accommodation_by_OA_replicate.csv"))

rm(list=c("Accommodation_by_OA", "accommodation_mean"))

## Household size - single person households (but could look at single parent)

# load household size data we need to replicate.
HHsize_by_OA <- read.csv(here("data", "QS116EWDATA.csv"))

# load list of output areas.
OA <- read.csv(here("data", "SAM_OA_DEC_2011_EW.csv"))

# remove all geographies that are not OAs.
HHsize_by_OA <- HHsize_by_OA %>%
  rename(OA11CD = GeographyCode) %>%
  semi_join(OA, by = "OA11CD")

# renaming, dropping missings pop and arranging OAs
hhsize_mean <-  HHsize_by_OA %>% 
  rename(HH_size = `QS116EW0001`,
         one_person = `QS116EW0002`) %>% 
  mutate(mean_one_person = one_person/HH_size) %>%
  select(OA11CD, HH_size, one_person, mean_one_person)

# save
write.csv(hhsize_mean, here("data", "HHsize_by_OA_replicate.csv"))

rm(list=c("HHsize_by_OA", "hhsize_mean"))


## Economic activity - identify no income (including)

# load economic activity data we need to replicate.
Economic_by_OA <- read.csv(here("data", "QS602EWDATA.csv"))

# load list of output areas.
OA <- read.csv(here("data", "SAM_OA_DEC_2011_EW.csv"))

# remove all geographies that are not OAs.
Economic_by_OA <- Economic_by_OA %>%
  rename(OA11CD = GeographyCode) %>%
  semi_join(OA, by = "OA11CD")

# renaming, dropping missings pop and arranging OAs
economic_mean <-  Economic_by_OA %>% 
  rename(HH_econ = `QS602EW0001`,
         inactive = `QS602EW0011`,
         student = `QS602EW0009`,
         unemployed = `QS602EW0010`) %>% 
  mutate(sum_no_income = inactive + student + unemployed,
         mean_no_income = sum_no_income/HH_econ) %>%
  select(OA11CD, HH_econ, sum_no_income, mean_no_income)

# save
write.csv(economic_mean, here("data", "Economic_by_OA_replicate.csv"))

rm(list=c("Economic_by_OA", "economic_mean"))


## Car/Van households 

# load car/van data we need to replicate.
Car_by_OA <- read.csv(here("data", "QS416EWDATA.csv"))

# load list of output areas.
OA <- read.csv(here("data", "SAM_OA_DEC_2011_EW.csv"))

# remove all geographies that are not OAs.
Car_by_OA <- Car_by_OA %>%
  rename(OA11CD = GeographyCode) %>%
  semi_join(OA, by = "OA11CD")

# renaming, dropping missings pop and arranging OAs
nocar_mean <-  Car_by_OA %>% 
  rename(HH_car = `QS416EW0001`,
         no_car = `QS416EW0002`) %>% 
  mutate(mean_no_car = no_car/HH_car) %>%
  select(OA11CD, HH_car, no_car, mean_no_car)

# save
write.csv(nocar_mean, here("data", "Car_by_OA_replicate.csv"))

rm(list=c("Car_by_OA", "nocar_mean"))


## Tenure - Social rented (could also look at private)

# load tenure data we need to replicate.
Tenure_by_OA <- read.csv(here("data", "QS405EWDATA.csv"))

# load list of output areas.
OA <- read.csv(here("dataSAM_OA_DEC_2011_EW.csv"))

# remove all geographies that are not OAs.
Tenure_by_OA <- Tenure_by_OA %>%
  rename(OA11CD = GeographyCode) %>%
  semi_join(OA, by = "OA11CD")

# renaming, dropping missings pop and arranging OAs
tenure_mean <-  Tenure_by_OA %>% 
  rename(HH_tenure = `QS405EW0001`,
         social_rent = `QS405EW0006`) %>% 
  mutate(mean_social_rent = social_rent/HH_tenure) %>%
  select(OA11CD, HH_tenure, social_rent, mean_social_rent)

# save
write.csv(tenure_mean, here("data", "Tenure_by_OA_replicate.csv"))

rm(list=c("Tenure_by_OA", "tenure_mean"))


## Age of HRP

# load income data we need to replicate.
Age_by_OA <- read.csv(here("data", "QS111EWDATA.csv"))

# load list of output areas.
OA <- read.csv(here("data", "SAM_OA_DEC_2011_EW.csv"))

# remove all geographies that are not OAs.
Age_by_OA <- Age_by_OA %>%
  rename(OA11CD = GeographyCode) %>%
  semi_join(OA, by = "OA11CD")

# renaming, dropping missings pop and arranging OAs
age_mean <-  Age_by_OA %>% 
  rename(HH_age = `QS111EW0001`,
         hrp_over65 = `QS111EW0014`) %>% 
  mutate(mean_hrp_over65 = hrp_over65/HH_age) %>%
  select(OA11CD, HH_age, hrp_over65, mean_hrp_over65)

# save
write.csv(age_mean, here("data, Agehh_by_OA_replicate.csv"))

rm(list=c("Age_by_OA", "age_mean"))


## Ethnicity

# load ethnicity data we need to replicate.
#NB. Remove first row prior to reading in file
Ethnic_by_OA <- read.csv(here("data", "EGHRP_HHDCOM_UNIT.csv"))

# load list of output areas.
OA <- read.csv(here("data", "SAM_OA_DEC_2011_EW.csv"))

# remove all geographies that are not OAs.
Ethnic_by_OA <- Ethnic_by_OA %>%
  rename(OA11CD = GEO_CODE) %>%
  semi_join(OA, by = "OA11CD")

# renaming, dropping missings pop and arranging OAs
ethnic_mean <- Ethnic_by_OA %>% 
  rename(HH_ethnic = `F130027`,
         White = `F130540`) %>% 
  mutate(mean_white = White/HH_ethnic) %>%
  select(OA11CD, HH_ethnic, White, mean_white)

# save
write.csv(ethnic_mean, here("data", "Ethnichh_by_OA_replicate.csv"))

rm(list=c("Ethnic_by_OA", "ethnic_mean"))


## Religious background - no-religious

#NB. Remove first row prior to reading in file
Religion_by_OA <- read.csv(here("data", "RELIGHRP_HHDCOM_UNIT.csv"))

# load list of output areas.
OA <- read.csv(here"data", "SAM_OA_DEC_2011_EW.csv"))

# remove all geographies that are not OAs.
Religion_by_OA <- Religion_by_OA %>%
  rename(OA11CD = GEO_CODE) %>%
  semi_join(OA, by = "OA11CD")

# renaming, dropping missings pop and arranging OAs
religion_mean <- Religion_by_OA %>% 
  rename(HH_religion = `F132425`,
         no_religion = `F132431`) %>% 
  mutate(mean_no_religion = no_religion/HH_religion) %>%
  select(OA11CD, HH_religion, no_religion, mean_no_religion)

# save
write.csv(religion_mean, here("data", "Religion_by_OA_replicate.csv"))

rm(list=c("Religion_by_OA", "religion_mean"))