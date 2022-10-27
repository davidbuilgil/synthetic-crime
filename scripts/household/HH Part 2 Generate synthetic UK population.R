################################
#
# Simulating crime data
#
# Part II a: Generate synthetic UK population
#
################################

rm(list=ls())

#seed is set for replication
set.seed(999)

#load packages
library(dplyr)
library(here)
library(BinNor)
library(BinOrdNonNor)
library(faux)

#load age data
Age_by_OA <- read.csv(here("data", "Agehh_by_OA_replicate.csv"))

#calculate number of households in England and Wales
N <- sum(Age_by_OA$HH_age)
N

#calculate number of Output Areas
D <- nrow(Age_by_OA)
D

#create a tibble with one row for each synthetic unit
#this will be filled with information later
syn_res <- tibble(ID = 1:N)

#assign each resident to an OA based on the size of the household populations.
syn_res <- syn_res %>% 
  mutate(OA11CD = rep.int(x = Age_by_OA$OA, times = Age_by_OA$HH_age),
         OA11NUM = rep.int(x = 1:D, times = Age_by_OA$HH_age))

#load accommodation type data
Accommodation_by_OA <- read.csv(here("data", "Accommodation_by_OA_replicate.csv"))

#load ethnicity data
Ethnic_by_OA <- read.csv(here("data", "Ethnichh_by_OA_replicate.csv"))

#load household size data
HHsize_by_OA <- read.csv(here("data", "HHsize_by_OA_replicate.csv"))

#load economic activity data
Economic_by_OA <- read.csv(here("data", "Economic_by_OA_replicate.csv"))

#load car/van data
Car_by_OA <- read.csv(here("data", "Car_by_OA_replicate.csv"))

#load tenure data
Tenure_by_OA <- read.csv(here("data", "Tenure_by_OA_replicate.csv"))

#load religion data
Religion_by_OA <- read.csv(here("data", "Religion_by_OA_replicate.csv"))

#merge census data for OAs
Census <- Age_by_OA %>%
  left_join(Accommodation_by_OA, by = "OA11CD") %>%
  left_join(Ethnic_by_OA, by = "OA11CD") %>%
  left_join(HHsize_by_OA, by = "OA11CD") %>%
  left_join(Economic_by_OA, by = "OA11CD") %>%
  left_join(Car_by_OA, by = "OA11CD") %>%
  left_join(Tenure_by_OA, by = "OA11CD") %>%
  left_join(Religion_by_OA, by = "OA11CD") %>%
  rename(Households = `HH_age`)%>% 
  dplyr::select(OA11CD, Households, mean_hrp_over65, mean_terraced, mean_white, mean_one_person, mean_no_income, mean_no_car,
                mean_social_rent, mean_no_religion)

#remove files to save memory
rm(list=c("Agehh_by_OA", "Accommodation_by_OA", "Ethnichh_by_OA", "HHsize_by_OA", "Economic_by_OA",
          "Car_by_OA", "Tenure_by_OA", "Religion_by_OA"))

#load CSEW non-victim form data
load(here("data", "csew_apr11mar12_nvf.Rdata"))

#recode sociodemographic variables in CSEW

#Head of houshold 65 or over
table(csew$hrpage)
csew <- csew %>% 
  mutate(hrpage = ifelse(test = hrpage > 120, yes = NA, no = hrpage),
         age_more65 = ifelse(test = hrpage >= 65, yes = 1, no = 0))
table(csew$age_more65)

#generate terraced (recode accharm3 1 terraced 0 other()
table(csew$accharm3)
csew <- csew %>% 
  mutate(terraced = if_else(condition = accharm3 == 3, true = 1, false = 0))
table(csew$terraced)

#Head of household white
table(csew$ethnhead)
csew <- csew %>% 
  mutate(hrp_white = if_else(condition = ethnhead == 1, true = 1, false = 0))
table(csew$hrp_white)

#one person households
table(csew$nadults, csew$nchil)
csew <- csew %>% 
  mutate(one_person = if_else(condition = nadults == 1 & nchil == 0, true = 1, false = 0))
table(csew$one_person)

#No income (include students as no income to match census)
table(csew$hemploy)
table(csew$hrpsec)
csew <- csew %>% 
  mutate(no_income = if_else(condition = hemploy == 2 | hemploy == 3 | hrpsec ==15, true = 1, false = 0))
table(csew$no_income)

#No car household
table(csew$numcars)
csew <- csew %>% 
  mutate(no_car = if_else(condition = is.na(csew$numcars), true = 1, false = 0))
table(csew$no_car)

#Social renters
table(csew$tenharm)
csew <- csew %>% 
  mutate(social_rent = if_else(condition = tenharm == 2, true = 1, false = 0))
table(csew$social_rent)

#Head of household non-religious
table(csew$hrprelg3)
csew <- csew %>% 
  mutate(hrp_no_religion = if_else(condition = hrprelg3 == 6, true = 1, false = 0))
table(csew$hrp_no_religion)

#select variables of interest in csew
csew <- csew %>%
  dplyr::select(age_more65, terraced, hrp_white, one_person, no_income, no_car, social_rent, hrp_no_religion)

#calculate correlation matrix in CSEW
cor <- cor(csew, use = "pairwise.complete.obs")
cor

#remove files to save memory
rm(list=c("csew"))

#join the OA-level data with the synthetic household-level data
syn_res_OA <- left_join(syn_res, Census, by = "OA11CD")

#remove files to save memory
rm(list=c("syn_res", "Census"))

#generate empty dataframe to save results
syn_res_OA_D <- data.frame()

#generate multivariate variables
library(BinNor)
library(BinOrdNonNor)
library(faux)
library(MultiOrd)

#garbage collection to save memory
gc()

for(d in 1:D) {
  
  print(d)
  
  while(TRUE) {
    
    #select units in area d
    syn_res_OA_d <- syn_res_OA %>%
      filter(OA11NUM == d)
    
    #save list of probability vectors for binary variables
    plist_d <- as.list(syn_res_OA_d[1,5:12])
    
    #avoid 0 and 1 in list of probability vectors
    plist_d <- ifelse(plist_d == 0, 0.01, plist_d)
    plist_d <- ifelse(plist_d == 1, 0.99, plist_d)
    
    #calculate feasible range for correlation matrix
    cor_d <- try(valid.limits.BinOrdNN(plist = plist_d, 
                                       skew.vec = 0, 
                                       kurto.vec = 0,
                                       no.bin = 8, no.ord = 0, 
                                       no.NN = 0),
                 silent=TRUE)
    
    #adjust correlation matrix to feasible range
    cor_d2 <- ifelse(cor < cor_d$lower, cor_d$lower + 0.01, cor)
    cor_d2 <- ifelse(cor_d2 > cor_d$upper, cor_d$upper - 0.01, cor_d2)
    
    #calculate sigma square
    sigma <- try(MultiOrd::compute.sigma.star(as.numeric(plist_d), cor_d2),
                 silent=TRUE)
    
    #generate multivariate binary and normal distributions
    syn_res_OA_v <- try(jointly.generate.binary.normal(no.rows = nrow(syn_res_OA_d),
                                                       no.bin = 8, no.nor = 0,
                                                       prop.vec.bin = as.numeric(plist_d),
                                                       #mean.vec.nor = as.numeric(syn_res_OA_d[1,5]),
                                                       #var.nor = as.numeric(syn_res_OA_d[1,6]^2),
                                                       sigma_star = sigma,
                                                       continue.with.warning=TRUE),
                        silent=TRUE)
    
    if(!is(sigma, 'try-error') & !is(cor_d, 'try-error') & !is(cor_d, 'syn_res_OA_v')) break
    
  }
  
  #convert generates population as dataframe
  syn_res_OA_v <- as.data.frame(syn_res_OA_v)
  
  #change names 
  syn_res_OA_v <- syn_res_OA_v %>%
    rename(age_more65 = V1,
           terraced = V2,
           hrp_white = V3,
           one_person = V4,
           no_income = V5,
           no_car = V6,
           social_rent = V7,
           hrp_no_religion = V8) 
  
  #create dataset of generated population in area d
  syn_res_OA_d <- cbind(syn_res_OA_d[,1:3], syn_res_OA_v)
  
  #append at the end of synthetic dataset
  syn_res_OA_D <- rbind(syn_res_OA_D, syn_res_OA_d)
  
}

#remove files to save memory
rm(list=c("syn_res_OA", "cor", "cor_d", "cor_d2", "sigma", "syn_res_OA_d", "syn_res_OA_v",
          "d", "D", "N"))

#select variables of interest
syn_res_OA <- syn_res_OA_D %>%
  dplyr::select(ID, OA11CD, age_more65, terraced, hrp_white, one_person, no_income, no_car, social_rent, hrp_no_religion)

#save synthetic UK population as RData
save(syn_res_OA, file = here("data", "HHsynthetic_population.RData"))
