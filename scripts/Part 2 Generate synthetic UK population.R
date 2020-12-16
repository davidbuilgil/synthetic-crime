################################
#
# Simulating crime data
#
# Part II: Generate synthetic UK population
#
################################

rm(list=ls())

#increase memory limit a bit
memory.limit()
memory.limit(size=9000)

#seed is set for replication
set.seed(999)

#load packages
library(dplyr)
library(here)
library(BinNor)
library(BinOrdNonNor)
library(faux)

#load age data
Age_by_OA <- read.csv(here("data", "Age_by_OA_replicate.csv"))

#calculate number of residents in the England and Wales
N <- sum(Age_by_OA$Pop)
N

#calculate number of Output Areas
D <- nrow(Age_by_OA)
D

#create tibble with one row for unit
syn_res <- tibble(ID = 1:N)

#assign each resident to an OA based on the size of the resident populations.
syn_res <- syn_res %>% 
  mutate(OA11CD = rep.int(x = Age_by_OA$OA, times = Age_by_OA$Pop),
         OA11NUM = rep.int(x = 1:D, times = Age_by_OA$Pop))

#load sex data
Sex_by_OA <- read.csv(here("data", "Sex_by_OA_replicate.csv"))

#load ethnicity data
Ethnic_by_OA <- read.csv(here("data", "Ethnic_by_OA_replicate.csv"))

#load income data
Income_by_OA <- read.csv(here("data", "Income_by_OA_replicate.csv"))

#load education data
Edu_by_OA <- read.csv(here("data", "Edu_by_OA_replicate.csv"))

#load marriage data
Married_by_OA <- read.csv(here("data", "Married_by_OA_replicate.csv"))

#load country born data
BornUK_by_OA <- read.csv(here("data", "BornUK_by_OA_replicate.csv"))

#merge census data for OAs
Census <- Age_by_OA %>%
  left_join(BornUK_by_OA, by = "OA11CD") %>%
  left_join(Edu_by_OA, by = "OA11CD") %>%
  left_join(Ethnic_by_OA, by = "OA11CD") %>%
  left_join(Income_by_OA, by = "OA11CD") %>%
  left_join(Married_by_OA, by = "OA11CD") %>%
  left_join(Sex_by_OA, by = "OA11CD") %>%
  dplyr::select(OA11CD, Pop, mean_age, sd_age, mean_bornuk, Mean_level4_edu,
                Mean_white, mean_no_income, mean_married, Mean_male)

#remove files to save memory
rm(list=c("Age_by_OA", "BornUK_by_OA", "Edu_by_OA", "Ethnic_by_OA", "Income_by_OA",
          "Married_by_OA", "Sex_by_OA"))

#load CSEW non-victim form data
load(here("data", "csew_apr11mar12_nvf.Rdata"))

#recode sociodemographic variables in CSEW

#recode NA age for those over 120 years old and create dummy variables by age groups
csew <- csew %>% 
  mutate(age = ifelse(test = age > 120, yes = NA, no = age),
         age_less20 = ifelse(test = age < 20, yes = 1, no = 0),
         age_20to34 = ifelse(test = age >= 20 & age < 35, yes = 1, no = 0),
         age_35to49 = ifelse(test = age >= 35 & age < 50, yes = 1, no = 0),
         age_50to65 = ifelse(test = age >= 50 & age < 66, yes = 1, no = 0),
         age_more65 = ifelse(test = age >= 65, yes = 1, no = 0))

#recode sex 1 male 0 other
csew <- csew %>% 
  mutate(sex = if_else(condition = sex == 2, true = 0, false = sex))

#recode ethnicity 1 white 0 other
csew <- csew %>% 
  mutate(reseth = if_else(condition = reseth != 1, true = 0, false = reseth))

#recode employment 1 no income 0 income
csew <- csew %>% 
  mutate(remploy = if_else(condition = remploy == 2 | remploy == 3, true = 1, false = 0))

#recode education 1 higher education 0 no
csew <- csew %>% 
  mutate(educat2 = ifelse(test = educat2 > 10, yes = NA, no = educat2),
         educat2 = if_else(condition = educat2 == 1 | educat2 == 2 | educat2 == 3, true = 1, false = 0))

#recode country of birth 1 born uk 0 other
csew <- csew %>%
  mutate(cry2 = ifelse(test = cry2 > 7, yes = NA, no = cry2),
         cry2 = if_else(condition = cry2 == 1 | cry2 == 2 | cry2 == 3 | cry2 == 4 | cry2 == 5, true = 1, false = 0))

#recode marriage status 1 married 0 no
csew <- csew %>%
  mutate(marsta = ifelse(test = marsta > 7, yes = NA, no = marsta),
         marsta = if_else(condition = marsta == 2 | marsta == 3, true = 1, false = 0))

#select variables of interest in csew
csew <- csew %>%
  dplyr::select(cry2, educat2, reseth, remploy, marsta, sex, age)

#calculate correlation matrix in CSEW
cor <- cor(csew, use = "pairwise.complete.obs")
cor

#remove files to save memory
rm(list=c("csew"))

#join the OA-level data with the synthetic individual-level data
syn_res_OA <- left_join(syn_res, Census, by = "OA11CD")

#remove files to save memory
rm(list=c("syn_res", "Census"))

#generate empty dataframe to save results
syn_res_OA_D <- data.frame()

#garbage collection to save memory
gc()

for(d in 1:D) {
  
  print(d)
  
  while(TRUE) {
    
    #select units in area d
    syn_res_OA_d <- syn_res_OA %>%
      filter(OA11NUM == d)
    
    #save list of probability vectors for binary variables
    plist_d <- as.list(syn_res_OA_d[1,7:12])
    
    #avoid 0 and 1 in list of probability vectors
    plist_d <- ifelse(plist_d == 0, 0.01, plist_d)
    plist_d <- ifelse(plist_d == 1, 0.99, plist_d)
    
    #calculate feasible range for correlation matrix
    cor_d <- try(valid.limits.BinOrdNN(plist = plist_d, 
                                       skew.vec = 0, 
                                       kurto.vec = 0,
                                       no.bin = 6, no.ord = 0, 
                                       no.NN = 1),
                 silent=TRUE)
    
    #adjust correlation matrix to feasible range
    cor_d2 <- ifelse(cor < cor_d$lower, cor_d$lower + 0.01, cor)
    cor_d2 <- ifelse(cor_d2 > cor_d$upper, cor_d$upper - 0.01, cor_d2)
    
    #calculate sigma square
    sigma <- try(compute.sigma.star(no.bin = 6, no.nor = 1,
                                    prop.vec.bin = as.numeric(plist_d),
                                    corr.mat = cor_d2),
                 silent=TRUE)
    
    #generate multivariate binary and normal distributions
    syn_res_OA_v <- try(jointly.generate.binary.normal(no.rows = nrow(syn_res_OA_d),
                                                       no.bin = 6, no.nor = 1,
                                                       prop.vec.bin = as.numeric(plist_d),
                                                       mean.vec.nor = as.numeric(syn_res_OA_d[1,5]),
                                                       var.nor = as.numeric(syn_res_OA_d[1,6]^2),
                                                       sigma_star = sigma$sigma_star,
                                                       continue.with.warning=TRUE),
                        silent=TRUE)
    
    if(!is(sigma, 'try-error') & !is(cor_d, 'try-error') & !is(cor_d, 'syn_res_OA_v')) break
    
  }
  
  #convert generates population as dataframe
  syn_res_OA_v <- as.data.frame(syn_res_OA_v)
  
  #change names and transform normal distribution of age into truncated normal
  syn_res_OA_v <- syn_res_OA_v %>%
    rename(BornUK    = V1,
           High_edu  = V2,
           White     = V3,
           No_income = V4,
           Married   = V5,
           Male      = V6,
           Age       = V7) %>%
    mutate(Age = norm2trunc(Age, min = 0, max = 100))
  
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
  dplyr::select(ID, OA11CD, Age, Male, BornUK, White, No_income, Married, High_edu)

#save synthetic UK population as RData
save(syn_res_OA, file = here("data", "synthetic_population.RData"))
