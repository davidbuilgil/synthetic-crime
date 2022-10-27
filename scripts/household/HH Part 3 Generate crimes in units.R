################################
#
# Simulating crime data
#
# Part III: Generate crimes suffered by population units
#
################################

rm(list=ls())

#seed is set for replication
set.seed(999)

#load packages
library(dplyr)
library(here)
library(tidyr)
library(MASS)
library(rsq)
library(DescTools)

#load synthetic population
load(here("data", "HHsynthetic_population.RData"))
syn_res_OA <- syn_res

#load CSEW non-victim form data
load(here("data", "csew_apr11mar12_nvf.Rdata"))

#Data manipulation - CSEW to match ONS data
#Head of household 65 or over
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

#Restrict to household crimes - theft and damage... [theft removes theftp...]
#data handling (alternative version)
csew <- csew %>%
  mutate(theft2    = (theftf_i + thefto_i + biketh_i + burgla_i) /10000,
         damage2   = (homeva_i + mv.van_i)/10000)

#run negative binomial models to generate estimates from CSEW

model_theft <- glm.nb(theft2 ~ age_more65 + terraced + hrp_white + one_person + no_income + no_car + social_rent + hrp_no_religion, data = csew)
summary(model_theft)
rsq.n(model_theft)
RMSE(model_theft)/(max(csew$theft2)-min(csew$theft2))

model_damage <- glm.nb(damage2 ~ age_more65 + terraced + hrp_white + one_person + no_income + no_car + social_rent + hrp_no_religion, data = csew)
summary(model_damage)
rsq.n(model_damage)
RMSE(model_damage)/(max(csew$damage2)-min(csew$damage2))

#remove files to save memory
rm(list=c("csew", "syn_res"))

#combine estimates for each crime type, assigning to a vector

#property crime
eta_theft <- model_theft$coefficients[1] +
  syn_res_OA$age_more65 * model_theft$coefficients[2] +
  syn_res_OA$terraced * model_theft$coefficients[3] +
  syn_res_OA$hrp_white * model_theft$coefficients[4] +
  syn_res_OA$one_person * model_theft$coefficients[5] +
  syn_res_OA$no_income * model_theft$coefficients[6] +
  syn_res_OA$no_car * model_theft$coefficients[7] +
  syn_res_OA$social_rent * model_theft$coefficients[8] +
  syn_res_OA$hrp_no_religion * model_theft$coefficients[9]

#damage
eta_damage <- model_damage$coefficients[1] +
  syn_res_OA$age_more65 * model_damage$coefficients[2] +
  syn_res_OA$terraced * model_damage$coefficients[3] +
  syn_res_OA$hrp_white * model_damage$coefficients[4] +
  syn_res_OA$one_person * model_damage$coefficients[5] +
  syn_res_OA$no_income * model_damage$coefficients[6] +
  syn_res_OA$no_car * model_damage$coefficients[7] +
  syn_res_OA$social_rent * model_damage$coefficients[8] +
  syn_res_OA$hrp_no_religion * model_damage$coefficients[9]

#save thetas
theta_theft <- model_theft$theta
theta_damage <- model_damage$theta

#remove files to save memory
rm(list=c("model_damage", "model_theft"))

#generate property crimes based on these estimates for the synthetic individual data
syn_res_OA <-  syn_res_OA %>% 
  mutate(theft = rnbinom(length(eta_theft), mu = exp(eta_theft), size = theta_theft))

#remove files to save memory
rm(list=c("eta_theft", "theta_theft"))

#generate damage crimes based on these estimates for the synthetic individual data
syn_res_OA <-  syn_res_OA %>% 
  mutate(damage = rnbinom(length(eta_damage), mu = exp(eta_damage), size = theta_damage))

#remove files to save memory
rm(list=c("eta_damage", "theta_damage"))

#check first rows
head(syn_res_OA, 30)

sum(syn_res_OA$theft)
sum(syn_res_OA$damage)

#cap five crimes per crime type
#cap <- function(x, na.rm = FALSE) ifelse(x > 5, 5, x)
#syn_res_OA <- syn_res_OA %>%
#  mutate_at(c("violence", "theft", "damage"), cap)

#save synthetic UK population with crimes as RData
save(syn_res_OA, file = here("data", "HHsynthetic_population_crimes.RData"))
