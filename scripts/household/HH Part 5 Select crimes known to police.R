################################
#
# Simulating crime data
#
# Part V: Select crimes known to police
#
################################

rm(list=ls())

#seed is set for replication
set.seed(999)

#load packages
library(dplyr)
library(here)
library(rsq)

#load synthetic population of crimes
load(here("data", "HHsynthetic_population_crimes.Rdata"))

#subset individuals who were victimised for property crime
syn_res_OA_theft <- syn_res_OA %>%
  filter(theft != 0)

#repeat Households as many times as they were victimised
syn_res_OA_theft <- as.data.frame(lapply(syn_res_OA_theft, rep, syn_res_OA_theft$theft))

#recode so property crime is 1, other crime types 0
Data_theft <- syn_res_OA_theft %>% 
  mutate(violence = 0,
         theft    = 1,
         damage   = 0)

#remove files to save memory
rm(list=c("syn_res_OA_theft", "syn_res_OA"))

#load in CSEW non-victim form data
load(here("data", "csew_apr11mar12_nvf.Rdata"))

#load CSEW victim form data
load(here("data", "csew_apr11mar12_vf.Rdata"))

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

#join csew with csew_vf. Warnings are not problematic
csew_vf <- left_join(csew_vf, csew, by = "rowlabel")

#recode copsknow variable to binary for regression model
csew_vf <- csew_vf %>% 
  mutate(copsknow = if_else(condition = copsknow == 2, true = 0, false = copsknow),
         copsknow = na_if(x = copsknow, 8),
         copsknow = na_if(x = copsknow, 9))

#filter property crime types
csew_vf_theft <-  csew_vf %>% 
  filter(offence == 61 | offence == 63 | #theftf_i
         offence == 60 | offence == 62 | #thefto_i
         offence == 64 | #biketh_i
         offence == 51 | offence == 52 | offence == 53) #burgla_i

#create GLM formula for predicting copsknow (dep. var.) with demographic variables (ind. var.)
glm_copsknow <- copsknow ~ age_more65 + terraced + hrp_white + one_person + no_income + no_car + social_rent + hrp_no_religion

#estimate copsknow model for property crime
model_repo_theft <- glm(formula = glm_copsknow, family = binomial(link = "logit"), data = csew_vf_theft)
summary(model_repo_theft)
rsq.n(model_repo_theft)

#extract estimates for property crime
Data_theft <- Data_theft %>% 
  mutate(estimates = model_repo_theft$coefficients[1] +
           Data_theft$age_more65       * model_repo_theft$coefficients[2] +
           Data_theft$terraced      * model_repo_theft$coefficients[3] +
           Data_theft$hrp_white     * model_repo_theft$coefficients[4] +
           Data_theft$one_person * model_repo_theft$coefficients[5] +
           Data_theft$no_income  * model_repo_theft$coefficients[6] +
           Data_theft$no_car    * model_repo_theft$coefficients[7] +
           Data_theft$social_rent   * model_repo_theft$coefficients[8] +
           Data_theft$hrp_no_religion   * model_repo_theft$coefficients[9],
         exp_estimates = exp(estimates) / (1 + exp(estimates)),
         copsknow = rbinom(nrow(Data_theft), 1, exp_estimates))

#check property crime frequency distributions comparison
prop.table(table(csew_vf_theft$copsknow))
prop.table(table(Data_theft$copsknow))

#select only crimes known to the police
Data_theft <- Data_theft %>%
  filter(copsknow == 1) %>%
  select(-estimates, -exp_estimates)

#check first rows
head(Data_theft)

#save synthetic police data of property crimes as RData
save(Data_theft, file = here("data", "HHsynthetic_police_property.RData"))

#remove files to save memory
rm(list=c("csew_vf_theft", "Data_theft", "csew", "csew_vf",
          "model_repo_theft"))

#load synthetic population of crimes
load(here("data", "HHsynthetic_population_crimes.Rdata"))

#subset individuals who were victimised for damage
syn_res_OA_dam <- syn_res_OA %>%
  filter(damage != 0)

#repeat individuals as many times as they were victimised
syn_res_OA_dam <- as.data.frame(lapply(syn_res_OA_dam, rep, syn_res_OA_dam$damage))

#recode so damage is 1, other crime types 0
Data_dam <- syn_res_OA_dam %>% 
  mutate(violence = 0,
         theft    = 0,
         damage   = 1)

#remove files to save memory
rm(list=c("syn_res_OA_dam", "syn_res_OA"))

#load in CSEW non-victim form data
load(here("data", "csew_apr11mar12_nvf.Rdata"))

#load CSEW victim form data
load(here("data", "csew_apr11mar12_vf.Rdata"))

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

#join csew with csew_vf. Warnings are not problematic
csew_vf <- left_join(csew_vf, csew, by = "rowlabel")

#recode copsknow variable to binary for regression model
csew_vf <- csew_vf %>% 
  mutate(copsknow = if_else(condition = copsknow == 2, true = 0, false = copsknow),
         copsknow = na_if(x = copsknow, 8),
         copsknow = na_if(x = copsknow, 9))

#filter damage types
csew_vf_dam <-  csew_vf %>% 
  filter(offence == 80 | offence == 83 | offence == 84 | offence == 85 | offence == 86 | #homeva_i
         offence == 81 | offence == 82) #mv.van_i

#estimate copsknow model for damage crime
model_repo_dam <- glm(formula = glm_copsknow, family = binomial(link = "logit"), data = csew_vf_dam)
summary(model_repo_dam)
rsq.n(model_repo_dam)

#extract estimates for damage crime
Data_dam <- Data_dam %>% 
  mutate(estimates = model_repo_dam$coefficients[1] +
           Data_dam$age_more65       * model_repo_dam$coefficients[2] +
           Data_dam$terraced      * model_repo_dam$coefficients[3] +
           Data_dam$hrp_white     * model_repo_dam$coefficients[4] +
           Data_dam$one_person * model_repo_dam$coefficients[5] +
           Data_dam$no_income  * model_repo_dam$coefficients[6] +
           Data_dam$no_car    * model_repo_dam$coefficients[7] +
           Data_dam$social_rent   * model_repo_dam$coefficients[8] +
           Data_dam$hrp_no_religion   * model_repo_dam$coefficients[9],
         exp_estimates = exp(estimates) / (1 + exp(estimates)),
         copsknow = rbinom(nrow(Data_dam), 1, exp_estimates))

#check damage crime frequency distributions comparison
prop.table(table(csew_vf_dam$copsknow))
prop.table(table(Data_dam$copsknow))

#select only crimes known to the police
Data_dam <- Data_dam %>%
  filter(copsknow == 1) %>%
  select(-estimates, -exp_estimates)

#check first rows
head(Data_dam)

#save synthetic police data of property crimes as RData
save(Data_dam, file = here("data", "HHsynthetic_police_damage.RData"))

#remove files to save memory
rm(list=c("csew_vf_dam", "csew", "csew_vf",
          "model_repo_dam"))

#load synthetic police data for property crime
load(here("data", "HHsynthetic_police_property.RData"))

#row bind each crime type data frame
Data_crimes <- bind_rows(Data_theft, Data_dam)

#load underrecording estimates in PFAs data
csew_under <- read.csv("data", "csew_prc_pfa_11_v3.csv"))

#select variables of interest
csew_under <- csew_under %>%
  dplyr::select(PFA17NM, theft.ratio, damage.ratio)

#remove files to save memory
rm(list=c("Data_dam", "Data_theft", "glm_copsknow"))

#load OA to LAD lookup
lookup <- read.csv(here("data", "Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District__December_2017__Lookup_with_Area_Classifications_in_Great_Britain.csv"))

#select variables of interest in lookup
lookup <- lookup %>%
  select(OA11CD, LAD17CD) 

#load LAD to PFA lookup
lookup2 <- read.csv("https://opendata.arcgis.com/datasets/31dec1a0f04a435dadd43de6292ea260_0.csv")

#select variables of interest in lookup
lookup2 <- lookup2 %>%
  select(LAD17CD, PFA17CD, PFA17NM) 

#merge two lookups
lookup <- left_join(lookup, lookup2, by = "LAD17CD")

#remove files to save memory 
rm(list=c("lookup2"))

#add PFA information to synthetic crime data
Data_crimes <- left_join(Data_crimes, lookup, by = "OA11CD")

#combine City of London Police and Metropolitan Police
Data_crimes <- Data_crimes %>%
  mutate(PFA17CD = ifelse(PFA17CD == 'E23000034', 'E23000001', PFA17CD),
         PFA17NM = ifelse(PFA17NM == "London, City of", "Metropolitan Police", PFA17NM))

#add underrecording estimates to synthetic crime data
Data_crimes <- left_join(Data_crimes, csew_under, by = "PFA17NM")

#select ratio that applies in each case
Data_crimes <- Data_crimes %>%
  mutate(ratio = ifelse(theft == 1, theft.ratio, NA),
         ratio = ifelse(damage == 1, damage.ratio, ratio),
         ratio = ifelse(ratio > 1, 1, ratio)) %>%
  dplyr::select(-theft.ratio, -damage.ratio, -LAD17CD)

#extract crime records
Data_crimes <- Data_crimes %>%
  mutate(copsrec = rbinom(nrow(Data_crimes), 1, ratio))

#select only crimes reported by the police
Data_crimes <- Data_crimes %>%
  filter(copsrec == 1) %>%
  dplyr::select(-PFA17CD, -PFA17NM, -ratio)

#check first rows
head(Data_crimes)

#save all synthetic police data as RData
save(Data_crimes, file = here("data", "HHsynthetic_police_crimes.RData"))
