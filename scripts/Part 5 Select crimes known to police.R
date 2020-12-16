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

#load synthetic population
load(here("data", "synthetic_population_crimes_a.Rdata"))
load(here("data", "synthetic_population_crimes_b.Rdata"))
load(here("data", "synthetic_population_crimes_c.Rdata"))
load(here("data", "synthetic_population_crimes_d.Rdata"))
load(here("data", "synthetic_population_crimes_e.Rdata"))
load(here("data", "synthetic_population_crimes_f.Rdata"))
load(here("data", "synthetic_population_crimes_g.Rdata"))
syn_res_OA <- rbind(syn_res_OA.a, syn_res_OA.b, syn_res_OA.c,
                    syn_res_OA.d, syn_res_OA.e, syn_res_OA.f,
                    syn_res_OA.g)

#remove objects
rm(list=c("syn_res_OA.a", "syn_res_OA.b", "syn_res_OA.c",
          "syn_res_OA.d", "syn_res_OA.e", "syn_res_OA.f",
          "syn_res_OA.g"))

#subset individuals who were victimised for violent crime
syn_res_OA_vio <- syn_res_OA %>%
  filter(violence != 0)

#repeat individuals as many times as they were victimised
syn_res_OA_vio <- as.data.frame(lapply(syn_res_OA_vio, rep, syn_res_OA_vio$violence))

#recode so violence is 1, other crime types 0
Data_violence <- syn_res_OA_vio %>% 
  mutate(violence = 1,
         theft    = 0,
         damage   = 0)

#remove objects
rm(list=c("syn_res_OA_vio"))

#load in CSEW non-victim form data
load(here("data", "csew_apr11mar12_nvf.Rdata"))

#load CSEW victim form data
load(here("data", "csew_apr11mar12_vf.Rdata"))

#recode NA age for those over 120 years old
csew <- csew %>% 
  mutate(age = ifelse(test = age > 120, yes = NA, no = age))

#recode sex 1 male 0 other
csew <- csew %>% 
  mutate(sex = if_else(condition = sex == 2, true = 0, false = sex))

#recode ethnicity 1 white 0 other
csew <- csew %>% 
  mutate(reseth = if_else(condition = reseth != 1, true = 0, false = reseth))

#recode employment 1 not employed 0 employed
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

#join csew with csew_vf. Warnings are not problematic
csew_vf <- left_join(csew_vf, csew, by = "rowlabel")

#recode copsknow variable to binary for regression model
csew_vf <- csew_vf %>% 
  mutate(copsknow = if_else(condition = copsknow == 2, true = 0, false = copsknow),
         copsknow = na_if(x = copsknow, 8),
         copsknow = na_if(x = copsknow, 9))

#filter violent crime types
csew_vf_violent <-  csew_vf %>% 
  filter(offence == 13 | offence == 21 | #common_i
           offence == 11 | offence == 12 | offence == 32 | offence == 33 | #wound_i
           offence == 41 | offence == 42) #robber_i

#create GLM formula for predicting copsknow (dep. var.) with demographic variables (ind. var.)
glm_copsknow <- copsknow ~ age + sex + reseth + remploy + educat2 + cry2 + marsta

#estimate copsknow model for violent crime
model_repo_violent <- glm(formula = glm_copsknow, family = binomial(link = "logit"), data = csew_vf_violent)
summary(model_repo_violent)
rsq.n(model_repo_violent)

#extract estimates for violent crime
Data_violence <- Data_violence %>% 
  mutate(estimates = model_repo_violent$coefficients[1] +
           Data_violence$Age       * model_repo_violent$coefficients[2] +
           Data_violence$Male      * model_repo_violent$coefficients[3] +
           Data_violence$White     * model_repo_violent$coefficients[4] +
           Data_violence$No_income * model_repo_violent$coefficients[5] +
           Data_violence$High_edu  * model_repo_violent$coefficients[6] +
           Data_violence$BornUK    * model_repo_violent$coefficients[7] +
           Data_violence$Married   * model_repo_violent$coefficients[8],
         exp_estimates = exp(estimates) / (1 + exp(estimates)),
         copsknow = rbinom(nrow(Data_violence), 1, exp_estimates))

#check violent crime frequency distributions comparison
prop.table(table(csew_vf_violent$copsknow))
prop.table(table(Data_violence$copsknow))

#select only crimes known to the police
Data_violence <- Data_violence %>%
  filter(copsknow == 1) %>%
  select(-estimates, -exp_estimates)

#check first rows
head(Data_violence)

#remove objects
rm(list=c("csew_vf_violent", "model_repo_violent"))

#subset individuals who were victimised for property crime
syn_res_OA_theft <- syn_res_OA %>%
  filter(theft != 0)

#repeat individuals as many times as they were victimised
syn_res_OA_theft <- as.data.frame(lapply(syn_res_OA_theft, rep, syn_res_OA_theft$theft))

#recode so property crime is 1, other crime types 0
Data_theft <- syn_res_OA_theft %>% 
  mutate(violence = 0,
         theft    = 1,
         damage   = 0)

#remove objects
rm(list=c("syn_res_OA_theft"))

#filter property crime types
csew_vf_theft <-  csew_vf %>% 
  filter(offence == 43 | offence == 44 | offence == 45 | #theftp_i
           offence == 61 | offence == 63 | #theftf_i
           offence == 60 | offence == 62 | #thefto_i
           offence == 64 | #biketh_i
           offence == 51 | offence == 52 | offence == 53) #burgla_i

#estimate copsknow model for property crime
model_repo_theft <- glm(formula = glm_copsknow, family = binomial(link = "logit"), data = csew_vf_theft)
summary(model_repo_theft)
rsq.n(model_repo_theft)

#extract estimates for property crime
Data_theft <- Data_theft %>% 
  mutate(estimates = model_repo_theft$coefficients[1] +
           Data_theft$Age       * model_repo_theft$coefficients[2] +
           Data_theft$Male      * model_repo_theft$coefficients[3] +
           Data_theft$White     * model_repo_theft$coefficients[4] +
           Data_theft$No_income * model_repo_theft$coefficients[5] +
           Data_theft$High_edu  * model_repo_theft$coefficients[6] +
           Data_theft$BornUK    * model_repo_theft$coefficients[7] +
           Data_theft$Married   * model_repo_theft$coefficients[8],
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

#remove objects
rm(list=c("csew_vf_theft", "model_repo_theft"))

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

#remove objects
rm(list=c("syn_res_OA_dam"))

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
           Data_dam$Age       * model_repo_dam$coefficients[2] +
           Data_dam$Male      * model_repo_dam$coefficients[3] +
           Data_dam$White     * model_repo_dam$coefficients[4] +
           Data_dam$No_income * model_repo_dam$coefficients[5] +
           Data_dam$High_edu  * model_repo_dam$coefficients[6] +
           Data_dam$BornUK    * model_repo_dam$coefficients[7] +
           Data_dam$Married   * model_repo_dam$coefficients[8],
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

#remove objects
rm(list=c("csew_vf_dam", "csew", "csew_vf",
          "model_repo_dam"))

#row bind each crime type data frame
Data_crimes <- bind_rows(Data_violence, Data_theft, Data_dam)

#load underrecording estimates in PFAs
csew_under <- read.csv(here("data", "csew_prc_pfa_11_v3.csv"))

#select variables of interest
csew_under <- csew_under %>%
  dplyr::select(PFA17NM, violence.ratio, theft.ratio, damage.ratio)

#remove objects
rm(list=c("Data_dam", "Data_theft", "Data_violence", "glm_copsknow"))

#load OA to LAD lookup
lookup <- read.csv(here("data", "Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District__December_2017__Lookup_with_Area_Classifications_in_Great_Britain.csv"))

#select variables of interest in lookup
lookup <- lookup %>%
  select(1, 10) %>%
  rename(OA11CD = 1)

#load LAD to PFA lookup
lookup2 <- read.csv(here("data", "Local_Authority_District_to_Community_Safety_Partnerships_to_Police_Force_Areas__January_2017__Lookup_in_England_and_Wales_Version_2.csv"))

#select variables of interest in lookup
lookup2 <- lookup2 %>%
  select(1, 3, 5) %>%
  rename(LAD17CD = 1)

#merge two lookups
lookup <- left_join(lookup, lookup2, by = "LAD17CD")

#remove objects 
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
  mutate(ratio = ifelse(violence == 1, violence.ratio, NA),
         ratio = ifelse(theft == 1, theft.ratio, ratio),
         ratio = ifelse(damage == 1, damage.ratio, ratio),
         ratio = ifelse(ratio > 1, 1, ratio)) %>%
  dplyr::select(-violence.ratio, -theft.ratio, -damage.ratio, -LAD17CD)

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
save(Data_crimes, file = here("data", "synthetic_police_crimes.RData"))
