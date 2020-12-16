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
load(here("data", "synthetic_population_a.Rdata"))
load(here("data", "synthetic_population_b.Rdata"))
load(here("data", "synthetic_population_c.Rdata"))
load(here("data", "synthetic_population_d.Rdata"))
load(here("data", "synthetic_population_e.Rdata"))
load(here("data", "synthetic_population_f.Rdata"))
load(here("data", "synthetic_population_g.Rdata"))
syn_res_OA <- rbind(syn_res_OA.a, syn_res_OA.b, syn_res_OA.c,
                    syn_res_OA.d, syn_res_OA.e, syn_res_OA.f,
                    syn_res_OA.g)

#remove objects
rm(list=c("syn_res_OA.a", "syn_res_OA.b", "syn_res_OA.c",
          "syn_res_OA.d", "syn_res_OA.e", "syn_res_OA.f",
          "syn_res_OA.g"))

#load CSEW non-victim form data
load(here("data", "csew_apr11mar12_nvf.Rdata"))

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
  mutate(work2 = ifelse(test = work2 > 3, yes = NA, no = work2), 
         work2 = if_else(condition = work2 == 2, true = 0, false = work2))

#recode employment (different variable) 1 not employed 0 employed
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

#data handling (alternative version)
csew <- csew %>%
  mutate(violence2 = (common_i + wound_i + robber_i) /10000,
         theft2    = (theftp_i + theftf_i + thefto_i + biketh_i + burgla_i) /10000,
         damage2   = (homeva_i + mv.van_i)/10000)

#run negative binomial models to generate estimates from CSEW

model_violence <- glm.nb(violence2 ~ age + sex + reseth + remploy + educat2 + cry2 + marsta, data = csew)
summary(model_violence)
rsq.n(model_violence)
RMSE(model_violence)/(max(csew$violence2)-min(csew$violence2))

model_theft <- glm.nb(theft2 ~ age + sex + reseth + remploy + educat2 + cry2 + marsta, data = csew)
summary(model_theft)
rsq.n(model_theft)
RMSE(model_theft)/(max(csew$theft2)-min(csew$theft2))

model_damage <- glm.nb(damage2 ~ age + sex + reseth + remploy + educat2 + cry2 + marsta, data = csew)
summary(model_damage)
rsq.n(model_damage)
RMSE(model_damage)/(max(csew$damage2)-min(csew$damage2))

#remove objects
rm(list=c("csew", "syn_res"))

#combine estimates for each crime type, assigning to a vector
#violent crime
eta_violence <- model_violence$coefficients[1] +
  syn_res_OA$Age * model_violence$coefficients[2] +
  syn_res_OA$Male * model_violence$coefficients[3] +
  syn_res_OA$White * model_violence$coefficients[4] +
  syn_res_OA$No_income * model_violence$coefficients[5] +
  syn_res_OA$High_edu * model_violence$coefficients[6] +
  syn_res_OA$BornUK * model_violence$coefficients[7] +
  syn_res_OA$Married * model_violence$coefficients[8]

#property crime
eta_theft <- model_theft$coefficients[1] +
  syn_res_OA$Age * model_theft$coefficients[2] +
  syn_res_OA$Male * model_theft$coefficients[3] +
  syn_res_OA$White * model_theft$coefficients[4] +
  syn_res_OA$No_income * model_theft$coefficients[5] +
  syn_res_OA$High_edu * model_theft$coefficients[6] +
  syn_res_OA$BornUK * model_theft$coefficients[7] +
  syn_res_OA$Married * model_theft$coefficients[8]

#damage
eta_damage <- model_damage$coefficients[1] +
  syn_res_OA$Age * model_damage$coefficients[2] +
  syn_res_OA$Male * model_damage$coefficients[3] +
  syn_res_OA$White * model_damage$coefficients[4] +
  syn_res_OA$No_income * model_damage$coefficients[5] +
  syn_res_OA$High_edu * model_damage$coefficients[6] +
  syn_res_OA$BornUK * model_damage$coefficients[7] +
  syn_res_OA$Married * model_damage$coefficients[8]

#save thetas
theta_violence <- model_violence$theta
theta_theft <- model_theft$theta
theta_damage <- model_damage$theta

#remove objects
rm(list=c("model_violence", "model_damage", "model_theft"))

#generate violent crimes based on these estimates for the synthetic individual data
syn_res_OA <-  syn_res_OA %>% 
  mutate(violence = rnbinom(length(eta_violence), mu = exp(eta_violence), size = theta_violence))

#remove objects
rm(list=c("eta_violence", "theta_violence"))

#generate property crimes based on these estimates for the synthetic individual data
syn_res_OA <-  syn_res_OA %>% 
  mutate(theft = rnbinom(length(eta_theft), mu = exp(eta_theft), size = theta_theft))

#remove objects
rm(list=c("eta_theft", "theta_theft"))

#generate damage crimes based on these estimates for the synthetic individual data
syn_res_OA <-  syn_res_OA %>% 
  mutate(damage = rnbinom(length(eta_damage), mu = exp(eta_damage), size = theta_damage))

#remove objects
rm(list=c("eta_damage", "theta_damage"))

#check first rows
head(syn_res_OA, 30)

#split in seven files to allow upload onto Github
syn_res_OA.a <- syn_res_OA %>% filter(ID <= 8010845)
syn_res_OA.b <- syn_res_OA %>% filter(ID >  8010845 & ID <= 16021690)
syn_res_OA.c <- syn_res_OA %>% filter(ID >  16021690 & ID <= 24032535)
syn_res_OA.d <- syn_res_OA %>% filter(ID >  24032535 & ID <= 32043380)
syn_res_OA.e <- syn_res_OA %>% filter(ID >  32043380 & ID <= 40054225)
syn_res_OA.f <- syn_res_OA %>% filter(ID >  40054225 & ID <= 48065070)
syn_res_OA.g <- syn_res_OA %>% filter(ID >  48065070 & ID <= 56075912)

#save synthetic UK population as RData
save(syn_res_OA.a, file = here("data", "synthetic_population_crimes_a.RData"))
save(syn_res_OA.b, file = here("data", "synthetic_population_crimes_b.RData"))
save(syn_res_OA.c, file = here("data", "synthetic_population_crimes_c.RData"))
save(syn_res_OA.d, file = here("data", "synthetic_population_crimes_d.RData"))
save(syn_res_OA.e, file = here("data", "synthetic_population_crimes_e.RData"))
save(syn_res_OA.f, file = here("data", "synthetic_population_crimes_f.RData"))
save(syn_res_OA.g, file = here("data", "synthetic_population_crimes_g.RData"))
