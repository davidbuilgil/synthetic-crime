################################
#
# Simulating crime data
#
# Part VI: Empirical evaluation
#
################################

rm(list=ls())

#load packages
library(dplyr)
library(here)

#increase memory limit a bit
memory.limit()
memory.limit(size=12000)

#load synthetic population of crimes
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

#save sample size
n_syn <- nrow(syn_res_OA)

#save correlation matrix
cor_syn <- cor(syn_res_OA[, 3:9])

#aggregate synthetic population in OAs
syn_res_by_OA <- syn_res_OA %>%
  group_by(OA11CD) %>%
  summarize(Age       = mean(Age),
            Male      = mean(Male),
            BornUK    = mean(BornUK),
            White     = mean(White),
            No_income = mean(No_income),
            Married   = mean(Married),
            High_edu  = mean(High_edu),
            violence  = sum(violence),
            theft     = sum(theft),
            damage    = sum(damage))

#remove objects
#rm(list=c("syn_res_OA"))

#load age data from Census
Age_by_OA <- read.csv(here("data", "Age_by_OA_replicate.csv"))

#load sex data from Census
Sex_by_OA <- read.csv(here("data", "Sex_by_OA_replicate.csv"))

#load ethnicity data from Census
Ethnic_by_OA <- read.csv(here("data", "Ethnic_by_OA_replicate.csv"))

#load income data from Census
Income_by_OA <- read.csv(here("data", "Income_by_OA_replicate.csv"))

#load education data from Census from Census
Edu_by_OA <- read.csv(here("data", "Edu_by_OA_replicate.csv"))

#load marriage data from Census
Married_by_OA <- read.csv(here("data", "Married_by_OA_replicate.csv"))

#load country born data from Census
BornUK_by_OA <- read.csv(here("data", "BornUK_by_OA_replicate.csv"))

#merge all census data
census_by_OA <- Age_by_OA %>%
  dplyr::select(OA11CD, mean_age) %>%
  left_join(Sex_by_OA, by = "OA11CD") %>%
  left_join(Ethnic_by_OA, by = "OA11CD") %>%
  left_join(Income_by_OA, by = "OA11CD") %>%
  left_join(Edu_by_OA, by = "OA11CD") %>%
  left_join(Married_by_OA, by = "OA11CD") %>%
  left_join(BornUK_by_OA, by = "OA11CD") %>%
  dplyr::select(OA11CD, mean_age, Mean_male, Mean_white, mean_no_income, Mean_level4_edu,
                mean_married, mean_bornuk)

#remove objects
rm(list=c("Age_by_OA", "Ethnic_by_OA", "Income_by_OA", "Edu_by_OA", "Married_by_OA",
          "BornUK_by_OA", "Sex_by_OA"))

#merge aggregates of synthetic population and census aggregates
census_by_OA <- census_by_OA %>%
  left_join(syn_res_by_OA, by = "OA11CD")

#remove objects
rm(list=c("syn_res_by_OA"))

# set plotting parameters
par(mfrow=c(3,3), mai = c(0.5, 0.5, 0.5, 0.5))

#correlation between aggregates of synthetic population and census aggregates
plot(census_by_OA$mean_age, census_by_OA$Age, pch = 20,
     ylim = c(0,100), xlim = c(0,100),
     main = "Mean age in OAs",
     xlab = "Census data",
     ylab = "Synthetic data")
round(cor(census_by_OA$mean_age, census_by_OA$Age), 3)
text(80, 20, "cor = 0.98", cex = 0.9)
plot(census_by_OA$Mean_male, census_by_OA$Male, pch = 20,
     ylim = c(0,1), xlim = c(0,1),
     main = "Proportion males in OAs",
     xlab = "Census data",
     ylab = "Synthetic data")
round(cor(census_by_OA$Mean_male, census_by_OA$Male), 3)
text(0.8, 0.2, "cor = 0.78", cex = 0.9)
plot(census_by_OA$Mean_white, census_by_OA$White, pch = 20,
     ylim = c(0,1), xlim = c(0,1),
     main = "Proportion whites in OAs",
     xlab = "Census data",
     ylab = "Synthetic data")
round(cor(census_by_OA$Mean_white, census_by_OA$White), 3)
text(0.8, 0.2, "cor = 0.99", cex = 0.9)
plot(census_by_OA$mean_no_income, census_by_OA$No_income, pch = 20,
     ylim = c(0,1), xlim = c(0,1),
     main = "Proportion without income in OAs",
     xlab = "Census data",
     ylab = "Synthetic data")
round(cor(census_by_OA$mean_no_income, census_by_OA$No_income), 3)
text(0.8, 0.2, "cor = 0.97", cex = 0.9)
plot(census_by_OA$Mean_level4_edu, census_by_OA$High_edu, pch = 20,
     ylim = c(0,1), xlim = c(0,1),
     main = "Proportion high education in OAs",
     xlab = "Census data",
     ylab = "Synthetic data")
round(cor(census_by_OA$Mean_level4_edu, census_by_OA$High_edu), 3)
text(0.8, 0.2, "cor = 0.99", cex = 0.9)
plot(census_by_OA$mean_married, census_by_OA$Married, pch = 20,
     ylim = c(0,1), xlim = c(0,1),
     main = "Proportion married in OAs",
     xlab = "Census data",
     ylab = "Synthetic data")
round(cor(census_by_OA$mean_married, census_by_OA$Married), 3)
text(0.8, 0.2, "cor = 0.98", cex = 0.9)
plot(0, type = 'n', axes = FALSE, ann = FALSE)
plot(census_by_OA$mean_bornuk, census_by_OA$BornUK, pch = 20,
     ylim = c(0,1), xlim = c(0,1),
     main = "Proportion born in UK in OAs",
     xlab = "Census data",
     ylab = "Synthetic data")
round(cor(census_by_OA$mean_bornuk, census_by_OA$BornUK), 3)
text(0.8, 0.2, "cor = 0.99", cex = 0.9)

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

#recode crime types
csew <- csew %>%
  mutate(violence2 = (common_i + wound_i + robber_i) /10000,
         theft2    = (theftp_i + theftf_i + thefto_i + biketh_i + burgla_i) /10000,
         damage2   = (homeva_i + mv.van_i)/10000)

#obtain correlation matrix
csew_matrix <- csew %>%
  dplyr::select(age, sex, cry2, reseth, remploy, marsta, educat2)
cor_csew <- cor(csew_matrix, use = "pairwise.complete.obs")

# set plotting parameters
par(mfrow=c(1,2), mai = c(1, 1, 1, 1))

#plot correlation matrices
library(corrplot)
colnames(cor_syn) <- c("age", "male", "born UK", "white", "no income",
                       "married", "high edu")
rownames(cor_syn) <- c("age", "male", "born UK", "white", "no income",
                       "married", "high edu")
corrplot(cor_syn, method = "circle")
mtext("Correlation matrix in synthetic population", at=2.9, line=-0.2, cex=1.2)
colnames(cor_csew) <- c("age", "male", "born UK", "white", "no income",
                        "married", "high edu")
rownames(cor_csew) <- c("age", "male", "born UK", "white", "no income",
                        "married", "high edu")
corrplot(cor_csew, method="circle")
mtext("Correlation matrix in CSEW", at=3.8, line=-0.2, cex=1.2)

#load synthetic survey data
load(here("synthetic_survey_crimes.RData"))

# set plotting parameters
par(mfrow=c(3,2), mai = c(0.6, 0.5, 0.5, 0.5))

#plot crime distributions in synthetic survey data and CSEW
plot(table(csew$violence2), main = "Violence crime in CSEW", 
     xlab = "Crime victimisations", ylab = "")
plot(table(syn_sample_OA$violence), main = "Violence crime in synthetic survey data", 
     xlab = "Crime victimisations", ylab = "")
plot(table(csew$theft2), main = "Property crime in CSEW", 
     xlab = "Crime victimisations", ylab = "")
plot(table(syn_sample_OA$theft), main = "Property crime in synthetic survey data", 
     xlab = "Crime victimisations", ylab = "",
     xlim = c(0, 13))
plot(table(csew$damage2), main = "Damage crime in CSEW", 
     xlab = "Crime victimisations", ylab = "")
plot(table(syn_sample_OA$damage), main = "Damage crime in synthetic survey data", 
     xlab = "Crime victimisations", ylab = "",
     xlim = c(0, 9))

#crime victimisation by population groups

#sex
syn_sample_OA %>%
  group_by(Male) %>%
  summarise(violence = mean(violence),
            property = mean(theft),
            damage   = mean(damage))
csew %>%
  group_by(sex) %>%
  filter(!is.na(sex)) %>%
  summarise(violence = mean(violence2),
            property = mean(theft2),
            damage   = mean(damage2))

#education level
syn_sample_OA %>%
  group_by(High_edu) %>%
  summarise(violence = mean(violence),
            property = mean(theft),
            damage   = mean(damage))
csew %>%
  filter(!is.na(educat2)) %>%
  group_by(educat2) %>%
  summarise(violence = mean(violence2),
            property = mean(theft2),
            damage   = mean(damage2))

#married
syn_sample_OA %>%
  group_by(Married) %>%
  summarise(violence = mean(violence),
            property = mean(theft),
            damage   = mean(damage))
csew %>%
  filter(!is.na(marsta)) %>%
  group_by(marsta) %>%
  summarise(violence = mean(violence2),
            property = mean(theft2),
            damage   = mean(damage2))

#born UK
syn_sample_OA %>%
  group_by(BornUK) %>%
  summarise(violence = mean(violence),
            property = mean(theft),
            damage   = mean(damage))
csew %>%
  filter(!is.na(cry2)) %>%
  group_by(cry2) %>%
  summarise(violence = mean(violence2),
            property = mean(theft2),
            damage   = mean(damage2))

#ethnic group
syn_sample_OA %>%
  group_by(White) %>%
  summarise(violence = mean(violence),
            property = mean(theft),
            damage   = mean(damage))
csew %>%
  filter(!is.na(reseth)) %>%
  group_by(reseth) %>%
  summarise(violence = mean(violence2),
            property = mean(theft2),
            damage   = mean(damage2))

#no income
syn_sample_OA %>%
  group_by(No_income) %>%
  summarise(violence = mean(violence),
            property = mean(theft),
            damage   = mean(damage))
csew %>%
  filter(!is.na(remploy)) %>%
  group_by(remploy) %>%
  summarise(violence = mean(violence2),
            property = mean(theft2),
            damage   = mean(damage2))

#age
syn_sample_OA %>%
  mutate(age_group = ifelse(Age < 35, 1, NA),
         age_group = ifelse(Age >= 35 & Age < 50, 2, age_group),
         age_group = ifelse(Age >= 50, 3, age_group)) %>%
  group_by(age_group) %>%
  summarise(violence = mean(violence),
            property = mean(theft),
            damage   = mean(damage))
csew %>%
  filter(!is.na(age)) %>%
  mutate(age_group = ifelse(age < 35, 1, NA),
         age_group = ifelse(age >= 35 & age < 50, 2, age_group),
         age_group = ifelse(age >= 50, 3, age_group)) %>%
  group_by(age_group) %>%
  summarise(violence = mean(violence2),
            property = mean(theft2),
            damage   = mean(damage2))

#load OA to LAD lookup
lookup <- read.csv("Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District__December_2017__Lookup_with_Area_Classifications_in_Great_Britain.csv")

#select variables of interest in lookup
lookup <- lookup %>%
  select(?..OA11CD, LSOA11CD, MSOA11CD, LAD17CD) %>%
  rename(OA11CD = ?..OA11CD)

#load LAD to PFA lookup
lookup2 <- read.csv("https://opendata.arcgis.com/datasets/31dec1a0f04a435dadd43de6292ea260_0.csv")

#select variables of interest in lookup
lookup2 <- lookup2 %>%
  select(?..LAD17CD, CSP17CD, CSP17NM, PFA17CD) %>%
  rename(LAD17CD = ?..LAD17CD)

#merge two lookups
lookup <- left_join(lookup, lookup2, by = "LAD17CD")

#remove files and garbage collection to save memory 
rm(list=c("lookup2"))
gc()

#add PFA information to synthetic data
syn_sample_OA <- left_join(syn_sample_OA, lookup, by = "OA11CD")

#open geographies of CSEW
csew_geos <- read.csv("csew_11_geos.csv")

#add PFA information to CSEW
csew <- left_join(csew, csew_geos, by = "rowlabel")

#count crime victimisation by PFAs
syn_PFAs <- syn_sample_OA %>%
  group_by(PFA17CD) %>%
  summarise(violence_syn = sum(violence),
            property_syn = sum(theft),
            damage_syn   = sum(damage))
csew_PFAs <- csew %>%
  mutate(PFA17CD = ifelse(PFA17CD == "E23000034", "E23000001", PFA17CD)) %>% #merge city of London and Met
  group_by(PFA17CD) %>%
  summarise(violence_csew = sum(violence2),
            property_csew = sum(theft2),
            damage_csew   = sum(damage2))

#merge both files at the PFA level
PFAs <- left_join(csew_PFAs, syn_PFAs, by = "PFA17CD")

# set plotting parameters
par(mfrow=c(2,2), mai = c(0.65, 1, 0.5, 0.5))

#correlation between count of crimes in CSEW and synthetic survey data at PFA level
plot(PFAs$violence_csew, PFAs$violence_syn, pch = 20,
     ylim = c(0,153), xlim = c(0,153),
     main = "Violent crime in PFAs",
     xlab = "CSEW",
     ylab = "Synthetic survey data")
abline(lm(PFAs$violence_syn ~ PFAs$violence_csew))
round(cor(PFAs$violence_csew, PFAs$violence_syn), 3)
text(120, 25, "cor = 0.52", cex = 0.9)
plot(PFAs$property_csew, PFAs$property_syn, pch = 20,
     ylim = c(0,650), xlim = c(0,650),
     main = "Property crime in PFAs",
     xlab = "CSEW",
     ylab = "Synthetic survey data")
abline(lm(PFAs$property_syn ~ PFAs$property_csew))
round(cor(PFAs$property_csew, PFAs$property_syn), 3)
text(520, 95, "cor = 0.88", cex = 0.9)
plot(PFAs$damage_csew, PFAs$damage_syn, pch = 20,
     ylim = c(0,305), xlim = c(0,305),
     main = "Damage crime in PFAs",
     xlab = "CSEW",
     ylab = "Synthetic survey data")
abline(lm(PFAs$damage_syn ~ PFAs$damage_csew))
round(cor(PFAs$damage_csew, PFAs$damage_syn), 3)
text(250, 40, "cor = 0.75", cex = 0.9)

#count crime victimisation by CSPs
syn_CSPs <- syn_sample_OA %>%
  group_by(CSP17CD) %>%
  summarise(violence_syn = sum(violence),
            property_syn = sum(theft),
            damage_syn   = sum(damage))
csew_CSPs <- csew %>%
  group_by(CSP17CD) %>%
  summarise(violence_csew = sum(violence2),
            property_csew = sum(theft2),
            damage_csew   = sum(damage2))

#merge both files at the PFA level
CSPs <- left_join(csew_CSPs, syn_CSPs, by = "CSP17CD")

# set plotting parameters
par(mfrow=c(2,2), mai = c(0.65, 1, 0.5, 0.5))

#correlation between count of crimes in CSEW and synthetic survey data at CSP level
plot(CSPs$violence_csew, CSPs$violence_syn, pch = 20,
     ylim = c(0,48), xlim = c(0,48),
     main = "Violent crime in CSPs",
     xlab = "CSEW",
     ylab = "Synthetic survey data")
abline(lm(CSPs$violence_syn ~ CSPs$violence_csew))
round(cor(CSPs$violence_csew, CSPs$violence_syn, use = "complete.obs"), 3)
text(40, 8, "cor = 0.43", cex = 0.9)
plot(CSPs$property_csew, CSPs$property_syn, pch = 20,
     ylim = c(0,84), xlim = c(0,84),
     main = "Property crime in CSPs",
     xlab = "CSEW",
     ylab = "Synthetic survey data")
abline(lm(CSPs$property_syn ~ CSPs$property_csew))
round(cor(CSPs$property_csew, CSPs$property_syn, use = "complete.obs"), 3)
text(70, 13, "cor = 0.58", cex = 0.9)
plot(CSPs$damage_csew, CSPs$damage_syn, pch = 20,
     ylim = c(0,95), xlim = c(0,95),
     main = "Damage crime in CSPs",
     xlab = "CSEW",
     ylab = "Synthetic survey data")
abline(lm(CSPs$damage_syn ~ CSPs$damage_csew))
round(cor(CSPs$damage_csew, CSPs$damage_syn, use = "complete.obs"), 3)
text(75, 14, "cor = 0.59", cex = 0.9)

#remove files to save memory
rm(list=c("CSPs", "PFAs", "csew_matrix", "cor_csew", "cor_syn", "census_by_OA", "csew_CSPs",
          "csew_geos", "csew_PFAs", "syn_PFAs", "syn_CSPs", "n_syn", "syn_sample_OA"))

#load synthetic police data
load(here("synthetic_police_crimes.Rdata"))

#load police data
police_data <- read.csv("Crime_by_lsoa_2013.csv")
police_data <- police_data %>%
  dplyr::select(LSOA.code, msoa, CSP17CD, violence.combined, property.combined, damage.combined) %>%
  rename(LSOA11CD = LSOA.code,
         MSOA11CD = msoa)

#add CSP information to synthetic data
Data_crimes <- left_join(Data_crimes, lookup, by = "OA11CD")

#count synthetic crime data in MSOAs
syn_data_MSOA <- Data_crimes %>%
  group_by(MSOA11CD) %>%
  summarise(violence_syn = sum(violence),
            property_syn = sum(theft),
            damage_syn   = sum(damage)) 

#count police recorded crimes in MSOAs
police_data_MSOA <- police_data %>%
  group_by(MSOA11CD) %>%
  summarise(violence_poli = sum(violence.combined),
            property_poli = sum(property.combined),
            damage_poli   = sum(damage.combined)) 

#merge both files
data_MSOA <- left_join(police_data_MSOA, syn_data_MSOA, by = "MSOA11CD")

# set plotting parameters
par(mfrow=c(2,2), mai = c(0.65, 1, 0.5, 0.5))

#correlation between count of crimes in police data and synthetic police data at CSP level
plot(data_MSOA$violence_poli, data_MSOA$violence_syn, pch = 20,
     ylim = c(0,1500), xlim = c(0,1500),
     main = "Violent crime in MSOAs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_MSOA$violence_syn ~ data_MSOA$violence_poli))
round(cor(data_MSOA$violence_poli, data_MSOA$violence_syn, use = "complete.obs"), 3)
text(1200, 1300, "cor = 0.40", cex = 0.9)
plot(data_MSOA$property_poli, data_MSOA$property_syn, pch = 20,
     ylim = c(0,2700), xlim = c(0,2700),
     main = "Property crime in MSOAs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_MSOA$property_syn ~ data_MSOA$property_poli))
round(cor(data_MSOA$property_poli, data_MSOA$property_syn, use = "complete.obs"), 3)
text(2100, 2300, "cor = 0.48", cex = 0.9)
plot(data_MSOA$damage_poli, data_MSOA$damage_syn, pch = 20,
     ylim = c(0,600), xlim = c(0,600),
     main = "Damage crime in MSOAs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_MSOA$damage_syn ~ data_MSOA$damage_poli))
round(cor(data_MSOA$damage_poli, data_MSOA$damage_syn, use = "complete.obs"), 3)
text(480, 500, "cor = 0.23", cex = 0.9)

#load police data
police_data <- read.csv("prc-csp-1112-1415-tables.csv")

#recode some PFA to CSP17NM
#Adjustments to match CSP labels from 2017/18
police_data$CSP.Name[police_data$CSP.Name=="Northern Devon" ] <- "North Devon"
police_data$CSP.Name[police_data$CSP.Name=="Somerset East_Mendip" ] <- "Somerset"
police_data$CSP.Name[police_data$CSP.Name=="Somerset East_South Somerset" ] <- "Somerset"
police_data$CSP.Name[police_data$CSP.Name=="Sedgemoor" ] <- "Somerset"
police_data$CSP.Name[police_data$CSP.Name=="Taunton Deane" ] <- "Somerset"
police_data$CSP.Name[police_data$CSP.Name=="West Somerset" ] <- "Somerset"
police_data$CSP.Name[police_data$CSP.Name=="Rhondda Cynon Taf" ] <- "Cwm Taf"
police_data$CSP.Name[police_data$CSP.Name=="Merthyr Tydfil" ] <- "Cwm Taf"
police_data$CSP.Name[police_data$CSP.Name=="Basingstoke and Deane" ] <- "North Hampshire"
police_data$CSP.Name[police_data$CSP.Name=="Hart" ] <- "North Hampshire"
police_data$CSP.Name[police_data$CSP.Name=="Rushmoor" ] <- "North Hampshire"
police_data$CSP.Name[police_data$CSP.Name=="Bromsgrove" ] <- "North Worcestershire"
police_data$CSP.Name[police_data$CSP.Name=="Redditch" ] <- "North Worcestershire"
police_data$CSP.Name[police_data$CSP.Name=="Wyre Forest" ] <- "North Worcestershire"

#count crimes in CSP
police_data <- police_data %>%
  mutate(Number.of.Offences = ifelse(Number.of.Offences < 0, 0, Number.of.Offences)) %>%
  group_by(CSP.Name) %>%
  summarise(violence_poli = sum(Number.of.Offences[Offence.Group == "Sexual offences" |
                                                     Offence.Group == "Violence against the person" |
                                                     Offence.Group == "Robbery"]),
            damage_poli = sum(Number.of.Offences[Offence.Group == "Criminal damage and arson"]),
            property_poli = sum(Number.of.Offences[Offence.Group == "Theft offences"]))

#count synthetic crime data in CSPs
syn_data_CSP <- Data_crimes %>%
  group_by(CSP17NM) %>%
  summarise(violence_syn = sum(violence),
            property_syn = sum(theft),
            damage_syn   = sum(damage)) 

#merge both files
data_CSP <- left_join(syn_data_CSP, police_data, by = c("CSP17NM" = "CSP.Name"))

# set plotting parameters
par(mfrow=c(2,2), mai = c(0.65, 1, 0.5, 0.5))

#correlation between count of crimes in police data and synthetic police data at CSP level
plot(data_CSP$violence_poli, data_CSP$violence_syn, pch = 20,
     ylim = c(0,26000), xlim = c(0,26000),
     main = "Violent crime in CSPs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_CSP$violence_syn ~ data_CSP$violence_poli))
round(cor(data_CSP$violence_poli, data_CSP$violence_syn, use = "complete.obs"), 3)
text(20000, 4000, "cor = 0.83", cex = 0.9)
plot(data_CSP$property_poli, data_CSP$property_syn, pch = 20,
     ylim = c(0,55000), xlim = c(0,55000),
     main = "Property crime in CSPs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_CSP$property_syn ~ data_CSP$property_poli))
round(cor(data_CSP$property_poli, data_CSP$property_syn, use = "complete.obs"), 3)
text(45000, 8000, "cor = 0.85", cex = 0.9)
plot(data_CSP$damage_poli, data_CSP$damage_syn, pch = 20,
     ylim = c(0,28000), xlim = c(0,28000),
     main = "Damage crime in CSPs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_CSP$damage_syn ~ data_CSP$damage_poli))
round(cor(data_CSP$damage_poli, data_CSP$damage_syn, use = "complete.obs"), 3)
text(20000, 4000, "cor = 0.80", cex = 0.9)

#add PFA in police data
lookup_CSP <- lookup %>%
  select(CSP17NM, PFA17CD) %>%
  unique()
data_PFA <- left_join(data_CSP, lookup_CSP, by = "CSP17NM")

#count crimes in PFAs
data_PFA <- data_PFA %>%
  group_by(PFA17CD) %>%
  summarise(violence_syn = sum(violence_syn ),
            violence_poli = sum(violence_poli),
            property_syn = sum(property_syn),
            property_poli = sum(property_poli),
            damage_syn = sum(damage_syn),
            damage_poli = sum(damage_poli))

# set plotting parameters and avoid scientific notation
par(mfrow=c(2,2), mai = c(0.65, 1, 0.5, 0.5))
options(scipen=999)

#correlation between count of crimes in police data and synthetic police data at PFA level
plot(data_PFA$violence_poli, data_PFA$violence_syn, pch = 20,
     ylim = c(0,220000), xlim = c(0,220000),
     main = "Violent crime in PFAs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_PFA$violence_syn ~ data_PFA$violence_poli))
round(cor(data_PFA$violence_poli, data_PFA$violence_syn, use = "complete.obs"), 3)
text(150000, 40000, "cor = 0.96", cex = 0.9)
plot(data_PFA$property_poli, data_PFA$property_syn, pch = 20,
     ylim = c(0,500000), xlim = c(0,500000),
     main = "Property crime in PFAs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_PFA$property_syn ~ data_PFA$property_poli))
round(cor(data_PFA$property_poli, data_PFA$property_syn, use = "complete.obs"), 3)
text(350000, 80000, "cor = 0.99", cex = 0.9)
plot(data_PFA$damage_poli, data_PFA$damage_syn, pch = 20,
     ylim = c(0,300000), xlim = c(0,300000),
     main = "Damage crime in PFAs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_PFA$damage_syn ~ data_PFA$damage_poli))
round(cor(data_PFA$damage_poli, data_PFA$damage_syn, use = "complete.obs"), 3)
text(200000, 50000, "cor = 0.91", cex = 0.9)

#plot only property crimes
# set plotting parameters and avoid scientific notation
par(mfrow=c(2,2), mai = c(0.65, 1, 0.5, 0.5))
options(scipen=999)

#correlation between count of crimes in police data and synthetic police data for property crimes
plot(data_MSOA$property_poli, data_MSOA$property_syn, pch = 20,
     ylim = c(0,2700), xlim = c(0,2700),
     main = "Property crime in MSOAs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_MSOA$property_syn ~ data_MSOA$property_poli))
round(cor(data_MSOA$property_poli, data_MSOA$property_syn, use = "complete.obs"), 3)
text(2100, 2300, "cor = 0.48", cex = 0.9)
plot(data_CSP$property_poli, data_CSP$property_syn, pch = 20,
     ylim = c(0,55000), xlim = c(0,55000),
     main = "Property crime in CSPs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_CSP$property_syn ~ data_CSP$property_poli))
round(cor(data_CSP$property_poli, data_CSP$property_syn, use = "complete.obs"), 3)
text(45000, 8000, "cor = 0.85", cex = 0.9)
plot(data_PFA$property_poli, data_PFA$property_syn, pch = 20,
     ylim = c(0,500000), xlim = c(0,500000),
     main = "Property crime in PFAs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_PFA$property_syn ~ data_PFA$property_poli))
round(cor(data_PFA$property_poli, data_PFA$property_syn, use = "complete.obs"), 3)
text(350000, 80000, "cor = 0.99", cex = 0.9)

#plot only violent crimes
# set plotting parameters and avoid scientific notation
par(mfrow=c(2,2), mai = c(0.65, 1, 0.5, 0.5))
options(scipen=999)

#correlation between count of crimes in police data and synthetic police data for violent crimes
plot(data_MSOA$violence_poli, data_MSOA$violence_syn, pch = 20,
     ylim = c(0,1500), xlim = c(0,1500),
     main = "Violent crime in MSOAs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_MSOA$violence_syn ~ data_MSOA$violence_poli))
round(cor(data_MSOA$violence_poli, data_MSOA$violence_syn, use = "complete.obs"), 3)
text(1200, 1300, "cor = 0.40", cex = 0.9)
plot(data_CSP$violence_poli, data_CSP$violence_syn, pch = 20,
     ylim = c(0,26000), xlim = c(0,26000),
     main = "Violent crime in CSPs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_CSP$violence_syn ~ data_CSP$violence_poli))
round(cor(data_CSP$violence_poli, data_CSP$violence_syn, use = "complete.obs"), 3)
text(20000, 4000, "cor = 0.83", cex = 0.9)
plot(data_PFA$violence_poli, data_PFA$violence_syn, pch = 20,
     ylim = c(0,220000), xlim = c(0,220000),
     main = "Violent crime in PFAs",
     xlab = "Police data",
     ylab = "Synthetic police data")
abline(lm(data_PFA$violence_syn ~ data_PFA$violence_poli))
round(cor(data_PFA$violence_poli, data_PFA$violence_syn, use = "complete.obs"), 3)
text(150000, 40000, "cor = 0.96", cex = 0.9)
