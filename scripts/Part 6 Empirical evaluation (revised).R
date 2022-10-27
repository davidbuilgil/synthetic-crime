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
library(ggplot2)
library(ggpubr)

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

#load synthetic household population of crimes
load(here("data", "HHsynthetic_population_crimes.Rdata"))

#aggregate synthetic household population in OAs
HHsyn_res_by_OA <- syn_res_OA %>%
  group_by(OA11CD) %>%
  summarize(AgeHH       = mean(age_more65),
            Terraced    = mean(terraced),
            WhiteHH     = mean(hrp_white),
            Oneperson   = mean(one_person),
            No_incomeHH = mean(no_income),
            No_car      = mean(no_car),
            Social_rent = mean(social_rent),
            No_religion = mean(hrp_no_religion),
            theftHH     = sum(theft),
            damageHH    = sum(damage))

#load household variables from Census
AgeHH_by_OA <- read.csv(here("data", "Agehh_by_OA_replicate.csv"))
Accommodation_by_OA <- read.csv(here("data", "Accommodation_by_OA_replicate.csv"))
EthnicHH_by_OA <- read.csv(here("data", "Ethnichh_by_OA_replicate.csv"))
HHsize_by_OA <- read.csv(here("data", "HHsize_by_OA_replicate.csv"))
Economic_by_OA <- read.csv(here("data", "Economic_by_OA_replicate.csv"))
Car_by_OA <- read.csv(here("data", "Car_by_OA_replicate.csv"))
Tenure_by_OA <- read.csv(here("data", "Tenure_by_OA_replicate.csv"))
Religion_by_OA <- read.csv(here("data", "Religion_by_OA_replicate.csv"))

#merge all census household data
censusHH_by_OA <- AgeHH_by_OA %>%
  dplyr::select(OA11CD, mean_hrp_over65) %>%
  left_join(Accommodation_by_OA, by = "OA11CD") %>%
  left_join(EthnicHH_by_OA, by = "OA11CD") %>%
  left_join(HHsize_by_OA, by = "OA11CD") %>%
  left_join(Economic_by_OA, by = "OA11CD") %>%
  left_join(Car_by_OA, by = "OA11CD") %>%
  left_join(Tenure_by_OA, by = "OA11CD") %>%
  left_join(Religion_by_OA, by = "OA11CD") %>%
  dplyr::select(OA11CD, mean_hrp_over65, mean_terraced, mean_white,
                mean_one_person, mean_no_income, mean_no_car, mean_social_rent,
                mean_no_religion)
#remove objects
rm(list=c("AgeHH_by_OA", "Accommodation_by_OA", "EthnicHH_by_OA",
          "HHsize_by_OA", "Economic_by_OA", "Car_by_OA", "Tenure_by_OA",
          "Religion_by_OA"))

#merge aggregates of synthetic population and census aggregates
census_by_OA <- census_by_OA %>%
  left_join(censusHH_by_OA, by = "OA11CD") %>%
  left_join(HHsyn_res_by_OA, by = "OA11CD")

#remove objects
rm(list=c("syn_res_by_OA", "HHsyn_res_by_OA", "censusHH_by_OA"))

#correlation between aggregates of synthetic population and census aggregates
age_plot <- ggplot(census_by_OA, aes(x = mean_age, y = Age)) +
  geom_point() +
  xlim(0, 100) + ylim(0, 100) +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue", fullrange = "T") +
  ggtitle("Mean age (Ind.)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 80, y = 20, label = "cor = 0.96")
males_plot <- ggplot(census_by_OA, aes(x = Mean_male, y = Male)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue", fullrange = "T") +
  ggtitle("Proportion males (Ind.)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.78")
whites_plot <- ggplot(census_by_OA, aes(x = Mean_white, y = White)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue", fullrange = "T") +
  ggtitle("Proportion whites (Ind.)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.99")
income_plot <- ggplot(census_by_OA, aes(x = mean_no_income.x, y = No_income)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue", fullrange = "T") +
  ggtitle("Proportion no income (Ind.)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.97")
education_plot <- ggplot(census_by_OA, aes(x = Mean_level4_edu, y = High_edu)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue", fullrange = "T") +
  ggtitle("Proportion high edu. (Ind.)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.99")
married_plot <- ggplot(census_by_OA, aes(x = mean_married, y = Married)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue", fullrange = "T") +
  ggtitle("Proportion married (Ind.)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.98")
bornUK_plot <- ggplot(census_by_OA, aes(x = mean_bornuk, y = BornUK)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue", fullrange = "T") +
  ggtitle("Proportion born UK (Ind.)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.99")

ageHH_plot <- ggplot(census_by_OA, aes(x = mean_hrp_over65, y = AgeHH)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Proportion over 65s (HH)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.96")
terraced_plot <- ggplot(census_by_OA, aes(x = mean_terraced, y = Terraced)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Proportion terraced (HH)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.99")
whiteHH_plot <- ggplot(census_by_OA, aes(x = mean_white, y = WhiteHH)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Proportion white HRP (HH)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.99")
oneperson_plot <- ggplot(census_by_OA, aes(x = mean_one_person, y = Oneperson)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Proportion one person (HH)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.95")
noincomeHH_plot <- ggplot(census_by_OA, aes(x = mean_no_income.y, y = No_incomeHH)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Proportion no income (HH)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.95")
nocar_plot <- ggplot(census_by_OA, aes(x = mean_no_car, y = No_car)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Proportion no car (HH)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.98")
socialrent_plot <- ggplot(census_by_OA, aes(x = mean_social_rent, y = Social_rent)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Proportion social renter (HH)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.99")
noreligion_plot <- ggplot(census_by_OA, aes(x = mean_no_religion, y = No_religion)) +
  geom_point() +
  xlim(0, 1) + ylim(0, 1) +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Proportion no religion (HH)") +
  labs(x = "Census", y = "Synthetic") + 
  annotate(geom = "text", x = 0.8, y = 0.2, label = "cor = 0.92")

#print scatter plots
ggarrange(age_plot, males_plot, whites_plot, income_plot, education_plot,
          married_plot, bornUK_plot, 
          ageHH_plot, terraced_plot, whiteHH_plot, oneperson_plot, 
          noincomeHH_plot, nocar_plot, socialrent_plot, noreligion_plot,
          nrow = 5, ncol = 3)

ggsave(here("plots/census_synth_plot.png"),
       width = 22, height = 35, units = "cm")

#remove objects
rm(list=c("age_plot", "males_plot", "whites_plot", "income_plot", "education_plot",
          "married_plot", "bornUK_plot", "ageHH_plot", "terraced_plot", 
          "whiteHH_plot", "oneperson_plot", 
          "noincomeHH_plot", "nocar_plot", "socialrent_plot", "noreligion_plot"))

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
         theft2    = (theftf_i + thefto_i + biketh_i + burgla_i) /10000,
         damage2   = (homeva_i + mv.van_i)/10000)

#obtain correlation matrix
csew_matrix <- csew %>%
  dplyr::select(age, sex, cry2, reseth, remploy, marsta, educat2)
cor_csew <- cor(csew_matrix, use = "pairwise.complete.obs")

# set plotting parameters
#par(mfrow=c(1,2), mai = c(1, 1, 1, 1))

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
load(here("data", "HHsynthetic_survey_crimes.RData"))
HHsyn_sample_OA <- syn_sample_OA
load(here("data", "synthetic_survey_crimes.RData"))

# set plotting parameters
#par(mfrow=c(3,2), mai = c(0.6, 0.5, 0.5, 0.5))

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
lookup <- read.csv(here("data", "Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District__December_2017__Lookup_with_Area_Classifications_in_Great_Britain.csv"))

#select variables of interest in lookup
lookup <- lookup %>%
  select(OA11CD, LSOA11CD, MSOA11CD, LAD17CD)

#load LAD to PFA lookup
lookup2 <- read.csv(here("data", "Local_Authority_District_to_Community_Safety_Partnerships_to_Police_Force_Areas__January_2017__Lookup_in_England_and_Wales_Version_2.csv"))

#select variables of interest in lookup
lookup2 <- lookup2 %>%
  select(LAD17CD, CSP17CD, CSP17NM, PFA17CD)

#merge two lookups
lookup <- left_join(lookup, lookup2, by = "LAD17CD")

#remove objects
rm(list=c("lookup2"))

#add PFA information to synthetic data
syn_sample_OA <- left_join(syn_sample_OA, lookup, by = "OA11CD")
HHsyn_sample_OA <- left_join(HHsyn_sample_OA, lookup, by = "OA11CD")

#remove objects
rm(list=c("csew_matrix", "cor_csew", "cor_syn", "census_by_OA", "n_syn"))

#load synthetic police data
load(here("data", "HHsynthetic_police_crimes.Rdata"))
HHData_crimes <- Data_crimes
load(here("data", "synthetic_police_crimes.Rdata"))

#load police data
police_data <- read.csv(here("data", "Crime_by_lsoa_2013.csv"))
police_data <- police_data %>%
  dplyr::select(LSOA.code, msoa, CSP17CD, violence.combined, property.combined.nopertheft, damage.combined) %>%
  rename(LSOA11CD = LSOA.code,
         MSOA11CD = msoa)

#add CSP information to synthetic data
Data_crimes <- left_join(Data_crimes, lookup, by = "OA11CD")
HHData_crimes <- left_join(HHData_crimes, lookup, by = "OA11CD")

#count synthetic crime data in MSOAs
syn_data_MSOA <- Data_crimes %>%
  group_by(MSOA11CD) %>%
  summarise(violence_syn = sum(violence),
            property_syn = sum(theft),
            damage_syn   = sum(damage)) 
HHsyn_data_MSOA <- HHData_crimes %>%
  group_by(MSOA11CD) %>%
  summarise(HHproperty_syn = sum(theft),
            HHdamage_syn   = sum(damage))

#count police recorded crimes in MSOAs
police_data_MSOA <- police_data %>%
  group_by(MSOA11CD) %>%
  summarise(violence_poli = sum(violence.combined),
            property_poli = sum(property.combined.nopertheft),
            damage_poli   = sum(damage.combined)) 

#merge both files
data_MSOA <- police_data_MSOA %>%
  left_join(syn_data_MSOA, by = "MSOA11CD") %>%
  left_join(HHsyn_data_MSOA, by = "MSOA11CD")

#load police data
police_data <- read.csv(here("data", "prc-csp-1112-1415-tables.csv"))

#recode some PFA to CSP17NM
#Adjustments to match CSP labels from 2017
police_data <- police_data %>%
  mutate(CSP.Name = ifelse(CSP.Name == "Northern Devon", "North Devon", CSP.Name),
         CSP.Name = ifelse(CSP.Name == "Somerset East_Mendip" |
                           CSP.Name == "Somerset East_South Somerset" |
                           CSP.Name == "Sedgemoor" |
                           CSP.Name == "Taunton Deane" |
                           CSP.Name == "West Somerset", "Somerset", CSP.Name),
         CSP.Name = ifelse(CSP.Name == "Rhondda Cynon Taf" |
                           CSP.Name == "Merthyr Tydfil", "Cwm Taf", CSP.Name),
         CSP.Name = ifelse(CSP.Name == "Basingstoke and Deane" |
                           CSP.Name == "Hart" |
                           CSP.Name == "Rushmoor", "North Hampshire", CSP.Name),
         CSP.Name = ifelse(CSP.Name == "Bromsgrove" |
                           CSP.Name == "Redditch" |
                           CSP.Name == "Wyre Forest", "North Worcestershire", CSP.Name))

#count crimes in CSP
police_data <- police_data %>%
  mutate(Number.of.Offences = ifelse(Number.of.Offences < 0, 0, Number.of.Offences)) %>%
  group_by(CSP.Name) %>%
  filter(Offence.Subgroup != "Theft from the person" & Offence.Subgroup != "Other theft offences" &
           Offence.Subgroup != "Fraud offences" & Offence.Subgroup != "Shoplifting") %>%
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
HHsyn_data_CSP <- HHData_crimes %>%
  group_by(CSP17NM) %>%
  summarise(HHproperty_syn = sum(theft),
            HHdamage_syn   = sum(damage))

#merge both files
data_CSP <- syn_data_CSP %>%
  left_join(police_data, by = c("CSP17NM" = "CSP.Name")) %>%
  left_join(HHsyn_data_CSP, by = "CSP17NM")

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
            HHproperty_syn = sum(HHproperty_syn),
            property_poli = sum(property_poli),
            HHdamage_syn = sum(HHdamage_syn),
            damage_poli = sum(damage_poli))

#avoid scientific notation
options(scipen=999)

#correlation between synthetic police data and police data
vio_PFA_plot <- ggplot(data_PFA, aes(x = violence_poli, y = violence_syn)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue", fullrange = "T") +
  ggtitle("Violence (PFAs)") +
  labs(x = "Police", y = "Synthetic") + 
  annotate(geom = "text", x = 120000, y = 40000, label = "cor = 0.96")
vio_CSP_plot <- ggplot(data_CSP, aes(x = violence_poli, y = violence_syn)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue", fullrange = "T") +
  ggtitle("Violence (CSPs)") +
  labs(x = "Police", y = "Synthetic") + 
  annotate(geom = "text", x = 15000, y = 4000, label = "cor = 0.83")
vio_MSOA_plot <- ggplot(data_MSOA, aes(x = violence_poli, y = violence_syn)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", color = "blue", fullrange = "T") +
  ggtitle("Violence (MSOAs)") +
  labs(x = "Police", y = "Synthetic") + 
  annotate(geom = "text", x = 900, y = 750, label = "cor = 0.40")
prop_PFA_plot <- ggplot(data_PFA, aes(x = property_poli, y = HHproperty_syn)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Property crime (PFAs)") +
  labs(x = "Police", y = "Synthetic") + 
  annotate(geom = "text", x = 100000, y = 135000, label = "cor = 0.98")
prop_CSP_plot <- ggplot(data_CSP, aes(x = property_poli, y = HHproperty_syn)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Property crime (CSPs)") +
  labs(x = "Police", y = "Synthetic") + 
  annotate(geom = "text", x = 10000, y = 13000, label = "cor = 0.88")
prop_MSOA_plot <- ggplot(data_MSOA, aes(x = property_poli, y = HHproperty_syn)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Property crime (MSOAs)") +
  labs(x = "Police", y = "Synthetic") + 
  annotate(geom = "text", x = 550, y = 350, label = "cor = 0.55")
dam_PFA_plot <- ggplot(data_PFA, aes(x = damage_poli, y = HHdamage_syn)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Damage (PFAs)") +
  labs(x = "Police", y = "Synthetic") + 
  annotate(geom = "text", x = 30000, y = 80000, label = "cor = 0.91")
dam_CSP_plot <- ggplot(data_CSP, aes(x = damage_poli, y = HHdamage_syn)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Damage (CSPs)") +
  labs(x = "Police", y = "Synthetic") + 
  annotate(geom = "text", x = 5000, y = 7800, label = "cor = 0.82")
dam_MSOA_plot <- ggplot(data_MSOA, aes(x = property_poli, y = HHproperty_syn)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", color = "red", fullrange = "T") +
  ggtitle("Damage (MSOAs)") +
  labs(x = "Police", y = "Synthetic") + 
  annotate(geom = "text", x = 550, y = 350, label = "cor = 0.29")

#print scatter plots
ggarrange(vio_PFA_plot, vio_CSP_plot, vio_MSOA_plot, 
          prop_PFA_plot, prop_CSP_plot, prop_MSOA_plot, 
          dam_PFA_plot, dam_CSP_plot, dam_MSOA_plot, 
          nrow = 3, ncol = 3)

ggsave(here("plots/police_synth_plot.png"),
       width = 20, height = 15, units = "cm")
