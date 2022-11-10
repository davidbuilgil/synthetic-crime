################################
#
# Simulating crime data
#
# Part XX: Explore measurement error
#
################################

rm(list=ls())

#load packages
library(dplyr)
library(here)
library(ggplot2)
library(ggpubr)

#load synthetic population of crimes
load(here("data", "HHsynthetic_population_crimes.Rdata"))
HHsyn_res_OA <- syn_res_OA
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

#add spatial information to synthetic data
syn_res_OA <- left_join(syn_res_OA, lookup, by = "OA11CD")
HHsyn_res_OA <- left_join(HHsyn_res_OA, lookup, by = "OA11CD")

#load synthetic police data
load(here("data", "HHsynthetic_police_crimes.Rdata"))
HHData_crimes <- Data_crimes
load(here("data", "synthetic_police_crimes.Rdata"))

#add spatial information to synthetic data
Data_crimes <- left_join(Data_crimes, lookup, by = "OA11CD")
HHData_crimes <- left_join(HHData_crimes, lookup, by = "OA11CD")

#count synthetic crime data in CSPs
syn_data_CSP <- syn_res_OA %>%
  group_by(CSP17NM) %>%
  summarise(violence_syn = sum(violence),
            property_syn = sum(theft),
            damage_syn   = sum(damage),
            pop = n()) 
HHsyn_data_CSP <- HHsyn_res_OA %>%
  group_by(CSP17NM) %>%
  summarise(HHproperty_syn = sum(theft),
            HHdamage_syn   = sum(damage),
            HHpop = n()) 
syn_police_CSP <- Data_crimes %>%
  group_by(CSP17NM) %>%
  summarise(violence_syn_pol = sum(violence),
            property_syn_pol = sum(theft),
            damage_syn_pol   = sum(damage)) 
HHsyn_police_CSP <- HHData_crimes %>%
  group_by(CSP17NM) %>%
  summarise(HHproperty_syn_pol = sum(theft),
            HHdamage_syn_pol   = sum(damage))

#merge both files
data_CSP <- syn_data_CSP %>% 
  left_join(syn_police_CSP, by = "CSP17NM") %>%
  left_join(HHsyn_data_CSP, by = "CSP17NM") %>%
  left_join(HHsyn_police_CSP, by = "CSP17NM")

#count synthetic crime data in PFAs
syn_data_PFA <- syn_res_OA %>%
  group_by(PFA17CD) %>%
  summarise(violence_syn = sum(violence),
            property_syn = sum(theft),
            damage_syn   = sum(damage),
            pop = n()) 
HHsyn_data_PFA <- HHsyn_res_OA %>%
  group_by(PFA17CD) %>%
  summarise(HHproperty_syn = sum(theft),
            HHdamage_syn   = sum(damage),
            HHpop = n()) 
syn_police_PFA <- Data_crimes %>%
  group_by(PFA17CD) %>%
  summarise(violence_syn_pol = sum(violence),
            property_syn_pol = sum(theft),
            damage_syn_pol   = sum(damage)) 
HHsyn_police_PFA <- HHData_crimes %>%
  group_by(PFA17CD) %>%
  summarise(HHproperty_syn_pol = sum(theft),
            HHdamage_syn_pol   = sum(damage)) 

#merge both files
data_PFA <- syn_data_PFA %>%
  left_join(syn_police_PFA, by = "PFA17CD") %>%
  left_join(HHsyn_data_PFA, by = "PFA17CD") %>%
  left_join(HHsyn_police_PFA, by = "PFA17CD")

#calculate measurement error (assuming multiplicative error)
data_CSP <- data_CSP %>%
  mutate(all_syn_pol = (violence_syn_pol + HHproperty_syn_pol + HHdamage_syn_pol) / pop,
         all_syn = (violence_syn + HHproperty_syn + HHdamage_syn) / pop,
         violence_syn_pol = violence_syn_pol / pop,
         violence_syn = violence_syn / pop ,
         property_syn_pol = HHproperty_syn_pol / HHpop ,
         property_syn = HHproperty_syn / HHpop ,
         damage_syn_pol = HHdamage_syn_pol / HHpop ,
         damage_syn = HHdamage_syn / HHpop ,
         all_ME = all_syn_pol / all_syn,
         violence_ME = violence_syn_pol / violence_syn,
         property_ME = property_syn_pol / property_syn,
         damage_ME = damage_syn_pol / damage_syn)
data_PFA <- data_PFA %>%
  mutate(all_syn_pol = (violence_syn_pol + HHproperty_syn_pol + HHdamage_syn_pol) / pop,
         all_syn = (violence_syn + HHproperty_syn + HHdamage_syn) / pop,
         violence_syn_pol = violence_syn_pol / pop,
         violence_syn = violence_syn / pop ,
         property_syn_pol = HHproperty_syn_pol / HHpop ,
         property_syn = HHproperty_syn / HHpop ,
         damage_syn_pol = HHdamage_syn_pol / HHpop ,
         damage_syn = HHdamage_syn / HHpop ,
         all_ME = all_syn_pol / all_syn,
         violence_ME = violence_syn_pol / violence_syn,
         property_ME = property_syn_pol / property_syn,
         damage_ME = damage_syn_pol / damage_syn)

#select only variables of interest
data_for_boxplot.4 <- data_CSP %>%
  mutate('Crime type' = "Violence",
         'Geography' = "CSP",
         'ME' = violence_ME) %>%
  select('Crime type', Geography, ME)
data_for_boxplot.5 <- data_CSP %>%
  mutate('Crime type' = "Property crime",
         'Geography' = "CSP",
         'ME' = property_ME) %>%
  select('Crime type', Geography, ME)
data_for_boxplot.6 <- data_CSP %>%
  mutate('Crime type' = "Damage",
         'Geography' = "CSP",
         'ME' = damage_ME) %>%
  select('Crime type', Geography, ME)
data_for_boxplot.7 <- data_PFA %>%
  mutate('Crime type' = "Violence",
         'Geography' = "PFA",
         'ME' = violence_ME) %>%
  select('Crime type', Geography, ME)
data_for_boxplot.8 <- data_PFA %>%
  mutate('Crime type' = "Property crime",
         'Geography' = "PFA",
         'ME' = property_ME) %>%
  select('Crime type', Geography, ME)
data_for_boxplot.9 <- data_PFA %>%
  mutate('Crime type' = "Damage",
         'Geography' = "PFA",
         'ME' = damage_ME) %>%
  select('Crime type', Geography, ME)

data_for_boxplot.15 <- data_CSP %>%
  mutate('Crime type' = "All",
         'Geography' = "CSP",
         'ME' = all_ME) %>%
  select('Crime type', Geography, ME)
data_for_boxplot.16 <- data_PFA %>%
  mutate('Crime type' = "All",
         'Geography' = "PFA",
         'ME' = all_ME) %>%
  select('Crime type', Geography, ME)

#create dataset for visualisation
data_for_boxplot <- rbind(data_for_boxplot.4,
                          data_for_boxplot.5, data_for_boxplot.6,
                          data_for_boxplot.7, data_for_boxplot.8,
                          data_for_boxplot.9, 
                          data_for_boxplot.15, data_for_boxplot.16)
data_for_boxplot <- data_for_boxplot %>%
  mutate(Geography = factor(Geography, levels = c("CSP", "PFA")))

#grouped boxplot
ggplot(data_for_boxplot, aes(x = `Crime type`, y = ME, fill = Geography)) + 
  geom_boxplot() +
  ggtitle("Boxplots of recording rates in police crime records (simulation)") +
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               linetype = "dashed", color = "red",
               position = position_dodge2(width = 0.75,   
                                          preserve = "single")) +
  theme_bw()

ggsave(here("plots/boxplots_ME.png"),
       width = 20, height = 10, units = "cm")

#print average and sd by groups
data_for_boxplot %>%
  group_by(`Crime type`, Geography) %>%
  summarize(Mean = mean(ME, na.rm = TRUE),
            SD = sd(ME, na.rm = TRUE))

#save data aggregates
#write.csv(data_CSP, here("data/synthetic_CSP.csv"))
#write.csv(data_PFA, here("data/synthetic_PFA.csv"))
#synthetic_CSP <- read.csv(here("data", "synthetic_CSP.csv"))
#synthetic_PFA <- read.csv(here("data", "synthetic_PFA.csv"))

#load CSEW estimates for crim variables at CSP and PFA levels
latent_scores_LAD <- read.csv("C:/Users/mllxwdb2/Dropbox (The University of Manchester)/Simulation - zero-inflated SAE/Other codes/latent_scores_LAD.csv")
latent_scores_PFA <- read.csv("C:/Users/mllxwdb2/Dropbox (The University of Manchester)/Simulation - zero-inflated SAE/Other codes/latent_scores_PFA.csv")

#merge CSEW estimates with synthetic data
synthetic_CSP <- synthetic_CSP %>%
  left_join(latent_scores_LAD, by = "CSP17NM")
synthetic_PFA <- synthetic_PFA %>%
  left_join(latent_scores_PFA, by = "PFA17CD")

#check correlations
cor.test(synthetic_CSP$scores_worry, synthetic_CSP$all_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_worry, synthetic_CSP$damage_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_worry, synthetic_CSP$property_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_worry, synthetic_CSP$violence_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_perceivedasb, synthetic_CSP$all_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_perceivedasb, synthetic_CSP$damage_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_perceivedasb, synthetic_CSP$property_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_perceivedasb, synthetic_CSP$violence_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_policeeffectiveness, synthetic_CSP$all_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_policeeffectiveness, synthetic_CSP$damage_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_policeeffectiveness, synthetic_CSP$property_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_policeeffectiveness, synthetic_CSP$violence_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_collectiveefficacy, synthetic_CSP$all_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_collectiveefficacy, synthetic_CSP$damage_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_collectiveefficacy, synthetic_CSP$property_ME, use = "pairwise.complete.obs")
cor.test(synthetic_CSP$scores_collectiveefficacy, synthetic_CSP$violence_ME, use = "pairwise.complete.obs")

cor.test(synthetic_PFA$scores_worry, synthetic_PFA$all_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_worry, synthetic_PFA$damage_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_worry, synthetic_PFA$property_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_worry, synthetic_PFA$violence_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_perceivedasb, synthetic_PFA$all_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_perceivedasb, synthetic_PFA$damage_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_perceivedasb, synthetic_PFA$property_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_perceivedasb, synthetic_PFA$violence_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_policeeffectiveness, synthetic_PFA$all_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_policeeffectiveness, synthetic_PFA$damage_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_policeeffectiveness, synthetic_PFA$property_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_policeeffectiveness, synthetic_PFA$violence_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_collectiveefficacy, synthetic_PFA$all_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_collectiveefficacy, synthetic_PFA$damage_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_collectiveefficacy, synthetic_PFA$property_ME, use = "pairwise.complete.obs")
cor.test(synthetic_PFA$scores_collectiveefficacy, synthetic_PFA$violence_ME, use = "pairwise.complete.obs")
