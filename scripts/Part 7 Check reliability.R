################################
#
# Simulating crime data
#
# Part VII: Check reliability
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

#load synthetic police data
load(here("data", "synthetic_police_crimes.Rdata"))

#add spatial information to synthetic data
Data_crimes <- left_join(Data_crimes, lookup, by = "OA11CD")

#count synthetic crime data in LSOAs
syn_data_LSOA <- syn_res_OA %>%
  group_by(LSOA11CD) %>%
  summarise(violence_syn = sum(violence),
            property_syn = sum(theft),
            damage_syn   = sum(damage),
            pop = n()) 
syn_police_LSOA <- Data_crimes %>%
  group_by(LSOA11CD) %>%
  summarise(violence_syn_pol = sum(violence),
            property_syn_pol = sum(theft),
            damage_syn_pol   = sum(damage)) 

#merge both files
data_LSOA <- left_join(syn_data_LSOA, syn_police_LSOA, by = "LSOA11CD")

#count synthetic crime data in MSOAs
syn_data_MSOA <- syn_res_OA %>%
  group_by(MSOA11CD) %>%
  summarise(violence_syn = sum(violence),
            property_syn = sum(theft),
            damage_syn   = sum(damage),
            pop = n()) 
syn_police_MSOA <- Data_crimes %>%
  group_by(MSOA11CD) %>%
  summarise(violence_syn_pol = sum(violence),
            property_syn_pol = sum(theft),
            damage_syn_pol   = sum(damage)) 

#merge both files
data_MSOA <- left_join(syn_data_MSOA, syn_police_MSOA, by = "MSOA11CD")

#count synthetic crime data in CSPs
syn_data_CSP <- syn_res_OA %>%
  group_by(CSP17NM) %>%
  summarise(violence_syn = sum(violence),
            property_syn = sum(theft),
            damage_syn   = sum(damage),
            pop = n()) 
syn_police_CSP <- Data_crimes %>%
  group_by(CSP17NM) %>%
  summarise(violence_syn_pol = sum(violence),
            property_syn_pol = sum(theft),
            damage_syn_pol   = sum(damage)) 

#merge both files
data_CSP <- left_join(syn_data_CSP, syn_police_CSP, by = "CSP17NM")

#count synthetic crime data in PFAs
syn_data_PFA <- syn_res_OA %>%
  group_by(PFA17CD) %>%
  summarise(violence_syn = sum(violence),
            property_syn = sum(theft),
            damage_syn   = sum(damage),
            pop = n()) 
syn_police_PFA <- Data_crimes %>%
  group_by(PFA17CD) %>%
  summarise(violence_syn_pol = sum(violence),
            property_syn_pol = sum(theft),
            damage_syn_pol   = sum(damage)) 

#merge both files
data_PFA <- left_join(syn_data_PFA, syn_police_PFA, by = "PFA17CD")

#calculate measurement error (assuming multiplicative error)
data_LSOA <- data_LSOA %>%
  mutate(all_syn_pol = (violence_syn_pol + property_syn_pol + damage_syn_pol) / pop,
         all_syn = (violence_syn + property_syn + damage_syn) / pop,
         violence_syn_pol = violence_syn_pol / pop,
         violence_syn = violence_syn / pop ,
         property_syn_pol = property_syn_pol / pop ,
         property_syn = property_syn / pop ,
         damage_syn_pol = damage_syn_pol / pop ,
         damage_syn = damage_syn / pop ,
         all_ME = all_syn_pol / all_syn,
         violence_ME = violence_syn_pol / violence_syn,
         property_ME = property_syn_pol / property_syn,
         damage_ME = damage_syn_pol / damage_syn)
data_MSOA <- data_MSOA %>%
  mutate(all_syn_pol = (violence_syn_pol + property_syn_pol + damage_syn_pol) / pop,
         all_syn = (violence_syn + property_syn + damage_syn) / pop,
         violence_syn_pol = violence_syn_pol / pop,
         violence_syn = violence_syn / pop ,
         property_syn_pol = property_syn_pol / pop ,
         property_syn = property_syn / pop ,
         damage_syn_pol = damage_syn_pol / pop ,
         damage_syn = damage_syn / pop ,
         all_ME = all_syn_pol / all_syn,
         violence_ME = violence_syn_pol / violence_syn,
         property_ME = property_syn_pol / property_syn,
         damage_ME = damage_syn_pol / damage_syn)
data_CSP <- data_CSP %>%
  mutate(all_syn_pol = (violence_syn_pol + property_syn_pol + damage_syn_pol) / pop,
         all_syn = (violence_syn + property_syn + damage_syn) / pop,
         violence_syn_pol = violence_syn_pol / pop,
         violence_syn = violence_syn / pop ,
         property_syn_pol = property_syn_pol / pop ,
         property_syn = property_syn / pop ,
         damage_syn_pol = damage_syn_pol / pop ,
         damage_syn = damage_syn / pop ,
         all_ME = all_syn_pol / all_syn,
         violence_ME = violence_syn_pol / violence_syn,
         property_ME = property_syn_pol / property_syn,
         damage_ME = damage_syn_pol / damage_syn)
data_PFA <- data_PFA %>%
  mutate(all_syn_pol = (violence_syn_pol + property_syn_pol + damage_syn_pol) / pop,
         all_syn = (violence_syn + property_syn + damage_syn) / pop,
         violence_syn_pol = violence_syn_pol / pop,
         violence_syn = violence_syn / pop ,
         property_syn_pol = property_syn_pol / pop ,
         property_syn = property_syn / pop ,
         damage_syn_pol = damage_syn_pol / pop ,
         damage_syn = damage_syn / pop ,
         all_ME = all_syn_pol / all_syn,
         violence_ME = violence_syn_pol / violence_syn,
         property_ME = property_syn_pol / property_syn,
         damage_ME = damage_syn_pol / damage_syn)

#select only variables of interest
data_for_boxplot.1 <- data_MSOA %>%
  mutate('Crime type' = "Violence",
         'Geography' = "MSOA",
         'ME' = violence_ME) %>%
  select('Crime type', Geography, ME)
data_for_boxplot.2 <- data_MSOA %>%
  mutate('Crime type' = "Property crime",
         'Geography' = "MSOA",
         'ME' = property_ME) %>%
  select('Crime type', Geography, ME)
data_for_boxplot.3 <- data_MSOA %>%
  mutate('Crime type' = "Damage",
         'Geography' = "MSOA",
         'ME' = damage_ME) %>%
  select('Crime type', Geography, ME)
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
data_for_boxplot.10 <- data_LSOA %>%
  mutate('Crime type' = "Violence",
         'Geography' = "LSOA",
         'ME' = violence_ME) %>%
  select('Crime type', Geography, ME)
data_for_boxplot.11 <- data_LSOA %>%
  mutate('Crime type' = "Property crime",
         'Geography' = "LSOA",
         'ME' = property_ME) %>%
  select('Crime type', Geography, ME)
data_for_boxplot.12 <- data_LSOA %>%
  mutate('Crime type' = "Damage",
         'Geography' = "LSOA",
         'ME' = damage_ME) %>%
  select('Crime type', Geography, ME)
data_for_boxplot.13 <- data_LSOA %>%
  mutate('Crime type' = "All",
         'Geography' = "LSOA",
         'ME' = all_ME) %>%
  select('Crime type', Geography, ME)
data_for_boxplot.14 <- data_MSOA %>%
  mutate('Crime type' = "All",
         'Geography' = "MSOA",
         'ME' = all_ME) %>%
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
data_for_boxplot <- rbind(data_for_boxplot.1, data_for_boxplot.2,
                          data_for_boxplot.3, data_for_boxplot.4,
                          data_for_boxplot.5, data_for_boxplot.6,
                          data_for_boxplot.7, data_for_boxplot.8,
                          data_for_boxplot.9, data_for_boxplot.10,
                          data_for_boxplot.11, data_for_boxplot.12,
                          data_for_boxplot.13, data_for_boxplot.14,
                          data_for_boxplot.15, data_for_boxplot.16)
data_for_boxplot <- data_for_boxplot %>%
  mutate(Geography = factor(Geography, levels = c("LSOA", "MSOA", "CSP", "PFA"))) %>%
  filter(Geography != "LSOA" & Geography != "MSOA")

#grouped boxplot
ggplot(data_for_boxplot, aes(x = `Crime type`, y = ME, fill = Geography)) + 
  geom_boxplot() +
  ggtitle("Boxplots of measurement error in police crime records (simulation)") +
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
write.csv(data_CSP, here("data/synthetic_CSP.csv"))
write.csv(data_PFA, here("data/synthetic_PFA.csv"))
