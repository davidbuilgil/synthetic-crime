################################
#
# Simulating crime data
#
# Part VII: Check reliability
#
################################

rm(list=ls())

#avoid scientific notation
options(scipen=999)

#load packages
library(dplyr)
library(here)
library(plotrix)
library(ggplot2)
library(sampling)
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

#select columns of interest in lookup
lookup <- lookup %>%
  select(-LSOA11CD, -LAD17CD, -CSP17NM)

#add spatial information to synthetic data
syn_res_OA <- left_join(syn_res_OA, lookup, by = "OA11CD")

#select variables of interest
syn_res_OA <- syn_res_OA %>%
  select(-Age, -Male, -BornUK, -White, -No_income, -Married, -High_edu,
         -theft, -damage)

#create lists of geographies, population, target sample and crime rate
PFA_list <- syn_res_OA %>%
  group_by(PFA17CD) %>%
  summarise(pop = n(),
            sample = pop * 0.10,
            sample = round(sample, 0),
            vio.count = sum(violence),
            vio.rate = vio.count / pop)
CSP_list <- syn_res_OA %>%
  group_by(CSP17CD) %>%
  summarise(pop = n(),
            sample = pop * 0.10,
            sample = round(sample, 0),
            vio.count = sum(violence),
            vio.rate = vio.count / pop)
MSOA_list <- syn_res_OA %>%
  group_by(MSOA11CD) %>%
  summarise(pop = n(),
            sample = pop * 0.10,
            sample = round(sample, 0),
            vio.count = sum(violence),
            vio.rate = vio.count / pop)

#order of areas
PFA_order <- PFA_list %>%
  mutate(order.PFA = row_number()) %>%
  select(PFA17CD, order.PFA)
CSP_order <- CSP_list %>%
  mutate(order.CSP = row_number()) %>%
  select(CSP17CD, order.CSP)
MSOA_order <- MSOA_list %>%
  mutate(order.MSOA = row_number()) %>%
  select(MSOA11CD, order.MSOA)

#append order of areas to synthetic data
syn_res_OA <- syn_res_OA %>%
  left_join(PFA_order, by = "PFA17CD") %>%
  left_join(CSP_order, by = "CSP17CD") %>%
  left_join(MSOA_order, by = "MSOA11CD")

#remove objects
rm(list=c("PFA_order", "CSP_order", "MSOA_order", "lookup"))

#create empty datasets
MSOA <- data.frame(matrix(NA, nrow = nrow(MSOA_list), ncol = 100))
CSP <- data.frame(matrix(NA, nrow = nrow(CSP_list), ncol = 100))
PFA <- data.frame(matrix(NA, nrow = nrow(PFA_list), ncol = 100))

#reorder by PFA
syn_res_OA <- syn_res_OA %>%
  arrange(order.PFA)

# loops
for(i in 1:100) {
  
  print(i)
  
  #calculate sampling probabilities (srswr)
  sample <- strata(syn_res_OA,
                   stratanames = "PFA17CD",
                   size = PFA_list$sample,
                   method =  "srswr")
  
  #get sample
  get.sample <- getdata(syn_res_OA, sample)
  
  #select variables of interest
  get.sample <- get.sample %>%
    select(-ID, -ID_unit, -Prob, -Stratum)
  
  #calculate crime total
  crime.total <- get.sample %>%
    group_by(PFA17CD) %>%
    summarise(vio.sample = sum(violence))
  
  #calculate crime rate
  crime.total <- crime.total %>%
    left_join(PFA_list, by = "PFA17CD") %>%
    mutate(vio.sample.rate = vio.sample / sample)
  
  #copy crime rate in main dataset
  PFA[, i] <- crime.total$vio.sample.rate
  
  }

#obtain mean and sd from crime rate estimates
PFA.vio.mean <- rowMeans(PFA, na.rm = TRUE)
PFA.vio.sd <- apply(PFA, 1, sd, na.rm = TRUE)
#PFA.vio.se <- PFA.vio.sd / sqrt(length(PFA.vio.sd))
#PFA.vio.se <- apply(PFA, 1, std.error, na.rm = TRUE)

#calculate margin of error at 95% CI
#PFA.vio.margin <- qt(0.975, df = PFA_list$sample - 1)*PFA.vio.se / sqrt(PFA_list$sample)
#PFA.vio.li <- PFA.vio.mean - PFA.vio.margin
#PFA.vio.ui <- PFA.vio.mean + PFA.vio.margin
PFA.vio.li <- PFA.vio.mean - (1.96 * PFA.vio.sd)
PFA.vio.ui <- PFA.vio.mean + (1.96 * PFA.vio.sd)

#merge with main dataset
PFA_list <- PFA_list %>%
  cbind(PFA.vio.li, PFA.vio.ui)

#create dataset to save
PFA_save <- PFA_list %>%
  select(PFA17CD, vio.rate, PFA.vio.li, PFA.vio.ui) %>%
  cbind(PFA)

#save dataset
write.csv(PFA_save, here("data/srswr", "srswr_vio_PFA.csv"))
#PFA_list <- read.csv(here("data/srswr", "srswr_vio_PFA.csv"))

#remove objects
rm(list=c("PFA", "PFA_save"))

#arrange from smaller to larger rate
PFA_list <- PFA_list %>%
  arrange(vio.rate) %>%
  mutate(vio.order = row_number())

#plot
plot.PFA.vio <- ggplot(PFA_list, aes(x = vio.order, y = vio.rate)) +
  geom_line() +
  ylim(0.0, 0.15) +
  geom_ribbon(aes(ymin = PFA.vio.li, ymax = PFA.vio.ui), alpha = 0.3) +
  ggtitle("Violence (PFAs)")  +
  labs(x = "PFAs", y = "Crime rate") +
  theme_bw()

#reorder by CSP
syn_res_OA <- syn_res_OA %>%
  arrange(order.CSP)

# loops
for(i in 1:100) {
  
  print(i)
  
  #calculate sampling probabilities (srswr)
  sample <- strata(syn_res_OA,
                   stratanames = "CSP17CD",
                   size = CSP_list$sample,
                   method =  "srswr")
  
  #get sample
  get.sample <- getdata(syn_res_OA, sample)
  
  #select variables of interest
  get.sample <- get.sample %>%
    select(-ID, -ID_unit, -Prob, -Stratum)
  
  #calculate crime total
  crime.total <- get.sample %>%
    group_by(CSP17CD) %>%
    summarise(vio.sample = sum(violence))
  
  #calculate crime rate
  crime.total <- crime.total %>%
    left_join(CSP_list, by = "CSP17CD") %>%
    mutate(vio.sample.rate = vio.sample / sample)
  
  #copy crime rate in main dataset
  CSP[, i] <- crime.total$vio.sample.rate
  
}

#obtain mean and sd from crime rate estimates
CSP.vio.mean <- rowMeans(CSP, na.rm = TRUE)
CSP.vio.sd <- apply(CSP, 1, sd, na.rm = TRUE)
#CSP.vio.se <- CSP.vio.sd / sqrt(length(CSP.vio.sd))
#CSP.vio.se <- apply(CSP, 1, std.error, na.rm = TRUE)

#calculate margin of error at 95% CI
#CSP.vio.margin <- qt(0.975, df = CSP_list$sample - 1)*CSP.vio.se / sqrt(CSP_list$sample)
#CSP.vio.li <- CSP.vio.mean - CSP.vio.margin
#CSP.vio.ui <- CSP.vio.mean + CSP.vio.margin
CSP.vio.li <- CSP.vio.mean - (1.96 * CSP.vio.sd)
CSP.vio.ui <- CSP.vio.mean + (1.96 * CSP.vio.sd)

#merge with main dataset
CSP_list <- CSP_list %>%
  cbind(CSP.vio.li, CSP.vio.ui)

#create dataset to save
CSP_save <- CSP_list %>%
  select(CSP17CD, vio.rate, CSP.vio.li, CSP.vio.ui) %>%
  cbind(CSP)

#save dataset
write.csv(CSP_save, here("data/srswr", "srswr_vio_CSP.csv"))
#CSP_list <- read.csv(here("data/srswr", "srswr_vio_CSP.csv"))

#remove objects
rm(list=c("CSP", "CSP_save"))

#arrange from smaller to larger rate
CSP_list <- CSP_list %>%
  arrange(vio.rate) %>%
  mutate(vio.order = row_number())

#plot
plot.CSP.vio <- ggplot(CSP_list, aes(x = vio.order, y = vio.rate)) +
  geom_line() +
  ylim(0.0, 0.15) +
  geom_ribbon(aes(ymin = CSP.vio.li, ymax = CSP.vio.ui), alpha = 0.3) +
  ggtitle("Violence (CSPs)")  +
  labs(x = "CSPs", y = "Crime rate") +
  theme_bw()

#reorder by MSOA
syn_res_OA <- syn_res_OA %>%
  arrange(order.MSOA)

# loops
for(i in 1:100) {
  
  print(i)
  
  #calculate sampling probabilities (srswr)
  sample <- strata(syn_res_OA,
                   stratanames = "MSOA11CD",
                   size = MSOA_list$sample,
                   method =  "srswr")
  
  #get sample
  get.sample <- getdata(syn_res_OA, sample)
  
  #select variables of interest
  get.sample <- get.sample %>%
    select(-ID, -ID_unit, -Prob, -Stratum)
  
  #calculate crime total
  crime.total <- get.sample %>%
    group_by(MSOA11CD) %>%
    summarise(vio.sample = sum(violence))
  
  #calculate crime rate
  crime.total <- crime.total %>%
    left_join(MSOA_list, by = "MSOA11CD") %>%
    mutate(vio.sample.rate = vio.sample / sample)
  
  #copy crime rate in main dataset
  MSOA[, i] <- crime.total$vio.sample.rate
  
}

#obtain mean and sd from crime rate estimates
MSOA.vio.mean <- rowMeans(MSOA, na.rm = TRUE)
MSOA.vio.sd <- apply(MSOA, 1, sd, na.rm = TRUE)
#MSOA.vio.se <- MSOA.vio.sd / sqrt(length(MSOA.vio.sd))
#MSOA.vio.se <- apply(MSOA, 1, std.error, na.rm = TRUE)

#calculate margin of error at 95% CI
#MSOA.vio.margin <- qt(0.975, df = MSOA_list$sample - 1)*MSOA.vio.se / sqrt(MSOA_list$sample)
#MSOA.vio.li <- MSOA.vio.mean - MSOA.vio.margin
#MSOA.vio.ui <- MSOA.vio.mean + MSOA.vio.margin
MSOA.vio.li <- MSOA.vio.mean - (1.96 * MSOA.vio.sd)
MSOA.vio.ui <- MSOA.vio.mean + (1.96 * MSOA.vio.sd)

#merge with main dataset
MSOA_list <- MSOA_list %>%
  cbind(MSOA.vio.li, MSOA.vio.ui)

#create dataset to save
MSOA_save <- MSOA_list %>%
  select(MSOA11CD, vio.rate, MSOA.vio.li, MSOA.vio.ui) %>%
  cbind(MSOA)

#save dataset
write.csv(MSOA_save, here("data/srswr", "srswr_vio_MSOA.csv"))
#MSOA_list <- read.csv(here("data/srswr", "srswr_vio_MSOA.csv"))

#remove objects
rm(list=c("MSOA", "MSOA_save"))

#arrange from smaller to larger rate
MSOA_list <- MSOA_list %>%
  arrange(vio.rate) %>%
  mutate(vio.order = row_number())

#plot
plot.MSOA.vio <- ggplot(MSOA_list, aes(x = vio.order, y = vio.rate)) +
  geom_line() +
  ylim(0.0, 0.15) +
  geom_ribbon(aes(ymin = MSOA.vio.li, ymax = MSOA.vio.ui), alpha = 0.3) +
  ggtitle("Violence (MSOAs)")  +
  labs(x = "MSOAs", y = "Crime rate") +
  theme_bw()

#remove objects
rm(list=c("syn_res_OA"))

#load synthetic population of household crimes
load(here("data", "HHsynthetic_population_crimes.Rdata"))

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

#select columns of interest in lookup
lookup <- lookup %>%
  select(-LSOA11CD, -LAD17CD, -CSP17NM)

#add spatial information to synthetic data
syn_res_OA <- left_join(syn_res_OA, lookup, by = "OA11CD")

#select variables of interest
syn_res_OA <- syn_res_OA %>%
  select(-age_more65, -terraced, -hrp_white, -one_person, -no_income,
         -no_car, -social_rent, -hrp_no_religion)

#create lists of geographies, population, target sample and crime rate
PFAhh_list <- syn_res_OA %>%
  group_by(PFA17CD) %>%
  summarise(pop = n(),
            sample = pop * 0.10,
            sample = round(sample, 0),
            theft.count = sum(theft),
            theft.rate = theft.count / pop,
            damage.count = sum(damage),
            damage.rate = damage.count / pop)
CSPhh_list <- syn_res_OA %>%
  group_by(CSP17CD) %>%
  summarise(pop = n(),
            sample = pop * 0.10,
            sample = round(sample, 0),
            theft.count = sum(theft),
            theft.rate = theft.count / pop,
            damage.count = sum(damage),
            damage.rate = damage.count / pop)
MSOAhh_list <- syn_res_OA %>%
  group_by(MSOA11CD) %>%
  summarise(pop = n(),
            sample = pop * 0.10,
            sample = round(sample, 0),
            theft.count = sum(theft),
            theft.rate = theft.count / pop,
            damage.count = sum(damage),
            damage.rate = damage.count / pop)

#order of areas
PFAhh_order <- PFAhh_list %>%
  mutate(order.PFA = row_number()) %>%
  select(PFA17CD, order.PFA)
CSPhh_order <- CSPhh_list %>%
  mutate(order.CSP = row_number()) %>%
  select(CSP17CD, order.CSP)
MSOAhh_order <- MSOAhh_list %>%
  mutate(order.MSOA = row_number()) %>%
  select(MSOA11CD, order.MSOA)

#append order of areas to synthetic data
syn_res_OA <- syn_res_OA %>%
  left_join(PFAhh_order, by = "PFA17CD") %>%
  left_join(CSPhh_order, by = "CSP17CD") %>%
  left_join(MSOAhh_order, by = "MSOA11CD")

#remove objects
rm(list=c("PFAhh_order", "CSPhh_order", "MSOAhh_order", "lookup"))

#create empty datasets
MSOA.theft <- data.frame(matrix(NA, nrow = nrow(MSOAhh_list), ncol = 100))
CSP.theft <- data.frame(matrix(NA, nrow = nrow(CSPhh_list), ncol = 100))
PFA.theft <- data.frame(matrix(NA, nrow = nrow(PFAhh_list), ncol = 100))
MSOA.damage <- data.frame(matrix(NA, nrow = nrow(MSOAhh_list), ncol = 100))
CSP.damage <- data.frame(matrix(NA, nrow = nrow(CSPhh_list), ncol = 100))
PFA.damage <- data.frame(matrix(NA, nrow = nrow(PFAhh_list), ncol = 100))

#reorder by PFA
syn_res_OA <- syn_res_OA %>%
  arrange(order.PFA)

# loops
for(i in 1:100) {
  
  print(i)
  
  #calculate sampling probabilities (srswr)
  sample <- strata(syn_res_OA,
                   stratanames = "PFA17CD",
                   size = PFAhh_list$sample,
                   method =  "srswr")
  
  #get sample
  get.sample <- getdata(syn_res_OA, sample)
  
  #select variables of interest
  get.sample <- get.sample %>%
    select(-ID, -ID_unit, -Prob, -Stratum)
  
  #calculate crime total
  crime.total <- get.sample %>%
    group_by(PFA17CD) %>%
    summarise(theft.sample = sum(theft),
              damage.sample = sum(damage))
  
  #calculate crime rate
  crime.total <- crime.total %>%
    left_join(PFAhh_list, by = "PFA17CD") %>%
    mutate(theft.sample.rate = theft.sample / sample,
           damage.sample.rate = damage.sample / sample)
  
  #copy crime rate in main dataset
  PFA.theft[, i] <- crime.total$theft.sample.rate
  PFA.damage[, i] <- crime.total$damage.sample.rate
  
}

#obtain mean and sd from crime rate estimates
PFA.theft.mean <- rowMeans(PFA.theft, na.rm = TRUE)
PFA.theft.sd <- apply(PFA.theft, 1, sd, na.rm = TRUE)
#PFA.theft.se <- PFA.theft.sd / sqrt(length(PFA.theft.sd))
#PFA.theft.se <- apply(PFA.theft, 1, std.error, na.rm = TRUE)

PFA.damage.mean <- rowMeans(PFA.damage, na.rm = TRUE)
PFA.damage.sd <- apply(PFA.damage, 1, sd, na.rm = TRUE)
#PFA.damage.se <- PFA.damage.sd / sqrt(length(PFA.damage.sd))
#PFA.damage.se <- apply(PFA.damage, 1, std.error, na.rm = TRUE)

#calculate margin of error at 95% CI
#PFA.theft.margin <- qt(0.975, df = PFA_list$sample - 1)*PFA.theft.se / sqrt(PFA_list$sample)
#PFA.theft.li <- PFA.theft.mean - PFA.theft.margin
#PFA.theft.ui <- PFA.theft.mean + PFA.theft.margin
PFA.theft.li <- PFA.theft.mean - (1.96 * PFA.theft.sd)
PFA.theft.ui <- PFA.theft.mean + (1.96 * PFA.theft.sd)

#PFA.damage.margin <- qt(0.975, df = PFA_list$sample - 1)*PFA.damage.se / sqrt(PFA_list$sample)
#PFA.damage.li <- PFA.theft.mean - PFA.theft.margin
#PFA.damage.ui <- PFA.theft.mean + PFA.theft.margin
PFA.damage.li <- PFA.damage.mean - (1.96 * PFA.damage.sd)
PFA.damage.ui <- PFA.damage.mean + (1.96 * PFA.damage.sd)

#merge with main dataset
PFAhh_list <- PFAhh_list %>%
  cbind(PFA.theft.li, PFA.theft.ui, PFA.damage.li, PFA.damage.ui)

#create dataset to save
PFA_save_theft <- PFAhh_list %>%
  select(PFA17CD, theft.rate, PFA.theft.li, PFA.theft.ui) %>%
  cbind(PFA.theft)
PFA_save_damage <- PFAhh_list %>%
  select(PFA17CD, damage.rate, PFA.damage.li, PFA.damage.ui) %>%
  cbind(PFA.damage)

#save dataset
write.csv(PFA_save_theft, here("data/srswr", "srswr_theft_PFA.csv"))
write.csv(PFA_save_damage, here("data/srswr", "srswr_damage_PFA.csv"))
#PFA_theft_list <- read.csv(here("data/srswr", "srswr_theft_PFA.csv"))
#PFA_damage_list <- read.csv(here("data/srswr", "srswr_damage_PFA.csv"))
#PFAhh_list <- PFA_theft_list %>%
#  select(PFA17CD, theft.rate, PFA.theft.li, PFA.theft.ui) %>%
#  left_join(PFA_damage_list, by = "PFA17CD") %>%
#  select(PFA17CD, theft.rate, PFA.theft.li, PFA.theft.ui,
#         damage.rate, PFA.damage.li, PFA.damage.ui)

#remove objects
rm(list=c("PFA.theft", "PFA.damage", "PFA_save_theft", "PFA_save_damage"))

#arrange from smaller to larger rate
PFAhh_list <- PFAhh_list %>%
  arrange(theft.rate) %>%
  mutate(theft.order = row_number())

#plot
plot.PFA.theft <- ggplot(PFAhh_list, aes(x = theft.order, y = theft.rate)) +
  geom_line() +
  ylim(0.0, 0.15) +
  geom_ribbon(aes(ymin = PFA.theft.li, ymax = PFA.theft.ui), alpha = 0.3) +
  ggtitle("Property crime (PFAs)")  +
  labs(x = "PFAs", y = "Crime rate") +
  theme_bw()

#arrange from smaller to larger rate
PFAhh_list <- PFAhh_list %>%
  arrange(damage.rate) %>%
  mutate(damage.order = row_number())

#plot
plot.PFA.damage <- ggplot(PFAhh_list, aes(x = damage.order, y = damage.rate)) +
  geom_line() +
  ylim(0.0, 0.15) +
  geom_ribbon(aes(ymin = PFA.damage.li, ymax = PFA.damage.ui), alpha = 0.3) +
  ggtitle("Damage (PFAs)")  +
  labs(x = "PFAs", y = "Crime rate") +
  theme_bw()

#reorder by CSP
syn_res_OA <- syn_res_OA %>%
  arrange(order.CSP)

# loops
for(i in 1:100) {
  
  print(i)
  
  #calculate sampling probabilities (srswr)
  sample <- strata(syn_res_OA,
                   stratanames = "CSP17CD",
                   size = CSPhh_list$sample,
                   method =  "srswr")
  
  #get sample
  get.sample <- getdata(syn_res_OA, sample)
  
  #select variables of interest
  get.sample <- get.sample %>%
    select(-ID, -ID_unit, -Prob, -Stratum)
  
  #calculate crime total
  crime.total <- get.sample %>%
    group_by(CSP17CD) %>%
    summarise(theft.sample = sum(theft),
              damage.sample = sum(damage))
  
  #calculate crime rate
  crime.total <- crime.total %>%
    left_join(CSPhh_list, by = "CSP17CD") %>%
    mutate(theft.sample.rate = theft.sample / sample,
           damage.sample.rate = damage.sample / sample)
  
  #copy crime rate in main dataset
  CSP.theft[, i] <- crime.total$theft.sample.rate
  CSP.damage[, i] <- crime.total$damage.sample.rate
  
}

#obtain mean and sd from crime rate estimates
CSP.theft.mean <- rowMeans(CSP.theft, na.rm = TRUE)
CSP.theft.sd <- apply(CSP.theft, 1, sd, na.rm = TRUE)
#CSP.theft.se <- CSP.theft.sd / sqrt(length(CSP.theft.sd))
#CSP.theft.se <- apply(CSP.theft, 1, std.error, na.rm = TRUE)

CSP.damage.mean <- rowMeans(CSP.damage, na.rm = TRUE)
CSP.damage.sd <- apply(CSP.damage, 1, sd, na.rm = TRUE)
#CSP.damage.se <- CSP.damage.sd / sqrt(length(CSP.damage.sd))
#CSP.damage.se <- apply(CSP.damage, 1, std.error, na.rm = TRUE)

#calculate margin of error at 95% CI
#CSP.theft.margin <- qt(0.975, df = CSP_list$sample - 1)*CSP.theft.se / sqrt(CSP_list$sample)
#CSP.theft.li <- CSP.theft.mean - CSP.theft.margin
#CSP.theft.ui <- CSP.theft.mean + CSP.theft.margin
CSP.theft.li <- CSP.theft.mean - (1.96 * CSP.theft.sd)
CSP.theft.ui <- CSP.theft.mean + (1.96 * CSP.theft.sd)

#CSP.damage.margin <- qt(0.975, df = CSP_list$sample - 1)*CSP.damage.se / sqrt(CSP_list$sample)
#CSP.damage.li <- CSP.theft.mean - CSP.theft.margin
#CSP.damage.ui <- CSP.theft.mean + CSP.theft.margin
CSP.damage.li <- CSP.damage.mean - (1.96 * CSP.damage.sd)
CSP.damage.ui <- CSP.damage.mean + (1.96 * CSP.damage.sd)

#merge with main dataset
CSPhh_list <- CSPhh_list %>%
  cbind(CSP.theft.li, CSP.theft.ui, CSP.damage.li, CSP.damage.ui)

#create dataset to save
CSP_save_theft <- CSPhh_list %>%
  select(CSP17CD, theft.rate, CSP.theft.li, CSP.theft.ui) %>%
  cbind(CSP.theft)
CSP_save_damage <- CSPhh_list %>%
  select(CSP17CD, damage.rate, CSP.damage.li, CSP.damage.ui) %>%
  cbind(CSP.damage)

#save dataset
write.csv(CSP_save_theft, here("data/srswr", "srswr_theft_CSP.csv"))
write.csv(CSP_save_damage, here("data/srswr", "srswr_damage_CSP.csv"))
#CSP_theft_list <- read.csv(here("data/srswr", "srswr_theft_CSP.csv"))
#CSP_damage_list <- read.csv(here("data/srswr", "srswr_damage_CSP.csv"))
#CSPhh_list <- CSP_theft_list %>%
#  select(CSP17CD, theft.rate, CSP.theft.li, CSP.theft.ui) %>%
#  left_join(CSP_damage_list, by = "CSP17CD") %>%
#  select(CSP17CD, theft.rate, CSP.theft.li, CSP.theft.ui,
#         damage.rate, CSP.damage.li, CSP.damage.ui)

#remove objects
rm(list=c("CSP.theft", "CSP.damage", "CSP_save_theft", "CSP_save_damage"))

#arrange from smaller to larger rate
CSPhh_list <- CSPhh_list %>%
  arrange(theft.rate) %>%
  mutate(theft.order = row_number())

#plot
plot.CSP.theft <- ggplot(CSPhh_list, aes(x = theft.order, y = theft.rate)) +
  geom_line() +
  ylim(0.0, 0.15) +
  geom_ribbon(aes(ymin = CSP.theft.li, ymax = CSP.theft.ui), alpha = 0.3) +
  ggtitle("Property crime (CSPs)")  +
  labs(x = "CSPs", y = "Crime rate") +
  theme_bw()

#arrange from smaller to larger rate
CSPhh_list <- CSPhh_list %>%
  arrange(damage.rate) %>%
  mutate(damage.order = row_number())

#plot
plot.CSP.damage <- ggplot(CSPhh_list, aes(x = damage.order, y = damage.rate)) +
  geom_line() +
  ylim(0.0, 0.15) +
  geom_ribbon(aes(ymin = CSP.damage.li, ymax = CSP.damage.ui), alpha = 0.3) +
  ggtitle("Damage (CSPs)")  +
  labs(x = "CSPs", y = "Crime rate") +
  theme_bw()

#reorder by MSOA
syn_res_OA <- syn_res_OA %>%
  arrange(order.MSOA)

# loops
for(i in 1:100) {
  
  print(i)
  
  #calculate sampling probabilities (srswr)
  sample <- strata(syn_res_OA,
                   stratanames = "MSOA11CD",
                   size = MSOAhh_list$sample,
                   method =  "srswr")
  
  #get sample
  get.sample <- getdata(syn_res_OA, sample)
  
  #select variables of interest
  get.sample <- get.sample %>%
    select(-ID, -ID_unit, -Prob, -Stratum)
  
  #calculate crime total
  crime.total <- get.sample %>%
    group_by(MSOA11CD) %>%
    summarise(theft.sample = sum(theft),
              damage.sample = sum(damage))
  
  #calculate crime rate
  crime.total <- crime.total %>%
    left_join(MSOAhh_list, by = "MSOA11CD") %>%
    mutate(theft.sample.rate = theft.sample / sample,
           damage.sample.rate = damage.sample / sample)
  
  #copy crime rate in main dataset
  MSOA.theft[, i] <- crime.total$theft.sample.rate
  MSOA.damage[, i] <- crime.total$damage.sample.rate
  
}

#obtain mean and sd from crime rate estimates
MSOA.theft.mean <- rowMeans(MSOA.theft, na.rm = TRUE)
MSOA.theft.sd <- apply(MSOA.theft, 1, sd, na.rm = TRUE)
#MSOA.theft.se <- MSOA.theft.sd / sqrt(length(MSOA.theft.sd))
#MSOA.theft.se <- apply(MSOA.theft, 1, std.error, na.rm = TRUE)

MSOA.damage.mean <- rowMeans(MSOA.damage, na.rm = TRUE)
MSOA.damage.sd <- apply(MSOA.damage, 1, sd, na.rm = TRUE)
#MSOA.damage.se <- MSOA.damage.sd / sqrt(length(MSOA.damage.sd))
#MSOA.damage.se <- apply(MSOA.damage, 1, std.error, na.rm = TRUE)

#calculate margin of error at 95% CI
#MSOA.theft.margin <- qt(0.975, df = MSOA_list$sample - 1)*MSOA.theft.se / sqrt(MSOA_list$sample)
#MSOA.theft.li <- MSOA.theft.mean - MSOA.theft.margin
#MSOA.theft.ui <- MSOA.theft.mean + MSOA.theft.margin
MSOA.theft.li <- MSOA.theft.mean - (1.96 * MSOA.theft.sd)
MSOA.theft.ui <- MSOA.theft.mean + (1.96 * MSOA.theft.sd)

#MSOA.damage.margin <- qt(0.975, df = MSOA_list$sample - 1)*MSOA.damage.se / sqrt(MSOA_list$sample)
#MSOA.damage.li <- MSOA.theft.mean - MSOA.theft.margin
#MSOA.damage.ui <- MSOA.theft.mean + MSOA.theft.margin
MSOA.damage.li <- MSOA.damage.mean - (1.96 * MSOA.damage.sd)
MSOA.damage.ui <- MSOA.damage.mean + (1.96 * MSOA.damage.sd)

#merge with main dataset
MSOAhh_list <- MSOAhh_list %>%
  cbind(MSOA.theft.li, MSOA.theft.ui, MSOA.damage.li, MSOA.damage.ui)

#create dataset to save
MSOA_save_theft <- MSOAhh_list %>%
  select(MSOA11CD, theft.rate, MSOA.theft.li, MSOA.theft.ui) %>%
  cbind(MSOA.theft)
MSOA_save_damage <- MSOAhh_list %>%
  select(MSOA11CD, damage.rate, MSOA.damage.li, MSOA.damage.ui) %>%
  cbind(MSOA.damage)

#save dataset
write.csv(MSOA_save_theft, here("data/srswr", "srswr_theft_MSOA.csv"))
write.csv(MSOA_save_damage, here("data/srswr", "srswr_damage_MSOA.csv"))

#remove objects
rm(list=c("MSOA.theft", "MSOA.damage", "MSOA_save_theft", "MSOA_save_damage"))

#arrange from smaller to larger rate
MSOAhh_list <- MSOAhh_list %>%
  arrange(theft.rate) %>%
  mutate(theft.order = row_number())

#plot
plot.MSOA.theft <- ggplot(MSOAhh_list, aes(x = theft.order, y = theft.rate)) +
  geom_line() +
  ylim(0.0, 0.15) +
  geom_ribbon(aes(ymin = MSOA.theft.li, ymax = MSOA.theft.ui), alpha = 0.3) +
  ggtitle("Property crime (MSOAs)")  +
  labs(x = "MSOAs", y = "Crime rate") +
  theme_bw()

#arrange from smaller to larger rate
MSOAhh_list <- MSOAhh_list %>%
  arrange(damage.rate) %>%
  mutate(damage.order = row_number())

#plot
plot.MSOA.damage <- ggplot(MSOAhh_list, aes(x = damage.order, y = damage.rate)) +
  geom_line() +
  ylim(0.0, 0.15) +
  geom_ribbon(aes(ymin = MSOA.damage.li, ymax = MSOA.damage.ui), alpha = 0.3) +
  ggtitle("Damage (MSOAs)")  +
  labs(x = "MSOAs", y = "Crime rate") +
  theme_bw()

#print scatter plots
ggarrange(plot.PFA.vio, plot.CSP.vio, plot.MSOA.vio,
          plot.PFA.theft, plot.CSP.theft, plot.MSOA.theft,
          plot.PFA.damage, plot.CSP.damage, plot.MSOA.damage,
          nrow = 3, ncol = 3)
ggarrange(plot.PFA.vio, plot.CSP.vio,
          plot.PFA.theft, plot.CSP.theft,
          plot.PFA.damage, plot.CSP.damage,
          nrow = 3, ncol = 2)

ggsave(here("plots/reliability_plot.png"),
       width = 20, height = 15, units = "cm")

