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

## continue here