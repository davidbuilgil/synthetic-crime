################################
#
# Simulating crime data
#
# Part IV: Select a sample as in the CSEW
#
################################

rm(list=ls())

#seed is set for replication
set.seed(999)

#load packages
library(dplyr)
library(here)
library(sampling)

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

#as in the CSEW, remove all individuals aged less than 16
syn_res_OA <- syn_res_OA %>%
  filter(Age >= 16)

#keep ID for each unit
syn_res_OA <- syn_res_OA %>%
  select(ID, OA11CD)

#load OA to LAD lookup
lookup <- read.csv(here("data", "Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District__December_2017__Lookup_with_Area_Classifications_in_Great_Britain.csv"))

#select variables of interest in lookup
lookup <- lookup %>%
  select(ï..OA11CD, LSOA11CD, MSOA11CD, LAD17CD) %>%
  rename(OA11CD = ï..OA11CD)

#load LAD to PFA lookup
lookup2 <- read.csv(here("data", "Local_Authority_District_to_Community_Safety_Partnerships_to_Police_Force_Areas__January_2017__Lookup_in_England_and_Wales_Version_2.csv"))

#select variables of interest in lookup
lookup2 <- lookup2 %>%
  select(ï..LAD17CD, CSP17CD, PFA17CD) %>%
  rename(LAD17CD = ï..LAD17CD)

#merge two lookups
lookup <- left_join(lookup, lookup2, by = "LAD17CD")

#add LAD information to synthetic data
syn_res_OA <- left_join(syn_res_OA, lookup, by = "OA11CD")

#remove objects
rm(list=c("lookup"))

#count population in each MSOA
pop_MSOA <- syn_res_OA %>%
  group_by(MSOA11CD, PFA17CD) %>%
  summarise(pop = n()) %>%
  arrange(PFA17CD, desc(pop))

#combine City of London Police and Metropolitan Police
pop_MSOA <- pop_MSOA %>%
  mutate(PFA17CD = ifelse(PFA17CD == 'E23000034', 'E23000001', PFA17CD))

#count population in each PFA
pop_PFA <- pop_MSOA %>%
  group_by(PFA17CD) %>%
  summarise(pop = sum(pop))

#count MSOAs in each PFA and count number of MSOAs in each stratum and number of citizens in each PFA
MSOA_PFA <- pop_MSOA %>%
  group_by(PFA17CD) %>%
  summarise(num = n()) %>%
  mutate(stratum.A = round(num/3),
         stratum.B = round(num/3),
         stratum.C = round(num/3),
         stratum.C = ifelse(test = stratum.A + stratum.B + stratum.C == num, yes = stratum.C, no = stratum.C+1),
         stratum.C = ifelse(test = stratum.A + stratum.B + stratum.C == num, yes = stratum.C, no = stratum.C-2)) %>%
  left_join(pop_PFA, by = "PFA17CD")

#load area sample sizes in CSEW
csew_11_sample <- read.csv(here("data", "csew_11_sample.csv"))

#merge CSEW sample size with lookup
csew_11_sample <- left_join(csew_11_sample, lookup2, by = "LAD17CD")

#combine City of London Police and Metropolitan Police
csew_11_sample <- csew_11_sample %>%
  mutate(PFA17CD = ifelse(PFA17CD == 'E23000034', 'E23000001', PFA17CD))

#check sample size in each PFA and calculate target sample size in each stratum
csew_PFA <- csew_11_sample %>%
  group_by(PFA17CD) %>%
  summarise(sample = sum(sample)) %>%
  mutate(sample.A = round(sample/3),
         sample.B = round(sample/3),
         sample.C = round(sample/3),
         sample.C = ifelse(test = sample.A + sample.B + sample.C == sample, yes = sample.C, no = sample.C+1),
         sample.C = ifelse(test = sample.A + sample.B + sample.C == sample, yes = sample.C, no = sample.C-2))

#add target sample size (based on CSEW samples) to PFA information
MSOA_PFA <- left_join(MSOA_PFA, csew_PFA, by = "PFA17CD")

#we know that we want to select 32 addresses in each sampled MSOA in stata B
#we know that we want to select 16 addresses in each of the 2 LSOAs samples in each sampled MSOA in strata C

#calculate number of MSOAs to be sampled in each area in strata B and strata C in each PFA
MSOA_PFA <- MSOA_PFA %>%
  mutate(MSOA.B = round(sample.B/32),
         MSOA.C = round(sample.C/(16*2)))

#split MSOAs in three strata based on population size
D <- 42
pop_MSOA_D <- data.frame()
for(d in 1:D) {
  
  print(d)
  
  #select units in area d
  pop_MSOA_d <- pop_MSOA %>%
    filter(PFA17CD == pop_PFA$PFA17CD[d])
  
  #split MSOAs in three strata based on population size
  pop_MSOA_d <- pop_MSOA_d %>%
    cbind(stratum = c(rep(1, MSOA_PFA$stratum.A[d]), rep(2, MSOA_PFA$stratum.B[d]), rep(3, MSOA_PFA$stratum.C[d])))
  
  #copy in main dataset of MSOAs
  pop_MSOA_D <- rbind(pop_MSOA_D, pop_MSOA_d)
  
}

#keep file as pop_MSOA
pop_MSOA <- pop_MSOA_D

#select MSOAs in stratum B
pop_MSOA.B <- pop_MSOA %>%
  filter(stratum == 2)

#select random sample of MSOAs for stratum B
sample.B <- strata(pop_MSOA.B, "PFA17CD", size = MSOA_PFA$MSOA.B,
                   method = "srswor", description = T)

#get random sample of MSOAs for stratum B
sample_MSOA.B <- getdata(pop_MSOA.B, sample.B)

#select MSOAs in stratum C
pop_MSOA.C <- pop_MSOA %>%
  filter(stratum == 3)

#select random sample of MSOAs for stratum C
sample.C <- strata(pop_MSOA.B, "PFA17CD", size = MSOA_PFA$MSOA.B,
                   method = "srswor", description = T)

#get random sample of MSOAs for stratum C
sample_MSOA.C <- getdata(pop_MSOA.C, sample.C)

#load OA to LAD lookup
lookup <- read.csv(here("data", "Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District__December_2017__Lookup_with_Area_Classifications_in_Great_Britain.csv"))

#select variables of interest in lookup
lookup <- lookup %>%
  select(LSOA11CD, MSOA11CD) %>%
  unique()

#merge lookup with random sample of MSOAs in stratum C
LSOAs.C <- lookup %>%
  left_join(sample_MSOA.C, by = "MSOA11CD") %>%
  filter(!is.na(pop))

#select random sample of LSOAs for each randomly selected MSOA in stratum C
sample.C <- strata(LSOAs.C, "MSOA11CD", size = rep(2, nrow(sample.C)),
                   method = "srswor", description = T)

#get random sample of LSOAs for stratum C
sample_LSOA.C <- getdata(LSOAs.C, sample.C)

#remove objects
rm(list=c("pop_MSOA", "pop_MSOA_d", "pop_MSOA_D", "pop_MSOA.B", "pop_MSOA.C", "pop_PFA",
          "sample_MSOA.C", "sample.B", "sample.C", "LSOAs.C", "csew_11_sample",
          "lookup", "lookup2", "MSOA_PFA", "d", "D"))

#combine City of London Police and Metropolitan Police
syn_res_OA <- syn_res_OA %>%
  mutate(PFA17CD = ifelse(PFA17CD == 'E23000034', 'E23000001', PFA17CD))

#select random sample within each PFA in stratum A
sample.A <- strata(syn_res_OA, "PFA17CD", size = csew_PFA$sample.A,
                   method = "srswor", description = T)

#select random sample within each MSOA selected randomly within each PFA in stratum B
sample.B <- strata(syn_res_OA[syn_res_OA$MSOA11CD %in% sample_MSOA.B$MSOA11CD,], "MSOA11CD", 
                   size = rep(32, nrow(sample_MSOA.B)),
                   method = "srswor", description = T)

#select random sample within each LSOA selected randomly within each PFA in stratum C
sample.C <- strata(syn_res_OA[syn_res_OA$LSOA11CD %in% sample_LSOA.C$LSOA11CD,], "LSOA11CD", 
                   size = rep(16, nrow(sample_LSOA.C)),
                   method = "srswor", description = T)

#get random sample for stratum A
sample_units.A <- getdata(syn_res_OA, sample.A)

#get random sample for stratum B
sample_units.B <- getdata(syn_res_OA[syn_res_OA$MSOA11CD %in% sample_MSOA.B$MSOA11CD,], 
                          sample.B)

#get random sample for stratum C
sample_units.C <- getdata(syn_res_OA[syn_res_OA$LSOA11CD %in% sample_LSOA.C$LSOA11CD,], 
                          sample.C)

#merge three random samples
sample <- rbind(sample_units.A, sample_units.B, sample_units.C)

#remove objects
rm(list=c("csew_PFA", "sample_LSOA.C", "sample_MSOA.B", "sample_units.A", "sample.B",
          "sample.C", "sample_units.B", "sample_units.C", "sample.A", "syn_res_OA"))

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

#select synthetic population sample as in the CSEW
syn_sample_OA <- syn_res_OA %>%
  filter(ID %in% sample$ID)

#save synthetic survey sample as RData
save(syn_sample_OA, file = here("data", "synthetic_survey_crimes.RData"))
