################################
#
# Simulating crime data
#
# Exemplar application: LISA
#
################################

rm(list=ls())

#set seed
set.seed(1234)

#load packages
library(dplyr)
library(here)
library(sf)
library(ggplot2)
library(sp)
library(spdep)
library(tmap)

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

#aggregate synthetic crimes in OAs
syn_res_by_OA <- syn_res_OA %>%
  group_by(OA11CD) %>%
  summarise(violence  = sum(violence),
            theft     = sum(theft),
            damage    = sum(damage))

#load OA to LAD lookup
lookup <- read.csv(here("data", "Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District__December_2017__Lookup_with_Area_Classifications_in_Great_Britain.csv"))

#select variables of interest in lookup
lookup <- lookup %>%
  select(1, 10) %>%
  rename(OA11CD = 1)

#merge lookup
syn_res_by_OA <- syn_res_by_OA %>%
  left_join(lookup, by = "OA11CD")

#count crimes in LADs
syn_res_by_LAD <- syn_res_by_OA %>%
  group_by(LAD17CD) %>%
  summarise(theft.t    = sum(theft),
            violence.t = sum(violence),
            damage.t   = sum(damage))

#remove objects
rm(list=c("syn_res_by_OA"))

#load synthetic police data
load(here("data", "synthetic_police_crimes.Rdata"))

#aggregate synthetic police counts in OAs
syn_pol_by_OA <- Data_crimes %>%
  group_by(OA11CD) %>%
  summarise(violence.p  = sum(violence),
            theft.p     = sum(theft),
            damage.p    = sum(damage))

#merge lookup
syn_pol_by_OA <- syn_pol_by_OA %>%
  left_join(lookup, by = "OA11CD")

#count crimes in LADs
syn_pol_by_LAD <- syn_pol_by_OA %>%
  group_by(LAD17CD) %>%
  summarise(theft.p    = sum(theft.p),
            violence.p = sum(violence.p),
            damage.p   = sum(damage.p))

#remove objects
rm(list=c("Data_crimes", "syn_pol_by_OA"))

#load synthetic survey data
load(here("data", "synthetic_survey_crimes.Rdata"))

#aggregate synthetic police counts in OAs
syn_sample_by_OA <- syn_sample_OA %>%
  group_by(OA11CD) %>%
  summarise(violence.s  = sum(violence),
            theft.s     = sum(theft),
            damage.s    = sum(damage))

#merge lookup
syn_sample_by_OA <- syn_sample_by_OA %>%
  left_join(lookup, by = "OA11CD")

#count crimes in LADs
syn_sample_by_LAD <- syn_sample_by_OA %>%
  group_by(LAD17CD) %>%
  summarise(theft.s    = sum(theft.s),
            violence.s = sum(violence.s),
            damage.s   = sum(damage.s))

#remove objects
rm(list=c("syn_sample_OA", "lookup", "syn_sample_by_OA"))

#merge three files
LAD_info <- syn_res_by_LAD %>%
  left_join(syn_pol_by_LAD,    by = "LAD17CD") %>%
  left_join(syn_sample_by_LAD, by = "LAD17CD") %>%
  replace(is.na(.), 0)

#load shapefile of LADs
LADs <- st_read(here("shapefile", "Local_Authority_Districts__December_2017__Boundaries_in_Great_Britain.shp"))

#merge shapefile
LAD_info <- LADs %>%
  left_join(LAD_info, by = c("lad17cd" = "LAD17CD")) %>%
  filter(!is.na(theft.t))

#remove objects
rm(list=c("LADs", "syn_pol_by_LAD", "syn_res_by_LAD", "syn_sample_by_LAD"))

#map distributions
t <- ggplot(data = LAD_info)+
  ggtitle("Total violence")+
  geom_sf(aes(fill = violence.t, geometry = geometry), color = NA)+
  theme_void()

p <- ggplot(data = LAD_info)+
  ggtitle("Police-recorded violence")+
  geom_sf(aes(fill = violence.p, geometry = geometry), color = NA)+
  theme_void()

s <- ggplot(data = LAD_info)+
  ggtitle("Survey-recorded violence")+
  geom_sf(aes(fill = violence.s, geometry = geometry), color = NA)+
  theme_void()

# plot three maps
ggpubr::ggarrange(t, p, s, nrow = 1)

#convert map into sp object
LAD_info <- as_Spatial(LAD_info)

#save lad code as character
LAD_info$lad17cd <- as.character(LAD_info$lad17cd)

#identify neighbours in map
w <- poly2nb(LAD_info, row.names = LAD_info$lad17cd)

#plot boundaries
plot(LAD_info, col = 'gray', border = 'blue', lwd = 0.1)
xy <- coordinates(LAD_info)
plot(w, xy, col = 'red', lwd = 1, add = TRUE)

#create spatial weights matrix
ww <-  nb2listw(w, style = 'W', zero.policy = TRUE)

#calculate Moran's I
moran(LAD_info$violence.t, ww, n = length(ww$neighbours), S0 = Szero(ww), 
      zero.policy = TRUE)
moran.mc(LAD_info$violence.t, ww, nsim = 99, zero.policy = TRUE)

moran(LAD_info$violence.p, ww, n = length(ww$neighbours), S0 = Szero(ww), 
      zero.policy = TRUE)
moran.mc(LAD_info$violence.p, ww, nsim = 99, zero.policy = TRUE)

moran(LAD_info$violence.s, ww, n = length(ww$neighbours), S0 = Szero(ww), 
      zero.policy = TRUE)
moran.mc(LAD_info$violence.s, ww, nsim = 99, zero.policy = TRUE)

#calculate local Moran
locm_bm.t <- localmoran(LAD_info$violence.t, ww, zero.policy = TRUE)
summary(locm_bm.t)

locm_bm.p <- localmoran(LAD_info$violence.p, ww, zero.policy = TRUE)
summary(locm_bm.p)

locm_bm.s <- localmoran(LAD_info$violence.s, ww, zero.policy = TRUE)
summary(locm_bm.s)

#scale variables of interest
LAD_info$s_violence.t <- scale(LAD_info$violence.t) %>% as.vector()
LAD_info$s_violence.p <- scale(LAD_info$violence.p) %>% as.vector()
LAD_info$s_violence.s <- scale(LAD_info$violence.s) %>% as.vector()

#create a spatial lag variable
LAD_info$lag_s_violence.t <- lag.listw(ww, LAD_info$s_violence.t, zero.policy = TRUE)
LAD_info$lag_s_violence.p <- lag.listw(ww, LAD_info$s_violence.p, zero.policy = TRUE)
LAD_info$lag_s_violence.s <- lag.listw(ww, LAD_info$s_violence.s, zero.policy = TRUE)

#check new variables
summary(LAD_info$s_violence.t)
summary(LAD_info$s_violence.p)
summary(LAD_info$s_violence.s)

summary(LAD_info$lag_s_violence.t)
summary(LAD_info$lag_s_violence.p)
summary(LAD_info$lag_s_violence.s)

#create groups according to p-values
LAD_info.t <- st_as_sf(LAD_info) %>% 
  mutate(quad_sig = ifelse(LAD_info$s_violence.t > 0 & 
                             LAD_info$lag_s_violence.t > 0 & 
                             locm_bm.t[,5] <= 0.05, 
                           "high-high",
                           ifelse(LAD_info$s_violence.t <= 0 & 
                                    LAD_info$lag_s_violence.t <= 0 & 
                                    locm_bm.t[,5] <= 0.05, 
                                  "low-low", 
                                  ifelse(LAD_info$s_violence.t > 0 & 
                                           LAD_info$lag_s_violence.t <= 0 & 
                                           locm_bm.t[,5] <= 0.05, 
                                         "high-low",
                                         ifelse(LAD_info$s_violence.t <= 0 & 
                                                  LAD_info$lag_s_violence.t > 0 & 
                                                  locm_bm.t[,5] <= 0.05,
                                                "low-high", 
                                                "non-significant")))))

LAD_info.p <- st_as_sf(LAD_info) %>% 
  mutate(quad_sig = ifelse(LAD_info$s_violence.p > 0 & 
                             LAD_info$lag_s_violence.p > 0 & 
                             locm_bm.p[,5] <= 0.05, 
                           "high-high",
                           ifelse(LAD_info$s_violence.p <= 0 & 
                                    LAD_info$lag_s_violence.p <= 0 & 
                                    locm_bm.p[,5] <= 0.05, 
                                  "low-low", 
                                  ifelse(LAD_info$s_violence.p > 0 & 
                                           LAD_info$lag_s_violence.p <= 0 & 
                                           locm_bm.p[,5] <= 0.05, 
                                         "high-low",
                                         ifelse(LAD_info$s_violence.p <= 0 & 
                                                  LAD_info$lag_s_violence.p > 0 & 
                                                  locm_bm.p[,5] <= 0.05,
                                                "low-high", 
                                                "non-significant")))))

LAD_info.s <- st_as_sf(LAD_info) %>% 
  mutate(quad_sig = ifelse(LAD_info$s_violence.s > 0 & 
                             LAD_info$lag_s_violence.s > 0 & 
                             locm_bm.s[,5] <= 0.05, 
                           "high-high",
                           ifelse(LAD_info$s_violence.s <= 0 & 
                                    LAD_info$lag_s_violence.s <= 0 & 
                                    locm_bm.s[,5] <= 0.05, 
                                  "low-low", 
                                  ifelse(LAD_info$s_violence.s > 0 & 
                                           LAD_info$lag_s_violence.s <= 0 & 
                                           locm_bm.s[,5] <= 0.05, 
                                         "high-low",
                                         ifelse(LAD_info$s_violence.s <= 0 & 
                                                  LAD_info$lag_s_violence.s > 0 & 
                                                  locm_bm.s[,5] <= 0.05,
                                                "low-high", 
                                                "non-significant")))))

#check results
table(LAD_info.t$quad_sig)
table(LAD_info.p$quad_sig)
table(LAD_info.s$quad_sig)

#map results
t <- tm_shape(LAD_info.t) +
  tm_borders("grey51", alpha = 0) +
  tm_polygons("quad_sig", title = "LISA All crime",
              palette = c('high-high' = '#CC6677', 'non-significant' = '#6699CC', 
                          'low-low' = '#999933', 'Missing' = '#888888')) +
  tm_layout(frame = FALSE)

p <- tm_shape(LAD_info.p) +
  tm_borders("grey51", alpha = 0) +
  tm_polygons("quad_sig", title = "LISA Police data",
              palette = c('high-high' = '#CC6677', 'non-significant' = '#6699CC', 
                          'low-low' = '#999933', 'Missing' = '#888888')) +
  tm_layout(frame = FALSE)

s <- tm_shape(LAD_info.s) +
  tm_borders("grey51", alpha = 0) +
  tm_polygons("quad_sig", title = "LISA Survey data",
              palette = c('high-high' = '#CC6677', 'non-significant' = '#6699CC', 
                          'low-low' = '#999933', 'Missing' = '#888888')) +
  tm_layout(frame = FALSE)

# plot three maps
tmap_arrange(t, p, s, nrow = 1)
