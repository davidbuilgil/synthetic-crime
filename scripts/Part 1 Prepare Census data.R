################################
#
# Simulating crime data
#
# Part I: Prepare Census data
#
################################

rm(list=ls())

#load dplyr package
library(dplyr)
library(here)

## Income

# load income data we need to replicate.
Income_by_OA <- read.csv(here("data", "KS601EWDATA.csv"))

# load list of output areas.
OA <- read.csv(here("data", "SAM_OA_DEC_2011_EW.csv"))

# remove all geographies that are not OAs.
Income_by_OA <- Income_by_OA %>%
  rename(OA11CD = GeographyCode) %>%
  semi_join(OA, by = "OA11CD")

# renaming, dropping missings pop and arranging OAs
income_mean <-  Income_by_OA %>% 
  rename(Pop_econ = `KS601EW0001`,
         unempl = `KS601EW0005`,
         student = `KS601EW0006`,
         inactive1 = `KS601EW0007`,
         inactive2 = `KS601EW0008`,
         inactive3 = `KS601EW0009`,
         inactive4 = `KS601EW0010`,
         inactive5 = `KS601EW0011`) %>% 
  mutate(sum_no_income = unempl + student + inactive1 + inactive2 +
           inactive3 + inactive4 + inactive5,
         mean_no_income = sum_no_income/Pop_econ) %>%
  select(OA11CD, Pop_econ, sum_no_income, mean_no_income)

# save
write.csv(income_mean, here("data", "Income_by_OA_replicate.csv"))

rm(list=c("Income_by_OA", "income_mean"))

## Higher education

# load educ data we need to replicate. Note that we actually only need the 'mean' level 4 education measure,
# so many of the other columns are not required.
Edu_by_OA <- read.csv(here("data", "KS501EWDATA.csv"))

# remove all geographies that are not OAs.
Edu_by_OA <- Edu_by_OA %>%
  rename(OA11CD = GeographyCode) %>%
  semi_join(OA, by = "OA11CD")

# renaming, drop missings, then create 'mean' measure
educ_mean <- Edu_by_OA %>% 
  rename(Pop_educ = `KS501EW0001`,
         Level4_educ = `KS501EW0007`) %>% 
  mutate(Mean_level4_edu = Level4_educ/Pop_educ) %>%
  select(OA11CD, Pop_educ, Level4_educ, Mean_level4_edu)

# save
write.csv(educ_mean, here("data", "Edu_by_OA_replicate.csv"))

rm(list=c("Edu_by_OA", "educ_mean"))

## Ethnicity

# load ethnicity data we need to replicate. Note that we actually only need the white measure
Ethnic_by_OA <- read.csv(here("data", "KS201EWDATA.csv"))

# remove all geographies that are not OAs.
Ethnic_by_OA <- Ethnic_by_OA %>%
  rename(OA11CD = GeographyCode) %>%
  semi_join(OA, by = "OA11CD")

# renaming, drop missings, then create 'mean' measure
ethnic_mean <- Ethnic_by_OA %>% 
  rename(Pop_ethnic = `KS201EW0001`,
         White1 = `KS201EW0002`,
         White2 = `KS201EW0003`,
         White3 = `KS201EW0004`,
         White4 = `KS201EW0005`) %>% 
  mutate(White = White1 + White2 + White3 + White4,
         Mean_white = White/Pop_ethnic) %>%
  select(OA11CD, Pop_ethnic, White, Mean_white)

# save
write.csv(ethnic_mean, here("data", "Ethnic_by_OA_replicate.csv"))

rm(list=c("Ethnic_by_OA", "ethnic_mean"))

## Sex

# load sex data we need to replicate.
Sex_by_OA <- read.csv(here("data", "KS101EWDATA.csv"))

# remove all geographies that are not OAs.
Sex_by_OA <- Sex_by_OA %>%
  rename(OA11CD = GeographyCode) %>%
  semi_join(OA, by = "OA11CD")

# renaming, drop missings, then create 'mean' measure
sex_mean <- Sex_by_OA %>% 
  rename(Pop_sex = `KS101EW0001`,
         Male = `KS101EW0002`) %>% 
  mutate(Mean_male = Male/Pop_sex) %>%
  select(OA11CD, Pop_sex, Male, Mean_male)

# save
write.csv(sex_mean, here("data", "Sex_by_OA_replicate.csv"))

rm(list=c("Sex_by_OA", "sex_mean"))

## Age

# load sex data we need to replicate.
Age_by_OA <- read.csv(here("data", "QS103EWDATA.csv"))

# remove all geographies that are not OAs.
Age_by_OA <- Age_by_OA %>%
  rename(OA11CD = GeographyCode,
         Pop = QS103EW0001) %>%
  semi_join(OA, by = "OA11CD")

# compute mean age and SD of age in each OA.
age_mean <- Age_by_OA %>%
  mutate(sum_age = QS103EW0002*0 + QS103EW0003*1 + QS103EW0004*2 + QS103EW0005*3 +
           QS103EW0006*4 + QS103EW0007*5 + QS103EW0008*6 + QS103EW0009*7 +
           QS103EW0010*8 + QS103EW0011*9 + QS103EW0012*10 + QS103EW0013*11 +
           QS103EW0014*12 + QS103EW0015*13 + QS103EW0016*14 + QS103EW0017*15 +
           QS103EW0018*16 + QS103EW0019*17 + QS103EW0020*18 + QS103EW0021*19 +
           QS103EW0022*20 + QS103EW0023*21 + QS103EW0024*22 + QS103EW0025*23 +
           QS103EW0026*24 + QS103EW0027*25 + QS103EW0028*26 + QS103EW0029*27 +
           QS103EW0030*28 + QS103EW0031*29 + QS103EW0032*30 + QS103EW0033*31 +
           QS103EW0034*32 + QS103EW0035*33 + QS103EW0036*34 + QS103EW0037*35 +
           QS103EW0038*36 + QS103EW0039*37 + QS103EW0040*38 + QS103EW0041*39 +
           QS103EW0042*40 + QS103EW0043*41 + QS103EW0044*42 + QS103EW0045*43 +
           QS103EW0046*44 + QS103EW0047*45 + QS103EW0048*46 + QS103EW0049*47 +
           QS103EW0050*48 + QS103EW0051*49 + QS103EW0052*50 + QS103EW0053*51 +
           QS103EW0054*52 + QS103EW0055*53 + QS103EW0056*54 + QS103EW0057*55 +
           QS103EW0058*56 + QS103EW0059*57 + QS103EW0060*58 + QS103EW0061*59 +
           QS103EW0062*60 + QS103EW0063*61 + QS103EW0064*62 + QS103EW0065*63 +
           QS103EW0066*64 + QS103EW0067*65 + QS103EW0068*66 + QS103EW0069*67 +
           QS103EW0070*68 + QS103EW0071*69 + QS103EW0072*70 + QS103EW0073*71 +
           QS103EW0074*72 + QS103EW0075*73 + QS103EW0076*74 + QS103EW0077*75 +
           QS103EW0078*76 + QS103EW0079*77 + QS103EW0080*78 + QS103EW0081*79 +
           QS103EW0082*80 + QS103EW0083*81 + QS103EW0084*82 + QS103EW0085*83 +
           QS103EW0086*84 + QS103EW0087*85 + QS103EW0088*86 + QS103EW0089*87 +
           QS103EW0090*88 + QS103EW0091*89 + QS103EW0092*90 + QS103EW0093*91 +
           QS103EW0094*92 + QS103EW0095*93 + QS103EW0096*94 + QS103EW0097*95 +
           QS103EW0098*96 + QS103EW0099*97 + QS103EW0100*98 + QS103EW0101*99 +
           QS103EW0102*101,
         mean_age = sum_age/Pop,
         dis_age  = (QS103EW0002*(0-mean_age)^2) + QS103EW0003*(1-mean_age)^2 + QS103EW0004*(2-mean_age)^2 +
           QS103EW0005*(3-mean_age)^2 + QS103EW0006*(4-mean_age)^2 + QS103EW0007*(5-mean_age)^2 +
           QS103EW0008*(6-mean_age)^2 + QS103EW0009*(7-mean_age)^2 + QS103EW0010*(8-mean_age)^2 +
           QS103EW0011*(9-mean_age)^2 + QS103EW0012*(10-mean_age)^2 + QS103EW0013*(11-mean_age)^2 +
           QS103EW0014*(12-mean_age)^2 + QS103EW0015*(13-mean_age)^2 + QS103EW0016*(14-mean_age)^2 +
           QS103EW0017*(15-mean_age)^2 + QS103EW0018*(16-mean_age)^2 + QS103EW0019*(17-mean_age)^2 +
           QS103EW0020*(18-mean_age)^2 + QS103EW0021*(19-mean_age)^2 + QS103EW0022*(20-mean_age)^2 +
           QS103EW0023*(21-mean_age)^2 + QS103EW0025*(22-mean_age)^2 + QS103EW0025*(23-mean_age)^2 +
           QS103EW0026*(24-mean_age)^2 + QS103EW0027*(25-mean_age)^2 + QS103EW0028*(26-mean_age)^2 +
           QS103EW0029*(27-mean_age)^2 + QS103EW0030*(28-mean_age)^2 + QS103EW0031*(29-mean_age)^2 +
           QS103EW0032*(30-mean_age)^2 + QS103EW0033*(31-mean_age)^2 + QS103EW0033*(32-mean_age)^2 +
           QS103EW0035*(33-mean_age)^2 + QS103EW0036*(34-mean_age)^2 + QS103EW0037*(35-mean_age)^2 +
           QS103EW0038*(36-mean_age)^2 + QS103EW0038*(37-mean_age)^2 + QS103EW0040*(38-mean_age)^2 +
           QS103EW0041*(39-mean_age)^2 + QS103EW0042*(40-mean_age)^2 + QS103EW0043*(41-mean_age)^2 +
           QS103EW0044*(42-mean_age)^2 + QS103EW0045*(43-mean_age)^2 + QS103EW0046*(44-mean_age)^2 +
           QS103EW0047*(45-mean_age)^2 + QS103EW0048*(46-mean_age)^2 + QS103EW0049*(47-mean_age)^2 +
           QS103EW0050*(48-mean_age)^2 + QS103EW0051*(49-mean_age)^2 + QS103EW0052*(50-mean_age)^2 +
           QS103EW0053*(51-mean_age)^2 + QS103EW0054*(52-mean_age)^2 + QS103EW0055*(53-mean_age)^2 +
           QS103EW0056*(54-mean_age)^2 + QS103EW0057*(55-mean_age)^2 + QS103EW0058*(56-mean_age)^2 +
           QS103EW0059*(57-mean_age)^2 + QS103EW0060*(58-mean_age)^2 + QS103EW0061*(59-mean_age)^2 +
           QS103EW0062*(60-mean_age)^2 + QS103EW0063*(61-mean_age)^2 + QS103EW0064*(62-mean_age)^2 +
           QS103EW0065*(63-mean_age)^2 + QS103EW0066*(64-mean_age)^2 + QS103EW0067*(65-mean_age)^2 +
           QS103EW0068*(66-mean_age)^2 + QS103EW0069*(67-mean_age)^2 + QS103EW0070*(68-mean_age)^2 +
           QS103EW0071*(69-mean_age)^2 + QS103EW0072*(70-mean_age)^2 + QS103EW0073*(71-mean_age)^2 +
           QS103EW0074*(72-mean_age)^2 + QS103EW0075*(73-mean_age)^2 + QS103EW0076*(74-mean_age)^2 +
           QS103EW0077*(75-mean_age)^2 + QS103EW0078*(76-mean_age)^2 + QS103EW0079*(77-mean_age)^2 +
           QS103EW0080*(78-mean_age)^2 + QS103EW0081*(79-mean_age)^2 + QS103EW0082*(80-mean_age)^2 +
           QS103EW0083*(81-mean_age)^2 + QS103EW0084*(82-mean_age)^2 + QS103EW0085*(83-mean_age)^2 +
           QS103EW0086*(84-mean_age)^2 + QS103EW0087*(85-mean_age)^2 + QS103EW0088*(86-mean_age)^2 +
           QS103EW0089*(87-mean_age)^2 + QS103EW0090*(88-mean_age)^2 + QS103EW0091*(89-mean_age)^2 +
           QS103EW0092*(90-mean_age)^2 + QS103EW0093*(91-mean_age)^2 + QS103EW0094*(92-mean_age)^2 +
           QS103EW0095*(93-mean_age)^2 + QS103EW0096*(94-mean_age)^2 + QS103EW0097*(95-mean_age)^2 +
           QS103EW0098*(96-mean_age)^2 + QS103EW0099*(97-mean_age)^2 + QS103EW0100*(98-mean_age)^2 +
           QS103EW0101*(99-mean_age)^2 + QS103EW0102*(101-mean_age)^2,
         sd_age = sqrt(dis_age / (Pop-1)),
         age_less20 = QS103EW0002 + QS103EW0003 + QS103EW0004 + QS103EW0005 + QS103EW0006 + QS103EW0007 + 
           QS103EW0008 + QS103EW0009 + QS103EW0010 + QS103EW0011 + QS103EW0012 + QS103EW0013 +
           QS103EW0014 + QS103EW0015 + QS103EW0016 + QS103EW0017 + QS103EW0018 + QS103EW0019 +
           QS103EW0020 + QS103EW0021,
         age_20to35 = QS103EW0022 + QS103EW0023 + QS103EW0024 + QS103EW0025 + QS103EW0026 + QS103EW0027 + 
           QS103EW0028 + QS103EW0029 + QS103EW0030 + QS103EW0031 + QS103EW0032 + QS103EW0033 + 
           QS103EW0034 + QS103EW0035 + QS103EW0036 + QS103EW0037,
         age_35to49 = QS103EW0038 + QS103EW0039 + QS103EW0040 + QS103EW0041 + QS103EW0042 + QS103EW0043 + 
           QS103EW0044 + QS103EW0045 + QS103EW0046 + QS103EW0047 + QS103EW0048 + QS103EW0049 +
           QS103EW0050 + QS103EW0051,
         age_50to65 = QS103EW0052 + QS103EW0053 + QS103EW0054 + QS103EW0055 + QS103EW0056 + QS103EW0057 +
           QS103EW0058 + QS103EW0059 + QS103EW0060 + QS103EW0061 + QS103EW0062 + QS103EW0063 + 
           QS103EW0064 + QS103EW0065 + QS103EW0066 + QS103EW0067,
         age_more65 = QS103EW0068 + QS103EW0069 + QS103EW0070 + QS103EW0071 + QS103EW0072 + QS103EW0073 +
           QS103EW0074 + QS103EW0075 + QS103EW0076 + QS103EW0077 + QS103EW0078 + QS103EW0079 + 
           QS103EW0080 + QS103EW0081 + QS103EW0082 + QS103EW0083 + QS103EW0084 + QS103EW0085 +
           QS103EW0086 + QS103EW0087 + QS103EW0088 + QS103EW0089 + QS103EW0090 + QS103EW0091 +
           QS103EW0092 + QS103EW0093 + QS103EW0094 + QS103EW0095 + QS103EW0096 + QS103EW0097 +
           QS103EW0098 + QS103EW0099 + QS103EW0100 + QS103EW0101 + QS103EW0102) %>%
  select(OA11CD, Pop, mean_age, sd_age, age_less20, age_20to35, age_35to49, age_50to65, age_more65)

# save
write.csv(age_mean, here("data", "Age_by_OA_replicate.csv"))

## Marital status

# load marital status data we need to replicate.
Marital_by_OA <- read.csv(here("data", "KS103EWDATA.csv"))

# remove all geographies that are not OAs.
Marital_by_OA <- Marital_by_OA %>%
  rename(OA11CD = GeographyCode) %>%
  semi_join(OA, by = "OA11CD")

# renaming, dropping missings pop and arranging OAs
marital_mean <-  Marital_by_OA %>% 
  rename(Pop_marit = `KS103EW0001`,
         married = `KS103EW0003`,
         same_sex = `KS103EW0004`) %>% 
  mutate(sum_married = married + same_sex,
         mean_married = sum_married/Pop_marit) %>%
  select(OA11CD, Pop_marit, sum_married, mean_married)

# save
write.csv(marital_mean, here("data", "Married_by_OA_replicate.csv"))

rm(list=c("Marital_by_OA", "marital_mean"))

## Born UK

# load country of birth data we need to replicate.
Country_by_OA <- read.csv(here("data", "KS204EWDATA.csv"))

# remove all geographies that are not OAs.
Country_by_OA <- Country_by_OA %>%
  rename(OA11CD = GeographyCode) %>%
  semi_join(OA, by = "OA11CD")

# renaming, dropping missings pop and arranging OAs
bornuk_mean <-  Country_by_OA %>% 
  rename(Pop_country = `KS204EW0001`,
         england = `KS204EW0002`,
         northireland = `KS204EW0003`,
         scotland = `KS204EW0004`,
         wales = `KS204EW0005`,
         otheruk = `KS204EW0006`) %>% 
  mutate(sum_bornuk = england + northireland + scotland + wales + otheruk,
         mean_bornuk = sum_bornuk/Pop_country) %>%
  select(OA11CD, Pop_country, sum_bornuk, mean_bornuk)

# save
write.csv(bornuk_mean, here("data", "BornUK_by_OA_replicate.csv"))

# Load in SPSS CSEW data (needs to be downloaded from UK Data Service).
csew    <- read_spss(file = here("data","csew_apr11mar12_nvf.sav"))
csew_vf <- read_spss(file = here("data","csew_apr11mar12_vf.sav"))

# Remove haven labels from CSEW data.
csew    <- zap_labels(csew)
csew_vf <- zap_labels(csew_vf)

# Save CSEW as .Rdata for later use.
save(csew   , file = here("data","csew_apr11mar12_nvf.Rdata"))
save(csew_vf, file = here("data","csew_apr11mar12_vf.Rdata"))
