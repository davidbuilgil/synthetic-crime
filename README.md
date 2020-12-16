# synthetic-crime

Authors: David Buil-Gil, Ian Brunton-Smith, Jose Pina-Sánchez, Alexandru Cernat

**Code and data to generate a synthetic dataset of crimes in England and Wales**

The following steps are followed to generate a synthetic dataset of crimes in England and Wales:

1.	Download Census data aggregates at the Output Area level under the [Open Government Licence](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)
2.  Download microdata of the Crime Survey for England and Wales (CSEW) 2011/12
3.	Generate a synthetic population of residents in Output Areas based on empirical parameters observed in Census data and covariance matrix observed in CSEW
4.	Based on parameters from the CSEW 2011/12, generate crimes (violence, property crime and damage) suffered by each unit in the synthetic population
5.	Based on parameters from the CSEW 2011/12, predict if each crime generated in Step 4 is known to, and recorded by, the police or not (this will be the synthetic dataset of police-recorded crimes)
6.	Draw a random sample of units from the synthetic population following sampling design of the CSEW (this will be the synthetic dataset of crimes recorded by the CSEW)

This generates three sets of synthetic crime data, which can be then compared at the different spatial scales:

1.	*'synthetic_population_crimes.RData'*: synthetic data of all crime (Generated in Step 4)
2.	*'synthetic_police_crimes.RData'*: synthetic data of police-recorded crime (Generated in Step 5)
3.	*'synthetic_survey_crimes.RData'*: synthetic data of police-recorded crime (Generated in Step 6)
With these data, we can:
1.	Try the MTMM at the level of crimes, individuals, and as many levels of geography as we wish (output areas, LSOAs, MSOAs, wards, local authorities, CSP, PFA, regions)
2.	Using the synthetic CSEW data, try different SAE approaches and see which one gets estimates closer to the empirical values

_Data download guide_

All data used in this project is either available under a [Open Government Licence](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) or simulated within the R environment:

1.  Census data aggregates at the Output Area level are downloaded from the [2011 Census Bulk Data Download](https://www.nomisweb.co.uk/census/2011/bulk/r2_2) of NOMIS. WE download the files for Output Areas:
a) *KS601EW - Economic Activity*
b) *KS501EW - Qualifications and Students*
c) *KS201EW - Ethnic Group*
d) *KS101EW - Usual Resident Population*
e) *QS103EW	- Age by Single Year*
f) *KS103EW	- Marital and Civil Partnership Status*
g) *KS204EW	- Country of Birth*
2.  CSEW 2011/12 data are downloaded from the [UK Data Service](https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=7252).
3.  Lookup tables from the Output Area level to Local Authority District and from Local Authority District to Police Force Area are downloaded from the [Open Geography portal from the Office for National Statistics](https://geoportal.statistics.gov.uk/).
4.  Police recorded data at the Community Safety Partnership level are downloaded from the ['Police recorded crime and outcomes open data tables' page](https://www.gov.uk/government/statistics/police-recorded-crime-open-data-tables) of GOV.UK.

_Script run guide_

All scripts should be ran from within the 'synthetic-crime.Rproj' R project file.

1.  The R script file named *'Part 1 Prepare Census data'* is used to wrangle census data into the appropriate format that will be used in the main scripts.
2.  The R script file named *'Part 2 a Generate synthetic UK population'* is used to generate a synthetic population of residents in England and Wales based on empirical parameters observed in Census data and covariance matrix observed in CSEW
3.  The R script file named *'Part 3 Generate crimes in units'* is used to generate crime victimisation in units based on parameters obtained from the CSEW.  
4.  The R script file named *'Part 4 Select sample as in CSEW'* is used to select a random sample of units from the synthetic population following sampling design of the CSEW.
5.  The R script file named *'Part 5 Select crimes known to police'* is used to generate whether each crime is known to the police or not, and select the subset of synthetic police-recorded crimes.
6.  The R script file named *'Part 6 Empirical evaluation'* is used to evaluate the results of the simulation study.
