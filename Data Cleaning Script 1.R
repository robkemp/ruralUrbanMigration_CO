##################################################
#  Data Cleaning Script
##################################################

library(readxl)
library(stringr)
library(codemog)
library(dplyr)
library(car)

rucc=read_excel("/Users/robkemp/Desktop/Demography Office R/ruralUrbanMigration_CO/ruralurbancodes2013.xls")%>%
  mutate(countyfips=as.numeric(str_sub(FIPS, -3,-1)),
         state=as.numeric(str_sub(FIPS, 1,2)))%>%
  filter(state==8)

data=county_profile%>%
  select(countyfips,year,householdPopulation, groupQuartersPopulation, netMigration)%>%
  mutate(totalPopulation=householdPopulation+groupQuartersPopulation,
         netMigrationRate=netMigration/totalPopulation)%>%
  inner_join(rucc)%>%
  mutate(rucc_short=recode(RUCC_2013, "1:3='Metro'; 4='Rural Adjacent'; 5='Rural Not Adjacent'; 6='Rural Adjacent';
                              7='Rural Not Adjacent'; 8='Rural Adjacent'; 9='Rural Not Adjacent'"))%>%
  select(countyfips, county=County_Name, year, totalPopulation, netMigration, netMigrationRate, RUCC_2013,rucc_short, Description)
