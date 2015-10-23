##################################################
#  Data Cleaning Script
##################################################

library(readxl)
library(stringr)
library(codemog)
library(dplyr)
library(car)

rucc=read_excel("ruralurbancodes2013.xls")%>%
  mutate(countyfips=as.numeric(str_sub(FIPS, -3,-1)),
         state=as.numeric(str_sub(FIPS, 1,2)))%>%
  filter(state==8)%>%
  mutate(rucc_short=recode(RUCC_2013, "1:3='Metro'; 4='Rural Adjacent'; 5='Rural Not Adjacent'; 6='Rural Adjacent';
                              7='Rural Not Adjacent'; 8='Rural Adjacent'; 9='Rural Not Adjacent'"))

econdep=read_excel("all_final_codes.xls", sheet=2)%>%
  mutate(countyfips=as.numeric(str_sub(FIPSTXT, -3,-1)),
         state=as.numeric(str_sub(FIPSTXT, 1,2)),
         econdep_factor=ordered(econdep, levels=1:6, 
                                labels=c("Farming", "Mining", "Manufacturing", "Government", "Services", "Nonspecialized")))%>%
  filter(state==8)

projregs=readr::read_csv("projregs.csv")%>%
  mutate(RegionName=ifelse(RegionName=="Planning Region 12", "Mountain Resorts", RegionName),
         RegionName=ifelse(RegionName=="Eastern Mountains", "Central Mountains", RegionName))

regfips2=readr::read_csv("regfips2.csv")%>%
  rename(countyfips=FIPS)

codemog_pal=c(rgb(31,73,125, max=255),
              rgb(191,32,38, max=255),
              rgb(216, 199, 34, max=255),
              rgb(239, 117, 33, max=255),
              rgb(130, 188, 0, max = 255),
              rgb(110, 196, 232, max = 255),
              rgb(67, 0, 152, max = 255))

data=county_profile%>%
  select(countyfips,year,householdPopulation, groupQuartersPopulation, netMigration)%>%
  mutate(totalPopulation=householdPopulation+groupQuartersPopulation,
         totalPopulation=ifelse(is.na(totalPopulation), 0, totalPopulation),
         netMigration=ifelse(is.na(netMigration), 0, netMigration),
         netMigrationRate=netMigration/totalPopulation)%>%
  inner_join(rucc)%>%
  inner_join(econdep)%>%
  inner_join(regfips2)%>%
  inner_join(projregs)%>%
  select(RegionNumber, RegionName, countyfips, county=County_Name, year, totalPopulation, netMigration, netMigrationRate, 
         RUCC_2013,rucc_short, rucc_desc=Description, econdep, econdep_factor)
