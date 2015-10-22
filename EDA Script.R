###########################
#  EDA Script
##########################

library(ggplot2)

source("Data Cleaning Script 1.R")


##### RUCC Changes over time

ts=data%>%
  group_by(rucc_short, year)%>%
  mutate(totalPopulation=ifelse(is.na(totalPopulation), 0, totalPopulation),
         netMigration=ifelse(is.na(netMigration), 0, netMigration))%>%
  summarize(totalPopulation=sum(totalPopulation),
            netMigration=sum(netMigration))%>%
  mutate(netMigrationRate=netMigration/totalPopulation)

p1=ts%>%
#   filter(year>2000)%>%
  ggplot( aes(x=year, y=netMigrationRate, color=rucc_short))+
  geom_line()
p1