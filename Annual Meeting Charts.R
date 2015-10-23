##################################
#  Annual Meeting Plots and Data #  
##################################

library(ggplot2)
library(codemog)
library(grid)

source("Data Cleaning Script 1.R")
netMigCluster=readr::read_csv("netMigClusters_AnnualMeeting.csv")
## Plot 1 ##

plot_1=data%>%
  inner_join(netMigCluster)%>%
  filter(year>2004)%>%
  mutate(year=recode(year, "2004:2006=1;2007:2009=2; 2010:2014=3"),
         year=ordered(year, levels=1:3, labels=c("2004 to 2006", "2007 to 2009","2010 to 2014")))%>%
  group_by(netMigClust, year)%>%
  summarize(totalPopulation=sum(totalPopulation),
            netMigration=sum(netMigration))%>%
  mutate(netMigrationRate=(netMigration/totalPopulation)*100)%>%
  ggplot(aes(x=year, y=netMigrationRate, fill=as.factor(netMigClust)))+
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_manual("Cluster",values=codemog_pal)+
  theme_codemog()+
  labs(title="Net Migration Rates by Recession Period, 2004 to 2014", x="Period", y="Net Migration Rate")
plot_1


plot_1=data%>%
  inner_join(netMigCluster)%>%
  filter(year>2004)%>%
  mutate(year=recode(year, "2004:2006=1;2007:2009=2; 2010:2014=3"),
         year=ordered(year, levels=1:3, labels=c("Pre-Recession: 2004 to 2006", "Recession: 2007 to 2009","Recovery: 2010 to 2014")))%>%
  group_by(netMigClust, year)%>%
  summarize(totalPopulation=sum(totalPopulation),
            netMigration=sum(netMigration))%>%
  mutate(netMigrationRate=(netMigration/totalPopulation)*100)%>%
  ggplot(aes(x=reorder(netMigClust,netMigrationRate),y=netMigrationRate, fill=as.factor(netMigClust)))+
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_manual("",values=codemog_pal)+
  theme_codemog()+
  theme(axis.text.x=element_blank())+
  facet_wrap("year")+
  labs(title="Net Migration Rates by Recession Period, 2004 to 2014", x="", y="Net Migration per 100 Population")
plot_1
