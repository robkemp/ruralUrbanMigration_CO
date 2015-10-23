###########################
#  EDA Script
##########################

library(ggplot2)

source("Data Cleaning Script 1.R")



##### RUCC Changes over time

p=data%>%
  group_by(year)%>%
  summarize(totalPopulation=sum(totalPopulation),
            netMigration=sum(netMigration))%>%
  mutate(netMigrationRate=netMigration/totalPopulation)%>%
  ggplot( aes(x=year, y=netMigrationRate))+
  geom_line()
p

p1=data%>%
  group_by(rucc_short, year)%>%
  summarize(totalPopulation=sum(totalPopulation),
            netMigration=sum(netMigration))%>%
  mutate(netMigrationRate=netMigration/totalPopulation)%>%
#   filter(year>2000)%>%
  ggplot( aes(x=year, y=netMigrationRate, color=rucc_short))+
  geom_line()
p1


p2=data%>%
  filter(year>2000)%>%
  group_by(econdep_factor, rucc_short, year)%>%
  summarize(totalPopulation=sum(totalPopulation),
            netMigration=sum(netMigration))%>%
  mutate(netMigrationRate=netMigration/totalPopulation)%>%
  ggplot( aes(x=year, y=netMigrationRate, color=econdep_factor))+
  geom_line()+
  facet_wrap("rucc_short")
p2


p3=data%>%
  filter(year>2001)%>%
  mutate(year=recode(year, "2001:2006=1;2007:2009=2; 2010:2014=3"))%>%
  group_by(rucc_short, year)%>%
  summarize(totalPopulation=sum(totalPopulation),
            netMigration=sum(netMigration))%>%
  mutate(netMigrationRate=netMigration/totalPopulation)%>%
  ggplot( aes(x=year, y=netMigrationRate, fill=rucc_short))+
  geom_bar(stat="identity", position = "dodge")
p3

p4=data%>%
  filter(year>2001)%>%
  mutate(year=recode(year, "2001:2006=1;2007:2009=2; 2010:2014=3"))%>%
  group_by(econdep_factor, year)%>%
  summarize(totalPopulation=sum(totalPopulation),
            netMigration=sum(netMigration))%>%
  mutate(netMigrationRate=netMigration/totalPopulation)%>%
  ggplot( aes(x=year, y=netMigrationRate, fill=econdep_factor))+
  geom_bar(stat="identity", position = "dodge")
p4

p5=data%>%
  filter(year>2001)%>%
  mutate(year=recode(year, "2001:2006=1;2007:2009=2; 2010:2014=3"))%>%
  group_by(RegionName, year)%>%
  summarize(totalPopulation=sum(totalPopulation),
            netMigration=sum(netMigration))%>%
  mutate(netMigrationRate=netMigration/totalPopulation)%>%
  ggplot( aes(x=year, y=netMigrationRate, fill=RegionName))+
  geom_bar(stat="identity", position = "dodge")
p5


#####################################
#   Cluster Analysis
#####################################

#creates a subset of the data to be numeric only
clust=data%>%
  inner_join(filter(county_jobs, sector_id==0)%>%select(countyfips, year, totalJobs=jobs))%>%
  filter(year>=2010)%>%
  select(countyfips,netMigration, totalPopulation, totalJobs)%>%
  group_by(countyfips)%>%
  summarize(netMigration=sum(netMigration),
            totalPopulation=sum(totalPopulation),
            totalJobs=sum(as.numeric(totalJobs)))%>%
  mutate(netMigrationRate=netMigration/totalPopulation)%>%
  select(netMigrationRate, totalJobs)
# clust=scale(clust)
#Pick number of groups...5 for now
# wss <- (nrow(clust)-1)*sum(apply(clust,2,var))
# for (i in 2:15) wss[i] <- sum(kmeans(clust, 
#                                      centers=i)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")

# Fit a K-Means Clustering Model using 
#     fit=kmeans(clust, 5)
#check the means of each variable and group
aggregate(clust,by=list(fit$cluster),FUN=mean)

netMigClust=data%>%
  filter(year>=2010)%>%
  select(countyfips,netMigration, totalPopulation)%>%
  group_by(countyfips)%>%
  summarize(netMigration=sum(netMigration),
            totalPopulation=sum(totalPopulation))%>%
  mutate(netMigrationRate=netMigration/totalPopulation)%>%
  bind_cols(data.frame(netMigClust=fit$cluster))%>%
  select(countyfips, netMigClust)
p6=data%>%
  inner_join(netMigClust_favorite2)%>%
  filter(year>2004)%>%
  mutate(year=recode(year, "2004:2006=1;2007:2009=2; 2010:2014=3"),
         year=ordered(year, levels=1:3, labels=c("2004:2006", "2007:2009","2010:2014")))%>%
  group_by(netMigClust, year)%>%
  summarize(totalPopulation=sum(totalPopulation),
            netMigration=sum(netMigration))%>%
  mutate(netMigrationRate=netMigration/totalPopulation)%>%
  ggplot( aes(x=year, y=netMigrationRate, fill=as.factor(netMigClust)))+
  geom_bar(stat="identity", position = "dodge")+
  scale_fill_manual(values=codemog_pal)+
  labs(title="Net Migration Rates by Recession Period, Favorite2")
p6

# mapcounty = map("county", regions = "Colorado", fill = TRUE, plot = FALSE)
# leaflet(data=mapcounty)%>%addProviderTiles("CartoDB.Positron")%>%
#   addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

write.csv(netMigClust,"C:/Users/RKemp/Downloads/netMigClust.csv")
write.csv(netMigClust_favorite2,"netMigClusters_AnnualMeeting.csv")


