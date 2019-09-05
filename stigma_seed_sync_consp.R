
## lets start incorporating density data

### need stig_seed_sync2 df from stigma_seed_synchrony script

stig_seed_sync2


#coblooming floral community data

cobloom<-read.csv("data/non_database_csvs/dalea-coblooming-density_25march2019.csv")

#make a quick summary

#first replace na w/ zero
cobloom$X.floral_units[is.na(cobloom$X.floral_units)]<-0

cobloom_ID<-cobloom%>%left_join(ID_trt, c("plantID"))

cobloom_summary<-cobloom_ID%>%
  group_by(treatment)%>%
  summarize(n_plants=n_distinct(cobloom$plantID),richness=n_distinct(plant_genus_sp),
            avg_blooms=mean(X.floral_units),
            max_blooms=max(X.floral_units))

ggplot(cobloom_ID, aes(X.floral_units,fill=treatment))+geom_histogram()+facet_grid(.~treatment)
#data zero inflated
# really low average

## now individual level


#conspecific density and distance to nearest neighbor data
con_dens<-read.csv("data/non_database_csvs/dalea-conspecific-density_23March2019.csv")
#how many plants?
con_dens_ID<-con_dens%>%left_join(ID_trt, c("plantID"))
#drop all obs where NA or estimate of distance (>4, >10) used
con_dens_ID$NN_1<-as.numeric(as.character(con_dens_ID$NN_1))
con_dens_ID$NN_2<-as.numeric(as.character(con_dens_ID$NN_2))
con_dens_ID$NN_3<-as.numeric(as.character(con_dens_ID$NN_3))

#some NAs left over from data cleaning- these should be 0
con_dens_ID$X.dalpur_1m[is.na(con_dens_ID$X.dalpur_1m)]<-0
con_dens_ID$X.dalpur_5m[is.na(con_dens_ID$X.dalpur_5m)]<-0

## first let's et the ratio of NAs to actual data across the 3 NN classes
sum(is.na(con_dens_ID$NN_1))/sum(!is.na(con_dens_ID$NN_1))
sum(is.na(con_dens_ID$NN_2))/sum(!is.na(con_dens_ID$NN_2))
sum(is.na(con_dens_ID$NN_3))/sum(!is.na(con_dens_ID$NN_3))
### pretty close to half of the data is NA here. 
# there are points in the data where this truly means no conspecifics are near
# but there are other points where this means we lack data.
# we shouldn't lose plants

## general treatment level summar

con_dens_summary<-con_dens_ID%>%
  group_by(treatment)%>%drop_na()%>%
  summarize(n_plants=n_distinct(plantID),mean_dens_1m=mean(X.dalpur_1m),
            mean_dens_5m=mean(X.dalpur_5m),
            mean_1st_NN=mean(NN_1),mean_2nd_NN=mean(NN_2),mean_3rd_NN=mean(NN_3))
#now let's visualized

#histograms

ggplot(con_dens_ID, aes(NN_1,fill=treatment))+geom_histogram()+facet_grid(.~treatment)+ggtitle("Distribution of 1st Nearest Neighbor dist by treatment")

#
ggplot(con_dens_ID, aes(X.dalpur_1m,fill=treatment))+geom_histogram()+facet_grid(.~treatment)+
  ggtitle("Distribution of conspecific density in 1m by treatment")
#lotta zeroes

ggplot(con_dens_ID, aes(X.dalpur_5m,fill=treatment))+geom_histogram()+
  facet_grid(.~treatment)+ggtitle("Distribution of conspecific density in 5m by treatment")
#fewer zeroes--more variance than 1m?

###boxplots or violin plots

ggplot(con_dens_ID, aes(treatment,NN_1,fill=treatment))+geom_boxplot()+
  ggtitle("Mean distance to NN1 by treatment")

ggplot(con_dens_ID, aes(treatment,NN_2,fill=treatment))+geom_boxplot()+
  ggtitle("Mean conspecific density in 5m by treatment")

ggplot(con_dens_ID, aes(treatment,X.dalpur_1m,fill=treatment))+geom_boxplot()+
  ggtitle("Mean conspecific density in 1m by treatment")

ggplot(con_dens_ID, aes(treatment,X.dalpur_5m,fill=treatment))+geom_boxplot()+
  ggtitle("Mean conspecific density in 5m by treatment")


### relationship over time?
ggplot(con_dens_ID, aes(as.factor(round),X.dalpur_1m,color=treatment))+geom_boxplot()+
  ggtitle("Change in conspecific density in 1m over time by treatment")

ggplot(con_dens_ID, aes(as.factor(round),X.dalpur_5m,color=treatment))+geom_boxplot()+
  ggtitle("Change in conspecific density in 5m over time by treatment")

ggplot(con_dens_ID, aes(as.factor(round),NN_1,color=treatment))+geom_boxplot()+
  ggtitle("Change in distance to NN1 over time by treatment")

### takeaways: mean density, and its patterns overtime look very similar for both treatments
### the peak number of flowering conspecific stems observed at 5m is higher in burned unit

### now let's look at individual means

indi_con_dens<-con_dens_ID%>%
  group_by(plantID,treatment)%>%drop_na()%>%
  summarize(n_obs=n(),mean_dens_1m=mean(X.dalpur_1m),
            mean_dens_5m=mean(X.dalpur_5m),
            mean_NN_1=mean(NN_1),mean_NN_2=mean(NN_2),mean_NN_3=mean(NN_3))

## visualize

ggplot(indi_con_dens, aes(treatment,mean_NN_1,fill=treatment))+geom_boxplot()+
  ggtitle("Individual mean distance to NN1 by treatment")
#individuals in burn unit on average have a closer 1st nearest neighbor (diff of 20cm?)
ggplot(indi_con_dens, aes(treatment,mean_dens_1m,fill=treatment))+geom_boxplot()+
  ggtitle("Individual mean conspecific density at 1m by treatment")
#individuals in burn unit on average have slightly higher mean conspecific density at 1m
# diff probably not significant
ggplot(indi_con_dens, aes(treatment,mean_dens_5m,fill=treatment))+geom_boxplot()+
  ggtitle("Individual mean conspecific density at 5m by treatment")
### individuals in burn unit on average have slightly higher mean conspecific density at 5m
### means almost the same--but the variance is much greater in burn unit


### exploring how this interacts with phenology


### merge indi_con_dens and stig_sync_seed_2

stig_seed_sync_dens<-indi_con_dens%>%left_join(stig_seed_sync2, c("plantID","treatment"))
## now join with visit_summary_indi from dalea_summary script
# this joins with individual level mean visitation rate (visits/head/minute)

stig_seed_sync_dens_vis<-stig_seed_sync_dens%>%left_join(visit_summary_indi, c("plantID","treatment"))

# and cobloom data
### lets explore phenology and density relationships
### if fire increases flowering you would expect that there are more individuals flowering near focal plants
###

stig_seed_sync_dens%>%ggplot(aes(start,mean_dens_5m,color=treatment))+
  geom_point()+stat_smooth(method="lm")+
  ggtitle("Average number of conspecifics in 5m as a function of start date")
#the earlier you started the more likely you were to have a higher mean number of nearby conspecifics
## in the burn unit!
stig_seed_sync_dens%>%ggplot(aes(start,mean_NN_1,color=treatment))+
  geom_point()+stat_smooth(method="lm")+
  ggtitle("Average distance to 1st NN as a function of start date")
#the earlier you started the more likely your 1st NN was close by
# pretty different intercepts!

stig_seed_sync_dens%>%ggplot(aes(duration,start,color=treatment))+
  geom_point()+stat_smooth(method="lm")+
  ggtitle("Remember: duration and start are correlated!")
###So the relationships observed in the preceding graphs of start date vs density variables should 
### more or less be flipped


stig_seed_sync_dens%>%ggplot(aes(duration,mean_dens_5m,color=treatment))+
  geom_point()+stat_smooth(method="lm")+
  ggtitle("Average number of conspecifics in 5m as a function of flowering duration")

stig_seed_sync_dens%>%ggplot(aes(duration,mean_NN_1,color=treatment))+
  geom_point()+stat_smooth(method="lm")+
  ggtitle("Average number of conspecifics in 5m as a function of flowering duration")
### Yep, look pretty much flipped

##### 

## pollen deposition, visitation, & conspecific

stig_seed_sync_dens%>%ggplot(aes(start,mean_dalpur,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average dalpur pollen deposted on a plant as a function of start date")

stig_seed_sync_dens%>%ggplot(aes(mean_NN_1,mean_dalpur,color=treatment))+
  geom_jitter()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average dalpur pollen deposted on a plant as a function of mean distance to 1st NN")

stig_seed_sync_dens%>%ggplot(aes(mean_dens_5m,mean_dalpur,color=treatment))+
  geom_jitter()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average dalpur pollen deposted on a plant as a function of mean density of conspecifics")
### density metrics and dalea pollination match expectations! the mean_dens_5m probably has a cleaner 
###  relationship

stig_seed_sync_dens%>%ggplot(aes(mean_dens_5m,mean_amocan,color=treatment))+
  geom_jitter()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average amocan pollen deposted on a plant as a function of mean density of conspecifics")
#no relationsnhip

stig_seed_sync_dens%>%ggplot(aes(mean_NN_1,mean_hetero,color=treatment))+
  geom_jitter()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average hetero pollen deposted on a plant as a function of mean distance to 1st NN")

### a weak relationship apparent--maybe due to correlation between mean_dalpur and mean_hetero deposition

stig_seed_sync_dens%>%ggplot(aes(mean_hetero,mean_dalpur,color=treatment))+
  geom_jitter()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average dalpur pollen deposted on a plant as a function of average hetero pollen")

###worth noting there's likely a positive correlation between hetero and dalea pollen counts
### also there's less variation present in the hetero deposition data in the unburned unit


###look at visits! Note that these visit rates are only visits/minute, not visits/head/minute!
stig_seed_sync_dens_vis%>%ggplot(aes(mean_dens_5m,mean_visit_rate,color=treatment))+
  geom_jitter()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average visit rate as a function of average dalea density in 5m")


stig_seed_sync_dens_vis%>%ggplot(aes(start,mean_visit_rate,color=treatment))+
  geom_jitter()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average visit rate as a function of start date") 
#this is interesting! visit rates are good on burn side early(possibly due to amorpha)

stig_seed_sync_dens_vis%>%ggplot(aes(mean_NN_1,mean_visit_rate,color=treatment))+
  geom_jitter()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average visit rate as a function of distance to NN1")
#not much oging on here

stig_seed_sync_dens_vis%>%ggplot(aes(heads,mean_visit_rate,color=treatment))+
  geom_jitter()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average visit rate as a function of number of flowering heads")

stig_seed_sync_dens_vis%>%ggplot(aes(heads,mean_n_visits,color=treatment))+
  geom_jitter()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average number of visits as a function of number of flowering heads")

stig_seed_sync_dens_vis%>%ggplot(aes(start,mean_n_visits,color=treatment))+
  geom_jitter()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average number of visits  as a function of start date")
## maybe 
stig_seed_sync_dens_vis%>%ggplot(aes(mean_visit_rate,seed_prop,color=treatment))+
  geom_jitter()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Seed produced as a function of average visit rate")

stig_seed_sync_dens_vis%>%ggplot(aes(mean_n_visits,mean_total_pollen,color=treatment))+
  geom_jitter()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average total pollen deposition as a function of number of mean number of visits")

### why does mean n vists work better than the mean rate?

