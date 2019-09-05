### data summary
library(tidyverse)

ID_trt<-focals%>%select(plantID, treatment)%>%group_by(plantID,treatment)%>%summarize()

#anti_join to see who has no stigma data
stig_check<-poll_stig%>%anti_join(ID_trt, c("plantID")) #check this one; likely 5 or 30 B
#30B was 50B; manually changed in csv because having issues
# ADD TO OPENREFINE.
# Also check NAs vs 0s in open refine

### stig data frame organizes pollen deposition data
stig<-poll_stig%>%left_join(ID_trt, c("plantID"))%>%filter(!is.na(dpurp_pollen))%>%
  group_by(plantID,round,month,day,year,treatment)%>%
  mutate(date=paste(month,day,year,sep="-"))%>%
  mutate(mdy=mdy(date),yday=yday(mdy))%>%
  
  #now we want to take each plant and take an average of each pollen type per stigma and total deposiiton period
  group_by(plantID,treatment)%>%
  summarise(n_stig=n(),sum_dalpur=sum(dpurp_pollen), mean_dalpur=mean(dpurp_pollen),
            sum_amocan=sum(amocan_pollen),
            mean_amocan=mean(amocan_pollen),
            sum_hetero=sum(amocan_pollen+
                             aster_pollen+
                             dcand_pollen+
                             unknown_hetero_pollen),
            mean_hetero=mean(amocan_pollen+
                               aster_pollen+
                               dcand_pollen+
                               unknown_hetero_pollen),
            sum_total_pollen=sum(dpurp_pollen+amocan_pollen+
                                   aster_pollen+
                                   dcand_pollen+
                                   unknown_hetero_pollen),
            mean_total_pollen=mean(dpurp_pollen+amocan_pollen+
                                     aster_pollen+
                                     dcand_pollen+
                                     unknown_hetero_pollen))
#so some plants we have few stigma, and others we have more.
# Think about how to model when exposure is so different?

#so some plants we have few stigma, and others we have more.
# Think about how to model when exposure is so different?

stig_seed_sync<-stig%>%left_join(seed_sync, c("plantID","treatment"))

###seed_sync2 is the updated data
stig_seed_sync2<-stig%>%left_join(seed_sync2, c("plantID","treatment"))

#### so now we have stigma, seeds, synchrony all in one data frame


### all that's left is adding visitation

## code below was originally how I added visitation-- by joining on a round basis 
### edited visitation csv to fix two time entry errors; these errors wouldn't have effected outcome

####
#pollinators
levels(as.factor(focals$day))
levels(as.factor(focals$round))
levels(as.factor(visit$day))
levels(as.factor(visit$round))
#join pollinators and focal_flw by plantID & new column, round_V

# get number of visits per interval, then divide by number of flowers on plant at that round
visit_summary<-visit%>%select(plantID,time, morphoID, touches)%>%
  group_by(plantID)%>%
  summarise( observations=n_distinct(time),total_visits=sum(!is.na(morphoID)),
           visit_rate=(total_visits/observations))

### now visit summary can be joined with stig_seed_sync2
stig_seed_sync_vis<-stig_seed_sync2%>%left_join(visit_summary, c("plantID"))
## now merge with focals to get 
stig_seed_sync_vis_foc<-stig_seed_sync_vis%>%left_join(focal_summary,c("plantID"))
#should have 94 obs

#so now we have # plants
# seeds
# visits
# observation minutes
# stigma
# stigma w/pollen

#summary for total; drop 8UB because we didn't collect any seed data from it
dalea_summary<-stig_seed_sync_vis_foc%>%group_by(treatment)%>%filter(plantID!="8UB")%>%
  summarize(n_plants=n(),n_fruit=sum(total_fruit),
            n_full_seeds=sum(full),mean_full_seeds=mean(full),
            n_flower_heads=sum(heads),mean_flower_heads=mean(heads),
            mean_flower_heads=mean(heads),
            mean_est_daily_flw_production=mean(est_mean_daily_flw_presentation),
            mean_est_total_observed_flw=mean(est_total_observed_flw_production),
            total_stigma_collected=sum(n_stig),
            mean_stigma_collected=mean(n_stig),
            total_unpollinated_stigma=sum(n_unpollinated_stigma),
            mean_unpollinated_stigma=mean(n_unpollinated_stigma),
            total_n_visits=sum(total_visits),
            mean_n_visits=mean(total_visits),
            total_observation_time=sum(observations),
            mean_visit_rate=mean(visit_rate),
            max_number_flowering_days=max(duration),mean_flowering_days=mean(duration),min_number_flowering_days=min(duration),
            median_flowering_duration=median(duration),median_flowering_start_day=median(start),
            mean_flowering_start_day=mean(start))
  
write.csv(dalea_summary,"data/dalea_summary_table.csv",row.names = FALSE)


## lets start incorporating density data


#coblooming floral community data

cobloom<-read.csv("data/non_database_csvs/dalea-coblooming-density_25march2019.csv")

#make a quick summary

#first replace na w/ zero
cobloom$X.floral_units[is.na(cobloom$X.floral_units)]<-0

cobloom_ID<-cobloom%>%left_join(ID_trt, c("plantID"))

cobloom_summary<-cobloom_ID%>%
  group_by(treatment)%>%
  summarize(n_plants=n_distinct(cobloom$plantID),n_genus_species=n_distinct(plant_genus_sp),
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

individual_con_dens_summary<-con_dens_ID%>%
  group_by(treatment)%>%drop_na()%>%
  summarize(n_plants=n_distinct(plantID),mean_dens_1m=mean(X.dalpur_1m),
            mean_dens_5m=mean(X.dalpur_5m),
            mean_1st_NN=mean(NN_1),mean_2nd_NN=mean(NN_2),mean_3rd_NN=mean(NN_3))
