### data summary for dan and gabby in early september 2019

###


library(tidyverse)
library(tidyverse)
#install.packages("mateable")
library(mateable)
library(lubridate)

#coblooming floral community data
cobloom<-read.csv("data/non_database_csvs/dalea-coblooming-density_25march2019.csv")

#conspecific density and distance to nearest neighbor data
con_dens<-read.csv("data/non_database_csvs/dalea-conspecific-density_23March2019.csv")

#focal plant flowering, pollination, and stigma data
focal_plt<-read.csv("data/non_database_csvs/focal_plant_stigma_26March2019.csv")


#updated seed count data with all seed counts

seed2<-read.csv("data/non_database_csvs/dalea_seed_counts_7May2019.csv")

#first the focal plant data:

#note this is duplicated in phenology data frame!
is.numeric(focals$bloom_heads)
focal_plt$X.bloom_flwheads<-as.numeric(focal_plt$X.bloom_flwheads)

#create a summation of flowers, a treatment column, and a yday column
focals<-focal_plt%>%
  rename(bloom_heads=X.bloom_flwheads)%>%filter(!is.na(head1))%>%
  mutate(total_flws_est=
           ifelse(bloom_heads > 5,
                  ((head1+head2+head3+head4+head5)/5)*bloom_heads,(head1+head2+head3+head4+head5)), 
         treatment=ifelse(grepl("UB",plantID),'UB','B'))%>%
  mutate(date=paste(year,month,day, sep="-"))%>%mutate(ymd=ymd(date),yday=yday(ymd))

### focal summary is used in dalea_sumary script
focal_summary<-focals%>%
  group_by(plantID)%>%
  summarise(n_obs=n(),max_flowering_heads=max(bloom_heads),
            mean_flowering_heads=mean(bloom_heads),
            est_mean_daily_flw_presentation=mean(total_flws_est))
#### this is contained in focal_pheno data frame

ID_trt<-focals%>%select(plantID, treatment)%>%group_by(plantID,treatment)%>%summarize()
###
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
#pollinator visitation
levels(as.factor(focals$day))
levels(as.factor(focals$round))
levels(as.factor(visit$day))
levels(as.factor(visit$round))
visit$plantID<-trimws(visit$plantID)#trim ws

#join pollinators and focal_flw by plantID & new column, round_V
visit<-visit%>%
  mutate(round_v=ifelse(day==16, "2a", 
                        ifelse(day==18,"2b",
                               ifelse(day==11,"1a",ifelse(day==12,"1b",
                                                          round)))))#created round_v

focal_flw<-focal_flw%>%
  mutate(round_v=ifelse(day==197, "2a", 
                        ifelse(day==199,"2b",
                               ifelse(day==192,"1a",ifelse(day==193,"1b",
                                                           round)))))#created round_v

poll_vis<-left_join(visit,focal_flw, by=c("plantID","round_v"))%>%
  filter(bloom_heads>0)
poll_check<-anti_join(visit,focal_flw, by=c("plantID","round_v"))#should be 0

# get number of visits per interval, then divide by number of flowers on plant at that round
visit_summary<-poll_vis%>%select(plantID, round_v, day.y,time, morphoID,date, touches,bloom_heads,treatment)%>%
  group_by(plantID,round_v,treatment,day.y)%>%
  summarise(n_visits=sum(!is.na(morphoID)), n_obs=n(),
            flw_heads=(sum(bloom_heads))/n(), intervals=n_distinct(time),
            visit_head_min=((n_visits/flw_heads)/intervals),visit_min=n_visits/intervals)
#this is a per round summary
#indivdual means:

visit_summary_indi<-visit_summary%>%
  group_by(plantID, treatment)%>%
  summarise(mean_n_visits=mean(n_visits),mean_flw_heads=mean(flw_heads),
            mean_visit_head_min=mean(visit_head_min),
            mean_visit_min=mean(visit_min))
### so we need to think about how to incorporate display size.


### now visit summary can be joined with stig_seed_sync2
stig_seed_sync_vis<-stig_seed_sync2%>%left_join(visit_summary, c("plantID"))
## now merge with focal summary from vis to get 
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


