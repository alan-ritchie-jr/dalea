
####RUN CODE IN PHENOLOGY SCRIPT FIRST


library(car)
library(reshape2)
library(lubridate)
library(tidyverse)
library(lme4)
library(glmmTMB)
library(DHARMa)
#########################
#seedchrony

# Need focal_pheno from phenology.R 

#upload focal_pheno
focal_pheno<-read.csv("data/focal_pheno.csv")


#updated seed data
seed2<-read.csv("data/non_database_csvs/dalea_seed_counts_7May2019.csv")

# cobloom<-read.csv("data/non_database_csvs/dalea-coblooming-density_25march2019.csv")

#conspecific density and distance to nearest neighbor data
con_dens<-read.csv("data/non_database_csvs/dalea-conspecific-density_23March2019.csv")
#cobloom data
cobloom<-read.csv("data/non_database_csvs/dalea-coblooming-density_25march2019.csv")
#focal plant flowering, pollination, and stigma collection data
focal_plt<-read.csv("data/non_database_csvs/focal_plant_stigma_26March2019.csv")

#stigma pollen counts
poll_stig<-read.csv("data/non_database_csvs/stigma-pollen-counts_23March2019.csv")

#visit data
visit<-read.csv("data/non_database_csvs/visitation_30Aug2019.csv")



##### first check and summarize seed data!

seed_check2<-seed2%>%anti_join(ID_trt, c("plantID")) # should only be controls

colnames(seed)

#without


seed_prop2<-seed2%>%left_join(ID_trt, c("plantID"))%>%filter(plantID!="8UBC")%>%
  replace_na(list(treatment= "Control"))%>%filter(treatment!="Control")%>%
  rename(full=X.full_seeds, part=X.partial_full_seeds,empty=X.aborted_seeds,
         heads=Num_Heads,abrt_heads=Aborted_Heads,dehisc_heads=Dehisced_Heads)%>%
  group_by(plantID)%>%
  summarise(n_collection_events=n(),heads=sum(heads),full=sum(full), part=sum(part), empty=sum(empty),
            total_fruit=full+part+empty, filling_frt_prop=(full+part)/total_fruit,
            seed_prop=full/total_fruit) 

#join seed data with focal_phenology data


seed_sync2<-focal_pheno%>%inner_join(seed_prop2,by="plantID")


####
#### Now add stigma!



#anti_join to see who has no stigma data
stig_check<-poll_stig%>%anti_join(ID_trt, c("plantID")) #check this one; likely 5 or 30 B
#30B was 50B; manually changed in csv because having issues
# ADD TO OPENREFINE.
# Also check NAs vs 0s in open refine

### stig data frame organizes pollen deposition data
stig<-poll_stig%>%left_join(ID_trt, c("plantID"))%>%filter(!is.na(dpurp_pollen))%>%
  group_by(plantID,round,month,day,year,treatment)%>%
  mutate(date=paste(month,day,year,sep="-"))%>%
  mutate(mdy=mdy(date),yday=yday(mdy), total_pollen=dpurp_pollen+amocan_pollen+
           aster_pollen+
           dcand_pollen+
           unknown_hetero_pollen)%>%
  #now we want to take each plant and take an average of each pollen type per stigma and total deposiiton period
  group_by(plantID,treatment)%>%
  summarise(n_stig=n(),n_unpollinated_stigma=length(total_pollen[total_pollen==0]),
            sum_dalpur=sum(dpurp_pollen), mean_dalpur=mean(dpurp_pollen),
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

###seed_sync2 is the updated data
stig_seed_sync2<-seed_sync2%>%left_join(stig, c("plantID","treatment"))


## code below was originally how I added visitation-- by joining on a round basis 
### edited visitation csv to fix two time entry errors; these errors wouldn't have effected outcome

####
#pollinator visitation

###### maek sure everything is a factor
levels(as.factor(focals$day))
levels(as.factor(focals$round))
levels(as.factor(visit$day))
levels(as.factor(visit$round))
visit$plantID<-trimws(visit$plantID)#trim ws because some IDs ended up with ws afterward

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


### note that this method drops all plants that had 0 blooms on a given round x visit combo
### alternatively you can ignore this, but then you can't take a visit/head/min measure
### since this doesn't affect the actual results I'm removing them

poll_vis<-left_join(visit,focal_flw, by=c("plantID","round_v"))%>%
  filter(bloom_heads>0)%>%filter(!is.na(time))# you have this to remove cases where there were 0 heads blooming, but check real quick
poll_check<-anti_join(visit,focal_flw, by=c("plantID","round_v"))#should be 0

# get number of visits per interval, then divide by number of flowers on plant at that round
visit_summary<-poll_vis%>%group_by(plantID,round_v,treatment,day.y)%>%
  mutate(bee_fly=ifelse(morphoID%in%"Syrphid","fly",ifelse(is.na(morphoID),"none","bee")))%>%
  summarise(n_visits=sum(!is.na(morphoID)),flw_heads=(sum(bloom_heads))/n(), 
            intervals=n_distinct(time),
            n_bee_visits=sum(bee_fly=="bee"), 
            n_fly_visits=sum(bee_fly=="fly"),bees_min=(n_bee_visits/intervals),
            bees_head_min=(n_bee_visits/flw_heads/intervals),fly_min=(n_fly_visits/intervals),
            fly_head_min=(n_fly_visits/flw_heads/intervals),
            visit_head_min=(n_visits/flw_heads)/intervals,
            visit_min=n_visits/intervals)

#this is a per round summary
#indivdual means:

visit_summary_indi<-visit_summary%>%
  group_by(plantID, treatment)%>%
  summarise(n_visit_obs=n(),mean_n_visits=mean(n_visits),mean_bee_visits=mean(n_bee_visits),
            mean_bee_min=mean(bees_min), mean_bee_head_min=mean(bees_head_min),
            mean_fly_visits=mean(n_fly_visits), mean_fly_min=mean(fly_min),
            mean_fly_head_min=mean(fly_head_min),
            mean_visit_head_min=mean(visit_head_min),
            mean_visit_min=mean(visit_min))
### so we need to think about how to incorporate display size.

### now visit summary can be joined with stig_seed_sync2
stig_seed_sync_vis<-stig_seed_sync2%>%left_join(visit_summary_indi, c("plantID","treatment"))


### now add conspecific and coblooming!


#coblooming floral community data

cobloom<-read.csv("data/non_database_csvs/dalea-coblooming-density_25march2019.csv")

#make a quick summary

#first replace na w/ zero
cobloom$X.floral_units[is.na(cobloom$X.floral_units)]<-0
#replace NA NA in plant genus sp column with true NA so we don't inflate plant_genus_sp
cobloom$plant_genus_sp[cobloom$plant_genus_sp == "NA NA"]<- NA

cobloom_ID<-cobloom%>%left_join(ID_trt, c("plantID"))

cobloom_summary<-cobloom_ID%>%
  group_by(treatment)%>%
  summarize(n_plants=n_distinct(cobloom$plantID),richness=n_distinct(plant_genus_sp),
            avg_blooms=mean(X.floral_units),
            max_blooms=max(X.floral_units))

#data zero inflated
# really low average

## now individual level means--these will be merged to full dataframe
indi_cobloom_dens<-cobloom_ID%>%
  group_by(plantID,round)%>%mutate(daily_richness=ifelse(is.na(plant_genus_sp), 0, n_distinct(plant_genus_sp)))%>%ungroup()%>%
  group_by(plantID,treatment)%>%
  summarize(n_cobloom_obs=n(),mean_cobloom_floral_units_1m=mean(X.floral_units),
            mean_cobloom_richness_1m=mean(daily_richness))



####
#### now consspecific density
#####

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
library(tidyverse)
## general treatment level summar
focal_plt_blooms<-focals%>%select_("plantID","round","yday","bloom_heads")%>%
  filter(bloom_heads!=0)%>%
  mutate(round_v=ifelse(yday==192,"1a",ifelse(yday==193,"1b",round)))#created round_v


con_dens_summary<-con_dens_ID%>%
  mutate(round_v=ifelse(day==11,"1a",ifelse(day==12,"1b", round)))%>%
  right_join(focal_plt_blooms, by=c("plantID","round_v"))%>%
  group_by(treatment)

### takeaways: mean density, and its patterns overtime look very similar for both treatments
### the peak number of flowering conspecific stems observed at 5m is higher in burned unit


### Now calculate individual level means; 
#we want only the density during when a plant was flowering

indi_con_dens<-con_dens_ID%>%
  mutate(round_v=ifelse(day==11,"1a",ifelse(day==12,"1b", round)))%>%
  right_join(focal_plt_blooms, by=c("plantID","round_v"))%>%
  group_by(plantID,treatment)%>%drop_na()%>%
  summarize(n_density_obs=n(),mean_dens_1m=mean(X.dalpur_1m),
            mean_dens_5m=mean(X.dalpur_5m))


### merge indi_con_dens and stig_sync_seed_vis

stig_seed_sync_dens_vis<-stig_seed_sync_vis%>%left_join(indi_con_dens, c("plantID","treatment"))

# and cobloom data for full dalea df

#### this is the full data frame--a summary of means of the variables of interest for each invidual
full_dalea_df<-stig_seed_sync_dens_vis%>%left_join(indi_cobloom_dens,c("plantID","treatment"))%>%filter(!is.na(treatment))%>%
  filter(plantID!="9B")#,plantID!="10B"&plantID!="48B")%>%filter(plantID!="28UB")
######Note:

### some plants may need to be dropped
#drop plant 9B  & 42UB (dropped earlier) because we did not collect all of their seed.

# 10B and 48 B were close to tension zone
#Note that including or excluding 10B or 48B doesn't change results


full_dalea_df%>%ggplot(aes(treatment,mean_dens_5m,fill=treatment))+geom_boxplot()
full_dalea_df2%>%ggplot(aes(treatment,mean_dens_5m,fill=treatment))+geom_boxplot()

full_dalea_df%>%ggplot(aes(mean_date_flw,seed_prop,color=treatment))+geom_point()+geom_smooth(method = "lm")
full_dalea_df%>%ggplot(aes(total_fruit,seed_prop,color=treatment))+geom_point()+geom_smooth(method="lm")

dens_mod<-lm(seed_prop~treatment,data=full_dalea_df)

summary(dens_mod)

seed_fruit_mod<-lm(seed_prop~total_fruit+mean_date_flw*treatment,data=full_dalea_df)
seed_fruit_mod<-lm(seed_prop~total_fruit+treatment,data=full_dalea_df)
summary(seed_fruit_mod)
#so larger plants tended to flower earlier.
qqPlot(residuals(seed_fruit_mod))

     