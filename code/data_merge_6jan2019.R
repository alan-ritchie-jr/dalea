
####RUN CODE IN PHENOLOGY SCRIPT FIRST
# or require(phenology.R)

library(car)
library(reshape2)
library(lubridate)
library(tidyverse)
library(lme4)
library(glmmTMB)
library(DHARMa)
#########################

library(RMariaDB) 
#get user name and password
source("user.R")
source("psw.R")


#connect to DB

conn <- dbConnect(RMariaDB::MariaDB(), host = '160.94.186.138',  dbname='dalea_2018', user = user, password = psw, port=8889)
dbListTables(conn)

### 6 tables
con_dens<- dbReadTable(conn, "dalea_conspecific_density")#conspecific density
cobloom<- dbReadTable(conn, "dalea_coblooming")# coblooming heterospecific data
#focal_plt<- dbReadTable(conn, "dalea_focal_floral") # this table has the focal plant data if needed; it is initially pulled in in the phenology script
poll_stig<- dbReadTable(conn, "dalea_stigma_pollen_counts")#stigma pollen data
visit<- dbReadTable(conn, "dalea_visitation")#bee and fly visitation data
seed2<- dbReadTable(conn, "dalea_seed_counts")#seed data


dbDisconnect(conn)

#
#Now we need the focal_pheno table from phenology.R 

#upload focal_pheno from local folder
focal_pheno<-read.csv("data/focal_pheno.csv")


##### first check and summarize seed data!

seed_check2<-seed2%>%anti_join(ID_trt, c("plantID")) # should only be controls

####

# next compare seed in focal versuse controls

###quick summarize for comparison
seed2$treatment2<-gsub("[[:digit:]]","",seed2$plantID)
seed_test<-seed2%>%left_join(ID_trt, c("plantID"))%>%filter(plantID!="8UBC")%>%filter(plantID!="42UB")%>%
  replace_na(list(treatment= "Control"))%>%
  rename(full=X.full_seeds, part=X.partial_full_seeds,
         empty=X.aborted_seeds, heads=Num_Heads,abrt_heads=Aborted_Heads,dehisc_heads=Dehisced_Heads)%>%
  group_by(plantID,treatment)%>%
  summarise(n_collection_events=n(),heads=sum(heads),full=sum(full), part=sum(part), empty=sum(empty),
            total_fruit=full+part+empty, filling_frt_prop=(full+part)/total_fruit,
            seed_prop=full/total_fruit)%>%mutate(cont=ifelse(treatment=="Control", "C","UC"))# this makes a categorical variable "cont"

### now use seed test for t test of seeds in focal vs control plants 
#control plants were randomly chosen at seed collection from both units
#compare mean seed set to test if stigma removal potentially impacted seed set.

# if we look at focals vs controls overall:
t.test(seed_prop~cont,data=seed_test)

#
#now split by treatment 2 for anova

seed_test$treatment2<-gsub("[[:digit:]]","",seed_test$plantID) #split plant ID letters from numbers to compare controls and focals within each unit
#

b_test<-seed_test%>%filter(treatment2=="BC"|treatment2=="B")

ub_test<-seed_test%>%filter(treatment2=="UBC"|treatment2=="UB")

t.test(seed_prop~treatment2,data=b_test) # burned focals vs controls
t.test(seed_prop~treatment2,data=ub_test)# unburned focals vs controls

##########################

### next:
### summarize seed data for joining with focal_pheno

seed_prop2<-seed2%>%left_join(ID_trt, c("plantID"))%>%filter(plantID!="8UBC")%>%filter(plantID!="42UB")%>%#drope 8ubc because lost seed, drop 42ub because no flowering obs
 filter(treatment2=="B"|treatment2=="UB")%>%#
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

#convert pollen counts to numeric; stored as varchar in SQL to retain NA values, which reflect stigma missing tip.
poll_stig$dpurp_pollen<-as.numeric(poll_stig$dpurp_pollen)
poll_stig$amocan_pollen<-as.numeric(poll_stig$amocan_pollen)
poll_stig$aster_pollen<-as.numeric(poll_stig$aster_pollen)
poll_stig$dcand_pollen<-as.numeric(poll_stig$dcand_pollen)
poll_stig$unknown_hetero_pollen<-as.numeric(poll_stig$unknown_hetero_pollen)


### stig data frame organizes pollen deposition data
stig<-poll_stig%>%left_join(ID_trt, c("plantID"))%>%filter(!is.na(dpurp_pollen))%>%
  group_by(plantID,round,month,day,year,treatment)%>%
  mutate(date=paste(month,day,year,sep="-"))%>%
  mutate(mdy=mdy(date),yday=yday(mdy), total_pollen=dpurp_pollen+amocan_pollen+
           aster_pollen+
           dcand_pollen+
           unknown_hetero_pollen)%>%
  #now we want to take each plant and take an average of each pollen type per stigma and total deposiiton period
 # what if we tally the number of stigma where hp > cp

   group_by(plantID,treatment)%>%
  summarise(n_stig=n(), n_unpollinated_stigma=length(total_pollen[total_pollen==0]),
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

###summary table?

st<-stig%>%group_by(treatment)%>%summarize(mean_n_stig=mean(n_stig),ratio_amo_hetero=(sum(sum_amocan)/sum(sum_hetero)),mean_sum_dalpur=mean(sum_dalpur), 
                                           mean_sum_hetero=mean(sum_hetero),
                                           mean_total_pollen=mean(sum_total_pollen))
view(st)

#so some plants we have few stigma, and others we have more.
#exposure is quite a bit different then. although this likely tracks with size--> bigger plants =more flowers=more pollen


###seed_sync2 is the updated data
stig_seed_sync2<-seed_sync2%>%left_join(stig, c("plantID","treatment"))


## code below was originally how I added visitation-- by joining on a round basis 
### edited visitation csv to fix two time entry errors; these errors wouldn't have effected outcome

####
#pollinator visitation
visit<-read.csv("data/non_database_csvs/visitation_30Aug2019.csv")
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
  filter(bloom_heads>0)%>%filter(!is.na(time)&time!="NA")%>%
  mutate(morphoID= fct_recode(morphoID,NULL = "NA"))# you have this to remove cases where there were 0 heads blooming, but check real quick

poll_vis$touches[is.na(poll_vis$morphoID)]<-NA
# get number of visits per interval, then divide by number of flowers on plant at that round
visit_summary<-poll_vis%>%group_by(plantID,round_v,treatment,day.y)%>%
  #create conditional mutate to convert NA values in morphoID
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
  summarise(n_visit_obs=n(),sum_intervals=sum(intervals),mean_intervals=mean(intervals),mean_n_visits=mean(n_visits),mean_bee_visits=mean(n_bee_visits),
            mean_bee_min=mean(bees_min), mean_bee_head_min=mean(bees_head_min),
            mean_fly_visits=mean(n_fly_visits), mean_fly_min=mean(fly_min),
            mean_fly_head_min=mean(fly_head_min),
            mean_visit_head_min=mean(visit_head_min),
            mean_visit_min=mean(visit_min))

### now visit summary can be joined with stig_seed_sync2
stig_seed_sync_vis<-stig_seed_sync2%>%left_join(visit_summary_indi, c("plantID","treatment"))

mean(visit_summary_indi$mean_bee_min)
mean(visit_summary_indi_old$mean_bee_min)
### now add conspecific and coblooming!


#coblooming floral community data

#cobloom<-read.csv("data/non_database_csvs/dalea-coblooming-density_25march2019.csv")

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
#first turn into NAs
con_dens_ID$X.dalpur_1m<-as.numeric(as.character(con_dens_ID$X.dalpur_1m))
con_dens_ID$X.dalpur_5m<-as.numeric(as.character(con_dens_ID$X.dalpur_5m))
#now shift to 0
con_dens_ID$X.dalpur_1m[is.na(con_dens_ID$X.dalpur_1m)]<-0
con_dens_ID$X.dalpur_5m[is.na(con_dens_ID$X.dalpur_5m)]<-0

## first let's et the ratio of NAs to actual data across the 3 NN classes
sum(is.na(con_dens_ID$NN_1))/sum(!is.na(con_dens_ID$NN_1))
sum(is.na(con_dens_ID$NN_2))/sum(!is.na(con_dens_ID$NN_2))
sum(is.na(con_dens_ID$NN_3))/sum(!is.na(con_dens_ID$NN_3))
### pretty close to half of the data is NA here. 
# there are points in the data where this truly means no conspecifics are near
# but there are other points where this means we lack data.



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

indi_con_dens2<-con_dens_ID%>%
  mutate(round_v=ifelse(day==11,"1a",ifelse(day==12,"1b", round)))%>% # for all density change
 right_join(focal_plt_blooms, by=c("plantID","round_v"))%>% # for all density change
  group_by(plantID,treatment)%>% drop_na()%>%
  summarize(n_density_obs=n(),mean_dens_1m=mean(X.dalpur_1m),
            mean_dens_5m=mean(X.dalpur_5m))


### merge indi_con_dens and stig_sync_seed_vis

stig_seed_sync_dens_vis<-stig_seed_sync_vis%>%left_join(indi_con_dens2, c("plantID","treatment"))

# and cobloom data for full dalea df

#### this is the full data frame--a summary of means of the variables of interest for each invidual
full_dalea_df2<-stig_seed_sync_dens_vis%>%left_join(indi_cobloom_dens,c("plantID","treatment"))%>%filter(!is.na(treatment))%>%
  filter(plantID!="9B")#,plantID!="10B"&plantID!="48B")%>%filter(plantID!="28UB")
######Note:

### some plants may need to be dropped
#drop plant 9B  & 42UB (dropped earlier) because we did not collect all of their seed.

# 10B and 48 B were close to tension zone
#Note that including or excluding 10B or 48B doesn't change results


 full_dalea_df2%>%drop_na()%>%ggplot(aes(treatment,mean_dens_5m,fill=treatment))+geom_boxplot()
 full_dalea_df2%>%drop_na()%>%ggplot(aes(treatment,mean_dens_5m,fill=treatment))+geom_boxplot()
 
full_dalea_df%>%drop_na()%>%ggplot(aes(treatment,full,fill=treatment))+geom_boxplot()
dalea%>%drop_na()%>%ggplot(aes(treatment,mean_dens_5m,fill=treatment))+geom_boxplot()

library(tidyverse)
dalea%>%ggplot(aes(max_flowering_heads,full,fill=treatment))+geom_point()+geom_smooth(method = "lm")
full_dalea_df%>%ggplot(aes(total_fruit,seed_prop,color=treatment))+geom_point()+geom_smooth(method="lm")

dens_mod<-lm(mean_dalpur~treatment+max_flowering_heads+ mean_visit_min,data=full_dalea_df)

vis_mod<-lm(mean_visit_min~max_flowering_heads+scale(mean_date_flw)+mean_dens_5m,data=full_dalea_df)

summary(vis_mod)
qqPlot(residuals(vis_mod))
seed_fruit_mod<-lm(seed_prop~total_fruit+mean_date_flw*treatment,data=full_dalea_df)
seed_fruit_mod<-lm(seed_prop~total_fruit+treatment,data=full_dalea_df)
summary(seed_fruit_mod)
#so larger plants tended to flower earlier.
qqPlot(residuals(seed_fruit_mod))

