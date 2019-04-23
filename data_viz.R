#load in tidyverse

library(tidyverse)
library(lubridate)

### upload dataframes

cobloom<-read.csv("data/non_database_csvs/dalea-coblooming-density_25march2019.csv")

con_dens<-read.csv("data/non_database_csvs/dalea-conspecific-density_23March2019.csv")

focal_plt<-read.csv("data/non_database_csvs/focal_plant_stigma_26March2019.csv")

poll_stig<-read.csv("data/non_database_csvs/stigma-pollen-counts_23March2019.csv")

visit<-read.csv("data/non_database_csvs/visitation_23March2019.csv")

seed<-read.csv("data/non_database_csvs/seed-counts_26March2019.csv")



###figure 1
### phenology

#create a summation of flowers and a treatment column
focals<-focal_plt%>%
  rename(bloom_heads=X.bloom_flwheads)%>%
  mutate(total_flws_est=head1+head2+head3+head4+head5, 
         treatment=ifelse(grepl("UB",plantID),'UB','B'))%>%
  mutate(date=paste(month,day,year,sep="-"))%>%mutate(mdy=mdy(date),yday=yday(mdy))

#summarize for a count of blooming focal plants per day and add yday component
foc<-focals%>%
  select(round,month,day,year,treatment,bloom_heads)%>%
  group_by(round,month,day,year,treatment)%>%
  summarize(sum_bloom = sum(bloom_heads > 0))%>%
  mutate(date=paste(month,day,year,sep="-"))%>%mutate(mdy=mdy(date),yday=yday(mdy))



foc%>%
  filter(day!=190)%>%
  mutate(day2=ifelse(day==208,207,day))%>%
ggplot(aes(as.factor(day2), sum_bloom, color=treatment))+
  geom_col(position="dodge",aes(group=treatment,fill=treatment))+
  labs(x="Round" ,y="Plants blooming")+
  ggtitle("Fig. 1a: Plants blooming at each sampling date per treatment")

#examine without the added individuals
#drop df

drop<-c("31UB","32UB","33UB","34UB","35UB", "36UB","37UB","38UB","39UB","40UB",
        "41UB","42UB","43UB","44UB","44UB","45UB", "46UB","47UB","48UB","49UB",
        "31B","32B","33B","34B","35B", "36B","37B","38B","39B","40B",
        "41B","42B","43B","44B","44B","45B", "46B","47B","48B","49B")
#drop values contained in drop from df
focal_drop<-focals[!focals$plantID %in%drop,]
#
foc_drop<-focal_drop%>%
  select(round,month,day,year,treatment,bloom_heads)%>%
  group_by(round, month,day,year,treatment)%>%
  summarize(sum_bloom = sum(bloom_heads > 0))%>%
  mutate(date=paste(month,day,year,sep="-"))%>%
  mutate(mdy=mdy(date),yday=yday(mdy))


  # much cleaner
  
  
  #
### 
foc_drop%>%
filter(day!=190)%>%  
  ggplot(aes(as.factor(day), sum_bloom, color=treatment))+
  geom_col(position="dodge",aes(group=treatment,fill=treatment))+
  labs(x="Round" ,y="Plants blooming")+
  ggtitle("Fig. 1b: Plants blooming at each sampling date per treatment",subtitle="Drop added plants")


#flowering heads + flowers

focal_flw<-focals%>%
  select(round,month,day,year,treatment,plantID,bloom_heads,total_flws_est)%>%
  group_by(round,month,day,year,treatment)%>%
  mutate(sum_bloom = sum(bloom_heads > 0))%>%
  mutate(date=paste(month,day,year,sep="-"))%>%
  replace_na(list(total_flws_est= 0))%>%
  ungroup()
#add yday
focal_flw$date<-mdy(focal_flw$date)
focal_flw$day<-yday(focal_flw$date)

#summary table
focaf<-focal_flw%>%filter(day!=190)%>% 
  select(round,month,day,year,treatment,plantID,bloom_heads,total_flws_est)%>%
  group_by(round,month,day,year,treatment)%>%
  mutate(sum_bloom = sum(bloom_heads > 0))%>%
  mutate(date=paste(month,day,year,sep="-"))%>%ungroup()%>%
 group_by(treatment,plantID)%>%
  summarize(mean_heads=mean(bloom_heads),mean_flws=mean(total_flws_est))

focaf%>%
  ggplot(aes(treatment, mean_heads))+
  geom_violin(aes(group=treatment,fill=treatment))+ geom_boxplot(width=0.1)+
  labs(x="Treatment" ,y="Avg. # flowering heads per individual")+
  ggtitle("Fig. 2a: # flowering heads per individual")

focaf%>%
  ggplot(aes(treatment, mean_flws))+
  geom_violin(aes(group=treatment,fill=treatment))+ geom_boxplot(width=0.1)+
  labs(x="Treatment" ,y="Avg estimated # flowering heads per individual")+
  ggtitle("Fig. 2b: # flowers per individual")

focal_flw%>% group_by(treatment)%>%drop_na()%>%
  summarize(mean_heads=mean(bloom_heads),mean_flws=mean(total_flws_est))
#plots

focal_flw%>%
  ggplot(aes(treatment, bloom_heads))+
  geom_violin(aes(group=treatment,fill=treatment))+ geom_boxplot(width=0.1)+
  labs(x="Treatment" ,y="# flowering heads per individual per day")+
  ggtitle("Fig. 2a: Avg. # flowering heads per treatment")

focal_flw%>%
  ggplot(aes(treatment, total_flws_est))+
  geom_violin(aes(group=treatment,fill=treatment))+ geom_boxplot(width=0.1)+
  labs(x="Treatment" ,y="Estimated # flowers per individual per day")+
  ggtitle("Fig. 2b: Avg. # flowers per treatment")

#now over time
focal_flw%>%
  filter(day!=190&day!=222)%>% mutate(day2=ifelse(day==208,207,day))%>%
  ggplot(aes(as.factor(day2), bloom_heads, group=interaction(treatment,day2),fill=treatment))+
  geom_boxplot()+
  labs(x="Yday" ,y="# flowering heads per individual")+
  ggtitle("Fig. 2c: Avg. # flowering heads per treatment over time")

focal_flw%>%
  filter(day!=190&day!=222)%>% mutate(day2=ifelse(day==208,207,day))%>%
  ggplot(aes(as.factor(day2), total_flws_est, group=interaction(treatment,day2),fill=treatment))+
  geom_boxplot()+
  labs(x="Yday" ,y="Estimated # flowers per individual")+
  ggtitle("Fig. 2d: Avg. # flowers per treatment over time")


####
#pollinators
levels(as.factor(focals$day))
levels(as.factor(focals$round))
levels(as.factor(visit$day))
levels(as.factor(visit$round))
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

# get number of visits per interval, then divide by number of flowers on plant at that round
poll<-poll_vis%>%select(plantID, round_v, day.y,time, morphoID,date, touches,bloom_heads,treatment)%>%
  group_by(plantID,round_v,treatment,day.y)%>%
  summarise(n_visits=sum(!is.na(morphoID)), n_obs=n(),
            flw_heads=(sum(bloom_heads))/n(), intervals=n_distinct(time),
            visit_rate=(n_visits/intervals)/flw_heads)%>%
  filter(intervals>1)

#diff in visit rate treatment
poll%>% 
  ggplot(aes(treatment, visit_rate, fill=treatment))+
  geom_boxplot()+
  labs(x="Treatment" ,y="Visits/minute/flower")+
  ggtitle("Fig. 3a: Visitation rate by treatment")
#visit rate  diff over time
poll%>% 
  ggplot(aes(as.factor(day.y), visit_rate, group=interaction(treatment,day.y),
             fill=treatment))+
  geom_boxplot()+
  labs(x="Day" ,y="Visits/Minute/Flower")+
  ggtitle("Fig. 3b: Visitation per treatment rate over time")


# # visiting morpho groups over time
morpho<-poll_vis%>%select(plantID, round_v, day.y,time, morphoID, touches,bloom_heads,treatment)%>%
  mutate(morphoID= fct_recode(morphoID, "Lasioglossum" = "Dialictus"))%>%
  mutate(bee_fly=ifelse(morphoID=="Syrphid","fly","bee"))%>%
  group_by(morphoID,day.y,treatment,bee_fly)%>%
  summarise(n_visits=n(),n_plants=n_distinct(plantID),
            n_touches=sum(touches))%>%filter(!is.na(morphoID))

morpho_summary<-poll_vis%>%select(plantID, round_v, day.y,time, morphoID, touches,bloom_heads,treatment)%>%
  mutate(morphoID= fct_recode(morphoID, "Lasioglossum" = "Dialictus"))%>%
  mutate(bee_fly=ifelse(morphoID=="Syrphid","fly","bee"))%>%
  group_by(bee_fly,treatment)%>%
  summarise(n_visits=n())
### total visits by morpho groups between treatments
morpho%>%
  ggplot(aes(morphoID, n_visits, fill=treatment))+
  geom_boxplot()+
  labs(x="Pollinator Morphospecies" ,y="# unique visits")+
  ggtitle("Fig. 3c: Within plant movement by morphogroup")

morpho%>%
  ggplot(aes(morphoID, n_visits/n_plants, fill=treatment))+
  geom_boxplot()+
  labs(x="Pollinator Morphospecies" ,y="# unique visits/# plants")+
  ggtitle("Fig. 3d: Visitation by morphogroup", subtitle="accounting for # plants observed on")

### total visits by morpho groups between treatments
morpho%>%
  ggplot(aes(morphoID, n_touches/n_visits, fill=treatment))+
  geom_boxplot()+
  labs(x="Pollinator Morphospecies" ,y="# moves within plant/visit")+
  ggtitle("Fig. 3e: Within plant movement by morphogroup")

morpho%>%
  ggplot(aes(bee_fly, n_visits, fill=treatment))+
  geom_boxplot()+
  labs(x="Course Morphospecies" ,y="# unique visits")+
  ggtitle("Visitation bee vs fly")

morpho%>%
  ggplot(aes(bee_fly, n_visits/n_plants, fill=treatment))+
  geom_boxplot()+
  labs(x="Course Morphospecies" ,y="# unique visits")+
  ggtitle("Visitation bee vs fly")
###
  
#pollen deposition
levels(as.factor(poll_stig$day))
levels(as.factor(focals$day))
levels(as.factor(poll_stig$round))
levels(as.factor(focals$round))
### create new round to account for weird day thing
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

###will have to determine a join for modeling ####

### for viz: just generate a yday


### calculate an average pollen depot for each type (dalpur & hetero & amorpha) plant at each day
### create new variable summing all pollen except dalpur
###plot for b and ub at each time
ID_trt<-focals%>%select(plantID, treatment)%>%group_by(plantID,treatment)%>%summarize()

#anti_join to see who has no stigma data
stig_check<-poll_stig%>%anti_join(ID_trt, c("plantID")) #check this one; likely 5 or 30 B
#30B was 50B; manually changed in csv because having issues
# ADD TO OPENREFINE.

stig<-poll_stig%>%left_join(ID_trt, c("plantID"))%>%
  group_by(plantID,round,month,day,year,treatment)%>%
  mutate(date=paste(month,day,year,sep="-"))%>%
  mutate(mdy=mdy(date),yday=yday(mdy))%>%
 gather(key = "pollen_type", value = "count",
        dpurp_pollen,amocan_pollen,
        aster_pollen,dcand_pollen,unknown_hetero_pollen)

stig_summary<-stig%>%filter(!is.na(count))%>%
  mutate(pollen_type2=ifelse(pollen_type=="dpurp_pollen", "dpurp","hetero"))%>%
    group_by(pollen_type2,treatment)%>%
  summarize(min_pollen=min(count),
            mean_pollen=mean(count),max_pollen=max(count))
#
stig%>%
  filter(!is.na(count))%>%
  ggplot(aes(treatment, count, fill=pollen_type))+
  geom_boxplot()+
  labs(x="treatment" ,y="# pollen grains per stigma")+
  ggtitle("Fig. 4a: # pollen grains deposited")


stig%>%
  filter(!is.na(count))%>%
  group_by(treatment,yday,pollen_type)%>%
  summarize(mean_poll=mean(count))%>%
  ggplot(aes(treatment, mean_poll, fill=pollen_type))+
  geom_boxplot()+
  labs(x="treatment" ,y="Mean # pollen grains per stigma")+
  ggtitle("Fig. 4a: Mean # pollen grains deposited per treatment")


    
stig%>%
  filter(!is.na(count))%>%
  group_by(treatment,yday,pollen_type)%>%
  ggplot(aes(as.factor(yday), count, fill=pollen_type,
             group=interaction(pollen_type,as.factor(yday))))+
  geom_boxplot()+facet_grid(.~treatment)+
  labs(x="treatment" ,y="# pollen grains per stigma")+
  ggtitle("Fig. 4b: # pollen grains deposited overtime")
###
#SEEEEDS

## add in ID_trt to seeds
seed_check<-seed%>%anti_join(ID_trt, c("plantID")) # should only be controls

colnames(seed)
### calculate seed prod difference for each group, accoutn for # heads
seed_count<-seed%>%left_join(ID_trt, c("plantID"))%>%
  rename(full=X.full_seeds, part=X.partial_full_seeds,empty=X.aborted_seeds,
         heads=Num_Heads,abrt_heads=Aborted_Heads,dehisc_heads=Dehisced_Heads)%>%
  gather(key = "seed_fill", value = "count",full,part,empty)%>% 
  replace_na(list(treatment= "Control"))

#without
seed_prop<-seed%>%left_join(ID_trt, c("plantID"))%>%
  rename(full=X.full_seeds, part=X.partial_full_seeds,empty=X.aborted_seeds,
         heads=Num_Heads,abrt_heads=Aborted_Heads,dehisc_heads=Dehisced_Heads)%>%
  mutate(total_fruit=(full+part+empty))

seed_summary<-seed_count%>%filter(!is.na(count))%>%filter(treatment!="Control")%>%
  group_by(seed_fill)%>%
  summarize(sum_heads=sum(heads),min_seed=min(count),
            mean_seed=mean(count),max_seed=max(count))

seed_prop_summary<-seed_prop%>%group_by(treatment)%>%
 

        
drop<-c("31UB","32UB","33UB","34UB","35UB", "36UB","37UB","38UB","39UB","40UB",
        "41UB","42UB","43UB","44UB","44UB","45UB", "46UB","47UB","48UB","49UB",
        "31B","32B","33B","34B","35B", "36B","37B","38B","39B","40B",
        "41B","42B","43B","44B","44B","45B", "46B","47B","48B","49B")
#drop values contained in drop from df
seed_drop<-seed_count[!seed_count$plantID %in%drop,]
seed_propdrop<-seed_prop[!seed_prop$plantID %in%drop,] 
  
  seed_count%>%
  ggplot(aes(treatment, count, fill=seed_fill))+
  geom_boxplot()+
  labs(x="treatment" ,y="Total Seeds produced")+
  ggtitle("Fig. 5: # Seeds by treatment")
  
  seed_drop%>%
    ggplot(aes(treatment, count/heads, fill=seed_fill))+
    geom_boxplot()+
    labs(x="treatment" ,y="Total Seeds produced")+
    ggtitle("Fig. 5: # Seeds by treatment",subtitle="remove added individuals")
  
  seed_propdrop%>%
    ggplot(aes(treatment, (full/total_fruit), fill=treatment))+
    geom_boxplot()+
    labs(x="treatment" ,y="Prop seeds filled")+
    ggtitle("Fig. 5: # Seeds by treatment",subtitle="remove added individuals")
  
  seed_propdrop%>%filter(!is.na(treatment))%>%
    ggplot(aes(treatment, (full/total_fruit)/heads, fill=treatment))+
    geom_boxplot()+
    labs(x="treatment" ,y="Prop seeds filled/heads")+
    ggtitle("Fig. 5: # Seeds by treatment",subtitle="remove added individuals")
  
  seed_count%>%
    ggplot(aes(treatment, count/heads, fill=seed_fill))+
    geom_boxplot()+
    labs(x="treatment" ,y="Total Seeds produced/#floral heads")+
    ggtitle("Fig. 5b: # Seeds by treatment",subtitle="offsetting for # of heads per plant")
  
  seed_prop%>%filter(!is.na(treatment))%>%
    ggplot(aes(treatment, ((full)/total_fruit)/heads, fill=treatment))+
    geom_boxplot()+
    labs(x="treatment" ,y="Prop Seeds Fill")+
    ggtitle("Fig. 5: # Seeds by treatment")
