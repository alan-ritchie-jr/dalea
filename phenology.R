library(tidyverse)
#install.packages("mateable")
library(mateable)
library(lubridate)
library(lme4)
library(DHARMa)
library(glmmTMB)
library(nlme)
library(car)
library(sjPlot)
library(ggeffects)
library(sjmisc)
library(sjlabelled)
library(snakecase)
library(DHARMa)
cobloom<-read.csv("data/non_database_csvs/dalea-coblooming-density_25march2019.csv")

con_dens<-read.csv("data/non_database_csvs/dalea-conspecific-density_23March2019.csv")

focal_plt<-read.csv("data/non_database_csvs/focal_plant_stigma_26March2019.csv")


### 1st create dataframe for augspurger synchrony measure
### 2nd create dataframe for bishop synchrony measure

#1st: augspurger requires: 
# n- the number plants in "defined" population,
# f, the number of days plant i is in flower, and 
# e- the number days plant i overlaps with another plant, summed across all days

# this can be calculated in mateable()
# we need a dataframe with: 
# plantID
# start day of flowering
# last day of flowering

#create a summation of flowers, a treatment column, and a yday column
focals<-focal_plt%>%
  rename(bloom_heads=X.bloom_flwheads)%>%
  mutate(total_flws_est=head1+head2+head3+head4+head5, 
         treatment=ifelse(grepl("UB",plantID),'UB','B'))%>%
  mutate(date=paste(year,month,day, sep="-"))%>%mutate(ymd=ymd(date),yday=yday(ymd))

#summarize for a count of blooming focal plants per day and add yday component
foc_dates<-focals%>%filter(yday!=190)%>%filter(plantID!="42UB")%>%#42UB has no flowers so we have to drop from analyses
  select(ymd,yday,treatment,bloom_heads,plantID)%>%
  group_by(plantID,yday,ymd, treatment)%>%
  summarize(bloom_heads)

foc_date_wide<-focals%>%filter(yday!=190)%>%
  select(yday,ymd,treatment,bloom_heads,plantID)%>%
  group_by(plantID,yday,ymd,treatment)%>%
  summarize(bloom_heads)%>%
  spread(yday,bloom_heads)
### use spread data to manually confirm whether correct end date is selected 

# #these are lists of each plant in each subpopulation; use these to make a new variable

pop1<-c("1B","2B","3B","4B","5B","6B","7B","8B",
        "9B","10B","31B","32B","33B","46B","47B","48B")
pop2<-c("11B","12B","13B","14B","15B","16B","17B","18B",
        "19B","20B","34B","35B","36B","43B","44B","45B")
pop3<-c("21B","22B","23B","24B","25B","26B","27B","28B",
        "29B","30B","37B","38B","39B","40B","41B","42B")


pop4<-c("1UB","2UB","3UB","4UB","5UB","6UB","7UB","8UB",
        "9UB","10UB","31UB","32UB","33UB","46UB","47UB","48UB")
pop5<-c("11UB","12UB","13UB","14UB","15UB","16UB","17UB","18UB",
        "19UB","20UB","34UB","35UB","36UB","43UB","44UB","45UB")
pop6<-c("21UB","22UB","23UB","24UB","25UB","26UB","27UB","28UB",
        "29UB","30UB","37UB","38UB","39UB","40UB","41UB","42UB")

            
flw_first_last<- foc_dates %>% group_by(plantID,treatment) %>%
  summarise(firstDay=ymd[first(which(bloom_heads>=1))],
            lastDay=ymd[last(which(bloom_heads >= 1))]) %>%ungroup()%>%
  mutate(pop=ifelse(plantID %in%pop1, "pop1",
             ifelse(plantID %in%pop2, "pop2", 
             ifelse(plantID %in%pop3, "pop3", 
            ifelse(plantID %in%pop4, "pop4",
             ifelse(plantID %in%pop5, "pop5","pop6"))))))%>%as.data.frame()


b_flw<-flw_first_last%>%filter(treatment=="B")
ub_flw<-flw_first_last%>%filter(treatment=="UB")

# for each subpop let's also make a dataframe,
#so we can compare synchrony and other measures within subsets

b_flw1<-b_flw%>%filter(pop=="pop1")
b_flw2<-b_flw%>%filter(pop=="pop2")
b_flw3<-b_flw%>%filter(pop=="pop3")

ub_flw4<-ub_flw%>%filter(pop=="pop4")
ub_flw5<-ub_flw%>%filter(pop=="pop5")
ub_flw6<-ub_flw%>%filter(pop=="pop6")




# step 2 make mating scene

#full scene
full_scene<-makeScene(flw_first_last, startCol="firstDay",
                      endCol="lastDay",idCol="plantID")


#Burned scene


Bscene<-makeScene(b_flw, startCol="firstDay",
                endCol="lastDay",idCol="plantID")# add sub pop variable as a "year" column to compare synchrony within subpop )
#unburned scene
Uscene<-makeScene(ub_flw, startCol="firstDay",endCol="lastDay",idCol="plantID")

#subpops

#burn
Bscene1<-makeScene(b_flw1, startCol="firstDay",
                  endCol="lastDay",idCol="plantID")
Bscene2<-makeScene(b_flw2, startCol="firstDay",
                   endCol="lastDay",idCol="plantID")
Bscene3<-makeScene(b_flw3, startCol="firstDay",
                   endCol="lastDay",idCol="plantID")

spBscene<- list('1' = Bscene1,'2' = Bscene2, '3' = Bscene3)


###unburn
Uscene4<-makeScene(ub_flw4, startCol="firstDay",
                   endCol="lastDay",idCol="plantID")
Uscene5<-makeScene(ub_flw5, startCol="firstDay",
                   endCol="lastDay",idCol="plantID")
Uscene6<-makeScene(ub_flw6, startCol="firstDay",
                   endCol="lastDay",idCol="plantID")

spUscene<- list('4' = Uscene4,'5' = Uscene5, '6' = Uscene6)


### plot general "scene"
plotScene(Bscene, "t")#check github
plotScene(Uscene,"t")

#subpop plots
plotScene(spBscene, "t")
plotScene(spUscene, "t")

plotScene(full_scene, "t")

#
###mating summaries

b_sum<-matingSummary(Bscene, type="t")

ub_sum<-matingSummary(Uscene,type="t")

spB_sum<-matingSummary(spBscene, type="t")

spU_sum<-matingSummary(spUscene, type="t")


#look at all the things in a summary!
b_sum$sdDur
b_sum$peak
ub_sum$sdDur
ub_sum$peak
b_sum$meanSD
ub_sum$meanSD
ub_sum$meanDur
ub_sum$sdDur
b_sum$meanDur
b_sum$sdDur
b_sum$meanSD
ub_sum$meanSD
b_sum$sdSD
ub_sum$sdSD


#full synchrony
fsynca<- synchrony(full_scene, "augspurger")
fsynco<- synchrony(full_scene, "overlap")

#syncrony with augspurger
bSyncA <- synchrony(Bscene, "augspurger")
hist(bSyncA$ind[, 2], 30)
abline(v = bSyncA$pop, col ="red", lwd = 2)
abline(v = synchrony(Bscene, "augspurger", averageType = "median")$pop,
       col = "blue", lwd = 2)

bSynck<-synchrony(Bscene, "kempenaers")
hist(bSynck$ind[, 2], 30)
abline(v = bSynck$pop, col ="red", lwd = 2)
abline(v = synchrony(Bscene, "kempenaers", averageType = "median")$pop,
       col = "blue", lwd = 2)

bSyncO<- synchrony(Bscene, "overlap")
hist(bSyncO$ind[, 2], 30)
abline(v = bSyncO$pop, col ="red", lwd = 2)
abline(v = synchrony(Bscene, "overlap", averageType = "median")$pop,
       col = "blue", lwd = 2)



##Unburned

ubSyncA<- synchrony(Uscene, "augspurger")
hist(ubSyncA$ind[, 2], 30)
abline(v = ubSyncA$pop, col ="red", lwd = 2)
abline(v = synchrony(Uscene, "augspurger", averageType = "median")$pop,
       col = "blue", lwd = 2)
abline(v = synchrony(Bscene, "augspurger", averageType = "median")$pop,
       col = "green", lwd = 2)


ubSyncO<- synchrony(Uscene, "overlap")
hist(ubSyncO$ind[, 2], 30)
abline(v = ubSyncO$pop, col ="red", lwd = 2)
abline(v = synchrony(Uscene, "overlap", averageType = "median")$pop,
       col = "blue", lwd = 2)
abline(v = synchrony(Bscene, "overlap", averageType = "median")$pop,
       col = "green", lwd = 2)

ubSynck<-synchrony(Uscene, "kempenaers")
ubSyncsp<-synchrony(Uscene, "sync_prop")


### subpops
spBscene

spUscene


## use this and extract it as "subpop_synchrony"
sp_bSyncA<- synchrony(spBscene, "augspurger")
hist(sp_bSyncA$`1`$ind[, 2], 30)
abline(v = sp_bSyncA$`1`$pop, col ="red", lwd = 2)
abline(v = synchrony(spBscene, "augspurger", averageType = "mean")$`2`$pop,
       col = "purple", lwd = 2)
abline(v = synchrony(spBscene, "augspurger", averageType = "mean")$`3`$pop,
       col = "green", lwd = 2)



sp_bSyncO<- synchrony(spBscene, "overlap")
hist(sp_bSyncO$`1`$ind[, 2], 30)
abline(v = sp_bSyncO$`1`$pop, col ="red", lwd = 2)
abline(v = synchrony(spBscene, "overlap", averageType = "median")$`1`$pop,
       col = "blue", lwd = 2)
abline(v = synchrony(spBscene, "overlap", averageType = "median")$`2`$pop,
       col = "purple", lwd = 2)
abline(v = synchrony(spBscene, "overlap", averageType = "median")$`3`$pop,
       col = "green", lwd = 2)

#unburned

sp_uSyncA<- synchrony(spUscene, "augspurger")
hist(sp_uSyncA$`4`$ind[, 2], 30)
abline(v = sp_uSyncA$`4`$pop, col ="red", lwd = 2)
abline(v = synchrony(spUscene, "augspurger", averageType = "mean")$`5`$pop,
       col = "purple", lwd = 2)
abline(v = synchrony(spUscene, "augspurger", averageType = "mean")$`6`$pop,
       col = "green", lwd = 2)

sp_uSyncO<- synchrony(spUscene, "overlap")
hist(sp_uSyncO$`4`$ind[, 2], 30)
abline(v = sp_uSyncO$`4`$pop, col ="red", lwd = 2)
abline(v = synchrony(spUscene, "overlap", averageType = "mean")$`5`$pop,
       col = "purple", lwd = 2)
abline(v = synchrony(spUscene, "overlap", averageType = "mean")$`6`$pop,
       col = "green", lwd = 2)





### Pieces to take for plotting:

#scenes for duration, startday, main thing you want
bscene<-Bscene
ub_scene<-Uscene

#join these for plotting
scene<-bscene%>%bind_rows(ub_scene)%>%mutate(plantID=id)
#now join with 

scene_trt<-scene%>%full_join(flw_first_last, by="plantID")
# population summaries of duration peak etc.
#these can be spread to each treatment to calcluate other synchrony measures?
b_sum
ub_sum

#synchrony of individuals; to get pop synchrony take mean or median 
#change synchrony variable names and drop old synchrony
ubSynca<-ubSyncA$ind%>%rename(syn_aug=synchrony)# just the mean of individuals

ubSynco<-ubSyncO$ind%>%rename(syn_o=synchrony)

ubsync<-ubSynca%>%full_join(ubSynco, by="id")


bSynca<-bSyncA$ind%>%rename(syn_aug=synchrony)# just the mean of individuals

bSynco<-bSyncO$ind%>%rename(syn_o=synchrony)

bsync<-bSynca%>%full_join(bSynco, by="id")


sync<-bsync%>%bind_rows(ubsync)

# now join with scene_trts
syn_scene<-scene_trt%>%full_join(sync,by="id")
#now a different sync measure--> sync within subpop

#quickest way may be to extract each of the 3 pop$ind combos from the object, row_bind
usp_synca<-sp_uSyncA$`4`$ind%>%bind_rows(sp_uSyncA$`5`$ind)%>%bind_rows(sp_uSyncA$`6`$ind)%>%
  rename(syn_aug_sp=synchrony)# just the mean of individuals

usp_Synco<-sp_uSyncO$`4`$ind%>%bind_rows(sp_uSyncO$`5`$ind)%>%bind_rows(sp_uSyncO$`6`$ind)%>%
  rename(syn_o_sp=synchrony)

usp_sync<-usp_synca%>%full_join(usp_Synco, by="id")


bsp_synca<-sp_bSyncA$`1`$ind%>%bind_rows(sp_bSyncA$`2`$ind)%>%bind_rows(sp_bSyncA$`3`$ind)%>%
  rename(syn_aug_sp=synchrony)# just the mean of individuals

bsp_Synco<-sp_bSyncO$`1`$ind%>%bind_rows(sp_bSyncO$`2`$ind)%>%bind_rows(sp_bSyncO$`3`$ind)%>%
  rename(syn_o_sp=synchrony)

bsp_sync<-bsp_synca%>%full_join(bsp_Synco, by="id")


sp_sync<-bsp_sync%>%bind_rows(usp_sync)

### now join with syn_scene and we have a full data frame!
pheno_sync<-syn_scene%>%full_join(sp_sync,by="id")


#### 
# let's get some plots going of b/ub and averages 



pheno_sync%>%ggplot(aes(treatment, duration))+geom_violin()

#duration with subpops split out

pheno_sync%>%ggplot(aes(pop, duration,fill=treatment))+geom_boxplot()

pheno_sync%>%ggplot(aes(pop, duration,fill=treatment))+geom_boxplot()


pheno_sync%>%ggplot(aes(pop, syn_aug,fill=treatment))+geom_boxplot()

pheno_sync%>%ggplot(aes(treatment, syn_aug,fill=treatment))+geom_boxplot()

pheno_sync%>%ggplot(aes(pop, syn_aug_sp,fill=treatment))+geom_boxplot()

pheno_sync%>%ggplot(aes(treatment, start,fill=treatment))+geom_boxplot()

summary(glmer(syn_aug~treatment+(1|pop),family="quasibinomial",data=pheno_sync))#look up quasibinomial



### pollen data

ID_trt<-focals%>%select(plantID, treatment)%>%group_by(plantID,treatment)%>%summarize()

#anti_join to see who has no stigma data
stig_check<-poll_stig%>%anti_join(ID_trt, c("plantID")) #check this one; likely 5 or 30 B
#30B was 50B; manually changed in csv because having issues
# ADD TO OPENREFINE.
# Also check NAs vs 0s in open refine
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
                                   unknown_hetero_pollen))
#so some plants we have few stigma, and others we have more.
# Think about how to model when exposure is so different?

  #so some plants we have few stigma, and others we have more.
# Think about how to model when exposure is so different?

  stig_seed_sync<-stig%>%left_join(seed_sync, c("plantID","treatment"))


  
###average dalea pollen deposited per stigma per plant
  stig_seed_sync%>% ggplot(aes(treatment, mean_dalpur,fill=treatment))+geom_boxplot()
  #average hetero per stigma per plant
  stig_seed_sync%>% ggplot(aes(treatment, mean_hetero,fill=treatment))+geom_boxplot() 
  #average amocan per stigma per plant
  stig_seed_sync%>% ggplot(aes(treatment, mean_amocan,fill=treatment))+geom_boxplot() 


#histograms 
#distribution of mean dalea pollen deposited   
 stig_seed_sync%>%ggplot(aes(mean_dalpur,color=treatment))+ 
    geom_histogram()+facet_grid(.~treatment)
 #distribution of mean amocan pollen deposted
 stig_seed_sync%>%ggplot(aes(sum_amocan/n_stig,color=treatment))+ 
   geom_histogram()+facet_grid(.~treatment)
 #hetero
 stig_seed_sync%>%ggplot(aes(mean_hetero,color=treatment))+ 
   geom_histogram()+facet_grid(.~treatment) 
 #mean total
 stig_seed_sync%>%ggplot(aes(mean_total_pollen,color=treatment))+ 
   geom_histogram()+facet_grid(.~treatment) 
 ## lotta zeroes in these data to deal with 
  
 ### average dalea pollen deposited as a function of synchrony
 stig_seed_sync%>%ggplot(aes(syn_aug,mean_dalpur,color=treatment))+
   geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")
 # pretty flat, local density probably more important
 
### average dalea pollen deposted as a function of start day
 stig_seed_sync%>%ggplot(aes(start,mean_dalpur,color=treatment))+
   geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")
 # given that
 
 stig_seed_sync%>%ggplot(aes(start,mean_amocan,color=treatment))+
   geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")
 
 
 stig_seed_sync%>%ggplot(aes(start,mean_hetero,color=treatment))+
   geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")
 
 
 #### modeling pollen deposition:
 
 ### stigma level: Pollen can be modelted as a bernoulli (0/1, absent or present)
 
 #plant level average over the whole season
 
 #stigma level: proportion of total pollen deposited that was amorpha vs dalea?
 
 
 # zero inflation seems to be an issue? 
 
 #overdispersion could be dealt with by an individual level random effect as in the seed data
 
 
 #see what data allow for---
 
 #
# model of the proportion of amorpha pollen/total pollen summed over all stigma for each plant
 stig_mod<-glmer(sum_amocan/sum_total_pollen~scale(start)*treatment+(1|pop),
                      weights=sum_total_pollen,
                      family=binomial,data=stig_seed_sync)
 summary(stig_mod)
 
 sim_stig<-simulateResiduals(fittedModel = stig_mod,n=1000)
 
 plot(sim_stig)
 
 testDispersion(sim_stig)
 testZeroInflation(sim_stig) #zero inflation test comes back significant
 
 
 #with observation level random effect
 
 stig_mod_olre<-glmer(sum_amocan/sum_total_pollen~scale(start)*treatment+(1|pop)+(1|plantID),
                 weights=sum_total_pollen,
                 family=binomial,data=stig_seed_sync)
summary(stig_mod_olre)
 
 sim_stig_olre<-simulateResiduals(fittedModel = stig_mod_olre,n=1000)
 
 plot(sim_stig_olre)
 
 testDispersion(sim_stig_olre)
 testZeroInflation(sim_stig_olre) # fixed overdispersion due to zero inflation
 
 #parameter estimates change a bit, but overall message of the model doesn't seem to change.
 

 
 #adding observation level random effect controls for the zero inflation, which is significant according to DHARMa
 
 #### Plot this dang stigma model
 
 
 s<-get_model_data(stig_mod,type="pred",terms=c("start","treatment"), 
                   pred.type="re", colors= "bw",ci.lvl= .95) 
 
s<-ggpredict(stig_mod_olre,type="re",terms=c("start","treatment"))
 #re=random effect conditioned 
 
 
 ### make separate dataframes for CI for plot
 sb<-s%>%filter(group=="B")
 sub<-s%>%filter(group=="UB")
 
 ###make plot for seed model
 ggplot(data=stig_seed_sync,aes(start, (sum_amocan/sum_total_pollen)))+
   geom_point(aes(shape=treatment),position="jitter")+
   geom_line(data=s, aes(x, predicted,linetype=group))+
   #geom_ribbon(data=sb,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
   #geom_ribbon(data=sub,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
   labs(shape="Treatment",linetype="Predicted Response")+
   xlab("Flowering Start Date")+ylab("Amorpha Pollen Grains/Total Pollen Grains Deposited")+
   ggtitle("Effect of Start Date on Amorpha Pollen Deposition",
           subtitle="Predicted Response vs. Data")
 
 
### seleciton gradient
 
 
 
 
 
 
 
 #########################
#seedchrony

seed_check<-seed%>%anti_join(ID_trt, c("plantID")) # should only be controls

colnames(seed)
### calculate seed prod difference for each group, accoutn for # heads
seed_count<-seed%>%left_join(ID_trt, c("plantID"))%>%
  rename(full=X.full_seeds, part=X.partial_full_seeds,empty=X.aborted_seeds,
         heads=Num_Heads,abrt_heads=Aborted_Heads,dehisc_heads=Dehisced_Heads)%>%
  gather(key = "seed_fill", value = "count",full,part,empty)%>% 
  replace_na(list(treatment= "Control"))

#without


seed_prop<-seed%>%left_join(ID_trt, c("plantID"))%>%filter(plantID!="8UBC")%>%
  replace_na(list(treatment= "Control"))%>%filter(treatment!="Control")%>%
  rename(full=X.full_seeds, part=X.partial_full_seeds,empty=X.aborted_seeds,
         heads=Num_Heads,abrt_heads=Aborted_Heads,dehisc_heads=Dehisced_Heads)%>% 
  mutate_all(funs(replace(., is.na(.), 0)))%>%group_by(plantID)%>%
summarise(n_collection_events=n(),heads=sum(heads),full=sum(full), part=sum(part), empty=sum(empty),
          total_fruit=full+part+empty, filling_frt_prop=(full+part)/total_fruit,
         seed_prop=full/total_fruit)

#join seed data with synchrony

seed_sync<-pheno_sync%>%left_join(seed_prop,by="plantID")



seed_sync%>% ggplot(aes(treatment, seed_prop,fill=treatment))+geom_boxplot()

seed_sync%>% ggplot(aes(pop, seed_prop,fill=treatment))+geom_boxplot()


seed_sync%>% ggplot(aes(treatment, filling_frt_prop,fill=treatment))+geom_boxplot()

seed_sync%>% ggplot(aes(pop, filling_frt_prop,fill=treatment))+geom_boxplot()

### seed_prop plots
seed_sync%>%ggplot(aes(syn_aug,seed_prop,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

seed_sync%>%ggplot(aes(syn_aug,filling_frt_prop,color=treatment))+
  facet_grid(.~treatment)+geom_point()+stat_smooth(method="lm")

seed_sync%>%ggplot(aes(start,seed_prop,color=treatment))+geom_point()+
  facet_grid(.~treatment)+stat_smooth(method="lm")

seed_sync%>%ggplot(aes(start,syn_aug,color=treatment))+geom_point()+
  facet_grid(.~treatment)+stat_smooth(method="lm")

#prop seeds vs start date
seed_sync%>%ggplot(aes(start,full/total_fruit,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

#full seeds vs start date
seed_sync%>%ggplot(aes(start,full,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")
#the number of seeds produced decreases at almost the same rate for each treatment
#histogram
seed_sync%>%ggplot(aes(seed_prop,color=treatment))+ 
  geom_histogram()+facet_grid(.~treatment)

#total filled seeds vs 
seed_sync%>%ggplot(aes(heads,seed_prop,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

sync_cor<-seed_sync%>%select(start, duration, syn_aug)%>%cor()

##models
trt_mod<-glmer(full/total_fruit~treatment+(1|pop)+(1|id),
              weights=total_fruit,family=binomial,data=seed_sync)

summary(trt_mod)
sim_trt<-simulateResiduals(fittedModel = trt_mod,n=1000)

plot(sim_trt)

testDispersion(sim_trt)


summary(glmer(full/total_fruit~start*treatment+(1|pop),
              weights=total_fruit,family=binomial,data=seed_sync))
        
        
summary(glmer((full+part)/total_fruit~start*treatment+(1|pop),
              weights=total_fruit,family=binomial,data=seed_sync))

# show to dan, check assumption
#check overdispersion
#from bolker faq
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}


#try out observation level random effect to deal with overdispersion

mod1<-glmer(full/total_fruit~start*treatment+(1|pop),
            weights=total_fruit,family=binomial,data=seed_sync)
summary(mod1)

mod1_olre<-glmer(full/total_fruit~scale(start)*treatment+(1|pop)+ (1|id),
            weights=total_fruit,family=binomial,data=seed_sync)
summary(mod1_olre)

mod2<- glmer((full+part)/total_fruit~scale(start)*treatment+(1|pop)+(1|id),
                weights=total_fruit,family=binomial,data=seed_sync)
summary(mod2)

overdisp_fun(mod1)
overdisp_fun(mod2)


gof(mod1)
gof(mod1s)
gof(mod2)


c_start<-seed_sync$start-mean(seed_sync$start)
c_start
s_start<-scale(seed_sync$start)
s_start
##dharma check of overdisp

sim_mod1<-simulateResiduals(fittedModel = mod1,n=1000)

plot(sim_mod1)

testDispersion(sim_mod1)

sim_mod1_olre<-simulateResiduals(fittedModel = mod1_olre,n=1000)

plot(sim_mod1_olre)

testDispersion(sim_mod1_olre)

sim_mod2<-simulateResiduals(fittedModel = mod2,n=1000)

plot(sim_mod2)

testDispersion(sim_mod2)

sim_mod3<-simulateResiduals(fittedModel = mod3,n=1000)
plot(sim_mod3)
testDispersion(sim_mod3)


### plot model
### figure out why this doesn;t work!

s<-ggpredict(mod1_olre,type="re",terms=c("start","treatment"))

plot(s)

ggplot(s, aes(x, predicted, colour = group)) + geom_line()+
  geom_point(data=stig_seed_sync,aes(shape=treatment),position="jitter")

s<-plot_model(mod1_olre, type = "pred", terms = c("start"), pred.type = "re")

#re=random effect conditioned 


### make separate dataframes for CI for plot
sb<-s%>%filter(group=="B")
sub<-s%>%filter(group=="UB")

###make plot for seed model
ggplot(data=stig_seed_sync,aes(start, (full/total_fruit)))+
  geom_line(data=s, aes(x, predicted,linetype=group))+
  geom_point(aes(shape=treatment),position="jitter")+
  geom_line(data=s, aes(x, predicted,linetype=group))+
  #geom_ribbon(data=sb,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  #geom_ribbon(data=sub,aes(x=x,ymin=conf.low, ymax=conf.high),alpha = 0.3,inherit.aes = FALSE)+
  labs(shape="Treatment",linetype="Predicted Response")+
  xlab("Flowering Start Date")+ylab("Amorpha Pollen Grains/Total Pollen Grains Deposited")+
  ggtitle("Effect of Start Date on Amorpha Pollen Deposition",
          subtitle="Predicted Response vs. Data")









## correlation between synchrony measures
library(reshape2)
melted_cormat <- melt(sync_cor)
head(melted_cormat)


ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()



