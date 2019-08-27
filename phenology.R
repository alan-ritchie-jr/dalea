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

#seed count data
seed<-read.csv("data/non_database_csvs/seed-counts_26March2019.csv")

#updated seed count data with all seed counts

seed2<-read.csv("data/non_database_csvs/dalea_seed_counts_7May2019.csv")

### this will create synchrony measures 
### and walk through the data and summaries created by mateable
### note this just uses the focal plant floral data
### seed_syncrhony, and stigma_seed_synchrony will merge the synchrony data to the other dataframes


### 1st we will create dataframe for augspurger synchrony measure

#1st: augspurger requires: 
# n- the number plants in "defined" population,
# f, the number of days plant i is in flower, and 
# e- the number days plant i overlaps with another plant, summed across all days

# this can be calculated in mateable()
# all we need a dataframe with: 
# plantID
# start day of flowering
# last day of flowering

#create a summation of flowers, a treatment column, and a yday column
focals<-focal_plt%>%
  rename(bloom_heads=X.bloom_flwheads)%>%
  mutate(total_flws_est=head1+head2+head3+head4+head5, 
         treatment=ifelse(grepl("UB",plantID),'UB','B'))%>%
  mutate(date=paste(year,month,day, sep="-"))%>%mutate(ymd=ymd(date),yday=yday(ymd))

#summarize a count of blooming focal plants per day and add yday component
foc_dates<-focals%>%filter(yday!=190)%>%# can't remember why this is filtered
  filter(plantID!="42UB")%>%#42UB has no flowers so we have to drop from analyses
  select(ymd,yday,treatment,bloom_heads,plantID)%>%
  group_by(plantID,yday,ymd, treatment)%>%
  summarize(bloom_heads)

#wide format 
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


 
 
 
 
 
