#distributions of data at daily level vs overall mean level

# run phenology and data_merge first!

library(lubridate)
library(tidyverse)

#phenology measures (and some trait means)
focal_pheno<-read.csv("data/focal_pheno.csv")

#focal plant flowering, pollination, and stigma data
focal_plt<-read.csv("data/non_database_csvs/focal_plant_stigma_26March2019.csv")


#updated seed data
seed2<-read.csv("data/non_database_csvs/dalea_seed_counts_7May2019.csv")

cobloom<-read.csv("data/non_database_csvs/dalea-coblooming-density_25march2019.csv")


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


#### first: Phenology

###distribution of phenology data
view(focal_pheno)

focal_pheno%>%ggplot(aes(treatment,mean_date_heads,fill=treatment))+geom_boxplot()+
  ggtitle("Mean date of flowering by # flw heads")

focal_pheno%>%ggplot(aes(treatment,mean_date_flw,fill=treatment))+geom_boxplot()+
  ggtitle("Mean date of flowering by # flws")

#distribution of mean dates of flowering 
focal_pheno%>%ggplot(aes(mean_date_flw,fill=treatment))+geom_density()+
  ggtitle("Distribution of mean date of flowering by # flws")+facet_grid(.~treatment)

#as a histogram
focal_pheno%>%ggplot(aes(mean_date_flw,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of mean date of flowering by # flws")+facet_grid(.~treatment)


#flowering display
#first up: mean_flw_heads: mean # of flower heads displayed by a plant per day
focal_pheno%>%ggplot(aes(treatment,mean_flowering_heads,fill=treatment))+geom_boxplot()+
  ggtitle("Mean flowering heads")
### slightly more for unburned

#distribution of mean flowering heads
focal_pheno%>%ggplot(aes(mean_flowering_heads,fill=treatment))+geom_density()+
  ggtitle("Distribution of mean flowering heads")+facet_grid(.~treatment)

#as a histogram
focal_pheno%>%ggplot(aes(mean_flowering_heads,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of mean flowering heads")+facet_grid(.~treatment)

#### max flowering
focal_pheno%>%ggplot(aes(treatment,max_flowering_heads,fill=treatment))+geom_boxplot()+
  ggtitle("Max flowering heads")
### slightly more for unburned

#distribution of mean flowering heads
focal_pheno%>%ggplot(aes(max_flowering_heads,fill=treatment))+geom_density()+
  ggtitle("Distribution of max flowering heads")+facet_grid(.~treatment)

#as a histogram
focal_pheno%>%ggplot(aes(max_flowering_heads,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of max flowering heads")+facet_grid(.~treatment)

#########
#### the mean of estimated daily flower production
focal_pheno%>%ggplot(aes(treatment,mean_daily_flowers,fill=treatment))+geom_boxplot()+
  ggtitle("Mean Daily flower presentation")
### slightly more for unburned

#distribution of mean flowering heads
focal_pheno%>%ggplot(aes(mean_daily_flowers,fill=treatment))+geom_density()+
  ggtitle("Distribution of mean daily flowers")+facet_grid(.~treatment)

#as a histogram
focal_pheno%>%ggplot(aes(max_flowering_heads,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of mean daily flowers")+facet_grid(.~treatment)



#################################################
#### Second: Focal plant traits--
#####################

is.numeric(focals$bloom_heads)
focal_plt$X.bloom_flwheads<-as.numeric(focal_plt$X.bloom_flwheads)

#create a summation of flowers, a treatment column, and a yday colum,
focals<-focal_plt%>%
  rename(bloom_heads=X.bloom_flwheads)%>%filter(!is.na(head1))%>%
  mutate(total_flws_est=
           ifelse(bloom_heads > 5,
                  ((head1+head2+head3+head4+head5)/5)*bloom_heads,(head1+head2+head3+head4+head5)), 
         treatment=ifelse(grepl("UB",plantID),'UB','B'))%>%
  mutate(date=paste(year,month,day, sep="-"))%>%mutate(ymd=ymd(date),yday=yday(ymd))



#### # of blooming heads per plant per round 
focals%>%ggplot(aes(treatment,bloom_heads,fill=treatment))+geom_boxplot()+
  ggtitle("Daily Flowering Heads")
###
#over time
focals%>%filter(!is.na(round))%>%ggplot(aes(as.factor(round),bloom_heads,fill=treatment))+geom_boxplot()+
  ggtitle("Daily Flowering Heads",subtitle="change over time")


#distribution of mean flowering heads
focals%>%ggplot(aes(bloom_heads,fill=treatment))+geom_density()+
  ggtitle("Distribution of daily flowering heads")+facet_grid(.~treatment)

#as a histogram
focals%>%ggplot(aes(bloom_heads,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of daily flowering heads")+facet_grid(.~treatment)


#### overall bloom_heads on a per round per plant basis mostly matches the mean measure

############################
###estimated total flowers 
####################################

focals%>%ggplot(aes(treatment,total_flws_est,fill=treatment))+geom_boxplot()+
  ggtitle("Total Flowers")
###pretty zero heavy

#distribution of total flowers
focals%>%ggplot(aes(total_flws_est,fill=treatment))+geom_density()+
  ggtitle("Distribution of total flowers")+facet_grid(.~treatment)


focals%>%filter(!is.na(round))%>%ggplot(aes(as.factor(round),total_flws_est,fill=treatment))+geom_boxplot()+
  ggtitle("Daily total flowers",subtitle="change over time")

#as a histogram
focals%>%ggplot(aes(total_flws_est,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of total flowers")+facet_grid(.~treatment)


#### drop zeroes real quick
focals%>%filter(total_flws_est>0)%>%ggplot(aes(treatment,total_flws_est,fill=treatment))+geom_boxplot()+
  ggtitle("Distribution of total flowers",subtitle="excluding zero flower days")
###

#distribution of mean flowering heads
focals%>%filter(total_flws_est>0)%>%ggplot(aes(total_flws_est,fill=treatment))+geom_density()+
  ggtitle("Distribution of total flowers",subtitle="excluding zero flower days")+
  facet_grid(.~treatment)

#as a histogram
focals%>%filter(total_flws_est>0)%>%ggplot(aes(total_flws_est,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of total flowers",subtitle="excluding zero flower days")+facet_grid(.~treatment)


###similar overall appearance to mean value.

### size can be calculated from # of stems (have to check for miscounts somehow)
### or size can be calculated from # of heads
###
### so one rare issue: daily flower presentation == 0, but flowers opened on subsequent visit observations.

#######################
#### now looking at mean vs daily (AKA per round) pollen deposition on stigma
###########################

ID_trt<-focals%>%select(plantID, treatment)%>%group_by(plantID,treatment)%>%summarize()

#anti_join to see who has no stigma data
stig_check<-poll_stig%>%anti_join(ID_trt, c("plantID")) #should be 0


###now make a dataframe of per day pollen observations
stig_daily_mean<-poll_stig%>%left_join(ID_trt, c("plantID"))%>%filter(!is.na(dpurp_pollen))%>%
  group_by(plantID,round,month,day,year,treatment)%>%
  mutate(date=paste(month,day,year,sep="-"))%>%
  mutate(mdy=mdy(date),yday=yday(mdy), total_pollen=dpurp_pollen+amocan_pollen+
           aster_pollen+
           dcand_pollen+
           unknown_hetero_pollen)%>%
  group_by(plantID,treatment,yday,mdy)%>%
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
  
#structure of this data frame: for each plant, 
#on each round/day, the number of collected stigma, and a summary.

## mean of daily means
### vs mean overall
  
full_dalea_df%>%ggplot(aes(treatment,mean_dalpur,fill=treatment))+geom_boxplot()+
  ggtitle("Overall mean number of pollen grains")
  
################
###total number of stigma per day
#########################

stig_daily%>%ggplot(aes(treatment,n_stig,fill=treatment))+geom_boxplot()+
  ggtitle("Number of daily collected stigma")

###pretty zero heavy

#distribution of mean flowering heads
stig_daily%>%ggplot(aes(n_stig,fill=treatment))+geom_density()+
  ggtitle("Distribution of stigma collected daily")+facet_grid(.~treatment)

stig_daily%>%ggplot(aes(n_stig,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of stigma collected daily")+facet_grid(.~treatment)


############################
####daily total pollen deposition
#############################
stig_daily%>%ggplot(aes(treatment,mean_total_pollen,fill=treatment))+geom_boxplot()+
  ggtitle("Mean number of pollen grains")


stig_daily%>%ggplot(aes(as.factor(yday),mean_total_pollen,fill=treatment))+geom_boxplot()+
  ggtitle("Daily mean pollen deposited",subtitle="change over time")


###bivariate plot of sum against treatment
stig_daily%>%ggplot(aes(sum_total_pollen,mean_total_pollen,color=treatment))+geom_point()+
  geom_smooth(method="lm")+
  ggtitle("Sum vs mean of total pollen grains")
#You probably want to use mean so you can control for larger plants getting more flowers pulled per day
#Although, this sample should represent ~10% of flowers from a plant, so in a way you've already controlled for this


#distribution of mean total of pollen deposited daily
stig_daily%>%ggplot(aes(mean_total_pollen,fill=treatment))+geom_density()+
  ggtitle("Distribution of mean total pollen grains (daily)")+facet_grid(.~treatment)
### vs sum
stig_daily%>%ggplot(aes(sum_total_pollen,fill=treatment))+geom_density()+
  ggtitle("Distribution of the sum of total pollen grains (daily)")+facet_grid(.~treatment)
### sum has more variation (skinnier distribution spread over more values)
### Unsure if that can be problematioc or not

###
stig_daily%>%ggplot(aes(sum_total_pollen,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of mean total pollen grains (daily)")+facet_grid(.~treatment)

###now compare to mean overall
full_dalea_df%>%ggplot(aes(mean_total_pollen,fill=treatment))+geom_density()+
  ggtitle("Distribution of mean total pollen grains overall")+facet_grid(.~treatment)
# fatter, not centered near 0

#####
# NOTE: the full_dalea_df calculates an overall mean, rather than the mean of daily means.
### Should we calculated the mean of daily means?

### #########################
###dalea pollen grains daily
####################
stig_daily%>%ggplot(aes(treatment,mean_dalpur,fill=treatment))+geom_boxplot()+
ggtitle("Mean number of dalea grains (daily")

stig_daily%>%ggplot(aes(as.factor(yday),mean_dalpur,fill=treatment))+geom_boxplot()+
  ggtitle("Daily mean dalpur pollen deposited",subtitle="change over time")

###bivariate plot of sum against mean
stig_daily%>%ggplot(aes(sum_dalpur,mean_dalpur,color=treatment))+geom_point()+
  geom_smooth(method="lm")+
  ggtitle("Sum vs mean of dalea grains")
### slopes a bit different, but overall close


#distribution of mean dalea pollen
stig_daily%>%ggplot(aes(sum_dalpur,fill=treatment))+geom_density()+
  ggtitle("Distribution of mean dalea grains (daily)")+facet_grid(.~treatment)

###
stig_daily%>%ggplot(aes(mean_dalpur,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of mean dalea grains (daily)")+facet_grid(.~treatment)

###now compare to mean overall
full_dalea_df%>%ggplot(aes(mean_dalpur,fill=treatment))+geom_density()+
  ggtitle("Distribution of mean dalea grains overall")+facet_grid(.~treatment)
# same as total pollen--distribution wider and fewer 0 obs


####################
### amorpha
###############

stig_daily%>%ggplot(aes(treatment,mean_amocan,fill=treatment))+geom_boxplot()+
  ggtitle("Daily mean number of amorpha grains")

stig_daily%>%ggplot(aes(as.factor(yday),mean_amocan,fill=treatment))+geom_boxplot()+
  ggtitle("Daily mean amocan pollen deposited",subtitle="change over time")


###bivariate plot of sum against mean
stig_daily%>%ggplot(aes(sum_amocan,mean_amocan,color=treatment))+geom_point()+
  geom_smooth(method="lm")+
  ggtitle("Sum vs mean of amorpha grains")
### less clean, because more zeroes overall and in the ub data

#distribution of mean amorpha pollen
stig_daily%>%ggplot(aes(mean_amocan,fill=treatment))+geom_density()+
  ggtitle("Distribution of mean amocan grains (daily)")+facet_grid(.~treatment)

###
stig_daily%>%ggplot(aes(mean_amocan,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of mean amocan grains (daily)")+facet_grid(.~treatment)

###now compare to mean overall

full_dalea_df%>%ggplot(aes(mean_amocan,fill=treatment))+geom_density()+
  ggtitle("Distribution of mean amocan grains overall")+facet_grid(.~treatment)
# same as total pollen--distribution wider and fewer 0 obs

### awith histogram
full_dalea_df%>%ggplot(aes(mean_amocan,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of mean amocan grains overall")+facet_grid(.~treatment)
# same as total pollen--distribution wider and fewer 0 obs

#######
#Hetero pollen
######


stig_daily%>%ggplot(aes(treatment,mean_hetero,fill=treatment))+geom_boxplot()+
  ggtitle("Mean number of hetero grains (daily")

stig_daily%>%ggplot(aes(as.factor(yday),mean_hetero,fill=treatment))+geom_boxplot()+
  ggtitle("Daily mean hetero pollen deposited",subtitle="change over time")

###bivariate plot of sum against mean
stig_daily%>%ggplot(aes(sum_hetero,mean_hetero,color=treatment))+geom_point()+
  geom_smooth(method="lm")+
  ggtitle("Sum vs mean of hetero grains")

#bivariate plot of mean amorpha vs mean hetero
stig_daily%>%ggplot(aes(mean_amocan,mean_hetero,color=treatment))+geom_point()+
  geom_smooth(method="lm")+
  ggtitle("Mean amorpha vs mean hetero grains")
### amorpha drove heterospecific pollen deposition for burned plants
# but mean_hetero specific pollen depo doesn't display the same variance as unburned

#distribution of mean amorpha pollen
stig_daily%>%ggplot(aes(mean_hetero,fill=treatment))+geom_density()+
  ggtitle("Distribution of mean hetero grains (daily)")+facet_grid(.~treatment)

###
stig_daily%>%ggplot(aes(mean_hetero,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of mean hetero grains (daily)")+facet_grid(.~treatment)

###now compare to mean overall

full_dalea_df%>%ggplot(aes(mean_hetero,fill=treatment))+geom_density()+
  ggtitle("Distribution of mean hetero grains overall")+facet_grid(.~treatment)
# same as total pollen--distribution wider and fewer 0 obs

### awith histogram
full_dalea_df%>%ggplot(aes(mean_hetero,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of mean hetero grains overall")+facet_grid(.~treatment)
# same as total pollen--distribution wider and fewer 0 obs



####################
####visitation
#################


### first ensure variables are factors
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
visit_summary<-poll_vis%>%select(plantID, round_v, day.y,time, morphoID,date, touches,bloom_heads,treatment)%>%
  mutate(bee_fly=ifelse(morphoID%in%"Syrphid","fly",ifelse(is.na(morphoID),"none","bee")))%>%
  group_by(plantID,round_v,treatment,day.y)%>%
  summarise(n_visits=sum(!is.na(morphoID)),n_bee_visits=sum(bee_fly=="bee"), 
            n_fly_visits=sum(bee_fly=="fly"),
            n_obs=n(),
            flw_heads=(sum(bloom_heads))/n(), intervals=n_distinct(time),
            visit_head_min=(n_visits/flw_heads)/intervals,
            visit_min=n_visits/intervals)

#this is a per round summary

visit_summary%>%ggplot(aes(treatment,visit_min,fill=treatment))+geom_boxplot()+
  ggtitle("Visits per min (daily")

visit_summary%>%ggplot(aes(as.factor(day.y),visit_min,fill=treatment))+geom_boxplot()+
  ggtitle("Daily visits per min",subtitle="change over time")

#distribution of visit min
visit_summary%>%ggplot(aes(visit_min,fill=treatment))+geom_density()+
  ggtitle("Distribution of visits per min(daily)")+facet_grid(.~treatment)

###
visit_summary%>%ggplot(aes(visit_min,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of visits per min (daily)")+facet_grid(.~treatment)


###now compare to mean overall

full_dalea_df%>%ggplot(aes(mean_visit_min,fill=treatment))+geom_density()+
  ggtitle("Distribution of mean visits per min overall")+facet_grid(.~treatment)

### awith histogram
full_dalea_df%>%ggplot(aes(mean_visit_min,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of mean visits per min overall")+facet_grid(.~treatment)

#### how visits per head per minute

visit_summary%>%ggplot(aes(treatment,visit_head_min,fill=treatment))+geom_boxplot()+
  ggtitle("Daily visits per head per min")

visit_summary%>%ggplot(aes(as.factor(day.y),visit_head_min,fill=treatment))+geom_boxplot()+
  ggtitle("Daily visits per head per min",subtitle="change over time")

#distribution of mean amorpha pollen
visit_summary%>%ggplot(aes(visit_head_min,fill=treatment))+geom_density()+
  ggtitle("Distribution of daily visits per head per min")+facet_grid(.~treatment)

###
visit_summary%>%ggplot(aes(visit_min,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of daily visits per head per min")+facet_grid(.~treatment)


###now compare to mean overall

full_dalea_df%>%ggplot(aes(mean_visit_head_min,fill=treatment))+geom_density()+
  ggtitle("Distribution of mean visits per head per min overall")+facet_grid(.~treatment)

### awith histogram
full_dalea_df%>%ggplot(aes(mean_visit_head_min,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of mean visits per head per min overall")+facet_grid(.~treatment)

#this tracks pretty well with mean visits per min



#n bee visits

visit_summary%>%ggplot(aes(treatment,n_bee_visits/flw_heads/intervals,fill=treatment))+geom_boxplot()+
  ggtitle("Daily number of bee visits per head per interval")

visit_summary%>%ggplot(aes(as.factor(day.y),n_bee_visits/flw_heads/intervals,fill=treatment))+geom_boxplot()+
  ggtitle("Daily number of bee visits per head per interval",subtitle="change over time")

#distribution of bee visits
visit_summary%>%ggplot(aes(n_bee_visits/flw_heads/intervals,fill=treatment))+geom_density()+
  ggtitle("Distribution of bee visits per head per interval")+facet_grid(.~treatment)


### looks like bee visits per head per interval are higher for burned plants
###
visit_summary%>%ggplot(aes(n_bee_visits/flw_heads/intervals,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of bee visits per head per interval")+facet_grid(.~treatment)

###now comapre to flies

visit_summary%>%ggplot(aes(treatment,n_fly_visits/flw_heads/intervals,fill=treatment))+geom_boxplot()+
  ggtitle("Daily number fly visits per head per interval")

visit_summary%>%ggplot(aes(as.factor(day.y),n_fly_visits/flw_heads/intervals,fill=treatment))+geom_boxplot()+
  ggtitle("Daily number of fly visits per head intervals",subtitle="change over time")

#distribution of fly visits
visit_summary%>%ggplot(aes(n_fly_visits/flw_heads/intervals,fill=treatment))+geom_density()+
  ggtitle("Distribution of fly visits per head")+facet_grid(.~treatment)

###
visit_summary%>%ggplot(aes(n_fly_visits/flw_heads/intervals,fill=treatment))+geom_histogram()+
  ggtitle("Distribution of fly visits per head per interval")+facet_grid(.~treatment)

  #########################
### community data: conspecifics and coblooming
##############

#make a quick summary

#first replace na w/ zero
cobloom$X.floral_units[is.na(cobloom$X.floral_units)]<-0
#replace NA NA in plant genus sp column with true NA so we don't inflate plant_genus_sp
cobloom$plant_genus_sp[cobloom$plant_genus_sp == "NA NA"]<- NA

cobloom_ID<-cobloom%>%left_join(ID_trt, c("plantID"))

cobloom_daily<-cobloom_ID%>%
  mutate(date=paste(year,month,day, sep="-"))%>%mutate(ymd=ymd(date),yday=yday(ymd))%>%
  group_by(plantID,treatment,yday)%>%
mutate(richness=ifelse(is.na(plant_genus_sp), 0, n_distinct(plant_genus_sp)))%>%ungroup()%>%
  group_by(plantID,treatment,yday)%>%
  summarize(daily_richness=((sum(richness)/n())),
            avg_blooms=mean(X.floral_units),
            sum_blooms=sum(X.floral_units))


#plots

ggplot(cobloom_daily, aes(treatment,daily_richness,fill=treatment))+geom_boxplot()+
  ggtitle("coblooming richness by treatment")

### relationship over time?
ggplot(cobloom_daily, aes(as.factor(yday),daily_richness,fill=treatment))+geom_boxplot()+
  ggtitle("Change in coblooming richness over time by treatment")

ggplot(cobloom_daily, aes(daily_richness,fill=treatment))+geom_histogram()+facet_grid(.~treatment)+
  ggtitle("distribution of coblooming richness by treatment")



##density
ggplot(cobloom_daily, aes(treatment,avg_blooms,fill=treatment))+geom_boxplot()+
  ggtitle(" coblooming density by treatment")
###
ggplot(cobloom_daily, aes(as.factor(yday),avg_blooms,fill=treatment))+geom_boxplot()+
  ggtitle("Change in mean coblooming density over time by treatment")

ggplot(cobloom_daily, aes(avg_blooms,fill=treatment))+geom_histogram()+facet_grid(.~treatment)+
  ggtitle("distribution of coblooming density by treatment")

## sum
ggplot(cobloom_daily, aes(treatment,sum_blooms,fill=treatment))+geom_boxplot()+
  ggtitle("Change in sum coblooming density by treatment")
### over time
ggplot(cobloom_daily, aes(as.factor(yday),sum_blooms,fill=treatment))+geom_boxplot()+
  ggtitle("Change in coblooming density over time by treatment")

# so coblooming data is pretty sparse and probably shouldn't be included.


###################


####
#### now consspecific density
#####

#how many plants?
con_dens_ID<-con_dens%>%left_join(ID_trt, c("plantID"))
#drop all obs where NA or estimate of distance (>4, >10) used
con_dens_ID$NN_1<-as.numeric(as.character(con_dens_ID$NN_1))
con_dens_ID$NN_2<-as.numeric(as.character(con_dens_ID$NN_2))
con_dens_ID$NN_3<-as.numeric(as.character(con_dens_ID$NN_3))

###coerce NAs to


#some NAs left over from data cleaning- these should be 0
con_dens_ID$X.dalpur_1m[is.na(con_dens_ID$X.dalpur_1m)]<-0
con_dens_ID$X.dalpur_5m[is.na(con_dens_ID$X.dalpur_5m)]<-0

## first let's et the ratio of NAs to actual data across the 3 NN classes
sum(is.na(con_dens_ID$NN_1))/sum(!is.na(con_dens_ID$NN_1))
sum(is.na(con_dens_ID$NN_2))/sum(!is.na(con_dens_ID$NN_2))
sum(is.na(con_dens_ID$NN_3))/sum(!is.na(con_dens_ID$NN_3))
### pretty close to half of the data is nearest neighbor data is NN, dependent on the distance
# there are points in the data where this truly means no conspecifics are near
# but there are other points where this means we lack data.


## make daily con density dataframe

con_dens_daily<-con_dens_ID%>%filter(!is.na(round))%>%
  mutate(date=paste(year,month,day, sep="-"))%>%mutate(ymd=ymd(date),yday=yday(ymd))%>%
  select(plantID, treatment,yday,X.dalpur_1m,X.dalpur_5m)%>%
  group_by(plantID,treatment,yday)%>% rename(consp_1m=X.dalpur_1m, consp_5m=X.dalpur_5m)

#now for nearest neighbor; which has some NAs
## drop all NAs
NN_daily<-con_dens_ID%>%filter(!is.na(round))%>%
  mutate(date=paste(year,month,day, sep="-"))%>%mutate(ymd=ymd(date),yday=yday(ymd))%>%
  select(plantID, treatment,yday,NN_1,NN_2,NN_3)%>%
  group_by(plantID,treatment,yday)



#now let's visualize

#histograms


ggplot(con_dens_daily, aes(consp_1m,fill=treatment))+geom_histogram()+facet_grid(.~treatment)+
  ggtitle("Distribution of conspecific density in 1m by treatment")
#lotta zeroes

ggplot(con_dens_daily, aes(consp_5m,fill=treatment))+geom_histogram()+
  facet_grid(.~treatment)+ggtitle("Distribution of conspecific density in 5m by treatment")
#fewer zeroes--probably a better predictor variable

ggplot(con_dens_daily, aes(consp_5m,fill=treatment))+geom_density()+
  facet_grid(.~treatment)+ggtitle("Distribution of conspecific density in 5m by treatment")
#fewer zeroes--probably a better predictor variable


###boxplots or violin plots


ggplot(con_dens_daily, aes(treatment,consp_1m,fill=treatment))+geom_boxplot()+
  ggtitle("Mean conspecific density in 1m by treatment")

ggplot(con_dens_daily, aes(treatment,consp_5m,fill=treatment))+geom_boxplot()+
  ggtitle("Mean conspecific density in 5m by treatment")

## over time
ggplot(con_dens_daily, aes(as.factor(yday),consp_1m,fill=treatment))+geom_boxplot()+
  ggtitle("Change in conspecific density in 1m over time by treatment")

ggplot(con_dens_daily, aes(as.factor(yday),consp_5m,fill=treatment))+geom_boxplot()+
  ggtitle("Change in conspecific density in 5m over time by treatment")


### NN-- note that you lose observations due to NAs as these data were only taken when focal plants were flowering
##boxplot
ggplot(NN_daily, aes(treatment,NN_1,fill=treatment))+geom_boxplot()+
  ggtitle("Mean distance to NN1 by treatment")

ggplot(NN_daily, aes(treatment,NN_2,fill=treatment))+geom_boxplot()+
  ggtitle("Mean distance to NN2 by treatment")

ggplot(NN_daily, aes(treatment,NN_3,fill=treatment))+geom_boxplot()+
  ggtitle("Mean distance to NN3 by treatment")

#change over time
ggplot(NN_daily, aes(as.factor(yday),NN_1,fill=treatment))+geom_boxplot()+
  ggtitle("Change in distance to NN1 over time by treatment")


ggplot(NN_daily, aes(as.factor(yday),NN_2,fill=treatment))+geom_boxplot()+
  ggtitle("Change in distance to NN2 over time by treatment")
#pretty similar to NN1

### histogram

ggplot(NN_daily, aes(NN_1,fill=treatment))+geom_histogram()+
  facet_grid(.~treatment)+ggtitle("Distribution of NN_1 by treatment")


ggplot(NN_daily, aes(NN_1,fill=treatment))+geom_density()+
  facet_grid(.~treatment)+ggtitle("Distribution of NN_1 by treatment")
#NN2
ggplot(NN_daily, aes(NN_2,fill=treatment))+geom_histogram()+
  facet_grid(.~treatment)+ggtitle("Distribution of NN_2 by treatment")
