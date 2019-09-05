
### pollen data

### use data from phenology.R and seed_synchrony.R scripts

###seed_prop2 and seed_prop
view(pol)

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

stig_seed_sync<-stig%>%left_join(seed_sync, c("plantID","treatment"))

###seed_sync2 is the updated data
stig_seed_sync2<-stig%>%left_join(seed_sync2, c("plantID","treatment"))


### from stiga



###average dalea pollen deposited per stigma per plant
stig_seed_sync2%>% ggplot(aes(treatment, mean_dalpur,fill=treatment))+geom_boxplot()

#average hetero per stigma per plant
stig_seed_sync2%>% ggplot(aes(treatment, mean_hetero,fill=treatment))+geom_boxplot() 
#average amocan per stigma per plant
stig_seed_sync2%>% ggplot(aes(treatment, mean_amocan,fill=treatment))+geom_boxplot() 


#histograms 
#distribution of mean dalea pollen deposited   
stig_seed_sync%>%ggplot(aes(mean_dalpur,color=treatment))+ 
  geom_histogram()+facet_grid(.~treatment)
#distribution of mean amocan pollen deposted
stig_seed_sync2%>%ggplot(aes(mean_amocan,color=treatment))+ 
  geom_histogram()+facet_grid(.~treatment)
#hetero
stig_seed_sync%>%ggplot(aes(mean_hetero,color=treatment))+ 
  geom_histogram()+facet_grid(.~treatment) 
#mean total
stig_seed_sync%>%ggplot(aes(mean_total_pollen,color=treatment))+ 
  geom_histogram()+facet_grid(.~treatment) 
## lotta zeroes in these data to deal with 

### average dalea pollen deposited as a function of synchrony
stig_seed_sync2%>%ggplot(aes(syn_o,mean_dalpur,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")
# pretty flat, local density probably more important

### average dalea pollen deposted as a function of start day
stig_seed_sync2%>%ggplot(aes(start,mean_dalpur,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average dalpur pollen deposted on a plant as a function of start date")

stig_seed_sync2%>%ggplot(aes(duration,mean_dalpur,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average dalpur pollen deposted on a plant as a function of duration")

stig_seed_sync2%>%ggplot(aes(start,mean_amocan,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average amocan pollen deposted on a plant as a function of start")
## start date is stronger in relationship, which makes sense
stig_seed_sync2%>%ggplot(aes(duration,mean_amocan,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Average amocan pollen deposted on a plant as a function of duration")
### what could underlie less dalea pollen being deposite in B side as start date increased?
# 1. Change in conspecific density
# 2. Chance 
# 3. facilitative effects via density or amorpha or both? 
#Greater visit number earlier = greater deposition

stig_seed_sync2%>%ggplot(aes(heads,mean_total_pollen,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")


stig_seed_sync2%>%ggplot(aes(start,mean_amocan,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

stig_seed_sync2%>%ggplot(aes(heads,mean_amocan,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

stig_seed_sync2%>%ggplot(aes(heads,sum_amocan,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

cor<-stig_seed_sync2%>%
  select(start,syn_aug,heads,full,total_fruit,seed_prop,sum_amocan,mean_amocan)%>%
  as.matrix()%>%
cor()

cor(cor_variables[,2:9])

###
stig_seed_sync2%>%ggplot(aes(sum_amocan/sum_total_pollen,seed_prop,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

stig_seed_sync2%>%ggplot(aes(sum_amocan/sum_total_pollen,seed_prop,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")



stig_seed_sync2%>%ggplot(aes(sum_amocan/sum_total_pollen,seed_prop,color=treatment))+
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
stig_mod<-glmmTMB(sum_amocan/sum_total_pollen~scale(start)*treatment+(1|pop),
                weights=sum_total_pollen,
                family=binomial,data=stig_seed_sync2)
summary(stig_mod)

sim_stig<-simulateResiduals(fittedModel = stig_mod,n=1000)

plot(sim_stig)

testDispersion(sim_stig)
testZeroInflation(sim_stig) #zero inflation test comes back significant


#with observation level random effect

stig_mod_olre<-glmmTMB(sum_amocan/sum_total_pollen~scale(start)*treatment+(1|pop)+(1|plantID),
                     weights=sum_total_pollen,
                     family=binomial,data=stig_seed_sync2)
summary(stig_mod_olre)

sim_stig_olre<-simulateResiduals(fittedModel = stig_mod_olre,n=1000)

plot(sim_stig_olre)

testDispersion(sim_stig_olre)
testZeroInflation(sim_stig_olre) # fixed overdispersion due to zero inflation

#parameter estimates change a bit, but overall message of the model doesn't seem to change.
#adding observation level random effect controls for the zero inflation or overdispersion

seed_mod<-glmmTMB(full~start+heads+mean_amocan+(1|pop),family=nbinom2,
                   data=stig_seed_sync2)
summary(seed_mod)
### make separate dataframes for CI for plot

sim_seed_stig<-simulateResiduals(fittedModel = seed_mod,n=1000)

plot(sim_seed_stig)

testDispersion(sim_seed_stig)
testZeroInflation(sim_stig_olre)

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
