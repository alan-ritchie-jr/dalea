#this requires fully merged dalea df from stigma_seed_sync_consp
library(DHARMa)
library(lme4)
##models
#seed vs flower peak
#try out observation level random effect to deal with overdispersion


mod3<-glmer(cbind(full, total_fruit)~scale(mean_date_flw)*treatment+(1|plantID),
          family=binomial,data=full_dalea_df)
summary(mod3)#same result as lm :)

##dharma check of overdisp

sim_mod1<-simulateResiduals(fittedModel = mod3,n=1000)

plot(sim_mod1)

testDispersion(sim_mod1)

sim_mod1_olre<-simulateResiduals(fittedModel = mod1_olre,n=1000)

plot(sim_mod1_olre)

testDispersion(sim_mod1_olre)
testZeroInflation(sim_mod1_olre)

sim_mod2<-simulateResiduals(fittedModel = mod2,n=1000)

plot(sim_mod2)

testDispersion(sim_mod2)



sim_mod3<-simulateResiduals(fittedModel = mod3,n=1000)
plot(sim_mod3)
testDispersion(sim_mod3)



###########
### Stigma models



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



