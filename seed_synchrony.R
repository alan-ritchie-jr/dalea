

library(car)
library(reshape2)
library(tidyverse)
library(lme4)
library(glmmTMB)
library(DHARMa)
#########################
#seedchrony

# use data from phenology.R and df_clean if needed

seed<-read.csv("data/non_database_csvs/seed-counts_26March2019.csv")

#updated seed data
seed2<-read.csv("data/non_database_csvs/dalea_seed_counts_7May2019.csv")


seed_check<-seed%>%anti_join(ID_trt, c("plantID")) # should only be controls

# 
seed_check2<-seed2%>%anti_join(ID_trt, c("plantID")) # should only be controls

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

seed_prop2<-seed2%>%left_join(ID_trt, c("plantID"))%>%filter(plantID!="8UBC")%>%
  replace_na(list(treatment= "Control"))%>%filter(treatment!="Control")%>%
  rename(full=X.full_seeds, part=X.partial_full_seeds,empty=X.aborted_seeds,
         heads=Num_Heads,abrt_heads=Aborted_Heads,dehisc_heads=Dehisced_Heads)%>% 
  mutate_all(funs(replace(., is.na(.), 0)))%>%group_by(plantID)%>%
  summarise(n_collection_events=n(),heads=sum(heads),full=sum(full), part=sum(part), empty=sum(empty),
            total_fruit=full+part+empty, filling_frt_prop=(full+part)/total_fruit,
            seed_prop=full/total_fruit) 
#### Note: 
##### add in relative fitness measures, and standardized traits to seed prop2 ####)

#join seed data with synchrony

seed_sync<-pheno_sync%>%inner_join(seed_prop,by="plantID")

seed_sync2<-pheno_sync%>%inner_join(seed_prop2,by="plantID")

####
#show ruth  starting here###
####



#seed set by treatment
seed_sync%>% ggplot(aes(treatment, seed_prop,fill=treatment))+geom_boxplot()


####seed_filled
#updated data
seed_sync2%>% ggplot(aes(treatment, seed_prop,fill=treatment))+geom_boxplot()

#####
###by pop

seed_sync%>% ggplot(aes(pop, seed_prop,fill=treatment))+geom_boxplot()
#updated data
seed_sync2%>% ggplot(aes(pop, seed_prop,fill=treatment))+geom_boxplot()

#####
###filling frt
seed_sync%>% ggplot(aes(treatment, filling_frt_prop,fill=treatment))+geom_boxplot()

seed_sync2%>% ggplot(aes(pop, filling_frt_prop,fill=treatment))+geom_boxplot()

####
### seed_prop plots

###relationship between seed fill and synchrony
seed_sync%>%ggplot(aes(syn_aug,seed_prop,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

#with updated seed dat
#
seed_sync2%>%ggplot(aes(syn_aug,seed_prop,color=treatment))+
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

####filling frt prop

seed_sync%>%ggplot(aes(syn_aug,filling_frt_prop,color=treatment))+
  facet_grid(.~treatment)+geom_point()+stat_smooth(method="lm")


##### most important piece
#####
#####
#### start date and seed
seed_sync2%>%ggplot(aes(start,seed_prop,color=treatment))+geom_point()+
  facet_grid(.~treatment)+stat_smooth(method="lm")

# this relationship could be due to 
# A. mate availability, duraiton, or synchrony
# B. Pollinator visitation
# C. Pollination quality

######
#relationship between syncrony and start date
seed_sync2%>%ggplot(aes(start,syn_aug,color=treatment))+geom_point()+
  facet_grid(.~treatment)+stat_smooth(method="lm")
# no difference between new and old seed data
###So synchrony relationship is the same between the two. 
### Need to check at overall density.####

seed_sync2%>%ggplot(aes(start,duration,color=treatment))+geom_point()+
  facet_grid(.~treatment)+stat_smooth(method="lm")
# no difference between new and old seed data
###So synchrony relationship is the same between the two. 
### Need to check at overall density.####
#full seeds vs start date
seed_sync2%>%ggplot(aes(start,full,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")
#the total number of seeds produced decreases at almost the same rate for each treatment
#
###duration
seed_sync2%>%ggplot(aes(start,heads,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

seed_sync2%>%ggplot(aes(start,seed_prop,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

seed_sync2%>%ggplot(aes(heads,full,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

seed_sync2%>%ggplot(aes(heads,total_fruit,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

seed_sync2%>%ggplot(aes(heads,seed_prop,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")
#### larger plants do nto seem to be any more or less prone to lower proportion seed set
# however you have very few observations for large plants.

seed_sync2%>%ggplot(aes(full,total_fruit,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")
# so total fruit and the number of full seeds correlate very strongly

seed_sync2%>%ggplot(aes(start,heads,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")

#histogram
seed_sync%>%ggplot(aes(seed_prop,color=treatment))+ 
  geom_histogram()+facet_grid(.~treatment)

#fecundity traits vs proportion of seed filled or total fruits 
seed_sync2%>%ggplot(aes(heads,seed_prop,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")+
ggtitle("Proportion Seed Filled as a function of number of flowering heads")

seed_sync2%>%ggplot(aes(full,seed_prop,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")+
ggtitle("Proportion Seed Filled as a function of full Seeds produced")

seed_sync2%>%ggplot(aes(total_fruit,seed_prop,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")+
ggtitle("Proportion Seed Filled as a function of total fruit produced")

seed_sync2%>%ggplot(aes(total_fruit,full,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Full seed as a function of Total Fruit")


seed_sync2%>%ggplot(aes(duration, total_fruit,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("total fruit as a function of Total Fruit")

seed_sync2%>%ggplot(aes(duration,full,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("full seed as a function of duration")

seed_sync2%>%ggplot(aes(start, total_fruit,color=treatment))+ 
  geom_point()+facet_grid(.~treatment)+stat_smooth(method="lm")+
  ggtitle("Total fruit asa function of start")


sync_cor<-seed_sync2%>%select(start, duration, syn_aug,heads,full,total_fruit,seed_prop)%>%cor()
sync_cor
#how does total seed filled vs proportion track?
seed_sync2%>%ggplot(aes(full,seed_prop,color=treatment))+ 
  geom_point()+stat_smooth(method="lm")

###selection gradients

### you'll use car and lm basically

## standardize fitness
seed_sync_<- cbind(seed_sync2, seed_sync2$seed_prop/mean(seed_sync2$seed_prop))
names(seed_sync_)[22] <- "relfitness_prop"
seed_sync_2<- cbind(seed_sync_, seed_sync_$full/mean(seed_sync_$full))
names(seed_sync_2)[23] <- "relfitness_full_seed"

# Standardize traits
seed_sync_3<- cbind(rescale(seed_sync_2[,2],"sd"))

seed_sync$seed_prop/mean(seed_sync2$seed_prop)

### ecr
mean(seed_sync2$seed_prop)
seed_sync2$seed_prop
##models
trt_mod<-glmer(full/total_fruit~treatment+(1|pop)+(1|id),
               weights=total_fruit,family=binomial,data=seed_sync)

summary(trt_mod)


trt_mod2<-glmer(full/total_fruit~treatment+(1|pop)+(1|id),
               weights=total_fruit,family=binomial,data=seed_sync2)

summary(trt_mod2)


####
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

mod1_olre<-glmer(full/total_fruit~start*treatment+duration+(1|pop)+ (1|id),
                 weights=total_fruit,family=binomial,data=seed_sync)
summary(mod1_olre)

mod2<- glmmTMB(full~start*treatment+duration+(1|pop),family=nbinom1,data=seed_sync)
summary(mod2)

mod3<- glmer.nb(full~start+treatment+duration+(1|pop),
               data=seed_sync)


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



