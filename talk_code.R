#plots and summaries
library(tidyverse)
#plot Fig. 2
#PLANT display SIZE
dalea%>%ggplot(aes(treatment,max_flowering_heads,fill=treatment))+
  geom_boxplot(aes(middle = mean(max_flowering_heads, color=black)),lwd=1.3)+ theme_bw()+
  labs(x="Treatment" ,y="Number of Flowering Heads")+
  ggtitle("Floral Display Size")+
  theme(axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=2),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")
#add title stuff
#normality of residuals assumption is poor,
#but all other assumptions met, namely homogeneity of variance between groups
t.tests

###Fig 3--peak flowering
dalea%>%ggplot(aes(treatment,std_md,fill=treatment))+
  geom_boxplot(aes(middle = mean(std_md, color=black)),lwd=1.3)+ theme_bw()+
  labs(x="Treatment" ,y="Peak Flowering Date")+
  ggtitle("Flowering Phenology")+
  theme(axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=2),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")

## fig. 4--density
dalea%>%ggplot(aes(treatment,mean_dens_5m,fill=treatment))+
  geom_boxplot(aes(middle = mean(mean_dens_5m, color=black)),lwd=1.3)+ theme_bw()+
  labs(x="Treatment" ,y="Conspecific Flowering Heads in 5m")+
  ggtitle("Conspecific Density")+
  theme(axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=22),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=1.5),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")

#Fig. 5
#bee visitation rates
dalea%>%ggplot(aes(treatment,mean_bee_min,fill=treatment))+
  geom_boxplot(aes(middle = mean(mean_bee_min, color=black)),lwd=1.3)+ theme_bw()+
  labs(x="Treatment" ,y="Mean Bee Visits per Minute")+
  ggtitle("Bee Visitation Rate")+
  theme(axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=1.5),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")



# Fig 6 cons pollen
dalea%>%ggplot(aes(treatment,mean_dalpur,fill=treatment))+
  geom_boxplot(aes(middle = mean(mean_dalpur)),lwd=1.3)+ theme_bw()+
  labs(x="Treatment" ,y="Mean Conspecific Pollen Grains per Stigma")+
  ggtitle("Conspecific Pollen Receipt")+
  theme(axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=18),title=element_text(size=22),
        axis.line = element_line(colour = "black", size=1.5),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")

#fig 7 hetero pollen!

dalea%>%ggplot(aes(treatment,mean_hetero,fill=treatment))+
  geom_boxplot(aes(middle = mean(mean_hetero)),lwd=1.3)+ theme_bw()+
  labs(x="Treatment" ,y="Mean Heterospecific Pollen Grains per Stigma")+
  ggtitle("Heterospecific Pollen Receipt")+
  theme(axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=16),title=element_text(size=18),
        axis.line = element_line(colour = "black", size=1.5),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")

mm<-lmer(mean_hetero~treatment+(1|pop),data=dalea)
summary(mm)
#fig 8 
# difference in seed set between treatments

dalea%>%ggplot(aes(treatment,seed_prop,fill=treatment))+
  geom_boxplot(aes(middle = mean(seed_prop,color=black)),lwd=1.3)+ theme_bw()+
  labs(x="Treatment" ,y="Seeds per Ovule")+
  ggtitle("Proportion Seed Set")+
  theme(axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=1.5),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")

### plot histogram
### gives number of plants flowering on given day.
#for a given yday how many plants were blooming for each pop??

## first run these
foc_dates_hist<-focals%>%# 
  filter(plantID!="42UB")%>%#42UB has no flowers so we have to drop from analyses
  group_by(plantID,yday,ymd, treatment)%>%
  summarise(bloom_heads)%>%
  group_by(yday,treatment)%>%
  summarize(n_flw=n_distinct(plantID[bloom_heads>0]))



foc_dates_hist$yday<-case_when(foc_dates_hist$yday %in% 208 ~ 207,# group these two
                               TRUE ~ as.numeric(foc_dates_hist$yday))

#make some new columns
foc_dates_hist$day<-1+foc_dates_hist$yday-min(foc_dates_hist$yday)
foc_dates_hist$sday<- as.numeric(as.factor(foc_dates_hist$yday))

#get confint and means
day_lm<-lm(day~treatment-1,data=foc_dates_hist)
summary(day_lm)
confint(day_lm)
# get mean line
foc_date_peak<-foc_dates_hist%>%
  group_by(treatment)%>%
  summarize(pop_peak=round(weighted.mean(day,n_flw)))

#what are the means of phenology?

foc_dates_hist_b<-foc_dates_hist%>%filter(treatment=="B")
foc_dates_hist_ub<-foc_dates_hist%>%filter(treatment=="UB")

round(weighted.mean(foc_dates_hist_b$day,foc_dates_hist_b$n_flw))
round(weighted.mean(foc_dates_hist_ub$day,foc_dates_hist_ub$n_flw))

# now plot

foc_dates_hist%>%ggplot(aes(sday,n_flw,fill=treatment))+geom_col(width=.8)+
  scale_fill_manual("treatment", values = c("B" = "black", "UB" = "lightgrey"))+
  facet_grid(treatment~.,scales="free")+ 
  scale_x_continuous(breaks=foc_dates_hist$sday)+
  geom_vline(data= foc_date_mean,aes(xintercept = pop_peak,color=treatment),lwd=2,linetype="dashed")+
  scale_color_manual("treatment", values = c("B" = "darkgray", "UB" = "black"))+
  theme_bw()+labs(x="Survey Day" ,y="Number of Flowering Focal Plants",
                  title=expression(paste("Fig. 9: ",italic("D. purpurea")," Flowering Phenology")),
                  subtitle="Burned vs. Unburned")+
  theme(axis.text=element_text(size=20,color="black"),
        axis.title=element_text(size=26),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=1.5),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),axis.text.x = element_blank(),
        axis.ticks.x= element_blank(),
        panel.grid.minor = element_blank(),strip.background = element_blank(),
        strip.text.y= element_blank(),legend.position = "none")

#### seed set plot

ggplot(dalea, aes(x = c_pheno, y = seed_prop, color = treatment) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(dalea, aes(x = c_pheno, y = seed_prop, color = treatment) ) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .15, aes(fill = treatment))

methods("predict")

fitlm = lm(seed_prop~ mean_hetero+mean_dalpur+max_flowering_heads+c_pheno*treatment, data = dalea)
dalea$predlm = predict(fitlm)

ggplot(dalea, aes(x = c_pheno, y = seed_prop, color = treatment) ) +
  geom_point() +
  geom_smooth(aes(c_pheno,predlm,group=treatment,color=treatment,fill=treatment),alpha=.15, method="lm")

predslm = simultaneous_CBs(fitlm)
head(predslm)

dalea_pred= cbind(dalea, predslm)

