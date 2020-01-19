#plots and summaries
library(tidyverse)
library(grid)
library(dplyr)
library(lubridate)
dalea$treatment_name<-ifelse(dalea$treatment=="B","Burned","Unburned")

#plot Fig. 2
#PLANT display SIZE
plot_a<-dalea%>%ggplot(aes(treatment_name,max_flowering_heads))+
  geom_boxplot(aes(middle = mean(max_flowering_heads, color=black)),lwd=1)+ theme_bw()+
  labs(x="Treatment" ,y="Number of Flowering Heads")+
  ggtitle("a")+
  theme(axis.text=element_text(size=18,color="black"),
        axis.title=element_text(size=22),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=1.5),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x=element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")
#add title stuff
#normality of residuals assumption is poor,
#but all other assumptions met, namely homogeneity of variance between groups


###Fig 3--peak flowering
plot_b<-dalea%>%ggplot(aes(treatment_name,std_md))+
  geom_boxplot(aes(middle = mean(std_md, color=black)),lwd=1)+ theme_bw()+
  labs(x="Treatment" ,y="Peak Flowering Date")+
  ggtitle("b")+
  theme(axis.text=element_text(size=18,color="black"),
        axis.title=element_text(size=22),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=1.5),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x=element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")

grid.draw(cbind(ggplotGrob(plot_a), ggplotGrob(plot_b), size = "last"))

## fig. 4--density
plot_c<-dalea%>%ggplot(aes(treatment_name,mean_dens_5m))+
  geom_boxplot(aes(middle = mean(mean_dens_5m, color=black)),lwd=1)+ theme_bw()+
  labs(x="Treatment" ,y="Density of Conspecific Flowering Heads")+
  ggtitle("c")+
  theme(axis.text=element_text(size=18,color="black"),
        axis.title=element_text(size=22),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=1.5),panel.border = element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")

#Fig. 5
#bee visitation rates
plot_d<-dalea%>%ggplot(aes(treatment_name,mean_bee_min))+
  geom_boxplot(aes(middle = mean(mean_bee_min, color=black)),lwd=1)+ theme_bw()+
  labs(x="Treatment" ,y="Mean Bee Visits per Minute")+
  ggtitle("d")+
  theme(axis.text=element_text(size=18,color="black"),
        axis.title=element_text(size=22),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=1.5),panel.border = element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")

grid.draw(cbind(ggplotGrob(plot_c), ggplotGrob(plot_d), size = "last"))
# Fig 6 cons pollen
plot_e<-dalea%>%ggplot(aes(treatment_name,mean_dalpur))+
  geom_boxplot(aes(middle = mean(mean_dalpur)),lwd=1)+ theme_bw()+
  labs(x="Treatment" ,y="Mean Conspecific Pollen Grains per Stigma")+
  ggtitle("e")+
  theme(axis.text=element_text(size=18,color="black"),
        axis.title=element_text(size=22),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=1.5),panel.border = element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")

#fig 7 hetero pollen!

plot_f<-dalea%>%ggplot(aes(treatment_name,mean_hetero))+
  geom_boxplot(aes(middle = mean(mean_hetero)),lwd=1)+ theme_bw()+
  labs(x="Treatment" ,y="Mean Heterospecific Pollen Grains per Stigma")+
  ggtitle("f")+
  theme(axis.text=element_text(size=18,color="black"),
        axis.title=element_text(size=22),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=1.5),panel.border = element_blank(),
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")

grid.draw(cbind(ggplotGrob(plot_e),ggplotGrob(plot_f), size = "last"))
#fig 8 
# difference in seed set between treatments

plot_g<-dalea%>%ggplot(aes(treatment_name,seed_prop))+
  geom_boxplot(aes(middle = mean(seed_prop,color=black)),lwd=1)+ theme_bw()+
  labs(x="Treatment" ,y="Seeds per Ovule")+
  ggtitle("g")+
  theme(axis.text=element_blank(),
        axis.title=element_text(size=24),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=1.5),panel.border = element_blank(),
      
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")
grid.draw(cbind(ggplotGrob(plot_g), ggplotGrob(plot_e), size = "last"))
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

ggplot(dalea, aes(x = std_md, y = seed_prop, color = treatment) ) +
  geom_point(size=2) +
  geom_smooth(method = "lm", se = FALSE,lwd=2,aes(linetype=treatment))+ theme_bw()+
  labs(x="Flowering Phenology" ,y="Seeds per Ovule")+
  theme(axis.text=element_text(size=24,color="black"),
        axis.title=element_text(size=26),title=element_text(size=26),
        axis.line = element_line(colour = "black", size=1.5),panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),legend.position = "none")

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


ggplot(dalea_pred, aes(x = c_pheno, y = seed_prop, color = treatment) ) +
  geom_point() +
  geom_smooth(aes(c_pheno,predlm,group=treatment,color=treatment), method="lm")+
  geom_ribbon(aes(ymin = LowerBound, ymax = UpperBound),alpha = 0.5, fill = "grey70")

simultaneous_CBs <- function(linear_model, newdata, level = 0.95){
  # Working-Hotelling 1 – α confidence bands for the model linear_model
  # at points newdata with α = 1 - level
  
  # estimate of residual standard error
  lm_summary <- summary(linear_model)
  # degrees of freedom 
  p <- lm_summary$df[1]
  # residual degrees of freedom
  nmp <-lm_summary$df[2]
  # F-distribution
  Fvalue <- qf(level,p,nmp)
  # multiplier
  W <- sqrt(p*Fvalue)
  # confidence intervals for the mean response at the new points
  CI <- predict(linear_model, newdata, se.fit = TRUE, interval = "confidence", 
                level = level)
  # mean value at new points
  Y_h <- CI$fit[,1]
  # Working-Hotelling 1 – α confidence bands
  LB <- Y_h - W*CI$se.fit
  UB <- Y_h + W*CI$se.fit
  sim_CB <- data.frame(LowerBound = LB, Mean = Y_h, UpperBound = UB)
}
