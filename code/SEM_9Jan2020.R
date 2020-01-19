source("phenology.R")
source("data_merge.R")

### packages
library(tidyverse)
library(lme4)
library(car)
library(glmmTMB)
library(mateable)
library(lubridate)
library(devtools)
library(margins)

#install latest version of piecewiseSEM
#install.packages("remotes")
#devtools::install_github("jslefche/piecewiseSEM@devel")
library(piecewiseSEM)
library(lavaan)
view(full_dalea_df)

#so for SEM we have
# treatment as the exogenous variable (burn vs unburn)
# See the SEM book by the package author for explanation of exogenous categorical variables:

# https://jslefche.github.io/sem_book/categorical-variables.html#introduction-to-exogenous-categorical-variables


## first prep data

#note 7Nov2019
# Thefull_dalea_df dataframe, for whatever reason, had NOT incorporated 
# a change to mean density where we only counted days where focal plants were blooming.
# it seems to have 
dalea_test<-na.omit(full_dalea_df2)# change this to full_dalea_df to add in overall density
dalea$trmnt<-as.numeric(dalea$treatment=="B")
dalea$fire<-as.numeric(dalea$treatment=="B")
dalea$trmnt
dalea$treatment

mean(dalea$max_flowering_heads)
mean(dalea_test$max_flowering_heads)
#center variables
dalea$c_pheno<-scale(dalea$mean_date_flw,scale=F)
dalea$c_dens<-scale(dalea$mean_dens_5m,scale=F)
dalea$c_max<-scale(dalea$max_flowering_heads,scale=F)
dalea$c_vis<-scale(dalea$mean_bee_visits,scale=F)
dalea$c_dalpur<-scale(dalea$mean_dalpur,scale=F)
dalea$c_sum_dalpur<-scale(dalea$sum_dalpur,scale=F)
dalea$c_hetero<-scale(dalea$mean_hetero,scale=F)
dalea$c_sum_hetero<-scale(dalea$sum_hetero,scale=F)
#some issues with normality of residuals in mean_bee_min, mean_dalpur, mean_hetero
#so sqrt transform

dalea$sqrt_peak<-sqrt(dalea$std_md)
dalea$sqrt_max_flw<-sqrt(dalea$max_flowering_heads)
dalea$sqrt_mean_dens<-sqrt(dalea$mean_dens_5m)
dalea$sqrt_mean_bee_min<-sqrt(dalea$mean_bee_min)
dalea$sqrt_mean_dalpur<-sqrt(dalea$mean_dalpur)
dalea$sqrt_mean_hetero<-sqrt(dalea$mean_hetero)
dalea$sqrt_seed_prop<-sqrt(dalea$seed_prop)

# standardize some variables just in case
dalea$std_md<-dalea$mean_date_flw-min(dalea$mean_date_flw)
dalea$std_mdxtrmnt<-dalea$std_md*dalea$trmnt
dalea$c_phenoxtrmnt<-dalea$c_pheno*dalea$trmnt
check<-dalea%>%select(std_md,mean_date_flw)

#for looking at treatment means
dalea_b<-dalea%>%filter(treatment=="B")
dalea_ub<-dalea%>%filter(treatment=="UB")
mean(dalea_b$total_fruit)
mean(dalea_ub$total_fruit)
#analysis 1: linear model comparisons (anovas) of variables
# first we compare mean values between treatments
# to see if treatment affects the mean of these variables important to pollination
# We can think of this as fire's proximate effect on the mean values of these variables
#We do this because sem doesn't test for differences in means in exogenous variables
#report analyses in main body, plots as supplements
library(car)
max_flw_lm<-lm(max_flowering_heads~treatment,data=dalea)
t.test(max_flowering_heads~treatment-1,data=dalea)
summary(max_flw_lm)
confint(max_flw_lm)
#no diff
plot(max_flw_lm)

#mean_date
t.test(std_md~treatment,data=dalea)
std_md_lm<-lm(std_md~treatment-1,data=dalea)
summary(std_md_lm) 

qqPlot(residuals(std_md_lm))
plot(std_md_lm)
confint(std_md_lm)
# all assumptions met
mean(dalea_ub$std_md)

#####
#cons_density
t.test(mean_dens_5m~treatment,data=dalea)
dens_lm<-lm(mean_dens_5m~treatment-1,data=dalea)
summary(dens_lm) 

qqPlot(residuals(dens_lm)) 
qqPlot(dens_lm)
plot(dens_lm)

mean(dalea_b$mean_dens_5m)
mean(dalea_ub$mean_dens_5m)

confint(dens_lm)
leveneTest(mean_dens_5m~treatment,data=dalea)
fligner.test(mean_dens_5m~treatment,data=dalea)

# normality of residuals is a bit off, but homogenity looks good
mean(dalea_b$std_md)
#plot Fig.1c
dalea%>%ggplot(aes(treatment,mean_dens_5m))+geom_boxplot(aes(middle = mean(mean_dens_5m)))
#add title stuff
sum(dalea_b$n_visit_obs)
sum(dalea_ub$n_visit_obs)
sum(dalea_b$sum_total_pollen)
sum(dalea_ub$sum_total_pollen)
#visits
t.test(mean_bee_min~treatment,data=dalea)
visit_lm<-lm(mean_bee_min~treatment,data=dalea)
summary(visit_lm)
qqPlot(residuals(visit_lm))
plot(visit_lm)
confint(visit_lm)
mean(dalea_b$mean_bee_min)
se <- function(x) sqrt(var(x)/length(x))
mean(dalea_b$mean_bee_min)
mean(dalea_ub$mean_bee_min)
se(dalea_ub$mean_bee_min)
se(dalea_b$mean_bee_min)
leveneTest(mean_bee_min~treatment,data=dalea)
#same, normality off but homogenity maintained

cor.test(dalea$start,dalea$duration)
cor.test(dalea_ub$mean_date_flw,dalea_ub$duration)
view(dalea)

#visits
t.test(mean_hetero~treatment,data=dalea)
hetero_lm<-lm(mean_hetero~treatment-1,data=dalea)
summary(hetero_lm)
qqPlot(residuals(hetero_lm))
t.test(mean_hetero~treatment)
plot(hetero_lm)
confint(hetero_lm)
mean(dalea_b$mean_hetero)
mean(dalea_ub$mean_hetero)
leveneTest(mean_hetero~treatment,data=dalea)
#dalpur
t.test(mean_dalpur~treatment,data=dalea)
dalpur_lm<-lm(mean_dalpur~treatment-1,data=dalea)
summary(dalpur_lm)
qqPlot(residuals(dalpur_lm))
plot(dalpur_lm)
confint(dalpur_lm)
mean(dalea_b$mean_dalpur)
mean(dalea_ub$mean_dalpur)
#same, normality off but homogenity maintained


#seed
full_seed_lm<-lm(log(full)~treatment,data=dalea)
summary(full_seed_lm)
qqPlot(residuals(full_seed_lm))
#same issues
#fig 1.e
dalea%>%ggplot(aes(treatment,full))+geom_boxplot(aes(middle = mean(full)))
#no diff
se <- function(x) sqrt(var(x)/length(x))
#prop seed 
t.test(seed_prop~treatment,data=dalea)
prop_seed_lm<-lm(seed_prop~treatment-1,data=dalea)
summary(prop_seed_lm)
plot(prop_seed_lm)
mean(dalea_b$seed_prop)
mean(dalea_ub$seed_prop)
confint(prop_seed_lm)
#all assumptions were met
dalea%>%ggplot(aes(treatment,seed_prop))+geom_boxplot(aes(middle = mean(seed_prop)))
#no diff



####
#step 2:
# test model and look at test of directed separation
# this tells us if we are missing any paths!
# from this we can also consolidate-- remove paths that aren't informing the model

#
dprop_full<-psem(
  lm(std_md~max_flowering_heads,data=dalea),
  lm(mean_bee_min~std_md+max_flowering_heads+mean_dens_5m,data=dalea),
  lm(mean_dalpur~mean_bee_min+max_flowering_heads+mean_dens_5m,data=dalea),
  lm(mean_hetero~mean_bee_min,data=dalea),
lm(seed_prop~mean_hetero+mean_dalpur+std_md+max_flowering_heads+mean_dens_5m,
      data=dalea),
data = dalea
)
summary(dprop_full)

 #fit needs improvement

dprop_full_fit<-psem(
  lm(max_flowering_heads~trmnt,data=dalea),
  lm(std_md~trmnt,data=dalea),
  lm(mean_dens_5m~trmnt,data=dalea),
  lm(mean_bee_min~std_md+max_flowering_heads+mean_dens_5m,data=dalea),
  lm(mean_dalpur~mean_bee_min+max_flowering_heads+mean_dens_5m,data=dalea),
  lm(mean_hetero~mean_bee_min+std_md+trmnt,data=dalea),
  mean_hetero%~~%mean_dalpur,
  mean_dens_5m%~~%std_md,
  max_flowering_heads%~~%std_md,
  lm(seed_prop~mean_hetero+mean_dalpur+std_md+trmnt+std_mdxtrmnt+max_flowering_heads,
     data=dalea),
  data = dalea
)
summary(dprop_full_fit)


#test model for interactions;
# this requires dropping burn as direct/indirect effect and using it as a grouping variable instead

dprop_group<-psem(
  lm(std_md~max_flowering_heads,data=dalea),
  lm(mean_bee_min~std_md+max_flowering_heads+mean_dens_5m,data=dalea),
  lm(mean_dalpur~mean_bee_min+max_flowering_heads+mean_dens_5m,data=dalea),
  lm(mean_hetero~mean_bee_min+std_md,data=dalea),
  mean_hetero%~~%mean_dalpur,
  mean_dens_5m%~~%std_md,
  lm(seed_prop~mean_hetero+mean_dalpur+std_md+max_flowering_heads,
     data=dalea),
  data = dalea
)
summary(dprop_group)

multigroup(dprop_group, group="treatment")
#interaction term makes sense for seed_prop!


##step3:
#Options: we can either pick the large complicated model,
# or we can shrink it down and look at the simpler modeladd in interactions and look at overall model fit (basically check for missing links)
multigroup(dprop,group="trmnt")

dprop_full<-psem(
  lm(std_md~n_stems,data=dalea),
  lm(seed_prop~trmnt+n_stems+mean_dens_5m,
     data=dalea),
  data = dalea
)
summary(dprop_full)

library(MASS)
multigroup(dprop,group="trmnt")

ggplot(data=dalea,aes(c_pheno,mean_dalpur,color=treatment))+geom_point()+geom_smooth(method="lm")
test_lm<-glm.nb(mean_bee_min~c_pheno*trmnt,offset=log(),data=dalea)
summary(test_lm)
#multigroup analysis indicates that the coefficient for mean date on proportion seed filled is
# close to 0 (e.g. flat line) for unburned
# while for burned its an order of magnitude higher, positive

#Step 2: take full model from d-separation test, test differences between two groups!

### lavaan version
library(lavaan)
library(tidyverse)
library(semTools)

seed_prop_lav_modmed<- '
#regressions
std_md~a*trmnt
seed_prop~std_md+trmnt+max_flowering_heads

#correlations
std_md~~max_flowering_heads
             
           
 
###

#add indirect effects of fire on pollinator visitation, het poll
# center for interpretation of std_md
'


seed_prop_lav_trmnt_x<- '
#regressions
max_flowering_heads~trmnt
mean_dens_5m~trmnt
std_md~trmnt
mean_bee_min~std_md+max_flowering_heads+mean_dens_5m
mean_dalpur~mean_bee_min+mean_dens_5m+max_flowering_heads
mean_hetero~mean_bee_min+std_md+trmnt+std_mdxtrmnt
seed_prop~mean_hetero+mean_dalpur+max_flowering_heads+std_md+trmnt+std_mdxtrmnt

#correlations
mean_dalpur~~mean_hetero
std_md~~mean_dens_5m
std_md~~max_flowering_heads
std_mdxtrmnt~~max_flowering_heads
std_mdxtrmnt~~trmnt
std_mdxtrmnt~~std_md
std_mdxtrmnt~~mean_dens_5m

 
###

'
seed_prop_lav_x_center<- '
#regressions
max_flowering_heads~trmnt
mean_dens_5m~trmnt
c_pheno~a*trmnt
mean_bee_min~b*c_pheno+max_flowering_heads+mean_dens_5m
mean_dalpur~mean_bee_min+mean_dens_5m+max_flowering_heads
mean_hetero~mean_bee_min+mean_dens_5m+c_pheno+trmnt+c_phenoxtrmnt
seed_prop~mean_hetero+mean_dalpur+max_flowering_heads+c_pheno+trmnt+c_phenoxtrmnt

#correlations
c_pheno~~max_flowering_heads
c_pheno~~mean_dens_5m
c_phenoxtrmnt~~max_flowering_heads
c_phenoxtrmnt~~trmnt
c_phenoxtrmnt~~c_pheno
c_phenoxtrmnt~~mean_dens_5m

#indirect
ab:= a*b
             
           
 
###

'


seed_prop_lav_x_center2<- '
#regressions
max_flowering_heads~trmnt
mean_dens_5m~trmnt
c_pheno~trmnt
mean_bee_min~c_pheno+max_flowering_heads+mean_dens_5m
mean_dalpur~mean_bee_min+mean_dens_5m
mean_hetero~mean_bee_min+c_pheno+trmnt+c_phenoxtrmnt
seed_prop~mean_hetero+mean_dalpur+max_flowering_heads+c_pheno+trmnt+c_phenoxtrmnt

#correlations
c_pheno~~max_flowering_heads
c_pheno~~mean_dens_5m
mean_dalpur~~mean_hetero
c_phenoxtrmnt~~max_flowering_heads
c_phenoxtrmnt~~trmnt
c_phenoxtrmnt~~c_pheno
c_phenoxtrmnt~~mean_dens_5m

             
 
###

'
seed_prop_lav_trmnt<- '
#regressions
max_flowering_heads~trmnt
mean_dens_5m~trmnt
std_md~a*trmnt
mean_bee_min~std_md+max_flowering_heads+mean_dens_5m
mean_dalpur~mean_bee_min+mean_dens_5m+max_flowering_heads
mean_hetero~mean_bee_min+std_md+trmnt
seed_prop~mean_hetero+mean_dalpur+max_flowering_heads+b*std_md+c*trmnt

#correlations
mean_dalpur~~mean_hetero
std_md~~mean_dens_5m
std_md~~max_flowering_heads

## indirect effect (a*b)
             ab := a*b
            
# total effect
             total fire_pheno:= c + (a*b)
             
           
 
###

'
seed_lav_trmnt<- '
#regressions
max_flowering_heads~d*trmnt
mean_dens_5m~trmnt
std_md~e*max_flowering_heads+a*trmnt
mean_bee_min~std_md+max_flowering_heads+mean_dens_5m
mean_dalpur~mean_bee_min+mean_dens_5m+max_flowering_heads
mean_hetero~mean_bee_min+std_md+trmnt
full~mean_hetero+mean_dalpur+f*max_flowering_heads+b*std_md+c*trmnt

#correlations
mean_dalpur~~mean_hetero
std_md~~mean_dens_5m



## indirect effect (a*b)
             ab := a*b
             df := d*f
             eb := e*b
# total effect
             total fire_pheno:= c + (a*b)
          
             total size_pheno:= f+(e*b)
###

'

seed_prop_lav_trmnt_no_cons<- '
#regressions
max_flowering_heads~d*trmnt
std_md~e*max_flowering_heads+a*trmnt
mean_bee_min~std_md+max_flowering_heads
mean_dalpur~mean_bee_min+max_flowering_heads
mean_hetero~mean_bee_min+std_md+trmnt
seed_prop~mean_hetero+mean_dalpur+f*max_flowering_heads+b*std_md+c*trmnt

#correlations
mean_dalpur~~mean_hetero



## indirect effect (a*b)
ab := a*b
df := d*f
eb := e*b
# total effect
total fire_pheno:= c + (a*b)

total size_pheno:= f+(e*b)
###

'


### no diff between uncon--> constrain this path

#install.packages("semPlot")
library(semPlot)
library(lavaan)
#install.packages("probemod")
library(probemod)
#test endogenous variables for multivariate normality
#install.packages("MVN")
library(MVN)
#test for multivariate normality
dal_mat<-dalea%>%select(sqrt_mean_bee_min,std_md,sqrt_mean_dalpur,seed_prop)
result <- mvn(data = dal_mat,  mvnTest= "mardia")
result$multivariateNormality

result <- mvn(data = dal_mat,  multivariatePlot = "qq")
result
# mean hetero throws off model normality a bit.
# using sqrts works, but hetero still problem

#given assumption of multivariate normality is violated let's incorporate a satorra bentler test
# this is a correction that can relax the multivariate normality assumption
#comparing this with the non-relaxed doesn't produce a difference.
library(lavaan)
#
seed_prop_x_center<-sem(seed_prop_lav_x_center2,
                        data=dalea,se="robust.sem",test="satorra.bentler")
summary(seed_prop_x_center,standardize=T,rsq=T, fit.measures=T)
fitted(seed_prop_x_center)

#non mod
seed_prop_x<-sem(seed_prop_lav_trmnt_x,data=dalea,
                     se="robust.sem",test="satorra.bentler")
summary(seed_prop_x,standardize=T,rsq=T, fit.measures=T)
parameterEstimates(seed_prop_x,
                   boot.ci.type = "bca.simple",
                   level =.95, ci = TRUE,standardized=T)

dalea%>%ggplot(aes(std_md,mean_dalpur,color=treatment))+geom_point()+geom_smooth(method="lm")
#full treatment model

seed_prop_trmnt<-sem(seed_prop_lav_trmnt,data=dalea,
                 se="robust.sem",test="satorra.bentler")
summary(seed_prop_trmnt,standardize=T,rsq=T, fit.measures=T)
#full seed
seed_trmnt<-sem(seed_lav_trmnt,data=dalea,
                     fixed.x=FALSE,se="robust.sem",test="satorra.bentler")
summary(seed_trmnt,standardize=T,rsq=T, fit.measures=T)

library(lme4)
test_mod<-lm(full~max_flowering_heads*trmnt*c_pheno,data=dalea)
summary(test_mod)
###
## total effect is significant but indirect effect through std_md is not.
#now without
seed_prop_SEM<-sem(seed_prop_lav_uncon,data=dalea, fixed.x=FALSE,group="treatment",se="robust.sem",test="satorra.bentler")
summary(seed_prop_SEM,standardize=T,rsq=T, fit.measures=T)






#now constrain all paths 

sem_con_all<-sem(seed_prop_lav_uncon,data=dalea,fixed.x=FALSE,group="treatment",
             se="robust.sem",test="satorra.bentler",
             group.equal=c( "intercepts", "means", "regressions","residuals",
                            "residual.covariances"))
summary(sem_con,standardize=T,rsq=T, fit.measures=T)
# residual covariance appears to be variance between specified correlations
anova(seed_prop_SEM,sem_con_all)
#model differs between groups!

#What if only residuals & residual covariances free
sem_con_resid<-sem(seed_prop_lav_uncon,data=dalea,fixed.x=FALSE,group="treatment",
             se="robust.sem",test="satorra.bentler",
             group.equal=c( "residuals"))
summary(sem_con_resid,standardize=T,rsq=T, fit.measures=T)
###anova between unconstrained and constrained model
anova(seed_prop_SEM,sem_con_resid) 
# yep that matters
# if we only hold residual covariances equal it doesn't effect model fit.
# figure out what that means

sem_con_res_reg<-sem(seed_prop_lav_uncon,data=dalea,fixed.x=FALSE,group="treatment",
                   se="robust.sem",test="satorra.bentler",
                   group.equal=c( "residuals","regressions"))
anova(seed_prop_SEM,sem_con_res_reg)

#
sem_con_int<-sem(seed_prop_lav_uncon,data=dalea,fixed.x=FALSE,group="treatment",
                     se="robust.sem",test="satorra.bentler",
                     group.equal=c("intercepts"))

anova(seed_prop_SEM,sem_con_int)
#even if regressions and residuals allowed to vary, fully unconstrained model fits better
# although its way less significant.

sem_con_int<-sem(seed_prop_lav_uncon,data=dalea,fixed.x=FALSE,group="treatment",
se="robust.sem",test="satorra.bentler",
group.equal=c("regressions"))

#test phenology path--> mm
sem_con2<-sem(lav_mm_con,data=dalea, group= "treatment", estimator="mlm",se="robust.sem",test="satorra.bentler")
summary(sem_con2,standardize=T,rsq=T, fit.measures=T)
anova(sem_uncon,sem_con2)

# no difference, constrain mm

# test 
sum(dalea$n_visit_obs)

###################################################################
#full seed
#
sem_full<--sem(seed_lav_trmnt,data=dalea, fixed.x=FALSE,se="robust.sem",test="satorra.bentler")
summary(sem_full,standardize=T,rsq=T, fit.measures=T)




anova(sem_uncon,sem_full_con)#all paths constrained compared to none
######

sem_ms_con<-sem(simple2,data=dalea, group= "treatment")


sem_fs_con<-sem(simple3,data=dalea, group= "treatment")


#compare constrained and unconstrained models 
anova(sem_uncon_test,sem_con_test)#all paths constrained compared to none
anova(sem_uncon_test,sem_ms_con_test)
anova(sem_uncon_test,sem_fs_con_test)

standardizedsolution(sem_test)
varTable(sem_test)

