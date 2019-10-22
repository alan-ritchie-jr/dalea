source("phenology.R")
source("data_merge.R")

### packages
library(tidyverse)
library(piecewiseSEM)
library(lme4)
library(glmmTMB)
library(mateable)
library(lubridate)

view(full_dalea_df)

#so for SEM we have
# treatment as the exogenous variable (burn vs unburn)

##########################
###floral trait tier
########################

### things that directly impact an individual's reproductive success

# 1. Floral display componenet: max_flowering_heads
### need to demonstrate the mean daily floral display size is correlated with max flow heads
# 2. Phenological component: Start or mean_date_flw
### need to select which one of these to use (can probably just compare models)

###################
### Mate availability 
############

# 3. Mean density of mates within 5m will be affected


####
# summarized first tier of models 
###

# 1. max_flowering_heads ~ treatment
# 2a. mean_date_flw ~ treatment
# 2b. start ~ treatment
# 3. mean_dens_5m ~ treatment



#### 
# VISITATION
####
# the paths:
#1. mean_visit_min~max_flower_heads+treatment

#2. mean_visit_min~mean_date_flw+treatment

#4. mean_visit_min~mean_dens_5m+treatment



# so these will need to have interactions?
# seed set (more ovules = greater seed set)-- 
# we need to think about whether this can be an unspecified path 

#

