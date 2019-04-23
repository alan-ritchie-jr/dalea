
################
#Some individuals were added part way through, around y day 204, and then 208;
# these all had yet or had just started blooming from what we could observe, but can be dropped
###################
drop<-c("40UB","41UB","42UB","43UB","44UB","44UB","45UB", "46UB","47UB","48UB","49UB",
        "40B","41B","42B","43B","44B","44B","45B", "46B","47B","48B","49B")

#drop values contained in drop from df
focal_drop<-foc_dates[!foc_dates$plantID %in%drop,]
#

flw_first_last_drop<- focal_drop%>% group_by(plantID,treatment) %>%
  summarise(firstDay=ymd[first(which(bloom_heads>=1))],
            lastDay=ymd[last(which(bloom_heads >= 1))]) %>%ungroup()%>%
  mutate(pop=ifelse(plantID %in%pop1, "pop1", 
                    ifelse(plantID %in%pop2, "pop2", 
                           ifelse(plantID %in%pop3, "pop3", 
                                  ifelse(plantID %in%pop4, "pop4",
                                         ifelse(plantID %in%pop5, "pop5","pop6"))))))%>%as.data.frame()


bd<-flw_first_last_drop%>%filter(treatment=="B")
ud<-flw_first_last_drop%>%filter(treatment=="UB")


###
bd1<-bd%>%filter(pop=="pop1")
bd2<-bd%>%filter(pop=="pop2")
bd3<-bd%>%filter(pop=="pop3")

ud4<-ud%>%filter(pop=="pop4")
ud5<-ud%>%filter(pop=="pop5")
ud6<-ud%>%filter(pop=="pop6")


# step 2 make mating scene
#Burned scene
Bdscene<-makeScene(bd, startCol="firstDay",
                   endCol="lastDay",idCol="plantID")# add sub pop variable as a "year" column to compare synchrony within subpop )
#unburned scene
Udscene<-makeScene(ud, startCol="firstDay",endCol="lastDay",idCol="plantID")

#subpops

#burn
Bdscene1<-makeScene(bd1, startCol="firstDay",
                    endCol="lastDay",idCol="plantID")
Bdscene2<-makeScene(bd2, startCol="firstDay",
                    endCol="lastDay",idCol="plantID")
Bdscene3<-makeScene(bd3, startCol="firstDay",
                    endCol="lastDay",idCol="plantID")

spBd_scene<- list('1' = Bdscene1,'2' = Bdscene2, '3' = Bdscene3)


###unburn
Udscene4<-makeScene(ud4, startCol="firstDay",
                    endCol="lastDay",idCol="plantID")
Udscene5<-makeScene(ud5, startCol="firstDay",
                    endCol="lastDay",idCol="plantID")
Udscene6<-makeScene(ud6, startCol="firstDay",
                    endCol="lastDay",idCol="plantID")

spUd_scene<- list('4' = Udscene4,'5' = Udscene5, '6' = Udscene6)


### plot general "scene"
plotScene(Bdscene, "t")#check github
plotScene(Udscene,"t")

#subpop plots
plotScene(spBd_scene, "t")
plotScene(spUd_scene, "t")

################################
#######

bd_sum<-matingSummary(Bdscene, type="t")

ud_sum<-matingSummary(Udscene,type="t")

spBd_sum<-matingSummary(spBd_scene, type="t")

spUd_sum<-matingSummary(spUd_scene, type="t")

spBd_sum$`3`$peak
spBd_sum$`1`$peak
spBd_sum$`2`$peak
spUd_sum$`4`$peak
spBd_sum$`1`$meanDur
spBd_sum$`2`$meanDur
spBd_sum$`3`$meanDur
spB_sum$`1`$meanDur
spB_sum$`2`$meanDur
spB_sum$`3`$meanDur
b_sum$meanDur
bd_sum$meanDur
ub_sum$meanDur
ud_sum$meanDur
