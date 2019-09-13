###selection gradients if wanted####

###first need seed_sync from data_merge file

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
