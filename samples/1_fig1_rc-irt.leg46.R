

###############################################################################################
# Fig.1: Estimation of roll call ideal points for 46th legislature and energy bill (no. 01.022) for Schwarz, Traber & Benoit (2014)
###############################################################################################


library(MCMCpack)

load("./fig1&4_rollcall_irt/fig1&4_rc-irt.RData")


## 46th legislature
## Estimate ideal points (MCMC 1-dimensional IRT model) for 46th legislature

posterior.leg46 <- MCMCirt1d(votes.leg46, theta.constraints=list("21"="+", "94"="-"), burnin = 50000, mcmc = 1000000, thin=1000)
#21=Ch.Blocher(SVP), 94=Ch.Goll(SP)


# Merge ideal points with mp_list & generate output file

sum.idealpoints.leg46 <- summary(posterior.leg46)
idealpoints.leg46 <- sum.idealpoints.leg46$statistics
idealpoints.leg46 <- cbind(idealpoints.leg46, sum.idealpoints.leg46$quantiles)
idealpoints.leg46 <- as.data.frame(idealpoints.leg46)
names(idealpoints.leg46) <- c("Mean","SD", "Naive SE", "Time-series SE", "Quant2.5", "Quant25","Quant50","Quant75","Quant97.5")
idealpoints.leg46$mp_id <- row.names(idealpoints.leg46)
idealpoints.leg46$mp_id <- gsub("theta.", "", idealpoints.leg46$mp_id, fixed=TRUE)
idealpoints.leg46$mp_id <- as.integer(idealpoints.leg46$mp_id)
idealpoints.leg46 <- idealpoints.leg46[order(idealpoints.leg46$mp_id),]

idealpoints.leg46 <- merge(idealpoints.leg46, mp_list.leg46)
idealpoints.leg46 <- idealpoints.leg46[order(idealpoints.leg46$Mean),]

idealpoints.leg46$party_abbreviation[idealpoints.leg46$party_abbreviation=="GB"] <- "GPS"
idealpoints.leg46$party_abbreviation[idealpoints.leg46$party_abbreviation=="Al"] <- "GPS"
idealpoints.leg46$party_abbreviation[idealpoints.leg46$party_abbreviation=="LPS"] <- "FDP-Liberale"
idealpoints.leg46$party_abbreviation[idealpoints.leg46$party_abbreviation=="AdG"] <- "PdA"






## Energy bill no. 01.022
## Estimate ideal points (MCMC 1-dimensional IRT model) for energy bill no. 01.022

posterior.01.022.all <- MCMCirt1d(votes.01.022, theta.constraints=list("254"="+", "348"="-"), burnin = 10000, mcmc = 1000000, thin=100)
#MPs: 254=Steiner(FDP), 348=R.Rechsteiner(SP)


# Merge ideal points with mp_list & generate output file

sum.idealpoints.01.022.all <- summary(posterior.01.022.all)
idealpoints.01.022.all <- sum.idealpoints.01.022.all$statistics
idealpoints.01.022.all <- cbind(idealpoints.01.022.all, sum.idealpoints.01.022.all$quantiles)
idealpoints.01.022.all <- as.data.frame(idealpoints.01.022.all)
names(idealpoints.01.022.all) <- c("Mean","SD", "Naive SE", "Time-series SE", "Quant2.5", "Quant25","Quant50","Quant75","Quant97.5")
idealpoints.01.022.all$mp_id <- row.names(idealpoints.01.022.all)
idealpoints.01.022.all$mp_id <- gsub("theta.", "", idealpoints.01.022.all$mp_id, fixed=TRUE)
idealpoints.01.022.all$mp_id <- as.integer(idealpoints.01.022.all$mp_id)
idealpoints.01.022.all <- idealpoints.01.022.all[order(idealpoints.01.022.all$mp_id),]

idealpoints.01.022.all <- merge(idealpoints.01.022.all, mp_list.leg46, all.x=T)
idealpoints.01.022.all <- subset(idealpoints.01.022.all, !is.na(mp_id))
idealpoints.01.022.all <- idealpoints.01.022.all[order(idealpoints.01.022.all$Mean),]

idealpoints.01.022.all$party_abbreviation[idealpoints.01.022.all$party_abbreviation=="GB"] <- "GPS"
idealpoints.01.022.all$party_abbreviation[idealpoints.01.022.all$party_abbreviation=="Al"] <- "GPS"
idealpoints.01.022.all$party_abbreviation[idealpoints.01.022.all$party_abbreviation=="LPS"] <- "FDP-Liberale"
idealpoints.01.022.all$party_abbreviation[idealpoints.01.022.all$party_abbreviation=="AdG"] <- "PdA"



save.image("./fig1&4_rollcall_irt/fig1&4_rc-irt.RData")

