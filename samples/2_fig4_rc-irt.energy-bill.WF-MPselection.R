
###############################################################################################

# Fig.4: Estimation of roll call ideal points for energy bill (no. 01.022) with speaker selection according to Wordfish text scaling in fig. 3 (German speakers, >= 500 words) -- Schwarz, Traber & Benoti (2014)

###############################################################################################


library(MCMCpack)

load("./fig1&4_rollcall_irt/fig1&4_rc-irt.RData")


## Energy bill no. 01.022, Wordfish speaker selection
## Estimate ideal points (MCMC 1-dimensional IRT model) for energy bill no. 01.022

posterior.01.022.de.500w <- MCMCirt1d(votes.01.022.de.500w, theta.constraints=list("254"="+", "348"="-"), burnin = 10000, mcmc = 1000000, thin=100)
#MPs: 254=Steiner(FDP)), 348=R.Rechsteiner(SP)


# Merge ideal points with mp_list & generate output file

sum.idealpoints.01.022.de.500w <- summary(posterior.01.022.de.500w)
idealpoints.01.022.de.500w <- sum.idealpoints.01.022.de.500w$statistics
idealpoints.01.022.de.500w <- cbind(idealpoints.01.022.de.500w, sum.idealpoints.01.022.de.500w$quantiles)
idealpoints.01.022.de.500w <- as.data.frame(idealpoints.01.022.de.500w)
names(idealpoints.01.022.de.500w) <- c("Mean","SD", "Naive SE", "Time-series SE", "Quant2.5", "Quant25","Quant50","Quant75","Quant97.5")
idealpoints.01.022.de.500w$mp_id <- row.names(idealpoints.01.022.de.500w)
idealpoints.01.022.de.500w$mp_id <- gsub("theta.", "", idealpoints.01.022.de.500w$mp_id, fixed=TRUE)
idealpoints.01.022.de.500w$mp_id <- as.integer(idealpoints.01.022.de.500w$mp_id)
idealpoints.01.022.de.500w <- idealpoints.01.022.de.500w[order(idealpoints.01.022.de.500w$mp_id),]

idealpoints.01.022.de.500w <- merge(idealpoints.01.022.de.500w, mp_list.leg46, all.x=T)
idealpoints.01.022.de.500w <- subset(idealpoints.01.022.de.500w, !is.na(mp_id))
idealpoints.01.022.de.500w <- idealpoints.01.022.de.500w[order(idealpoints.01.022.de.500w$Mean),]

idealpoints.01.022.de.500w$party_abbreviation[idealpoints.01.022.de.500w$party_abbreviation=="GB"] <- "GPS"
idealpoints.01.022.de.500w$party_abbreviation[idealpoints.01.022.de.500w$party_abbreviation=="Al"] <- "GPS"
idealpoints.01.022.de.500w$party_abbreviation[idealpoints.01.022.de.500w$party_abbreviation=="LPS"] <- "FDP-Liberale"
idealpoints.01.022.de.500w$party_abbreviation[idealpoints.01.022.de.500w$party_abbreviation=="AdG"] <- "PdA"


# csv export of ideal points
write.csv(idealpoints.01.022.de.500w, file="~/idealpoints.01.022.de.500w.csv")


save.image("./fig1&4_rollcall_irt/fig1&4_rc-irt.RData")
