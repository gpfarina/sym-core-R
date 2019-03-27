
###########################################################################################
# Fig.2: Box plots to Calculation of agreement rates for 46th legislature and energy bill
###########################################################################################


# !!! First run R script "fig2_agreement_rates.R" and save RData !!!


load("fig2_agreement_rates.RData")


# Combine the plots on 1 page

library(lattice)


votes.energybill.mpagreement.mainparties <- subset(votes.energybill.mpagreement, party_abbreviation %in% c("CVP","FDP-Liberals","SVP","GPS","EVP","SP"))
votes.leg46.mpagreement.mainparties <- subset(votes.leg46.mpagreement, party_abbreviation %in% c("CVP","FDP-Liberals","SVP","GPS","EVP","SP"))


attach(votes.energybill.mpagreement.mainparties)

cols <- list(col=c("orange","blue","darkgreen","turquoise","green2","red"),pch=c(16))
boxplot1 <- bwplot(reorder(factor(party_abbreviation), 0.01*agreement, median) ~ 0.01*agreement, main="", xlim=c(0.35,1.02), xlab="Agreement rate (energy bill votes)", 
                   par.settings = list(
                     #plot.symbol=cols,
                     box.rectangle = cols,
                     box.dot = cols,
                     box.umbrella=cols 
                   ))
print(boxplot1)
detach(votes.energybill.mpagreement.mainparties)


attach(votes.leg46.mpagreement.mainparties)
cols <- list(col=c("orange","blue","darkgreen","red","green2","turquoise"),pch=c(16))
boxplot2 <- bwplot(reorder(factor(party_abbreviation), 0.01*agreement, median) ~ 0.01*agreement, main="", xlim=c(0.35,1.02), xlab="Agreement rate (46th legislature)", 
                   par.settings = list(
                     #plot.symbol=cols,
                     box.rectangle = cols,
                     box.dot = cols,
                     box.umbrella=cols 
                   ))
print(boxplot2)

detach(votes.leg46.mpagreement.mainparties)


print(boxplot1, split = c(1,1,1,2), more = T)
print(boxplot2, split = c(1,2,1,2))



