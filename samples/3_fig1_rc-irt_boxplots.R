
###############################################################################################
# Fig.1: Box plots to Estimation of roll call ideal points for 46th legislature and energy bill
###############################################################################################




load("./fig1&4_rollcall_irt/fig1&4_rc-irt.RData")


# Restrict datasets/plots to 6 larger parties with speaking MPs

idealpoints.01.022.speaking <- subset(idealpoints.01.022.all, party_abbreviation %in% c("SVP","FDP-Liberale","CVP","EVP","SP","GPS"))

idealpoints.leg46.speaking <- subset(idealpoints.leg46, party_abbreviation %in% c("SVP","FDP-Liberale","CVP","EVP","SP","GPS"))


# Combine the plots on 1 page

library(lattice)

attach(idealpoints.01.022.speaking)
cols <- list(col=c("green2","red","turquoise","orange","blue","darkgreen"),pch=c(16))
boxplot1 <- bwplot(reorder(factor(party_abbreviation), Mean, median) ~ Mean, main="", xlim=c(-1.7,2.1), xlab="IRT ideal points (energy bill votes)", 
                   par.settings = list(
                    #plot.symbol=cols,
                    box.rectangle = cols,
                    box.dot = cols,
                    box.umbrella=cols 
                    ))
print(boxplot1)
detach(idealpoints.01.022.speaking)


attach(idealpoints.leg46.speaking)
cols <- list(col=c("green2","red","turquoise","orange","blue","darkgreen"),pch=c(16))
boxplot2 <- bwplot(reorder(factor(party_abbreviation), Mean, median) ~ Mean, main="", xlim=c(-4.2,2.6), xlab="IRT ideal points (46th legislature)", 
                   par.settings = list(
                     #plot.symbol=cols,
                     box.rectangle = cols,
                     box.dot = cols,
                     box.umbrella=cols 
                   ))
print(boxplot2)
detach(idealpoints.leg46.speaking)


print(boxplot1, split = c(1,1,1,2), more = T)
print(boxplot2, split = c(1,2,1,2))


