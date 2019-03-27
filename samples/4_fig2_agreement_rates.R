

###############################################################################################
# Fig.2: Calculation of agreement rates for 46th legislature and energy bill (no. 01.022);  Schwarz, Traber & Benoit (2014)
###############################################################################################


load("./fig2_agreement_rates/agreement_rates.RData")


# count number of answers in new variables (code.0=no, code.1=yes, code.2=abstention)
# per party
library(plyr)
votes.longformat.leg46.party <- ddply(votes.longformat.leg46, .(bill_id, vote_id, party_abbreviation, yea_votes, nay_votes, abstain_votes, absent_votes, parlyear, legislat), summarise, 
                                      answer.code.0.N = sum(!is.na(answer.code.0)),
                                      answer.code.1.N = sum(!is.na(answer.code.1)),
                                      answer.code.2.N = sum(!is.na(answer.code.2)),
                                      answer.code.3.N = sum(!is.na(answer.code.3)),
                                      answer.code.4.N = sum(!is.na(answer.code.4)),
                                      answer.code.5.N = sum(!is.na(answer.code.5)),
                                      .progress = "text")




votes.longformat.leg46.ext <- merge(votes.longformat.leg46, votes.longformat.leg46.party[c(1:3,10:12)], by=c("bill_id", "vote_id", "party_abbreviation"), all.x=T)
votes.longformat.leg46.ext$answer.code.0[is.na(votes.longformat.leg46.ext$answer.code.0)] <- 0
votes.longformat.leg46.ext$answer.code.1[is.na(votes.longformat.leg46.ext$answer.code.1)] <- 0
votes.longformat.leg46.ext$answer.code.2[is.na(votes.longformat.leg46.ext$answer.code.2)] <- 0



# assign agreement score of MPs per vote
attach(votes.longformat.leg46.ext)
votes.leg46.mpagreement.pervote <- transform(votes.longformat.leg46.ext,  
                                            agreement_vote = ifelse(apply(votes.longformat.leg46.ext[c(21:23)], 1, sum, na.rm=TRUE) <= 1,
                                                                    NA,
                                                                    ifelse(apply(votes.longformat.leg46.ext[c(18:20)], 1, sum, na.rm=TRUE) >= 1,
                                                                           NA,
                                                                           ifelse((answer.code.1.N / apply(votes.longformat.leg46.ext[c(21:23)], 1, sum, na.rm=TRUE) > 0.5 & answer.code.1 == 1) | (answer.code.0.N / apply(votes.longformat.leg46.ext[c(21:23)], 1, sum, na.rm=TRUE) > 0.5 & answer.code.0 == 1) | (answer.code.2.N / apply(votes.longformat.leg46.ext[c(21:23)], 1, sum, na.rm=TRUE) > 0.5 & answer.code.2 == 1),
                                                                                  1,
                                                                              ifelse((answer.code.1.N / apply(votes.longformat.leg46.ext[c(21:23)], 1, sum, na.rm=TRUE) > 0.5 & answer.code.2 == 1) | (answer.code.0.N / apply(votes.longformat.leg46.ext[c(21:23)], 1, sum, na.rm=TRUE) > 0.5 & answer.code.2 == 1),
                                                                                     0.5, 
                                                                                  ifelse((answer.code.1.N / apply(votes.longformat.leg46.ext[c(21:23)], 1, sum, na.rm=TRUE) > 0.5 & answer.code.1 == 0 & answer.code.2 == 0) | (answer.code.0.N / apply(votes.longformat.leg46.ext[c(21:23)], 1, sum, na.rm=TRUE) > 0.5 & answer.code.0 == 0 & answer.code.2 == 0) | (answer.code.2.N / apply(votes.longformat.leg46.ext[c(21:23)], 1, sum, na.rm=TRUE) > 0.5 & answer.code.2 == 0),
                                                                                         0, NA))))))
detach(votes.longformat.leg46.ext) 


# adjust party labels

votes.leg46.mpagreement.pervote$party_abbreviation[votes.leg46.mpagreement.pervote$party_abbreviation=="FDP-Liberale"] <- "FDP-Liberals"
votes.leg46.mpagreement.pervote$party_abbreviation[votes.leg46.mpagreement.pervote$party_abbreviation=="GB"] <- "GPS"  #GB belongs to GPS
votes.leg46.mpagreement.pervote$party_abbreviation[votes.leg46.mpagreement.pervote$party_abbreviation=="LPS"] <- "FDP-Liberals"  #LPS belongs to FDP



# calculate agreement rate for 46th legislature

votes.leg46.mpagreement <- ddply(votes.leg46.mpagreement.pervote, .(mp_id), summarise, 
                                            agreement = 100* mean(agreement_vote, na.rm=T),
                                            N    = sum(!is.na(agreement_vote)),
                                            .progress = "text")

votes.leg46.mpagreement <- merge(votes.leg46.mpagreement, mp_list.leg46, by="mp_id", all.x=T)

votes.leg46.partyagreement <- ddply(votes.leg46.mpagreement, .(party_abbreviation), summarise, 
                                      agreement_rate = mean(agreement),
                                      N = length(agreement),
                                      sd   = sd(agreement))
                                      




# calculate agreement rate for energy bill (id 01.022)
votes.energybill.mpagreement <- ddply(subset(votes.leg46.mpagreement.pervote, bill_id=="01.022"), .(mp_id), summarise, 
                                 agreement = 100* mean(agreement_vote, na.rm=T),
                                 N    = sum(!is.na(agreement_vote)),
                                 .progress = "text")

votes.energybill.mpagreement <- merge(votes.energybill.mpagreement, mp_list.leg46, by="mp_id", all.x=T)

votes.energybill.partyagreement <- ddply(votes.energybill.mpagreement, .(party_abbreviation), summarise, 
                                 agreement_rate = mean(agreement),
                                 N = length(agreement),
                                 sd   = sd(agreement) )




