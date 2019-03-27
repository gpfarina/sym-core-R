

###############################################################################################
# Fig.4: Dot plot IRT vs. Wordfish scalings
###############################################################################################


# !!!  First run R scripts in folders "fig1&4_rollcall_irt" and "fig3_wordfish_scalings" !!!

# After that:

load("./fig1&4_rollcall_irt/fig1&4_rc-irt.RData")
load("./fig3_wordfish_scalings/energy-debate.corpus.RData")


library(ggplot2)


# Merge Wordfish and IRT scalings into one file

combined.idealpoints.energy <- merge(wfish.energy, idealpoints.01.022.de.500w, by.x="docs", by.y="mp_id", all.x=T)

combined.idealpoints.energy$color[combined.idealpoints.energy$party_abbreviation.y=="CVP"] <- "orange"
combined.idealpoints.energy$color[combined.idealpoints.energy$party_abbreviation.y=="EVP"] <- "turquoise"
combined.idealpoints.energy$color[combined.idealpoints.energy$party_abbreviation.y=="FDP-Liberale"] <- "blue"
combined.idealpoints.energy$color[combined.idealpoints.energy$party_abbreviation.y=="GPS"] <- "green2"
combined.idealpoints.energy$color[combined.idealpoints.energy$party_abbreviation.y=="SP"] <- "red"
combined.idealpoints.energy$color[combined.idealpoints.energy$party_abbreviation.y=="SVP"] <- "darkgreen"




# draw dot plot

ggplot(combined.idealpoints.energy, aes(x=Mean, y=theta)) +
  geom_point(shape=16, aes(color=party_abbreviation.y)) +
  scale_colour_manual(limits = combined.idealpoints.energy$party_abbreviation.y, values = combined.idealpoints.energy$color) +
  #geom_smooth(method=lm) +
  xlab("IRT ideal points") +
  ylab("Wordfish ideal points") +
  theme(legend.key = element_blank()) +
  theme_bw()


