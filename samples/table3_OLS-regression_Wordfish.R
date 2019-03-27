

###################################################################
# Table 3: OLS Regression Models to Predict Wordfish Text Scalings
# -- Schwarz, Traber and Benoit (2014)
###################################################################


# load object with Wordfish and IRT ideal point for MPs and additional covariates

load("./table3_regression_wordfish/OLS-regression_Wordfish.RData")


# Model 1 in Table 3
lm_1 <- lm(wfish_theta_de500w ~ irt_Mean_de500w, data=combined.idealpoints.energy1, subset=party %in% c("CVP","EVP","FDP-Liberale","SP","SVP","GPS"))
summary(lm_1)


# Model 2 in Table 3
lm_2 <- lm(wfish_theta_de500w ~ irt_Mean_de500w + party, data=combined.idealpoints.energy1, subset=party %in% c("CVP","EVP","FDP-Liberale","SP","SVP","GPS"))
summary(lm_2)


# Model 3 in Table 3
lm_3 <- lm(wfish_theta_de500w ~ irt_Mean_de500w + popinit_yesshare_phaseout + age + female + party, data=combined.idealpoints.energy1, subset=party %in% c("CVP","EVP","FDP-Liberale","SP","SVP","GPS"))
summary(lm_3)


# Model 4 in Table 3
lm_4 <- lm(wfish_theta_de500w ~ irt_Mean_de500w + popinit_yesshare_moratorium + age + female + party, data=combined.idealpoints.energy1, subset=party %in% c("CVP","EVP","FDP-Liberale","SP","SVP","GPS"))
summary(lm_4)



# latex output of results

library(stargazer)

stargazer(lm_1, lm_2, lm_3, lm_4, title="OLS Regression Models to Predict Wordfish Text Scalings", align=TRUE, ci=T, ci.separator=", ")


