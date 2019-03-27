###################################################################
# Table 2: Models to Predict Speech Act and Speech Length
# -- Schwarz, Traber and Benoit (2014)
###################################################################


library(Zelig)
options(scipen=999)


# #  1. P R E P A R E   D A T A 

# 1.1 Speaker Attributes

setwd("~/Dropbox/transcripts_rollcalls/manuscript/mpsa2013/PRSM_submission/data_for_replication/table2_regression_speeches")

# Load data with speaker attributes
load("speakers.RData")
speakers <- combined.idealpoints.energy1

# Speaker attributes: select the Energy Bill 
speak.e1 <- subset(speakers, speakers$bill_id == "01.022" )


# 1.2 Speeches

# Load dataset including the speeches 
load("debates_corpus.Rdata")
debate <- debates.corpus$attribs

# Speeches dataset: select the Energy Bill
deb.e1 <- subset(debate, bill == "energy1")

# Omit speeches where an MP speaks "for commission"
deb.e1 <- subset(deb.e1, role == "")

# Generate dataframe that includes word count / character count
deb.e1 <- data.frame(number=deb.e1$number, firstname= deb.e1$firstname, 	
		lastname=deb.e1$lastname, mp_id=deb.e1$mp_id, lang=deb.e1$language, 
		genpos=deb.e1$general_positiontaking,
		interact=deb.e1$interaction, proc=deb.e1$procedural,
		nchar=nchar(deb.e1$texts), nwords=sapply(strsplit(deb.e1$texts," "),length))
 
# Drop speeches that are purely procedural (e.g. order of debate)
deb.e1 <- subset(deb.e1, interact==0) 

# Drop speeches that are simple replies to the previous speaker (without substantial content)
deb.e1 <- subset(deb.e1, proc==0) 

 
# Calculate total speechlength (sum of words and sum of characters in all speeches per MP if an MP speaks several times)
deb.e1$sumchar <- rep(NA,length(deb.e1$mp_id))
deb.e1$sumwords <- rep(NA,length(deb.e1$mp_id))
 
 for (i in deb.e1$mp_id) {
 	deb.e1$sumchar[deb.e1$mp_id==i]  <- sum(deb.e1$nchar[deb.e1$mp_id==i])
 	deb.e1$sumwords[deb.e1$mp_id==i]  <- sum(deb.e1$nwords[deb.e1$mp_id==i])
 }
 
 
# 1.3 Merge speaker attributes and speeches dataset 
 
# Pre-select Variables in speakers dataset 
speak.e1 <- speak.e1[c(1:12,14:17,19:22,29,52:56,65:68)] 
  
# Merge datasets
data.e1 <- merge(speak.e1, deb.e1, by="mp_id", all=T)

# Select variables in combined dataset
data.e1 <- data.e1[c(1:30,34:41)]

# Rename variables
names(data.e1)[5] <- "lastname"
names(data.e1)[6] <- "firstname"


# 1.4 Recode variables in the combined dataset

# Recode speech variables: MPs who did not speak at all during the debate
data.e1$nchar[is.na(data.e1$nchar)] <- 0
data.e1$nwords[is.na(data.e1$nwords)] <- 0
data.e1$sumchar[is.na(data.e1$sumchar)] <- 0
data.e1$sumwords[is.na(data.e1$sumwords)] <- 0
 
# Generate dependent variable for Model 1: speak (yes/no)
data.e1$speak <- data.e1$sumchar
data.e1$speak[data.e1$sumchar > 0] <- 1

# Seniority of weeks as numeric variable
data.e1$seniority_weeks <- as.numeric(data.e1$seniority_weeks)

# Order Dataset
data.e1 <- data.e1[order(data.e1$mp_id),]


# 1.5 Aggregate dataset: only 1 entry per MP

data.e1 <- data.frame(data.e1)
data.e1 <- subset(data.e1, select=c(1,11,13,17,19,20,25,26,28,37,38,39))
e1.aggr <- aggregate(data.e1, by=list(data.e1$mp_id), FUN="mean")



# #  2. M O D E L S

# quadratic term for yes-voteshare in pop ref 
e1.aggr$yshare <- e1.aggr$popinit_yesshare_average
e1.aggr$yshare2 <- e1.aggr$popinit_yesshare_average^2


# 2.1 Model 1 (logit): predict speech act

out.e1.sp1 <- zelig(speak~ 
  						party_leader + 
  						latin +
  						female + 
  						log(seniority_weeks) +
  						committee_member +
  						irt_AbsDistToMedian_all +
  						irt_Mean_46leg +
  						yshare + yshare2,
  						model="logit", data=e1.aggr)

# Model 1: Odds Ratio
format(round(exp(summary(out.e1.sp1)$coefficients[,1]),3),nsmall=3)

# Model 1: Confidence Intervals
format(round(exp( confint(out.e1.sp1)) ,3),nsmall=3)


# 2.2 Model 2 (OLS): predict speech length

# Exlude MPs who did not speak  
e1.aggr2 <- subset(e1.aggr, subset=sumchar!=0)
  
out.e1.le1 <- lm(log(sumwords) ~ 
						party_leader + 
  						latin +
  						female + 
  						log(seniority_weeks) +
  						committee_member +
  						irt_AbsDistToMedian_all +
  						irt_Mean_46leg +
  						yshare + yshare2,
  						data=e1.aggr2)
summary(out.e1.le1)

# Model 2: Confidence Intervals
format(round( confint(out.e1.le1) ,2),nsmall=3)

