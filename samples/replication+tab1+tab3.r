#######################################################################################
# REPLICATION CODE FOR RELIABILITY MEASURES IN:
### 'Analyzing Manifestos in their Electoral Context: A New Approach Applied to Austria, 2002-2008'
### forthcoming in Political Science Research and Methods
### Authors: Martin Dolezal, Laurenz Ennser-Jedenastik, Wolfgang C. Müller & Anna Katharina Winkler
### Department of Government, University of Vienna
### Austrian National Election Study (AUTNES) | Supply Side, www.autnes.at

### set working directory:
# setwd("C:/ [...] /replication data/data")
setwd("Y:/Democratic Governance/Projekte/Papiere/Manifesto Analysis in Context/00_Paper/Political Science Research and Methods/09_Replication Material/replication data/data")

### load irr package
require(irr) # if package not installed run: 'install.packages("irr")'


#################
### UNITIZING ###
#################

### load data files (unitizing)
bz06.u <- read.table("relunit_BZ_2006.csv", sep=";", header=T)
bz08.u <- read.table("relunit_BZ_2008.csv", sep=";", header=T)
fp02.u <- read.table("relunit_FP_2002.csv", sep=";", header=T)
fp06.u <- read.table("relunit_FP_2006.csv", sep=";", header=T)
fp08.u <- read.table("relunit_FP_2008.csv", sep=";", header=T)
gr02.u <- read.table("relunit_GR_2002.csv", sep=";", header=T)
gr06.u <- read.table("relunit_GR_2006.csv", sep=";", header=T)
gr08.u <- read.table("relunit_GR_2008.csv", sep=";", header=T)
lf02.u <- read.table("relunit_LF_2002.csv", sep=";", header=T)
lf08.u <- read.table("relunit_LF_2008.csv", sep=";", header=T)
vp02.u <- read.table("relunit_VP_2002.csv", sep=";", header=T)
vp06.u <- read.table("relunit_VP_2006.csv", sep=";", header=T)
vp08.u <- read.table("relunit_VP_2008.csv", sep=";", header=T)
sp02.u <- read.table("relunit_SP_2002.csv", sep=";", header=T)
sp06.u <- read.table("relunit_SP_2006.csv", sep=";", header=T)
sp08.u <- read.table("relunit_SP_2008.csv", sep=";", header=T)


### Replication of values in Table 1: Inter-coder reliability of the unitizing procedure
# SPÖ:
kripp.alpha(t(sp02.u[2:3]), method="ratio")
kripp.alpha(t(sp06.u[2:3]), method="ratio")
kripp.alpha(t(sp08.u[2:3]), method="ratio")
# ÖVP:
kripp.alpha(t(vp02.u[2:3]), method="ratio")
kripp.alpha(t(vp06.u[2:3]), method="ratio")
kripp.alpha(t(vp08.u[2:3]), method="ratio")
# FPÖ:
kripp.alpha(t(fp02.u[2:3]), method="ratio")
kripp.alpha(t(fp06.u[2:3]), method="ratio")
kripp.alpha(t(fp08.u[2:3]), method="ratio")
# Greens:
kripp.alpha(t(gr02.u[2:3]), method="ratio")
kripp.alpha(t(gr06.u[2:3]), method="ratio")
kripp.alpha(t(gr08.u[2:3]), method="ratio")
# BZÖ:
kripp.alpha(t(bz06.u[2:3]), method="ratio")
kripp.alpha(t(bz08.u[2:3]), method="ratio")
# LIF:
kripp.alpha(t(lf02.u[2:3]), method="ratio")
kripp.alpha(t(lf08.u[2:3]), method="ratio")




####################
### ISSUE CODING ###
####################


### load data files (issue coding)
bz06.c <- read.table("relcod_BZ_2006.csv", sep=";", header=T)
bz08.c <- read.table("relcod_BZ_2008.csv", sep=";", header=T)
fp02.c <- read.table("relcod_FP_2002.csv", sep=";", header=T)
fp06.c <- read.table("relcod_FP_2006.csv", sep=";", header=T)
fp08.c <- read.table("relcod_FP_2008.csv", sep=";", header=T)
gr02.c <- read.table("relcod_GR_2002.csv", sep=";", header=T)
gr06.c <- read.table("relcod_GR_2006.csv", sep=";", header=T)
gr08.c <- read.table("relcod_GR_2008.csv", sep=";", header=T)
lf02.c <- read.table("relcod_LF_2002.csv", sep=";", header=T)
lf08.c <- read.table("relcod_LF_2008.csv", sep=";", header=T)
vp02.c <- read.table("relcod_VP_2002.csv", sep=";", header=T)
vp06.c <- read.table("relcod_VP_2006.csv", sep=";", header=T)
vp08.c <- read.table("relcod_VP_2008.csv", sep=";", header=T)
sp02.c <- read.table("relcod_SP_2002.csv", sep=";", header=T)
sp06.c <- read.table("relcod_SP_2006.csv", sep=";", header=T)
sp08.c <- read.table("relcod_SP_2008.csv", sep=";", header=T)


### matrices with reliability values
relcod2002 <- matrix(NA, 5, 3)
for (i in 1:3) {
  relcod2002[1,i] <- kripp.alpha(t(sp02.c[(i*2):(i*2-1)]), method="nominal")$value
  relcod2002[2,i] <- kripp.alpha(t(vp02.c[(i*2):(i*2-1)]), method="nominal")$value
  relcod2002[3,i] <- kripp.alpha(t(fp02.c[(i*2):(i*2-1)]), method="nominal")$value
  relcod2002[4,i] <- kripp.alpha(t(gr02.c[(i*2):(i*2-1)]), method="nominal")$value
  relcod2002[5,i] <- kripp.alpha(t(lf02.c[(i*2):(i*2-1)]), method="nominal")$value
}

relcod2006 <- matrix(NA, 5, 3)
for (i in 1:3) {
  relcod2006[1,i] <- kripp.alpha(t(sp06.c[(i*2):(i*2-1)]), method="nominal")$value
  relcod2006[2,i] <- kripp.alpha(t(vp06.c[(i*2):(i*2-1)]), method="nominal")$value
  relcod2006[3,i] <- kripp.alpha(t(fp06.c[(i*2):(i*2-1)]), method="nominal")$value
  relcod2006[4,i] <- kripp.alpha(t(gr06.c[(i*2):(i*2-1)]), method="nominal")$value
  relcod2006[5,i] <- kripp.alpha(t(bz06.c[(i*2):(i*2-1)]), method="nominal")$value
}

relcod2008 <- matrix(NA, 6, 3)
for (i in 1:3) {
  relcod2008[1,i] <- kripp.alpha(t(sp08.c[(i*2):(i*2-1)]), method="nominal")$value
  relcod2008[2,i] <- kripp.alpha(t(vp08.c[(i*2):(i*2-1)]), method="nominal")$value
  relcod2008[3,i] <- kripp.alpha(t(fp08.c[(i*2):(i*2-1)]), method="nominal")$value
  relcod2008[4,i] <- kripp.alpha(t(gr08.c[(i*2):(i*2-1)]), method="nominal")$value
  relcod2008[5,i] <- kripp.alpha(t(bz08.c[(i*2):(i*2-1)]), method="nominal")$value
  relcod2008[6,i] <- kripp.alpha(t(lf08.c[(i*2):(i*2-1)]), method="nominal")$value
}


### Replication of values in Table 3: Inter-coder reliability at three levels of issue coding scheme
# Manifestos 2002, Level 1:
round(summary(relcod2002[,3]), 2)
# Manifestos 2002, Level 2:
round(summary(relcod2002[,2]), 2)
# Manifestos 2002, Level 3:
round(summary(relcod2002[,1]), 2)


# Manifestos 2006, Level 1:
round(summary(relcod2006[,3]), 2)
# Manifestos 2006, Level 2:
round(summary(relcod2006[,2]), 2)
# Manifestos 2006, Level 3:
round(summary(relcod2006[,1]), 2)

# Manifestos 2008, Level 1:
round(summary(relcod2008[,3]), 2)
# Manifestos 2008, Level 2:
round(summary(relcod2008[,2]), 2)
# Manifestos 2008, Level 3:
round(summary(relcod2008[,1]), 2)