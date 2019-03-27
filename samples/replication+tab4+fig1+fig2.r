#######################################################################################
# REPLICATION CODE FOR LEFT-RIGHT SCALES IN:
### 'Analyzing Manifestos in their Electoral Context: A New Approach Applied to Austria, 2002-2008'
### forthcoming in Political Science Research and Methods
### Authors: Martin Dolezal, Laurenz Ennser-Jedenastik, Wolfgang C. Müller & Anna Katharina Winkler
### Department of Government, University of Vienna
### Austrian National Election Study (AUTNES) | Supply Side, www.autnes.at
#######################################################################################


### load austin library
library(austin)
# if package not installed, install with 'install.packages("austin", repos="http://r-forge.r-project.org", type="source")'
# or check out http://conjugateprior.org/software/austin/

### set working directory
# setwd("C:/ [...] /replication data/data")


##########################################################################
### AUTNES Manifesto Scores (for Figure 1)

### load left-right codes
lr <- read.table("lr_codes.csv", header=T, sep=";")

### load manifesto data sets
mf02 <- read.table("mf2002.csv", header=T, sep=";")
names(mf02) <- c("id", "sentnr", "sub_org", "predicate", "pred_spec", "issue", "obj_org", "author")
mf06 <- read.table("mf2006.csv", header=T, sep=";")
names(mf06) <- c("id", "sentnr", "sub_org", "predicate", "pred_spec", "issue", "obj_org", "author")
mf08 <- read.table("mf2008.csv", header=T, sep=";")
names(mf08) <- c("id", "sentnr", "sub_org", "predicate", "pred_spec", "issue", "obj_org", "author")

### generate left-right variable
mf02$lr <- lr$direction[match(mf02$issue, lr$issue_lr)] * mf02$predicate
mf06$lr <- lr$direction[match(mf06$issue, lr$issue_lr)] * mf06$predicate
mf08$lr <- lr$direction[match(mf08$issue, lr$issue_lr)] * mf08$predicate

### set observations with object actor to missing
mf02$lr[which(is.na(mf02$obj_org)==F)] <- NA
mf06$lr[which(is.na(mf06$obj_org)==F)] <- NA
mf08$lr[which(is.na(mf08$obj_org)==F)] <- NA

### set observations with reality statements to missing
mf02$lr[mf02$pred_spec==2] <- NA
mf06$lr[mf06$pred_spec==2] <- NA
mf08$lr[mf08$pred_spec==2] <- NA

### calculate left-right party positions
ppos <- matrix(NA, 3, 6)
ppos[1, c(1:4,6)]<- tapply(mf02$lr, mf02$sub_org, mean, na.rm=T)[1:5]
ppos[2, 1:5] <- tapply(mf06$lr, mf06$sub_org, mean, na.rm=T)[1:5]
ppos[3, 1:6] <- tapply(mf08$lr, mf08$sub_org, mean, na.rm=T)[1:6]


##########################################################################
### WORDSCORES (for Figure 1)
### references scores from Chapel Hill Expert Survey 1999 (scales from 0 to 10):
lrgen <- c(2.9, 4, 4.6, 6.2, 7.9)
# lrecon <- c(3, 4.2, 7.6, 6.2, 6.4)
# galtan <- c(2, 4, 1.8, 6.5, 7.1)

### import word frequency matrix
d.wfm <- wfm("wfm_stemstop.csv")
d.wfm <- d.wfm[,c(1:5,7:21,6)]

### estimates wordscores
ws <- classic.wordscores(d.wfm[,1:5], scores=lrgen)
ws.pred <- predict(ws, newdata=d.wfm[,6:21])


##########################################################################
### WORDFISH (for Figure 1)
wf <- wordfish(d.wfm[,6:21], dir=c(14,1))


##########################################################################
# build data frame (LR-general)
lr.df <- as.data.frame(as.numeric(ppos)[is.na(as.numeric(ppos))==F])
names(lr.df) <- "autnes"
row.names(lr.df) <- c("sp02", "sp06", "sp08", "vp02", "vp06", "vp08", "fp02", "fp06", "fp08",
                      "gr02", "gr06", "gr08", "bz06", "bz08", "lf02", "lf08")
lr.df$cmp <- c(-17.64, -15.12, -17.87, -0.58, -2.96, -1.15, -18.07, -4.31, 
               -1.30, -16.74, -20.06, -9.47, -3.58, -8.70, NA, NA)
lr.df$experts <- c(3.75, 3.38, 3.36, 7, 7, 7.07, 8.63, 9.67, 8.93, 2.83,
                   2.17, 2.29, 8.83, 8.29, 4.6, NA) # Chapel Hill Expert Survey
lr.df$candidates <- c(3.4, 2.58, 2.39, 6.3, 6.14, 6.16, 6.7, 7.62, 7.43, 2.6, 
                      2.54, 2.31, 6.65, 6.58, 5, 3.69) # AUTNES Candidate Surveys & Müller et al. 2001
lr.df$wordscores <- ws.pred[order(row.names(ws.pred))[c(11:16, 3:8, 1:2, 9:10)],3]
lr.df$wordfish <- wf$theta[c(14:16,1:3,6:11,4:5,12:13)]


################
### FIGURE 1 ###
################

# jpeg("Figure 1 - General left-right scale.jpg", width = 15, height = 20, units="cm", res=600)

par(mfrow=c(6,1), oma=c(1,0,0,0), mar=c(1,10,1,1))

plot(NA, NA, ann=F, axes=F, xlim=c(-1.1,1), ylim=c(0,0.6))
axis(1, pos=0.1, at=c(-1,0,1), lty=1, col="grey70")
for (k in c(0.3, 0.5)) lines(c(-1,1), c(k, k), col="grey70")
for (i in c(1,4,7,10)) lines(lr.df[i:(i+2),1], c(0.5,0.3,0.1), type="o", pch=19, lty=3)
lines(lr.df[13:14,1], c(0.3,0.1), type="o", pch=19, lty=3)
lines(lr.df[15:16,1], c(0.5,0.1), type="o", pch=19, lty=3)
text(lr.df[c(1,4,7,10,13,15), 1], c(0.5,0.5,0.5,0.5,0.3,0.5), labels=c("S", "V", "F", "G", "B", "L"), pos=3)
text(c(-1,-1,-1),c(0.1,0.3,0.5),labels=c("2008","2006","2002"), pos=2, cex=0.9, col="black")
mtext("AUTNES Left-right",side=2,line=1,outer=F,las=1, cex=0.8)

plot(NA, NA, ann=F, axes=F, xlim=c(-27.5,25), ylim=c(0,0.6))
axis(1, pos=0.1, at=c(-25,0,25), lty=1, col="grey70")
for (k in c(0.3, 0.5)) lines(c(-25,25), c(k, k), col="grey70")
for (i in c(1,4,7,10)) lines(lr.df[i:(i+2),2], c(0.5,0.3,0.1), type="o", pch=19, lty=3)
lines(lr.df[13:14,2], c(0.3,0.1), type="o", pch=19, lty=3)
lines(lr.df[15:16,2], c(0.5,0.1), type="o", pch=19, lty=3)
text(lr.df[c(1,4,7,10,13,15), 2], c(0.5,0.5,0.5,0.5,0.3,0.5), labels=c("S", "V", "F", "G", "B", "L"), pos=3)
text(c(-25,-25,-25),c(0.1,0.3,0.5),labels=c("2008","2006","2002"), pos=2, cex=0.9, col="black")
mtext("CMP Rile",side=2,line=1,outer=F,las=1, cex=0.8)

plot(NA, NA, ann=F, axes=F, xlim=c(-0.5,10), ylim=c(0,0.6))
axis(1, pos=0.1, at=c(0,5,10), lty=1, col="grey70")
for (k in c(0.3, 0.5)) lines(c(0,10), c(k, k), col="grey70")
for (i in c(1,4,7,10)) lines(lr.df[i:(i+2),3], c(0.5,0.3,0.1), type="o", pch=19, lty=3)
lines(lr.df[13:14,3], c(0.3,0.1), type="o", pch=19, lty=3)
lines(c(lr.df[15,3], 4.4), c(0.5,0.3), type="o", pch=19, lty=3) # LIF has position=4.4 in CHES 2006 (not in data frame)
text(lr.df[c(1,4,7,10,13,15), 3], c(0.5,0.5,0.5,0.5,0.3,0.5), labels=c("S", "V", "F", "G", "B", "L"), pos=3)
text(c(0,0,0),c(0.1,0.3,0.5),labels=c("2010","2006","2002"), pos=2, cex=0.9, col="black")
mtext("CHES Left-right",side=2,line=1,outer=F,las=1, cex=0.8)

plot(NA, NA, ann=F, axes=F, xlim=c(-0.5,10), ylim=c(0,0.6))
axis(1, pos=0.1, at=c(0,5,10), lty=1, col="grey70")
for (k in c(0.3, 0.5)) lines(c(0,10), c(k, k), col="grey70")
for (i in c(1,4,7,10)) lines(lr.df[i:(i+2),4], c(0.5,0.3,0.1), type="o", pch=19, lty=3)
lines(lr.df[13:14,4], c(0.3,0.1), type="o", pch=19, lty=3)
lines(lr.df[15:16,4], c(0.5,0.1), type="o", pch=19, lty=3)
text(lr.df[c(1,4,7,10,13,15), 4], c(0.5,0.5,0.5,0.5,0.3,0.5), labels=c("S", "V", "F", "G", "B", "L"), pos=3)
text(c(0,0,0),c(0.1,0.3,0.5),labels=c("2008","2006","1997"), pos=2, cex=0.9, col="black")
mtext("Candidates/MPs",side=2,line=1,outer=F,las=1, cex=0.8)

plot(NA, NA, ann=F, axes=F, xlim=c(-0.5,10), ylim=c(0,0.6))
axis(1, pos=0.1, at=c(0,5,10), lty=1, col="grey70")
for (k in c(0.3, 0.5)) lines(c(0,10), c(k, k), col="grey70")
for (i in c(1,4,7,10)) lines(lr.df[i:(i+2),5], c(0.5,0.3,0.1), type="o", pch=19, lty=3)
lines(lr.df[13:14,5], c(0.3,0.1), type="o", pch=19, lty=3)
lines(lr.df[15:16,5], c(0.5,0.1), type="o", pch=19, lty=3)
text(lr.df[c(1,4,7,10,13,15), 5], c(0.5,0.5,0.5,0.5,0.3,0.5), labels=c("S", "V", "F", "G", "B", "L"), pos=3)
text(c(0,0,0),c(0.1,0.3,0.5),labels=c("2008","2006","2002"), pos=2, cex=0.9, col="black")
mtext("Wordscores",side=2,line=1,outer=F,las=1, cex=0.8)

plot(NA, NA, ann=F, axes=F, xlim=c(-2.75,2.5), ylim=c(0,0.6))
axis(1, pos=0.1, at=c(-2.5,0,2.5), lty=1, col="grey70")
for (k in c(0.3, 0.5)) lines(c(-2.5,2.5), c(k, k), col="grey70")
for (i in c(1,4,7,10)) lines(lr.df[i:(i+2),6], c(0.5,0.3,0.1), type="o", pch=19, lty=3)
lines(lr.df[13:14,6], c(0.3,0.1), type="o", pch=19, lty=3)
lines(lr.df[15:16,6], c(0.5,0.1), type="o", pch=19, lty=3)
text(lr.df[c(1,4,7,10,13,15), 6], c(0.5,0.5,0.5,0.5,0.3,0.5), labels=c("S", "V", "F", "G", "B", "L"), pos=3)
text(c(-2.5,-2.5,-2.5),c(0.1,0.3,0.5),labels=c("2008","2006","2002"), pos=2, cex=0.9, col="black")
mtext("Wordfish",side=2,line=1,outer=F,las=1, cex=0.8)

# dev.off()


##########################################################################
### CRIMINAL PUNISHMENT (Figure 2)

### load criminal punishment codes
crim <- read.table("crimpunish.csv", header=T, sep=";")

### generate cp variable
mf08$cp <- crim$direction[match(mf08$issue, crim$issue_lr)] * mf08$predicate

### set observations with object actor to missing
mf08$cp[which(is.na(mf08$obj_org)==F)] <- NA

### set observations with reality statements to missing
mf08$cp[mf08$pred_spec==2] <- NA

### calculate left-right party positions
cp.scores <- tapply(mf08$cp, mf08$sub_org, mean, na.rm=T)[1:6]

### plot criminal punishment positions (only AUTNES manifesto data)
plot(NA, NA, xlim=c(-1, 1), ylim=c(0.5, 1.5), axes=F, xlab="Criminal punishment (AUTNES manifesto scores)", ylab="")
axis(1, at=-1:1, pos=1)
points(cp.scores, rep(1,6), pch=16)
text(cp.scores, rep(1,6), labels=c("S", "V", "F", "G", "B", "L"), pos=3)
# NB: the Greens did not mention criminal punishment in their 2008 manifesto


##########################################################################
### calculate AUTNES economic & cultural scores (for Table 4)
### load left-right codes
lreco <- read.table("lreco_codes.csv", header=T, sep=";")
lrcul <- read.table("lrcul_codes.csv", header=T, sep=";")

### generate left-right variables
mf02$lreco <- lreco$direction[match(mf02$issue, lreco$issue_lreco)] * mf02$predicate
mf06$lreco <- lreco$direction[match(mf06$issue, lreco$issue_lreco)] * mf06$predicate
mf08$lreco <- lreco$direction[match(mf08$issue, lreco$issue_lreco)] * mf08$predicate
mf02$lrcul <- lrcul$direction[match(mf02$issue, lrcul$issue_lrcul)] * mf02$predicate
mf06$lrcul <- lrcul$direction[match(mf06$issue, lrcul$issue_lrcul)] * mf06$predicate
mf08$lrcul <- lrcul$direction[match(mf08$issue, lrcul$issue_lrcul)] * mf08$predicate

### set observations with object actor to missing
mf02$lreco[which(is.na(mf02$obj_org)==F)] <- NA
mf06$lreco[which(is.na(mf06$obj_org)==F)] <- NA
mf08$lreco[which(is.na(mf08$obj_org)==F)] <- NA
mf02$lrcul[which(is.na(mf02$obj_org)==F)] <- NA
mf06$lrcul[which(is.na(mf06$obj_org)==F)] <- NA
mf08$lrcul[which(is.na(mf08$obj_org)==F)] <- NA

### set observations with reality statements to missing
mf02$lreco[mf08$pred_spec==2] <- NA
mf06$lreco[mf08$pred_spec==2] <- NA
mf08$lreco[mf08$pred_spec==2] <- NA
mf02$lrcul[mf08$pred_spec==2] <- NA
mf06$lrcul[mf08$pred_spec==2] <- NA
mf08$lrcul[mf08$pred_spec==2] <- NA

### calculate left-right party positions
ppos.eco <- matrix(NA, 3, 6)
ppos.eco[1, c(1:4,6)]<- tapply(mf02$lreco, mf02$sub_org, mean, na.rm=T)[1:5]
ppos.eco[2, 1:5] <- tapply(mf06$lreco, mf06$sub_org, mean, na.rm=T)[1:5]
ppos.eco[3, 1:6] <- tapply(mf08$lreco, mf08$sub_org, mean, na.rm=T)[1:6]
ppos.cul <- matrix(NA, 3, 6)
ppos.cul[1, c(1:4,6)]<- tapply(mf02$lrcul, mf02$sub_org, mean, na.rm=T)[1:5]
ppos.cul[2, 1:5] <- tapply(mf06$lrcul, mf06$sub_org, mean, na.rm=T)[1:5]
ppos.cul[3, 1:6] <- tapply(mf08$lrcul, mf08$sub_org, mean, na.rm=T)[1:6]


##########################################################################
### calculate Wordscores & Wordfish economic & cultural scores (for Table 4)

# WORDSCORES
# references scores from Chapel Hill Expert Survey 1999 (scales from 0 to 10):
lrecon <- c(3, 4.2, 7.6, 6.2, 6.4)
galtan <- c(2, 4, 1.8, 6.5, 7.1)
### estimate Wordscores
ws.eco <- classic.wordscores(d.wfm[,1:5], scores=lrecon)
ws.eco.pred <- predict(ws.eco, newdata=d.wfm[,6:21])
ws.cul <- classic.wordscores(d.wfm[,1:5], scores=galtan)
ws.cul.pred <- predict(ws.cul, newdata=d.wfm[,6:21])

# WORDFISH
# import economic & cultural word frequency matrices
wfm.eco <- wfm("wfm_eco_stemstop.csv")
wfm.cul <- wfm("wfm_cul_stemstop.csv")
# run Wordfish
wf.eco <- wordfish(wfm.eco, dir=c(14,1))
wf.cul <- wordfish(wfm.cul, dir=c(14,1))


##########################################################################
### CORRELATIONS BETWEEN LR-MEASURES (Table 4)

# correlation matrix LR general:
round(cor(lr.df, use="pairwise.complete.obs"), 2)

# build data frame (LR-economic)
lreco.df <- as.data.frame(as.numeric(ppos.eco)[is.na(as.numeric(ppos.eco))==F])
names(lreco.df) <- "autnes"
row.names(lreco.df) <- c("sp02", "sp06", "sp08", "vp02", "vp06", "vp08", "fp02", "fp06", "fp08",
                      "gr02", "gr06", "gr08", "bz06", "bz08", "lf02", "lf08")
lreco.df$cmp <- c(-0.36,-0.33,-0.39,-0.05,-0.21,-0.32,-0.13,-0.32,-0.38,-0.62,-0.46,-0.42,-0.24,-0.45,NA,NA)
lreco.df$experts <- c(3.44, 2.83, 2.64, 7.56, 6.83, 6.86, 7.31, 4.83, 5, 
                      2.83, 2.6, 2.21, 6, 7.29, 7, NA) # Chapel Hill Expert Survey
lreco.df$candidates <- c(1.62, 1.59, 1.74, 3.13, 2.9, 2.95, 3, 2.82, 2.79, 
                         1.5, 1.69, 1.71, 2.86, 2.7, 3.73, 2.79) # AUTNES Candidate Surveys & Müller et al. 2001
lreco.df$wordscores <- ws.eco.pred[order(row.names(ws.eco.pred))[c(11:16, 3:8, 1:2, 9:10)],3]
lreco.df$wordfish <- wf.eco$theta[c(8:13,16,1:5,14:15,6:7)] *-1

# build data frame (LR-cultural)
lrcul.df <- as.data.frame(as.numeric(ppos.cul)[is.na(as.numeric(ppos.cul))==F])
names(lrcul.df) <- "autnes"
row.names(lrcul.df) <- c("sp02", "sp06", "sp08", "vp02", "vp06", "vp08", "fp02", "fp06", "fp08",
                         "gr02", "gr06", "gr08", "bz06", "bz08", "lf02", "lf08")
lrcul.df$cmp <- c(-0.28,0.03,-0.09,0.03,0.35,0.34,0.19,0.66,0.13,-0.55,-0.27,-0.64,0.39,0.63,NA,NA)
lrcul.df$experts <- c(3.38, 3.5, 3.93, 8.25, 7.67, 7.21, 9, 9.67, 8.71, 
                      1.38, 0.83, 1.5, 8.83, 7.79, 2.4, NA) # Chapel Hill Expert Survey
lrcul.df$candidates <- c(2.82, 2.71, 2.8, 4, 3.7, 3.79, 3.95, 4.55, 4.53, 1.45, 
                         1.96, 2.11, 4.16, 4.19, 1.53, 1.99) # AUTNES Candidate Surveys & Müller et al. 2001
lrcul.df$wordscores <- ws.cul.pred[order(row.names(ws.cul.pred))[c(11:16, 3:8, 1:2, 9:10)],3]
lrcul.df$wordfish <- wf.cul$theta[c(4:9,12:16,1,10:11,2:3)]


###############
### TABLE 4 ###
###############

# correlation matrix left-right general:
round(cor(lr.df, use="pairwise.complete.obs"), 2)
# correlation matrix left-right economic:
round(cor(lreco.df, use="pairwise.complete.obs"), 2)
# correlation matrix left-right cultural:
round(cor(lrcul.df, use="pairwise.complete.obs"), 2)


##################################################################
### export position data for later use in appendix replication
write.csv(lr.df, "positions_lr-general.csv", quote=F)
write.csv(lreco.df, "positions_lr-economic.csv", quote=F)
write.csv(lrcul.df, "positions_lr-cultural.csv", quote=F)
