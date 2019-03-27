#######################################################################################
# REPLICATION CODE FOR APPENDIX FIGURES AND TABLES IN:
### 'Analyzing Manifestos in their Electoral Context: A New Approach Applied to Austria, 2002-2008'
### forthcoming in Political Science Research and Methods
### Authors: Martin Dolezal, Laurenz Ennser-Jedenastik, Wolfgang C. Müller & Anna Katharina Winkler
### Department of Government, University of Vienna
### Austrian National Election Study (AUTNES) | Supply Side, www.autnes.at
#######################################################################################


### set working directory
# setwd("C:/ [...] /replication data/data")


#############################################################################
### Benoit et al. (2009) uncertainty applied to AUTNES 2008 manifesto data (Figure A1)

### load left-right codes
lr <- read.table("lr_codes.csv", header=T, sep=";")

### load manifesto data sets
mf08 <- read.table("mf2008.csv", header=T, sep=";")
names(mf08) <- c("id", "sentnr", "sub_org", "predicate", "pred_spec", "issue", "obj_org", "author")

### simplify predicate values
mf08$predicate[mf08$predicate==0.5] <- 1
mf08$predicate[mf08$predicate==-0.5] <- -1

### remove observations with missing issues, reality statements, or actor-actor statements:
mf08 <- mf08[is.na(mf08$issue)==F,]
mf08 <- mf08[which(is.na(mf08$obj_org)==T),]
mf08 <- mf08[which(mf08$pred_spec!=2),]

### generate predicate-issue combinations from which to draw samples
mf08$converter <- mf08$predicate
mf08$converter[mf08$predicate==-1] <- 100000
mf08$converter[mf08$predicate==0] <- 200000
mf08$converter[mf08$predicate==1] <- 300000
mf08$issue.dir <- mf08$issue + mf08$converter

### number of issue statements per party
sp.len <- nrow(mf08[mf08$sub_org==1100000,])
vp.len <- nrow(mf08[mf08$sub_org==1200000,])
fp.len <- nrow(mf08[mf08$sub_org==1300000,])
gr.len <- nrow(mf08[mf08$sub_org==1400000,])
bz.len <- nrow(mf08[mf08$sub_org==1500000,])
lf.len <- nrow(mf08[mf08$sub_org==1900000,])

### create empty matrices with 1000 columns
sp.mat <- matrix(NA, sp.len, 1000)
vp.mat <- matrix(NA, vp.len, 1000)
fp.mat <- matrix(NA, fp.len, 1000)
gr.mat <- matrix(NA, gr.len, 1000)
bz.mat <- matrix(NA, bz.len, 1000)
lf.mat <- matrix(NA, lf.len, 1000)

### draw 1000 samples per party from issue statements, with replacement
for (i in 1:1000) sp.mat[,i] <- sample(mf08$issue.dir[mf08$sub_org==1100000], sp.len, replace=T)
for (i in 1:1000) vp.mat[,i] <- sample(mf08$issue.dir[mf08$sub_org==1200000], vp.len, replace=T)
for (i in 1:1000) fp.mat[,i] <- sample(mf08$issue.dir[mf08$sub_org==1300000], fp.len, replace=T)
for (i in 1:1000) gr.mat[,i] <- sample(mf08$issue.dir[mf08$sub_org==1400000], gr.len, replace=T)
for (i in 1:1000) bz.mat[,i] <- sample(mf08$issue.dir[mf08$sub_org==1500000], bz.len, replace=T)
for (i in 1:1000) lf.mat[,i] <- sample(mf08$issue.dir[mf08$sub_org==1900000], lf.len, replace=T)

### generate predicate matrices
sp.predmat <- as.matrix(trunc(sp.mat/100000), 0) - 2
vp.predmat <- as.matrix(trunc(vp.mat/100000), 0) - 2
fp.predmat <- as.matrix(trunc(fp.mat/100000), 0) - 2 
gr.predmat <- as.matrix(trunc(gr.mat/100000), 0) - 2
bz.predmat <- as.matrix(trunc(bz.mat/100000), 0) - 2
lf.predmat <- as.matrix(trunc(lf.mat/100000), 0) - 2

### remove first digits from party matrices
sp.mat <- sp.mat - trunc(sp.mat/100000)*100000
vp.mat <- vp.mat - trunc(vp.mat/100000)*100000
fp.mat <- fp.mat - trunc(fp.mat/100000)*100000
gr.mat <- gr.mat - trunc(gr.mat/100000)*100000
bz.mat <- bz.mat - trunc(bz.mat/100000)*100000
lf.mat <- lf.mat - trunc(lf.mat/100000)*100000

### calculate left-right positions for all 1000 draws per party
for (i in 1:1000) sp.mat[,i] <- lr$direction[match(sp.mat[,i], lr$issue_lr)]
sp.pos <- colMeans((sp.mat * sp.predmat), na.rm=T)
for (i in 1:1000) vp.mat[,i] <- lr$direction[match(vp.mat[,i], lr$issue_lr)]
vp.pos <- colMeans((vp.mat * vp.predmat), na.rm=T)
for (i in 1:1000) fp.mat[,i] <- lr$direction[match(fp.mat[,i], lr$issue_lr)]
fp.pos <- colMeans((fp.mat * fp.predmat), na.rm=T)
for (i in 1:1000) gr.mat[,i] <- lr$direction[match(gr.mat[,i], lr$issue_lr)]
gr.pos <- colMeans((gr.mat * gr.predmat), na.rm=T)
for (i in 1:1000) bz.mat[,i] <- lr$direction[match(bz.mat[,i], lr$issue_lr)]
bz.pos <- colMeans((bz.mat * bz.predmat), na.rm=T)
for (i in 1:1000) lf.mat[,i] <- lr$direction[match(lf.mat[,i], lr$issue_lr)]
lf.pos <- colMeans((lf.mat * lf.predmat), na.rm=T)

### AUTNES manifesto positions:
autnespos <- c(-0.5420475, 0.13922356, 0.09664694, -0.5491991, 0.14392060)


#################
### FIGURE A1 ###
#################

# jpeg("Figure A1 - AUTNES left-right plus confidence intervals.jpg", width=20, height=11, unit="cm", res=600)
plot(NA, NA, xlim=c(-1,1), ylim=c(1,6), axes=F, xlab="AUTNES left-right positions", ylab="")
points(autnespos, 1:5, pch=19)
lines(quantile(sp.pos, c(0.975, 0.025)), c(1,1))
lines(quantile(vp.pos, c(0.975, 0.025)), c(2,2))
lines(quantile(fp.pos, c(0.975, 0.025)), c(3,3))
lines(quantile(gr.pos, c(0.975, 0.025)), c(4,4))
lines(quantile(bz.pos, c(0.975, 0.025)), c(5,5))
text(autnespos, 1:5, labels=c("SPÖ", "ÖVP", "FPÖ", "Greens", "BZÖ"), pos=3)
axis(1)
# dev.off()


#############################################################################
### Benoit et al. (2009) uncertainty measures applied to Austrian CMP data 2008:
cmp08 <- read.table("cmp2008.csv", header=T, sep=";")

### number of quasi-sentences per party
qs <- c(1203, 610, 462, 697, 345)

### draw 1000 samples per party from CMP category distribution
sp.cmp <- matrix(NA, qs[1], 1000)
vp.cmp <- matrix(NA, qs[2], 1000)
fp.cmp <- matrix(NA, qs[3], 1000)
gr.cmp <- matrix(NA, qs[4], 1000)
bz.cmp <- matrix(NA, qs[5], 1000)
for (i in 1:1000) sp.cmp[,i] <- sample(cmp08$category, qs[1], replace=T, prob=cmp08$s/100)
for (i in 1:1000) vp.cmp[,i] <- sample(cmp08$category, qs[2], replace=T, prob=cmp08$v/100)
for (i in 1:1000) fp.cmp[,i] <- sample(cmp08$category, qs[3], replace=T, prob=cmp08$f/100)
for (i in 1:1000) gr.cmp[,i] <- sample(cmp08$category, qs[4], replace=T, prob=cmp08$g/100)
for (i in 1:1000) bz.cmp[,i] <- sample(cmp08$category, qs[5], replace=T, prob=cmp08$b/100)

### RILE left & right categories
rile.l <- c(103, 105, 106, 107, 202, 403, 404, 406, 412, 413, 504, 506, 701)
rile.r <- c(104, 201, 203, 305, 401, 402, 407, 414, 505, 601, 603, 605, 606)

### calculate RILE scores for draws
sp.cmp.pos <- rep(NA, 1000)
vp.cmp.pos <- rep(NA, 1000)
fp.cmp.pos <- rep(NA, 1000)
gr.cmp.pos <- rep(NA, 1000)
bz.cmp.pos <- rep(NA, 1000)
for (i in 1:1000) sp.cmp.pos[i] <- (mean(sp.cmp[,i] %in% rile.r) - mean(sp.cmp[,i] %in% rile.l)) * 100
for (i in 1:1000) vp.cmp.pos[i] <- (mean(vp.cmp[,i] %in% rile.r) - mean(vp.cmp[,i] %in% rile.l)) * 100
for (i in 1:1000) fp.cmp.pos[i] <- (mean(fp.cmp[,i] %in% rile.r) - mean(fp.cmp[,i] %in% rile.l)) * 100
for (i in 1:1000) gr.cmp.pos[i] <- (mean(gr.cmp[,i] %in% rile.r) - mean(gr.cmp[,i] %in% rile.l)) * 100
for (i in 1:1000) bz.cmp.pos[i] <- (mean(bz.cmp[,i] %in% rile.r) - mean(bz.cmp[,i] %in% rile.l)) * 100


#################
### FIGURE A2 ###
#################
cmppos <- c(-17.8720, -1.1475, -1.2987, -9.4692, -8.6957)

# jpeg("Figure A2 - CMP Rile plus confidence intervals.jpg", width=20, height=11, unit="cm", res=600)
plot(NA, NA, xlim=c(-100,100), ylim=c(1,6), axes=F, xlab="CMP Rile scores plus confidence intervals", ylab="")
points(cmppos, 1:5, pch=19)
lines(quantile(sp.cmp.pos, c(0.975, 0.025)), c(1,1))
lines(quantile(vp.cmp.pos, c(0.975, 0.025)), c(2,2))
lines(quantile(fp.cmp.pos, c(0.975, 0.025)), c(3,3))
lines(quantile(gr.cmp.pos, c(0.975, 0.025)), c(4,4))
lines(quantile(bz.cmp.pos, c(0.975, 0.025)), c(5,5))
text(cmppos, 1:5, labels=c("SPÖ", "ÖVP", "FPÖ", "Greens", "BZÖ"), pos=3)
axis(1)
# dev.off()



################
### TABLE A2 ###
################

### reload manifesto data
mf02 <- read.table("mf2002.csv", header=T, sep=";")
mf06 <- read.table("mf2006.csv", header=T, sep=";")
mf08 <- read.table("mf2008.csv", header=T, sep=";")
names(mf02) <- c("id", "sentnr", "sub_org", "predicate", "pred_spec", "issue", "obj_org", "author")
names(mf06) <- c("id", "sentnr", "sub_org", "predicate", "pred_spec", "issue", "obj_org", "author")
names(mf08) <- c("id", "sentnr", "sub_org", "predicate", "pred_spec", "issue", "obj_org", "author")

### set issue codes '99999' to NA 
mf02$issue[mf02$issue==99999] <- NA
mf06$issue[mf06$issue==99999] <- NA
mf08$issue[mf08$issue==99999] <- NA

### generate issue level 1 and level 2 categories
mf02$issue2 <- floor(mf02$issue/100)
mf02$issue1 <- floor(mf02$issue/1000)
mf06$issue2 <- floor(mf06$issue/100)
mf06$issue1 <- floor(mf06$issue/1000)
mf08$issue2 <- floor(mf08$issue/100)
mf08$issue1 <- floor(mf08$issue/1000)

### generate author+sentence number variable
mf02$authorsent <- mf02$author + mf02$sentnr
mf06$authorsent <- mf06$author + mf06$sentnr
mf08$authorsent <- mf08$author + mf08$sentnr

### count number of sentences with # of different issue codes
mf02count3 <- tapply(mf02$issue, mf02$authorsent, function(x) length(table(x)))
mf02count2 <- tapply(mf02$issue2, mf02$authorsent, function(x) length(table(x)))
mf02count1 <- tapply(mf02$issue1, mf02$authorsent, function(x) length(table(x)))
mf06count3 <- tapply(mf06$issue, mf06$authorsent, function(x) length(table(x)))
mf06count2 <- tapply(mf06$issue2, mf06$authorsent, function(x) length(table(x)))
mf06count1 <- tapply(mf06$issue1, mf06$authorsent, function(x) length(table(x)))
mf08count3 <- tapply(mf08$issue, mf08$authorsent, function(x) length(table(x)))
mf08count2 <- tapply(mf08$issue2, mf08$authorsent, function(x) length(table(x)))
mf08count1 <- tapply(mf08$issue1, mf08$authorsent, function(x) length(table(x)))

### Produce percentages reported in Table A2:
# 2002, Level 3:
sum(table(mf02count3)[1:2]) / sum(table(mf02count3))
sum(table(mf02count3)[3]) / sum(table(mf02count3))
sum(table(mf02count3)[4:length(table(mf02count3))]) / sum(table(mf02count3))
# 2002, Level 2:
sum(table(mf02count2)[1:2]) / sum(table(mf02count2))
sum(table(mf02count2)[3]) / sum(table(mf02count2))
sum(table(mf02count2)[4:length(table(mf02count2))]) / sum(table(mf02count3))
# 2002, Level 1:
sum(table(mf02count1)[1:2]) / sum(table(mf02count1))
sum(table(mf02count1)[3]) / sum(table(mf02count1))
sum(table(mf02count1)[4:length(table(mf02count1))]) / sum(table(mf02count3))

# 2006, Level 3:
sum(table(mf06count3)[1:2]) / sum(table(mf06count3))
sum(table(mf06count3)[3]) / sum(table(mf06count3))
sum(table(mf06count3)[4:length(table(mf06count3))]) / sum(table(mf06count3))
# 2006, Level 2:
sum(table(mf06count2)[1:2]) / sum(table(mf06count2))
sum(table(mf06count2)[3]) / sum(table(mf06count2))
sum(table(mf06count2)[4:length(table(mf06count2))]) / sum(table(mf06count3))
# 2006, Level 1:
sum(table(mf06count1)[1:2]) / sum(table(mf06count1))
sum(table(mf06count1)[3]) / sum(table(mf06count1))
sum(table(mf06count1)[4:length(table(mf06count1))]) / sum(table(mf06count3))

# 2008, Level 3:
sum(table(mf08count3)[1:2]) / sum(table(mf08count3))
sum(table(mf08count3)[3]) / sum(table(mf08count3))
sum(table(mf08count3)[4:length(table(mf08count3))]) / sum(table(mf08count3))
# 2008, Level 2:
sum(table(mf08count2)[1:2]) / sum(table(mf08count2))
sum(table(mf08count2)[3]) / sum(table(mf08count2))
sum(table(mf08count2)[4:length(table(mf08count2))]) / sum(table(mf08count3))
# 2008, Level 1:
sum(table(mf08count1)[1:2]) / sum(table(mf08count1))
sum(table(mf08count1)[3]) / sum(table(mf08count1))
sum(table(mf08count1)[4:length(table(mf08count1))]) / sum(table(mf08count3))


### share of single-statement sentences (Table A2)
# 2002:
mean(table(mf02$authorsent)==1)
# 2006:
mean(table(mf06$authorsent)==1)
# 2008:
mean(table(mf08$authorsent)==1)





################
### TABLE A3 ###
################


### re-load left-right codes
lr <- read.table("lr_codes.csv", header=T, sep=";")

### re-load manifesto data sets
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

### gen dichotomous indicator for left-right coverage
mf02$lr.cov <- 0
mf02$lr.cov[is.na(mf02$lr)==F] <- 1
mf06$lr.cov <- 0
mf06$lr.cov[is.na(mf06$lr)==F] <- 1
mf08$lr.cov <- 0
mf08$lr.cov[is.na(mf08$lr)==F] <- 1

### set observations missing that cannot fall on lr-scale (actor-actor statements, reality statements)
mf02$lr.cov[which(is.na(mf02$obj_org)==F)] <- NA
mf06$lr.cov[which(is.na(mf06$obj_org)==F)] <- NA
mf08$lr.cov[which(is.na(mf08$obj_org)==F)] <- NA
mf02$lr.cov[mf02$pred_spec==2] <- NA
mf06$lr.cov[mf06$pred_spec==2] <- NA
mf08$lr.cov[mf08$pred_spec==2] <- NA

### Coverage of AUTNES general left-right scale:
# 11=SPÖ, 12=ÖVP, 13=FPÖ, 14=Greens, 15=BZÖ, 19=LIF
tapply(mf02$lr.cov, mf02$sub_org, mean, na.rm=T)[1:5] # 2002:
# 11=SPÖ, 12=ÖVP, 13=FPÖ, 14=Greens, 15=BZÖ, 19=LIF
tapply(mf06$lr.cov, mf06$sub_org, mean, na.rm=T)[1:5] # 2006:
# 11=SPÖ, 12=ÖVP, 13=FPÖ, 14=Greens, 15=BZÖ, 19=LIF
tapply(mf08$lr.cov, mf08$sub_org, mean, na.rm=T)[1:6] # 2008:





################
### TABLE A4 ###
################

### load library
require(irr) # if package not installed run: 'install.packages("irr")'


### load double-coded manifesto data for left-right reliability
lrrel08 <- read.table("lr_reliability_2008.csv", header=T, sep=";")
lrrel06 <- read.table("lr_reliability_2006.csv", header=T, sep=";")
lrrel02 <- read.table("lr_reliability_2002.csv", header=T, sep=";")

### generate left-right variable
lrrel08$lr1 <- lr$direction[match(lrrel08$issue1, lr$issue_lr)] * lrrel08$predicate1
lrrel08$lr2 <- lr$direction[match(lrrel08$issue2, lr$issue_lr)] * lrrel08$predicate2
lrrel06$lr1 <- lr$direction[match(lrrel06$issue1, lr$issue_lr)] * lrrel06$predicate1
lrrel06$lr2 <- lr$direction[match(lrrel06$issue2, lr$issue_lr)] * lrrel06$predicate2
lrrel02$lr1 <- lr$direction[match(lrrel02$issue1, lr$issue_lr)] * lrrel02$predicate1
lrrel02$lr2 <- lr$direction[match(lrrel02$issue2, lr$issue_lr)] * lrrel02$predicate2

### set observations with object actor to missing
lrrel08$lr1[lrrel08$obj_org1!=""] <- NA
lrrel08$lr2[lrrel08$obj_org2!=""] <- NA
lrrel06$lr1[lrrel06$obj_org1!=""] <- NA
lrrel06$lr2[lrrel06$obj_org2!=""] <- NA
lrrel02$lr1[lrrel02$obj_org1!=""] <- NA
lrrel02$lr2[lrrel02$obj_org2!=""] <- NA

### set observations with reality statements to missing
lrrel08$lr1[lrrel08$pred_spec1==2] <- NA
lrrel08$lr2[lrrel08$pred_spec2==2] <- NA
lrrel06$lr1[lrrel06$pred_spec1==2] <- NA
lrrel06$lr2[lrrel06$pred_spec2==2] <- NA
lrrel02$lr1[lrrel02$pred_spec1==2] <- NA
lrrel02$lr2[lrrel02$pred_spec2==2] <- NA

### simplify predicate values
lrrel08$lr1[lrrel08$lr1==0.5] <- 1
lrrel08$lr1[lrrel08$lr1==-0.5] <- -1
lrrel08$lr2[lrrel08$lr2==0.5] <- 1
lrrel08$lr2[lrrel08$lr2==-0.5] <- -1
lrrel06$lr1[lrrel06$lr1==0.5] <- 1
lrrel06$lr1[lrrel06$lr1==-0.5] <- -1
lrrel06$lr2[lrrel06$lr2==0.5] <- 1
lrrel06$lr2[lrrel06$lr2==-0.5] <- -1
lrrel02$lr1[lrrel02$lr1==0.5] <- 1
lrrel02$lr1[lrrel02$lr1==-0.5] <- -1
lrrel02$lr2[lrrel02$lr2==0.5] <- 1
lrrel02$lr2[lrrel02$lr2==-0.5] <- -1

### arrange left-right reliability values in matrix
relmat <- matrix(NA, 3, 6)
relmat[3,1] <- kripp.alpha(t(lrrel08[lrrel08$sub_org1=="spoe", 11:12]), method="nominal")$value
relmat[3,2] <- kripp.alpha(t(lrrel08[lrrel08$sub_org1=="oevp", 11:12]), method="nominal")$value
relmat[3,3] <- kripp.alpha(t(lrrel08[lrrel08$sub_org1=="fpoe", 11:12]), method="nominal")$value
relmat[3,4] <- kripp.alpha(t(lrrel08[lrrel08$sub_org1=="greens", 11:12]), method="nominal")$value
relmat[3,5] <- kripp.alpha(t(lrrel08[lrrel08$sub_org1=="bzoe", 11:12]), method="nominal")$value
relmat[3,6] <- kripp.alpha(t(lrrel08[lrrel08$sub_org1=="lif", 11:12]), method="nominal")$value
relmat[2,1] <- kripp.alpha(t(lrrel06[lrrel06$sub_org1=="spoe", 11:12]), method="nominal")$value
relmat[2,2] <- kripp.alpha(t(lrrel06[lrrel06$sub_org1=="oevp", 11:12]), method="nominal")$value
relmat[2,3] <- kripp.alpha(t(lrrel06[lrrel06$sub_org1=="fpoe", 11:12]), method="nominal")$value
relmat[2,4] <- kripp.alpha(t(lrrel06[lrrel06$sub_org1=="greens", 11:12]), method="nominal")$value
relmat[2,5] <- kripp.alpha(t(lrrel06[lrrel06$sub_org1=="bzoe", 11:12]), method="nominal")$value
relmat[1,1] <- kripp.alpha(t(lrrel02[lrrel02$sub_org1=="spoe", 11:12]), method="nominal")$value
relmat[1,2] <- kripp.alpha(t(lrrel02[lrrel02$sub_org1=="oevp", 11:12]), method="nominal")$value
relmat[1,3] <- kripp.alpha(t(lrrel02[lrrel02$sub_org1=="fpoe", 11:12]), method="nominal")$value
relmat[1,4] <- kripp.alpha(t(lrrel02[lrrel02$sub_org1=="greens", 11:12]), method="nominal")$value
relmat[1,6] <- kripp.alpha(t(lrrel02[lrrel02$sub_org1=="lif", 11:12]), method="nominal")$value

### inspect matrix (=TABLE A4)
round(relmat, 2)






#################
### FIGURE A3 ###
#################

### load left-right positions (exported in replication file for main document)
lr.df <- read.table("positions_lr-general.csv", sep=",", header=T)
lreco.df <- read.table("positions_lr-economic.csv", sep=",", header=T)
lrcul.df <- read.table("positions_lr-cultural.csv", sep=",", header=T)

### set values for graph
x <- 0.9
mar <- 0.6


# jpeg("Figure A3 - Scatterplots of LR scales.jpg", width=35, height=20, unit="cm", res=600)
### GENERAL LR SCALES
par(mfrow=c(3,5), mar=c(3,4,1,1), oma=c(2,2,1,1))

plot(NA, NA, xlim=c(-1,1), ylim=c(-25,25), xlab="AUTNES general LR", ylab="CMP RILE", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lr.df$cmp~lr.df$autnes)$coeff, lwd=3, col="grey80")
abline(0,25, lty=3, col="grey50")
points(lr.df$autnes, lr.df$cmp, pch=1, cex=0.6, lwd=3)

plot(NA, NA, xlim=c(-1,1), ylim=c(0,10), xlab="AUTNES general LR", ylab="CHES LR", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lr.df$experts~lr.df$autnes)$coeff, lwd=3, col="grey80")
abline(a=5,b=5, lty=3, col="grey50")
points(lr.df$autnes, lr.df$experts, pch=1, cex=0.6, lwd=3)

plot(NA, NA, xlim=c(-1,1), ylim=c(0,10), xlab="AUTNES general LR", ylab="Candidates/MPs LR", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lr.df$candidates~lr.df$autnes)$coeff, lwd=3, col="grey80")
abline(5,5, lty=3, col="grey50")
points(lr.df$autnes, lr.df$candidates, pch=1, cex=0.6, lwd=3)

plot(NA, NA, xlim=c(-1,1), ylim=c(0,10), xlab="AUTNES general LR", ylab="Wordscores LR", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lr.df$wordscores~lr.df$autnes)$coeff, lwd=3, col="grey80")
abline(5,5, lty=3, col="grey50")
points(lr.df$autnes, lr.df$wordscores, pch=1, cex=0.6, lwd=3)

plot(NA, NA, xlim=c(-1,1), ylim=c(-2.5, 2.5), xlab="AUTNES general LR", ylab="Wordfish LR", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lr.df$wordfish~lr.df$autnes)$coeff, lwd=3, col="grey80")
abline(0,2.5, lty=3, col="grey50")
points(lr.df$autnes, lr.df$wordfish, pch=1, cex=0.6, lwd=3)

### ECONOMIC SCALES
plot(NA, NA, xlim=c(-1,1), ylim=c(-1,1), xlab="AUTNES economic LR", ylab="CMP economic LR", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lreco.df$cmp~lreco.df$autnes)$coeff, lwd=3, col="grey80")
abline(0,1, lty=3, col="grey50")
points(lreco.df$autnes, lreco.df$cmp, pch=1, cex=0.6, lwd=3)

plot(NA, NA, xlim=c(-1,1), ylim=c(0,10), xlab="AUTNES economic LR", ylab="CHES economic LR", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lreco.df$experts~lreco.df$autnes)$coeff, lwd=3, col="grey80")
abline(a=5,b=5, lty=3, col="grey50")
points(lreco.df$autnes, lreco.df$experts, pch=1, cex=0.6, lwd=3)

plot(NA, NA, xlim=c(-1,1), ylim=c(1,5), xlab="AUTNES economic LR", ylab="Cand/MPs economic LR", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lreco.df$candidates~lreco.df$autnes)$coeff, lwd=3, col="grey80")
abline(3,2, lty=3, col="grey50")
points(lreco.df$autnes, lreco.df$candidates, pch=1, cex=0.6, lwd=3)

plot(NA, NA, xlim=c(-1,1), ylim=c(0,10), xlab="AUTNES economic LR", ylab="WS economic LR", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lreco.df$wordscores~lreco.df$autnes)$coeff, lwd=3, col="grey80")
abline(5,5, lty=3, col="grey50")
points(lreco.df$autnes, lreco.df$wordscores, pch=1, cex=0.6, lwd=3)

plot(NA, NA, xlim=c(-1,1), ylim=c(-2.5, 2.5), xlab="AUTNES economic LR", ylab="WF economic LR", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lreco.df$wordfish~lreco.df$autnes)$coeff, lwd=3, col="grey80")
abline(0,2.5, lty=3, col="grey50")
points(lreco.df$autnes, lreco.df$wordfish, pch=1, cex=0.6, lwd=3)

### CULTURAL SCALES
plot(NA, NA, xlim=c(-1,1), ylim=c(-1,1), xlab="AUTNES cultural LR", ylab="CMP cultural LR", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lrcul.df$cmp~lrcul.df$autnes)$coeff, lwd=3, col="grey80")
abline(0,1, lty=3, col="grey50")
points(lrcul.df$autnes, lrcul.df$cmp, pch=1, cex=0.6, lwd=3)

plot(NA, NA, xlim=c(-1,1), ylim=c(0,10), xlab="AUTNES cultural LR", ylab="CHES GAL/TAN", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lrcul.df$experts~lrcul.df$autnes)$coeff, lwd=3, col="grey80")
abline(a=5,b=5, lty=3, col="grey50")
points(lrcul.df$autnes, lrcul.df$experts, pch=1, cex=0.6, lwd=3)

plot(NA, NA, xlim=c(-1,1), ylim=c(1,5), xlab="AUTNES cultural LR", ylab="Cand/MPs cultural LR", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lrcul.df$candidates~lrcul.df$autnes)$coeff, lwd=3, col="grey80")
abline(3,2, lty=3, col="grey50")
points(lrcul.df$autnes, lrcul.df$candidates, pch=1, cex=0.6, lwd=3)

plot(NA, NA, xlim=c(-1,1), ylim=c(0,10), xlab="AUTNES cultural LR", ylab="WS cultural LR", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lrcul.df$wordscores~lrcul.df$autnes)$coeff, lwd=3, col="grey80")
abline(5,5, lty=3, col="grey50")
points(lrcul.df$autnes, lrcul.df$wordscores, pch=1, cex=0.6, lwd=3)

plot(NA, NA, xlim=c(-1,1), ylim=c(-2.5, 2.5), xlab="AUTNES cultural LR", ylab="WF cultural LR", las=1, mgp=c(2,mar,0), tcl=-0.3)
abline(lm(lrcul.df$wordfish~lrcul.df$autnes)$coeff, lwd=3, col="grey80")
abline(0,2.5, lty=3, col="grey50")
points(lrcul.df$autnes, lrcul.df$wordfish, pch=1, cex=0.6, lwd=3)

# dev.off()



#############################################################################
### Concordance correlation coefficients (for Figure A4)
require(epiR) # if package epiR not avaiable, type 'install.packages("epiR")'

### rescale left-right scores (all estimates to scale from -1 to +1, except Wordfish to mean and sd of AUTNES data)
lr.df.r <- lr.df
lr.df.r$cmp <- lr.df$cmp/100
lr.df.r$experts <- lr.df$experts / 5 - 1
lr.df.r$candidates <- lr.df$candidates / 5 - 1
lr.df.r$wordscores <- lr.df$wordscores / 5 - 1
lr.df.r$wordfish <- mean(lr.df$autnes) + (lr.df$wordfish - mean(lr.df$wordfish)) * (sd(lr.df$autnes)/sd(lr.df$wordfish))

lreco.df.r <- lreco.df
lreco.df.r$cmp <- lreco.df$cmp
lreco.df.r$experts <- lreco.df$experts / 5 - 1
lreco.df.r$candidates <- ((lreco.df$candidates-1)/2)-1
lreco.df.r$wordscores <- lreco.df$wordscores / 5 - 1
lreco.df.r$wordfish <- mean(lreco.df$autnes) + 
  (lreco.df$wordfish - mean(lreco.df$wordfish)) * (sd(lreco.df$autnes)/sd(lreco.df$wordfish))

lrcul.df.r <- lrcul.df
lrcul.df.r$cmp <- lrcul.df$cmp
lrcul.df.r$experts <- lrcul.df$experts / 5 - 1
lrcul.df.r$candidates <- ((lrcul.df$candidates-1)/2)-1
lrcul.df.r$wordscores <- lrcul.df$wordscores / 5 - 1
lrcul.df.r$wordfish <- mean(lrcul.df$autnes) + 
  (lrcul.df$wordfish - mean(lrcul.df$wordfish)) * (sd(lrcul.df$autnes)/sd(lrcul.df$wordfish))

### BUILD DATA FRAME
x <- data.frame(cbind(est=rep(NA,15),lower=rep(NA,15),upper=rep(NA,15)))

x[15,] <- epi.ccc(lrcul.df.r$autnes, lrcul.df.r$cmp)$rho.c
x[14,] <- epi.ccc(lrcul.df.r$autnes, lrcul.df.r$experts)$rho.c
x[13,] <- epi.ccc(lrcul.df.r$autnes, lrcul.df.r$candidates)$rho.c
x[12,] <- epi.ccc(lrcul.df.r$autnes, lrcul.df.r$wordscores)$rho.c
x[11,] <- epi.ccc(lrcul.df.r$autnes, lrcul.df.r$wordfish)$rho.c

x[10,] <- epi.ccc(lreco.df.r$autnes, lreco.df.r$cmp)$rho.c
x[9,] <- epi.ccc(lreco.df.r$autnes, lreco.df.r$experts)$rho.c
x[8,] <- epi.ccc(lreco.df.r$autnes, lreco.df.r$candidates)$rho.c
x[7,] <- epi.ccc(lreco.df.r$autnes, lreco.df.r$wordscores)$rho.c
x[6,] <- epi.ccc(lreco.df.r$autnes, lreco.df.r$wordfish)$rho.c

x[5,] <- epi.ccc(lr.df.r$autnes, lr.df.r$cmp)$rho.c
x[4,] <- epi.ccc(lr.df.r$autnes, lr.df.r$experts)$rho.c
x[3,] <- epi.ccc(lr.df.r$autnes, lr.df.r$candidates)$rho.c
x[2,] <- epi.ccc(lr.df.r$autnes, lr.df.r$wordscores)$rho.c
x[1,] <- epi.ccc(lr.df.r$autnes, lr.df.r$wordfish)$rho.c



#################
### FIGURE A4 ###
#################

# jpeg("Figure A4 - Concordance correlation coefficients.jpg", width=20, height=20, unit="cm", res=600)
par(mfrow=c(3,1), mar=c(3,12,1,1))
plot(NA,NA,xlim=c(-1,1), ylim=c(0,4.5), ann=F, axes=F)
abline(v=seq(-1,1,0.5), lty=3, col="grey80")
points(x$est[1:5], 0:4, pch=19)
for (i in 1:5) lines(c(x$lower[i], x$upper[i]), c(i-1,i-1))
axis(1, pos=-.25)
text(x$est[1:5], 0:4, labels=c("Wordfish", "Wordscores", "Cand/MPs", "Experts", "CMP"), pos=3, cex=1)
mtext("General left-right", side=2, las=1)

plot(NA,NA,xlim=c(-1,1), ylim=c(0,4.5), ann=F, axes=F)
abline(v=seq(-1,1,0.5), lty=3, col="grey80")
points(x$est[6:10], 0:4, pch=19)
for (i in 1:5) lines(c(x$lower[i+5], x$upper[i+5]), c(i-1,i-1))
axis(1, pos=-.25)
text(x$est[6:10], 0:4, labels=c("Wordfish", "Wordscores", "Cand/MPs", "Experts", "CMP"), pos=3, cex=1)
mtext("Economic left-right", side=2, las=1)

plot(NA,NA,xlim=c(-1,1), ylim=c(0,4.5), ann=F, axes=F)
abline(v=seq(-1,1,0.5), lty=3, col="grey80")
points(x$est[11:15], 0:4, pch=19)
for (i in 1:5) lines(c(x$lower[i+10], x$upper[i+10]), c(i-1,i-1))
axis(1, pos=-.25)
text(x$est[11:15], 0:4, labels=c("Wordfish", "Wordscores", "Cand/MPs", "Experts", "CMP"), pos=3, cex=1)
mtext("Cultural left-right", side=2, las=1)
# dev.off()