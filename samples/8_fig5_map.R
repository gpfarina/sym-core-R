###
### FIGURE: Draw canton maps of political support for referendum
###         overlaid with nuclear power plant locations
###
### Ken Benoit 4-Apr-2013
###

if (!require(maptools)) install.packages("maptools", dependencies=TRUE)
if (!require(RColorBrewer)) install.packages("RColorBrewer", dependencies=TRUE)
if (!require(classInt)) install.packages("classInt", dependencies=TRUE)
library(maptools)
library(RColorBrewer)
library(classInt)

load("./fig5_map/CHE_adm1.RData")
cantons <- gadm
poldata <- read.csv("./fig5_map/mapdata.csv")
cantons$nukeplants <- poldata$nuclear_plants
## nuclear power plant map info
plants <- read.table(text="
plant     lat      long
BeznauI   47°33.09 8°13.43
BeznauII  47°33.06 8°13.42
Mühleberg 46°58.08 7°16.05
Gösgen    47°21.58 7°58.00
Leibstadt 47°36.06 8°10.57",
                     header=TRUE, stringsAsFactors=FALSE)
convert <- function(c) {
  z <- sapply((strsplit(c, "[°\\.]")), as.numeric)
  z[1, ] + z[2, ]/60 + z[3, ]/3600
} 
plants$lat <- convert(plants$lat)
plants$long <- convert(plants$long)
plants[1,c(3,2)] <- plants[1,c(3,2)] + c(-.01, -.01) # slight offset for Beznau II
plants$plant[1] <- "Beznau I"
plants$plant[2] <- "Beznau II"

## Popular vote for phase out
cantons$phaseout <- poldata$popinit_yesshare_phaseout
colours <- brewer.pal(5, "Blues")
brks <- classIntervals(cantons$phaseout, n=5, style="quantile")
brks <- brks$brks
quartz(height=8,width=10)
par(mar=c(0,0,0,0))
plot(cantons, 
     col=colours[findInterval(cantons$phaseout, brks, all.inside=TRUE)], 
     axes=F)
legend(x=9.3, y=46.2, legend=leglabs(brks), fill=colours, bty="n")
points(plants$long, plants$lat, col="red", pch=19)
points(plants$long, plants$lat, col="white", pch=21)
identify(plants$long, plants$lat, plants$plant, col="Red")
dev.copy2pdf(file="mapphaseout.pdf")

## Popular vote for moratorium
cantons$moratorium <- poldata$popinit_yesshare_moratorium
colours <- brewer.pal(5, "Blues")
brks <- classIntervals(cantons$moratorium, n=5, style="quantile")
brks <- brks$brks
quartz(height=8,width=10)
par(mar=c(0,0,0,0))
plot(cantons, 
     col=colours[findInterval(cantons$moratorium, brks, all.inside=TRUE)], 
     axes=F)
legend(x=9.3, y=46.2, legend=leglabs(brks), fill=colours, bty="n")
points(plants$long, plants$lat, col="red", pch=19)
points(plants$long, plants$lat, col="white", pch=21)
identify(plants$long, plants$lat, plants$plant, col="Red")
dev.copy2pdf(file="mapmoratorium.pdf")





## Major cities:
# Zurich   08°25'15.9"O (8.42109), 47°19'15.8"N (47.32104)
# Geneva  06°07'18.7"O (6.12187), 46°13'53.7"N (46.23159)
# Bern     07°20'18.2"O (7.33839), 46°53'50.8"N (46.89745)
# Basel    07°29'07.4"O (7.48539), 47°32'44.3"N (47.54564)
# Lucerne  08°13'42.3"O (8.22842), 46°59'16.7"N (46.98797)
#cities <- matrix(c(8.42109, 47.32104,
#                  6.12187, 46.23159,
#                7.33839, 46.89745,
#                 7.48539, 47.54564,
#                 8.22842, 46.98797),
#                ncol=2, byrow=TRUE)
#points(cities, pch=19)


## Nuclear plants:
# Beznau I+II AG  08°13'48.6"O (8.23018), 47°33'02.6"N (47.55073)
# Leibstadt AG     08°10'42.3"O (8.17840), 47°35'39.3"N (47.59425)
# Gösgen SO      07°57'45.2"O (7.96255), 47°22'20.3"N (47.37230)
# Mühleberg BE   08°10'38.4"O (8.17734), 47°35'39.1"N (47.59421)
#points(8.23018, 47.55073, pch=19, col="red")  # Beznau I+II AG
#points(8.16840, 47.58,    pch=19, col="red")     # Leibstadt AG
#points(7.96255, 47.37230, pch=19, col="red")  # Gösgen SO 
#points(8.17734, 47.59419, pch=19, col="red")  # Mühleberg BE
#points(8.23018, 47.55073, pch=21, col="white")  # Beznau I+II AG
#points(8.16840, 47.58,    pch=21, col="white")     # Leibstadt AG
#points(7.96255, 47.37230, pch=21, col="white")  # Gösgen SO 
#points(8.17734, 47.59421, pch=21, col="white")  # Mühleberg BE
