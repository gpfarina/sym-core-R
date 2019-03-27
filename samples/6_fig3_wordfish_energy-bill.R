
####################################################################
#
# Fig. 3: Wordfish ideal point estimation for energy bill no. 01.022
# -- Schwarz, Traber and Benoit (2014)
#
####################################################################


## source the QUANTEDA text library functions incl. library "austin"
if (!require(austin))
    install.packages("austin", repos="http://r-forge.r-project.org",
                     dependencies=TRUE, type="source")

source("./QUANTEDA_0.11.R")

# load text corpus object of analyzed energy bill debate
load("./fig3_wordfish_scalings/energy-debate.corpus.RData")



#### Create term-speaker matrix and further objects for analysis
#### from the energy debate corpus


## extract the term-speaker matrices from text corpus

for (b in c("energy1")) {                 #select energy bill
  for (l in c("de")) {                    #select German texts only
    cat("Processing bill:", b, "\n")
     assign(paste("fvm", l, "orig.500w", b, sep="."), 
           create.fvm.corpus(debates.corpus, groups="mp_id",
                             subset=(role==""       # only MPs without specific parliamentary role
                                     & language==l
                                     & bill==b 
                                     & procedural==0    # no procedural speeches
                                     & interaction==0   # no interaction speeches
                                     & wordcount>=500  # only speeches with >= 500 words
                                     
                             )))
                         }
                       }


# apply trimmer function (if no trimmer apllied then both parameters=1)
wfm.de.orig.500w.energy <- trim(as.wfm(fvm.de.orig.500w.energy1), min.count=1, min.doc=1)



#######################################
# Wordfish ideal point estimation
#######################################

wfish.de.orig.500w.energy <- wordfish((wfm.de.orig.500w.energy), dir=(c(15,5)))
# settings in dir=c(): 15=mp_id 359 Teuscher(GPS) / 5=mp_id 254 Steiner(FDP-Lib)


# # Merge Wordfish ideal points with mp_list & generate output file

wfish.energy <- data.frame(wfish.de.orig.500w.energy[c("docs","theta","se.theta")])
wfish.energy <- unique(merge(wfish.energy, mp_list.leg46, by.x="docs", by.y="mp_id", all.x=T))


save.image("./fig3_wordfish_scalings/energy-debate.corpus.RData")




####################################################
# Fig. 3: Plot for Wordfish ideal point estimation
####################################################


wfish.plot <- wfish.energy

wfish.plot$party_abbreviation[wfish.plot$party_abbreviation=="FDP-Liberale"] <- "FDP"
wfish.plot$party_abbreviation[wfish.plot$party_abbreviation=="LPS"] <- "FDP" #LPS is part of FDP
wfish.plot$party_abbreviation[wfish.plot$party_abbreviation=="GB"] <- "GPS" #GB is part of GPS

wfish.plot$lastname.id <- paste(wfish.plot$lastname,wfish.plot$party_abbreviation, sep = " ")

wfish.plot$group <- 0
wfish.plot$group[wfish.plot$party_abbreviation=="GPS"] <- 5
wfish.plot$group[wfish.plot$party_abbreviation=="SP"] <- 6
wfish.plot$group[wfish.plot$party_abbreviation=="EVP"] <- 4
wfish.plot$group[wfish.plot$party_abbreviation=="CVP"] <- 3
wfish.plot$group[wfish.plot$party_abbreviation=="FDP"] <- 2
wfish.plot$group[wfish.plot$party_abbreviation=="SVP"] <- 1


wfish.plot <- wfish.plot[with(wfish.plot, order(-group, theta)), ]
wfish.plot$rank <- 1:nrow(wfish.plot)

wfish.plot$lastname.id <- reorder(wfish.plot$lastname.id, wfish.plot$rank)


wfish.plot$color <- "grey0" # other parties are grey
wfish.plot$color[wfish.plot$party=="GPS"] <- "green2"
wfish.plot$color[wfish.plot$party=="SP"] <- "red"
wfish.plot$color[wfish.plot$party=="EVP"] <- "turquoise"
wfish.plot$color[wfish.plot$party=="CVP"] <- "orange"
wfish.plot$color[wfish.plot$party=="BDP"] <- "yellow"
wfish.plot$color[wfish.plot$party=="FDP"] <- "blue"
wfish.plot$color[wfish.plot$party=="SVP"] <- "darkgreen"

library("lattice")
dotplot(lastname.id ~ theta, data = wfish.plot,
        #scales=list(y=list(cex=0.8), x=list(cex=0.8)),
        #aspect = "fill",
        groups = wfish.plot$party_abbreviation,
        #xlim = c(min(wfish.plot$theta), max(wfish.plot$theta)),
        xlab = "Wordfish ideal points",
        #main = list(label="Wordfish idealpoints", cex=1),
        panel = function (x, y) {
          panel.xyplot(x, y, pch = 16, col = wfish.plot$color)
          panel.segments(wfish.plot$theta - wfish.plot$se.theta, as.numeric(y),
                         wfish.plot$theta + wfish.plot$se.theta, as.numeric(y), lty = 1,
                         col = wfish.plot$color)
        }) 





