# Load the packages from this week's tutorial.
#In the tutorial we looked at the community as a whole and the swimmers which ultimately matched a prediction we had for their distribution.
setwd("C:/GitHub/lawsonle/week 10")
library(spdep)
library(adespatial)
library(vegan)


#Part 1: Look at two other subsets of the community and determine the relative influence of space and habitat on each following the methods in the tutorial. (10 points)
#The options include groupings by taxonomy, where Diptera (true flies) have the strongest flight ability, Trichoptera the 2nd strongest, 
    #Ephemeroptera are 3rd, and non insects are 4th...because they don't have wings.
#Groupings by habits include the swimmers (off limits for the assignment) as most mobile, sprawlers as 2nd (they move in search of food, but not quickly),
    #and the clingers come in last (they might move up and down on individual rocks).

#Ephemeroptera
PatchLatLon.csv <- read.csv("PatchLatLon.csv", header=T)
BugsByPatch.csv <- read.csv("BugsByPatch.csv", header=T)
HabitatbyPatch.csv <- read.csv("HabitatbyPatch.csv", header=T)
Ephemeroptera.csv <- read.csv("Ephemeroptera.csv", header=T)

PatchLatLon.mat <- as.matrix(PatchLatLon.csv[,-1])
BugsByPatch.mat <- as.matrix(BugsByPatch.csv)
HabitatbyPatch.mat <- as.matrix(HabitatbyPatch.csv)
Ephemeroptera.mat <- as.matrix(Ephemeroptera.csv)

nb<-cell2nb(3,30,"queen")
nb1 <- droplinks(nb, 19, sym=TRUE)
nb2 <- droplinks(nb1, 22, sym=TRUE)
nb3 <- droplinks(nb2, 25, sym=TRUE)
nb4 <- droplinks(nb3, 30, sym=TRUE)

bin.mat <- aem.build.binary(nb4, PatchLatLon.mat, unit.angle = "degrees", rot.angle = 90, rm.same.y = TRUE, plot.connexions = TRUE)
plot(PatchLatLon.mat[,2]~PatchLatLon.mat[,3], xlim = rev(c(76.75,77)))

aem.ev <- aem(aem.build.binary=bin.mat)
aem.df <- aem.ev$vectors[c(-19,-22,-25,-30),]
aem.df

Space.rda <- rda(BugsByPatch.mat, as.data.frame(aem.df))
Space.r2a <- RsquareAdj(Space.rda)$adj.r.squared
aem.fwd <- forward.sel(BugsByPatch.mat,aem.df, adjR2thresh=Space.r2a)

aem.fwd$order
SpaceNoHab.rda <- rda(BugsByPatch.mat, as.data.frame(aem.df[,aem.fwd$order]), HabitatbyPatch.mat)
SpaceNoHab.rda 
anova(SpaceNoHab.rda, perm.max = 10000)
RsquareAdj(SpaceNoHab.rda)

HabNoSpace.rda <- rda(BugsByPatch.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,aem.fwd$order]))
HabNoSpace.rda 
anova(HabNoSpace.rda, perm.max = 10000)
RsquareAdj(HabNoSpace.rda)

EphemeropteraSpace.rda <- rda(Ephemeroptera.mat, as.data.frame(aem.df))
EphemeropteraSpace.r2a <- RsquareAdj(EphemeropteraSpace.rda)$adj.r.squared
Ephemeropteraaem.fwd <- forward.sel(Ephemeroptera.mat,as.data.frame(aem.df), adjR2thresh=Space.r2a)

EphemeropteraSpaceNoHab.rda <- rda(Ephemeroptera.mat, as.data.frame(aem.df[,Ephemeropteraaem.fwd$order]), HabitatbyPatch.mat)
EphemeropteraSpaceNoHab.rda 
anova(EphemeropteraSpaceNoHab.rda, perm.max = 10000)
RsquareAdj(EphemeropteraSpaceNoHab.rda)

EphemeropteraHabNoSpace.rda <- rda(Ephemeroptera.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,Ephemeropteraaem.fwd$order]))
EphemeropteraHabNoSpace.rda 
anova(EphemeropteraHabNoSpace.rda, perm.max = 10000)
RsquareAdj(EphemeropteraHabNoSpace.rda)

#CLINGERS
PatchLatLon.csv <- read.csv("PatchLatLon.csv", header=T)
BugsByPatch.csv <- read.csv("BugsByPatch.csv", header=T)
HabitatbyPatch.csv <- read.csv("HabitatbyPatch.csv", header=T)
Clingers.csv <- read.csv("Clingers.csv", header=T)

PatchLatLon.mat <- as.matrix(PatchLatLon.csv[,-1])
BugsByPatch.mat <- as.matrix(BugsByPatch.csv)
HabitatbyPatch.mat <- as.matrix(HabitatbyPatch.csv)
Clingers.mat <- as.matrix(Clingers.csv)

nb<-cell2nb(3,30,"queen")
nb1 <- droplinks(nb, 19, sym=TRUE)
nb2 <- droplinks(nb1, 22, sym=TRUE)
nb3 <- droplinks(nb2, 25, sym=TRUE)
nb4 <- droplinks(nb3, 30, sym=TRUE)

bin.mat <- aem.build.binary(nb4, PatchLatLon.mat, unit.angle = "degrees", rot.angle = 90, rm.same.y = TRUE, plot.connexions = TRUE)
plot(PatchLatLon.mat[,2]~PatchLatLon.mat[,3], xlim = rev(c(76.75,77)))

aem.ev <- aem(aem.build.binary=bin.mat)
aem.df <- aem.ev$vectors[c(-19,-22,-25,-30),]
aem.df

Space.rda <- rda(BugsByPatch.mat, as.data.frame(aem.df))
Space.r2a <- RsquareAdj(Space.rda)$adj.r.squared
aem.fwd <- forward.sel(BugsByPatch.mat,aem.df, adjR2thresh=Space.r2a)

aem.fwd$order
SpaceNoHab.rda <- rda(BugsByPatch.mat, as.data.frame(aem.df[,aem.fwd$order]), HabitatbyPatch.mat)
SpaceNoHab.rda 
anova(SpaceNoHab.rda, perm.max = 10000)
RsquareAdj(SpaceNoHab.rda)

HabNoSpace.rda <- rda(BugsByPatch.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,aem.fwd$order]))
HabNoSpace.rda 
anova(HabNoSpace.rda, perm.max = 10000)
RsquareAdj(HabNoSpace.rda)

ClingSpace.rda <- rda(Clingers.mat, as.data.frame(aem.df))
ClingSpace.r2a <- RsquareAdj(ClingSpace.rda)$adj.r.squared
Clingaem.fwd <- forward.sel(Clingers.mat,as.data.frame(aem.df), adjR2thresh=Space.r2a)

ClingSpaceNoHab.rda <- rda(Clingers.mat, as.data.frame(aem.df[,Clingaem.fwd$order]), HabitatbyPatch.mat)
ClingSpaceNoHab.rda 
anova(ClingSpaceNoHab.rda, perm.max = 10000)
RsquareAdj(ClingSpaceNoHab.rda)

ClingHabNoSpace.rda <- rda(Clingers.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,Clingaem.fwd$order]))
ClingHabNoSpace.rda 
anova(ClingHabNoSpace.rda, perm.max = 10000)
RsquareAdj(ClingHabNoSpace.rda)


#Part 2: What is your interpretation of the pattern for each group individually, and the two in comparison, based on their mobility? (5 points)
#EphemeropteraSpaceNoHab - Pr(>F)=0.001, rsquared=0.1074084
#EphemeropteraHabNoSpace - Pr(>f)=0.003, rsquared=0.0699888

#ClingSpaceNoHab - Pr(>F)=0.001, rsquared=0.4425484
#ClingHabNoSpace - Pr(>F)=0.251, rsquared=0.01079957


#Part 3: For each of your chosen groups of bugs, perform variable selection for the habitat data rather than the AEM data. Which habitat variables are significant for each? (10 points)
  # Definitions for the habitat column names:
    #Inorg = total suspended inorganic solids in the water column
    #Organ = total suspended organic solids in the water column
    #Chla = Chlorophyll a concentration from benthic rocks collected in-stream
    #BOM = total benthic organic matter in the sample
    #Depth = water depth
    #Flow	= water velocity where samples were collected
    #Fines = Percent of the substrate as "fines" i.e. small particles too small to measure
    #AveAr = The average size of rocks where each sample was collected

EphDF <- data.frame(Ephemeroptera.mat)
HabDF <- data.frame(HabitatbyPatch.mat)

model <- lm(EphDF~Inorg+Organ+Chla+BOM+Depth+Flow+Fines+AveAr, data=HabDF)
anova(model)
ols_step_all_possible(model)


plot(HabitatbyPatch.mat$Inorg~Ephemeroptera.mat)

#Part 4: How do you expect selecting both the spatial and the habitat variables would change the results of the RDAs from Part 1 above? (5 points)
  #(You do not need to redo the RDAs, unless you *want* to.)