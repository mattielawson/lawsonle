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
#EphemeropteraSpaceNoHab -  32% constrained & 28% conditional
#EphemeropteraHabNoSpace - 10% constrained & 50% conditional

#ClingSpaceNoHab - 48% constrained & 25% conditional
#ClingHabNoSpace - 43% constrained & 69% conditional ##4.3% constrained?

#Individually: 
#The Ephemeroptera had a higher percentage of conditional in habitat so because
#Ephemeroptera are the species with the weakest flight ability (of those with wings) they would
#have lower mobility and be more stuck with the habitat they are thrown into with less of 
#a choice/chance to just move wherever they want.

#The Clingers saw the same pattern as the Ephemeroptera as they too had a higher percentage
#in the HabNoSpace. So they too have low mobility as they are more constrained by space. Because
#they are the type in the grouping by habitat, they too are stuck where they are put with 
#less opportunity to move where they want.

#Two in Comparison
#Because I did pick the two out of the two groupings with the least mobility it does make
#sense that they would both have a higher conditional percentage in habitat as they have
#less opportunity to move by themselves as they are not the most mobile.
#Fair point. What about the relative dispersal of each?

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
EphemeropteraHabitat.rda <- rda(Ephemeroptera.mat, as.data.frame(HabitatbyPatch.mat))
EphemeropteraHabitat.r2a <- RsquareAdj(EphemeropteraHabitat.rda)$adj.r.squared
EphemeropteraHabitat.fwd <- forward.sel(Ephemeroptera.mat,as.data.frame(HabitatbyPatch.mat), adjR2thresh=Space.r2a)
EphemeropteraHabitat.fwd
#Significant = Depth, Flow, Inorg, BOM

ClingHabitat.rda <- rda(Clingers.mat, as.data.frame(HabitatbyPatch.mat))
ClingHabitat.r2a <- RsquareAdj(ClingHabitat.rda)$adj.r.squared
ClingHabitat.fwd <- forward.sel(Clingers.mat,as.data.frame(HabitatbyPatch.mat), adjR2thresh=Space.r2a)
ClingHabitat.fwd
#Significant = Depth


#Part 4: How do you expect selecting both the spatial and the habitat variables would change the results of the RDAs from Part 1 above? (5 points)
  #(You do not need to redo the RDAs, unless you *want* to.)
#I think that selecting both spatial and habitat would allow for us to see more general
#information about the community as a whole because instead of just looking at hoe spatial 
#variables effect the community, we would see how both spatial and habitat together effect 
#the community. We could see more of the variable responses of the community because
#more variables are being looked at.

#This might be true, but it's tangential to the question. Without selecting variables for habitat, you are likely overfitting that part of the model.
#So the "real" influence of space is likely much stronger than what you generated with the above models.