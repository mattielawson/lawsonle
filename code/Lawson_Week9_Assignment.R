# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.

# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
  # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.
library(readxl)
setwd("C:/GitHub/lawsonle/week 9")

abiotic.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")
abiotic <- as.data.frame(abiotic.tibble)
head(abiotic)

abiotic.names <- paste(abiotic$Parcel, abiotic$Land_Use)
abiotic$names <- abiotic.names
head(abiotic)

invert.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Invertebrate_community")
invert <- as.data.frame(invert.tibble)
head(invert)

invert.names <- paste(invert$Parcel, invert$Landuse)
invert$names <- invert.names
head(invert)

abiotic.means <- aggregate(x = abiotic, by = list(abiotic$names), FUN = "mean")
head(abiotic.means)
invert.means <- aggregate(x = invert, by = list(invert$names), FUN = "mean")
head(invert.means)

abiotic.means1 <- abiotic.means[,-16]
abiotic.means2 <- abiotic.means1[,-1:-6]
abiotic.means2 <- sapply(abiotic.means2, as.numeric )
abiotic.means2 <- as.data.frame(abiotic.means2)

invert.means1 <- invert.means[,-41]
invert.means2 <- as.data.frame(invert.means1[,c(-1:-4,-72)])
invert.means2<-invert.means2[-5,]
invert.means2 <- as.data.frame(sapply(invert.means2, as.numeric ))

library(vegan)
colnames(abiotic.means2)
ord <- rda(invert.means2 ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al + TotalP + OlsenP, abiotic.means2)
ord
anova(ord)
plot(ord, ylim = c(-2,2), xlim = c(-5,5))
ord <- rda(invert.means2 ~., abiotic.means2)
ord.int <- rda(invert.means2 ~1, abiotic.means2)
step.mod <- ordistep(ord.int, scope = formula(ord), selection = "both")
step.mod$anova
step.R2mod <- ordiR2step(ord.int, scope = formula(ord), selection = "forward")

ord2 <- rda(invert.means2 ~ pH, abiotic.means2)
ord2
anova(ord2)
plot(ord2)

ord2 <- rda(invert.means2 ~ totalN, abiotic.means2)
ord2
anova(ord2)
plot(ord2)

ord2 <- rda(invert.means2 ~ Perc_ash, abiotic.means2)
ord2
anova(ord2)
plot(ord2)

ord2 <- rda(invert.means2 ~ Kalium, abiotic.means2)
ord2
anova(ord2)
plot(ord2)

ord2 <- rda(invert.means2 ~ Magnesium, abiotic.means2)
ord2
anova(ord2)
plot(ord2)

ord2 <- rda(invert.means2 ~ Ca, abiotic.means2)
ord2
anova(ord2)
plot(ord2)

ord2 <- rda(invert.means2 ~ Al, abiotic.means2)
ord2
anova(ord2)
plot(ord2)

ord2 <- rda(invert.means2 ~ TotalP, abiotic.means2)
ord2
anova(ord2)
plot(ord2)

ord2 <- rda(invert.means2 ~ OlsenP, abiotic.means2)
ord2
anova(ord2)
plot(ord2)
  #None of the predictor variables were significant in explaining ecological significance within the 
  #community. None of the P values of every abiotic factor did not prove to be statistically significant.
  #Things not being significant can tell you things about your community just as significance can.
  #Because we are testing an entire community against single abiotic is saying that the entire community
  #is too big and complex to explain by a single variable and looking at a single species vs a single
  #abiotic factor may prove to be more significant.

# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.
plot(invert.means2$Ixodida~abiotic.means2$pH)
plot(invert.means2$Ixodida~abiotic.means2$totalN)
plot(invert.means2$Ixodida~abiotic.means2$Perc_ash)
plot(invert.means2$Ixodida~abiotic.means2$Kalium)
plot(invert.means2$Ixodida~abiotic.means2$Magnesium)
plot(invert.means2$Ixodida~abiotic.means2$Ca)
plot(invert.means2$Ixodida~abiotic.means2$Al)
plot(invert.means2$Ixodida~abiotic.means2$TotalP)
plot(invert.means2$Ixodida~abiotic.means2$OlsenP)

  #The ecological importance of the lack of significant predictors here is that maybe I just
  #need to look at a different species and one will have significance with an abiotic variable
  #but by looking at the abiotics individually and a single species it is easier to see
  #what is and is not having an effect rather than looking at tons of species at once.

# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.
  #These resulta relate to one another because they are both using data from the same data set so
  #we are looking at the same community of invertebrates and the same abiotic factors in the two
  #types of comparisons, just in different ways. The RDA analysis is looking at the entire community
  #of invertebrates all at one time so you can kind of get a bigger picture and the linear model is
  #(if i did it right) looking at a single species of invertebrate versus an abiotic factor so you
  #are looking at a more specific relationship. 
  #There is value in considering both together when interpreting biotic-abiotic specific interactions
  #because you can see what (or if) some abiotic factor is affecting the whole community but then
  #you can also get a closer look and see what abiotics affect individual biotic factors as well.
  #An abiotic not having an effect can also tell you something about the community as well.

