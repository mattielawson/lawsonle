# Read in the "Toscano_Griffen_Data.csv" data from GitHub and load the three packages we used in the tutorial this week.
# The paper these data came from is uploaded to Canvas as "Toscano&Griffen_2014_JAE..."
setwd("C:/GitHub/lawsonle/week 6")
df <- read.csv("Toscano_Griffen_Data.csv")
head(df)
library(MASS)
library(MuMIn)

# First create models with the same (y) and method (GLMM) as the published paper, using the GLMM function from the tutorial. 
  #Create two different models using the same 3 predictor (x) variables from the dataset. (4 points each) 
    # In one model only include additive effects.
glmm.mod1 <- glmmPQL(activity.level~prey, family = binomial, random = ~ 1 | block, data = df)
summary(glmm.mod1)
r.squaredGLMM(glmm.mod1)
    # In the other model include one interactive effect.
glmm.mod2 <- glmmPQL(activity.level~eaten, family = binomial, random = ~ 1 | block, data = df)
summary(glmm.mod2)
r.squaredGLMM(glmm.mod2)
    # Use a binomial distribution and block as a random effect in both models to match the paper's analyses. Remember ?family to find distribution names.
?family
#need three predictor variables in each model! You setup everything right, but needed activity~prey+eaten+cue and activity~prey*eaten+cue (or something like that)

# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming df= your data frame (feel free to change that):
df$prop.cons <- df$eaten/df$prey

# (Q1) - The code in line 8 is performing two operations at once. What are they? (2 pts)
    #it is creating a new variable in the data frame, and it is combining the eaten numbers with the
    #prey numbers to make the proportional consumption of prey so that we can use it as the y
    #variable instead of just using one or the other.

# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, specifically, did the results change? (5 pts)
glmm.mod3 <- glmmPQL(activity.level~prop.cons, family = binomial, random = ~ 1 | block, data = df)
summary(glmm.mod3)  
    #The interactive effect did change the variables that predict proportional consumption.
    #The results changed as it made the residual smaller, the p-value was higher than when
    #just comparing to eaten.
  #Where's the model with the interactive effect?

# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)
plot(glmm.mod1)
plot(glmm.mod2)
    #I think that both of the residuals are both very random, but glmm.mod2 seems to be a better
    #fit because the glmm.mod1 seems to have data points forming in vertical lines in the plot
    #but glmm.mod2 is more random
#I would say they both have some patterns going on (horizontal patterns in the second model) but in general agree.

# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)
library(mgcv)
gam.mod2 <- gam(activity.level~ prey, family = binomial, random = list(ID=~ 1), data = df)
summary(gam.mod2)
AIC(gam.mod2)

gam.mod1 <- gam(activity.level~ eaten, family = binomial, random = list(ID=~ 1), data = df)
summary(gam.mod1)
AIC(gam.mod1)

# (Q4) - Which model is a better fit? (2 pt)
    #The second model is a better fit, with activity.level and eaten because it has a lower AIC score.

# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)
    #Honestly, not very personally confident 
#Where are the residual plots for the gams?

