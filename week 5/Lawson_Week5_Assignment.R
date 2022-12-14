# First, recreate Figure 4 from Herron et al. (2019). De novo origins of multicellularity in response to predation. Scientific Reports.
  # Search datadryad.org by the paper title and download the dataset. It will include .csv files and R scripts, organized by figure.
  # Save the script and change the working directory on lines 8 and 115 to match your GitHub repository. (6 points)
  # Export and save the plot you've created. (2 points)
  # Zoom into your plot to look at the distribution for different strains.

# Do all of the strains in the plot have the same distributions (yes/no)? (2 pt)
  #No, the strains do not have the same distributions

# Based on these observations of your strain distributions, why did the authors use a Kruskal-Wallis test rather than ANOVA to compare the strains? (2 pts)
  #Because this data does not have a normal distribution so an ANOVA would not work
#Why does K-W work instead? (median/anova by ranks)

# Use the fitdist() and gofstat() functions to compare the poisson, negative binomial, and logistic distributions for:
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)
  # (2) - The replication time (data$RepTime.sec)
      # 3 points each
    #HINT- "Num.Cells.Progeny" has defined breaks. To display results, use the formula with the "chisqbreaks" argument as follows:
      #gofstat(list(fit.1, fit.2, fit.3, etc), chisqbreaks=c(1,2,4,8,16,32,64))
library(fitdistrplus)
library(logspline)
setwd("C:/GitHub/lawsonle/week 5")
data <- read.csv(file=("Figure4Data.csv"), header=TRUE)
data <-na.omit(data)

one.col <- data$Num.Cells.Progeny
fit.norm <- fitdist(one.col, distr = "norm")
fit.norm <- fitdist(one.col*100, distr = "norm")
fit.logis <- fitdist(one.col*100, distr = "logis")
fit.weibull <- fitdist(one.col*100, distr = "weibull", lower = c(0, 0), start = list(scale = 1, shape = 1))
fit.gamma <- fitdist(one.col*100, distr = "gamma", lower = c(0, 0), start = list(scale = 1, shape = 1))
#gofstat(list(fit.1, fit.2, fit.3, etc), chisqbreaks=c(1,2,4,8,16,32,64))
gofstat(list(fit.weibull, fit.gamma, fit.norm, fit.logis), chisqbreaks=c(1,2,4,8,16,32,64))

one.col <- data$RepTime.sec
fit.norm <- fitdist(one.col, distr = "norm")
fit.norm <- fitdist(one.col*100, distr = "norm")
fit.logis <- fitdist(one.col*100, distr = "logis")
fit.weibull <- fitdist(one.col*100, distr = "weibull", lower = c(0, 0), start = list(scale = 1, shape = 1))
fit.gamma <- fitdist(one.col*100, distr = "gamma", lower = c(0, 0), start = list(scale = 1, shape = 1))
gofstat(list(fit.weibull, fit.gamma, fit.norm, fit.logis), chisqbreaks=c(1,2,4,8,16,32,64))

# Based on the AIC scores, which distribution is the best fit for: (4 pts)
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)?
#The best fit is gamma.
  # (2) - The replication time (data$RepTime.sec)?
#The best fit is weibull.
#This aren't the distributions from the directions...

# Plot a generic histogram for the replication time (data$RepTime.sec) (2 pt)
hist(data$RepTime.sec)

# Based on the patterns of this histograms and Figure 4:
  #Give one hypothesis for an evolutionary process represented by the two tallest bars in your histogram. (6 pts)
  # Don't cheat by looking at the paper! 
    # This hypothesis does not need to be correct - it only needs to be ecologically rational based these two figures.
#My hypothesis is that these two tallest bars represent an adaptation that the observed
#specimen developed to avoid getting killed by a predator or something in its environment
#so it did not get killed as often. But as the bars go down after, the predator evolved too 
#in response to the prey, or something in the environment changed after the mutation.
