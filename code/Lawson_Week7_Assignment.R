# Load the "anytime" and "ggplot2" packages to complete this week's assignment.
library("anytime")
library("ggplot2")

# Read the "Plankton_move_average" CSV in from GitHub. 
# These are data from the Great Lakes Environmental Research Laboratory plankton sampling.
setwd("C:/GitHub/lawsonle/week 7")
data <- read.csv("Plankton_move_average.csv")

#Used the following lines to format the date and remove NAs from the dataset:
data$Date <- as.Date(data$Date, origin = "0001-01-01") # Setting values to "day zero".
data <- na.omit(data)

#Plot these population data over time with the following code:
ggplot(data)  +
  xlab("Numeric Date") + ylab("Density Individuals")+
  geom_line(data=data, aes(Date, D.mendotae), color="black", alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, LimncalanusF+LimncalanusM), color="orange",  alpha = 0.7, size=1)+ # adding males and females together, hint: this is actually spelled Limnocalanus
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  theme_bw() 

# Export this plot to have on hand for reference in the next section of the assignment (and upload with your script).

# (1) - Which species is most likely to be r-selected prey and which its primary predator? (2 pts)
  #The species most likely to be r-selected prey is the one represented by the black line.
  #It's primary predator would be the one represented by the yellow line
# What is one relationship the third species MIGHT have to the first two? (2 pts)
  #One relationship the third species might have on the other two is that it could be something
  #that is effected by environmental changes the relationship between the other two causes

#Now copy/paste in the Lotka-Volterra function, plotting script, and load the "deSolve" package from the tutorial:
library("deSolve")
dev.off()
LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

Pars <- c(alpha = 3, beta = 0.5, gamma = .2, delta = .6) 


Pars <- c(alpha = 1, beta = 0.2, gamma = .4, delta = .5) 

State <- c(x = 10, y = 10)
Time <- seq(0, 100, by = 1)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")

legend("topright", c("Limncalanus/R Assignments", "D.mendotae/ Mattie's Stress"), lty = c(1,2), col = c(1,2), box.lwd = 0)



# (2) - What do alpha, beta, gamma, and delta represent in this function? (4 pts)
  #alpha represents the rate of prey population growth
  #beta represents the rate of predation
  #gamma represents rate of prey consumption/ population stability
  #delta represents rate of prey consumption/ predator die-off 

# (3) - By only changing values for alpha, beta, gamma, and/or delta
# change the default parameters of the L-V model to best approximate the relationship between Limncalanus and D.mendotae, assuming both plots are on the same time scale.

    #changed: Pars <- c(alpha = 3, beta = 0.5, gamma = .2, delta = .6)

    #change:

# What are the changes you've made to alpha, beta, gamma, and delta from the default values; and what do they say in a relative sense about the plankton data? (4 pts)
    #The changes that I made was I increased alpha by 1 compares to the default calues.
    #This says that the Plankton data had a greater prey population growth rate than
    #the default parameters did.
# Are there other paramenter changes that could have created the same end result? (2 pts)
# Export your final L-V plot with a legend that includes the appropriate genus and/or species name as if the model results were the real plankton data, 
# and upload with your script. (hint - remember which one is the predator and which is the prey)




