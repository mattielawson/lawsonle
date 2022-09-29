# Look at the plot and model results for our Dryad data in the tutorial. Part 1: Without knowing which points represent which groups, 
  # give one explanation for why these data might be difficult to draw spatial inferences about genes.(3 points)
#One explanation for why is that we can not draw inferences about genes if we do not know which oens
#to compare to one another. In order to draw inferences about a place, we need to know which data points
#to compare, if we do not know which point is from where, we can nor draw conclusions about specific places
#and also can not compare those places to one another.

  # Part 2: Despite the drawbacks, give the result or interpretation that you feel most confident in (3 points), and EXPLAIN WHY (4 points).
#So, to finally answer this question,the interpretation that I feel most confident in is that
#the black data points/genes that are all spread out through the graph is a common gene, or possibly a
#housekeeping gene. This is because it is less concentrated than the other plot points which means that
#it does not belong to only one concentrated area, but is seen spread out over multiple areas studied.

# For your scripting assignment we will use the "ge_data" data frame found in the "stability" package.
  # Install the "stability" package, load it into your R environment, and use the data() function to load the "ge_data". (2 points)
install.packages('stability')
library(stability)
data("ge_data")

# Create two linear models for Yield Response: one related to the Environment and one to the Genotype. (2 points each)
  # 'Yield Response' in this dataset is a measure of phenotype expression.
  # Hint: Look at the help file for this dataset.
setwd("C:/GitHub/lawsonle/week 4")
data <- ge_data
head(data)
ge_data
mod.env <- lm(data$Yield ~ data$Env)
mod.env <- lm(Yield ~ Env, data = ge_data)
anova(mod.env)
summary(mod.env)

mod.gen <- lm(Yield ~ Gen, data = ge_data)
anova(mod.gen)
summary(mod.gen)

# Test the significance of both models and look at the model summary. (3 points each)
  # Which model is a better fit to explain the yield response, and WHY? (6 points)
#The model that is a better fit to explain the yield response is the genotype model. This is because
#it has a smaller p-value from the summary so it has a higher statistical significance than the 
#environment does.
  # Hint: Does one model seem more likely to be over-fitted?

# Which environment would be your very WORST choice for generating a strong yield response? (2 points)
#The environment that would be the worst is EnvSargodha because it had the least significance.
