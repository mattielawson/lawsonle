# (1) Approximately how many hours ahead of Sunbury was the peak flow in Lewisburg during the 2011 flood? (2 pt)
  #The peak flow in Lewisburg was about 9 hours ahead of Sunbury

# (2) Give one reason why information on the time between peak flow events up- and downstream could be valuable? (4 pts)
  #One reason why it would be important is because if we can tell at what time peak flow was upstream, and then what time 
  #peak flow was downstream was, we can see the difference between them and how long it takes for the high flow to travel 
  #throughout the river, or even calculate the flow rate itself. We can then try to see the difference in effects to up 
  #and downstream. High flow can have damaging effects to streams and communities that live in them, we can see if some 
  #species can handle the flow rates.


# Package scavenger hunt! (12 pts each)

## (3) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that contains at least one function specifically designed to measure genetic drift.
    # Copy-paste into your script - and run - an example from the reference manual for a function within this package related to a measure of genetic drift. 
        # Depending on the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
    # After running the function example, manipulate a parameter within the function to create a new result. 
        # Common options might be allele frequency, population size, fitness level, etc. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
       
          # By manipulating these parameters you can see how it impacts the results.
          # This type of manipulation is one example of how theoretical ecology and modelling are used to predict patterns in nature.

#Genetic Drift Code
Alleles <- c("A","B")
sample_size <- c(5,10,20,50,100)
df <- data.frame(N=rep(sample_size,each=50),FreqA=NA)
for( row in 1:nrow(df) ) {
  a <- sample( Alleles, size=df$N[row],replace = T)
  f <- sum( a == "A" ) / length(a)
  df$FreqA[row] <- f
}
summary(df)
library(ggplot2)
p <- ggplot(df, aes(x=factor(N), y=FreqA)) + geom_boxplot() 
p + xlab("Population Size") + ylab("Estimated Allele Frequency")

#Manipulation of Script for Genetic Drift (manipulate sample size)
Alleles <- c("A","B")
sample_size <- c(7,13,28,55,198)
df <- data.frame(N=rep(sample_size,each=50),FreqA=NA)
for( row in 1:nrow(df) ) {
  a <- sample( Alleles, size=df$N[row],replace = T)
  f <- sum( a == "A" ) / length(a)
  df$FreqA[row] <- f
}
summary(df)
library(ggplot2)
p <- ggplot(df, aes(x=factor(N), y=FreqA)) + geom_boxplot() 
p + xlab("Population Size") + ylab("Estimated Allele Frequency")


## (4) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that will generate standard diversity metrics for community ecology, specifically Simpson's Diversity Index.
    # Copy-paste into your script - and run - an example from the reference manual for a function to calculate Simpson's diversity. 
        # Depending on the example usage of the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
    # After running the function example, modify your script to generate another diversity metric that is NOT part of the example. 
        # If there are two diversity metrics in the example script, neither of these will count as the modified script.
        # Hint: If the function can "only" caluclate Simpson's diversity, the inverse of Simpson's diversity is another common metric. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
        
          # Diversity metrics are frequently used in community ecology for reasons ranging from a quick comparison between sites to understanding community stability.
         # Their calculation can be very tedious by hand - and very fast with a package designed for the operation.

#Simpson's Diversity Index? Maybe.
plot1<-c(30,34,36)
plot2<-c(20,7,73)
plots<-cbind(plot1,plot2)
simpson<-function(data,type="complement"){
  simpson.diversity<-numeric(ncol(data))
  for(j in 1:ncol(data)){
    soma<-sum(data[,j])
    prop<-data[,j]/soma
    prop2<-prop^2
    D<-sum(prop2)
    if(type=="inverse") (simp<-1/D)
    if(type=="complement") (simp<-1-D)
    simpson.diversity[j]<-simp
  }
  plot.number<-1:ncol(data)
  return(rbind(plot.number,simpson.diversity))
}
summary(plots)
print(> plot1<-c(30,34,36)
      > plot2<-c(20,7,73)
      > plots<-cbind(plot1,plot2)
      > simpson<-function(data,type="complement"){
        +   simpson.diversity<-numeric(ncol(data))
        +   for(j in 1:ncol(data)){
          +     soma<-sum(data[,j])
          +     prop<-data[,j]/soma
          +     prop2<-prop^2
          +     D<-sum(prop2)
          +     if(type=="inverse") (simp<-1/D)
          +     if(type=="complement") (simp<-1-D)
          +     simpson.diversity[j]<-simp
          +   }
        +   plot.number<-1:ncol(data)
        +   return(rbind(plot.number,simpson.diversity))
        + }
      > summary(plots)
      plot1           plot2      
      Min.   :30.00   Min.   : 7.00  
      1st Qu.:32.00   1st Qu.:13.50  
      Median :34.00   Median :20.00  
      Mean   :33.33   Mean   :33.33  
      3rd Qu.:35.00   3rd Qu.:46.50  
      Max.   :36.00   Max.   :73.00)

#Simpson's Diversity Index Manipulation
plot1<-c(70,44,95)
plot2<-c(3,58,89)
plots<-cbind(plot1,plot2)
simpson<-function(data,type="complement"){
  simpson.diversity<-numeric(ncol(data))
  for(j in 1:ncol(data)){
    soma<-sum(data[,j])
    prop<-data[,j]/soma
    prop2<-prop^2
    D<-sum(prop2)
    if(type=="inverse") (simp<-1/D)
    if(type=="complement") (simp<-1-D)
    simpson.diversity[j]<-simp
  }
  plot.number<-1:ncol(data)
  return(rbind(plot.number,simpson.diversity))
}
summary(plots)
print(> plot1<-c(70,44,95)
      > plot2<-c(3,58,89)
      > plots<-cbind(plot1,plot2)
      > simpson<-function(data,type="complement"){
        +   simpson.diversity<-numeric(ncol(data))
        +   for(j in 1:ncol(data)){
          +     soma<-sum(data[,j])
          +     prop<-data[,j]/soma
          +     prop2<-prop^2
          +     D<-sum(prop2)
          +     if(type=="inverse") (simp<-1/D)
          +     if(type=="complement") (simp<-1-D)
          +     simpson.diversity[j]<-simp
          +   }
        +   plot.number<-1:ncol(data)
        +   return(rbind(plot.number,simpson.diversity))
        + }
      > summary(plots)
      plot1           plot2     
      Min.   :44.00   Min.   : 3.0  
      1st Qu.:57.00   1st Qu.:30.5  
      Median :70.00   Median :58.0  
      Mean   :69.67   Mean   :50.0  
      3rd Qu.:82.50   3rd Qu.:73.5  
      Max.   :95.00   Max.   :89.0)