###DATA GROOMING###

#Set working directory
setwd("C:/GitHub/lawsonle/Final Project")

#Load in first data file
bear1 <- read.csv("YWW Black Bear Data.csv")
#Create data file into data frame
beardf1 <- as.data.frame(bear1)
head(bear1)

#Sum of how many total black bear were seen by date - get rid of unneeded columns
bear_ag1 <- aggregate(bear1$how_many, by=list(bear1$date), FUN = "sum")


#Load in second data file
bear2 <- read.csv("Remote_camera_data.csv")
#Create data file into data frame
beardf2 <- as.data.frame(bear2)
head(bear2)

#Sum of how many total black bear were seen by date - get rid of unneeded columns 
bear_ag2 <- aggregate(bear2$BlackBear, by=list(bear2$Date), FUN ="sum")

#split apart dates so day, month, and year are all separate for first data set
df1<-data.frame(do.call("rbind", strsplit(as.character(bear_ag2$Group.1), "-", fixed = TRUE)))
#combine date parts to original data frame 
df3 <- cbind(bear_ag2,df1)
#split apart dates so day, month, and year are all separate for secind data set
df2<-data.frame(do.call("rbind", strsplit(as.character(bear_ag1$Group.1), "-", fixed = TRUE)))
#combine date parts to original data frame
df4<- cbind(bear_ag1, df2)

#Subset for only month and number of bears seen for each data set
df5 <- subset(df4, select=-c(Group.1,X1,X3))
df6 <- subset(df3, select=-c(Group.1,X1,X3))

#rbind both data sets together to put both sets of date and number of bears seen together in one place
#This will be used to analyze total sightings per month
final<-rbind(df5, df6)

#perform an Anova test to see if month has a statistocally significant effect on number of bear sightings
test.mod <- lm(final$bears~final$month)
anova(test.mod)

#To calculate r-squared, need summary statistics
sum <- lm(final$month~final$bears, data = final) 
#Extracting R-squared parameter from summary 
summary(sum)$r.squared

#change names in final data frame to what they represent (bears & month)
colnames(final)[1]="bears"
colnames(final)[2]="month"


#aggregate the two columns so that each month only has one row of the mean number of bears seen in that month
#This will be used to analyze mean sightings per month
final2 <- aggregate(x=final$bears, by = list(final$month), FUN = "mean", na.rm=TRUE)

#Change columnnames to match what they represent
colnames(final2)[1]="month"
colnames(final2)[2]="bears"

###DATA ANALYSIS###

#boxplot
#a boxplot will allow to see summary data with the minimum, first quartile, median, third quartile, and maximum
#adjusting the ylim will allow to not view outliers and get a closer view

boxplot(bears~month, data=final, main="Total Bears Sightings in Each Month",
        xlab="Month", ylab="Total Number of Bears", ylim = c(0,45))
#Anova test to see significance
anova(lm(final$bears~final$month))



#barplot
#a barplot will allow us to see the mean number of bears per month compared to one another

library(ggplot2)
ggplot(final2, aes(x = month, y = bears)) + 
         geom_bar(stat = "identity") +
        labs(x="Month", y="Mean Number of Bears") +
        labs(main="Mean Bear Sightings per Month") +
        ggtitle ("Mean Bear Sightings per Month")


#connected scatterplot
#a connected scatterplot will allow to view the trend of increasing or decreasing sightings throughout the year

plot(final2$month, final2$bears, type = "b",
     xlab = "Month", ylab = "Mean Number of Bears", main = "Mean Bears Sightings per Month")


