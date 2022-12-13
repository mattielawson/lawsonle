##DATA GROOMING
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
final<-rbind(df5, df6)

#change names in final data frame to what they represent (bears & month)
colnames(final)[1]="bears"
colnames(final)[2]="month"

?aggregate
final2 <- aggregate(x=final$bears, by = list(final$month), FUN = "mean")

#DATA ANALYSIS

#boxplot
boxplot(bears~month, data=final, main="Bears Seen in Each Month",ylim=c(0,100))

#barplot
library(ggplot2)
ggplot(final, aes(x = month, y = bears)) + 
         geom_bar(stat = "identity")

#histogram
hist(final$bears~final$month)


