#Set working directory
setwd("C:/GitHub/lawsonle/Final Project")

#Load in first data file
bear1 <- read.csv("YWW Black Bear Data.csv")
#Create data file into data frame
beardf1 <- as.data.frame(bear1)
head(bear1)

?aggregate
bear_ag1 <- aggregate(bear1$how_many, by=list(bear1$date), FUN = "sum")


#Load in second data file
bear2 <- read.csv("Remote_camera_data.csv")
#Create data file into data frame
beardf2 <- as.data.frame(bear2)
head(bear2)

bear_ag2 <- aggregate(bear2$BlackBear, by=list(bear2$Date), FUN ="sum")

