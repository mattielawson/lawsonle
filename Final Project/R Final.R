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

###Sum of how many total black bear were seen by date - get rid of unneeded columns 
bear_ag2 <- aggregate(bear2$BlackBear, by=list(bear2$Date), FUN ="sum")

###Change date format in data set to match with other
bear_ag2$Date <- format(as.POSIXct(bear_ag2$Group.1,format='%d-%m-%Y'),format='%m/%d/%Y')

bear_ag2$Date <- format(as.Date(bear_ag2$Group.1,format='%d-%m-%Y'),format='%m/%d/%Y')
?as.Date
as.Date(as.character(bear_ag2$Group.1),format='%d/%m/%Y')


data.frame(do.call("rbind", strsplit(as.character(bear_ag2$Group.1), "-", fixed = TRUE)))







