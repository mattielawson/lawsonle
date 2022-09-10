# With the data frame you created last week you will:

# Create a barplot for one numeric column, grouped by the character vector with 3 unique values
  # Add error bars with mean and standard deviation to the plot
  # Change the x and y labels and add a title
  # Export the plot as a PDF that is 4 inches wide and 7 inches tall.

# Create a scatter plot between two of your numeric columns.
  # Change the point shape and color to something NOT used in the example.
  # Change the x and y labels and add a title
  # Export the plot as a JPEG by using the "Export" button in the plotting pane.

# Upload both plots with the script used to create them to GitHub.
  # Follow the same file naming format as last week for the script.
  # Name plots as Lastname_barplot or Lastname_scatterplot. Save them to your "plots" folder.

#barplot
unique.char <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')
group.char <- c('a','b','d','d','d','d','d','d','d','d','d','d','d','d','d')
uniqu.num <- c(0,10,10,20,30,50,80,13,23,34,66,89,156,247,457)
rep.num <- c(10,20,30,10,20,30,10,20,30,50,20,30,40,10,60)
dec.num <- c(11,2.1,3.1,1.2,2.3,3.5,1.8,2.13,3.21,5.34,2.55,3.89,4.155,0.243,5.377)
df <- as.data.frame(cbind(unique.char,group.char,uniqu.num,rep.num,dec.num))
df$uniqu.num <- as.numeric(as.character(df$uniqu.num))
df$rep.num <- as.numeric(as.character(df$rep.num))
df$dec.num <- as.numeric(as.character(df$dec.num))

df.mean <- aggregate(df$rep.num ~df$group.char, FUN = "mean")
df.mean
colnames(df.mean) <- c("Factor","Mean")
df.mean
barplot(df.mean$Mean)
df.sd <- aggregate(df$rep.num ~df$group.char, FUN = "sd")
colnames(df.sd) <- c("Factor","StanDev")
df.sd
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor)
arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)
setwd("C:/GitHub/lawsonle")
pdf( file = "MyBarplot.pdf", width = 4, height = 7)
par(family = "serif")
dev.off()



#scatterplot 
plot(df$dec.num ~ df$rep.num)
plot(df$dec.num ~ df$uniqu.num)
plot(df$dec.num ~ df$uniqu.num, xlab = "colors", ylab = "shapes")
?pch
plot(df$dec.num ~ df$uniqu.num, xlab = "Colors", ylab = "Shapes", main = "R You Kidding Me", 
     cex.axis=1.0, cex.main = 1.0, cex.lab = 1.0, pch=8, col="red")

