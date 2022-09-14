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
uniqu.num <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
rep.num <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5)
dec.num <- c(1.1,2.2,3.3,4.4,5.5,6.6,7.7,8.8,9.9,10.1,11.1,12.1,13.1,14.1,15.1)
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
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Factor, ylim = c(0,5))
arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)
title("barplot")
title(ylab="ylabel")
title(xlab="xlabel")

setwd("C:/GitHub/lawsonle")



#scatterplot 
plot(df$dec.num ~ df$rep.num)
plot(df$dec.num ~ df$uniqu.num)
plot(df$dec.num ~ df$uniqu.num, xlab = "colors", ylab = "shapes")
?pch
plot(df$dec.num ~ df$uniqu.num, xlab = "Colors", ylab = "Shapes", main = "R You Kidding Me", 
     cex.axis=1.0, cex.main = 1.0, cex.lab = 1.0, pch=8, col="red") # I approve of the cheap R pun

#everything is spot-on, except corrupt PDF file.

