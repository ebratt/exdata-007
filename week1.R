## week 1
# cribbed from Edward Tufte's Beautiful Data
# Principal 1: evidence is always relative -- compared to what?
#              there should always be a control group to act as a basis to compare to
# Principal 2: show causality, mechanism, explanation, etc.
#              include a plot of another comparison explaining causality
# Principal 3: show multivariate data (as much data on a single plot as you can)
#              multivariate = more than 2 variables
#              there may be other relationships in the variables causing confounding
# Principal 4: integrate the evidence
#              don't let the tool drive the analysis
#              completely integrate words, numbers, images, and diagrams
#              data graphics should make use of many different modes
#              measure the strength of the evidence
# Principal 5: describe and document evidence with appropriate labels, scales, sources, etc.
#              more evidence backup makes the analysis more believable - more credibility
# Principal 6: conent is king
#              analytic presentations ultimately stand or fall depending on the quality, relevance, and integrity of the content

## Exploratory Graphs (understand data properties, find patterns, suggest modeling strategies, debug analysis, communicate results)
# they are made quickly, you make a large number of them, goal is for personal understanding, axes/legends are generally cleaned up later, color/size are primarily used for information
# example: air pollution in the US (EPA)
# start with one-dimensional summaries (summary, boxplot & abline, hist & rug & breaks & abline, barplot)
# also look at two-dimensional summaries (Lattice, ggplot2, scatterplots, smooth scatterplots)
# and > 2 dimensions (multiple 2-d plots, coplots, use color, size, shape, spinning plots, actual 3-d plots)

## plotting systems - there are three primary basic plotting systems: base, lattice, and ggplot2
# base: start with blank canvas and build-up from there (start with plot() function and then annotate)
library(datasets)
data(cars)
with(cars, plot(speed, dist))

# lattice: rather than piecing-together, every plot is constructed with a single function call
library(lattice)
# cannot add to the plot, annotation is not intuitive, sometimes awkward to specify entire plot in single function call
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data=state, layout=c(4,1))

# ggplot2: splits the difference between base and lattice in a number of ways
install.packages("ggplot2")
library(ggplot2)
data(mpg)
qplot(displ, hwy, data=mpg)

## Base plotting system in R in detail (most commonly used)
# graphics contains plotting functions
# grDevices contains code implementing the various graphics devices
# lattice and grid are used by lattice
# have to consider several things when making a plot (where will it be made, used? how will it be used? is there a large amount of data? do you need to be able to resize the plot?)
# we will focus on the base plotting system using the screen device in this lecture
# there are two phases: 1) initialize the plot and 2) annotate the plot
library(datasets)
hist(airquality$Ozone) # step 1, initialize the plot
with(airquality, plot(Wind, Ozone))
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab="Month",ylab="Ozone (ppb)")
# key base graphics parameters:
#   pch: plotting symbol (default is open circle)
#   lty: the line type (default is solit line)
#   lwd: the line width (default is 1)
#   col: the plotting color; colors() function gives you all the colors you can use
#   xlab: character string for the x-axis label
#   ylab: character string for the y-axis label
#   par() function:
#     las: orientation of the axis labels on the plot
#     bg: background color
#     mar: margin size
#     oma: outer margin size
#     mfrow: plots per row, column (plots are filled row-wise)
#     mfcol: number of plots per row, column (plots are filled column-wise)
par("lty")
par("col")
par("pch")
par("bg")
par("mar") # bottom, left, top, right
par("mfrow")
# plotting functions:
#   plot: makes a scatterplot
#   lines: adds a line
#   points: adds points
#   text: adds text
#   title: adds title annotation
#   mtext: adds arbitrary text to margins
#   axis: adds axis ticks/labels
with(airquality, plot(Wind, Ozone), type="n")
title(main="Ozone and Wind in New York City")
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch=1, col=c("blue","red"), legend=c("May", "Other Months"))
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd=2)
par(mfrow=c(1,3), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(airquality, {
  plot(Wind, Ozone, main="Ozone and Wind")
  plot(Solar.R, Ozone, main="Ozone and Solar Radiation")
  plot(Temp, Ozone, main="Ozone and Temperature")
})

## demonstrations
x <- rnorm(100)
par(mfrow=c(1,1), mar=c(5,4,4,1), oma=c(1,1,1,1))
hist(x)
y <- rnorm(100)
plot(x,y)
z <- rnorm(100)
plot(x,z)
plot(x, y, pch = 20)
plot(x, y, pch = 19)
plot(x, y, pch = 2)
plot(x, y, pch = 3)
plot(x, y, pch = 4)
plot(x, y, pch = 5)
example(points)
x <- rnorm(100)
y <- rnorm(100)
plot(x, y, pch = 25)
title("Scatterplot")
text(-1, -1, "Label", col="cyan")
legend("topright", legend="Data", pch=20)
fit <- lm(y~x)
abline(fit, lwd=2,col="blue")
z <- rpois(100,2)
par(mfrow=c(2,1))
plot(x, y, pch=20)
plot(x, z, pch=19)
par(mar = c(2,2,1,1))
plot(x, y, pch=20)
plot(x, z, pch=19)
par(mar = c(4,4,2,2))
par(mfrow=c(1,2))
plot(x, y, pch=20)
plot(x, z, pch=19)
par(mfrow=c(2,2))
par(mar = c(4,2,2,2))
plot(x, y)
plot(x, z)
plot(x, z)
plot(y, z)
par(mfrow=c(1,1))
x <- rnorm(100)
y <- x + rnorm(100)
g <- gl(2, 50, labels=c("Male","Female"))
str(g)
plot(x,y)
plot(x, y, type = "n") # creates the plot but does not add the data
points(x[g=="Male"],y[g=="Male"], col="green",pch=2)
points(x[g=="Female"],y[g=="Female"], col="blue",pch=20)

# graphics devices (places where you can make plots appear)
#   window on your computer (quartz() on a mac; windows() on windows; x11() on unix)
#   a PDF file - useful for papers
#   a PNG or JPEG file - useful for website
#   a scalable vector graphics (SVG) file
?Devices
library(datasets)
with(faithful, plot(eruptions, waiting))
title(main="Old Faithful Geyser Data")
pdf(file="myplot.pdf") # open PDF device; create 'myplot.pdf' in my working directory
with(faithful, plot(eruptions, waiting))
title(main="Old Faithful Geyser Data")
dev.off() # closes the grDevice

png(file="myplot.png") # open PDF device; create 'myplot.pdf' in my working directory
with(faithful, plot(eruptions, waiting))
title(main="Old Faithful Geyser Data")
dev.off() # closes the grDevice

jpeg(file="myplot.jpeg") # open PDF device; create 'myplot.pdf' in my working directory
with(faithful, plot(eruptions, waiting))
title(main="Old Faithful Geyser Data")
dev.off() # closes the grDevice

# two broad categories for devices: vector and bitmap-format
#   pdf: useful for line-type graphics, resizes well, usually portable, not good w/lots of points
#   svg: XML-based scalable vector graphics; supports animation and interactivity, potentially useful for web-based plots
#   win.metafile: Windows metafile format (windows only)
#   postscript: older format, also resizes well, usually portable, not used often
#   png: bitmap format, good for line drawings or images with solid colors, use lossless compression (like old GIF), most web browsers can read this format natively, good for plotting many many points, does not resize well
#   jpeg: good for photos; uses lossy, good for plotting many many points, does not resize well, not great for line drawings
#   tiff: creates bitmap files in TIFF format; supports lossless compression
#   bmp: a native Windows bitmap format
bmp(file="myplot.bmp") # open PDF device; create 'myplot.pdf' in my working directory
with(faithful, plot(eruptions, waiting))
title(main="Old Faithful Geyser Data")
dev.off() # closes the grDevice

# mutilple open grDevices
# can only plot to one device at a time; figure out the active one by calling dev.cur()
dev.cur() # 2 means screen
# use dev.set(<integer>) to switch to the grDevice you want
# you can copy plots from one device to another using dev.copy() or dev.copy2pdf()
# ***DON'T FORGET TO CLOSE THE DEVICE***
