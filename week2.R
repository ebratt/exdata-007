## week 2 - Lattice plotting system
# lattice: contains code for producing Trellis graphics, which are independent from "base" graphics; includes functions like xyplot, bwplot, levelplot
# grid: implements a different graphing system independent of "base" system; lattice builds on top of grid
# no two-phase process; all done at once
#   xyplot: main function for scatterplots
#   bwplot: box-and-whiskers plot ("boxplots")
#   histogram
#   stripplot: like boxplot but with actual points
#   dotplot: plot dots on "violin strings"
#   splom: catterplot matrix; like "pairs" in base system
#   levelplot, contourplot: for plotting "image" data

# xyplot(y ~ x | f * g, data)
#   y is y-axis variable
#   x is x-axis variable
#   f and g a re contitioning variables (optional)
#   * indicates interaction
library(lattice)
library(datasets)
xyplot(Ozone ~ Wind, data=airquality)
airquality <- transform(airquality, Month=factor(Month))
xyplot(Ozone ~ Wind | Month, data=airquality, layout=c(5,1)) # groups by Month factor

# lattice functions behave differently from base functions
#    base functions plot data directly to the graphics device
#    lattice functions return an object of class trellis
#    then the print method plots the trellis data
#    lattice functions return "plot objects" that can be stored (not typical)
p <- xyplot(Ozone ~ Wind, data=airquality)
print(p)
xyplot(Ozone ~ Wind, data=airquality) #uses "auto-printing" feature in R

# lattice has "panel" functions which controls what happens inside each panel of the plot
#    "panel" functions receive the x/y coordinates fo the data points in their panel
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each=50)
y <- x + f - f * x + rnorm(100, sd=0.5)
f <- factor(f, labels=c("Group 1", "Group 2"))
xyplot(y ~ x | f, layout=c(2,1)) # plot with 2 panels
# you can create a custom function to control the content of the panels
xyplot(y ~ x | f, panel = function (x,y,...) {
  panel.xyplot(x, y, ...) # first call default panel function
  panel.lmline(x, y, col=2) # overlay a simple linear regression line
  panel.axis(labels=F)
})
# cannot use any of the base functions to annotate lattice plots -- have to use lattice functions
# lattice plots are created using a single function call to a core lattice function
# margins are automatically handled and defaults are usually sufficient
# ideal for creating conditioning plots where you examine same kind of plot under different conditions
# panel functions can be specified/customized to modify what is plotted in each panel

## ggplot2 package
# implements Grammer of Graphics by Leland Wilkinson
# written by Handley Wickham while he was a grad student at Iowa State
# "third" graphics system for R
# http://ggplot2.org
# built upon the grid graphics system
# grammer of graphics represents an abstraction of graphics ideas/objects
# think "verb", "noun", "adjective" for graphics
# allows for a "theory" of graphics on which to build new graphics and graphic objects
# the grammer tells us that a statistical graphic is a mapping from data to aesthetic
#   attributes of geometric objects
install.packages("ggplot2")
library(ggplot2)
# qplot() function works like plot() in base system
#   looks for data in a data frame, similar to lattice
#   plots are made up of aesthetics (size, shape, color) and geoms (points, lines)
#   factors are important for indicating subsets of the data and they should be labeled
#   qplot() hides whats going on underneath
#   ggplot() is the core function and very flexible for doing things that qplot() cannot do
df <- data.frame(gp = factor(rep(letters[1:3], each = 10)),
                 y = rnorm(30))
# Compute sample mean and standard deviation in each group
library(plyr)
ds <- ddply(df, .(gp), summarise, mean = mean(y), sd = sd(y))
# Declare the data frame and common aesthetics.
# The summary data frame ds is used to plot
# larger red points in a second geom_point() layer.
# If the data = argument is not specified, it uses the
# declared data frame from ggplot(); ditto for the aesthetics.
ggplot(df, aes(x = gp, y = y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean),
             colour = 'red', size = 3)
str(mpg)
qplot(displ, hwy, data=mpg, color=drv) # subset the drv variable and map it to color
qplot(displ, hwy, data=mpg, color=drv, geom=c("point", "smooth")) # add a geom to represent confidence intervals 95%
# histogram filing colors based on drv condition
qplot(hwy, data=mpg, fill=drv)
# you can make panel plots using facets
qplot(displ,hwy,data=mpg,facets=.~drv)
qplot(hwy,data=mpg,facets=drv~.) # the .~ handles the layout (left-hand side of the ~ controls the columns; right-hand controls the rows); "." means nothing
# qplot is a simple function but with many built-in features
# syntax is somewhere between base and lattice
# produces very nice graphics that are publication-ready
# difficult to go against the grain/customize; use full ggplot2 power if you want to customize
# components: data frame, aesthetic mappings, geoms, facets, stats, scales, coordinate system
# plots are built-up in layers:
#   plot the data
#   overlay a summary
#   metadata and annotation
# examples:
df <- data.frame(gp = factor(rep(letters[1:3], each = 10)),
                 y = rnorm(30))
# Compute sample mean and standard deviation in each group
library(plyr)
ds <- ddply(df, .(gp), summarise, mean = mean(y), sd = sd(y))

# Declare the data frame and common aesthetics.
# The summary data frame ds is used to plot
# larger red points in a second geom_point() layer.
# If the data = argument is not specified, it uses the
# declared data frame from ggplot(); ditto for the aesthetics.
ggplot(df, aes(x = gp, y = y)) +
  geom_point() +
  geom_point(data = ds, aes(y = mean),
             colour = 'red', size = 3)
# Same plot as above, declaring only the data frame in ggplot().
# Note how the x and y aesthetics must now be declared in
# each geom_point() layer.
ggplot(df) +
  geom_point(aes(x = gp, y = y)) +
  geom_point(data = ds, aes(x = gp, y = mean),
             colour = 'red', size = 3)
# Set up a skeleton ggplot object and add layers:
ggplot() +
  geom_point(data = df, aes(x = gp, y = y)) +
  geom_point(data = ds, aes(x = gp, y = mean),
             colour = 'red', size = 3) +
  geom_errorbar(data = ds, aes(x = gp, y = mean,
                               ymin = mean - sd, ymax = mean + sd),
                colour = 'red', width = 0.4)
# annotation
#   labels: xlab(), ylab(), ggtitle()
#   each "geom" function has options to modify
#   check out theme()
#   two standard appearance themes are included: theme_gray() and theme_bw()
# axis limits are different using ggplot2 versus base system
testdat <- data.frame(x=1:100,y=rnorm(100))
testdat[50,2] <- 100
plot(testdat$x, testdat$y, type="l",ylim=c(-3,3))
g <- ggplot(testdat,aes(x=x,y=y))
g + geom_line() + coord_cartesian(ylim=c(-3,3))
# ggplot will remove outliers if you are not careful!
# use cut() function to make continuous variables categorical
# ggplot2 is very powerful and flexible if you learn the "grammar"

library(nlme)
library(lattice)
xyplot(weight ~ Time | Diet, BodyWeight)
library(datasets)
data(airquality)
xyplot(Ozone ~ Wind | factor(Month), data=airquality)
qplot(Wind, Ozone, data=airquality, geom="smooth")
qplot(Wind, Ozone, data=airquality, facets=.~factor(Month))
airquality <- transform(airquality, Month=factor(Month))
qplot(Wind, Ozone, data=airquality, facets=.~Month)
g <- ggplot(movies, aes(votes, rating))
print(g)
qplot(votes, rating, data=movies) + stats_smooth("loess")
