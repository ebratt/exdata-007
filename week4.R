##week 4
rm(list=ls())
# EDA case study with samsung data
# http://www.samsung.com/global/galaxys3/
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
## subject: Coursera getdata-007 final project
## title: "run_analysis.R"
## author: "Eric Bratt"
## date: "Wednesday,September 17,2014"
## output: R script
################################################################################
## clear out the environment
rm(list=ls())

################################################################################
## function that checks to see if a package is installed and,if not,installs it
## portions of this code came from http://stackoverflow.com/questions/9341635/how-can-i-check-for-installed-r-packages-before-running-install-packages
load_package <- function(x) {
  if (x %in% rownames(installed.packages())) { print("package already installed...") }
  else { install.packages(x) }
}

################################################################################
# install necessary packages
load_package("lubridate") # for easy date-handling
load_package("dplyr")     # for data manipulation (ie, joining data frames)

# load necessary libraries
library(lubridate)
library(dplyr)

################################################################################
## UTILITY FUNCTIONS
## function that reads a file and returns a data.frame
read_file <- function(x) {
  result <- read.table(x,header=F,strip.white=T)
  return(result)
}

## function that concatenates strings (useful for directory paths)
concat <- function(x1,x2) {
  result <- paste(x1,x2,sep="")
  return(result)
}

################################################################################
## define some global variables
DATA_DIR          <- "./data"
DATASET_DIR         <- concat(DATA_DIR,"/UCI HAR Dataset")
TEST_DIR            <- concat(DATASET_DIR,"/test")
TRAIN_DIR           <- concat(DATASET_DIR,"/train")
TEST_FILE           <- concat(TEST_DIR,"/X_test.txt")
TEST_LABELS_FILE    <- concat(TEST_DIR,"/Y_test.txt")
TEST_SUBJECTS_FILE  <- concat(TEST_DIR,"/subject_test.txt")
TRAIN_FILE          <- concat(TRAIN_DIR,"/X_train.txt")
TRAIN_LABELS_FILE   <- concat(TRAIN_DIR,"/Y_train.txt")
TRAIN_SUBJECTS_FILE <- concat(TRAIN_DIR,"/subject_train.txt")
FEATURES_FILE       <- concat(DATASET_DIR,"/features.txt")
DATA_FILE           <- concat(DATA_DIR,"/data.zip")
FILE_URL            <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
LABELS_FILE         <- concat(DATASET_DIR,"/activity_labels.txt")
EXTRACT_FILE        <- concat(DATA_DIR,"/tidy_data.txt")

################################################################################
# ensure the local data directory exists
if (!file.exists(DATA_DIR)) { dir.create(DATA_DIR) }
# log the date the archive was downloaded
dateDownloaded <- now()
# download the archive file
if (.Platform$OS.type == "unix") {
  download.file(FILE_URL,destfile = DATA_FILE,method="curl")
} else {
  download.file(FILE_URL,destfile = DATA_FILE)
}
# unzip the archive file to the data directory
unzip(DATA_FILE,exdir = DATA_DIR)

################################################################################
# step 1. Merge the training and the test sets to create one data set.         #
################################################################################
test  <- read_file(TEST_FILE)
train <- read_file(TRAIN_FILE)

# replace activity labels
labels              <- read_file(LABELS_FILE)
names(labels)       <- c("activityid", "activity")
test_labels         <- read_file(TEST_LABELS_FILE)
names(test_labels)  <- c("activityid")
test_labels         <- inner_join(test_labels, labels)
train_labels        <- read_file(TRAIN_LABELS_FILE)
names(train_labels) <- c("activityid")
train_labels        <- inner_join(train_labels, labels)

# add a column to each data table for labels
test$activityid  <- test_labels$activityid
test$activity    <- test_labels$activity
train$activityid <- train_labels$activityid
train$activity   <- train_labels$activity

# add a column to each data table for subjects
test_subjects         <- read_file(TEST_SUBJECTS_FILE)
names(test_subjects)  <- c("subject")
test$subject          <- test_subjects$subject
train_subjects        <- read_file(TRAIN_SUBJECTS_FILE)
names(train_subjects) <- c("subject")
train$subject         <- train_subjects$subject

# combine test and train data
data <- rbind(test,train)

# clean-up the feature names
cols<-read_file(FEATURES_FILE)
cols<-as.vector(cols[,2])                              # convert to a character vector
cols<-gsub("-","",cols)                                # remove -'s
cols<-gsub("^t","Time",cols)                           # replace t with Time
cols<-gsub("^f","Frequency",cols)                      # replace f with Frequency
cols<-gsub("Acc","Acceleration",cols)                  # replace Acc with Acceleration
cols<-gsub("gyro","AngularVelocity",cols)              # replace gyro with AngularVelocity
cols<-gsub("Mag","Magnitude",cols)                     # replace Mag with Magnitude
cols<-gsub("X","Xdirection",cols)                      # replace X with Xdirection
cols<-gsub("Y","Ydirection",cols)                      # replace Y with Ydirection
cols<-gsub("Z","Zdirection",cols)                      # replace Z with Zdirection
cols<-gsub("mean","Mean",cols)                         # replace mean with Mean
cols<-gsub("std","STD",cols)                           # replace std with STD
cols<-gsub("mad","MedianAbsoluteDeviation",cols)       # replace mad with MedianAbsoluteDeviation
cols<-gsub("max","Max",cols)                           # replace max with Max
cols<-gsub("min","Min",cols)                           # replace min with Min
cols<-gsub("sma","SignalMagnitudeArea",cols)           # replace sma with Signal MagnitudeArea
cols<-gsub("energy","Energy",cols)                     # replace energy with Energy
cols<-gsub("iqr","InterQuartileRange",cols)            # replace iqr with InterQuartileRange
cols<-gsub("entropy","Entropy",cols)                   # replace entropy with Entropy
cols<-gsub("arCoeff","AutoRegressionCoefficient",cols) # replace arCoeff with AutoRegressionCoefficient
cols<-gsub("correlation","Correlation",cols)           # replace correlation with Correlation
cols<-gsub("maxInds","MaxMagnitudeIndex",cols)         # replace maxInds with MaxMagnitudeIndex
cols<-gsub("Freq","Frequency",cols)                    # replace Freq with Frequency
cols<-gsub("skewness","Skewness",cols)                 # replace skewness with Skewness
cols<-gsub("kurtosis","Kurtosis",cols)                 # replace kurtosis with Kurtosis
cols<-gsub("bandsEnergy","BandsEnergy",cols)           # replace bandsEnergy with BandsEnergy
cols<-gsub("angle","Angle",cols)                       # replace angle with Angle
cols<-gsub("gravity","Gravity",cols)                   # replace gravity with Gravity
cols<-gsub("\\(","",cols)                              # remove ('s
cols<-gsub("\\)","",cols)                              # remove )'s
cols<-gsub(",","to",cols)                              # remove ,'s

# apply features to data
cols <- c(cols, "activityid", "activity", "subject")
names(data) <- cols

table(data$activity)

# analysis of mean body acceleration
myplclust <- function(hclust, lab=hclust$labels, lab.col = rep(1, length(hclust$labels)),
                      hang = 0.1, ...) {
  ## modification of plclust for plotting hclust objects *in colour*! Copyright
  ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
  ## of labels of the leaves of the tree lab.col: colour for the labels;
  ## NA=default device foreground colour hang: as in hclust & plclust Side
  ## effect: A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height, 2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x < 0)]
  x <- x[which(x < 0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot(hclust, labels=F, hang=hang, ...)
  text(x=x, y=y[hclust$order] - (max(hclust$height) * hang), labels=lab[hclust$order],
       col=lab.col[hclust$order], srt=90, adj = c(1, 0.5), xpd=NA, ...)
}
par(mfrow = c(1,2), mar=c(5,4,1,1))
sub1 <- subset(data, subject == 1)
plot(sub1[, 1], col=sub1$activity, ylab=names(sub1)[1])
legend("bottomright", legend=unique(sub1$activity), col=unique(sub1$activity), pch=1)
plot(sub1[, 2], col=sub1$activity, ylab=names(sub1)[2])
legend("bottomright", legend=unique(sub1$activity), col=unique(sub1$activity), pch=1)
# look at max body acceleration
par(mfrow = c(1,2))
plot(sub1[, 10], col=sub1$activity, ylab=names(sub1)[10], pch=19)
plot(sub1[, 11], col=sub1$activity, ylab=names(sub1)[11], pch=19)
# singular value decomposition
svd1 <- svd(scale(sub1[, -c(562,563,564)]))
par(mfrow=c(1,2))
plot(svd1$u[, 1], col=sub1$activity, pch=19)
plot(svd1$u[, 2], col=sub1$activity, pch=19)
# find maximum contributor
plot(svd1$v[, 2], pch=19)
# clustering with max contributor
par(mfrow=c(1,1))
maxContrib <- which.max(svd1$v[, 2])
distanceMatrix <- dist(sub1[, c(10:12, maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(sub1$activity))
# now there appears to be some clustering
# the various non-moving activities seem to be jumbled, but it seemed to help to separate the moving activities
names(data)[maxContrib]
# try another clustering technique -- k-means (nstart=1, first try)
kClust <- kmeans(sub1[, -c(562,563,564)], centers = 6, nstart=1)
table(kClust$cluster, sub1$activity)
kClust <- kmeans(sub1[, -c(562,563,564)], centers = 6, nstart=100)
table(kClust$cluster, sub1$activity)
kClust <- kmeans(sub1[, -c(562,563,564)], centers = 6, nstart=100)
table(kClust$cluster, sub1$activity)
# cluster 1 seems to be "LAYING"
plot(kClust$center[1, 1:10], pch=19, ylab="Cluster Center", xlab="")
# cluster 6 seems to be "WALKING"
plot(kClust$center[6, 1:10], pch=19, ylab="Cluster Center", xlab="")
# may be interested in understanding what separates the movement activities (walking, walking up, and walking down)
# want to separate them out into their own primary components


