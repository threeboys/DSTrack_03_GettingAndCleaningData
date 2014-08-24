########################################################################
## File Name   : run_analysis.R  (for Mac only)
## Writer      : Dongwoo Lee (leewow9@naver.com)
## Last Update : 2014.08.25
##
## Data Description : Human Activity Recognition Using Smartphones Data Set 
##  - Human Activity Recognition database built from the recordings of 30 subjects performing activities of daily living (ADL) 
##    while carrying a waist-mounted smartphone with embedded inertial sensors.
##  - url : http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
##
## Program Desciption   
##  - This program file does does the following. 
##     1. Merges the training and the test sets to create one data set.
##     2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##     3. Uses descriptive activity names to name the activities in the data set
##     4. Appropriately labels the data set with descriptive variable names. 
##     5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
##
########################################################################

## 0. Getting & Setting Data

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file (fileUrl, destfile = "./GetData_ProjectFiles.zip", method="curl")
system("unzip GetData_ProjectFiles.zip")
system("ls -al")
system("mv 'UCI HAR Dataset2' UCI_HAR_Dataset")
setwd("UCI_HAR_Dataset")
system("ls -al")

###############################################################
## 1. Merges the training and the test sets to create one data set.

# merge test Data

testDataSet1 <- read.table("./test/subject_test.txt", header = FALSE)
testDataSet2 <- read.table("./test/X_test.txt", header = FALSE)
testDataSet3 <- read.table("./test/y_test.txt", header = FALSE)

testDataSet <- cbind('test', testDataSet1, testDataSet3, testDataSet2)
tempNames <- names(testDataSet)
tempNames[1] <- 'type'
tempNames[2] <- 'subject'
tempNames[3] <- 'activity'
names(testDataSet) <- tempNames

rm(testDataSet1, testDataSet2, testDataSet3, tempNames)

# merge train Data

trainDataSet1 <- read.table("./train/subject_train.txt", header = FALSE)
trainDataSet2 <- read.table("./train/X_train.txt", header = FALSE)
trainDataSet3 <- read.table("./train/y_train.txt", header = FALSE)

trainDataSet <- cbind('train', trainDataSet1, trainDataSet3, trainDataSet2)
tempNames <- names(trainDataSet)
tempNames[1] <- 'type'
tempNames[2] <- 'subject'
tempNames[3] <- 'activity'
names(trainDataSet) <- tempNames

rm(trainDataSet1, trainDataSet2, trainDataSet3, tempNames)

# merge train data & test data

dataset <- rbind(trainDataSet, testDataSet)

rm(trainDataSet, testDataSet)

# Result 1
dataset

###############################################################
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("./features.txt", header=FALSE)
meanSDMeasurementsIndex <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
meanSDMeasurementsName <- features[meanSDMeasurementsIndex, 2]

measureDataset <- dataset[, c(1:3, meanSDMeasurementsIndex + 3)]

rm (features, meanSDMeasurementsIndex)

# Result 2
measureDataset

###############################################################
##  3. Uses descriptive activity names to name the activities in the data set

measureDataset$activity[which(measureDataset$activity == 1)] <- "WALKING"
measureDataset$activity[which(measureDataset$activity == 2)] <- "WALKING_UPSTAIRS"
measureDataset$activity[which(measureDataset$activity == 3)] <- "WALKING_DOWNSTAIRS"
measureDataset$activity[which(measureDataset$activity == 4)] <- "SITTING"
measureDataset$activity[which(measureDataset$activity == 5)] <- "STANDING"
measureDataset$activity[which(measureDataset$activity == 6)] <- "LAYING"

# Result 3
measureDataset$activity

###############################################################
## 4. Appropriately labels the data set with descriptive variable names. 

modifyFeaturesName <- gsub("\\(|\\)", "", meanSDMeasurementsName)
modifyFeaturesName <- gsub(",", "-", modifyFeaturesName)

tempNames <- names(measureDataset)
tempNames <- c(tempNames[1:3], modifyFeaturesName)
names(measureDataset) <- tempNames

rm(modifyFeaturesName, tempNames)

# Result 4
measureDataset
write.table(dataset, "merge_and_tidy_dataset.txt", row.name=FALSE)

###############################################################
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

library(reshape2)

dsMelt <- melt(measureDataset, id=c('type', 'subject', 'activity'), measure.vars= c(4:69))

meanSDsByActivityAndSubject <- ddply(dsMelt, .(type, subject, activity, variable), summarize, 
                                     mean = round(mean(value), 4))

newTidyDataset <- dcast(meanSDsByActivityAndSubject, type+subject+activity ~ variable,mean)

# Result 5
head(newTidyDataset)
write.table(newTidyDataset, "tidy_dataset_with_the_averages.txt", row.name=FALSE)

################### End of Document ################
