## Purpose: collect, work with, and clean a data set.
## Data for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## Task: create a R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set.
## 4. Appropriately labels the data set with descriptive variable names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Set Working Directory
> setwd("C:/Users/cecilia/Desktop/Coursera - Analytics/Johns Hopkins University/3-Cleaning Data/UCI HAR Dataset")

# 1. Merge the training and the test data

# 1.1. Read the general and training data
features        <- read.table("./features.txt",header=FALSE)
activityLabel   <- read.table("./activity_labels.txt",header=FALSE)
subjectTrain    <-read.table("./train/subject_train.txt", header=FALSE)
xTrain          <- read.table("./train/X_train.txt", header=FALSE)
yTrain          <- read.table("./train/y_train.txt", header=FALSE)

# 1.2. Assign column names to the general and training data
colnames(activityLabel)<-c("activityId","activityType")
colnames(subjectTrain) <- "subId"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityId"

# 1.3. Merge training data
trainData <- cbind(yTrain,subjectTrain,xTrain)

# 1.4. Read the test data
subjectTest    <-read.table("./test/subject_test.txt", header=FALSE)
xTest         <- read.table("./test/X_test.txt", header=FALSE)
yTest         <- read.table("./test/y_test.txt", header=FALSE)

# 1.5. Assign column names to the test data
colnames(subjectTest) <- "subId"
colnames(xTest) <- features[,2]
colnames(yTest) <- "activityId"

# 1.6. Merge test data
testData <- cbind(yTest,subjectTest,xTest)

# 1.7. Merge the training and the test sets to create one data set.
finalData <- rbind(trainData,testData)

# 2. Measurement on the mean and standard deviation for each measurement
data_mean_std <-finalData[,grepl("mean|std|subject|activityId",colnames(finalData))]

# 3. Descriptive activity names to name the activities in the data set.
library(plyr)
data_mean_std <- join(data_mean_std, activityLabel, by = "activityId", match = "first")
data_mean_std <-data_mean_std[,-1]

4. Label the data set with descriptive variable names.

# 4.1. Remove parentheses
names(data_mean_std) <- gsub("\\(|\\)", "", names(data_mean_std), perl  = TRUE)

# 4.1. Add descriptive labels

names(data_mean_std) <- gsub("Acc", "Accelerometer", names(data_mean_std))
names(data_mean_std) <- gsub("Gyro", "Gyroscope", names(data_mean_std))
names(data_mean_std) <- gsub("^t", "Time", names(data_mean_std))
names(data_mean_std) <- gsub("^f", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("BodyBody", "Body", names(data_mean_std))
names(data_mean_std) <- gsub("mean", "Mean", names(data_mean_std))
names(data_mean_std) <- gsub("std", "Std", names(data_mean_std))
names(data_mean_std) <- gsub("Freq", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("Mag", "Magnitude", names(data_mean_std))

# 5. Second, independent tidy data set with the average of each variable for each activity and each subject.
FinalTidyData <- data_mean_std %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean))
write.table(FinalTidyData,file="tidydata.txt")


