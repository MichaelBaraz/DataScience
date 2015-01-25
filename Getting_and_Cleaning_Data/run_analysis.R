# Purpose of this project is to demonstrate your ability to collect, work with, 
# and clean a data set. The goal is to prepare tidy data that can be used for later 
# analysis. You will be graded by your peers on a series of yes/no questions 
# related to the project. You will be required to submit: 1) a tidy data set as 
# described below, 2) a link to a Github repository with your script for performing 
# the analysis, and 3) a code book that describes the variables, the data, and any 
# transformations or work that you performed to clean up the data called CodeBook.md. 
# You should also include a README.md in the repo with your scripts. This repo 
# explains how all of the scripts work and how they are connected.
# 
# One of the most exciting areas in all of data science right now is wearable 
# computing - see for example this article . Companies like Fitbit, Nike, and 
# Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
# The data linked to from the course website represent data collected from the 
# accelerometers from the Samsung Galaxy S smartphone. A full description is 
# available at the site where the data was obtained:
# 
#  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Here are the data for the project:
# 
#  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following.
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the 
#    average of each variable for each activity and each subject.


## 1a) read in and merge test and train subjects data
subject_train <- read.table("train/subject_train.txt")
subject_test <- read.table("test/subject_test.txt")
# Merge datasets
subject <- rbind(subject_train, subject_test)
# set colname to "subject"
colnames(subject) <- "subject"


## 1b) read in and merge test and train activities data
y_train <- read.table("train/y_train.txt")
y_test <- read.table("test/y_test.txt")
# Merge datasets
y <- rbind(y_train, y_test)
# Get activity labels
activity_labels <- read.table("activity_labels.txt")
# Decode activity labels
activity <- merge(y, activity_labels, by=1)[,2]


## 1c) Read in and merge test and train features data
### Read in and train features data
X_test <- read.table("test/X_test.txt")
X_train <- read.table("train/X_train.txt")
### Merge datasets
X <- rbind(X_train, X_test)
### Get features column names
features <- read.table("features.txt",sep="") )
### set colnames by features
colnames(X) <- features[, 2]


## 1d). Combine datasets
data <- cbind(subject, activity, X)


## 2) Extract only the measurements on the mean and standard deviation for each measurement
# Contains: -mean()-X; -mean()-Y; -mean()-Z; -mean(); -std()-X; -std()-Y; -std()-Z; -std()
search <- grep("-mean\\(\\)|-std\\(\\)", colnames(data))
extracted_data <- data[,c(1,2,search)]

## 3) Compute the means, grouped by activity/subject
#install.packages("reshape")
#library(reshape)
melted = melt(extracted_data, id.var = c("activity", "subject"))

#install.packages("reshape2")
#library(reshape2)
# Applying mean function to the melted dataset
result = dcast(melted, activity + subject ~ variable, mean)


## 5) Writing the result to the file
write.table(result, file="average_of_each_variable_for_each_activity_and_subject.txt", sep=";", row.names=FALSE)
