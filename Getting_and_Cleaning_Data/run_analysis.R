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
# Implementation (per documentation in codebook.md):
# Step 1 - Read and combine subjects data ("train/subject_train.txt"; "test/subject_test.txt").
# Step 2 - Read and combine actvity data ("train/y_train.txt"; "test/y_test.txt") and combined rows decode by activity labels ("activity_labels.txt").
# Step 3 - Read and combine features data ("test/X_test.txt"; "train/X_train.txt") and set column names for subjects ("features.txt").
# Step 4 - Combine the datasets.
# Step 5 - Extract from dataset only the measured columns, e.g. "-mean()" and "-std()".
# Step 6 - Compute the means and group by activity and subject.
# Step 7 - Writing the result to semicolon (";") separated flat (text) file.

# Step 1 - Read and combine test and train subjects data
subject_train <- read.table("train/subject_train.txt")
subject_test <- read.table("test/subject_test.txt")
# Merge datasets
subject <- rbind(subject_train, subject_test)
# set colname to "subject"
colnames(subject) <- "subject"


# Step 2 - Read and combine train and test data and combined rows decode by activity labels
# read activity data
y_train <- read.table("train/y_train.txt")
y_test <- read.table("test/y_test.txt")
# Merge datasets
y <- rbind(y_train, y_test)
# Get activity labels
activity_labels <- read.table("activity_labels.txt")
# Decode activity labels
activity <- merge(y, activity_labels, by=1)[,2]


# Step 3 - Read and combine features data and set column names.
# Read in and train features data
X_test <- read.table("test/X_test.txt")
X_train <- read.table("train/X_train.txt")
### Merge datasets
X <- rbind(X_train, X_test)
### Get features column names
features <- read.table("features.txt",sep="") )
### set colnames by features
colnames(X) <- features[, 2]


# Step 4 - Combine datasets
data <- cbind(subject, activity, X)


## step 5 - Extract from dataset only the measured columns, e.g. "-mean()" and "-std()"
# Contains: -mean()-X; -mean()-Y; -mean()-Z; -mean(); -std()-X; -std()-Y; -std()-Z; -std()
search <- grep("-mean\\(\\)|-std\\(\\)", colnames(data))
extracted_data <- data[,c(1,2,search)]


# Step 6 - Compute the means and group by activity and subject.
#install.packages("reshape")
#library(reshape)
melted = melt(extracted_data, id.var = c("activity", "subject"))

#install.packages("reshape2")
#library(reshape2)
# Applying mean function to the melted dataset
result = dcast(melted, activity + subject ~ variable, mean)


# Step 7 - Writing the result to semicolon (";") separated flat (text) file.
write.table(result, file="average_of_each_variable_for_each_activity_and_subject.txt", sep=";", row.names=FALSE)
