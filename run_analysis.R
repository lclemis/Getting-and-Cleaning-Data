## Load the needed packages
library(dplyr)
library(tidyr)
library(data.table)

## Get Data 

# Download from website and save in proper directory
path <- "/Users/Elizabeth/Desktop/Coursera/R/getting-and-cleaning-data"
setwd("path")
if(!file.exists("./data")){dir.create("./data")}
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./data/HARDataset.zip", method = "curl")
unzip(zipfile="./data/HARDataset.zip", exdir="./data")
path <- "/Users/Elizabeth/Desktop/Coursera/R/getting-and-cleaning-data/data/UCI HAR Dataset"

# Read the files and create the "test" tables
testSubject <- tbl_df(read.table(file.path(path, "test", "subject_test.txt")))
testActivity <- tbl_df(read.table(file.path(path, "test", "Y_test.txt")))
testData <- tbl_df(read.table(file.path(path, "test", "X_test.txt")))

# Read the files and create the "train" tables
trainSubject <- tbl_df(read.table(file.path(path, "train", "subject_train.txt")))
trainActivity <- tbl_df(read.table(file.path(path, "train", "Y_train.txt")))
trainData <- tbl_df(read.table(file.path(path, "train", "X_train.txt")))

# Read names of column variables and create table
features <- tbl_df(read.table(file.path(path, "features.txt")))

# Read the links the class labels with their activity name and create table.
activity <- tbl_df(read.table(file.path(path, "activity_labels.txt")))


## 1. Merges the training and the test sets to create one data set.

# Merge the training and test sets of the "subject" tables
mergeSubject <- rbind(testSubject, trainSubject)
# Merge the training and test sets of the "activity" tables
mergeActivity <- rbind(testActivity, trainActivity)
# Merge the training and test sets of the "data" tables
mergeData <- rbind(testData, trainData)
# Name the columns in the merged datasets
setnames(mergeSubject, "V1", "Subject")
setnames(mergeActivity, "V1", "Activity_Number")
setnames(features, names(features), c("Feature_Number", "Feature_Name"))
setnames(activity, names(activity), c("Activity_Number", "Activity_Name"))
colnames(mergeData) <- features$Feature_Name

# Merge all table columns
mergeSubjectActivity <- cbind(mergeSubject, mergeActivity)
dataTable <- cbind(mergeSubjectActivity, mergeData)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
featuresMeanStd <- grep("mean\\(\\)|std\\(\\)",features$Feature_Name,value=TRUE)
featuresMeanStd <- union(c("Subject","Activity_Number"), featuresMeanStd)
dataTable<- subset(dataTable,select=featuresMeanStd) 

## 3.Uses descriptive activity names to name the activities in the data set
# Naming was done above in step 1.


## 4. Appropriately labels the data set with descriptive variable names.
dataTable <- merge(activity, dataTable , by="Activity_Number", all.x=TRUE)
dataTable$Activity_Name <- as.character(dataTable$Activity_Name)
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))


## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
dataSort<- aggregate(. ~ Subject - Activity_Name, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataSort,Subject,Activity_Name))
write.table(dataTable, "TidyData.txt", row.name=FALSE)
