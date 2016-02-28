library(data.table)
library(dplyr)

#  Supporting Metadata: 
#  1) activityLabels and 2) featureNames.
featureNames   <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

# ************************************************************************
#  PART 1. Merges the training and the test sets to create one data set.
# ************************************************************************

#  Reading Training data
subjectTrain  <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

#  Reading Test data
subjectTest  <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

subjects   <- rbind(subjectTrain, subjectTest)
activities <- rbind(activityTrain, activityTest)
features   <- rbind(featuresTrain, featuresTest)
colnames(features) <- t(featureNames[2])

colnames(activities) <- "Activity"
colnames(subjects) <- "Subject"
completeData <- cbind(features,activities,subjects)


# ************************************************************************
#  PART 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
# ************************************************************************

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
extractedData <- completeData[,requiredColumns]

# ************************************************************************
#  PART 3 - Uses descriptive activity names to name the activities in the data set.
# ************************************************************************

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
        extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)


# ************************************************************************
#  PART 4 - Appropriately labels the data set with descriptive variable names.
# ************************************************************************

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "Std", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
names(extractedData)<-gsub("angle", "Angle", names(extractedData))


# ************************************************************************
#  PART 5 - From the data set in step 4, creates a second, independent tidy data set 
#  with the average of each variable for each activity and each subject.
# ************************************************************************

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
