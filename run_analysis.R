#Read Supporting Metadata
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

#Read training data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)


#Read test data
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

#Merge the training and the test sets to create one data set
#We can use combine the respective data in training and test data sets corresponding to subject, activity and features. The results are stored in  subject, activity and features.
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

#Naming the columns
#The columns in the features data set can be named from the metadata in featureNames
colnames(features) <- t(featureNames[2])

#Merge the columns
#The data in features,activity and subject are merged and the complete data is now stored in completeData.
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#Extracts only the measurements on the mean and standard deviation for each measurement
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

#Add activity and subject columns to the list and look at the dimension of completeData
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

#We create extractedData with the selected columns in requiredColumns. And again, we look at the dimension of requiredColumns.
extractedData <- completeData[,requiredColumns]
dim(extractedData)

#Uses descriptive activity names to name the activities in the data set
#The activity field in extractedData is originally of numeric type. We need to change its type to character so that it can accept activity names. The activity names are taken from metadata activityLabels.
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

#We need to factor the activity variable, once the activity names are updated.
extractedData$Activity <- as.factor(extractedData$Activity)

#Appropriately labels the data set with descriptive variable names
names(extractedData)

#By examining extractedData, we can say that the following acronyms can be replaced:

#Acc can be replaced with Accelerometer

#Gyro can be replaced with Gyroscope

#BodyBody can be replaced with Body

#Mag can be replaced with Magnitude

#Character f can be replaced with Frequency

#Character t can be replaced with Time

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

#Here are the names of the variables in extractedData after they are edited
names(extractedData)


#We create tidyData as a data set with average for each activity and subject. Then, we order the enties in tidyData and write it into data file Tidy.txt that contains the processed data.
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
