library(plyr)

# Downloading dataset
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

# Unzip dataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")


# 1. Merging the training and the test sets to create one data set:

# 1.1 Reading files

# 1.1.1 Reading trainings tables:
a_train <- read.table("./data/UCI HAR Dataset/train/x_train.txt")
b_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
c_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")

# 1.1.2 Reading testing tables:
a_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
b_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
c_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

# 1.1.3 Reading feature vector:
features <- read.table('./data/UCI HAR Dataset/features.txt')

# 1.1.4 Reading activity labels:
activityLabels = read.table('./data/UCI HAR Dataset/activity_labels.txt')

# 1.2 Assigning column names:
colnames(a_train) <- features[,2] 
colnames(b_train) <-"activityId"
colnames(c_train) <- "subjectId"

colnames(a_test) <- features[,2] 
colnames(b_test) <- "activityId"
colnames(c_test) <- "subjectId"

colnames(activityLabels) <- c('activityId','activityType')

# 1.3 Merging all data in one set:
merge_train <- cbind(b_train, c_train, a_train)
merge_test <- cbind(b_test, c_test, a_test)
AllInOne <- rbind(merge_train, merge_test)

# 2. Extracting only the measurements on the mean and standard deviation for each measurement

# 2.1 Reading column names:
colNames <- colnames(AllInOne)

# 2.2 Create vector for defining ID, mean and standard deviation:
mean_and_std <- (grepl("activityId" , colNames) | 
                     grepl("subjectId" , colNames) | 
                     grepl("mean.." , colNames) | 
                     grepl("std.." , colNames) 
)

# 2.3 Making nessesary subset from AllInOne:
ForMeanAndStd <- AllInOne[ , mean_and_std == TRUE]

# 3. Using descriptive activity names to name the activities in the data set:
WithActivityNames <- merge(ForMeanAndStd, activityLabels,
                              by='activityId',
                              all.x=TRUE)

# 4. Appropriately labeling the data set with descriptive variable names.
# Completed in steps 1.3, 2.2, 2.3

# 5. Creating a tidy data set with the average of each variable for each activity and each subject:

# 5.1 Making alt tidy data set 
TidySet <- aggregate(. ~subjectId + activityId, WithActivityNames, mean)
TidySet <- TidySet[order(TidySet$subjectId, TidySet$activityId),]

# 5.2 Writing  tidy data set in txt file
write.table(TidySet, "Tidy.txt", row.name=FALSE)
