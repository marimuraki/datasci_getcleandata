# Steps:
#   Merge the training and the test sets to create one data set.
#   Extract only the measurements on the mean and standard deviation for each measurement. 
#   Use descriptive activity names to name the activities in the data set
#   Appropriately label the data set with descriptive variable names. 
#   Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

setwd("~/Dropbox/Mari/courses/Coursera/DS Track/Getting & Cleaning Data/")

packages <- c("data.table", "reshape2")
sapply(packages, require, character.only = TRUE, quietly = TRUE)

# Download file

path  <- getwd()
url   <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f     <- "Dataset.zip"

download.file(url, file.path(path, f))

# Manually unzipped file opens in "UCI HAR Dataset" folder

pathdata <- file.path(path, "UCI HAR Dataset")

# Read files

dtTrain <- data.table(read.table(file.path(pathdata, "train", "X_train.txt")))
dtTest  <- data.table(read.table(file.path(pathdata, "test", "X_test.txt")))

dtSubjectTrain  <- fread(file.path(pathdata, "train", "subject_train.txt"))
dtSubjectTest   <- fread(file.path(pathdata, "test", "subject_test.txt"))

dtActivityTrain <- fread(file.path(pathdata, "train", "Y_train.txt"))
dtActivityTest  <- fread(file.path(pathdata, "test", "Y_test.txt"))

dtActivityNames <- fread(file.path(pathdata, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

dtFeatures      <- fread(file.path(pathdata, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

# Merge training & test sets

# Combine rows

dtSubject   <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")

dtActivity  <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")

dt <- rbind(dtTrain, dtTest)

# Combine columns

dtSubjectActivity <- cbind(dtSubject, dtActivity)
dt                <- cbind(dtSubjectActivity, dt)

# Extract only mean (mean) and standard deviation (std) for each measurement
# e.g., tBodyAcc-mean()-X, tBodyAcc-std()-X

dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]

# Recode featureNum to be consistent with featureCode
# e.g., "1" to "V1", "2" to "V2", ...

dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]

# Subset data for only features in dtFeatures

setkey(dt, subject, activityNum)
select  <- c(key(dt), dtFeatures$featureCode)
dt      <- dt[, select, with = FALSE]

# Reshape data

setkey(dt, subject, activityNum)
dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))

# Merge in data labels

dt <- merge(dt, 
            dtActivityNames, 
            by = "activityNum", 
            all.x = TRUE)
dt <- merge(dt, 
            dtFeatures[, list(featureNum, featureCode, featureName)], 
            by = "featureCode", 
            all.x = TRUE)

# Split featureName
#   Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
#   Triaxial Angular velocity from the gyroscope. 
#   A 561-feature vector with time and frequency domain variables. 

dt$featureType    <- as.character(lapply(strsplit(as.character(dt$featureName), split = "-"), "[", 1))
dt$featureStat    <- as.character(lapply(strsplit(as.character(dt$featureName), split = "-"), "[", 2))
dt$featureAxis    <- as.character(lapply(strsplit(as.character(dt$featureName), split = "-"), "[", 3))
dt$featureDomain  <- substr(dt$featureType, 1, 1)

# Create tidy dataset of averages

setkey(dt, subject, activityName, featureType, featureDomain, featureStat, featureAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]

write.table(dtTidy, "tidydata.txt", sep="\t")
