README: run_analysis
============

# Steps:
#   Merge the training and the test sets to create one data set.
#   Extract only the measurements on the mean and standard deviation for each measurement. 
#   Use descriptive activity names to name the activities in the data set
#   Appropriately label the data set with descriptive variable names. 
#   Create a second, independent tidy data set with the average of each variable for each activity and each subject. 


```{r}
setwd("~/Dropbox/Mari/courses/Coursera/DS Track/Getting & Cleaning Data/")

packages <- c("data.table", "reshape2")
sapply(packages, require, character.only = TRUE, quietly = TRUE)
```
# Download file

```{r}
path  <- getwd()
url   <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f     <- "Dataset.zip"

if (!file.exists(path)) {
  dir.create(path)
}
download.file(url, file.path(path, f))
```
# Manually unzipped file opens in "UCI HAR Dataset" folder

```{r}
pathdata <- file.path(path, "UCI HAR Dataset")
list.files(pathdata, recursive = TRUE)
```

# Read files

```{r}
fileToDataTable <- function(f) {
  df <- read.table(f)
  dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(pathdata, "train", "X_train.txt"))
dtTest  <- fileToDataTable(file.path(pathdata, "test", "X_test.txt"))

dtSubjectTrain  <- fread(file.path(pathdata, "train", "subject_train.txt"))
dtSubjectTest   <- fread(file.path(pathdata, "test", "subject_test.txt"))

dtActivityTrain <- fread(file.path(pathdata, "train", "Y_train.txt"))
dtActivityTest  <- fread(file.path(pathdata, "test", "Y_test.txt"))

dtActivityNames <- fread(file.path(pathdata, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

dtFeatures      <- fread(file.path(pathdata, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
```

# Merge training & test sets

```{r}
dtSubject   <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")

dtActivity  <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")

dt <- rbind(dtTrain, dtTest)

dtSubjectActivity <- cbind(dtSubject, dtActivity)
dt                <- cbind(dtSubjectActivity, dt)
```

# Extract only mean (mean) and standard deviation (std) for each measurement
# e.g., tBodyAcc-mean()-X, tBodyAcc-std()-X

```{r}
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
```

# Recode featureNum to be consistent with featureCode
# e.g., "1" to "V1", "2" to "V2", ...

```{r}
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
```

# Subset data for only features in dtFeatures

```{r}
setkey(dt, subject, activityNum)
select  <- c(key(dt), dtFeatures$featureCode)
dt      <- dt[, select, with = FALSE]
```

# Reshape data

```{r}
setkey(dt, subject, activityNum, activityName)
dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))
```

# Merge in data labels

```{r}
dt <- merge(dt, 
            dtActivityNames, 
            by = "activityNum", 
            all.x = TRUE)
dt <- merge(dt, 
            dtFeatures[, list(featureNum, featureCode, featureName)], 
            by = "featureCode", 
            all.x = TRUE)
```

# Split featureName

```{r}
dt$featureType = as.character(lapply(strsplit(as.character(dt$featureName), split = "-"), "[", 1))
dt$featureStat = as.character(lapply(strsplit(as.character(dt$featureName), split = "-"), "[", 2))
dt$featureAxis = as.character(lapply(strsplit(as.character(dt$featureName), split = "-"), "[", 3))
```

# Create tidy dataset of averages

```{r}
setkey(dt, subject, activityName, featureType, featureStat, featureAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]

write.table(dtTidy, "tidydata.txt", sep="\t")
```
