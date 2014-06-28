# Codebook
**Source: Human Activity Recognition Using Smartphones Dataset Version 1.0**

**Dataset: tidydata.txt**

**Code: run_analysis.R**

**Variables**

| Name 			| Description           																| Type		|
| ------------- | ------------------------------------------------------------------------------------- | ---------	|
| subject      	| subject																				| numeric	|
| activityName  | activity: LAYING, SITTING, STANDING, WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS    | char		|
| featureType 	| feature signals																		| char		|
| featureStat 	| mean(), std()      																	| char		|
| featureAxis 	| 3-axial signals in the X, Y, Z  directions    										| char		|
| count 		| calculated count      																| numeric	|
| average 		| calculated average      																| numeric	|

**Features**

> "The features selected for this database come from the accelerometer and gyroscope 
> 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to 
> denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a 
> median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz 
> to remove noise. Similarly, the acceleration signal was then separated into body and 
> gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass 
> Butterworth filter with a corner frequency of 0.3 Hz."

> "Subsequently, the body linear acceleration and angular velocity were derived in time 
> to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of 
> these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, 
> tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag)."

> "Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing 
> fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. 
> (Note the 'f' to indicate frequency domain signals)."

> "These signals were used to estimate variables of the feature vector for each pattern: 
> '-XYZ' is used to denote 3-axial signals in the X, Y and Z directions."

