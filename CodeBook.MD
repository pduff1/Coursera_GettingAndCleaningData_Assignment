## Codebook for Getting and Cleaning Data Class Assignment

This CodeBook describes the data in the output files from the Getting and Cleaning Data course assignment.

The data in the course assignment datasets works on a set of data taken from a Human Activity Recognition database built from the recordings of 30 subjects performing activities of daily living (ADL) while carrying a waist-mounted smartphone with embedded inertial sensors.

The Assignment goal for the data was to carry out a number steps using an R script (run_analysis.R) and output tidy data sets meeting the following assignment criteria:

1. Merges the training and the test sets to create one data set.
1. Extracts only the measurements on the mean and standard deviation for each measurement. 
1. Uses descriptive activity names to name the activities in the data set
1. Appropriately labels the data set with descriptive variable names. 
1. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


The contents of this CodeBook should be read in conjunction with the corresponding descriptions
of the source data used as the starting point for run_analysis.R.

This information is contained in the following files and this CodeBook does not seek to duplicate information from these sources:

* features_info.txt
* features.txt
* the README file supplied with the project data files.

Further detail around the background of the data is also available form the following site that the project data was obtained from:

[http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

The CodeBook should also be read along side the "run_analysis.R" script, as the code comments with this script provide additional detail on the transformation of the data performed by the script.

### Outputs from run_analysis.R:

The CodeBook contains details of Variables of both the data sets output from running run_analysis.R:

Both data sets have been transformed/formatted as tidy data sets containing 
- Each Variable measured in one Column
- Each different Observation in a different Row
- First row of output data files contains the Variable names
- Human readability factored into decisions around naming (i.e. Use of lowerCamelCase over all lower case.


1. **tidy_stdmean.txt** (10299 x 68) - Tidy dataset output from Step 4 in the assignment with the follow characteristics:

 * Merges the training and the test sets to create one data set
	 * Combines X\_test.txt and X\_train.txt to provide the full set of Variables and data measured (10299 observations)
	 * Combines Y\_test.txt and Y\_train.txt to provide the full set Activity Ids for the Activity that was measured for each subject for each observation
	 * subject\_test and subject\_train.txt to identify the subject ID of the subject that took part in each observation.
 * Extracts only the measurements on the mean and standard deviation for each measurement
	 * Variables selected that contained only mean or standard deviation results (contain "mean()" or "std()") based on the features_info.txt and features.txt content.  This results in 66 Variables retained for the final dataset giving a total of 68 when combined with subjectId and activity.
 * Uses descriptive activity names to name the activities in the data set
	 * uses the activity\_labels.txt file to replace the Ids obtained from Y\_test.txt and Y\_train.txt with meaningful names (formatted as discribed further down this file).
 * Appropriately labels the data set with descriptive variable names.
	 * Variable names formatted as described further down this file to meet a tidy data and human readability criteria, based on the Variable names contained in features.txt.

* **tidy_avgs.txt** (180 x 68) - From the data in tidy_stdmean.txt (output from step 4), a second, independent tidy data set: 
 * with the averages of each variable for each activity and each subject
	 * data grouped by subjectId and activity (using dplyr group_by() and summarise_each(funs(mean)) functions to give a data set of average values for each Variable for each subject and activity (180 observations x 68 columns)
 
The "run_analysis.R" script contains further comments highlighting rationale and detail for the transformation and formatting decisions made, and should be viewed alongside this file.

The tidy_stdmean.txt files contains all observations from the test and train data files provided (X\_test, X\_train, Y\_test, Y\_train, subject\_test & subject\_train), which results in 10299 rows of data.

### Variables in tidy_stdmean.txt

The tidy_stdmean.txt data contains 68 variables as listed below.  Variables 1 & 2 contain observation identifiers/names for the subject and activity that the measurements are for.

Variables 3 to 68 are the 66 columns identified as containing data that contains mean or standard deviation values out of the original set of 561 measures.  See the run_analysis.R script for more detailed comments on inclusion criteria of fields, however at a high level this was filtered based on those containing "mean()" or "std() in the Variable name, following analysis of the contents of the variable names in the features.txt file and the supporting information in features_info.txt".

Variables 3 to 68 contain Measure values where the meaning of the column heading can be interpreted from the name components as follows:

- **time** -  Time domain signals captured at a rate of 50Hz from the accelerometer and gyroscope 3-axial raw signals (see features_info.txt)
- **freq** - Frequency domain signals, where a Fast Fourier Transform was applied to some of signals.
- **Body** - Body Acceleration or Gyroscope signal
- **Acc** - Accelerometer signal
- **Gravity** - Gravity acceleration signal
- **Jerk** - Body linear acceleration and angular velocity derived in time to give the Jerk signal 
- **Gyro** - Gyroscope signal
- **Mean** - Mean value
- **Std** - Standard Deviation
- **X** - Axis for the 3-axial signals in the X direction
- **Y** - Axis for the 3-axial signals in the Y direction
- **Z** - Axis for the 3-axial signals in the Z direction
- **Mag** - magnitude of three dimensional signals calculated using the Euclidian norm

**Units** - For variables 3 to 68 data values are normalized and bounded within \[-1,1\] (see README file supplied with the project data).  Original measurements were conducted using the following units:

- acceleration units 'g's (gravity of earth -> 9.80665 m/sec2)
- gyroscope units rad/sec
- data was captured at a frequency of 50Hz

Note unit data of original measurements taken from the following source:
[http://archive.ics.uci.edu/ml/datasets/Smartphone-Based+Recognition+of+Human+Activities+and+Postural+Transitions](http://archive.ics.uci.edu/ml/datasets/Smartphone-Based+Recognition+of+Human+Activities+and+Postural+Transitions)

Text of all variable names has been formatted to lowerCamelCase to meet the criteria of tidy data while maintaining human readability and formatted using the following principles:

1. format as lowerCamelCase (rather than all lower case to maintain readability of long compound word based names)
1. Descriptive (abbreviations expanded where useful - e.g. "t" to "time")
1. Not duplicated
1. Not have underscores or dots or white spaces.
1. remove parenthesis "(" & ")" in Variable names (e.g. for std() and mean()).
1. remove "-" characters in Variable names.

Variables in tidy_stdmean.txt are as follows:

1. **subjectId** - Subject Identifier (1 - 30). The source data was generated from experiments with a group of 30 volunteers aged 19-48 years.  
1. **activity** - Activity Name.  Each of the 30 subjects performed six activities wearing a smartphone on the waist.  The activities are identified by the following 6 possible values (formatted to lowerCamelCase from the original activity labels described in activity_labels.txt:
	1. walking
	2. walkingUpstairs
	3. walkingDownstairs
	4. sitting
	5. standing
	6. laying
1. **timeBodyAccMeanX**
1. **timeBodyAccMeanY**
1. **timeBodyAccMeanZ**
1. **timeBodyAccStdX**
1. **timeBodyAccStdY**
1. **timeBodyAccStdZ**
1. **timeGravityAccMeanX**
1. **timeGravityAccMeanY**
1. **timeGravityAccMeanZ**
1. **timeGravityAccStdX**
1. **timeGravityAccStdY**
1. **timeGravityAccStdZ**
1. **timeBodyAccJerkMeanX**
1. **timeBodyAccJerkMeanY**
1. **timeBodyAccJerkMeanZ**
1. **timeBodyAccJerkStdX**
1. **timeBodyAccJerkStdY**
1. **timeBodyAccJerkStdZ**
1. **timeBodyGyroMeanX**
1. **timeBodyGyroMeanY**
1. **timeBodyGyroMeanZ**
1. **timeBodyGyroStdX**
1. **timeBodyGyroStdY**
1. **timeBodyGyroStdZ**
1. **timeBodyGyroJerkMeanX**
1. **timeBodyGyroJerkMeanY**
1. **timeBodyGyroJerkMeanZ**
1. **timeBodyGyroJerkStdX**
1. **timeBodyGyroJerkStdY**
1. **timeBodyGyroJerkStdZ**
1. **timeBodyAccMagMean**
1. **timeBodyAccMagStd**
1. **timeGravityAccMagMean**
1. **timeGravityAccMagStd**
1. **timeBodyAccJerkMagMean**
1. **timeBodyAccJerkMagStd**
1. **timeBodyGyroMagMean**
1. **timeBodyGyroMagStd**
1. **timeBodyGyroJerkMagMean**
1. **timeBodyGyroJerkMagStd**
1. **freqBodyAccMeanX**
1. **freqBodyAccMeanY**
1. **freqBodyAccMeanZ**
1. **freqBodyAccStdX**
1. **freqBodyAccStdY**
1. **freqBodyAccStdZ**
1. **freqBodyAccJerkMeanX**
1. **freqBodyAccJerkMeanY**
1. **freqBodyAccJerkMeanZ**
1. **freqBodyAccJerkStdX**
1. **freqBodyAccJerkStdY**
1. **freqBodyAccJerkStdZ**
1. **freqBodyGyroMeanX**
1. **freqBodyGyroMeanY**
1. **freqBodyGyroMeanZ**
1. **freqBodyGyroStdX**
1. **freqBodyGyroStdY**
1. **freqBodyGyroStdZ**
1. **freqBodyAccMagMean**
1. **freqBodyAccMagStd**
1. **freqBodyBodyAccJerkMagMean**
1. **freqBodyBodyAccJerkMagStd**
1. **freqBodyBodyGyroMagMean**
1. **freqBodyBodyGyroMagStd**
1. **freqBodyBodyGyroJerkMagMean**
1. **freqBodyBodyGyroJerkMagStd**

### Variables in tidy_avgs.txt

The variables in tidy_avgs.txt have names and descriptions, corresponding to those in tidy_stdmean.txt (outlined above).  Columns 1 and 2 are identical and represent subjectId and activity.  All other columns are the average of the values described for tidy_stdmean.txt, but for observations grouped by subjectId and activity.  The names of these columns have therefore been updated to include the "avg" prefix and formatted to maintain lowerCamelCase format.  All units of measurements and meanings are the same as described for tidy_stdmean.

1. **subjectId**
1. **activity**
1. **avgTimeBodyAccMeanX**
1. **avgTimeBodyAccMeanY**
1. **avgTimeBodyAccMeanZ**
1. **avgTimeBodyAccStdX**
1. **avgTimeBodyAccStdY**
1. **avgTimeBodyAccStdZ**
1. **avgTimeGravityAccMeanX**
1. **avgTimeGravityAccMeanY**
1. **avgTimeGravityAccMeanZ**
1. **avgTimeGravityAccStdX**
1. **avgTimeGravityAccStdY**
1. **avgTimeGravityAccStdZ**
1. **avgTimeBodyAccJerkMeanX**
1. **avgTimeBodyAccJerkMeanY**
1. **avgTimeBodyAccJerkMeanZ**
1. **avgTimeBodyAccJerkStdX**
1. **avgTimeBodyAccJerkStdY**
1. **avgTimeBodyAccJerkStdZ**
1. **avgTimeBodyGyroMeanX**
1. **avgTimeBodyGyroMeanY**
1. **avgTimeBodyGyroMeanZ**
1. **avgTimeBodyGyroStdX**
1. **avgTimeBodyGyroStdY**
1. **avgTimeBodyGyroStdZ**
1. **avgTimeBodyGyroJerkMeanX**
1. **avgTimeBodyGyroJerkMeanY**
1. **avgTimeBodyGyroJerkMeanZ**
1. **avgTimeBodyGyroJerkStdX**
1. **avgTimeBodyGyroJerkStdY**
1. **avgTimeBodyGyroJerkStdZ**
1. **avgTimeBodyAccMagMean**
1. **avgTimeBodyAccMagStd**
1. **avgTimeGravityAccMagMean**
1. **avgTimeGravityAccMagStd**
1. **avgTimeBodyAccJerkMagMean**
1. **avgTimeBodyAccJerkMagStd**
1. **avgTimeBodyGyroMagMean**
1. **avgTimeBodyGyroMagStd**
1. **avgTimeBodyGyroJerkMagMean**
1. **avgTimeBodyGyroJerkMagStd**
1. **avgFreqBodyAccMeanX**
1. **avgFreqBodyAccMeanY**
1. **avgFreqBodyAccMeanZ**
1. **avgFreqBodyAccStdX**
1. **avgFreqBodyAccStdY**
1. **avgFreqBodyAccStdZ**
1. **avgFreqBodyAccJerkMeanX**
1. **avgFreqBodyAccJerkMeanY**
1. **avgFreqBodyAccJerkMeanZ**
1. **avgFreqBodyAccJerkStdX**
1. **avgFreqBodyAccJerkStdY**
1. **avgFreqBodyAccJerkStdZ**
1. **avgFreqBodyGyroMeanX**
1. **avgFreqBodyGyroMeanY**
1. **avgFreqBodyGyroMeanZ**
1. **avgFreqBodyGyroStdX**
1. **avgFreqBodyGyroStdY**
1. **avgFreqBodyGyroStdZ**
1. **avgFreqBodyAccMagMean**
1. **avgFreqBodyAccMagStd**
1. **avgFreqBodyBodyAccJerkMagMean**
1. **avgFreqBodyBodyAccJerkMagStd**
1. **avgFreqBodyBodyGyroMagMean**
1. **avgFreqBodyBodyGyroMagStd**
1. **avgFreqBodyBodyGyroJerkMagMean**
1. **avgFreqBodyBodyGyroJerkMagStd**