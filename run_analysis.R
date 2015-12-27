## run_analysis.R
##
## Script for the Getting and Cleaning Data Course Project
##
## Peforms the following functionality:
##   1. Merges the Training and Test set to create one data set
##   2. Extracts only the measurements on the mean and standard 
##      deviation for each measurement
##   3. Uses descriptive activity names to name the activities
##      in the data set
##   4. Appropriately labels the data set with descriptive
##      variable names
##   5. Form the data set up in step 4, creastes a second, 
##      independent tidy data set with the average of each
##      variable for each activity and each subject.
##
## Outputs:
##   1.  merged tidy data set written to "tidy_stdmean.txt"
##       this is the output at the end of step 4.
##   2.  independent tidy data set of averages written to
##      "tidy_avgs.txt"
##       this is the output at the end of step 5.
##
##   Before running this script ensure that the UCI HAR dataset
##   folder supplied as part of the Coursera project discription
##   page is present in the working directory for R.
##   This folder should be as extracted from the download and
##   contain the test and train sub-folders which in turn
##   contain the data files needed for this script, as well as
##   the activity labels, features and features info files.
##
##   File download commands have not been included in this
##   script due to difference in arguments required in different
##   environments (e.g. use of curl on a mac)
##
##   Processing in the script makes use of R packages that
##   should be installed prior to running the script.
##   1. plyr
##   2. dplyr
##   3. data.table
##
##   Note - plyr should be loaded before dplyr to ensure the
##   script runs as intended.
################################################################

#run_analysis <- function(){
###############################################################
##   1. Merge the train and test sets to create one data set:
##      Initail merging of each of the 3 data file sets of
##      interest:
##      1. X_test & X_train #core Variable data for each obs
##      2. Y_test & Y_train #activity identifiers for each obs
##      3. subject_test & subject_train #subject id for each obs
##
##      These 3 merged data sets will be left seperate until step 3
##      to make further processing easier in steps 2 & 3 prior 
##      to creating the fully joined up view needed for the output 
##      in step 4.

#load in plyr and dplyr packages
library(plyr)
library(dplyr)

# Read in the test data sets
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Read in the train data sets
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Read in the files for activity labels, features
act_labs <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

# join test and train datasets to create a single set for each file type
# (variable data (testX and trainX), label data (testY and trainY), and
# subject data (subject_test and subject_train)

X_full <- rbind(X_test, X_train)
Y_full <- rbind(Y_test, Y_train)
subject_full <- rbind(subject_test, subject_train)

#set column names for X_full to the feature labels in features
names(X_full) <- features[,2]

# set column name for Y_full to "activityId", as Y_full contains the activity
# identifier.  format aligned with how all columns names will be formatted
# in later steps.
names(Y_full) <- "activityId"

# set column name for subject_full, to subjectId, as subject_full contains
# the subject identifier.
names(subject_full) <- "subjectId"

# set column names for act_labs as "activityId" and "activity"
names(act_labs) <- c("activityId", "activity")
#print(names(act_labs))

# set column names for features as Feature_ID and Feature_Name
names(features) <- c("featureId", "featureName")
#names(features)

################################################################
##   2. Extract only the measurements on the mean and standard
##      deviation for each measurement.
##
##      selecting the columns from X_full is easier before joining
##      to the subject (subject_full) and activity (activity_full)
##      data, as X_full can be subsetted based on the columns
##      identified as required in the output set.
##
##      Columns to retain in the ouput identified through searching
##      for the "mean()" and "std()" strings.
##      This approach intentionally excludes the following as
##      I have assumed they do not fall into the category of
##      measurements asked for by the assignment:
##        - meanfreq() values
##        - the vectors with names containing Mean used on the angle()
##          variable (see features_info for details of these).

# Build Boolean vector that identifies variable names with "mean()" and
# "std()" in them:
# uses the grepl() function and regular expressions introduced in week 
# four lecture videos. 

keep_features <- grepl("mean\\(\\)|std\\(\\)", features[,2])

# Extract only the columns of interest (mean and standard deviation measurements)
# by subsetting the columns of X_full based on the keep_features vector.

tidy_stdmean <- X_full[, keep_features]

################################################################
##   3. Use descriptive activity names to name the activites in the data

# Create a list of Activity Names corresponding to the Activity IDs
# in Y_full:
# replace Activity_IDs with Activity Names from act_labs
# plyr join() function used in preference to merge(), as merge does 
# not preserve row order whereas join does for the first data frame
# in the call to it.
# The "[-1]" at the end of the command drops the Activity_ID column
# so that only the column with the Activity Names remains

# convert activity names to lowercase
act_labs[[2]] <- tolower(as.character(act_labs[[2]]))

# remove "_" characters and convert to lowerCamelCase (to be consistent
# with later choices for formatting column names).
act_labs[[2]] <- gsub("_d", "D", act_labs[[2]])
act_labs[[2]] <- gsub("_u", "U", act_labs[[2]])

Activities <- join(Y_full, act_labs, by="activityId")[-1]

#convert activity names to lowercase
#Activities[1] <- tolower(Activites[1])

# add the activity Names as a column on the front of the varaible data
tidy_stdmean <- cbind(Activities, tidy_stdmean)

# To complete the structure for the tidy dataset the Subject list needs
# to be added onto the front of columns
tidy_stdmean <- cbind(subject_full, tidy_stdmean)

################################################################
##   4. Appropriately label the data set with descriptive variable names
##      use of the following tidy data principles from the week 4 lecture
##      to decide appropriate label format.  The main deviation from
##      the week 4 lecture principles is the choice of using lowerCamel
##      case rather than all lower case.  This is due to the compound
##      nature of the column names and to make them more easily readable.
##
##      1. format as lowerCamelCase (rather than all lower case)
##      2. Descriptive
##      3. Not duplicated
##      4. Not have underscores or dots or white spaces.
##
##      Additional principles used for this data set
##      5. remove parenthesis "(" & ")" in Variable names (e.g. for std() and mean()).
##      6. remove "-" characters in Variable names.

# make initial letters of all words uppercase (except for the first
# word. Data is already in this form apart from the words "mean" and "std"
#names(tidy_stdmean) <- tolower(names(tidy_stdmean))
names(tidy_stdmean) <- gsub("mean","Mean", names(tidy_stdmean))
names(tidy_stdmean) <- gsub("std","Std", names(tidy_stdmean))

# make names more descriptive by expanding some abbreviated terms based on
# information in the features_info.txt file.  uses gsub()
# "t" at the start of a name replaced with "time"
# "f" at the start of a name replaced with "freq"
#
# all other abbreviated terms retained to prevent variable names
# becoming too long and unreadable
names(tidy_stdmean) <- gsub("^t","time", names(tidy_stdmean))
names(tidy_stdmean) <- gsub("^f","freq", names(tidy_stdmean))


# count of variable names should be 68
# test that all names are unique using unique() and names():
print("performing uniqueness check on variable names: expected 68")
cat("unique names: ",length(unique(names(tidy_stdmean))),"\n") #this should be 68

# remove unwanted characters from variable names using gsub
# "\\" required as escape character for "(" or ")".
names(tidy_stdmean) <- gsub("_|-|\\(|\\)","", names(tidy_stdmean))

print("creating output file for tidy_stdmean.txt")
write.table(tidy_stdmean, file = "./tidy_stdmean.txt", row.names=FALSE)


################################################################
##   5. From the data set in step 4. create a second, independent
##      tidy data set with the average of each variable for each
##      activity and each subject.

# use dplyr package group_by() and summarise_each() functions to 
# group data and generate means for each column across the full 
# set of columns (excluding the subjectid and activity columns
# used for grouping the data).  Acting on ouput dataset from step 4.
# dply used over the plyr ddply approach (now commented out)
# due to better perfomance and readability of code. 
#tidy_avgs <- ddply(tidy_stdmean, .(SubjectId, Activity), colwise(mean))

tidy_avgs <- tidy_stdmean %>% group_by(subjectId, activity) %>% 
              summarise_each(funs(mean))

#  headings relableled to be prefixed with "avg" to reflect that the data
#  is now an average of the measure across the grouped criteria (subject
#  and activity.
names(tidy_avgs) <- gsub("^time", "avgTime", names(tidy_avgs))
names(tidy_avgs) <- gsub("^freq", "avgFreq", names(tidy_avgs))

#Write output file
print("creating output file for tidy_avgs.txt")
write.table(tidy_avgs, file = "./tidy_avgs.txt", row.names=FALSE)

#}
