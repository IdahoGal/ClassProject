/* Course Assignment */
/* This R Script performs the following:
	1. Merge the training and test sets to create one data set
      2. Extract only the measurements on the mean and standard deviation
      3. Use descriptive activity names to name the activities in the data set
      4. Appropriately label the dataset with descriptive variable names
      5. Create a tidy data set with the average of each variable

## SETUP
getwd()
list.files("./data")

# load libraries
library(data.table)
library(reshape2)   # needed for melt(), dcast()

## READ in data files including observations, labels, and subjects for both Training data
##	and test data.  Also read in the features.txt file for column/feature names.
##    	There should be 7,352 rows for X_train.txt and 2,947 rows for X_Test.txt  
TrainSet <- read.table("./data/train/X_train.txt")
TestSet <- read.table("./data/test/X_test.txt")

TrainSetLabels <- read.table("./data/train/y_train.txt")
TestSetLabels <- read.table("./data/test/y_test.txt")

TrainSetSubjects <- read.table("./data/train/subject_train.txt")
TestSetSubjects <- read.table("./data/test/subject_test.txt")

# For column names, read in Feature.txt.   
FNames <- readLines("./data/features.txt")

# Create data tables from files.   
TrainData <- data.table(TrainSet)
TestData <- data.table(TestSet)


## STEP 1: Merge the data sets.  In this section the train and tests data sets  
##	are combined.  In STEP3, all 3 data stes will be combined.  
# 	Because the data in the 2 sets represent different observations,
# 	Append the rows together using rbind(). Do this for the observation data,
# 	the label data, and the subject data.     
combineData <- rbind(TrainData, TestData)	# 10,299 rows
combineLabels <- rbind(TrainSetLabels, TestSetLabels)  # 10,299 rows
combineSubjects <- rbind(TrainSetSubjects, TestSetSubjects)  # 10,299 rows

## Write out the combineData. Not required, just for traceability.
write.csv(combineData, "CombinedDataRaw.csv", row.names=FALSE)


# STEP2: Extract the measurements on the mean and standard deviation
# to determine which columns qualify to be extracted, if the name
# of the feature includes mean() or std(), then it is assumed to
# be a measurement on the mean or standard deviation.   

# Just keep the columns with 'mean()' or 'std()' in their name. 
# Create a vector of indices that match the columns with 'mean' or 'stnd'.
j <- grep(("mean\\(\\)|std\\(\\)"), FNames)

k <-subset(combineData, select=grep(("mean\\(\\)|std\\(\\)"), FNames))
names(k) <- make.names(FNames[j])

# Add the activity/labels to the data
AllData <- cbind(combineLabels, k)  
# Change the column name to be descriptive
names(AllData)[names(AllData)=="V1"] <- "activity"

# Add the subject data to the data set.
AllData <- cbind(combineSubjects, AllData)  
names(AllData)[names(AllData)=="V1"] <- "subject"

## STEP 3: Use descriptive activity names to name the activities in the data set
AllData$activity <- gsub("1","walking",AllData$activity)
AllData$activity <- gsub("2","walking upstairs",AllData$activity)
AllData$activity <- gsub("3","walking downstairs",AllData$activity)
AllData$activity <- gsub("4","sitting",AllData$activity)
AllData$activity <- gsub("5","standing",AllData$activity)
AllData$activity <- gsub("6","laying",AllData$activity)

## STEP 4:  Appropriately label the dataset with descriptive variable names
##	Column names will be modified to meet these criteria: (1) lower case,
##    (2) unique, (3) descriptive, (4) no underscores or dots or white spaces
##	At the end of step 4, the combined data set is cleaned.

## For column names starting with X+numbers, remove  
names(AllData)<- gsub("X[0-9]+\\.","",names(AllData))

## For column names ending in ...X/Y/Z, remove
names(AllData) <- gsub("\\.\\.\\.[XYZ]$","",names(AllData))

## Remove any periods in the column names
names(AllData) <- gsub("\\.","",names(AllData))

## STEP 5:  Create a tidy data set with the average of each variable
##	For each subject, create a mean for each activity. 
##	The definition of a tidy data is (1) each variable forms a column
##	(2) each observation forms a row, (3) each table/file stores data about one kind
##	of observation
head(AllData)

## Write this file out. Not required, just for traceability.
write.csv(AllData, "CombinedData.csv", row.names=FALSE)

# In this data set, there are two ID fields (subject, activity) 
# and 66 variable measure fields. 
measure.vars <- names(AllData)[3:68]
id.vars <- names(AllData)[1:2]

# Use melt, pass it the data set and tell it which variables are ID variables
# There are 2 ways to create a tidy data set. The first way is to create a narrow,
# tall data set where each subject/activity/measure has a row. 
# In this case there are 679,734 rows.
mAllData <- melt(AllData, id.vars, measure.vars,	
			variable.name="metric",
			value.name="measurement")  
write.csv(mAllData, "NameValuePairs.csv")

# This collapses the data into subject, activies.  30 rows (1 for each subject)
# The aggregation defaults to a count of rows for each activity per subject. 
a = dcast(mAllData, subject~activity)
head(a)

# This collapses the data into subject, activies
b <- dcast(mAllData, subject~activity, mean)
head(b)

# Collapse the data so there is 1 row for each activity per subject. 
c <- dcast(mAllData, subject + activity ~ metric, 
		value.var="measurement",mean)   

c <- dcast(mAllData, subject + activity ~ metric, 
		value.var="measurement")   

c <- dcast(mAllData, subject + activity ~ metric, 
		value.var=measurement, mean)   

c <- dcast(mAllData, subject + activity ~ metric, 
		value.var="measurement", fun.aggregate = mean, na.rm = TRUE)   

mAllData2 <- melt(AllData, id.vars, measure.vars )	
				
d <- dcast(mAllData2, subject + activity ~ variable, 
		fun.aggregate = mean, na.rm = TRUE)   

e <- dcast(mAllData2, subject + activity ~ "Avg"+... , 
		fun.aggregate = mean, na.rm = TRUE)   

f <- dcast(mAllData2, subject + activity ~ "Avg"+... , 
		fun.aggregate = mean)   

## Rename the headings and include 'Avg_' as a 
namesnnames(f) <- gsub("NA_","Avg_",names(f))


# Rename the headings, pre-pend 'Avg_'   
AllData$activity <- gsub("NA_","Avg_",AllData$activity)
		
# Write this file out
write.csv(c, "summary.csv", row.names=FALSE)


This is the part of the assignment that many with need to think through. 
I would certainly recommend consulting the Reshaping Data section of the 
lectures (see ddply, melt, cast, etc.), search out the aggregate() 
function, many other functions like summarize_each(), mutate_each(), by(), etc.  

# Assign names to the variables: names(dataframe) <-c('name1', 'name3', 'name3')
# Consider running the names in features through make.names() and compare with the originals.  
# use gsub to fix up the names. 

/* Get the activity numers in the data and replace them
  with descriptive terms.  Keep in mind, merge reorders the data. */

/* Upload the second data set.  This is the set created in step 4 and the only 
# one explicitly described in the instructions as tidy.



