
# Readme for Course Assignment  

## Goal  
The goal for this assignment is to prepare tidy data for analyisis.  The deliverables
for this assignment include (1) a tidy data set (2) a link to a Gihub repository,
(3) an R script names run_analysis.R, (4) a code book which is uploaded to
the GitHub directory (and named CodeBook.md), and (5) this README.md.   The purpose of this
Readme is to explain how the script works.

## Assumptions  
The following assumptions were made for this project:
1. The R script assumes the data for the exercize has been downloaded and unzipped and
	starts with reading in the unzipped .txt files.  The link for original zip file
	is referenced towards in the Citations & References section below. 
2. The order of the observations written to the test and train data sets aligns with
	the order of subjects written to subject*.txt and labels*.txt files.
3. Attributes were named using recommended practices that include the following criteria
	(1) mostly lower case - in some cases caps and _ are used for readability, (2) descriptive, 
	(3) unique/not duplicated, and (4) no (minimal) underscores, dots, or white spaces. 
4. Hadley Wickham's criteria for a tidy data include the following and for the purposes
	of this assignment, the uploaded data set will meet these requirements:
	1. Each variable forms a column
	2. Each observation forms a row
	3. Each data set contains info on only one observational unit of analysis
5. Features will be selected based upon their name: A feature with std() or mean() in
	it's name will be selected. 
	
## R Script Description of Data Preprocessing Tasks  

### Setup
	Setup includes loading the libraries needed to work through the script.
   	Other defaults like working directory, etc. have been removed from the 
	script before it was uploaded to GitHub.

### Read the files into R.   This is a total of 7 files that will be combined
	at a later stage into one combined data set.  Check rows counts to make
	sure they are correct (train data) 7,352 and (test data) 2,947.  

TrainSet <- read.table("./data/train/X_train.txt")
TestSet <- read.table("./data/test/X_test.txt")
TrainSetLabels <- read.table("./data/train/y_train.txt")
TestSetLabels <- read.table("./data/test/y_test.txt")
TrainSetSubjects <- read.table("./data/train/subject_train.txt")
TestSetSubjects <- read.table("./data/test/subject_test.txt")
FNames <- readLines("./data/features.txt")

### Explore Data, Verify counts & amounts   
dim(TrainData)		#7,342 rows
dim(TestData)		#2,947 rows
dim(TrainSetLabels)	#7,342 rows
dim(TestSetLabels)	#2,947 rows
dim(TrainSetSubjects)	#7,342 rows
dim(TestSetSubjects)	#2,947 rows
str(TrainData)
str(TestData)
names(TrainData)
names(TestData)
head(TrainData)
head(TestData)
tail(TrainData)
tail(TestData)

How many features are numerics? All 561.
numerics <- which(sapply(TestData, is.numeric)) 
length(numerics) 

How many features are categorical? None.
categorics <- which(sapply(TestData, is.factor)) 
length(categorics) 

Any Nulls or NAs? None. */
TrainNAs <- is.na(TrainData)
sum(TrainNAs)
TestNAs <- is.na(TestData)
sum(TrainNAs)

## STEP 1. Merge the data sets.
	For this step, data sets will be merged in a particular order:
	Train and test observation data will be combined into one data set,
	the subject data will be combined into one subject data set, the label data
	sets will be combined into one label data set.

combineData <- rbind(TrainData, TestData)	# 10,299 rows
combineLabels <- rbind(TrainSetLabels, TestSetLabels)  # 10,299 rows
combineSubjects <- rbind(TrainSetSubjects, TestSetSubjects)  # 10,299 rows

## STEP 2. Select features based on the feature name (see Assumptions). 
	Add the activity data to the primary data set, then 
	add the label data to the primary data set.

	j <- grep(("mean\\(\\)|std\\(\\)"), FNames)
	k <-subset(combineData, select=grep(("mean\\(\\)|std\\(\\)"), FNames))
	names(k) <- make.names(FNames[j])
	AllData <- cbind(combineLabels, k)  
	names(AllData)[names(AllData)=="V1"] <- "activity"
	AllData <- cbind(combineSubjects, AllData)  
	names(AllData)[names(AllData)=="V1"] <- "subject"

## STEP 3. Use descriptive activity names to name the activities in the data set
	AllData$activity <- gsub("1","walking",AllData$activity)
	AllData$activity <- gsub("2","walking upstairs",AllData$activity)
	AllData$activity <- gsub("3","walking downstairs",AllData$activity)
	AllData$activity <- gsub("4","sitting",AllData$activity)
	AllData$activity <- gsub("5","standing",AllData$activity)
	AllData$activity <- gsub("6","laying",AllData$activity)

## STEP 4. Appropriately label the dataset with descriptive variable names.
	See 'Assumptions' for criteria for modifying colum names. 
For column names starting with X+numbers, remove  
	names(AllData)<- gsub("X[0-9]+\\.","",names(AllData))
For column names ending in ...X/Y/Z, remove
	names(AllData) <- gsub("\\.\\.\\.[XYZ]$","",names(AllData))
Remove any periods in the column names
	names(AllData) <- gsub("\\.","",names(AllData))

## STEP 5:  Create a tidy data set with the average of each variable.
	See 'Assumptions' for criteria for a tidy data set.
In this data set, there are two ID fields (subject, activity) 
and 66 variable measure fields. 
	measure.vars <- names(AllData)[3:68]
	id.vars <- names(AllData)[1:2]

Use melt to create a narrow, tall data set where each
subject/activity/measure has a row.  Rows: 679,734
	mAllData <- melt(AllData, id.vars, measure.vars)	
Collapse the data set into the final structure required for the assignment
	fDataSet <- dcast(mAllData, subject + activity ~ "Avg"+... , 
		fun.aggregate = mean)   

Pre-pend the headings to include 'Avg_'  
	amesnnames(fDataSet) <- gsub("NA_","Avg_",names(fDataSet))

Write this file out.  This is the data set that will be uploaded.
	write.csv(f, "summary.csv", row.names=FALSE)

## Citations & References  
1.	Hadley Wickham's Tidy Data paper: http://vita.had.co.nz/papers/tidy-data.pdf
2.	Downloaded original data set: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
3.	RCode and data files from Hadleys "tidy data" presentation: https://github.com/justmarkham/tidy-data 