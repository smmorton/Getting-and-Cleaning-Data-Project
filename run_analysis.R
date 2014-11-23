### getting and cleaning data
### course project - 19 Nov 2014

### file name: run_analysis.R
### output file: run_analysis_out.txt

###
### files:
###
working.directory <- '~/downloads/get/proj/UCI HAR Dataset'
train.data <- 'train/X_train.txt'
test.data <- 'test/X_test.txt'
vble.names.data <- 'features.txt'
subject.test.data <- 'test/subject_test.txt'
subject.train.data <- 'train/subject_train.txt'
activity.test.data <- 'test/y_test.txt'
activity.train.data <- 'train/y_train.txt'
activity.label.data <- 'activity_labels.txt'
output.file <- "run_analysis_out.txt"

###
### read the main data sets:
###

# change to the project's working directory
setwd(working.directory)

# test data (2947x561) and train data (7352x561)
# these are tabular data without variable names
# ultimately we will stack and merge these into one

df.test <- read.table(test.data)
df.train <- read.table(train.data)

###
### filter out some variables
###
### we want "only the measurements on the mean and standard deviation"
###

# read the 561 variable names. vble.names[,2] is a vector
# containing the 561 vble names, in order

vble.names <- read.table(vble.names.data)  # 561 x 2: vble number, vble name

# get the col.nums of variable names containing 'mean' or 'std' in the name
# then remove all columns not in this list from the data sets

mnsd.cols <- grep('(mean)|(std)', vble.names[,2]) # length 79
df.test <- df.test[ , mnsd.cols] # now 2947 x 79
df.train <- df.train[ , mnsd.cols] # now 7352 x 79

# name these columns with their variable names, removing unused names

names(df.test) <- vble.names[,2][mnsd.cols]
names(df.train) <- vble.names[,2][mnsd.cols]

###
### get two more factor vbles to categorize the data:
### subject and activity
###

# get the subject id vble, for each obs

subject.test <- read.table(subject.test.data) # data frame 2947 x 1
subject.train <- read.table(subject.train.data) # data frame 7352 x 1

# get the activity being performed during each obs.
# read the activity data, of the form: obsnum, activity code
# activity codes are integers 1...6

activity.test <- read.table(activity.test.data) # data frame 2947 x 2
activity.train <- read.table(activity.train.data) # data frame 7352 x 2

# get the file which translates numeric activity code
# to activity name: 6 activities of the form: code,name: 6 x 2

activity.label <- read.table(activity.label.data)

# translate the activity code to the activity name

activity.test <- sapply( activity.test, function (x) activity.label[x,2] )
activity.train <- sapply( activity.train, function (x) activity.label[x,2] )

###
### merge data sets
###

# stack the data sets into one data set

df <- rbind(df.test, df.train)
activity <- rbind(activity.test, activity.train)
subject <- rbind(subject.test, subject.train)
rm(df.test, df.train)

###
### calculate means for each subject and activity 
###

# "create a second, independent tidy data set with the average
# of each variable for each activity and each subject."

df.means <- aggregate(df, by=list(activity[,1], subject[,1]),
                      FUN=mean, simplify=T)
colnames(df.means)[1] <- 'activity'
colnames(df.means)[2] <- 'subject'

# save the data with write.table() using row.name=FALSE
write.table(df.means, file = output.file, row.names=F)

###
### END
###

