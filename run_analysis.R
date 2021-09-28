library(dplyr)
library(plyr)

#Read txt files for train and test data:
list.files(path='./UCI HAR Dataset/', pattern="*.txt")
list.files(path='./UCI HAR Dataset/test/', pattern="*.txt")
list.files(path='./UCI HAR Dataset/train/', pattern="*.txt")

x_test <- read.table(file = './UCI HAR Dataset/test/X_test.txt')
y_test <- read.table(file = './UCI HAR Dataset/test/y_test.txt')
subject_test <- read.table(file = './UCI HAR Dataset/test/subject_test.txt')

x_train <- read.table(file = './UCI HAR Dataset/train/X_train.txt')
y_train <- read.table(file = './UCI HAR Dataset/train/y_train.txt')
subject_train <- read.table(file = './UCI HAR Dataset/train/subject_train.txt')

# 1.Merges the training and the test sets to create one data set.
train <- cbind(subject_train, y_train, x_train)
test <- cbind(subject_test, y_test, x_test)
data <- rbind(test, train)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement.
#Read features table that has column names
features <- read.table(file = './Course 3/UCI HAR Dataset/features.txt')
#look for column names that include 'mean' and 'std' measurements
means <- grep("mean",features$V2)
stds <- grep("std",features$V2)

#add 2 to column index numbers as there two additional columns in data (IDs and activity names)
means <- means + 2
stds <- stds + 2
#Use column index numbers as to find columns in data
#Select only the columns in data that contain mean and std and create a new dataframe
data2 <- data[,c(1:2,means,stds)]

# 3.Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table(file = './Course 3/UCI HAR Dataset/activity_labels.txt')
#Use activity labels to replace ID numbers in a new column called Activity
data2$Activity <- mapvalues(data2$V1.1, from = c(1,2,3,4,5,6), to = activity_labels$V2)
#Delete column with ID numbers
data3 <- data2[,-2]

# 4.Appropriately labels the data set with descriptive variable names. 
#Adjust variable names to make them more descriptive and remove special characters
features$V2 <- sub("t", "time", features$V2)
features$V2 <- sub("f", "freq", features$V2)
features$V2 <- gsub("-", ".", features$V2)
features$V2 <- gsub(",", ".", features$V2)

#Add 'V' to column name index to match current column names
features$V1 <- paste("V", features$V1, sep="")
#Copy dataset
data4 <- data3
#Adjust duplicate column names
names(data4)[names(data4) == 'V1'] <- 'Subject'
names(data4)[names(data4) == 'V1.2'] <- 'V1'
#Rename column names based on matches in features data
names(data4)[which(names(data4)%in%features$V1)]=as.character(features$V2[match(names(data4)[which(names(data4)%in%features$V1)],features$V1)])
#Print names
names(data4)

# 5.From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
#Copy dataset
data5 <- data4
as.numeric(as.character())

#Group data set by Subject and Activity and then summarise each remaining variable by its average
tidy_data <- data5 %>%
  group_by(Subject, Activity) %>%
  summarise_each(funs(mean))

write.table(tidy_data,"tidy_data.txt",sep=" ",row.names=TRUE)
