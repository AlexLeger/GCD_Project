library(dplyr)
library(reshape2)

# I downloaded the data and unzipped it in my working directory
# in the UCI HAR Dataset folder

# I start by reading the features.txt file to get the names of
# the features, which will help build the names for my other files
features <- read.table("UCI HAR Dataset\\features.txt",sep=" ")

# Read train data
X_train <- read.table("UCI HAR Dataset\\train\\X_train.txt")
y_train <- read.table("UCI HAR Dataset\\train\\y_train.txt")
subject_train <- read.table("UCI HAR Dataset\\train\\subject_train.txt")

# Read test data
X_test <- read.table("UCI HAR Dataset\\test\\X_test.txt")
y_test <- read.table("UCI HAR Dataset\\test\\y_test.txt")
subject_test <- read.table("UCI HAR Dataset\\test\\subject_test.txt")

# I use the feature data to set column names for both train and test data
colnames(X_train) <- features$V2
colnames(X_test) <- features$V2

# Now I need to select the features I want to keep and then merge the two sets together
# Given the clever naming scheme in the features, I can do this selection
# by keeping only features with a name containing either "mean" or "std"

# Create a logical vector to do this filtering (using filter from dplyr and grep)
# First I tried to use grep but grepl is nicer here because it gives a logical vector
features_to_keep <- filter(features, grepl('mean',V2) | grepl('std',V2))

# I can now use this vector on my test and train data to "select" my columns
X_train <- X_train[,features_to_keep$V1]
X_test <- X_test[,features_to_keep$V1]

# Create data frames joining subject info, features, and activity label (by columns)
train_data <- cbind(subject_train,X_train,y_train)
colnames(train_data)[1] <- "Subject"
colnames(train_data)[81] <- "Activity"
test_data <- cbind(subject_test,X_test,y_test)
colnames(test_data)[1] <- "Subject"
colnames(test_data)[81] <- "Activity"

# Join both sets (by rows)
all_data <- rbind(train_data,test_data)

# The column names from the features are not very nice so I used the 
# gsub function to try to remove/replace the unwanted characters

colnames(all_data) <- gsub('-','_',colnames(all_data))
colnames(all_data) <- gsub('\\(','',colnames(all_data)) # The ) character needs to be escaped
colnames(all_data) <- gsub('\\)','',colnames(all_data))

# I considered replacing abbreviations such as "Acc" by the full words
# but then the names where really too long.

# The Activity column still contains numbers rather than activity labels
# It needs to be turned into a factor using the Activity_labels file
activity_labels <- read.table("UCI HAR Dataset\\activity_labels.txt",sep=" ")
all_data$Activity <- factor(activity_labels$V2[all_data$Activity])

# Now I can create the data set with the means by activity and by subjects
# This would be easy to do if I had one "measure" column for each feature
# So I reshape my data using the melt function from reshape2

mean_data <- melt(all_data,id=c('Activity','Subject'))

# I can then use dcast from the same package to calculate the means for each
# subgroups
mean_data <- dcast(mean_data,Subject+Activity~variable,mean)

# Print new tidy data file to txt file
write.table(mean_data, file = "my_tidy_data.txt", row.names = FALSE)
