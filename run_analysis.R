fileUrl<- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#X<-unz("data.zip")

temp <- tempfile()
download.file(fileUrl, temp)
#data <- read.table(unz(temp, "UCI HAR Dataset/test/X_test.txt"))



## Read data sets and combine
testData <- read.table(unz(temp, "UCI HAR Dataset/test/X_test.txt"))
str(testData)
trainData <- read.table(unz(temp, "UCI HAR Dataset/train/X_train.txt"))

X <- rbind(testData, trainData)

## Garbage collection
rm(testData)
rm(trainData)

## Read subjects and combine
testSub <- read.table(unz(temp, "UCI HAR Dataset/test/subject_test.txt"))
trainSub <- read.table(unz(temp, "UCI HAR Dataset/train/subject_train.txt"))
S <- rbind(testSub, trainSub)

## Garbage collection
rm(testSub)
rm(trainSub)

## Read in data labels and combine
testLabel <- read.table(unz(temp, "UCI HAR Dataset/test/y_test.txt"))
trainLabel <- read.table(unz(temp, "UCI HAR Dataset/train/y_train.txt"))
Y <- rbind(testLabel, trainLabel)

## Garbage collection
rm(testLabel)
rm(trainLabel)

## Read Features List (to be used as column names for data)
featuresList <- read.table(unz(temp, "UCI HAR Dataset/features.txt"), stringsAsFactors = FALSE)

## Use only names from features list
features <- featuresList$V2

## Logical Vector to keep only std and mean columns
keepColumns <- grepl("(std|mean[^F])", features, perl=TRUE)

## Keep only data we want, and name it human readable
X <- X[, keepColumns]
names(X) <- features[keepColumns]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

## Read ActivityList (to add descriptive names to data set)
activities <- read.table(unz(temp, "UCI HAR Dataset/activity_labels.txt"))
activities[,2] = gsub("_", "", tolower(as.character(activities[,2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity" ## Add activity label

## Add human readable labels to activity names
names(S) <- "subject"
tidyData <- cbind(S, Y, X)
write.table(tidyData, "tidyData.txt")

## Create second tiny data set with avg of each var for each act and each sub
uS = unique(S)[,1]
nS = length(uS)
nA = length(activities[,1])
nC = length(names(tidyData))
td = tidyData[ 1:(nS*nA), ]

row = 1
for (s in 1:nS) {
	for (a in 1:nA) {
		td[row,1] = uS[s]
		td[row,2] = activities[a, 2]
		tmp <- tidyData[tidyData$subject==s & tidyData$activity==activities[a,2],]
		td[row, 3:nC] <- colMeans(tmp[, 3:nC])
		row = row + 1
	}
}

write.table(td, "tidyData2.txt")
unlink(temp)
