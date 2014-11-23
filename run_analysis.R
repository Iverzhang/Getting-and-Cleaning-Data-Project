#FIRST SET YOUR DATA AND PROGRAM PATH TO UNZIPED ROOT LOCATION
setwd("F:/Code/R/datacourse/getting&cleaning/UCI HAR Dataset")
#READ FILES
featurename <- read.table("features.txt",stringsAsFactors = FALSE)
actname <- read.table("activity_labels.txt",stringsAsFactors = FALSE)
train_x <- read.table("train/X_train.txt",stringsAsFactors = FALSE,col.names = featurename[,2])
train_y <- read.table("train/Y_train.txt",stringsAsFactors = FALSE)
test_x <- read.table("test/X_test.txt",stringsAsFactors = FALSE,col.names = featurename[,2])
test_y <- read.table("test/Y_test.txt",stringsAsFactors = FALSE)
subject_train <- read.table("train/subject_train.txt",stringsAsFactors = FALSE)
subject_test <- read.table("test/subject_test.txt",stringsAsFactors = FALSE)
#COMBINE TRAIN AND TEST FEATURES
data <- train_x
data <- rbind(data,test_x)
#GREP MEAN AND SD
mean_and_std <- grep("mean|std",featurename[,2])
data <- data[,mean_and_std]
#PASTE ACTIVITY AND SUBJECT TO DATA
activity <- rbind(train_y,test_y)
data$activity <- factor(activity[,1], labels = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
subject <- rbind(subject_train,subject_test)
data$subject <- as.factor(subject[,1])
#COMPUTE EVERY FEATURE'S MEAN AND SD
fac <- list(data$activity,data$subject)
activity <- rep(actname[,2],30)
subject <- rep(1:30,each = 6)
result <- cbind(activity,subject)
for (i in 1:79)
{
  mean <- tapply(data[,i],fac,mean)
  sd <- tapply(data[,i],fac,sd)
  dim(mean)<-c(180,1)
  dim(sd)<-c(180,1)
  result <- cbind(result,mean)
  colnames(result)[2*i+1]<-paste("mean of",colnames(data)[i])
  result <- cbind(result,sd)
  colnames(result)[2*i+2]<-paste("sd of",colnames(data)[i])
}
#OUTPUT
write.table(result,file="result.txt",row.names = FALSE)