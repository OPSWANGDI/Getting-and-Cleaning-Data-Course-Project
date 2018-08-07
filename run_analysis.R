##Setup working directory
setwd("/Users/dishing/datascience3homework/homework5")

##Read Data
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)

subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)

features<-read.table("./UCI HAR Dataset/features.txt",header = FALSE)
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt",header = FALSE)

##Merges the training and the test sets to create one data set.
subject<-rbind(subject_train,subject_test)
X<-rbind(X_train,X_test)
y<-rbind(y_train,y_test)

names(subject)<-"subject"
names(y)<-"activity"
names(X)<-features[,2]

data<-cbind(X,y,subject)

##Extracts only the measurements on the mean and standard deviation for each measurement.
index<-grep("mean\\(\\)|std\\(\\)",names(data))
result1<-cbind(data[,index],y,subject)

##Uses descriptive activity names to name the activities in the data set
result1$activity<-as.character((result1$activity))
for(i in 1:length(activity_labels[,1])){
        result1$activity<-sub(as.character(activity_labels[i,1]),activity_labels[i,2],result1$activity)
        }

##Appropriately labels the data set with descriptive variable names.
names(result1)<-gsub("^t", "time", names(result1))
names(result1)<-gsub("^f", "frequency", names(result1))
names(result1)<-gsub("Acc", "Accelerometer", names(result1))
names(result1)<-gsub("Gyro", "Gyroscope", names(result1))
names(result1)<-gsub("Mag", "Magnitude", names(result1))
names(result1)<-gsub("BodyBody", "Body", names(result1))

##From the data set in step 4, creates a second, independent tidy data set with the average of each variable
##for each activity and each subject.
tidydata<-aggregate(. ~subject + activity, result1, mean)
tidydata<-tidydata[order(tidydata$subject,tidydata$activity),]
write.table(tidydata, file = "tidydata.txt",row.name=FALSE)
write(codebook(tidydata), file="tidydata-codebook.txt")