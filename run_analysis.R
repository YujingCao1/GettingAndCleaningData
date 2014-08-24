#Step 1.Merges the training and the test sets to create one data set.

## load train datasets
features=read.table('./features.txt',header=FALSE) # read features file
activitylabels=read.table("./activity_labels.txt",header=FALSE) # read a file about activity types
subjecttrain=read.table("./train/subject_train.txt",header=FALSE) # read subject_train file
Xtrain=read.table("./train/X_train.txt",header=FALSE) # read X_train file
Ytrain=read.table("./train/y_train.txt",header=FALSE) # read y_train file

##Assign column names to these aboved train files
colnames(activitylabels)=c("activityID","activityType")
colnames(subjecttrain)="subjectID"
colnames(Xtrain)=features[,2]
colnames(Ytrain)="activityID"

##create a new train dataset by combining subjecttrain, Xtrain and Ytrain (PS:combind by columns)
Train=cbind(subjecttrain,Xtrain,Ytrain)

##load test datasets
Xtest=read.table("./test/X_test.txt",header=FALSE)
Ytest=read.table("./test/y_test.txt",header=FALSE)
subjecttest=read.table("./test/subject_test.txt",header=FALSE)

##Assign column names to these aboved test files
colnames(Xtest)=features[,2]
colnames(Ytest)="activityID"
colnames(subjecttest)="subjectID"

#create a new test dataset by combining sujecttest,Xtest and Ytest together by columns
Test=cbind(subjecttest,Xtest,Ytest)
#create a new dataset by combining Test,Train and features together by rows
data=rbind(Train,Test)

#Step 2.Extracts only the measurements on the mean and standard deviation for each measurement. 

##create a logical vector which contains true values for ID,mean() and std() columns and false values for others
mean_and_std=grepl("mean|std|subjectID|activityID",colnames(data)) # logical vector which only contains TRUE and FALSE

##get a subset that just contains the columns we want based on the logical vector we got 
sub_data=data[mean_and_std==TRUE]

#Step 3.Uses descriptive activity names to name the activities in the data set

##merge sub_data with activitylabels to obtain descriptive activity names
finaldata=merge(sub_data,activitylabels,by="activityID",all=TRUE)

#Step 4. Appropriately labels the data set with descriptive variable names

##clear up variable names and use descriptive names to name the columns 
names(finaldata)=gsub("-","",names(finaldata))
names(finaldata)=gsub("\\(\\)","",names(finaldata)) ##remove parentheses
names(finaldata)=gsub("-mean","Mean",names(finaldata))
names(finaldata)=gsub("-std$","Stddev",names(finaldata))
names(finaldata)=gsub("^t","Time",names(finaldata))
names(finaldata)=gsub("^f","Freq",names(finaldata))
names(finaldata)=gsub("[Bb]ody[Bb]ody|[Bb]ody","Body",names(finaldata))
names(finaldata)=gsub("[Gg]yro","Gyro",names(finaldata))
names(finaldata)=gsub("JerkMag","JerkMagnitude",names(finaldata))
names(finaldata)=gsub("[Gg]ravity","Gravity",names(finaldata))
names(finaldata)=gsub("GyroMag","GyroMagnitude",names(finaldata))

#Step 5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject

##make a new dataset without activityType named finaldata1
finaldata1=finaldata[,names(finaldata)!="activityType"]

##summarizing the finaldata1,then we can get a new dataset which just contains the average of each variable for each activity and each subject
finaldata2=aggregate(finaldata1[,names(finaldata1)!=c("activityID","subjectID")],by=list(activityID=finaldata$activityID,subjectID=finaldata$subjectID),FUN="mean",na.rm=TRUE)

##merge finaldata2 with activitylabels with descriptive names to tidy data which is what we want finally
TidyData=merge(finaldata2,activitylabels,by="activityID",all=TRUE)

#Export the tidy dataset
write.table(TidyData,"./Tidydata.txt",row.names=TRUE,sep="\t")
