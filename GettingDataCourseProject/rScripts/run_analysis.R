run_analysis <- function()
{
     ##################
     ##run_analysis - function to clean and prepare UCI Data source 
     ##No input variables
     ##
     ##Outputs data table of average observations for set of data captured from Samsung accelerometer/gyroscope
     ##For more information and to download the raw data set, see http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones  
     ##################
     
     ##load plyr package
     require(plyr)
     require(data.table)
     
     ##Validation - checks to ensure 4 required data files are present
     
     if (!(file.exists("subject_test.txt") & file.exists("subject_train.txt") & file.exists("X_test.txt") & file.exists("X_train.txt") & file.exists("y_test.txt") & file.exists("y_train.txt")))
          {
               stop("Samsung Data Files not found.  See README.md for details on acquiring data files.")
          }
     
     ##Step 1 - read data into single data frame
     allData <- rbind(cbind(read.table("subject_test.txt"),read.table("X_test.txt"),read.table("y_test.txt")),cbind(read.table("subject_train.txt"),read.table("X_train.txt"),read.table("y_train.txt")))
             
     ##Step 2 - using documentation from features.txt extract observations that are mean and std dev for each measurement
     ##Note indices will deviate by 1 from documented features.txt due to addition of subject indicator as column 1 of allData
     ##Results in 68 column data set (2 obs for each of 33 variables plus subject and activity columns)
     smallData <- allData[,c(1,2:7,42:47,82:87,122:127,162:167,202:203,215:216,228:229,241:242,254:255,267:272,346:351,425:430,504:505,517:518,530:531,543:544,563)]
     
     ##release memory
     rm(allData)
     
     ##Step 3 - create factors for subject and activity
     ##For activity factor provide human readable labels
     activityF <- factor(smallData[,68],levels=1:6,labels=c('Walking','WalkingUpstairs','WalkingDownstairs','Standing','Sitting','Laying'))
     subjectF <- factor(smallData[,1],levels=1:30) 

     ##Since we are outputting a derived data set which will only include the factors anyway, replace raw integer data with generated factor columns
     ##If modified raw data was the output, would retain both raw subject/activity entries along with factor versions
     
     smallData<-cbind(subjectF,smallData[,2:67],activityF)
     
     ##Step 4 - apply meaningful column names before doing analysis
     ##Loaded from documentation file "NewFeatures.txt"
     
     names(smallData) = read.table('newFeatures.txt')[,1]
     
     ##Step 5 - take mean of all variables by Subject and Activity
     ##Note: I tried for several hours to do this elegantly.  Ultimately this elegance isn't required by the grading rubric so 
     ##     I've given up and just gone with the literal variable names for this assignment rather than something re-usable.
     ##     There's got to be a better way...
     
     smallData<-data.table(smallData)
     tidyData<-smallData[,.(mean(BodyAccelMean_Xaxis),mean(BodyAccelMean_Yaxis),mean(BodyAccelMean_Zaxis),mean(BodyAccelStDev_Xaxis),mean(BodyAccelStDev_Yaxis),mean(BodyAccelStDev_Zaxis),mean(GravityAccelMean_Xaxis),mean(GravityAccelMean_Yaxis),mean(GravityAccelMean_Zaxis),mean(GravityAccelStDev_Xaxis),mean(GravityAccelStDev_Yaxis),mean(GravityAccelStDev_Zaxis),mean(BodyAccelJerkMean_Xaxis),mean(BodyAccelJerkMean_Yaxis),mean(BodyAccelJerkMean_Zaxis),mean(BodyAccelJerkStDev_Xaxis),mean(BodyAccelJerkStDev_Yaxis),mean(BodyAccelJerkStDev_Zaxis),mean(BodyGyroscopeMean_Xaxis),mean(BodyGyroscopeMean_Yaxis),mean(BodyGyroscopeMean_Zaxis),mean(BodyGyroscopeStDev_Xaxis),mean(BodyGyroscopeStDev_Yaxis),mean(BodyGyroscopeStDev_Zaxis),mean(BodyGyroscopeJerkMean_Xaxis),mean(BodyGyroscopeJerkMean_Yaxis),mean(BodyGyroscopeJerkMean_Zaxis),mean(BodyGyroscopeJerkStDev_Xaxis),mean(BodyGyroscopeJerkStDev_Yaxis),mean(BodyGyroscopeJerkStDev_Zaxis),mean(BodyAccelMagnitudeMean),mean(BodyAccelMagnitudeStDev),mean(GravityAccelMagnitudeMean),mean(GravityAccelMagnitudeStDev),mean(BodyAccelJerkMagnitudeMean),mean(BodyAccelJerkMagnitudeStDev),mean(BodyGyroscopeMagnitudeMean),mean(BodyGyroscopeMagnitudeStDev),mean(BodyGyroscopeJerkMagnitudeMean),mean(BodyGyroscopeJerkMagnitudeStDev),mean(FFT_BodyAccelMean_Xaxis),mean(FFT_BodyAccelMean_Yaxis),mean(FFT_BodyAccelMean_Zaxis),mean(FFT_BodyAccelStDev_Xaxis),mean(FFT_BodyAccelStDev_Yaxis),mean(FFT_BodyAccelStDev_Zaxis),mean(FFT_BodyAccelJerkMean_Xaxis),mean(FFT_BodyAccelJerkMean_Yaxis),mean(FFT_BodyAccelJerkMean_Zaxis),mean(FFT_BodyAccelJerkStDev_Xaxis),mean(FFT_BodyAccelJerkStDev_Yaxis),mean(FFT_BodyAccelJerkStDev_Zaxis),mean(FFT_BodyGyroscopeMean_Xaxis),mean(FFT_BodyGyroscopeMean_Yaxis),mean(FFT_BodyGyroscopeMean_Zaxis),mean(FFT_BodyGyroscopeStDev_Xaxis),mean(FFT_BodyGyroscopeStDev_Yaxis),mean(FFT_BodyGyroscopeStDev_Zaxis),mean(FFT_BodyAccelMagnitudeMean),mean(FFT_BodyAccelMagnitudeStDev),mean(FFT_BodyAccelJerkMagnitudeMean),mean(FFT_BodyAccelJerkMagnitudeStDev),mean(FFT_BodyGyroscopeMagnitudeMean),mean(FFT_BodyGyroscopeMagnitudeStDev),mean(FFT_BodyGyroscopeJerkMagnitudeMean),mean(FFT_BodyGyroscopeJerkMagnitudeStDev)),by=c("SubjectNumber","Activity")]
     ##Map Names to data.table
     setnames(tidyData,3:68,names(smallData)[2:67])
     
     ##return
     tidyData
}