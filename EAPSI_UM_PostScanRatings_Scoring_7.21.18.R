
rm(list=ls()) #removes all the variables from workspace

library(ggplot2)
library(dplyr)
library(plyr)
library(stringr) # to add 0 in subject number
library(tidyr) #for extract_numeric function
library(scales) #rescale function
library(foreign)
library(reshape2)

#disable scientific notation
options(scipen=999)

#---------------------------------------------------------
#Task1
# update this file path to point toward appropriate folder on your computer

getwd()

#TASK1
task1_data =ldply(list.files(path="C:/Users/Psychology/Desktop/EAPSI_Miami/Raw_Data/Presentation_logfiles/PostScanRatings/Task1",pattern="*EN.log",full.names=TRUE, recursive = TRUE),function(filename) {
  dum=read.csv(filename,header=T,skip=3,sep="\t",fill=T)
  dum$filename=filename
  
  return(dum)
})

master_task1<-task1_data[which(task1_data$letter.str. != ''),]

#-----------------------------------------

#TASK2
task2_data =ldply(list.files(path="C:/Users/Psychology/Desktop/EAPSI_Miami/Raw_Data/Presentation_logfiles/PostScanRatings/Task2",pattern="*EN.log",full.names=TRUE, recursive = TRUE),function(filename) {
  dum=read.csv(filename,header=T,skip=3,sep="\t",fill=T)
  dum$filename=filename
  
  return(dum)
})

master_task2<-task2_data[which(task2_data$letter.str. != ''),]

#-----------------------------------------

#Select Subject number - task1
subject_task1<-as.data.frame(unique(master_task1$Subject))
subject_task1_long <- as.data.frame(subject_task1[rep(1:nrow(subject_task1),each=40),] )
colnames(subject_task1_long)<-c("SubjectID")

#-----------------------------------------

#Select Subject number - task2
subject_task2<-as.data.frame(unique(master_task2$Subject))
subject_task2_long <- as.data.frame(subject_task2[rep(1:nrow(subject_task2),each=108),] )
colnames(subject_task2_long)<-c("SubjectID")

#-----------------------------------------

#Create empty matrix - task1
#number of replicates times number of subjects included = 40 X 1 = 40
dat_task1<-data.frame(matrix(NA_integer_, nrow=40, ncol=0))

#-----------------------------------------

#Create empty matrix - task2
#number of replicates times number of subjects included = 108 X 1 = 108
dat_task2<-data.frame(matrix(NA_integer_, nrow=108, ncol=0)) 

#-----------------------------------------

#task1
#get stim IDs
stim_id_task1<-as.data.frame(filter(master_task1, grepl("stim_",Code)))
stim_id_task1<-as.data.frame(stim_id_task1$letter.str.)
colnames(stim_id_task1)<-("stim_id")
stim_id_task1<-as.data.frame(str_split_fixed(stim_id_task1$stim_id, "_", 3)) #split by underscore to get stim #
colnames(stim_id_task1)<-c("stim","cond","id")

#-----------------------------------------

#task2
#get stim IDs
stim_id_task2<-as.data.frame(filter(master_task2, grepl("stim_",Code)))
stim_id_task2<-as.data.frame(stim_id_task2$letter.str.)
colnames(stim_id_task2)<-("stim_id")
stim_id_task2<-as.data.frame(str_split_fixed(stim_id_task2$stim_id, "_", 3)) #split by underscore to get stim #
colnames(stim_id_task2)<-c("stim","cond","id")

#-----------------------------------------

#get ratings and rescale

#Task1
#get painint rating
painint_rating<-as.data.frame(filter(master_task1, grepl("painint_rating_",Code)))
painint_rating<-as.data.frame(extract_numeric(painint_rating$Code))
colnames(painint_rating)<-("PainInt_Rating")

#rescale painint rating 
painint_rating<-as.data.frame(rescale(painint_rating$PainInt_Rating, to = c(0, 10), from = range(c(-100, 100), na.rm = TRUE, finite = TRUE)))
colnames(painint_rating)<-("PainInt_Rating")

#get affect rating
affect_rating_task1<-as.data.frame(filter(master_task1, grepl("affect_rating_",Code)))
affect_rating_task1<-as.data.frame(extract_numeric(affect_rating_task1$Code))
colnames(affect_rating_task1)<-("Affect_Rating")

#rescale affect rating 
affect_rating_task1<-as.data.frame(rescale(affect_rating_task1$Affect_Rating, to = c(1, 9), from = range(c(-100, 100), na.rm = TRUE, finite = TRUE)))
colnames(affect_rating_task1)<-("Affect_Rating")

#get arousal rating
arousal_rating_task1<-as.data.frame(filter(master_task1, grepl("arousal_rating_",Code)))
arousal_rating_task1<-as.data.frame(extract_numeric(arousal_rating_task1$Code))
colnames(arousal_rating_task1)<-("Arousal_Rating")

#rescale arousal rating 
arousal_rating_task1<-as.data.frame(rescale(arousal_rating_task1$Arousal_Rating, to = c(1, 9), from = range(c(-100, 100), na.rm = TRUE, finite = TRUE)))
colnames(arousal_rating_task1)<-("Arousal_Rating")

#-----------------------------------------

#Task2
#get affect rating
affect_rating_task2<-as.data.frame(filter(master_task2, grepl("affect_rating_",Code)))
affect_rating_task2<-as.data.frame(extract_numeric(affect_rating_task2$Code))
colnames(affect_rating_task2)<-("Affect_Rating")

#rescale affect rating 
affect_rating_task2<-as.data.frame(rescale(affect_rating_task2$Affect_Rating, to = c(1, 9), from = range(c(-100, 100), na.rm = TRUE, finite = TRUE)))
colnames(affect_rating_task2)<-("Affect_Rating")

#get arousal rating
arousal_rating_task2<-as.data.frame(filter(master_task2, grepl("arousal_rating_",Code)))
arousal_rating_task2<-as.data.frame(extract_numeric(arousal_rating_task2$Code))
colnames(arousal_rating_task2)<-("Arousal_Rating")

#rescale arousal rating 
arousal_rating_task2<-as.data.frame(rescale(arousal_rating_task2$Arousal_Rating, to = c(1, 9), from = range(c(-100, 100), na.rm = TRUE, finite = TRUE)))
colnames(arousal_rating_task2)<-("Arousal_Rating")

#-----------------------------------------

#Combine into one DF
#Task1
task1_ratings<-cbind(subject_task1_long,stim_id_task1,painint_rating,affect_rating_task1,arousal_rating_task1)

#make long form
task1_ratings_melted<-melt(task1_ratings, id=c("SubjectID","cond"))

#subset only rows you need
task1_ratings_melted2 <- task1_ratings_melted[81:200,]

#make numeric
task1_ratings_melted2[, c(4)] <- sapply(task1_ratings_melted2[, c(4)], as.numeric)

#-----------------------------------------

#Task2
task2_ratings<-cbind(subject_task2_long,stim_id_task2,affect_rating_task2,arousal_rating_task2)

#make long form
task2_ratings_melted<-melt(task2_ratings, id=c("SubjectID","cond"))

#subset only rows you need
task2_ratings_melted2 <- task2_ratings_melted[217:432,]

#make numeric
task2_ratings_melted2[, c(4)] <- sapply(task2_ratings_melted2[, c(4)], as.numeric)

#-----------------------------------------

#Create DF of means by condition for each subject - task 1

#subsetting based on condition X variable
#Pain Intensity Rating
task1_ratings_melted2_painint_pain <- task1_ratings_melted2[ which(task1_ratings_melted2$cond=='pain' & task1_ratings_melted2$variable=='PainInt_Rating' ), ]
task1_ratings_melted2_painint_neu <- task1_ratings_melted2[ which(task1_ratings_melted2$cond=='lookneu' & task1_ratings_melted2$variable=='PainInt_Rating' ), ]

#Affect Rating
task1_ratings_melted2_affect_pain <- task1_ratings_melted2[ which(task1_ratings_melted2$cond=='pain' & task1_ratings_melted2$variable=='Affect_Rating' ), ]
task1_ratings_melted2_affect_neu <- task1_ratings_melted2[ which(task1_ratings_melted2$cond=='lookneu' & task1_ratings_melted2$variable=='Affect_Rating' ), ]

#Arousal Rating
task1_ratings_melted2_arousal_pain <- task1_ratings_melted2[ which(task1_ratings_melted2$cond=='pain' & task1_ratings_melted2$variable=='Arousal_Rating' ), ]
task1_ratings_melted2_arousal_neu <- task1_ratings_melted2[ which(task1_ratings_melted2$cond=='lookneu' & task1_ratings_melted2$variable=='Arousal_Rating' ), ]

#get mean for each subject in each condition/rating combination (25 ratings X 4)
#Pain Intensity Rating Mean
task1_painint_pain<-aggregate(task1_ratings_melted2_painint_pain[, 4], list(task1_ratings_melted2_painint_pain$SubjectID), mean)
task1_painint_neu<-aggregate(task1_ratings_melted2_painint_neu[, 4], list(task1_ratings_melted2_painint_neu$SubjectID), mean)

#Affect Rating Mean
task1_affect_pain<-aggregate(task1_ratings_melted2_affect_pain[, 4], list(task1_ratings_melted2_affect_pain$SubjectID), mean)
task1_affect_neu<-aggregate(task1_ratings_melted2_affect_neu[, 4], list(task1_ratings_melted2_affect_neu$SubjectID), mean)

#Arousal Rating Mean
task1_arousal_pain<-aggregate(task1_ratings_melted2_arousal_pain[, 4], list(task1_ratings_melted2_arousal_pain$SubjectID), mean)
task1_arousal_neu<-aggregate(task1_ratings_melted2_arousal_neu[, 4], list(task1_ratings_melted2_arousal_neu$SubjectID), mean)

#combine into one df
task1_ratings_mean_df<-cbind(task1_painint_pain,task1_painint_neu$x,task1_affect_pain$x,task1_affect_neu$x,task1_arousal_pain$x,task1_arousal_neu$x)
colnames(task1_ratings_mean_df)<-c("SubjectID","PainIntensityRating_pain","PainIntensityRating_neu","AffectRating_pain","AffectRating_neu","ArousalRating_pain","ArousalRating_neu")

#make long form
task1_ratings_mean_df_melted<-melt(task1_ratings_mean_df, id=c("SubjectID"))

#split varible into two columns, one for variable and one for condition
task1_ratings_mean_df_melted$variable<-as.factor(task1_ratings_mean_df_melted$variable)
task1_ratings_mean_df_melted2<-separate(data=task1_ratings_mean_df_melted,col=variable,into=c("variable","cond"),sep="\\_")

#rename observations in variable column to match long format with raw data
task1_ratings_mean_df_melted2$variable<-as.factor(task1_ratings_mean_df_melted2$variable)

task1_ratings_mean_df_melted3<-as.data.frame(revalue(task1_ratings_mean_df_melted2$variable, c("PainIntensityRating"="PainInt_Rating", "AffectRating"="Affect_Rating","ArousalRating"="Arousal_Rating")))
names(task1_ratings_mean_df_melted3)

#add back to previous df
task1_ratings_mean_df_melted4<-cbind(task1_ratings_mean_df_melted2$SubjectID,task1_ratings_mean_df_melted3,task1_ratings_mean_df_melted2$cond,task1_ratings_mean_df_melted2$value)
colnames(task1_ratings_mean_df_melted4)<-c("SubjectID","variable","cond","value")

#reorder cond in mean df
task1_ratings_mean_df_melted4$variable <- as.character(task1_ratings_mean_df_melted4$variable)
task1_ratings_mean_df_melted4$variable <- factor(task1_ratings_mean_df_melted4$variable, levels=unique(task1_ratings_mean_df_melted4$variable))

#-----------------------------------------

#Create DF of means by condition for each subject - task 2

#subsetting based on condition X variable
#Affect Rating
task2_ratings_melted2_affect_lookneg <- task2_ratings_melted2[ which(task2_ratings_melted2$cond=='lookneg' & task2_ratings_melted2$variable=='Affect_Rating' ), ]
task2_ratings_melted2_affect_lookneu <- task2_ratings_melted2[ which(task2_ratings_melted2$cond=='lookneu' & task2_ratings_melted2$variable=='Affect_Rating' ), ]
task2_ratings_melted2_affect_suppress <- task2_ratings_melted2[ which(task2_ratings_melted2$cond=='suppress' & task2_ratings_melted2$variable=='Affect_Rating' ), ]

#Arousal Rating
task2_ratings_melted2_arousal_lookneg <- task2_ratings_melted2[ which(task2_ratings_melted2$cond=='lookneg' & task2_ratings_melted2$variable=='Arousal_Rating' ), ]
task2_ratings_melted2_arousal_lookneu <- task2_ratings_melted2[ which(task2_ratings_melted2$cond=='lookneu' & task2_ratings_melted2$variable=='Arousal_Rating' ), ]
task2_ratings_melted2_arousal_suppress <- task2_ratings_melted2[ which(task2_ratings_melted2$cond=='suppress' & task2_ratings_melted2$variable=='Arousal_Rating' ), ]

#get mean for each subject in each condition/rating combination (25 ratings X 4)
#Affect Rating Mean
task2_affect_lookneg<-aggregate(task2_ratings_melted2_affect_lookneg[, 4], list(task2_ratings_melted2_affect_lookneg$SubjectID), mean)
task2_affect_lookneu<-aggregate(task2_ratings_melted2_affect_lookneu[, 4], list(task2_ratings_melted2_affect_lookneu$SubjectID), mean)
task2_affect_suppress<-aggregate(task2_ratings_melted2_affect_suppress[, 4], list(task2_ratings_melted2_affect_suppress$SubjectID), mean)

#Arousal Rating Mean
task2_arousal_lookneg<-aggregate(task2_ratings_melted2_arousal_lookneg[, 4], list(task2_ratings_melted2_arousal_lookneg$SubjectID), mean)
task2_arousal_lookneu<-aggregate(task2_ratings_melted2_arousal_lookneu[, 4], list(task2_ratings_melted2_arousal_lookneu$SubjectID), mean)
task2_arousal_suppress<-aggregate(task2_ratings_melted2_arousal_suppress[, 4], list(task2_ratings_melted2_arousal_suppress$SubjectID), mean)

#combine into one df
task2_ratings_mean_df<-cbind(task2_affect_lookneg,task2_affect_lookneu$x,task2_affect_suppress$x,task2_arousal_lookneg$x,task2_arousal_lookneu$x,task2_arousal_suppress$x)
colnames(task2_ratings_mean_df)<-c("SubjectID","AffectRating_lookneg","AffectRating_lookneu","AffectRating_suppress","ArousalRating_lookneg","ArousalRating_lookneu","ArousalRating_suppress")

#create column with negative conditions averaged together
task2_ratings_mean_df$AffectRating_allneg<-rowMeans(task2_ratings_mean_df[,c("AffectRating_lookneg", "AffectRating_suppress")], na.rm=F)

task2_ratings_mean_df$ArousalRating_allneg<-rowMeans(task2_ratings_mean_df[,c("ArousalRating_lookneg", "ArousalRating_suppress")], na.rm=F)

#make long form
task2_ratings_mean_df_melted<-melt(task2_ratings_mean_df, id=c("SubjectID"))

#split varible into two columns, one for variable and one for condition
task2_ratings_mean_df_melted$variable<-as.factor(task2_ratings_mean_df_melted$variable)
task2_ratings_mean_df_melted2<-separate(data=task2_ratings_mean_df_melted,col=variable,into=c("variable","cond"),sep="\\_")

#rename observations in variable column to match long format with raw data
task2_ratings_mean_df_melted2$variable<-as.factor(task2_ratings_mean_df_melted2$variable)

task2_ratings_mean_df_melted3<-as.data.frame(revalue(task2_ratings_mean_df_melted2$variable, c("AffectRating"="Affect_Rating","ArousalRating"="Arousal_Rating")))
names(task2_ratings_mean_df_melted3)

#add back to previous df
task2_ratings_mean_df_melted4<-cbind(task2_ratings_mean_df_melted2$SubjectID,task2_ratings_mean_df_melted3,task2_ratings_mean_df_melted2$cond,task2_ratings_mean_df_melted2$value)
colnames(task2_ratings_mean_df_melted4)<-c("SubjectID","variable","cond","value")

#separate allneg out into its own df
task2_ratings_mean_df_melted4_sepneg<-task2_ratings_mean_df_melted4[1:174,]
task2_ratings_mean_df_melted4_allneg<-task2_ratings_mean_df_melted4[c(30:58,117:145,175:232),]
#-----------------------------------------
#combine task1 and task2 ratings to compare between tasks

#rename columns to differentiate face from IAPS photos in variable column
colnames(task1_ratings_mean_df)[4] <- "AffectRatingFace_pain"
colnames(task2_ratings_mean_df)[2] <- "AffectRatingIAPS_lookneg"
colnames(task1_ratings_mean_df)[5] <- "AffectRatingFace_neu"
colnames(task2_ratings_mean_df)[3] <- "AffectRatingIAPS_lookneu"
colnames(task2_ratings_mean_df)[4] <- "AffectRatingIAPS_suppress"

colnames(task2_ratings_mean_df)[8] <- "AffectRatingIAPS_allneg"

#combine into one df
task1task2_meandf <- cbind(task1_ratings_mean_df,task2_ratings_mean_df)

#make columns numeric
task1task2_meandf[, c(9:16)] <- sapply(task1task2_meandf[, c(9:16)], as.numeric)

#get means
#IAPS - lookneg
mean(task1task2_meandf$AffectRatingIAPS_lookneg,na.rm=T)#3.52
sd(task1task2_meandf$AffectRatingIAPS_lookneg,na.rm=T)#

#IAPS - suppress
mean(task1task2_meandf$AffectRatingIAPS_suppress,na.rm=T)#3.7
sd(task1task2_meandf$AffectRatingIAPS_suppress,na.rm=T)#

#output as .csv for use with ROI analyses
#setwd("//datastore01.psy.miami.edu/Groups/ELosin_Lab/Research/Research_Projects/EAPSI_Beijing/Data_Files")
#write.csv(task1task2_meandf, file = "task1task2_meandf.csv")

#combine into melted form for plotting
task1task2_meandf_neg <- task1task2_meandf[,c(1,4,9)]
task1task2_meandf_neu <- task1task2_meandf[,c(1,5,10)]
task1task2_meandf_suppress <- task1task2_meandf[,c(1,4,11)]
task1task2_meandf_allneg <- task1task2_meandf[,c(1,15,16)]

#melt
task1task2_meandf_neg_melted<-melt(task1task2_meandf_neg, id=c("SubjectID"))
task1task2_meandf_neu_melted<-melt(task1task2_meandf_neu, id=c("SubjectID"))
task1task2_meandf_suppress_melted<-melt(task1task2_meandf_suppress, id=c("SubjectID"))
task1task2_meandf_allneg_melted<-melt(task1task2_meandf_allneg, id=c("SubjectID"))

#split variable column into cond and variable
task1task2_meandf_neg_melted2 <- as.data.frame(str_split_fixed(task1task2_meandf_neg_melted$variable, "_", 2))
task1task2_meandf_neu_melted2 <- as.data.frame(str_split_fixed(task1task2_meandf_neu_melted$variable, "_", 2))
task1task2_meandf_suppress_melted2 <- as.data.frame(str_split_fixed(task1task2_meandf_suppress_melted$variable, "_", 2))
task1task2_meandf_allneg_melted2 <- as.data.frame(str_split_fixed(task1task2_meandf_allneg_melted$variable, "_", 2))

#combine with subjectid and value column 
task1task2_meandf_neg_melted3 <- cbind(task1task2_meandf_neg_melted$SubjectID,task1task2_meandf_neg_melted2,task1task2_meandf_neg_melted$value)
task1task2_meandf_neu_melted3 <- cbind(task1task2_meandf_neu_melted$SubjectID,task1task2_meandf_neu_melted2,task1task2_meandf_neu_melted$value)
task1task2_meandf_suppress_melted3 <- cbind(task1task2_meandf_suppress_melted$SubjectID,task1task2_meandf_suppress_melted2,task1task2_meandf_suppress_melted$value)
task1task2_meandf_allneg_melted3 <- cbind(task1task2_meandf_allneg_melted$SubjectID,task1task2_meandf_allneg_melted2,task1task2_meandf_allneg_melted$value)

#rename columns
colnames(task1task2_meandf_neg_melted3)<-c("SubjectID","variable","cond","value")
colnames(task1task2_meandf_neu_melted3)<-c("SubjectID","variable","cond","value")
colnames(task1task2_meandf_suppress_melted3)<-c("SubjectID","variable","cond","value")
colnames(task1task2_meandf_allneg_melted3)<-c("SubjectID","variable","cond","value")

#-----------------------------------------
#paired t-tests to check if differences between conditions/tasks is significant

###TASK1
#pain rating: neutral vs. painful face
t.test(task1_ratings_mean_df$PainIntensityRating_neu,task1_ratings_mean_df$PainIntensityRating_pain,paired=T) 

#affect rating: neutral vs. painful face
t.test(task1_ratings_mean_df$AffectRatingFace_neu,task1_ratings_mean_df$AffectRatingFace_pain,paired=T) 

#arousal rating: neutral vs. painful face
t.test(task1_ratings_mean_df$ArousalRating_neu,task1_ratings_mean_df$ArousalRating_pain,paired=T) 

###TASK2
#affect rating: lookneu vs. lookneg
t.test(task2_ratings_mean_df$AffectRating_lookneu,task2_ratings_mean_df$AffectRating_lookneg,paired=T) 

#affect rating: lookneu vs. suppress
t.test(task2_ratings_mean_df$AffectRating_lookneu,task2_ratings_mean_df$AffectRating_suppress,paired=T) 

#affect rating: lookneu vs. all negative (lookneg averaged with suppressneg)
t.test(task2_ratings_mean_df$AffectRating_lookneu,task2_ratings_mean_df$AffectRating_allneg,paired=T) 

#affect rating: lookneg vs. suppress
t.test(task2_ratings_mean_df$AffectRating_lookneg,task2_ratings_mean_df$AffectRating_suppress,paired=T) 

#arousal rating: lookneu vs. lookneg
t.test(task2_ratings_mean_df$ArousalRating_lookneu,task2_ratings_mean_df$ArousalRating_lookneg,paired=T) 

#arousal rating:lookneu vs. suppress
t.test(task2_ratings_mean_df$ArousalRating_lookneu,task2_ratings_mean_df$ArousalRating_suppress,paired=T) 

#arousal rating: lookneg vs. suppress
t.test(task2_ratings_mean_df$ArousalRating_lookneg,task2_ratings_mean_df$ArousalRating_suppress,paired=T) 

#arousal rating: lookneu vs. all negative (lookneg averaged with suppressneg)
t.test(task2_ratings_mean_df$ArousalRating_lookneu,task2_ratings_mean_df$ArousalRating_allneg,paired=T) 

###TASK1 TASK2 COMPARISON
#affect rating: painful face vs. negative IAPS image
t.test(task1task2_meandf$AffectRatingFace_pain,task1task2_meandf$AffectRating_lookneg,paired=T) 

#affect rating: painful face vs. suppress IAPS image
t.test(task1task2_meandf$AffectRatingFace_pain,task1task2_meandf$AffectRating_suppress,paired=T) 

#affect rating: painful face vs. all negative IAPSP (lookneg averaged with suppressneg)
t.test(task1task2_meandf$AffectRatingFace_pain,task1task2_meandf$AffectRatingIAPS_allneg,paired=T) 

#affect rating: neutral face vs. neutral IAPS image
t.test(task1task2_meandf$AffectRatingFace_neu,task1task2_meandf$AffectRatingIAPS_lookneu,paired=T) 0.8727
#-----------------------------------------

#Plot Task 1

#modify df to get better labels for plotting
levels(task1_ratings_mean_df_melted4$cond)[levels(task1_ratings_mean_df_melted4$cond)=="neu"] <- "Neutral"
levels(task1_ratings_mean_df_melted4$cond)[levels(task1_ratings_mean_df_melted4$cond)=="pain"] <- "Negative"
names(task1_ratings_mean_df_melted4)[names(task1_ratings_mean_df_melted4)=="value"]  <- "Rating"
names(task1_ratings_mean_df_melted4)[names(task1_ratings_mean_df_melted4)=="cond"]  <- "Condition"

#reorder factor 
task1_ratings_mean_df_melted4$Condition <- factor(task1_ratings_mean_df_melted4$Condition, levels = c("Negative", "Neutral"))

myColors1 <- c("darkturquoise","salmon")
m <- ggplot(task1_ratings_mean_df_melted4, aes(factor(variable,labels=c("Pain Intensity","Valence","Arousal")), Rating,(colour=Condition))) + guides(fill=FALSE) 
n <- m + geom_boxplot(aes(colour=Condition),width = 0.5,outlier.shape=NA) + theme(legend.position="right") + labs(x = "\nPostScan Ratings (Face)")  
p <- n + geom_point(data=task1_ratings_mean_df_melted4,aes(colour=Condition),width=.25,position=position_jitterdodge(dodge.width = 0.53)) + stat_summary(aes(group=Condition),geom = "point", fun.y = mean, shape = 18, size = 4,col="gray34",position = position_dodge(width=.48))
x <- p + labs(colour='Condition')
x + scale_colour_manual(values = myColors1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#-----------------------------------------
#Plot Task 2


#modify df to get better labels
levels(task2_ratings_mean_df_melted4_sepneg$cond)[levels(task2_ratings_mean_df_melted4_sepneg$cond)=="lookneu"] <- "Look Neutral"
levels(task2_ratings_mean_df_melted4_sepneg$cond)[levels(task2_ratings_mean_df_melted4_sepneg$cond)=="lookneg"] <- "Look Negative"
levels(task2_ratings_mean_df_melted4_sepneg$cond)[levels(task2_ratings_mean_df_melted4_sepneg$cond)=="suppress"] <- "Suppress Negative"
names(task2_ratings_mean_df_melted4_sepneg)[names(task2_ratings_mean_df_melted4_sepneg)=="value"]  <- "Rating"

#subset rows
task2_ratings_mean_df_melted4_sepneg2 <- task2_ratings_mean_df_melted4_sepneg[c(2,5,7,8),]

# Rename the column and the values in the factor
levels(task2_ratings_mean_df_melted4_sepneg2$cond)[levels(task2_ratings_mean_df_melted4_sepneg2$cond)=="allneg"] <- "Negative"
levels(task2_ratings_mean_df_melted4_sepneg2$cond)[levels(task2_ratings_mean_df_melted4_sepneg2$cond)=="Look Neutral"] <- "Neutral"

#plot 
m <- ggplot(task2_ratings_mean_df_melted4_sepneg2, aes(factor(variable,labels=c("Valence","Arousal")), Rating,(colour=cond)),alpha=.06) + guides(fill=FALSE)
n <- m + geom_boxplot(aes(colour=cond),width = 0.5,outlier.shape=NA) + theme(legend.position="right") + labs(x = "\nPostScan Ratings (IAPS)") 
p <- n + geom_point(data=task2_ratings_mean_df_melted4_sepneg2,aes(colour=cond),alpha=.8,width=.25,position=position_jitterdodge(dodge.width = 0.53))+ stat_summary(aes(group=cond),geom = "point", fun.y = mean, shape = 18, size = 4,col="gray34",position = position_dodge(width=.48))
x <- p + labs(colour='Condition') 
x + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#-----------------------------------------
#Plot both tasks to compare stimuli (paper 2)

#affect rating: painful face vs. negative IAPS
myColors2 <- c("salmon", "darkturquoise")
m <- ggplot(task1task2_meandf_neg_melted3, aes(factor(variable,labels=c("Pain Face", "Neg IAPS")), value,(colour=cond)),alpha=.06) + guides(fill=FALSE)
n <- m + geom_boxplot(aes(colour=variable),width = 0.2,outlier.shape=NA) + theme(legend.position="none") + labs(x = "\nNegative Affect Rating Face vs. IAPS Task") 
p <- n + geom_point(data=task1task2_meandf_neg_melted3,aes(colour=variable),alpha=.8,width=.25,position=position_jitterdodge(dodge.width = 0.53))+ scale_color_manual(values=myColors2) + scale_color_hue(labels = c("pain","lookneg")) + stat_summary(aes(group=variable),geom = "point", fun.y = mean, shape = 18, size = 4,col="gray34",position = position_dodge(width=.48))
x <- p + labs(colour='condition') 
x + scale_colour_manual(values = myColors2) + coord_cartesian(ylim = c(0, 6)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#affect rating: neutral face vs. neutral IAPS
myColors2 <- c("salmon", "darkturquoise")
m <- ggplot(task1task2_meandf_neu_melted3, aes(factor(variable,labels=c("Neu Face", "Neu IAPS")), value,(colour=cond)),alpha=.06) + guides(fill=FALSE)
n <- m + geom_boxplot(aes(colour=variable),width = 0.2,outlier.shape=NA) + theme(legend.position="none") + labs(x = "\nNeutral Affect Rating Face vs. IAPS Task") 
p <- n + geom_point(data=task1task2_meandf_neu_melted3,aes(colour=variable),alpha=.8,width=.25,position=position_jitterdodge(dodge.width = 0.53))+ scale_color_manual(values=myColors2) + scale_color_hue(labels = c("neutral","lookneu")) + stat_summary(aes(group=variable),geom = "point", fun.y = mean, shape = 18, size = 4,col="gray34",position = position_dodge(width=.48))
x <- p + labs(colour='condition') 
x + scale_colour_manual(values = myColors2) + coord_cartesian(ylim = c(0, 8)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#affect rating: pain vs. suppress IAPS
myColors2 <- c("salmon", "darkturquoise")
m <- ggplot(task1task2_meandf_suppress_melted3, aes(factor(variable,labels=c("Pain Face", "SupNeg IAPS")), value,(colour=cond)),alpha=.06) + guides(fill=FALSE)
n <- m + geom_boxplot(aes(colour=variable),width = 0.2,outlier.shape=NA) + theme(legend.position="none") + labs(x = "\nSuppress Affect Rating Face vs. IAPS Task") 
p <- n + geom_point(data=task1task2_meandf_suppress_melted3,aes(colour=variable),alpha=.8,width=.25,position=position_jitterdodge(dodge.width = 0.53))+ scale_color_manual(values=myColors2) + scale_color_hue(labels = c("neutral","lookneu")) + stat_summary(aes(group=variable),geom = "point", fun.y = mean, shape = 18, size = 4,col="gray34",position = position_dodge(width=.48))
x <- p + labs(colour='condition') 
x + scale_colour_manual(values = myColors2) + coord_cartesian(ylim = c(0, 6)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))

#-----------------------------------------
#Testing if valence and arousal values for IAPS pictures are significantly different
IAPS_values<-read.csv(file="//datastore01.psy.miami.edu/Groups/ELosin_Lab/Research/Research_Projects/EAPSI_Beijing/Stimuli/IAPS/IAPS_Stimuli_Values/NSF_EAPSI_IAPS_Stimuli_Values_Rinput.csv",header=T)

#test if negative images in lookneg and suppress conditions are sig diff
t.test(IAPS_values$Valence_LookNeg,IAPS_values$Valence_Suppress)#p = .96
t.test(IAPS_values$Arousal_LookNeg,IAPS_values$Arousal_Suppress)#p = .20

#test if negative images in lookneg and suppress conditions are sig diff from neutral images in lookneu condition
t.test(IAPS_values$Valence_LookNeg,IAPS_values$Valence_LookNeutral)#p<.001
t.test(IAPS_values$Valence_Suppress,IAPS_values$Valence_LookNeutral)#p<.001
t.test(IAPS_values$Arousal_LookNeg,IAPS_values$Arousal_LookNeutral)#p<.001
t.test(IAPS_values$Arousal_Suppress,IAPS_values$Arousal_LookNeutral)#p<.001

#-----------------------------------------
#SummarySE Function

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
    library(plyr)

    # Measure var on left, idvar + between vars on right of formula.
    data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
     .fun = function(xx, col, na.rm) {
        c(subjMean = mean(xx[,col], na.rm=na.rm))
      },
      measurevar,
      na.rm
    )

    # Put the subject means with original data
    data <- merge(data, data.subjMean)

    # Get the normalized data in a new column
    measureNormedVar <- paste(measurevar, "_norm", sep="")
    data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
                               mean(data[,measurevar], na.rm=na.rm)

    # Remove this subject mean column
    data$subjMean <- NULL

    return(data)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {

  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
    FUN=is.factor, FUN.VALUE=logical(1))

  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }

  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)

  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL

  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)

  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")

  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)

  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                           FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )

  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor

  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}
