---
title: "EAPSI Timing Files"
author: "Steven R. Anderson"
output: html_notebook
---

rm(list=ls()) #removes all the variables from workspace

library(dplyr)

#formula to select run_number
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#enter subjects here
IDs<-c("0001","0002","0003","0004","0005","0006","0007","0008","0009","0010","0011","0012","0013","0014",
       "0015","0016","0017","0018","0019","0020","0021","0022","0023","0024","0025","0026","0027","0028","0029",
       "0030")

#runs
FuncRuns<-c("run1","run2","run3")

#loop through runs per subject
for (ID in IDs){
  
  for (FuncRun in FuncRuns){

#read in presentation logfile
original<- read.csv(paste("//datastore01.psy.miami.edu/Groups/ELosin_Lab/Research/Research_Projects/EAPSI_Beijing/Data_Files/Presentation_Logfiles_CSV/",(ID),"/",(ID),"-EAPSI_Scan_Task1_7_9_17_CH_",(FuncRun),".csv",sep = ""))

master<-original

#Select initial time (=fmri_trigger)
master<-master[-1,]

#define start_time using time as fmri trigger
  i<-as.numeric(match('fmri_trigger',master$Code))
  start_time<-as.numeric(master[i+1, c("Time")])
  discarded_TR_duration<-100000

#Get Run Number
run<-filter(master, grepl("run_",Code))
run<- as.character(run$Code[1])
run<-as.numeric(substrRight(run, 1))
#run<-paste0("0", run)

#Select Subject number
subject<- as.numeric(master[1,c("Subject")])
#subject<-str_pad(subject, 4, pad = "0")


#Remove rows not needed (e.g. response lines, bp_lines)
#Empty rows
master<-master[which(master$letter.str. != ''),]

#rows with answers
#remove all the rows with response in Code
master<-filter(master, !grepl("Response",Code))
  
#Only image trials
image<-filter(master, grepl("stim",Code))


#Subtract (time - initial time) and divide by 10,000 to get initial time
for (i in 1:nrow(master)){
  master[i, c("Time_sec")]<- ((master[i, c("Time")] - start_time)-discarded_TR_duration)/10000
}


#Get duration: (cell n+1 - Cell n)
for (i in 1:nrow(master)){
  master[i, c("Duration_sec")]<- (master[(i+1), c("Time_sec")] -master[(i), c("Time_sec")])
}

#loop through each element and parse
stim_list<-c()

#stim by level
levels_a<- c( "_lookneg_" )
names_a<- c("stim")
    
for (a in 1:length(levels_a)){
  temp_level_a<- (levels_a[a])
  level_df_a<- filter(master, grepl(temp_level_a,Code))
  level_name_a<-gsub(".$", "", temp_level_a) 
  for (k in 1:length(names_a)){
    temp_a<-data.frame(matrix(nrow=2, ncol=4))
    stim_type_a<-(names_a[k])
    #print(paste(stim_type, temp_level))
    for (i in 1:nrow(level_df_a)){
      if (grepl(stim_type_a, level_df_a[i,c("Code")])==TRUE){
        temp_a[i,1]<- as.character(level_df_a[i, c("Code")])
        temp_a[i,2]<- level_df_a[i, c("Time_sec")]
        temp_a[i,3]<- level_df_a[i, c("Duration_sec")]
        temp_a[i,4]<-1
      }
      temp_a = temp_a %>% na.omit() #remove empty rows
      new_temp_a<- temp_a[,2:4]
      assign(paste(stim_type_a,level_name_a,sep=""),new_temp_a)
      
    }
    #stim_list<- c(stim_list, (paste(stim_type_a,level_name_a,sep="")))
  }
}

#stim by level
levels_b<- c( "_lookneu_" )
names_b<- c("stim")
    
for (a in 1:length(levels_b)){
  temp_level_b<- (levels_b[a])
  level_df_b<- filter(master, grepl(temp_level_b,Code))
  level_name_b<-gsub(".$", "", temp_level_b) 
  for (k in 1:length(names_b)){
    temp_b<-data.frame(matrix(nrow=2, ncol=4))
    stim_type_b<-(names_b[k])
    #print(paste(stim_type, temp_level))
    for (i in 1:nrow(level_df_b)){
      if (grepl(stim_type_b, level_df_b[i,c("Code")])==TRUE){
        temp_b[i,1]<- as.character(level_df_b[i, c("Code")])
        temp_b[i,2]<- level_df_b[i, c("Time_sec")]
        temp_b[i,3]<- level_df_b[i, c("Duration_sec")]
        temp_b[i,4]<-(-1)
      }
      temp_b = temp_b %>% na.omit() #remove empty rows
      new_temp_b<- temp_b[,2:4]
      assign(paste(stim_type_b,level_name_b,sep=""),new_temp_b)
      
    }
    #stim_list<- c(stim_list, (paste(stim_type_b,level_name_b,sep="")))
  }
}

levels_c<- c( "_lookneg_" )
names_c<- c("stim")
    
for (a in 1:length(levels_c)){
  temp_level_c<- (levels_c[a])
  level_df_c<- filter(master, grepl(temp_level_c,Code))
  level_name_c<-gsub(".$", "", temp_level_c) 
  for (k in 1:length(names_c)){
    temp_c<-data.frame(matrix(nrow=2, ncol=4))
    stim_type_c<-(names_c[k])
    #print(paste(stim_type, temp_level))
    for (i in 1:nrow(level_df_c)){
      if (grepl(stim_type_c, level_df_c[i,c("Code")])==TRUE){
        temp_c[i,1]<- as.character(level_df_c[i, c("Code")])
        temp_c[i,2]<- level_df_c[i, c("Time_sec")]
        temp_c[i,3]<- level_df_c[i, c("Duration_sec")]
        temp_c[i,4]<-(-1)
      }
      temp_c = temp_c %>% na.omit() #remove empty rows
      new_temp_c<- temp_c[,2:4]
      assign(paste(stim_type_c,level_name_c,sep=""),new_temp_c)
      
    }
    #stim_list<- c(stim_list, (paste(stim_type_c,level_name_c,sep="")))
  }
}

levels_d<- c( "_suppress_" )
names_d<- c("stim")
    
for (a in 1:length(levels_d)){
  temp_level_d<- (levels_d[a])
  level_df_d<- filter(master, grepl(temp_level_d,Code))
  level_name_d<-gsub(".$", "", temp_level_d) 
  for (k in 1:length(names_d)){
    temp_d<-data.frame(matrix(nrow=2, ncol=4))
    stim_type_d<-(names_d[k])
    #print(paste(stim_type, temp_level))
    for (i in 1:nrow(level_df_d)){
      if (grepl(stim_type_d, level_df_d[i,c("Code")])==TRUE){
        temp_d[i,1]<- as.character(level_df_d[i, c("Code")])
        temp_d[i,2]<- level_df_d[i, c("Time_sec")]
        temp_d[i,3]<- level_df_d[i, c("Duration_sec")]
        temp_d[i,4]<-1
      }
      temp_d = temp_d %>% na.omit() #remove empty rows
      new_temp_d<- temp_d[,2:4]
      assign(paste(stim_type_d,level_name_d,sep=""),new_temp_d)
      
    }
    #stim_list<- c(stim_list, (paste(stim_type_d,level_name_d,sep="")))
  }
}

#stim by level
levels_e<- c( "_lookneu_" )
names_e<- c("stim")
    
for (a in 1:length(levels_e)){
  temp_level_e<- (levels_e[a])
  level_df_e<- filter(master, grepl(temp_level_e,Code))
  level_name_e<-gsub(".$", "", temp_level_e) 
  for (k in 1:length(names_e)){
    temp_e<-data.frame(matrix(nrow=2, ncol=4))
    stim_type_e<-(names_e[k])
    #print(paste(stim_type, temp_level))
    for (i in 1:nrow(level_df_e)){
      if (grepl(stim_type_e, level_df_e[i,c("Code")])==TRUE){
        temp_e[i,1]<- as.character(level_df_e[i, c("Code")])
        temp_e[i,2]<- level_df_e[i, c("Time_sec")]
        temp_e[i,3]<- level_df_e[i, c("Duration_sec")]
        temp_e[i,4]<-1
      }
      temp_e = temp_e %>% na.omit() #remove empty rows
      new_temp_e<- temp_e[,2:4]
      assign(paste(stim_type_e,level_name_e,sep=""),new_temp_e)
      
    }
    #stim_list<- c(stim_list, (paste(stim_type_e,level_name_e,sep="")))
  }
}

#stim by level
levels_f<- c( "_suppress_" )
names_f<- c("stim")
    
for (a in 1:length(levels_f)){
  temp_level_f<- (levels_f[a])
  level_df_f<- filter(master, grepl(temp_level_f,Code))
  level_name_f<-gsub(".$", "", temp_level_f) 
  for (k in 1:length(names_f)){
    temp_f<-data.frame(matrix(nrow=2, ncol=4))
    stim_type_f<-(names_f[k])
    #print(paste(stim_type, temp_level))
    for (i in 1:nrow(level_df_f)){
      if (grepl(stim_type_f, level_df_f[i,c("Code")])==TRUE){
        temp_f[i,1]<- as.character(level_df_f[i, c("Code")])
        temp_f[i,2]<- level_df_f[i, c("Time_sec")]
        temp_f[i,3]<- level_df_f[i, c("Duration_sec")]
        temp_f[i,4]<-(-1)
      }
      temp_f = temp_f %>% na.omit() #remove empty rows
      new_temp_f<- temp_f[,2:4]
      assign(paste(stim_type_f,level_name_f,sep=""),new_temp_f)
      
    }
    #stim_list<- c(stim_list, (paste(stim_type_e,level_name_e,sep="")))
  }
}

#PPI 

#rbind two conditions of interest
#new_temp_a = LNeg +1
#new_temp_b = LNeu -1
#new_temp_c = LNeg -1
#new_temp_d = Suppress +1
#new_temp_e = LNeu +1
#new_temp_f = Suppress - 1

#Neurosynth: A-B (comparing LNEG vs. Suppress)
#PPI_neurosynth1 <- rbind.data.frame(new_temp_a,new_temp_f)
#colnames(PPI_neurosynth1)<-c("X2","X3","X4")

#Neurosynth: A+B (comparing LNEG vs. Suppress)
#PPI_neurosynth2 <- rbind.data.frame(new_temp_a,new_temp_d)
#colnames(PPI_neurosynth2)<-c("X2","X3","X4")

#Neurosynth: B-A (comparing eSUP vs. LNEG)
PPI_neurosynth3 <- rbind.data.frame(new_temp_d,new_temp_c)
colnames(PPI_neurosynth3)<-c("X2","X3","X4")

#Neurosynth: B+A (comparing eSUP vs. LNEG)
PPI_neurosynth4 <- rbind.data.frame(new_temp_d,new_temp_a)
colnames(PPI_neurosynth4)<-c("X2","X3","X4")

#add to stim_list
stim_list<- c(stim_list, "PPI_neurosynth3","PPI_neurosynth4")

#Saving all data files using loop
    
    #Setting up Directory names
    mainDir <- "//datastore01.psy.miami.edu/Groups/ELosin_Lab/Research/Research_Projects/EAPSI_Beijing/Data_Files/Timing_Files/PPI/neurosynth"
    subDir <- paste("sub-", (subject), sep="")
    
    #Creating folder for specific subject
    if (file.exists(subDir)){
      setwd(file.path(mainDir, subDir))
    } else {
      dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
      setwd(file.path(mainDir, subDir))
    }
    
    #saving
    for (i in 1:length(stim_list)){
      write.table(get(stim_list[i]), paste(mainDir, "/",subDir,"/", "sub-",(subject), "_","task-01","_", "run-0",(run),"_",(stim_list)[i], "_","events",".tsv", sep = ""), col.names = FALSE, row.names = FALSE, sep = "\t", quote = FALSE)
    } 

    #end loop
    
  }}

