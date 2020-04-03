#VIOLIN BOXPLOTS WITH SUBJECT LINES AND FACET

#------------------------------------
#Read in packages
library(reshape2)
library(ggplot2)
library(ggpubr)

#------------------------------------
##Read in data

#read in PINES values from IAPS Task cope6 (LNEU>Baseline) and cope7 (LNEG>Baseline) 
ca_pines_values_iaps_cope6_cope7 <- read.csv(file="//datastore01.psy.miami.edu/Groups/ELosin_Lab/Research/Research_Projects/EAPSI_Miami/Data_Files/vps-pines_analyzed/UM_PINES_task2_cope6_cope7.csv", header=T, sep=",")

#create subject variable
ca_pines_values_iaps_cope6_cope7$Subject<-c(31,32,33,34,35,36,37,38,39,40,41,42,43,44,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61)

#make Subject factor
ca_pines_values_iaps_cope6_cope7$Subject<-as.factor(ca_pines_values_iaps_cope6_cope7$Subject)

#read in PINES values from IAPS cope7 (LNEG>Baseline) and cope8 (eSUP>Baseline) 
ca_pines_values_iaps_cope7_cope8 <- read.csv(file="//datastore01.psy.miami.edu/Groups/ELosin_Lab/Research/Research_Projects/EAPSI_Miami/Data_Files/vps-pines_analyzed/UM_PINES_task2_cope7_cope8.csv", header=T, sep=",")

#create subject variable
ca_pines_values_iaps_cope7_cope8$Subject<-c(31,32,33,34,35,36,37,38,39,40,41,42,43,44,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61)

#make Subject factor
ca_pines_values_iaps_cope7_cope8$Subject<-as.factor(ca_pines_values_iaps_cope7_cope8$Subject)

#------------------------------------
##Combine into 1 plot - PINES IAPS Task

#combine into 1 plot 
ca_pines_iaps_all1<-merge(ca_pines_values_iaps_cope6_cope7,ca_pines_values_iaps_cope7_cope8,id="Subject")

#add another LNEG column
ca_pines_iaps_all2<-cbind(ca_pines_iaps_all1$PINES_task2_cope7,ca_pines_iaps_all1)

#reorder columns
ca_pines_iaps_all3<-ca_pines_iaps_all2[,c(3,4,1,2,5)]

#rename columns
colnames(ca_pines_iaps_all3)<-c("Subject","LNEU","LNEG1","LNEG2","eSUP")

#melt
ca_pines_iaps_all3_melted<-melt(ca_pines_iaps_all3,id="Subject")

#create Part variable for easier faceting
ca_pines_iaps_all3_melted$Part = ifelse(grepl("(LNEU|LNEG1)",ca_pines_iaps_all3_melted$variable),"LNEU        LNEG",
                                         ifelse(grepl("(LNEG2|eSUP)",ca_pines_iaps_all3_melted$variable),"LNEG         eSUP",0))

#reorder levels of Part factor
ca_pines_iaps_all3_melted$Part <- factor(ca_pines_iaps_all3_melted$Part, levels = c("LNEU        LNEG", "LNEG         eSUP"))

#create Treatment variable for easier coloring
ca_pines_iaps_all3_melted$Treatment = ifelse(grepl("(LNEU|LNEG2)",ca_pines_iaps_all3_melted$variable),"Control",
                                              ifelse(grepl("(LNEG1|eSUP)",ca_pines_iaps_all3_melted$variable),"Treatment",0))

#------------------------------------
##Plot with ggplot
m <- ggplot(ca_pines_iaps_all3_melted, aes(x=Treatment, y=value))  + ylim(-10,20)
h <- m + geom_violin(aes(fill = Treatment), alpha=.7,trim = FALSE,size=.01)
n <- h + geom_line(aes(group = Subject),col="gray78") 
o <- n + geom_boxplot(width = 0.15,size=.3)
p <- o + stat_summary(aes(group=Treatment),geom = "point", fun.y = mean, shape = 18, size = 4,col="black",position = position_dodge(width=.48)) 
q <- p + theme_classic() + theme(legend.position = "none", text = element_text(size=20)) + ylab("CA PINES - IAPS") + geom_hline(yintercept=0, linetype="dashed", color = "black")+  scale_fill_manual(values = c("cornflowerblue", "sandybrown"))+scale_color_manual(values = c("cornflowerblue", "sandybrown")) + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

#plot with facet grid and signficance labels
plot1 <- q  + facet_grid(~Part,switch="x")+theme(
  strip.background = element_rect(
    color="white", fill="white", size=1.5, linetype="solid"
  )
) + stat_compare_means(label = "p.signif",paired=TRUE,method="t.test", label.y = 16,label.x = 1.5,size=7,hide.ns=F,symnum.args = list(cutpoints = c(0,0.001, 0.01, 0.05, 1), symbols = c("***", "**", "*", "ns")))

plot1

