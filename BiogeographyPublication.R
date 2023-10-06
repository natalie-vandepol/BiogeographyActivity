setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
library(ggplot2)

# IMPORT SS18 DATA
SS18_obj4.raw <- read.csv("MMG425_SS18_Obj4.csv", fill = F)
SS18_data<-SS18_obj4.raw[1:58,1:10]
colnames(SS18_data)<-c("LastName", "FirstName", "F22_M17", "F23_M18a", "F24_M18b",
                       "F25_M21a", "F26_M21b", "F27_M21c", "F28_M22", "F52_M18c")
SS18_points <- c(1,1,1,1,1,1,2,4)


# IMPORT SS19 Data
SS19_data.raw <- read.table("MMG425_SS19_MIdTerm2019_Natalie.txt",header=T, sep="\t", 
                            stringsAsFactors = F, na.strings="NA")
SS19_data <- SS19_data.raw[4:nrow(SS19_data.raw),2:13]
colnames(SS19_data)<-c("Name", "F22_M17", "F23_M18a", "F24_M18b", "F52_M18c", "M19a",
                       "M19b", "M20", "F25_M21a", "F26_M21b", "F27_M21c", "F28_M22")

SS19_data$F22_M17 <-as.numeric(SS19_data$F22_M17)
SS19_data$F23_M18a<-as.numeric(SS19_data$F23_M18a)
SS19_data$F24_M18b<-as.numeric(SS19_data$F24_M18b)
SS19_data$F52_M18c<-as.numeric(SS19_data$F52_M18c)
SS19_data$M19a    <-as.numeric(SS19_data$M19a)
SS19_data$M19b    <-as.numeric(SS19_data$M19b)
SS19_data$F25_M21a<-as.numeric(SS19_data$F25_M21a)
SS19_data$F26_M21b<-as.numeric(SS19_data$F26_M21b)
SS19_data$F27_M21c<-as.numeric(SS19_data$F27_M21c)

SS19_points <- as.numeric(SS19_data.raw[3,3:13])



# Visualize SS19 data
SS19_mean_by_Q <- colMeans(SS19_data[2:ncol(SS19_data)], na.rm = T)
SS19_mean_by_Q
SS19_percent_by_Q <- 100*SS19_mean_by_Q/SS19_points
SS19_percent_by_Q
barplot(SS19_percent_by_Q, main="SS19 Objective 4 Questions", ylab="Percent Score",
        xlab="Question #", ylim=c(0,100))

SS19_data_percent<-SS19_data
SS19_data_percent[,2:12]<-100*SS19_data[,2:12]
for (i in 2:ncol(SS19_data_percent)){
  SS19_data_percent[,i]<-SS19_data_percent[,i]/SS19_points[i-1]
}

SS19_data.long<-gather(SS19_data_percent[,2:12])
SS19_data.long$q_num <- factor(SS19_data.long$key, levels=c("F22_M17", "F23_M18a", "F24_M18b", "F52_M18c",
                                                                 "F25_M21a", "F26_M21b", "F27_M21c","F28_M22",
                                                                 "M19a", "M19b", "M20"))

ggplot(SS19_data.long) + 
  geom_histogram(aes(x=value, y=..count../63), binwidth = 20)+
  ylim(0,1)+
  facet_grid(~q_num) +
  ggtitle("SS19 Exam Scores")+
  xlab("Percentage of Possible Points")+
  ylab("Proportion of Scores")

# Visualize SS18 Data
SS18_mean_by_Q <- colMeans(SS18_data[3:ncol(SS18_data)], na.rm = T)
SS18_mean_by_Q
SS18_percent_by_Q <- 100*SS18_mean_by_Q/SS18_points
SS18_percent_by_Q
barplot(SS18_percent_by_Q, main="SS18 Objective 4 Questions", ylab="Percent Score",
        xlab="Question #", ylim=c(0,100))

SS18_data_percent <- SS18_data
SS18_data_percent[,3:10] <- 100*SS18_data_percent[,3:10]
for (j in 3:10){
  SS18_data_percent[,j]<-SS18_data_percent[,j]/SS18_points[j-2]
}

SS18_data.long <- gather(SS18_data_percent[,3:10])
SS18_data.long$q_num <- as.factor(SS18_data.long$key)


# IMPORT FS19 DATA
FS19_data.long <- read.csv("FS19_long.csv", fill = F, header=T)
FS19_points <- c(1,1,1,2,1,1,1,0.5,0.5,1,2)



SS19_data.long$Year<-rep("SS19",length(SS19_data.long$key))
SS18_data.long$Year<-rep("SS18",length(SS18_data.long$key))

library(dplyr)

merged.long<- full_join(FS19_data.long, SS19_data.long)
merged.long<- full_join(merged.long, SS18_data.long)
merged.long$Year<-as.factor(merged.long$Year)
merged.long$q_num<-factor(merged.long$q_num, 
                             levels=c("F22_M17","F23_M18a","F24_M18b","F52_M18c",
                                      "F25_M21a","F26_M21b","F27_M21c","F28_M22",
                                      "M19a","M19b","M20"))
merged.long$q_code<-factor(merged.long$q_num, levels=c("F1_M1","F2_M2a","F3_M2b","F4_M2c", "F5_M3a","F6_M3b","F7_M3c","F8_M4","M5a","M5b","M6"))
code<- rbind(c("F22_M17","F23_M18a","F24_M18b","F52_M18c","F25_M21a","F26_M21b","F27_M21c","F28_M22","M19a","M19b","M20"),
             c("F1_M1","F2_M2a","F3_M2b","F4_M2c", "F5_M3a","F6_M3b","F7_M3c","F8_M4","M5a","M5b","M6"))

for (n in 1:length(merged.long$q_code)){
  index<-which(code==merged.long$q_num[n], arr.ind=TRUE)
  merged.long$q_code[n]<-code[2,index[1,2]]
}

#install.packages("ggpubr")
#install.packages("ggsignif")
library(ggpubr)
library(ggsignif)


#ggbarplot(merged.long, x= "q_num", y= "value", add = "mean_se", color= "black",
#          fill = "Year", position=position_dodge(width=0.75), width=0.7,
#          order=c("F22_M17", "F23_M18a", "F24_M18b", "F52_M18c", "F25_M21a", "F26_M21b", 
#                  "F27_M21c", "F28_M22", "M19a", "M19b", "M20"))+
#  stat_compare_means(aes(group = Year), label = "p.format", label.y = 101)+
#  stat_compare_means(aes(group = Year), label = "p.signif", label.y = 105)+
#  theme(legend.title = element_blank())+ 
#  xlab("Question\n(SS18exam#_SS19exam#)")+
#  ylab("Mean Percent Score")
#  scale_fill_manual(values = c("#0072B2", "#F0E442"))
  


anno_df<-compare_means(value~Year, data=merged.long, group.by="q_num") %>%
  mutate(y_pos = 100, p.adj = format.pval(p.adj, digits = 2))

ggbarplot(merged.long, x= "Year", y= "value", facet.by="q_code", add = "mean_se", 
          color= "black", fill="Year", width=0.7, ylim=c(0,140),
          order=c("SS18","SS19","FS19"))+
  stat_compare_means(comparisons = list(c("SS18", "SS19"),  
                                        c("FS19", "SS19"),
                                        c("SS18", "FS19")), 
                     label = "p.format")+
  theme(legend.title = element_blank())+ 
  xlab("")+
  ylab("Mean Percent Score")+
  scale_fill_manual(values = c("#666666", "#006633", "#3399ff"))




write.csv(N_Q_yr, "N_Q_yr.csv")



ggplot(SS18_data.long) + 
  geom_histogram(aes(x=value, y=..count../58), binwidth = 20)+
  ylim(0,1)+
  facet_grid(~q_num) +
  ggtitle("SS18 Exam Scores")+
  xlab("Percentage of Possible Points")+
  ylab("Proportion of Scores")

ggplot(SS19_data.long) + 
  geom_histogram(aes(x=value, y=..count../58), binwidth = 20)+
  ylim(0,1)+
  facet_grid(~q_num) +
  ggtitle("SS19 Exam Scores")+
  xlab("Percentage of Possible Points")+
  ylab("Proportion of Scores")


# STATISTICAL TESTS BY QUESTION
t.test(SS19_data_percent$F22_M17, SS18_data_percent$F22_M17, mu=0,
       alternative="greater", conf.level=0.95)
t.test(SS19_data_percent$F23_M18a, SS18_data_percent$F23_M18a, mu=0,
       alternative="greater", conf.level=0.95)
t.test(SS19_data_percent$F24_M18b, SS18_data_percent$F24_M18b, mu=0,
       alternative="greater", conf.level=0.95)
t.test(SS19_data_percent$F25_M21a, SS18_data_percent$F25_M21a, mu=0,
       alternative="greater", conf.level=0.95)
t.test(SS19_data_percent$F26_M21b, SS18_data_percent$F26_M21b, mu=0,
       alternative="greater", conf.level=0.95)
t.test(SS19_data_percent$F27_M21c, SS18_data_percent$F27_M21c, mu=0,
       alternative="greater", conf.level=0.95)
t.test(SS19_data_percent$F28_M22, SS18_data_percent$F28_M22, mu=0,
       alternative="greater", conf.level=0.95)
t.test(SS19_data_percent$F52_M18c, SS18_data_percent$F52_M18c, mu=0,
       alternative="less", conf.level=0.95)


N_Q_yr <- rbind(c(0,0,0,0,0,0,0,0), c(0,0,0,0,0,0,0,0))
colnames(N_Q_yr) <- c("F22_M17", "F23_M18a", "F24_M18b", "F25_M21a", "F26_M21b",
                      "F27_M21c", "F28_M22", "F52_M18c")
rownames(N_Q_yr) <- c("SS18", "SS19")
SS18_Nstudent <- nrow(SS18_data)
SS19_Nstudent <- nrow(SS19_data)
N_Q_yr[,1]<-c(SS18_Nstudent-sum(is.na(SS18_data$F22_M17)),SS19_Nstudent-sum(is.na(SS19_data$F22_M17)))
N_Q_yr[,2]<-c(SS18_Nstudent-sum(is.na(SS18_data$F23_M18a)),SS19_Nstudent-sum(is.na(SS19_data$F23_M18a)))
N_Q_yr[,3]<-c(SS18_Nstudent-sum(is.na(SS18_data$F24_M18b)),SS19_Nstudent-sum(is.na(SS19_data$F24_M18b)))
N_Q_yr[,4]<-c(SS18_Nstudent-sum(is.na(SS18_data$F25_M21a)),SS19_Nstudent-sum(is.na(SS19_data$F25_M21a)))
N_Q_yr[,5]<-c(SS18_Nstudent-sum(is.na(SS18_data$F26_M21b)),SS19_Nstudent-sum(is.na(SS19_data$F26_M21b)))
N_Q_yr[,6]<-c(SS18_Nstudent-sum(is.na(SS18_data$F27_M21c)),SS19_Nstudent-sum(is.na(SS19_data$F27_M21c)))
N_Q_yr[,7]<-c(SS18_Nstudent-sum(is.na(SS18_data$F28_M22)),SS19_Nstudent-sum(is.na(SS19_data$F28_M22)))
N_Q_yr[,8]<-c(SS18_Nstudent-sum(is.na(SS18_data$F52_M18c)),SS19_Nstudent-sum(is.na(SS19_data$F52_M18c)))
N_Q_yr


# Not part of biogeography, needs to be recoded for SS19_data.raw
#ggplot(SS19_data.long[SS19_data.long$variable=="Q46",])+
#  geom_histogram(aes(x=value, y=..count../63), binwidth = 20)+
#  ylim(0,1)+
#  ggtitle("SS19 - Q46")+
#  xlab("Percentage of Possible Points")+
#  ylab("Proportion of Scores")



















# Jen's Code




data=read.table("MMG425_MIdTerm2019_ForR.txt", header=TRUE, row.names=1,sep="\t")
map=read.table("Map_MMG425_MIdTerm2019_ForR.txt", header=TRUE, sep = "\t")
score.a=colSums(data[1:58,])

#distribution of Ashley's exam questions
hist(score.a, main="Exam scores on Shade midterm questions \n MMG425 2019", xlab="Points awarded (out of 105 possible)")
summary(score.a)
#max score is 92, so adjust upwared +13 points to bring best student to 100%

#distribution of Ned's exam questions
score.w=as.numeric(data[59,])
hist(score.w, main="Exam scores on Walker midterm questions \n MMG425 2019", xlab="Points awarded (out of 11 possible)")
summary(score.w)

#distribution of combined scores - Ashley and Ned
score.c=colSums(data)
hist(score.c, main="Exam scores on Shade + Walker midterm questions \n MMG425 2019", xlab="Points awarded (out of 116 possible)")
summary(score.c)

#to adjust score, add 13 to take the highest performer to 100%
score.adj=score.c+13
score.adj=as.numeric(score.adj)
hist(score.adj, main="Exam scores on adjusted (+13) \n Shade + Walker midterm questions \n MMG425 2019", xlab="Points awarded (out of 116 possible)")
summary(score.adj)
sum(score.adj <= 64)

#which students failed?
colnames(data)[score.adj <= 64]
#[1] "Andinorios..Thesla.Gabriela"  "Chase..Austin.David"         
#[3] "Faski..Daniel.Stanley"        "Gray..Kathrine.Ann"          
#[5] "Monge..Nick"                  "Peake..Alexandrea.Morgan"    
#[7] "Peng..Shuya"                  "Schmidt..Rebecca.Janet"      
#[9] "Szczepanski..Elizabeth.Maria"

#adjust grades
library(ProfessR)
D1 = do.grades(score.adj, tit="MMG425 Midterm - adjHigh")
#select the lower-most blue lines from L to R = adjHigh
#select the lower most red lines from L to R = adjLow
checkgrades(D1)
#adjLow= D1$scor 
adjHigh=D1$scor


#must proceed wtih AdjHigh, otherwise several students will fail
OutScores=rbind(D1$grades, D1$lett, D1$scor)
colnames(OutScores)=colnames(data)

data.adj<-data
point.value <- c(map[,3])
for(row in 1:length(point.value)){
  data.adj[row,]<-100*data[row,]/point.value[row]
}
rows_want<-c(17,18,19,20,21,22,23,45,53,46)
data.t<-t(data.adj[rows_want,])
data.melt<-melt(data.t)[,2:3]

ggplot(data.melt) + 
  geom_histogram(aes(x=value, y=20*..density..), binwidth = 20)+
  facet_grid(~Var2) +
  ggtitle("SS19")+
  xlab("Percentage of Possible Points")+
  ylab("Proportion of Scores")


