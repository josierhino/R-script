Chapter 6:
#Correlation and working with data 
library(boot); 
library(ggm); 
library(ggplot2); 
library(Hmisc);
library(polycor)

#Data entry 
adverts<-c(5,4,4,6,8)
packets<-c(8,9,10,13,15)
advertData<-data.frame(adverts, packets)
 
scatter<-ggplot(advertData, aes(adverts, packets))
scatter + geom_point()
scatter + geom_point(size = 3) + labs(x = "Adverts", y = "Packets") + scale_y_continuous(limits = c(0, 15), breaks = 0:15) + scale_x_continuous(limits = c(0, 9), breaks = 0:9) 

#6.5.3 - Load exam data 
examData = read.delim("Exam Anxiety.dat", header = TRUE)

cor(examData, use = "complete.obs", method = "pearson")

levels(examData$Gender)<-c(0,1)
View(examData)

cor(examData [, sapply(examData, is.numeric)], use = "complete.obs", method = "pearson")
View(examData)

#Correlation of anxiety and exam  
cor(examData$Exam, examData$Anxiety, use="complete.obs", method = "pearson")
cor(examData$Exam, examData$Anxiety, use = "complete.obs", method = "kendall")
cor(examData$Exam, examData$Anxiety, use = "pairwise.complete.obs", method = "kendall")


examMatrix<-as.matrix(examData[, c("Exam", "Anxiety", "Revise")])
Hmisc::rcorr(examMatrix)
cor.test(examData$Exam, examData$Anxiety, method = "pearson")
cor.test(examData$Exam, examData$Revise, method = "pearson")
cor.test(examData$Revise, examData$Anxiety, method = "pearson")


examData2 <- examData[, c("Exam", "Anxiety", "Revise")]
cor(examData2) 
examMatrix2<-as.matrix(examData2)
Hmisc::rcorr(examMatrix2)
cor.test(examData2$Exam, examData2$Anxiety, method = "pearson")
cor(examData2)^2
cor(examData2)^2 * 100


liarData = read.delim("The Biggest Liar.dat", header = TRUE)
cor(liarData$Position, liarData$Creativity, method = "spearman")

liarMatrix<-as.matrix(liarData[, c("Position", "Creativity")])
Hmisc::rcorr(liarMatrix)
cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "spearman")
cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "spearman", exact = FALSE)

#6.5.6 - Kendall's tau 
cor(liarData$Position, liarData$Creativity, method = "kendall")
cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "kendall")

#6.5.7 - Bootstrapping our correlations
#creating function - bootstrapping with i
bootTau<-function(liarData,i)cor(liarData$Position[i],liarData$Creativity[i], use = "complete.obs", method = "kendall")
boot_kendall<-boot(liarData, bootTau, 2000)
boot_kendall
boot.ci(boot_kendall)

#6.5.8 - biserial (continuous) and point-biserial (discrete)
catData = read.csv("pbcorr.csv", header = TRUE)
catData 
cor(catData$time, catData$gender, method = "pearson") 
cor.test(catData$time, catData$gender, method = "pearson")
cor.test(catData$time, catData$recode, method = "pearson")
 
catFrequencies<-table(catData$gender)
prop.table(catFrequencies)

polyserial(catData$time, catData$gender)

#6.6.2 - Partial correlation
pcor(c("Exam", "Anxiety", "Revise"), var(examData2)) 
pc<-pcor(c("Exam", "Anxiety", "Revise"), var(examData2))
pc^2 
pcor.test(pc, 1, 103)