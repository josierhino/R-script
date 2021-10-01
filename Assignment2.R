#Chapter 4 - Graphs
#set working directory setwd("C:/Users/Chang/Downloads/IS372")
#install and load ggplo2#
library(ggplot2)

#Use theme instead of "opts" - opts no longer used

#load FB data - id,NPQC_R_Total, Rating_Type, Rating
facebookData<-read.delim("FacebookNarcissism.dat", header = TRUE)
#create object "graph" that specifies plot based on FB Dataframe, x axis (NPCO_R_Total) and y axis (Rating)
graph<-ggplot(facebookData,aes(NPQC_R_Total, Rating))
#add visual elements - this case is circles
graph + geom_point()
# changes dots to triangles
graph + geom_point(shape = 17)
# changes the size
graph + geom_point(size = 6)
# changes both -- not in the book
graph + geom_point(shape = 17, size = 6)
#changes the graph with different types of ratings in different colors
graph + geom_point(aes(colour = Rating_Type))
#use jitter to avoid overplotting
graph + geom_point(aes(colour = Rating_Type), position = "jitter")
#changes the graph with different types of ratings in different shapes
graph + geom_point(aes(shape = Rating_Type), position = "jitter")
#4.5.1 simple scatterplots
#create examData dataframe based on exam anxiety data
examData<-read.delim("Exam Anxiety.dat", header = TRUE)
#create scatter plot object anxiety - x axis, exam - y axis
scatter<-ggplot(examData,aes(Anxiety, Exam))
#add VE - dots
scatter + geom_point()
#add labels for x and y axes
scatter + geom_point() + labs(x="Exam Anxiety", y = "Exam Performance %")
#add curved line - "smoother"
scatter + geom_point() + geom_smooth() +labs(x="Exam Anxiety", y = "Exam Performance %")
#adds linear model fit to line and changes color to red
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red") +labs(x="Exam Anxiety", y = "Exam Performance %")
#switches off confidence interval se=f means "standard error = false"
scatter + geom_point()+geom_smooth(method ="lm", colour = "Red", se = F) +labs(x="Exam Anxiety", y = "Exam Performance %")
#confidence interval is blue and somewhat transparent - 0.1, ranges from 0 to 1
scatter + geom_point()+geom_smooth(method = "lm", alpha = 0.1, fill = "Blue") + labs(x="Exam Anxiety", y = "Exam Performance %")
#4.5.3 grouped scatterplot - initiate plot object - gende ar colour aesthetic
scatter <-ggplot(examData, aes(Anxiety, Exam, colour = Gender))
#need to add visual elements based on regression lines
scatter + geom_point() + geom_smooth(method="lm")
#improve the graph by coloring confidence intervals differently
scatter + geom_point() +geom_smooth(method="lm",aes(fill=Gender),alpha = 0.1)
#add lables
scatter +geom_point()+geom_smooth(method="lm", aes(fill=Gender),alpha =0.1) +labs(x="Exam Anxiety", y = "Exam Performance %", colour ="Gender")
#4.6 Histograms
#create dataframe based on downloadfestival data
festivalData<-read.delim("DownloadFestival.dat",header = TRUE)
#create plot object - again use theme, not opts
festivalHistogram <-ggplot(festivalData,aes(day1)) + theme(legend.position = "none")
#add graphical layers - histogram geom
festivalHistogram +geom_histogram()
#change width of bins to present the histogram clearly
festivalHistogram +geom_histogram(binwidth = 0.4)
#add labels
festivalHistogram +geom_histogram(binwidth = 0.4) + labs(x="Hygiene (Day 1 of Festival)", y = "Frequency")
#4.7 Boxplots
#create boxplot object
festivalBoxplot<-ggplot(festivalData,aes(gender,day1))
#add layers, boxplot and labels - notice outlier
festivalBoxplot + geom_boxplot() +labs(x= "Gender", y = "Hygiene (Day 1 of Festival)")
#orders data to identify outlier
festivalData<-festivalData[order(festivalData$day1),]
#display data
festivalData
View(festivalData)
#switch to datset with no outlier starting p 147 - recreate dataframe
festivalData<-read.delim("DownloadFestival(No Outlier).dat",header = TRUE)
#create boxplot object
festivalBoxplot<-ggplot(festivalData,aes(gender,day1))
#add layers, boxplot and labels
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")
#4.8 Density plots
#create density plot object
Density <-ggplot(festivalData, aes(day1))
#add layer
density +geom_density()
#add layer and labels
density+geom_density() + labs(x= "Hygiene (Day 1 of Festival)", y = "Density Estimate")
#4.9 Graphing means
#create chickFlick dataframe
chickFlick<- read.delim("ChickFlick.dat",header = TRUE)
#create bar graph object
bar<-ggplot(chickFlick,aes(film, arousal))
#we are plotting means not raw data must use stat summary - uses Hmisc package - load package from packages tab
#use stat_summary function, fun.y = mean is the mean of th y variable, use geom bar, bars white, outline is black
bar + stat_summary(fun.y = mean,geom = "bar", fill = "white", colour = "Black")
#add error bars
bar + stat_summary(fun.y = mean,geom = "bar", fill = "white", colour = "Black") + stat_summary(fun.data=mean_cl_normal,geom="pointrange")
#add labels
bar + stat_summary(fun = mean,geom = "bar", fill = "white", colour = "Black") + stat_summary(fun.data=mean_cl_normal,geom="pointrange") +labs(x="film", y = "mean Arousal")
#4.9.1.2 Bar chart for multiple independent variables
#bar chart object based on chickflick dataframe
bar<-ggplot(chickFlick,aes(film,arousal,fill =gender))
#create layers, dodge used to make bars side by side
bar+stat_summary(fun=mean, geom="bar",position= "dodge")
#add error bars
bar+stat_summary(fun=mean, geom="bar",position= "dodge") +stat_summary(fun.data=mean_cl_normal,geom="errorbar", position_dodge(width=.90), width = 0.2)
#reduce width of error bar =.2 and add labels - used two lines for this command
bar+stat_summary(fun=mean, geom="bar",position= "dodge") +stat_summary(fun.data=mean_cl_normal,geom="errorbar", position_dodge(width=.90), 
width = 0.2) +labs(x="Film", y = "Mean Arousal", fill = "Gender")
#set up graph as two different plots for males and females using facets
#create bar object
bar<-ggplot(chickFlick,aes(film,arousal,fill=film))
#add layers based on mean
bar + stat_summary(fun=mean, geom= "bar")
#add error bar
bar + stat_summary(fun=mean, geom="bar") +stat_summary(fun.data=mean_cl_normal,geom="errorbar", width=0.2)
#use facet to get different plots for men and women
bar+stat_summary(fun=mean,geom="bar") +stat_summary(fun.data=mean_cl_normal,geom="errorbar", width = 0.2) +facet_wrap(~gender)
#add labels and remove unnecessary legend - again use two lines of code
bar+stat_summary(fun=mean,geom="bar") +stat_summary(fun.data=mean_cl_normal,geom="errorbar", width = 0.2) +facet_wrap(~gender)+
labs(x="film", y="mean Arousal") + theme(legend.position = "none")
#create hiccups Data dataframe
hiccupsData<-read.delim("Hiccups.dat",header=TRUE)
#restructures the data = read this section carefully!!!
hiccups<-stack(hiccupsData)
#names function to add names
names(hiccups)<-c("Hiccups", "Intervention")
#view restructured data
hiccups
#Don't need this code - R knows you are using a factor
hiccups$Intervention_Factor <- factor(hiccups$Intervention,levels = hiccups$Intervention)
#create line graph object
line<-ggplot(hiccups, aes(Intervention, Hiccups))
#add layer, points
line + stat_summary(fun=mean,geom= "point")
#add a line between points
line +stat_summary(fun=mean, geom="point") + stat_summary(fun=mean,geom="line",aes(group=1))
#make the line dashed and blue
line +stat_summary(fun=mean, geom="point") + stat_summary(fun=mean,geom="line",aes(group=1), colour="Blue", linetype="dashed")
#add error bars
line +stat_summary(fun=mean, geom="point") + stat_summary(fun=mean,geom="line",aes(group=1), colour="Blue", linetype="dashed")+
stat_summary(fun.data=mean_cl_boot,geom="errorbar")
#reduce width of error bars
line +stat_summary(fun=mean, geom="point") + stat_summary(fun=mean,geom="line",aes(group=1), colour="Blue", linetype="dashed")+
  stat_summary(fun.data=mean_cl_boot,geom="errorbar", width = 0.2)
#make the bars red
line +stat_summary(fun=mean, geom="point") + stat_summary(fun=mean,geom="line",aes(group=1), colour="Blue", linetype="dashed")+
  stat_summary(fun.data=mean_cl_boot,geom="errorbar", width = 0.2, colour="Red")
#add labels
line +stat_summary(fun=mean, geom="point") + stat_summary(fun=mean,geom="line",aes(group=1), colour="Blue", linetype="dashed")+
  stat_summary(fun.data=mean_cl_boot,geom="errorbar", width = 0.2, colour="Red") +labs(x="Intervention", y = "Mean Number of Hiccups")
#4.9.2.2 Line graphs for several independent variables
#create textData dataframe
textData<-read.delim("TextMessages.dat",header = TRUE)
#view datagrame
textData
#self test on p159 - necessary for line graph examples
#add id variable for participants across time variable
textData$id=row(textData[1])
#creates a new dataframe textMessages based on textData DF
textMessages<-melt(textData,id=c("id", "Group"), measured = c("Baseline", "Six_Months"))
#Names the variables in the textMessages Datframe
names(textMessages)<-c("id", "Group", "Time", "Grammar_Score")
#Creates new time ID variable
textMessages$Time<-factor(textMessages$Time,labels =c("Baseline", "6 Months"))
#displays new textMessages dataframe
print(textMessages)
#uses the new dataframe to create line graph object
line<-ggplot(textMessages,aes(Time,Grammar_Score,colour=Group))
#adds layer - point
line+stat_summary(fun=mean,geom="point")
#adds line between points
line+stat_summary(fun=mean,geom="point") +stat_summary(fun=mean,geom="line",aes(group=Group))
#adds error bars and lables
line+stat_summary(fun=mean,geom="point") +stat_summary(fun=mean,geom="line",aes(group=Group))+
stat_summary(fun.data=mean_cl_boot,geom= "errorbar", width=0.2) +labs(x="Time",y="Mean Grammar Score", colour = "Group")


