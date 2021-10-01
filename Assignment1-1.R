# Chapter 3 Example Exercises

#Using RStudio
#Set working Directory - can use Session tab, write code, etc. For me - setwd (C:Users/chang/Downloads/IS372")
#Install and load packages for Chapter 3
#intall.packages (foreign) - once installed can be selected from packages tab
#Skipping R Commander examples

#adding simple data manually
metallicaNames<-c("lars","James","Kirk","Rob")
metallicaAges<-c(47,47,48,46)
#creating new dataframe from names and ages objects
metallica<-data.frame(Name = metallicaNames, Age = metallicaAges)
#view new data frame
metallica
#view Age variable
metallica$Age
#add new childAge variable
metallica$childAge<-c(12, 12, 4, 6)
#view dataset with childAge added
metallica
#view the names of the variables -- see also environment tab
names(metallica)
#add new variable fatherhoodAge based on age - childage variables
metallica$fatherhoodAge<-metallica$Age - metallica$childAge
#view with new fatherhoodAge Variable
metallica
#enter new data
name<-c("Ben", "Martin", "Andy", "Paul", "Graham", "Carina", "Karina", "Doug", "Mark", "Zoe")
#be careful with as.Date function, need exact format
birth_date<-as.Date(c("1977-07-03", "1969-05-24", "1973-06-21", "1970-07-16", "1940-10-10", "1983-11-05", "1987-10-08", "1989-09-16", "1973-05-20", "1984-11-12"))
#create coding variable 1 for lecturer, 2 for student
job<-c(1,1,1,1,1,2,2,2,2,2)
#we could also use the rep() function - repeat 1 five times and repeat 2 five times
job<-c(rep(1, 5), rep(2, 5))
#several ways to create factor -- below is one example from book
job<-gl(2, 5, labels = c("Lecturer", "Student"))
#view job levels
levels(job)
#add data for friends, alcohol, income, and neurotic variables
friends<-c(5,2,0,4,1,10,12,15,12,17)
alcohol<-c(10,15,20,5,30,25,20,16,17,18)
income<-c(20000,40000,35000,22000,50000,5000,100,3000,10000,10)
neurotic<-c(10,17,14,13,21,7,13,9,14,13)
#next create dataframe for the variables created
lecturerData<-data.frame(name,birth_date,job,friends,alcohol,income,neurotic)
#view the new frame
lecturerData
#another way to data frame -- sort, etc.
View (lecturerData)
#--Note: lots of methods for manipulating data outside of R, Excel, SPSS, etc. ---
#importing tab delimited data from a .dat file
lecturerData<-read.delim("Lecturer Data.dat", header = TRUE)
#view data
lecturerData
#again can use other methods
View(lecturerData)
#reading data directly from SPSS files
lecturerData<-read.spss("LecturerData.sav", use.value.labels = TRUE, to.data.frame = TRUE)
#view dataframe, notice birth_date has not been imported correctly... p 100 explaination)
#if values are specifed in SPSS data will be imported with lables -- in this case lecturer and student rather than 1 and 2
lecturerData
#we can also write data to save -- here metallica dataframe to a .csv file
write.csv(metallica, "Metallica Data.csv")
#and here as a text file
write.table(metallica, "Metallica Data.txt", sep = "\t", row.names = FALSE)
#3.9.1 Manipulating data
#selecting subsets of data by creating new dataframe -- variables(columns)
lecturerPersonality <-lecturerData[, c("friends", "alcohol", "neurotic")]
#view new dataframe
lecturerPersonality
#selecting subsets of data by creating new dataframe -- cases(rows) ---notice "Lecturer" use 1 if using .dat file
lecturerOnly <-subset(lecturerData, job=="Lecturer")
#view dataframe cases
lecturerOnly
#subset based on columns and rows
alcoholPersonality <-subset(lecturerData, alcohol>10, select = c("friends", "alcohol", "neurotic"))
#view subset
alcoholPersonality
#creating matrices because some functions do not work with data frames
alcoholPersonalityMatrix <-as.matrix(alcoholPersonality)
#subsets and matrices
alcoholPersonalityMatrix <-as.matrix(lecturerData[alcohol > 10, c("friends", "alcohol", "neurotic")])
#view matrix
alcoholPersonalityMatrix
#3.9.4 Reshaping data
#creating satisfactionData dataframe from Honeymoon Period data
satisfactionData = read.delim("Honeymoon Period.dat", header = TRUE)
#view dataframe - each row represents a person
satisfactionData
#restacking only life satisfaction scores -- for simple data okay
satisfactionStacked<-stack(satisfactionData, select = c("Satisfaction_Base", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"))
#view stacked data
satisfactionStacked
# unstacked examples in book
install.packages("reshape")
library(reshape)
install.packages("reshape2")
library(reshape2)
#for more complex data -- dataframe, id = variables that do not vary over time, measured - variables that do vary over time or are repeated
restructureData<-melt(satisfactionData, id = c("Person", "Gender"), measured = c("SatisfactionBase", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"))
#view restructured Data
restructureData
#or
View(restructureData)
#changing back to wide format - not varying variables to left of - and varying to the right
wideData<-cast(restructureData, Person+Gender - variable, value = "value")
#view original data
wideData
