getwd()
setwd("D://ADESOYE'S DOCUMENTS//NTU SCHOOL WORK//STATISTICAL DATA ANALYSIS_VIS//STATS COURSEWORK")
diabetes_dataset<-read.csv("diabetes_2.csv")
diabetes_dataset
attach(diabetes_dataset)
?tidyverse
?dplyr
browseVignettes(package = "dplyr")
library(tidyverse)
install.packages("gapminder")
diabetes_dataset2<-select(Glucose,BloodPressure,SkinThickness,Insulin,BMI,DiabetesPedigreeFunction,Age,Outcome)
library(dplyr)
#Drop the "Pregnancies" column
diabetes_dataset2<-diabetes_dataset%>%select(-Pregnancies)
diabetes_dataset2
diabetes_dataset2<-diabetes_dataset2%>%mutate_at(c('Glucose','BloodPressure','SkinThickness','Insulin','BMI','DiabetesPedigreeFunction','Age'),~na_if(.,0))
attach(diabetes_dataset2)
getwd()
setwd("D://ADESOYE'S DOCUMENTS//NTU SCHOOL WORK//STATISTICAL DATA ANALYSIS_VIS//STATS COURSEWORK")
library(tidyverse)
diabetes_dataset2%>%min(Age)
View(diabetes_dataset2)
dir()
getwd()
diabetes_dataset2
View(diabetes_dataset2)
diabetes_dataset2<-read.csv("diabetes_dataset2.csv")
getwd()
setwd("D://ADESOYE'S DOCUMENTS//NTU SCHOOL WORK//STATISTICAL DATA ANALYSIS_VIS//STATS COURSEWORK")
diabetes_dataset<-read.csv("diabetes_2.csv")
diabetes_dataset<-diabetes_dataset%>%select(-Pregnancies)
diabetes_dataset<-diabetes_dataset%>%mutate_at(c('Glucose','BloodPressure','SkinThickness','Insulin','BMI','DiabetesPedigreeFunction','Age'),~na_if(.,0))
write.csv(diabetes_dataset, "D:\\ADESOYE'S DOCUMENTS\\NTU SCHOOL WORK\\STATISTICAL DATA ANALYSIS_VIS\\STATS COURSEWORK\\diabetes_dataset.csv", row.names=FALSE)
diabetes_dataset<-read.csv("diabetes_dataset.csv")
View(diabetes_dataset)
attach(diabetes_dataset)
min(Age)
max(Age)
#Group the patients into age brackets
deciles<-seq(20,90,by=10)
age_cuts<-cut(Age, breaks = deciles)
age_cuts
#Create a new column called "AgeRange" to categorize the ages into groups
diabetes_dataset$AgeRange<-rep(0,768) #Number of cases = 768
diabetes_dataset$AgeRange[which(age_cuts==levels(age_cuts)[1])] <- 1
diabetes_dataset$AgeRange[which(age_cuts==levels(age_cuts)[2])] <- 2
diabetes_dataset$AgeRange[which(age_cuts==levels(age_cuts)[3])] <- 3
diabetes_dataset$AgeRange[which(age_cuts==levels(age_cuts)[4])] <- 4
diabetes_dataset$AgeRange[which(age_cuts==levels(age_cuts)[5])] <- 5
diabetes_dataset$AgeRange[which(age_cuts==levels(age_cuts)[6])] <- 6
diabetes_dataset$AgeRange[which(age_cuts==levels(age_cuts)[7])] <- 7
View(diabetes_dataset)
diabetes_dataset<-diabetes_dataset%>%select(-AgeRange)
diabetes_dataset
summary(diabetes_dataset)
View(summary(diabetes_dataset))
plot(Glucose)
ggplot(data = diabetes_dataset, mapping = aes(x = Glucose))+ geom_histogram()
ggplot(data = diabetes_dataset, mapping = aes(x = BloodPressure))+ geom_histogram()
ggplot(data = diabetes_dataset, mapping = aes(x = SkinThickness))+ geom_bar()
ggplot(data = diabetes_dataset, mapping = aes(Insulin))+ geom_histogram()
ggplot(data = diabetes_dataset, mapping = aes(BMI))+ geom_histogram()
ggplot(data = diabetes_dataset, mapping = aes(DiabetesPedigreeFunction))+ geom_histogram()
ggplot(data = diabetes_dataset, mapping = aes(Age))+ geom_bar()
ggplot(data = diabetes_dataset, mapping = aes(Outcome))+ geom_bar()
summary(Glucose)
summary(BloodPressure)
summary(SkinThickness)
summary(Insulin)
summary(BMI)
summary(DiabetesPedigreeFunction)
summary(Age)
summary(Outcome)
write.csv(diabetes_dataset, "D:\\ADESOYE'S DOCUMENTS\\NTU SCHOOL WORK\\STATISTICAL DATA ANALYSIS_VIS\\STATS COURSEWORK\\diabetes_dataset.csv", row.names=FALSE)
diabetes_dataset_2<-diabetes_dataset%>%drop_na()
View(diabetes_dataset_2)
attach(diabetes_dataset_2)
ks.test(Glucose,"pnorm", mean = mean(Glucose), sd = sd(Glucose))
write.csv(diabetes_dataset, "D:\\ADESOYE'S DOCUMENTS\\NTU SCHOOL WORK\\STATISTICAL DATA ANALYSIS_VIS\\STATS COURSEWORK\\diabetes_dataset.csv", row.names=FALSE)
