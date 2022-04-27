library(openxlsx)
library(ggplot2)
library(dplyr)
data<-read.xlsx("Data.xlsx")
View(data)

#OS is left eye OD is right eye:
#https://www.webmd.com/eye-health/how-read-eye-glass-prescription#:~:text=When%20you%20look%20at%20your,means%20something%20involving%20both%20eyes.

#Quotes from client email about variable importance:
# "I think the factors that are important to consider when examining patients that had IOP increases after SLT and ultimately had a subsequent surgical intervention or patients that while not requiring a surgery had subsequent decline on their HVF or OCT parameters would be: Age at SLT, Pre-VA, Pre-Refraction, Latest Pre-IOP,  CCT, Pre-HVF MD and PSD, and Pre-OCT Mean, Sup, and Inf Thicknesses. Secondary factors would be: Ethnicity (might not have a varied enough sample), Gender, Eye, and PMH (diabetes and hypertension). Not sure I have enough patients with each Glaucoma Dx to really see differences. "


DuplicationIndicator<-duplicated(data$PatientID2)
data<-cbind(data, d)

data$PatientID2<-data$Patient.ID
test<-data%>%group_by(PatientID2)%>%summarize(count=n())
data<-left_join(data, test, by="PatientID2")
d2<-distinct(data, PatientID2, count, Age.at.SLT, Gender, Ethnicity)
d2$count<-ifelse(d2$count==2, 1, 0)

ggplot(d2, aes(x=Age.at.SLT, fill=as.factor(count)))+geom_bar()+ggtitle("Initial SLT Ages")+labs(fill="Duplicate Eye Flag")+  scale_fill_manual(values=c("1"="blue", "0"="gray")) 

ggplot(d2, aes(x=Ethnicity, fill=Gender))+geom_bar()+ggtitle("Ethnicity and Gender of Patients")

ggplot(data, aes(x=Eye))+geom_bar(fill="purple")+ggtitle("Left and Right Eye Counts")

ggplot(data, aes(x=`Latest.Pre-IOP`, y=`Post-IOP-5yr`))+geom_point(color="#4B9094")+ggtitle("Latest Pre OP vs Post OP 5 yrs")

ggplot(data, aes(x=`Pre-VA`, y=`Post-IOP-5yr`))+geom_point(color="#784B94")+ggtitle("Pre-VA vs Post OP 5 yrs")

#Chose to just use variables where all the data was present, but there's plenty missing

r<-lm(`Post-IOP-5yr`~Age.at.SLT+`Pre-VA`+`Latest.Pre-IOP`+Gender+Eye+Ethnicity, data=data)
summary(r)



