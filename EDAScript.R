library(openxlsx)
library(ggplot2)
library(d)
data<-read.xlsx("Data.xlsx")
View(data)

#OS is left eye OD is right eye:
#https://www.webmd.com/eye-health/how-read-eye-glass-prescription#:~:text=When%20you%20look%20at%20your,means%20something%20involving%20both%20eyes.

#Quotes from client email about variable importance:
"I think the factors that are important to consider when examining patients that had IOP increases after SLT and ultimately had a subsequent surgical intervention or patients that while not requiring a surgery had subsequent decline on their HVF or OCT parameters would be: Age at SLT, Pre-VA, Pre-Refraction, Latest Pre-IOP,  CCT, Pre-HVF MD and PSD, and Pre-OCT Mean, Sup, and Inf Thicknesses. Secondary factors would be: Ethnicity (might not have a varied enough sample), Gender, Eye, and PMH (diabetes and hypertension). Not sure I have enough patients with each Glaucoma Dx to really see differences. "


d<-duplicated(data$PatientID2)
data<-cbind(data, d)

ggplot(data, aes(x=Age.at.SLT, fill=d))+geom_bar()+  theme(legend.position = "none")+ggtitle("Initial SLT Ages with Duplicate Patients")

ggplot(data, aes(x=Ethnicity, fill=Gender))+geom_bar()+ggtitle("Ethnicity of Patients") 

ggplot(data, aes(x=Eye))+geom_bar()+ggtitle("Ethnicity of Patients") 


#add in labels for counts?

ggplot(data, aes(x=`Latest.Pre-IOP`, y=`Post-IOP-5yr`))+geom_point()

ggplot(data, aes(x=`Latest.Pre-IOP`, y=`Pre-VA`))+geom_point()

#Chose to just use variables where all the data was present, but there's plenty missing

r<-lm(`Post-IOP-5yr`~Age.at.SLT+`Pre-VA`+`Latest.Pre-IOP`+Gender+Eye+Ethnicity, data=data)
summary(r)



