library(Boruta)
library(readr)
library(ggplot2)

data <- read_csv("9.8data.csv")
data <- as.data.frame(data)
data<-na.omit(data)
str(data)

data$Sex <- factor(data$Sex)
data$Marital.status <- factor(data$Marital.status)
data$Race <- factor(data$Race)
data$income <- factor(data$income)
data$TG <- factor(data$TG)
data$NG <- factor(data$NG)
data$Radiation <- factor(data$Radiation)
data$Chemotherapy <- factor(data$Chemotherapy)
data$Surgery <- factor(data$Surgery)
data$Bone <- factor(data$Bone)
data$Brain <- factor(data$Brain)
data$Lung <- factor(data$Lung)
str(data)


set.seed(123)
boruta_obj<-Boruta(Lung ~Age+Sex+Marital.status+Race+income+TG+NG+Radiation+Chemotherapy+Surgery+Bone+Brain,
                   data=data,
                   doTrace=0,
                   ntree=500,
                   pValue=0.001)

print(boruta_obj)

opar <- par(no.readonly=TRUE) 
par(mar=c(7,4,3,1)) 
ori_plot<-plot(boruta_obj,las=3,xlab='',ylab='Importance: Z-score',main='Key variables ')
legend(2,140,c('Shadow','Rejected','Tendensive','Confirmed'),fill=c('blue','red','yellow','green'))

formula<-getConfirmedFormula(boruta_obj)
formula

attStats(boruta_obj)