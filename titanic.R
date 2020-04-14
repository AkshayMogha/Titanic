
#setting working directory

setwd("C:/Users/Akshay Rajput/Downloads/titanic")

#loading raw data 
train<-read.csv("train.csv",header=TRUE,stringsAsFactors = FALSE)
test<-read.csv("test.csv",header=TRUE,stringsAsFactors = FALSE)

#adding a survived variable to test
test_survived<-data.frame(Survived=rep("None",nrow(test)),test[,])

#combining train and test dataset
titanic<-rbind(train,test_survived)


#structure of dataset
str(titanic)



#checking for duplicate names in titanic having train and test
duplicate.names<-as.character(titanic[which(duplicated(as.character(titanic$Name))),"Name"])
duplicate.names

#checking whether we have same observations again
titanic[which(titanic$Name %in% duplicate.names ),]

#No, Passengers have different PassengerId

#Filling NA values in Variables
which(is.na(titanic$PassengerId))
which(is.na(titanic$Survived))
which(is.na(titanic$Pclass))
which(is.na(titanic$Name))
which(is.na(titanic$Sex))
which(is.na(titanic$Age))
which(is.na(titanic$SibSp))
which(is.na(titanic$Parch))
which(is.na(titanic$Ticket))
which(is.na(titanic$Fare))
which(is.na(titanic$Cabin))
which(is.na(titanic$Embarked))

#we  have two variables with na values i.e age and fare

#for fare
FareNA<-mean(titanic[which(!is.na(titanic$Fare)),"Fare"])
titanic[which(is.na(titanic$Fare)),"Fare"]<-FareNA

library(stringr)

#for age
titanicNA<-titanic[which(!is.na(titanic$Age)),]

miss<-titanicNA[which(str_detect(titanicNA$Name,"Miss.")),]
missNA<-round(mean(miss$Age))
missNA

mrs<-titanicNA[which(str_detect(titanicNA$Name,"Mrs.")),]
mrsNA<-round(mean(mrs$Age))
mrsNA

master<-titanicNA[which(str_detect(titanicNA$Name,"Master.")),]
masterNA<-round(mean(master$Age))
masterNA

mr<-titanicNA[which(str_detect(titanicNA$Name,"Mr.")),]
mrNA<-round(mean(mr$Age))
mrNA


NAage<-titanic[which(is.na(titanic$Age)),]

NAagemiss<-NAage[which(str_detect(NAage$Name,"Miss.")),"PassengerId"]
NAagemrs<-NAage[which(str_detect(NAage$Name,"Mrs.")),"PassengerId"]
NAagemaster<-NAage[which(str_detect(NAage$Name,"Master.")),"PassengerId"]
NAagemr<-NAage[which(str_detect(NAage$Name,"Mr.")),"PassengerId"]



for(i in 1:length(NAagemiss)){
  titanic[NAagemiss[i],"Age"]<-missNA}
for(i in 1:length(NAagemrs)){
  titanic[NAagemrs[i],"Age"]<-mrsNA}
for(i in 1:length(NAagemaster)){
  titanic[NAagemaster[i],"Age"]<-masterNA}
for(i in 1:length(NAagemr)){
  titanic[NAagemr[i],"Age"]<-mrNA}

NAextramean<-round(mean(titanicNA$Age))
NAextra<-titanic[which(is.na(titanic$Age)),"PassengerId"]
NAextra
for(i in 1:length(NAextra))
{titanic[NAextra[i],"Age"]<-NAextramean}


#Now there is no NA values . Data is clean 


#checking for blank values
for(i in 1:nrow(titanic))
{if(titanic[i,"Embarked"]!="S"&titanic[i,"Embarked"]!="C"&titanic[i,"Embarked"]!="Q")
{print(i)
  titanic[i,"Embarked"]<-"S"
  print(titanic[i,"Embarked"])
  
}
  
}


#distribution of data

boxplot(titanic$Age~titanic$Sex)
boxplot(titanic$Fare~titanic$Age)
boxplot(titanic$Fare~titanic$Sex)
boxplot(titanic$Fare~titanic$Embarked)

plot(density(titanic$Fare),
     col="red",
     main="Density plot for Fare",
     xlab = "Fare",
     ylab="Dnesity")
polygon(density(titanic$Fare),col = "red")

plot(titanic$Age, titanic$Fare, main = "flow",
     xlab = "Age", ylab ="Fare",
     pch = 19, frame = FALSE)
# Add regression line
plot(titanic$Age, titanic$Fare, main = "flow",
     xlab = "Age", ylab = "Fare",
     pch = 19, frame = FALSE)
abline(lm(Fare ~ Age, data = titanic), col = "blue")

# It shows Fare is increasing with Age





#converting Survived and Pclass variables into factors
titanic$Survived<-as.factor(titanic$Survived)
titanic$Pclass<-as.factor(titanic$Pclass)



#count of factor classes of each variable factor
table(titanic$Survived)
table(titanic$Pclass)


#Hypothesis - Pclass 1 passengers are more in survived numbers
#cause they are rich and had paid more 


outof<-"survived out of"
percentage<-"%"

#No. of passangers which survived and belongs to Pclass 1
survived.1<-length(train[which(train$Survived==1 & train$Pclass==1),"PassengerId"])
Pclass1<-length(train[which(train$Pclass==1),"PassengerId"])
survived.1.per<-(survived.1/Pclass1)*100
paste(survived.1,outof,Pclass1,collapse = " ")
paste(survived.1.per,percentage,collapse=" ") 


#No. of passangers which survived and belongs to Pclass 2
survived.2<-length(train[which(train$Survived==1 & train$Pclass==2),"PassengerId"])
Pclass2<-length(train[which(train$Pclass==2),"PassengerId"])
survived.2.per<-(survived.2/Pclass2)*100
paste(survived.2,outof,Pclass2,collapse = " ")
paste(survived.2.per,percentage,collapse=" ") 


#No. of passangers which survived and belongs to Pclass 3
survived.3<-length(train[which(train$Survived==1 & train$Pclass==3),"PassengerId"])
Pclass3<-length(train[which(train$Pclass==3),"PassengerId"])
survived.3.per<-(survived.3/Pclass3)*100
paste(survived.3,outof,Pclass3,collapse = " ")
paste(survived.3.per,percentage,collapse=" ") 


#Percentage of passengers total survived
total.survived<-sum(survived.1,survived.2,survived.3)
PassengerId.total<-length(train$PassengerId)
total.survived.per<-(total.survived/PassengerId.total)*100
paste(total.survived.per,percentage,collapse = " ")

#plotting Pie Chart for survived or not

library(plotrix)

pct=round(table(train$Survived)/sum(table(train$Survived))*100)
lbs=paste(c("Dead","Survived")," ",pct,"%",sep=" ")
pie3D(table(train$Survived),labels = lbs,main="Pie chart showing percentag of survived vs dead")


#plotting Pie Chart for survived.1 for Pclass1 or not

piesurvived1<-c(survived1=survived.1,dead1=Pclass1-survived.1)
pct=round(piesurvived1/sum(piesurvived1)*100)
lbs=paste(c("Survived","Dead")," ",pct,"%",sep=" ")
pie3D(piesurvived1,labels = lbs,main="Pie chart showing percentag of survived vs dead of Pclass 1")


#plotting Pie Chart for survived.2 for Pclass2 or not

piesurvived2<-c(survived2=survived.2,dead2=Pclass2-survived.2)
pct=round(piesurvived2/sum(piesurvived2)*100)
lbs=paste(c("Survived","Dead")," ",pct,"%",sep=" ")
pie3D(piesurvived2,labels = lbs,main="Pie chart showing percentag of survived vs dead of Pclass 2")


#plotting Pie Chart for survived.3 for Pclass3 or not

piesurvived3<-c(survived3=survived.3,dead3=Pclass3-survived.3)
pct=round(piesurvived3/sum(piesurvived3)*100)
lbs=paste(c("Survived","Dead")," ",pct,"%",sep=" ")
pie3D(piesurvived3,labels = lbs,main="Pie chart showing percentag of survived vs dead of Pclass 3")


#plotting bar chart for survival of each pclass


library(ggplot2)

ggplot(train,aes(x=Pclass,fill=factor(Survived)))+
  geom_bar(position="dodge")+
  xlab("Pclass")+
  ylab("No. of Passengers survived/not survived")+
  labs(fill="0 : Dead / 1 : Survived")

library(scales)
library(dplyr)

train$Survived=as.factor(train$Survived)
train$Pclass=as.factor(train$Pclass)
str(train)

Pclass_survival<- train%>%group_by(Pclass,Survived)%>%dplyr::summarize(No._of_Passengers_survived_not_survived=n())
str(Pclass_survival)

ggplot(Pclass_survival,aes(Pclass,No._of_Passengers_survived_not_survived,fill=Survived))+
  geom_bar(position="dodge",stat="identity")+
  ggtitle("bar chart for survived in each pclass")+
  scale_y_continuous(labels = comma)+
  scale_fill_manual(values = c("#CC0000", "#006600"))
 # scale_y_continuous(labels ="monthy")+
 ## scale_fill_manual(values = colors)
  

#examining the survival with gender bar chart
titanic$Sex<-as.factor(titanic$Sex)

ggplot(train,aes(Sex,fill=Survived))+
  geom_bar(position = "dodge")+
  ggtitle("survival with Sex")+
  xlab("Sex")+
  ylab("Total Survived")+
  labs(fill="Survived")
  
#examining the survival with female with pie chart

female.sur<-train%>%select(Sex,Survived)%>%filter(Sex=="female" & Survived==1)
female.survive<-length(female.sur$Survived)

female.de<-train%>%select(Sex,Survived)%>%filter(Sex=="female" & Survived==0)
female.dead<-length(female.de$Survived)

femalesurvived<-c(femalesur=female.survive,femaledead=female.dead)
pct=round(femalesurvived/sum(femalesurvived)*100)
lbs=paste(c("Survived","Dead")," ",pct,"%",sep=" ")
pie3D(femalesurvived,labels = lbs,main="female with Survival")



#examining the survival with male with pie chart

male.sur<-train%>%select(Sex,Survived)%>%filter(Sex=="male" & Survived==1)
male.survive<-length(male.sur$Survived)

male.de<-train%>%select(Sex,Survived)%>%filter(Sex=="male" & Survived==0)
male.dead<-length(male.de$Survived)

malesurvived<-c(malesur=male.survive,maledead=male.dead)
pct=round(malesurvived/sum(malesurvived)*100)
lbs=paste(c("Survived","Dead")," ",pct,"%",sep=" ")
pie3D(malesurvived,labels = lbs,main="male with Survival")


#examining survival with males and females

sexsurvive<-c(female.survive,male.survive)
pct<-round(sexsurvive/(sum(malesurvived)+sum(femalesurvived))*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
pie3D(sexsurvive,labels=lbs,main="Survival per Sex")




## Examining the names
head(as.character(train$Name))


#examining survival rate by TITLES such as Mr. ,Mrs. ,etc
#first we have to make a title variable

title<-NULL

titler<-function(Name)
{
  name<-as.character(Name)
  if(length(grep("Miss.",name))>0)
  {return ("Miss.")}
  else if(length(grep("Mrs.",name))>0)
  {return("Mrs.")}else if(length(grep("Master.",name))>0)
  {return("Master.")}else if(length(grep("Mr.",name))>0)
  {return("Mr.")}
  else{
    for(i in 1:nrow(titanic)){
    if(titanic[i,"Name"]==name)
    {if(titanic[i,"Sex"]=="male")
    {if(titanic[i,"Age"]<=masterNA)
    {return("Master.")}
      else{return("Mr.")}
      
    }
      else{
        if(titanic[i,"Age"]<=sum(missNA,mrsNA)/2)
        {return("Miss.")}
        else{return("Mrs.")}
      }
      break
    }}}}


for(i in 1:nrow(titanic))
{title<-c(title,titler(titanic[i,"Name"]))}

titanic$title<-as.factor(title)

# now we will plot the survival based on titles using train set only,
#but we have made changes to titanic set

ggplot(titanic[1:nrow(train),],aes(x=title,fill=Survived))+
  geom_bar(position="dodge")+
  # facet_wrap(~Pclass)+
  ggtitle("titlewise survival")+
  xlab("title")+
  ylab("No. of people survived/dead")+
  labs(fill="Survived")


#division on Pclass

ggplot(titanic[1:nrow(train),],aes(x=title,fill=Survived))+
  geom_bar(position="dodge")+
  facet_wrap(~Pclass)+
  ggtitle("Pclasswise titlewise survival")+
  xlab("title")+
  ylab("No. of people survived/dead")+
  labs(fill="Survived")

#We concluded that Master titles in both Pclass1 and Pclass2 have no deaths .
#so we can conclude that every master. in  Pclass1 and Pclass2  will  survive in Test dataset observations



summary(titanic$Age)

titanic$Age<-as.factor(titanic$Age)

ggplot(titanic[1:nrow(train),],aes(x=Age,fill=Survived))+
  geom_bar(position="dodge")+
  facet_wrap(~title+Pclass)+
  ggtitle("Pclasswise titlewise survival")+
  xlab("Age")+
  ylab("No. of people survived/dead")+
  labs(fill="Survived")

#we concluded that almost every Miss. and Mrs. in Pclass1 and Pclass2 survived 

#Examining the survival rate by sibling/spouse 

titanic$SibSp<-as.factor(titanic$SibSp)

ggplot(titanic[1:nrow(train),],aes(x=SibSp,fill=Survived))+
  geom_bar(position="dodge")+
  facet_wrap(~title+Pclass)+
  ggtitle("Pclasswise titlewise survival")+
  xlab("Siblings/Spouse")+
  ylab("No. of people survived/dead")+
  labs(fill="Survived")

#We concluded that every master in Pclass3 with 0,1 siblings/spouse survived

#Examining  the survival rate by parents/child
titanic$Parch<-as.factor(titanic$Parch)

ggplot(titanic[1:nrow(train),],aes(x=Parch,fill=Survived))+
  geom_bar(position="dodge")+
  facet_wrap(~title+Pclass)+
  ggtitle("Pclasswise titlewise survival")+
  xlab("Parent/Children")+
  ylab("No. of people survived/dead")+
  labs(fill="Survived")


#Examining  the survival rate by Fare
titanic$Fare<-as.factor(titanic$Fare)

ggplot(titanic[1:nrow(train),],aes(x=Fare,fill=Survived))+
  geom_bar(position="dodge")+
  facet_wrap(~title+Pclass)+
  ggtitle("Pclasswise titlewise survival")+
  xlab("Fare")+
  ylab("No. of people survived/dead")+
  labs(fill="Survived")

#We concluded that almost every Mr. in Pclass 2 did not survive


#Examining the survival rate by Embarked

titanic$Embarked<-as.factor(titanic$Embarked)

ggplot(titanic[1:nrow(train),],aes(x=Embarked,fill=Survived))+
  geom_bar(position="dodge")+
  facet_wrap(~title+Pclass)+
  ggtitle("Pclasswise titlewise survival")+
  xlab("Embarked")+
  ylab("No. of people survived/dead")+
  labs(fill="Survived")

#We concluded that no Mr. from Pclass1 and Pclass2 with Embarked "Q" survived
#also master from Pclass3 did not survive with Embarked "Q'

