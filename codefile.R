setwd("C:/Users/user/Desktop/analytics vidhya/Smart")
train<-read.csv("training.csv",header=T)
test<-read.csv("test.csv",header=T)
test$Business_Sourced<--1

train1<-train[is.na(train$Manager_Num_Products2),]
test1<-test[is.na(test$Manager_Num_Products2),]
train<-subset(train,train$Manager_Num_Products2!="NA")
test<-subset(test,test$Manager_Num_Products2!="NA")

X_All<-rbind(train,test)
X_All1<-rbind(train1,test1)
summary(X_All)

table(is.na(X_All))
str(X_All)
cor(X_All$Manager_Num_Products,X_All$Manager_Business)
#Applicant Gender
levels(X_All$Applicant_Gender)[levels(X_All$Applicant_Gender)==""]="M"

#Applicant DOB
X_All$Applicant_BirthDate<-as.character(X_All$Applicant_BirthDate)
X_All$Applicant_BirthDate<-strsplit(X_All$Applicant_BirthDate,"/")
for( i in 1:nrow(X_All)){
X_All[i,'Applicant_BirthDate']<-X_All$Applicant_BirthDate[[i]][3]
}
X_All$Applicant_BirthDate<-as.character(X_All$Applicant_BirthDate)
X_All$Applicant_BirthDate<-as.numeric(X_All$Applicant_BirthDate)
X_All$Applicant_age<-2009-X_All$Applicant_BirthDate
X_All$Applicant_BirthDate<-NULL
boxplot(X_All$Applicant_age)
table(is.na(X_All$Applicant_age))
summary(X_All$Applicant_age)
X_All$Applicant_age[is.na(X_All$Applicant_age)]<-mean(X_All$Applicant_age,na.rm=T)
X_All$Applicant_age[X_All$Applicant_age<27]=27
X_All$Applicant_age[X_All$Applicant_age>55]=55

#Applicatant marrital status
levels(X_All$Applicant_Marital_Status)[levels(X_All$Applicant_Marital_Status) %in% c("D","W")]="S"
levels(X_All$Applicant_Marital_Status)[levels(X_All$Applicant_Marital_Status) %in% c("")]="M"

#Manager DOJ
X_All$Manager_DOJ<-as.character(X_All$Manager_DOJ)
X_All$Manager_DOJ<-strsplit(X_All$Manager_DOJ,"/")

for( i in 1:nrow(X_All)){
  X_All[i,'Manager_yr']<-X_All$Manager_DOJ[[i]][3]
}
X_All$Manager_yr<-as.numeric(X_All$Manager_yr)
X_All$Manager_yr<-(2009-X_All$Manager_yr)*12
for (i in 1:nrow(X_All)){
  X_All[i,'Manager_yr']=X_All[i,'Manager_yr']-as.numeric(X_All$Manager_DOJ[[i]][1])+4
}

X_All$Manager_DOJ<-NULL
boxplot(X_All$Manager_yr)
summary(X_All$Manager_yr)
X_All$Manager_yr[X_All$Manager_yr>(1.5*IQR(X_All$Manager_yr))]=1.5*IQR(X_All$Manager_yr)

#manager dob
X_All$Manager_DoB<-as.character(X_All$Manager_DoB)
X_All$Manager_DoB<-strsplit(X_All$Manager_DoB,"/")

for( i in 1:nrow(X_All)){
  X_All[i,'Manager_DoB']<-X_All$Manager_DoB[[i]][3]
}
X_All$Manager_DoB<-as.numeric(X_All$Manager_DoB)
X_All$Manager_age<-2009-X_All$Manager_DoB
X_All$Manager_DoB<-NULL
boxplot(X_All$Manager_age)
X_All$Manager_age[X_All$Manager_age>53]=53

#Manager joining position
table(X_All$Manager_Joining_Designation)
levels(X_All$Manager_Joining_Designation)[levels(X_All$Manager_Joining_Designation)==""]<-"Other"
levels(X_All$Manager_Joining_Designation)[levels(X_All$Manager_Joining_Designation)=="Other"]<-"Level 3"
X_All$Manager_Joining_Designation<-substr(X_All$Manager_Joining_Designation,7,8)
X_All$Manager_Joining_Designation<-as.numeric(X_All$Manager_Joining_Designation)

#Manager current designation
table(X_All$Manager_Current_Designation)
levels(X_All$Manager_Current_Designation)[levels(X_All$Manager_Current_Designation)==""]<-"Level 1"
X_All$Manager_Current_Designation<-substr(X_All$Manager_Current_Designation,7,8)
X_All$Manager_Current_Designation<-as.numeric(X_All$Manager_Current_Designation)

X_All$MAnager_growth<-X_All$Manager_Current_Designation-X_All$Manager_Joining_Designation
table(is.na(X_All$MAnager_growth))
summary(X_All$MAnager_growth)
X_All$MAnager_growth<-as.factor(X_All$MAnager_growth)
X_All$Manager_Joining_Designation<-NULL
X_All$Manager_Current_Designation<-NULL
X_All$MAnager_growth<-as.character(X_All$MAnager_growth)
X_All$MAnager_growth<-as.numeric(X_All$MAnager_growth)
X_All$MAnager_growth[is.na(X_All$MAnager_growth)]<-median(X_All$MAnager_growth,na.rm=T)

#Manger total product
X_All$Manager_total_product<-(X_All$Manager_Num_Products+X_All$Manager_Num_Products2)/2
table(X_All$Manager_total_product)
boxplot(X_All$Manager_total_product)
X_All$Manager_total_product[X_All$Manager_total_product>1.5*IQR(X_All$Manager_total_product)]<-1.5*IQR(X_All$Manager_total_product)
X_All$Manager_total_product<-(X_All$Manager_total_product-min(X_All$Manager_total_product))/(max(X_All$Manager_total_product)-min(X_All$Manager_total_product))

#Manager total business
X_All$Manager_total_manager<-(X_All$Manager_Business+X_All$Manager_Business2)/2
boxplot(X_All$Manager_total_manager)
summary(X_All$Manager_total_manager)
X_All$Manager_total_manager[X_All$Manager_total_manager>1.5*IQR(X_All$Manager_total_manager)]<-1.5*IQR(X_All$Manager_total_manager)
X_All$Manager_Business<-NULL
X_All$Manager_Num_Products<-NULL
X_All$Manager_Business2<-NULL
X_All$Manager_Num_Products2<-NULL
X_All$Manager_total_manager<-(X_All$Manager_total_manager-min(X_All$Manager_total_manager))/(max(X_All$Manager_total_manager)-min(X_All$Manager_total_manager))

#MAnager grade
X_All$Manager_Grade<-as.factor(X_All$Manager_Grade)
table(X_All$Manager_Grade)

#Manager status
X_All$Manager_Status<-droplevels(X_All$Manager_Status)


#Manager gender
X_All$Manager_Gender<-droplevels(X_All$Manager_Gender)

#Applicant education
table(X_All$Applicant_Qualification)
levels(X_All$Applicant_Qualification)
levels(X_All$Applicant_Qualification)[levels(X_All$Applicant_Qualification) %in% c("Associate / Fellow of Institute of Chartered Accountans of India","Associate/Fellow of Acturial Society of India","Associate/Fellow of Institute of Company Secretories of India","Associate/Fellow of Insurance Institute of India","Associate/Fellow of Institute of Institute of Costs and Works Accountants of India")]<-"Associate"
X_All$Applicant_Qualification<-droplevels(X_All$Applicant_Qualification)
levels(X_All$Applicant_Qualification)[levels(X_All$Applicant_Qualification) %in% c("Professional Qualification in Marketing","Masters of Business Administration")]<-"MBA"
levels(X_All$Applicant_Qualification)[levels(X_All$Applicant_Qualification) %in% c("")]<-"Others"

#Applicant Occupation
table(X_All$Applicant_Occupation)
levels(X_All$Applicant_Occupation)[levels(X_All$Applicant_Occupation)==""]<-"Others2"
#levels(X_All$Applicant_Occupation)[levels(X_All$Applicant_Occupation)=="Self Employed"]<-"Business"

#PIN
X_All$Office_PIN<-substr(X_All$Office_PIN,1,2)
X_All$Applicant_City_PIN<-substr(X_All$Applicant_City_PIN,1,2)
X_All$Office_PIN<-as.numeric(X_All$Office_PIN)
X_All$Applicant_City_PIN<-as.numeric(X_All$Applicant_City_PIN)
X_All$City<-X_All$Applicant_City_PIN-X_All$Office_PIN
table((X_All$City))
X_All$City[is.na(X_All$City)]<-0
X_All$City[X_All$City>0|X_All$City<0]<-1
X_All$Office_PIN<-NULL
X_All$Applicant_City_PIN<-NULL
X_All$City<-as.factor(X_All$City)

#manager num application
X_All$Manager_Num_Application[X_All$Manager_Num_Application>7]=7
boxplot(X_All$Manager_Num_Application)

#manger num coded
boxplot(X_All$Manager_Num_Coded)
X_All$Manager_Num_Coded[X_All$Manager_Num_Coded>2]=3

#APPLICANT RECEIPT YR
# X_All$Application_Receipt_Date<-as.character(X_All$Application_Receipt_Date)
# X_All$Applicant_Receipt_Date<-strsplit(X_All$Application_Receipt_Date,"/")
# X_All$Applicant_Receipt_Yr<-strsplit(X_All$Application_Receipt_Date,"/")
# for (i in 1:nrow(X_All)){
#   X_All[i,'Applicant_Receipt_Yr']<-X_All$Applicant_Receipt_Yr[[i]][3]
# }
# X_All$Applicant_Receipt_Yr<-as.numeric(X_All$Applicant_Receipt_Yr)
# X_All$Applicant_yr<-(2009-X_All$Applicant_Receipt_Yr)*12
# for(i in 1:nrow(X_All)){
# X_All[i,'Applicant_yr']<-X_All[i,'Applicant_yr']-as.numeric(X_All$Applicant_Receipt_Date[[i]][1])
# }
# X_All$Applicant_yr<-X_All$Applicant_yr+1
# X_All$Applicant_Receipt_Date<-NULL
# X_All$Applicant_Receipt_Yr<-NULL
# X_All$Application_Receipt_Date<-NULL
# boxplot(X_All$Applicant_yr)

boxplot(X_All$Manager_age)
X_All$Recruit_yr<-NULL
X_All$Recruit_month<-NULL
summary(X_All)
library(dummies)
X_All_2=dummy.data.frame(X_All,names=c("Applicant_Gender","Applicant_Marital_Status","Applicant_Occupation","Applicant_Qualification","Manager_Grade","Manager_Status","Manager_Gender"))

X_train1<-X_All_2[1:nrow(train),]
y<-X_train1[,34]
X_train1<-X_train1[,-c(1,34)]
X_test1<-X_All_2[-(1:nrow(train)),-c(1,34)]
library(xgboost)
model_xgb_cv<- xgb.cv(data=data.matrix(X_train1),label=data.matrix(y),objective="binary:logistic",nfold=5,nrounds=200,eta=0.02,max_depth=5,subsample=0.6,colsample_bytree=0.85,min_child_weight=1,eval_metric="auc")
model_xgb<- xgboost(data=data.matrix(X_train1),label=data.matrix(y),objective="binary:logistic",nrounds=200,eta=0.02,max_depth=5,subsample=0.6,colsample_bytree=0.85,min_child_weight=1,eval_metric="auc")
pred1<-predict(model_xgb,data.matrix(X_test1))


#Glmnet
library(glmnet)
x=model.matrix(~.,data=X_train1)
gcv <- cv.glmnet(x=x ,
                 y = y,
                 family = 'binomial',
                 type.measure = 'auc',
                 nfolds = 4)
coeffs <- as.matrix(coef(gcv, s = 'lambda.1se'))
chosenVars1 <- rownames(coeffs)[abs(coeffs[,1]) > 0]
chosenVars1
X_train1_new=X_train1[,c(1,2,3,4,5,7,9,10,11,15,18,21,22,23,24,27,28,29,31,32,33,35,40,42,44,45,47)]
X_test1_new=X_test1[,c(1,2,3,4,5,7,9,10,11,15,18,21,22,23,24,27,28,29,31,32,33,35,40,42,44,45,47)]

#############################NA
X_All1<-X_All1[,c(1:9,23)]
levels(X_All1$Applicant_Gender)[levels(X_All1$Applicant_Gender)==""]="M"

#Applicant DOB
X_All1$Applicant_BirthDate<-as.character(X_All1$Applicant_BirthDate)
X_All1$Applicant_BirthDate<-strsplit(X_All1$Applicant_BirthDate,"/")
for( i in 1:nrow(X_All1)){
  X_All1[i,'Applicant_BirthDate']<-X_All1$Applicant_BirthDate[[i]][3]
}
X_All1$Applicant_BirthDate<-as.character(X_All1$Applicant_BirthDate)
X_All1$Applicant_BirthDate<-as.numeric(X_All1$Applicant_BirthDate)
X_All1$Applicant_age<-2009-X_All1$Applicant_BirthDate
X_All1$Applicant_BirthDate<-NULL
boxplot(X_All1$Applicant_age)
table(is.na(X_All1$Applicant_age))
summary(X_All1$Applicant_age)
X_All1$Applicant_age[is.na(X_All1$Applicant_age)]<-mean(X_All1$Applicant_age,na.rm=T)
X_All1$Applicant_age[X_All1$Applicant_age>60]=60

#Applicatant marrital status
levels(X_All1$Applicant_Marital_Status)[levels(X_All1$Applicant_Marital_Status) %in% c("D","W")]="S"
levels(X_All1$Applicant_Marital_Status)[levels(X_All1$Applicant_Marital_Status) %in% c("")]="M"


#Applicant education
table(X_All1$Applicant_Qualification)
levels(X_All1$Applicant_Qualification)
levels(X_All1$Applicant_Qualification)[levels(X_All1$Applicant_Qualification) %in% c("Associate / Fellow of Institute of Chartered Accountans of India","Associate/Fellow of Acturial Society of India","Associate/Fellow of Institute of Company Secretories of India","Associate/Fellow of Insurance Institute of India","Associate/Fellow of Institute of Institute of Costs and Works Accountants of India")]<-"Associate"
X_All1$Applicant_Qualification<-droplevels(X_All1$Applicant_Qualification)
levels(X_All1$Applicant_Qualification)[levels(X_All1$Applicant_Qualification) %in% c("Professional Qualification in Marketing","Masters of Business Administration")]<-"MBA"
levels(X_All1$Applicant_Qualification)[levels(X_All1$Applicant_Qualification) %in% c("")]<-"Others"

#Applicant Occupation
table(X_All1$Applicant_Occupation)
levels(X_All1$Applicant_Occupation)[levels(X_All1$Applicant_Occupation)==""]<-"Others2"
#levels(X_All1$Applicant_Occupation)[levels(X_All1$Applicant_Occupation)=="Self Employed"]<-"Business"

#PIN
X_All1$Office_PIN<-substr(X_All1$Office_PIN,1,2)
X_All1$Applicant_City_PIN<-substr(X_All1$Applicant_City_PIN,1,2)
X_All1$Office_PIN<-as.numeric(X_All1$Office_PIN)
X_All1$Applicant_City_PIN<-as.numeric(X_All1$Applicant_City_PIN)
X_All1$City<-X_All1$Applicant_City_PIN-X_All1$Office_PIN
table((X_All1$City))
X_All1$City[is.na(X_All1$City)]<-0
X_All1$City[X_All1$City>0|X_All1$City<0]<-1
X_All1$Office_PIN<-NULL
X_All1$Applicant_City_PIN<-NULL

# #APPLICANT RECEIPT YR
# X_All1$Application_Receipt_Date<-as.character(X_All1$Application_Receipt_Date)
# X_All1$Applicant_Receipt_Date<-strsplit(X_All1$Application_Receipt_Date,"/")
# X_All1$Applicant_Receipt_Yr<-strsplit(X_All1$Application_Receipt_Date,"/")
# for (i in 1:nrow(X_All1)){
#   X_All1[i,'Applicant_Receipt_Yr']<-X_All1$Applicant_Receipt_Yr[[i]][3]
# }
# X_All1$Applicant_Receipt_Yr<-as.numeric(X_All1$Applicant_Receipt_Yr)
# X_All1$Applicant_yr<-(2009-X_All1$Applicant_Receipt_Yr)*12
# for(i in 1:nrow(X_All1)){
#   X_All1[i,'Applicant_yr']<-X_All1[i,'Applicant_yr']-as.numeric(X_All1$Applicant_Receipt_Date[[i]][1])
# }
# X_All1$Applicant_yr<-X_All1$Applicant_yr+1
# X_All1$Applicant_Receipt_Date<-NULL
# X_All1$Applicant_Receipt_Yr<-NULL
# X_All1$Application_Receipt_Date<-NULL
boxplot(X_All1$Applicant_age)
# 
boxplot(X_All1)

X_All_3=dummy.data.frame(X_All1,names=c("Applicant_Gender","Applicant_Marital_Status","Applicant_Occupation","Applicant_Qualification"))


X_train2<-X_All_3[1:nrow(train1),]
z<-X_train2[,18]
X_train2<-X_train2[,-c(1,18)]
X_test2<-X_All_3[-(1:nrow(train1)),-c(1,18)]
model_xgb_cv<- xgb.cv(data=data.matrix(X_train2),label=data.matrix(z),objective="binary:logistic",nfold=5,nrounds=50,eta=0.02,max_depth=5,subsample=0.6,colsample_bytree=0.99,min_child_weight=1,eval_metric="auc")
model_xgb<- xgboost(data=data.matrix(X_train2),label=data.matrix(z),objective="binary:logistic",nrounds=50,eta=0.02,max_depth=5,subsample=0.6,colsample_bytree=0.85,min_child_weight=1,eval_metric="auc")
pred2<-predict(model_xgb,data.matrix(X_test2))

#glmnet
x=model.matrix(~.,data=X_train2)
gcv <- cv.glmnet(x=x ,
                 y = z,
                 family = 'binomial',
                 type.measure = 'auc',
                 nfolds = 4)
coeffs <- as.matrix(coef(gcv, s = 'lambda.1se'))
chosenVars1 <- rownames(coeffs)[abs(coeffs[,1]) > 0]
chosenVars1
X_train2_new =as.data.frame( X_train2[,17])
X_test2_new=as.data.frame(X_test2[,'Applicant_age'])
age=(X_test2$Applicant_age-min(X_test2$Applicant_age))/(max(X_test2$Applicant_age)-min(X_test2$Applicant_age))

sub1<-data.frame("ID"=test$ID,"Business_Sourced"=pred1)
sub2<-data.frame("ID"=test1$ID,"Business_Sourced"=pred2)
sub<-rbind(sub1,sub2)
write.csv(sub,"sub.csv",row.names = F)

write.csv(X_All_2,"X_All.csv",row.names = F)
write.csv(X_All_3,"X_NA.csv",row.names = F)

library(glmnet)
install.packages("glmnet")
