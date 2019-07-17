library(dplyr)
library(gbm)
library(randomForest)
library(ggplot2)
library(cvTools)
library(xgboost)
library(tree)
library(car)
library(pROC)

setwd()
getwd("D:/DATA analytics/R/R PROJECT/BANK")

## load the train and test data

b.train=read.csv("bank-full_train.csv",stringsAsFactors = F)

b.test=read.csv("bank-full_test.csv",stringsAsFactors = F)

## data preparation phase
b.test$y=NA

# put place holder for train and test data for identification
b.train$data = "train"
b.test$data = "test"

## combine data for data preparation
bank=rbind(b.train,b.test)

glimpse(bank)
head(bank$y)

unique(bank$)
##unique(bank$balance)
##unique(bank$duration)

## removing variable with more then 1000 unique value

bank = bank %>%select (-ID,- balance,-duration)


### create dummies

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("/","",name)
    name=gsub(">=","",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}



## coverting the target variable  in the form of numeric 0 and 1

bank$y = as.numeric(bank$y=='yes')


##sapply to get all the column names which are of character type

x = sapply(bank,is.character)
x


cat.col=names(bank)[x]
cat.col





###we dont want data column so will remove data column
cat.col=cat.col[-10]
cat.col

## creating dummy variable for all the categorical columns of character types
# we are using frequency cutoff as 100, there is no magic number here,
# lower cutoffs will simply result in more number of dummy vars


for(col in cat.col){
  bank=CreateDummies(bank,col,100)
  
}

glimpse(bank)



table(bank$y)


#################################NA value



sum(sapply(bank,function(x) is.character(x)))
# that tells us that there are no character columns remaining [ 1 comes for column 'data']



lapply(bank,function(x) sum(is.na(x)))

for(col in names(bank)){
  
  if(sum(is.na(bank[,col]))>0 & !(col %in% c("data","y"))){
    
    bank[is.na(bank[,col]),col]=mean(bank[bank$data=='train',col],na.rm=T)
  }
  
}


## Sepeartion of data into train and test

b.train=bank%>% filter(data=='train') %>% select(-data)
b.test=bank%>% filter(data=='test') %>% select (-data,-y)



## seprate train and test data from train_data
set.seed(2)
s=sample(1:nrow(b.train),0.8*nrow(b.train))

b.training_data =b.train[s,]
b.testing_data=b.train[-s,]



## VIF to remove multicolinearity between dependent variable

fit=lm(y~.,data=b.training_data)
vif(fit)
sort(vif(fit),decreasing = T)[1:8]

fit=lm(y~.- month_may,data=b.training_data)
sort(vif(fit),decreasing = T)[1:8]

fit=lm(y~.-month_may- job_blue_collar,data=b.training_data)
sort(vif(fit),decreasing = T)[1:8]

fit=lm(y~.- month_may- job_blue_collar- poutcome_unknown,data=b.training_data)
sort(vif(fit),decreasing = T)[1:8]

fit=lm(y~.- month_may- job_blue_collar- poutcome_unknown-education_secondary,data=b.training_data)
sort(vif(fit),decreasing = T)[1:8]

fit=lm(y~.- month_may- job_blue_collar- poutcome_unknown-education_secondary- contact_unknown,data=b.training_data)
sort(vif(fit),decreasing = T)[1:8]

#vif is under control

log.fit=glm(y~.- month_may- job_blue_collar- poutcome_unknown-education_secondary- contact_unknown,data=b.training_data,family = "binomial")

## now with the help of step function we remove variable which have probality greater than 0.05
log.fit=step(log.fit)

summary(log.fit)
###round off the value and 999 is any number
options(scipen = 999)
summary(log.fit)


formula(log.fit)

log.fit=glm(y ~ age + campaign + pdays + previous + job_student + job_housemaid + 
              job_retired + marital_single + marital_married + education_primary + 
              education_tertiary + default_no + housing_yes + loan_no + 
              contact_cellular + month_mar + month_sep + month_oct + month_jan + 
              month_feb + month_apr + month_jun + month_aug + month_jul + 
              poutcome_other + poutcome_failure,data=b.training_data,family='binomial')
summary(log.fit)

#### performance of score model on validation data i.e b.testing_data
library(pROC)

val.score=predict(log.fit,newdata =b.testing_data,type='response')
val.score[1:5]

#area under the curve (auc)

auc(roc(b.testing_data$y,val.score))

#Area under the curve: 0.7271

## score isless so  we can try randomforest model


#######################################RANDOMFOREST###########################################

### RF and convert response to factor

b.rf= randomForest(as.factor(y)~.- month_may- job_blue_collar- 
                     poutcome_unknown-education_secondary- contact_unknown,data=b.training_data)

b.rf


#####################################################################################################################################################
#out of bag error (OOB) 10.92%. out of the bag means out of training data so ASSUME that RF algo will build the model on b. train data 
# so ASSUME it will split and build  90% of data for train and 10% for Test 

# Confusion matrix:
# error with respect to class O is (1.4%) 324/(21964+324) 
# error with respect to class 1 is (80%)2440/(2440+589) almost 80% records are misclasiified 

# however over all estimate error is10.92%
############################################################################

## predict the score on testing data

predict.score=predict(b.rf,newdata = b.testing_data,type='prob')[,2]


#########

table(bank$y)

#0      1 
#27927 3720

levels(as.factor(bank$y))

# "0" "1"

#y - has the client subscribed a term deposit? (binary: "yes" (1)  ,"no" (0))
##what do we model: the probability of the outcome being 1 or the probability of the outcome being 0?
## you are not goin to predict the value for  prob with 0 value(27927) hence will calcute prob for value 1 (3720)and use this in our model
## predict.score=predict(s.rf,newdata = s.testing_data,type='prob')[,2] not [,1]


## obtaining auc on testing_data ---------------------------------------------------------------------------------------------------------------

pROC::roc(b.testing_data$y,predict.score)$auc

##The AUC value lies between 0.5 to 1 where 0.5 denotes a bad classifer and 1 denotes an excellent classifier
# since AUC value is (0.7492) it is good classifier
## auc score is fine as it is ~ 0.75 so this is the best fittted model here 


#######################################LETS Build the FINAL MOdel###################################################################


### RF and convert response to factor

final.rf= randomForest(as.factor(y)~.- month_may- job_blue_collar- 
                     poutcome_unknown-education_secondary- contact_unknown,data=b.train)

final.rf


# Confusion matrix:
# error with respect to class O is (1.3%) 372/(27555+372)
# error with respect to class 1 is (80%)3036/(3036+684)
# however over all estimate error is 10.77%


###### Applying model predict function 




val.score=predict(final.rf,newdata = b.train,type='prob')[,2]

pROC::roc(b.train$y,val.score)$auc

##The AUC value lies between 0.5 to 1 where 0.5 denotes a bad classifer and 1 denotes an excellent classifier
# since AUC value is near by 1 (0.9624) it is good classifier
## auc score is fine as it is ~ 0.95 so this is the best fittted model here

test.score=predict(b.rf,newdata=b.test,type='prob')[,2]

test.score[1:3]

write.csv(test.score,"karan_Rajyaguru_P4_part2.csv",row.names = F)


## Variable IMportance

d=importance(final.rf)
d=as.data.frame(d)
d$VariableName=rownames(d)
d %>% arrange(desc(MeanDecreaseGini))

varImpPlot(final.rf)

#Evaluation Criterion :KS score on test data. larger KS, better Model

#Your KS score for test data should come out to be more than : 0.47



####################################### we have to give answer in hard class values so  lets do it

train.score=predict(b.rf,newdata=b.train,type='prob')[,2]
real=b.train$y

cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=((TP/P)-(FP/N))
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}


View(cutoff_data)
cutoff_data=cutoff_data[-1,]

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

# KS score 0.7320673

## Once we know the cutoff we can use it to convert test score to 
## hard classes

test.predicted=as.numeric(test.score>my_cutoff)

Write.csv(test.predicted,"karan_Rajyaguru_P4_part2.csv",row.names = F)


## score can be calculated as --------------------------------------------------------------------------------------------------------------
score = 1-(0.025/max(cutoff_data$KS))
score
## predicting the value in the form of 1 and 0 ---------------------------------------------------------------------------------------------
final.test.prediction =as.numeric(test.score >my_cutoff)

View(final.test.prediction)

## predicting the value in the form of Yes and NO 
final.test.prediction = as.character(final.test.prediction == 1)
final.test.prediction = gsub("FALSE","No",final.test.prediction)
final.test.prediction = gsub("TRUE","Yes",final.test.prediction)











