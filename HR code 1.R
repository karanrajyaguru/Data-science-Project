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
getwd( "D:/DATA analytics/R/R PROJECT/HR")



hr.train=read.csv("hr_train.csv",stringsAsFactors = F)

hr.test=read.csv("hr_test.csv",stringsAsFactors = F)

hr.test$left=NA

hr.train$data = "train"
hr.test$data = "test"

hr=rbind(hr.train,hr.test)

glimpse(hr)

###################################### dummies

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


####### sapply to get all the col names which are of character type


x =sapply(hr,is.character)
x


cat.col=names(hr)[x]
cat.col


###we dont want data column so will remove data column
cat.col=cat.col[-3]
cat.col


# we are using frequency cutoff as 50, there is no magic number here,
# lower cutoffs will simply result in more number of dummy vars


for(col in cat.col){
  hr=CreateDummies(hr,col,50)
  
}

glimpse(hr)


table(hr$left)





################ NA values

sum(sapply(hr,function(x) is.character(x)))
# that tells us that there are no character columns remaining [ 1 comes for column 'data']

lapply(hr,function(x) sum(is.na(x)))

for(col in names(hr)){
  
  if(sum(is.na(hr[,col]))>0 & !(col %in% c("data","left"))){
    
    hr[is.na(hr[,col]),col]=mean(hr[hr$data=='train',col],na.rm=T)
  }
  
}

## Sepeartion of data into train and test

hr.train=hr %>% filter(data=='train') %>% select(-data)
hr.test=hr%>% filter(data=='test') %>% select (-data,-left)


## seprate train and test data from train_data


set.seed(2)
s=sample(1:nrow(hr.train),0.8*nrow(hr.train))

hr.training_data =hr.train[s,]
hr.testing_data=hr.train[-s,]


## ----
library(car)


for_vif=lm(left~.,data=hr.training_data)

###round off the value and 999 is any number
options(scipen = 999)
sort(vif(for_vif),decreasing = T)[1:10]


##in lin reg we eliminate var which is greater then 5 in logistic reg threashold is 10 we can take
## VIF is under control

log_fit=glm(left~.,data=hr.training_data,family = "binomial")


log_fit=step(log_fit)

summary(log_fit)

formula(log_fit)

log_fit=glm(left ~ satisfaction_level + last_evaluation + number_project + 
              average_montly_hours + time_spend_company + Work_accident + 
              promotion_last_5years + sales_hr + sales_accounting + sales_marketing + 
              sales_product_mng + sales_support + sales_technical + sales_sales + 
              salary_medium + salary_low,data=hr.training_data,family='binomial')
summary(log_fit)


#### performance of score model on validation data
library(pROC)

val.score=predict(log_fit,newdata =hr.testing_data,type='response')
val.score[1:10]


#area under the curve (auc)

auc(roc(hr.testing_data$left,val.score))

## AUC score is 0.7246 and our requirement is . 
#Your auc score for test data should come out to be more than 0.84
       
# ## score is very less so logistic model cannot be fiited into the data very well, hence wil use 
## Decision Treee (DT) and Random foreast (RF)


#--------------------------------------------------------------------------------------------------------------

#will use Decision Tree 

hr.tree=tree(as.factor(left)~.,data=hr.training_data)


## Tree in text format

hr.tree

## Visual Format

plot(hr.tree)
text(hr.tree)
## Performance on validation set

val.score=predict(hr.tree,newdata = hr.testing_data,type='vector')[,2]
pROC::roc(hr.testing_data$left,val.score)$auc


## AUC score is 0.8347 and out requirement is . 
#Your auc score for test data should come out to be more than 0.84


#==========================================================================================================================

#RANDOM FOREAST



### RF and convert response to factor


hr.rf= randomForest(as.factor(left)~.,data=hr.training_data)

hr.rf

#####################################################################################################################################################
#out of bag error (OOB) 12.22%. out of the bag means out of training data so ASSUME that RF algo will build the model on HR train data 

# so ASSUME it will split and build  90% of data for train and 10% for Test 

# Confusion matrix:

# error with respect to class O is (4%) 244/(5691+244)

# error with respect to class 1 is (31%) 782/(782+1682)

# however over all estimate error is 12.04%
########################################################################################################################################

## predict the score on testing data

predict.score=predict(hr.rf,newdata = hr.testing_data,type='prob')[,2]



#### see the table(hr$left) 0 and 1 means column 1 and 2  and here we habe taken [,2] bz we are building a model on 1 i.e who are about to left

table(hr$left)

#0    1 
#7424 3075 

levels(as.factor(hr$left))

# "0" "1"
##what do we model: the probability of the outcome being 1 or the probability of the outcome being 0?

## you are not goin to predict the value for  prob with 0 value(7424) hence will calcute prob for value 1 (3075)and use this in our model

## val.score=predict(hr.rf,newdata = hr.train,type='prob')[,2] not [,1]


## obtaining auc on testing_data ---------------------------------------------------------------------------------------------------------------

pROC::roc(hr.testing_data$left,val.score)$auc

##The AUC value lies between 0.5 to 1 where 0.5 denotes a bad classifer and 1 denotes an excellent classifier
# since AUC value is near by 1 (0.8347) it is good classifier
## auc score is fine as it is ~ 0.83 so this is the best fittted model here


# Lets build the Final Model


hr.rf= randomForest(as.factor(left)~.,data=hr.train)

hr.rf

val.score=predict(hr.rf,newdata = hr.train,type='prob')[,2]

pROC::roc(hr.train$left,val.score)$auc


#since AUC value is near by 1 (0.8347) it is good classifier


test.score=predict(hr.rf,newdata=hr.test,type='prob')[,2]


write.csv(test.score,"karan_Rajyaguru_P4_part2.csv",row.names = F)


## Variable IMportance

d=importance(hr.rf)
d=as.data.frame(d)
d$VariableName=rownames(d)
d %>% arrange(desc(MeanDecreaseGini))

varImpPlot(hr.rf)

## from the graph we can say that satisfaction level is the most IMP varaible followed by avg monthly hours to time spend with company




























