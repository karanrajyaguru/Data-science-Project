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
getwd( "D:/DATA analytics/R/R PROJECT/Retail")


## load the train and test data

s.train=read.csv("store_train.csv",stringsAsFactors = FALSE)

s.test=read.csv("store_test.csv",stringsAsFactors = FALSE)

## data preparation phase

s.test$store=NA

# put place holder for train and test data for identification
s.train$data = "train"
s.test$data = "test"

## combine data for data preparation
sto = rbind(s.train,s.test)

glimpse(sto)


###########  Create DUMMIES  

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


#################
## countyname has more then 1000 unique vale
unique(sto$countyname)

## storecode has more then 1000 unique value
unique(sto$storecode)

###Area name has more then 1000 unique value
unique(sto$Areaname)

## countytown name has more then 1000 UV
unique(sto$countytownname)

#########

sto = sto %>%select (-countyname,- storecode,-Areaname,- countytownname)


##sapply to get all the column names which are of character type


x = sapply(sto,is.character)
x


cat.col=names(sto)[sapply(sto,is.character)]
cat.col


###we dont want data column so will remove data column
cat.col=cat.col[-3]
cat.col


## creating dummy variable for all the categorical columns of character types
# we are using frequency cutoff as 50, there is no magic number here,
# lower cutoffs will simply result in more number of dummy vars

for(col in cat.col){
  sto=CreateDummies(sto,col,50)
  
}

glimpse(sto)



###NA value

lapply(sto,function(x) sum(is.na(x)))

sum(sapply(sto,function(x) is.character(x)))
# that tells us that there are no character columns remaining [ 1 comes for column 'data']

for(col in names(sto)){
  
  if(sum(is.na(sto[,col]))>0 & !(col %in% c("data","store"))){
    
    sto[is.na(sto[,col]),col]=mean(sto[sto$data=='train',col],na.rm=T)
  }
  
}


## Sepeartion of data into train and test

s.train=sto%>% filter(data=='train') %>% select(-data)
s.test=sto%>% filter(data=='test') %>% select (-data,-store)


## seprate train and test data from train_data


set.seed(2)
s=sample(1:nrow(s.train),0.8*nrow(s.train))

s.training_data =s.train[s,]
s.testing_data=s.train[-s,]


## VIF to remove multicolinearity between dependent variable

library(car)


for.vif=lm(store~.-Id,data=s.training_data)

###round off the value and 999 is any number
options(scipen = 999)
sort(vif(for.vif),decreasing = T)[1:10]

for.vif=lm(store~.-Id- sales0 ,data=s.training_data)
sort(vif(for.vif),decreasing = T)[1:10]

for.vif=lm(store~.-Id- sales0-sales2,data=s.training_data)
sort(vif(for.vif),decreasing = T)[1:10]

for.vif=lm(store~.-Id- sales0-sales2-sales3,data=s.training_data)
sort(vif(for.vif),decreasing = T)[1:10]

for.vif=lm(store~.-Id- sales0-sales2-sales3-State,data=s.training_data)
sort(vif(for.vif),decreasing = T)[1:10]

##in lin reg we eliminate var which is greater then 5 in logistic reg threashold is 10 we can take
## VIF is under control

log.fit=glm(store~.-Id- sales0-sales2-sales3-State,data=s.training_data,family = "binomial")


log.fit=step(log.fit)

## ignore warning messgae
##Warning messages:
##glm.fit: fitted probabilities numerically 0 or 1 occurred

summary(log.fit)

formula(log.fit)

log.fit=glm(store ~ sales4 + CouSub + population + state_alpha_WV + state_alpha_CA + 
              state_alpha_CO + state_alpha_LA + state_alpha_PR + state_alpha_IN + 
              state_alpha_TN + state_alpha_GA + state_alpha_VT + state_alpha_NH + 
              state_alpha_MA,data=s.training_data,family='binomial')
summary(log.fit)

#### performance of score model on validation data
library(pROC)

val.score=predict(log.fit,newdata =s.testing_data,type='response')
val.score[1:5]

#area under the curve (auc)

auc(roc(s.testing_data$store,val.score))

## AUC score is 0.7617 and our requirement is Your auc score for test data should come out to be more than 0.80

## score is very less so logistic model cannot be fiited into the data very well, hence wil use 

## Decision Treee (DT) and Random foreast (RF)

#========================================================================================================================================

#will use Decision Tree 

s.tree=tree(as.factor(store)~.-Id- sales0-sales2-sales3-State,data=s.training_data)


## Tree in text format

s.tree

## Visual Format

plot(s.tree)
text(s.tree)
## Performance on validation set

val.score=predict(s.tree,newdata = s.testing_data,type='vector')[,2]
pROC::roc(s.testing_data$store,val.score)$auc


## AUC score is 0.7227 and out requirement is Your auc score for test data should come out to be more than 0.80


#======================================================================================================================================

#Random Foreast

### RF and convert response to factor


s.rf= randomForest(as.factor(store)~.-Id- sales0-sales2-sales3-State,data=s.training_data)

s.rf


#####################################################################################################################################################
#out of bag error (OOB) 24.38%. out of the bag means out of training data so ASSUME that RF algo will build the model on s. train data 
# so ASSUME it will split and build  90% of data for train and 10% for Test 

# Confusion matrix:
# error with respect to class O is (16%) 248/(1236+248)
# error with respect to class 1 is (33%) 783/(403+783)

# however over all estimate error is 24.38%
############################################################################

## predict the score on testing data

predict.score=predict(s.rf,newdata = s.testing_data,type='prob')[,2]


#########

table(sto$store)

#0    1 
#1875 1463

levels(as.factor(sto$store))

# "0" "1"
##what do we model: the probability of the outcome being 1 or the probability of the outcome being 0?

## you are not goin to predict the value for  prob with 0 value(1875) hence will calcute prob for value 1 (1463)and use this in our model

## predict.score=predict(s.rf,newdata = s.testing_data,type='prob')[,2] not [,1]


## obtaining auc on testing_data ---------------------------------------------------------------------------------------------------------------

pROC::roc(s.testing_data$store,predict.score)$auc

##The AUC value lies between 0.5 to 1 where 0.5 denotes a bad classifer and 1 denotes an excellent classifier
# since AUC value is (0.7951) it is good classifier
## auc score is fine as it is ~ 0.79 so this is the best fittted model here 


#######################################LETS Build the FINAL MOdel###################################################################


### RF and convert response to factor

final.rf= randomForest(as.factor(store)~.-Id- sales0-sales2-sales3-State,data=s.train)
final.rf



#out of bag error (OOB) 23.82%. out of the bag means out of training data so ASSUME that RF algo will build the model on s. train data 
# so ASSUME it will split and build  90% of data for train and 10% for Test 

# Confusion matrix:
# error with respect to class O is (15%) 287/(1588+287)
# error with respect to class 1 is (34%) 490/(490+973)
# however over all estimate error is 23.82%







###### Applying model predict function 




val.score=predict(final.rf,newdata = s.train,type='prob')[,2]

pROC::roc(s.train$store,val.score)$auc

##The AUC value lies between 0.5 to 1 where 0.5 denotes a bad classifer and 1 denotes an excellent classifier
# since AUC value is near by 1 (0.9558) it is good classifier
## auc score is fine as it is ~ 0.95 so this is the best fittted model here

test.score=predict(s.rf,newdata=s.test,type='prob')[,2]

test.score[1:3]

write.csv(test.score,"karan_Rajyaguru_P4_part2.csv",row.names = F)


## Variable IMportance

d=importance(final.rf)
d=as.data.frame(d)
d$VariableName=rownames(d)
d %>% arrange(desc(MeanDecreaseGini))

varImpPlot(final.rf)


## from the graph we can conclud that population is the most imp variable followed by sales4 till state alpha vr












