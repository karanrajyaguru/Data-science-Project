
library(dplyr)
library(gbm)
library(randomForest)
library(ggplot2)
library(cvTools)
library(xgboost)
library(tree)
library(car)
library(pROC)


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



x =sapply(hr,is.character)
x


cat.col=names(hr)[x]
cat.col


###we dont want data column so will remove data column
cat.col=cat.col[-3]
cat.col

for(col in cat.col){
  hr=CreateDummies(hr,col,100)
  
}

glimpse(hr)


table(hr$left)


################ NA values


lapply(hr,function(x) sum(is.na(x)))

for(col in names(hr)){
  
  if(sum(is.na(hr[,col]))>0 & !(col %in% c("data","left"))){
    
    hr[is.na(hr[,col]),col]=mean(hr[hr$data=='train',col],na.rm=T)
  }
  
}

## separate train and test

hr.train=hr %>% filter(data=='train') %>% select(-data)
hr.test=hr%>% filter(data=='test') %>% select (-data,-left)



###eleminnation of variable with highets VIF

for.vif=lm(left~.,data=hr.train)

###round off the value and 999 is any number
options(scipen = 999)
sort(vif(for.vif),decreasing = T)[1:10]





### RF and convert response to factor


hr.rf= randomForest(as.factor(left)~.,data=hr.train)
hr.rf


###### Applying model predict function 

#### see the table(hr$left) 0 and 1 means row 1 and 2  and here we habe taken [,2] bz we are building a model on 1 i.e who are about to left

table(hr$left)

#0    1 
#7424 3075 

levels(as.factor(hr$left))

# "0" "1"
##what do we model: the probability of the outcome being 1or the probability of the outcome being 0?

## you are not goin to predict the value for  prob with 0 value(7424) hence will calcute prob for value 1 (3075)and use this in our model

## val.score=predict(hr.rf,newdata = hr.train,type='prob')[,2] not [,1]



val.score=predict(hr.rf,newdata = hr.train,type='prob')[,2]

pROC::roc(hr.train$left,val.score)$auc


test.score=predict(hr.rf,newdata=hr.test,type='prob')[,2]


write.csv(test.score,"karan_Rajyaguru_P4_part2.csv",row.names = F)

#######################################################################



