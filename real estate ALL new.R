library(dplyr)
library(gbm)
library(randomForest)
library(ggplot2)
library(cvTools)
library(xgboost)
library(tree)
library(car)
library(pROC)

getwd()
setwd( "D:/DATA analytics/R/R PROJECT/Real estate")

## load the train and test data

housing.train=read.csv("housing_train.csv",stringsAsFactors = FALSE)

housing.test=read.csv("housing_test.csv",stringsAsFactors = FALSE)


## data preparation phase

housing.test$Price=NA

# put place holder for train and test data for identification
housing.train$data = "train"
housing.test$data = "test"


colnames(housing.train)
colnames(housing.test)

## rearrange colnames

housing.test = housing.test[,c(1,2,3,4,16,5:15,17)]


## combine data for data preparation
house = rbind(housing.train,housing.test)

glimpse(house)


## removing variable which has more then 1000 unique value
#unique(house$Address)
#unique(house$Landsize)

house = house %>%select (-Address,-Postcode,-Landsize)

glimpse(house)
############# create dummies
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



##sapply to get all the column names which are of character type
x =sapply(house,is.character)
x


cat.col=names(house)[x]
cat.col


###we dont want data column so will remove data column
cat.col=cat.col[-6]
cat.col


### create dummis 
for(col in cat.col){
  house=CreateDummies(house,col,100)
  
}

glimpse(house)



####### NA vale

lapply(house,function(x) sum(is.na(x)))

sum(sapply(house,function(x) is.character(x)))
# that tells us that there are no character columns remaining [ 1 comes for column 'data']

for(col in names(house)){
  
  if(sum(is.na(house[,col]))>0 & !(col %in% c("data","Price"))){
    
    house[is.na(house[,col]),col]=mean(house[house$data=='train',col],na.rm=T)
  }
  
}

## Sepeartion of data into train and test
housing.train=house%>% filter(data=='train') %>% select(-data)
housing.test=house%>% filter(data=='test') %>% select (-data,-Price)


## seprate train and test data from train_data
set.seed(2)
s=sample(1:nrow(housing.train),0.8*nrow(housing.train))
housing.training=housing.train[s,]
housing.testing=housing.train[-s,]



## VIF to remove multicolinearity between dependent variable

fit=lm(Price~.,data=housing.training)




library(car)

###multicolinearity & vif elimination


vif(fit)

sort(vif(fit),decreasing = T)

fit=lm(Price~.-CouncilArea_,data=housing.training)

sort(vif(fit),decreasing = T)


### vif elmination is done

summary(fit)

###round off the value and 999 is any number
options(scipen = 999)
summary(fit)


###now will eliminate the variable with highest P value , we  done mananually or we can use STEP function

#### now with the help of step function we remove variable which have probality greater than 0.05

fit=step(fit)



summary(fit)



formula(fit)


## copy the whole formula  function dont forget to add data=ld.train 1


fit =lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + BuildingArea + 
          YearBuilt + Suburb_Doncaster + Suburb_AscotVale + Suburb_Thornbury + 
          Suburb_Hampton + Suburb_Balwyn + Suburb_MalvernEast + Suburb_Camberwell + 
          Suburb_PortMelbourne + Suburb_Bentleigh + Suburb_BrightonEast + 
          Suburb_Hawthorn + Suburb_BalwynNorth + Suburb_Kew + Suburb_Brighton + 
          Suburb_Glenroy + Suburb_GlenIris + Suburb_Essendon + Suburb_SouthYarra + 
          Suburb_Preston + Suburb_Reservoir + Type_u + Type_h + Method_PI + 
          Method_S + SellerG_Kay + SellerG_McGrath + SellerG_Miles + 
          SellerG_Greg + SellerG_Sweeney + SellerG_RT + SellerG_Fletchers + 
          SellerG_Woodards + SellerG_Biggin + SellerG_Buxton + SellerG_Marshall + 
          SellerG_Jellis + CouncilArea_Whitehorse + CouncilArea_Brimbank + 
          CouncilArea_HobsonsBay + CouncilArea_Bayside + CouncilArea_Melbourne + 
          CouncilArea_Banyule + CouncilArea_PortPhillip + CouncilArea_Yarra + 
          CouncilArea_Maribyrnong + CouncilArea_Stonnington + CouncilArea_GlenEira + 
          CouncilArea_MooneeValley + CouncilArea_Moreland + CouncilArea_Boroondara,data=housing.training)

summary(fit)


############applying model housing.testing i.e validation part


val.pred=predict(fit,newdata=housing.testing)

## changing name for easy comparision

housing.testing$predictedPrice=val.pred

colnames(housing.testing)

## rearrange column
housing.testing = housing.testing[,c(1,3:82,2,83)]

View(housing.testing)


errors=housing.testing$Price-housing.testing$predictedPrice


#RMSE root mean sequred error
errors**2 %>% mean() %>% sqrt()

#RMSE 376519.1


##Score will be calculated as:Score =212467/RMSE 
#(Note : Dont worry about change in scoring method , this is just a cosmetic change to alter scale of score , passing criterion hasn't changed and you dont need to resubmit )

#Your score for test data should come out to be more than 0.51

### score can be calculated using the formula below and the number 212467 is used to remove scale factor
#Score = 212467/376519.1
#Score = 0.56

## As we have calculated score on testing_data and it is fine and above 0.51 and a better score


### ######################model for predcition on the entire train  data ld.train


final=lm(Price~.-CouncilArea_,data=housing.train)

final=step(final)

summary(final)


###applying model on test data

test.pred=predict(final,newdata=housing.test)


write.csv(test.pred,"karan_rajyaguru_P2_part2.csv",row.names = F)

####################################################################################################################

# Random Forest 

h.rf= randomForest(Price~.-CouncilArea_,data=housing.training)

h.rf

val.score=predict(h.rf,newdata =housing.testing)


rmse_val=((val.score)-(housing.testing$Price))^2 %>% mean() %>% sqrt()
rmse_val

#332270.1

##Score will be calculated as:Score =212467/RMSE 
#(Note : Dont worry about change in scoring method , this is just a cosmetic change to alter scale of score , passing criterion hasn't changed and you dont need to resubmit )

#Your score for test data should come out to be more than 0.51

### score can be calculated using the formula below and the number 212467 is used to remove scale factor
#Score = 212467/332270.1
#Score = 0.63

## As we have calculated score on housing.testing (validation) and it is fine and above 0.51 and a better score


## build this model on full train_data and the below is final model


h.rf.final= randomForest(Price~.-CouncilArea_,data=housing.train)

h.rf.final

## predict the Price for the test_data that is for housing_test.csv

val.score=predict(h.rf.final,newdata =housing.test)

write.csv(test.score,"karan_rajyaguru_P2_part2.csv",row.names = F)



## Variable IMportance

d=importance(h.rf.final)
d=as.data.frame(d)
d$VariableName=rownames(d)
d %>% arrange(desc(MeanDecreaseGini))

varImpPlot(h.rf.final)




