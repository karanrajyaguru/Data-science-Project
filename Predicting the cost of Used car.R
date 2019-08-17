library(cvTools)
library(xgboost)
library(tree)
library(randomForest)
library(car)
library(pROC)
library(dplyr)
library(readxl)
library(writexl)
library(xlsx)

#xlsx -- requires rJava package
#openxlsx -- does not require rJava package
#readxl / writexl -- does not require rJava package

Predicting The Costs Of Used Cars - Hackathon By Imarticus Learning

##########################################################################################################
car.train = read_excel("Data_Train.xlsx")

car.test = read_excel("Data_test.xlsx")


car.test$Price = NA

getwd()

car.train$data='train'
car.test$data='test'



car=rbind(car.train,car.test)


glimpse(car)



#unique(car$Name)      ## name has more then 1000 unique name
#unique(car$Location)
#unique(car$Fuel_Type)
#unique(car$Mileage)

car=car %>% select(-Name)
glimpse(car)

## removing Lakh from variable New_Price and converting in to numeric
car=car %>%
  mutate(New_Price=as.numeric(gsub("Lakh","",New_Price)))


car=car[!(is.na(car$New_Price)),]

glimpse(car)
#################################

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
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}



#char.logical=sapply(car,is.character)

#char.logical


cat.cols=names(car)[sapply(car,is.character)]

cat.cols


###we dont want data column so will remove data column
cat.col=cat.cols[-8]
cat.col






# we are using frequency cutoff as 50,
for(col in cat.col){
  car=CreateDummies(car,col,50)
  
}

glimpse(car)

###############################NA Value

##for na value we can used any of the following code


###########

lapply(car,function(x) sum(is.na(x)))

for(col in names(car)){
  
  if(sum(is.na(car[,col]))>0 & !(col %in% c("data","Price"))){
    
    car[is.na(car[,col]),col]=mean(car[,col],na.rm=T)
  }
  
}

######


## separate train and test

car.train=car %>% filter(data=='train') %>% select(-data)

car.test=car %>% filter(data=='test') %>% select(-data,-Price)

##### vif check multicolineratiy

fit=lm(Price~.,data=car.train)

vif(fit)




####    RF #########
library(randomForest)
#if your data contain Na or missing values you can use this it will pass the data exactly the same as it is in datasets.

#rf<-randomForest(target~.,data=train,na.action = na.roughfix)

#car.rf= randomForest(Price~.,data=car.train,na.action = na.roughfix)


car.rf= randomForest(Price~.,data=car.train)
car.rf

val.score=predict(car.rf,newdata = car.train)

val.score[1:5]


rmse_val=((val.score)-(car.train$Price))^2 %>% mean() %>% sqrt()
rmse_val
#1.518076

test.score=predict(car.rf,newdata=car.test)

Price=test.score

Price[1:5]

#write.csv(test.score,"karan_rajyaguru_P2_part2.csv",row.names = F)


########## IMP################

#to write xlsx file we need to convert test.pred in to data frame..?


Price= as.data.frame(Price)
str(Price)


write_xlsx(Price,"Price.xlsx")









