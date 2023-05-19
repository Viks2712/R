#reading train test files-
setwd(r'(D:\Data Science\R Course\Retail Project)')
s_train=read.csv("store_train.csv",stringsAsFactors = FALSE)

s_test=read.csv("store_test.csv",stringsAsFactors = FALSE)

#labeling train and test data-
s_test$store=NA
s_train$data="train"
s_test$data="test"
s=rbind(s_train,s_test)


library(dplyr)
library (tidyverse)
library(car)
library(visdat)

glimpse(s)

str(s)

table(s$country)

table(s$State)

s$store=as.factor(s$store)

glimpse(s)

#Dummies Function -->
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
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

names(s)[sapply(s,function(x) is.character(x))]

length(unique(s$countyname))

length(unique(s$storecode))

length(unique(s$Areaname))

length(unique(s$countytownname))

length(unique(s$state_alpha))

length(unique(s$store_Type))

s$storecode_NCNTY = replace(substr(s$storecode,1,5)=="NCNTY",1,0)

s$storecode_METRO = replace(substr(s$storecode,1,5)=="METRO",1,0)

s=s %>% select(-countyname,-Areaname,-countytownname-storecode)

cat_cols=c("state_alpha","store_Type")

for(cat in cat_cols){
  s=CreateDummies(s,cat,5) 
}

glimpse(s)

lapply(s,function(x) sum(is.na(x)))

for(col in names(s)){
  if(sum(is.na(s[,col]))>0 & !(col %in% c("data","store"))){
    s[is.na(s[,col]),col]=median(s[s$data=='train',col],na.rm=TRUE)
  }
}

lapply(s,function(x) sum(is.na(x)))


s_train=s %>% filter(data=="train") %>% select(-data)

s_test=s %>% filter(data=="test") %>% select(-data,-store)
# Split-->
set.seed(2)
s=sample(1:nrow(s_train),0.8*nrow(s_train))
s_train1=s_train[s,]
s_train2=s_train[-s,]

#Model Building-->
library(randomForest)

model_rf=randomForest(store~.-Id,data=s_train1,mtry=10,ntree=100)

model_rf

val.score=predict(model_rf,newdata=s_train2,type='response')

val.score2=predict(model_rf,newdata=s_train1,type='response')

library(caret)
confusionMatrix(val.score,s_train2$store)

val.prob_score=predict(model_rf,newdata=s_train2,type='prob')

val.prob_score2=predict(model_rf,newdata=s_train1,type='prob')

library(pROC)
auc_score=auc(roc(s_train2$store,val.prob_score[,1]))
auc_score

auc_score2=auc(roc(s_train1$store,val.prob_score2[,1]))
auc_score2

plot(roc(s_train2$store,val.prob_score[,1]))

#Final Fit-->

model_rf_final=randomForest(store~.-Id,data=s_train,mtry=10,ntree=100)

model_rf_final

test.score=predict(model_rf_final,newdata = s_test,type='prob')[,1]

test.score = round(test.score,0)

## Saving outcome in csv---> 

write.csv(test.score,'Vikram_Patil_P2_part21.csv',row.names = TRUE )


