#setting working directory-

setwd("D:/Data Science/R Course/Retail Project") 

#reading train test files-

store_train=read.csv("store_train.csv",stringsAsFactors = FALSE)

store_test=read.csv("store_test.csv",stringsAsFactors = FALSE)

#labeling train and test data-

store_test$store=NA
store_train$data='train'
store_test$data='test'

#combining both the data-
store_all=rbind(store_train,store_test)
head(store_all)

library(dplyr)
library (tidyverse)
library(car)
library(visdat)
#install.packages("visdat")
glimpse(store_all)

table(unique(store_train$storecode))
table(unique(store_train$state_alpha))

## if you look at variables storecode and state_alpha, you will notice that it has lot more unique values,having none of the individual values having frequency higher than a good number , we'll drop those vars.

#store_all=store_all |> select(-storecode)
#store_all=store_all |> select(-state_alpha)

#We'll create dummies for rest

char_logical=sapply(store_all,is.character)

cat_cols=names(store_all)[char_logical]
cat_cols=cat_cols[!(cat_cols %in% c('data','store'))]
cat_cols

#library(dplyr)

for(col in cat_cols){
  store_all=CreateDummies(store_all,col,50)
}
CreateDummies=function(data,var,freq_cutoff=100){
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
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    name=gsub("/","_",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

# replacing NA with mean in all cols except data and store col-->

store_all = store_all[!((is.na(store_all$store)) & store_all$data=='train'), ]
for(col in names(store_all)){
  if(sum(is.na(store_all[,col]))>0 & !(col %in% c("data","store"))){
    store_all[is.na(store_all[,col]),col]=mean(store_all[store_all$data=='train',col],na.rm=T)
  }
}

# Seperating train and test data->

store_train=store_all |>  filter(data=='train') |>  select(-data)
store_test=store_all |>  filter(data=='test') |>  select(-data,-store)

# lets remove variables which have redundant information first on the basis of vif

for_vif=lm(store~.-Id-`Areaname_AroostookCounty,ME`-`Areaname_Boston_Cambridge_Quincy,MA_NHHUDMetroFMRArea`-`Areaname_PenobscotCounty,ME(part)HUDMetroFMRArea`-storecode_NCNTY23003N23003-sales0-sales2-sales3-State -sales1 ,data=store_train)

sort(vif(for_vif),decreasing = TRUE)[1:3]

#alias(for_vif)

'''#alias(for_vif)
#library(car)
summary(for_vif)

names(store_train)

## I have removed aroostookCounty and 3 others as it has NA in estimates when I ran summary function
#now i can do VIF removal successfully

for_vif=lm(store~.-Id-Areaname_AroostookCountyME-Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea,data=store_train)

#library(car)

sort(vif(for_vif),decreasing = TRUE)[1:3]

for_vif=lm(store~.-Id-Areaname_AroostookCountyME-Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0,data=store_train)

sort(vif(for_vif),decreasing = TRUE)[1:3]

for_vif=lm(store~.-Id-Areaname_AroostookCountyME-Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2,data=store_train)

sort(vif(for_vif),decreasing = TRUE)[1:3]

for_vif=lm(store~.-Id-Areaname_AroostookCountyME-Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3,data=store_train)

sort(vif(for_vif),decreasing = TRUE)[1:3]

for_vif=lm(store~.-Id-Areaname_AroostookCountyME           -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1,data=store_train)

sort(vif(for_vif),decreasing = TRUE)[1:3]'''

##lets build our model now that all VIFs are below 5 with removing high p-values

#rm(for_vif)

fit=glm(store~.-Id-`Areaname_AroostookCounty,ME`-`Areaname_Boston_Cambridge_Quincy,MA_NHHUDMetroFMRArea`-`Areaname_PenobscotCounty,ME(part)HUDMetroFMRArea`-storecode_NCNTY23003N23003-sales0-sales2-sales3-State -sales1,data=store_train,family = 'binomial')


summary(fit)

fit=stats::step(fit)

formula(fit)
 
# final model fit-->

fit=glm(store ~ sales4 + CouSub + population + countyname_WorcesterCounty + 
          countyname_PenobscotCounty + storecode_METRO14460MM1120 + 
          state_alpha_WV + state_alpha_CA + state_alpha_CO + state_alpha_LA + 
          state_alpha_AL + state_alpha_PA + state_alpha_WI + state_alpha_AR + 
          state_alpha_OK + state_alpha_PR + state_alpha_MS + state_alpha_OH + 
          state_alpha_IN + state_alpha_TN + state_alpha_IL + state_alpha_KS + 
          state_alpha_MO + state_alpha_KY + state_alpha_GA + state_alpha_TX + 
          state_alpha_VT + state_alpha_NH + state_alpha_MA + state_alpha_ME,data=store_train,family = 'binomial')

summary(fit)

saveRDS(fit,file='D:/mylogit.RDS')

readRDS(file = 'D:/mylogit.RDS')
'''fit=glm(store~.-Id-Areaname_AroostookCountyME     -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1-State-store_Type_SupermarketType1,data=store_train)

summary(fit)

fit=glm(store~.-Id-Areaname_AroostookCountyME   -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-store_Type_SupermarketType1-store_Type_SupermarketType3,data=store_train)

summary(fit)

fit=glm(store~.-Id-Areaname_AroostookCountyME       -`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
       -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
       -State-store_Type_SupermarketType1-store_Type_SupermarketType3
       -store_Type_GroceryStore,data=store_train)

summary(fit)

#

fit=glm(store~.-Id-`Areaname_PenobscotCountyME(part)HUDMetroFMRArea`
        -Areaname_Boston_Cambridge_QuincyMA_NHHUDMetroFMRArea-sales0-sales2-sales3-sales1
        -store_Type_SupermarketType1-store_Type_SupermarketType3
        -store_Type_GroceryStore-State-state_alpha_FL-state_alpha_CT-storecode_NCNTY23003N23003-Areaname_AroostookCountyME,data=store_train)

summary(fit)'''

## predicting model on train data--->

options(scipen=999)

train.score=predict(fit,newdata = store_train,type='response')

train.score=round(train.score,0)

## predicting model on test data--->

test.score=predict(fit,newdata=store_test,type='response')

test.score = round(test.score,0)

## ROC/AUC--->

pROC::auc(pROC::roc(store_train$store,train.score)) 
#Trying XGBOOST--->
library(caret)
library(xgboost)

# Fit the model on the training set
set.seed(123)
model <- train(
  store ~ sales4 + CouSub + population + countyname_WorcesterCounty + 
    countyname_PenobscotCounty + storecode_METRO14460MM1120 + 
    state_alpha_WV + state_alpha_CA + state_alpha_CO + state_alpha_LA + 
    state_alpha_AL + state_alpha_PA + state_alpha_WI + state_alpha_AR + 
    state_alpha_OK + state_alpha_PR + state_alpha_MS + state_alpha_OH + 
    state_alpha_IN + state_alpha_TN + state_alpha_IL + state_alpha_KS + 
    state_alpha_MO + state_alpha_KY + state_alpha_GA + state_alpha_TX + 
    state_alpha_VT + state_alpha_NH + state_alpha_MA + state_alpha_ME, data = store_train, method = "xgbTree", na.action=na.exclude,
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter
model$bestTune

# Make predictions on the test data
predicted.classes <- model %>% predict(store_test)
score = round(predicted.classes,0)

## Saving outcome in csv---> 
write.csv(test.score,'Vikram_Patil_P2_part2.csv',row.names = FALSE)

