#load library
library(tidyverse)
getwd()
setwd("D:/Data Science/R Course/")
#loading dataset
train <- read.csv("D:/Data Science/R Course/Real Estate Project/housing_train.csv")
test <- read.csv("D:/Data Science/R Course/Real Estate Project/housing_test.csv")
head(train)
head(test)

length(colnames(test))

#checking NA values in train dataset and replacing them
colSums(is.na(train))

train$Bedroom2[is.na(train$Bedroom2)]=median(train$Bedroom2,na.rm=TRUE)

train$Bathroom[is.na(train$Bathroom)]=round(median(train$Bathroom,na.rm=TRUE),0)

train$Car[is.na(train$Car)]=round(median(train$Car,na.rm=TRUE),0)

train$Landsize[is.na(train$Landsize)]=round(mean(train$Landsize,na.rm=TRUE),0)

train$BuildingArea[is.na(train$BuildingArea)]=round(mean(train$BuildingArea,na.rm=TRUE),0)

train$YearBuilt[is.na(train$YearBuilt)]=round(median(train$YearBuilt,na.rm=TRUE),0)

colSums(is.na(train))

#checking NA values in test dataset and replacing them
colSums(is.na(test))

test$Bedroom2[is.na(test$Bedroom2)]=median(train$Bedroom2,na.rm=TRUE)

test$Bathroom[is.na(test$Bathroom)]=round(median(train$Bathroom,na.rm=TRUE),0)

test$Car[is.na(test$Car)]=round(median(train$Car,na.rm=TRUE),0)

test$Landsize[is.na(test$Landsize)]=round(mean(train$Landsize,na.rm=TRUE),0)

test$BuildingArea[is.na(test$BuildingArea)]=round(mean(train$BuildingArea,na.rm=TRUE),0)

test$YearBuilt[is.na(test$YearBuilt)]=round(median(train$YearBuilt,na.rm=TRUE),0)

colSums(is.na(test))

#Combining both data

test$Price=NA
train$data='train'
test$data='test'
all_data=rbind(train,test)
apply(all_data,2,function(x)sum(is.na(x)))

glimpse(all_data)

t=table(all_data$Suburb)
#View(t)

t1=round(tapply(all_data$Price,all_data$Suburb,mean,na.rm=TRUE),0)
#View(t1)
t1=sort(t1)

all_data=all_data %>% 
  mutate(
    sub_1=as.numeric(Suburb %in% c("Campbellfield","Jacana")),
    sub_2=as.numeric(Suburb%in%c("Kealba","Brooklyn","Albion","Sunshine West","Ripponlea","Fawkner")),
    sub_3=as.numeric(Suburb%in%c("Glenroy","Southbank","Sunshine North","Keilor Park","Heidelberg West","Reservoir","Braybrook","Kingsbury","Gowanbrae","Hadfield","Watsonia","Footscray","South Kingsville","Balaclava","Melbourne","Maidstone","Sunshine")),
    sub_4=as.numeric(Suburb%in%c("Airport West","Heidelberg Heights","Pascoe Vale","West Footscray","Altona North","Williamstown North","Brunswick West","Keilor East","Oak Park","Maribyrnong","Altona","Flemington","Coburg North","Yallambie","Avondale Heights","Bellfield")),
    sub_5=as.numeric(Suburb%in%c("Strathmore Heights","Glen Huntly","Kensington","Essendon North","St Kilda","Preston","North Melbourne","Coburg","Kingsville","Collingwood","Brunswick East","Gardenvale","Thornbury","Niddrie","West Melbourne","Viewbank")),
    sub_6=as.numeric(Suburb%in%c("Spotswood","Carnegie","Elwood","Heidelberg","Moorabbin","Oakleigh","Rosanna","Docklands","Yarraville","Cremorne","Seddon","Brunswick","Oakleigh South","Ascot Vale","Windsor","Caulfield","Essendon West","Newport")),
    sub_7=as.numeric(Suburb%in%c("Chadstone","South Yarra","Essendon","Bentleigh East","Murrumbeena","Hughesdale","Fairfield","Ashwood","Clifton Hill","Caulfield North","Abbotsford","Carlton","Prahran","Fitzroy","Ivanhoe","Hampton East","Caulfield East")),
    sub_8=as.numeric(Suburb%in%c("Richmond","Travancore","Templestowe Lower","Ormond","Caulfield South","Moonee Ponds","Hawthorn","Box Hill","Bulleen","Burnley","Burwood","Strathmore","Port Melbourne","Fitzroy North","Alphington")),
    sub_9=as.numeric(Suburb%in%c("Doncaster","South Melbourne","Northcote","Aberfeldie","Elsternwick","Bentleigh","Kooyong","Parkville")),
    sub_10=as.numeric(Suburb%in%c("Williamstown","East Melbourne","Seaholme")),
    sub_11=as.numeric(Suburb%in%c("Malvern East","Carlton North","Hawthorn East","Surrey Hills")),
    sub_12=as.numeric(Suburb%in%c("Princes Hill","Mont Albert","Armadale","Kew East","Glen Iris","Ashburton")),
    sub_13=as.numeric(Suburb%in%c("Brighton East","Eaglemont","Hampton")),
    sub_14=as.numeric(Suburb%in%c("Toorak","Ivanhoe East","Camberwell","Balwyn North","Kew")),
    sub_15=as.numeric(Suburb%in%c("Brighton","Middle Park")),
    sub_16=as.numeric(Suburb%in%c("Albert Park","Balwyn","Malvern"))
  ) %>% 
  select(-Suburb)

glimpse(all_data)

all_data=all_data %>% 
  select(-Address)

table(all_data$Type)

all_data=all_data %>%
  mutate(Type_t=as.numeric(Type=="t"),
         Type_u=as.numeric(Type=="u")
         )

all_data=all_data %>% 
  select(-Type)

table(all_data$Method)

all_data=all_data %>%
  mutate(Method_PI=as.numeric(Method=="PI"),
         Method_S=as.numeric(Method=="S"),
         Method_SP=as.numeric(Method=="SP"),
         Method_VB=as.numeric(Method=="VB" )
         ) %>% 
  select(-Method)

t=table(all_data$SellerG)
sort(t)

all_data=all_data %>%
  mutate(Gnelson=as.numeric(SellerG=="Nelson"),
         GJellis=as.numeric(SellerG=="Jellis"),
         Ghstuart=as.numeric(SellerG=="hockingstuart"),
         Gbarry=as.numeric(SellerG=="Barry"),
         GMarshall=as.numeric(SellerG=="Marshall"),
         GWoodards=as.numeric(SellerG=="Woodards"),
         GBrad=as.numeric(SellerG=="Brad"),
         GBiggin=as.numeric(SellerG=="Biggin"),
         GRay=as.numeric(SellerG=="Ray"),
         GFletchers=as.numeric(SellerG=="Fletchers"),
         GRT=as.numeric(SellerG=="RT"),
         GSweeney=as.numeric(SellerG=="Sweeney"),
         GGreg=as.numeric(SellerG=="Greg"),
         GNoel=as.numeric(SellerG=="Noel"),
         GGary=as.numeric(SellerG=="Gary"),
         GJas=as.numeric(SellerG=="Jas"),
         GMiles=as.numeric(SellerG=="Miles"),
         GMcGrath=as.numeric(SellerG=="McGrath"),
         GHodges=as.numeric(SellerG=="Hodges"),
         GKay=as.numeric(SellerG=="Kay"),
         GStockdale=as.numeric(SellerG=="Stockdale"),
         GLove=as.numeric(SellerG=="Love"),
         GDouglas=as.numeric(SellerG=="Douglas"),
         GWilliams=as.numeric(SellerG=="Williams"),
         GVillage=as.numeric(SellerG=="Village"),
         GRaine=as.numeric(SellerG=="Raine"),
         GRendina=as.numeric(SellerG=="Rendina"),
         GChisholm=as.numeric(SellerG=="Chisholm"),
         GCollins=as.numeric(SellerG=="Collins"),
         GLITTLE=as.numeric(SellerG=="LITTLE"),
         GNick=as.numeric(SellerG=="Nick"),
         GHarcourts=as.numeric(SellerG=="Harcourts"),
         GCayzer=as.numeric(SellerG=="Cayzer"),
         GMoonee=as.numeric(SellerG=="Moonee"),
         GYPA=as.numeric(SellerG=="YPA")
  ) %>% 
  select(-SellerG)

glimpse(all_data)

table(all_data$CouncilArea)

all_data=all_data %>%
  mutate(CA_Banyule=as.numeric(CouncilArea=="Banyule"),
         CA_Bayside=as.numeric(CouncilArea=="Bayside"),
         CA_Boroondara=as.numeric(CouncilArea=="Boroondara"),
         CA_Brimbank=as.numeric(CouncilArea=="Brimbank"),
         CA_Darebin=as.numeric(CouncilArea=="Darebin"),
         CA_Glen_Eira=as.numeric(CouncilArea=="Glen Eira"),
         CA_Monash=as.numeric(CouncilArea=="Monash"),
         CA_Melbourne=as.numeric(CouncilArea=="Melbourne"),
         CA_Maribyrnong=as.numeric(CouncilArea=="Maribyrnong"),
         CA_Manningham=as.numeric(CouncilArea=="Manningham"),
         CA_Kingston=as.numeric(CouncilArea=="Kingston"),
         CA_Hume=as.numeric(CouncilArea=="Hume"),
         CA_HobsonsB=as.numeric(CouncilArea=="Hobsons Bay"),
         CA_MoonValley=as.numeric(CouncilArea=="Moonee Valley"),
         CA_Moreland=as.numeric(CouncilArea=="Moreland"),
         CA_PortP=as.numeric(CouncilArea=="Port Phillip"),
         CA_Stonnington=as.numeric(CouncilArea=="Stonnington"),
         CA_Whitehorse=as.numeric(CouncilArea=="Whitehorse"),
         CA_Yarra=as.numeric(CouncilArea=="Yarra")) %>% 
  select(-CouncilArea)

glimpse(all_data)

# Splitting data in train and test
train=all_data %>% 
  filter(data=='train') %>% 
  select(-data)


test=all_data %>% 
  filter(data=='test') %>% 
  select(-data,-Price)

glimpse(train)
glimpse(test)

# train data splitting into 75-25
set.seed(123)
s=sample(1:nrow(train),0.75*nrow(train))
train_75=train[s,] 
test_25=train[-s,] 

library(car)

#finding multicollinearity with vif

LRf=lm(Price ~ .-Postcode,data=train_75)

summary(LRf)

sort(vif(LRf),decreasing = TRUE)[1:3]

LRf=lm(Price ~ .-Postcode-Method_S-sub_3 ,data=train_75)

sort(vif(LRf),decreasing = TRUE)[1:3]

#fitting to the model as vif is under 4

LRf=lm(Price ~ .-Postcode-Method_S-sub_3,data=train_75)

summary(LRf)

#stepping down variables

LRf=stats::step(LRf)

#final fit

LRf = lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + 
           BuildingArea + YearBuilt + sub_1 + sub_2 + sub_5 + sub_6 + 
           sub_7 + sub_8 + sub_9 + sub_10 + sub_11 + sub_12 + sub_13 + 
           sub_14 + sub_15 + sub_16 + Type_t + Type_u + Method_PI + 
           Method_SP + Method_VB + Gnelson + GJellis + Ghstuart + GMarshall + 
           GRT + GMiles + GMcGrath + GKay + GStockdale + GDouglas + 
           GRaine + GChisholm + GCollins + CA_Banyule + CA_Bayside + 
           CA_Boroondara + CA_Brimbank + CA_Darebin + CA_Monash + CA_Melbourne + 
           CA_Maribyrnong + CA_Manningham + CA_Kingston + CA_HobsonsB + 
           CA_MoonValley + CA_Moreland + CA_PortP + CA_Stonnington + 
           CA_Yarra)

PP_test_25=predict(LRf,newdata =test_25)
PP_test_25=round(PP_test_25,1)
class(PP_test_25)

plot(test_25$Price,PP_test_25)


res=test_25$Price-PP_test_25 

RMSE_test_25=sqrt(mean(res^2))
RMSE_test_25
212467/RMSE_test_25

library(ggplot2)
d=data.frame(real=test_25$Price,predicted=PP_test_25)
ggplot(d,aes(x=real,y=predicted))+geom_point()

plot(LRf,which = 1)
plot(LRf,which = 2) 
plot(LRf,which = 3)
plot(LRf,which = 4)

PP_test_final=predict(LRf,newdata =test)
PP_test_final=round(PP_test_final,1)
class(PP_test_final)

write.csv(PP_test_final, 'Vikram_Patil_P1_part2.csv',row.names = FALSE)
summary(LRf)


var(train$Price)
sum(is.na(train$YearBuilt))
