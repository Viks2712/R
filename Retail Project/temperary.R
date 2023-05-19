##1st in quiz----------------------------------------------------------------------------------
##------------------------------------Quiz----------------------------------------------------------------------------------
#(1) - what is the total sales (sum of all sales) of Supermarket Type1 in area Kennebec County, ME? 
glimpse(store_train)

store_train |> 
  filter(Areaname =="Kennebec County") |> 
  filter(store_Type =="Supermarket Type1") |> 
  summarise(Sales = sum(sales0,sales1,sales2,sales3,sales4))

#4th in quiz
#Ans - 38680

#(2) - Should storecode be included in building models?
#Ans - No

#(3) -  should country be treated as numeric type or character?
#Ans - character

#(4) - Find out number of unique categories of variable Areaname
length(unique(store_train$store_Type))--> 1891 
#Ans - 1891

#(5) - For store type grocery store what is the response rate ? 
#[ what % of obs have response value as 1 ]  Round off to two decimal digits. 
dim(store_train)
store_train %>%
  filter(store_Type =="Grocery Store") %>%
  filter(store ==1) 
182/3338
#Ans - 42.13

#6th\
#(6) - Do all the sales variable follow normal distribution?
library(ggplot2)
ggplot(store_train,aes(x=sales4))+geom_histogram()

#Ans - No

#(7) - Number of outliers for total sales based on following limits (q1-1.5*IQR, q3+1.5*IQR)?
#Ans - 140
total_sales = store_train$sales0+store_train$sales1+store_train$sales2+store_train$sales3+store_train$sales4

summary(total_sales)

IQR=4969-3422=1547

Q1-1.5*IQR
3422-1.5*1547=1101.5
Q3+1.5*IQR
4969+1.5*1547=7289

length(total_sales[total_sales>7289])
#=140

#(8) - which store type has maximum variance in total sales?
#Ans - Grocery Store
store_train |> mean(store_train) |> 
  group_by(store_train)


#(9) - How many dummies will you create for variable state_alpha?

unique(store_train$state_alpha)

#Ans - 53  as unique so n-1 variables  will be created by you

#(10) - What should be the type of categorical variable when using the function randomForest?
#Ans - factor

