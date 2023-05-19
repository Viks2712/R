#Q1. varience of price 
var(train$Price) -- 432958829215

#Q2.Find out how many observations have missing values for variable 'YearBuilt'?
sum(is.na(train$YearBuilt))--3717

#Q3.What is the difference in average price between house type h and t?
train |> group_by(Type) |> summarise(mean(Price))
1294320-901936=392384

#Q4.How many unique values variable postcode takes?
length(unique(train$Postcode))=94

#Q5.how should you treat post code . As a categorical variable or numeric variable ( write "categorical" or "numeric" as your answer)
  categorical

#Q6.Does distance follow a normal distribution?
  hist(train$Distance)==No

#Q7.Which seller has maximum value transactions? [ sum of price ]
b= train |> group_by(SellerG) |> summarise(sum(Price ))
View((b))
ans='Jellis'

#Q8. Which CouncilArea has maximum average price?
 library(dplyr)
train |>  group_by (CouncilArea) |>summarise( mean (Price) ) = "Bayside"

#Q9. which CouncilArea has maximum variance in the price?
train |> group_by (CouncilArea) |> summarise(var(Price))='Stonnington'

#Q10.Should we use Address as is in the modeling process?
No
  
  
