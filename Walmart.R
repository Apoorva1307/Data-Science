#Import the dataset Walmart_Sales_store.csv from the location it saved in your computer

read.csv("Walmart_Store_sales.csv")->walmart
View(walmart)
head(walmart)

library(dplyr)
library(corrplot)
library(reshape2)
library(caTools)

#Analysis Tasks
#Basic Statistics tasks

#Q.1. Which store has maximum sales?

walmart%>%group_by(Store)%>%
  summarise(Sales = sum(Weekly_Sales))%>%
  mutate(Rank = rank(desc(Sales)))%>%filter(Rank==1)

#Q.2. Which store has maximum standard deviation?
#i.e., the sales vary a lot. 
#Also, find out the coefficient of mean to standard deviation

walmart%>%group_by(Store)%>%
  summarise(Dev_Sales = sd(Weekly_Sales), Mean_Sales = mean(Weekly_Sales))%>%
  mutate(cv = Dev_Sales*100/Mean_Sales)%>%
  mutate(Rank = rank(desc(Dev_Sales)))%>%filter(Rank==1)  

#Q.3. Which store/s has good quarterly growth rate in Q3'2012?

as.Date(walmart$Date, format = "%d-%m-%Y")->walmart$Date
format.Date(x = walmart$Date, "%m")->walmart$Month
format.Date(x = walmart$Date, "%Y")->walmart$Year

data.frame(lapply(walmart[,c("Month", "Year")], as.numeric))->walmart[,c("Month", "Year")]
str(walmart)
v = c(1,3, 6, 9, 12)

cut(walmart$Month, breaks = v, labels = c(1,2,3,4), 
    include.lowest = T, ordered_result = T)->walmart$Quarters

walmart%>%filter(Year ==2012)%>%group_by(Store,Quarters)%>%
  summarize(Sales = sum(Weekly_Sales))%>%filter(Quarters%in% c(2,3))->res

dcast(res, Store ~ Quarters, value.var = "Sales")->d
head(d)
names(d)[c(2,3)] = c("Q2", "Q3")
d%>%mutate(Growth = (Q3-Q2)*100/Q2)%>%mutate(Rank = rank(desc(Growth)))%>%filter(Rank==1)

#Q.4. Some holidays have a negative impact on sales.
#Find out holidays which have higher sales than the mean sales 
#in non-holiday season for all stores together.

walmart = as_tibble(walmart)

#mean sales in non-holiday season for all stores together
walmart%>%filter(Holiday_Flag==0)%>%summarize(mean(Weekly_Sales))->x
x[1,1]

#those holidays which have higher sales than the above value
walmart%>%filter(Holiday_Flag==1)%>%filter(Weekly_Sales>x[1,1])

#Q.5. Provide a monthly and semester view of sales in units and give insights

walmart%>%group_by(Month, Year)%>%
  summarise(AveSales = mean(Weekly_Sales))%>%arrange(Year, Month)%>%
  mutate(M_Y = paste(Month, Year, sep = "/"))

#Building a Linear Regression model

#Q. Restructure dates as 1 for 5 Feb 2010 
#(starting from the earliest date in order)

as.numeric(format.Date(walmart$Date, "%d"))->walmart$Day
walmart$Day
ifelse(walmart$Day<=7, 1,
       ifelse(walmart$Day<=14,2,
              ifelse(walmart$Day<=21,3,
                     ifelse(walmart$Day<=28,4,5))))->walmart$WeekNum

#Q. Change dates into days by creating new variable.

as.numeric(format.Date(walmart$Date, "%d"))->walmart$Day
format.Date(walmart$Date, "%A")->walmart$WeekDay
walmart$WeekDay

#looking for rows with data-entry error
#i.e. in this case negative data
which(is.na(walmart$Weekly_Sales))
#since there is no negative data, no need to delete rows


str(walmart)
walmart$Date = NULL
walmart$Holiday_Flag = as.numeric(walmart$Holiday_Flag)
walmart$Quarters = as.numeric(walmart$Quarters)
walmart$Store = as.factor(walmart$Store)
w = walmart

model.matrix(w$Weekly_Sales~., data = w)->mat
mat
mat = mat[,-1]
df = as.data.frame(mat)
head(df)
cbind(w$Weekly_Sales, df)->w
names(w)[1] = "Weekly_Sales"  
head(w)

set.seed(100)
split = sample.split(Y = w$Weekly_Sales, SplitRatio = .7)  
training = w[split,]  
test = w[!split,]

lm(formula = Weekly_Sales~., data = training)->model
summary(model)  
step(model, direction = "both")->mod
summary(mod)

predict(mod, newdata = test)->p
e = p - test$Weekly_Sales
sse = sum(e^2)
sst = sum((test$Weekly_Sales-mean(training$Weekly_Sales))^2)
rsq = 1-sse/sst
rsq

#Linear Regression Model for Store 1 only
walmart%>%filter(Store == 1)->w
model.matrix(w$Weekly_Sales~.,data=w)->mat
mat = mat[,-1]
df = as.data.frame(mat)
head(df)
cbind(w$Weekly_Sales, df)->w
names(w)[1] = "Weekly_Sales"
head(w)


set.seed(100)
split = sample.split(Y = w$Weekly_Sales, SplitRatio = .7)
training = w[split,]
test = w[!split,]

lm(formula = Weekly_Sales~., data = training)->model
summary(model)  
step(model, direction = "both")->mod
summary(mod)

predict(mod, newdata = test)->p
e = p - test$Weekly_Sales
sse = sum(e^2)
sst = sum((test$Weekly_Sales-mean(training$Weekly_Sales))^2)
rsq = 1-sse/sst
rsq

#Demand forecasting for Store 1
head(walmart)
walmart%>%filter(Store==1)%>%select(Weekly_Sales)->store1
store1
length(store1$Weekly_Sales)
plot(1:length(store1$Weekly_Sales), store1$Weekly_Sales)
