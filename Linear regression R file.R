setwd("C:\\Users\\PHANI KUMAR\\Desktop\\Machine Learning Projects\\Linear regression")

###########################Data cleaning########################

#Loading required packages
library(readxl)
library(Hmisc)
library(caret)
library(MASS)
library(car)

#reading data

mydata <- read_excel("Linear Regression Case.xlsx")

#removing irrelevant variables

mydata$custid <- NULL
mydata$birthmonth <- NULL

continuous_vars <- mydata[,c("age","ed","income","lninc","debtinc","creddebt","lncreddebt", "othdebt",
                             "lnothdebt","spoused","reside","pets","pets_cats","pets_dogs",
                             "pets_birds","pets_reptiles","pets_small","pets_saltfish","pets_freshfish",
                             "carditems","cardspent","card2items","card2spent","tenure", "longmon",
                             "lnlongmon", "longten","lnlongten","tollmon","lntollmon","tollten","lntollten",
                             "equipmon", "lnequipmon","equipten","lnequipten","cardmon","lncardmon",
                             "cardten","lncardten","wiremon","lnwiremon","wireten","lnwireten","owntv",
                             "hourstv")]

categorical_vars <- mydata[,c("region","townsize" ,"gender","agecat","edcat","jobcat","union","employ","empcat","retire",
                             "inccat","default","jobsat","marital","spousedcat","homeown","hometype","address","addresscat","cars"
                             ,"carown","cartype","carcatvalue","carbought","carbuy",'commute','commutecat',"commutecar","commutemotorcycle"
                             ,"commutecarpool","commutebus","commuterail","commutepublic","commutebike","commutewalk","commutenonmotor"
                             ,"telecommute","reason","polview","polparty","polcontrib","vote","card","cardtype","cardbenefit","cardfee","cardtenure"
                             ,"cardtenurecat","card2","card2type","card2benefit","card2fee","card2tenure","card2tenurecat","active","bfast"
                             ,"churn","tollfree","equip","callcard","wireless","multline","voice","pager","internet","callid","callwait","forward","confer"
                             ,"ebill","owntv","ownvcr","owndvd","owncd","ownpda","ownpc","ownipod","owngame","ownfax","news","response_01","response_02"
                             ,"response_03")]

continuous_vars$total_spend <- continuous_vars$cardspent + continuous_vars$card2spent
continuous_vars$cardspent <- NULL
continuous_vars$card2spent <- NULL


mydata <- cbind(continuous_vars,categorical_vars)

describe(mydata)


#user defined function to get the descriptive statistics

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

descriptive_stats <- data.frame(t(apply(continuous_vars,2,FUN = mystats)))

write.csv(descriptive_stats,"Descriptive_stats.csv")

################################Data Preparation##########################

#counting missing values

colSums(is.na(mydata))

replace_missing_cont <- function(x){
   x <- replace(x, is.na(x), mean(x, na.rm=TRUE))
   }

replace_missing_cat <- function(x){
  x <- as.character(x)
  x[is.na(x)] <- names(which.max(table(x)))
  return(factor(x))
}

#missing value treatment for continuous variables

continuous_vars <- apply(data.frame(continuous_vars), 2,FUN = replace_missing_cont)

continuous_vars <- data.frame(continuous_vars)

#missing value treatment for categorical variables

categorical_vars <- apply(data.frame(categorical_vars), 2,FUN = replace_missing_cat)

categorical_vars <- data.frame(categorical_vars)

mydata <- cbind(continuous_vars,categorical_vars)

colSums(is.na(mydata))
###########################################outlier treatment############################

#Outlier treatment(User defined function)
#UC = 0.99
#LC = 0.1

Outlier_treat <- function(x){
  quantiles = quantile(x,c(0.99,0.01),na.rm =T)
  x[x > quantiles[1]] = quantiles[1]
  x[x < quantiles[2]] = quantiles[2]
  return(x)
}

continuous_vars <- data.frame(apply(continuous_vars,2,FUN = Outlier_treat))

mydata <- cbind(continuous_vars,categorical_vars)

descriptive_stats <- data.frame(t(apply(continuous_vars,2,FUN = mystats)))

write.csv(descriptive_stats,"Descriptive_stats.csv")

#checking for normal distribution of dependent variable

hist(mydata$total_spend)

#using log to convert the skewed data to normally distributed form using log values

continuous_vars$log_total_spend <- log(continuous_vars$total_spend)

mydata <- cbind(continuous_vars,categorical_vars)

hist(continuous_vars$log_total_spend,main = "Log_total_spend",breaks = 6)

#Preparing correlation matrix

correlaton_matrix <- cor(continuous_vars)

#Exporting it to a csv file.

write.csv(correlaton_matrix,"Correlation matrix.csv")

#Performing ANOVA for continuous variable log total spend and the categorical variables

anova1 <- aov(total_spend~region+townsize +gender+agecat+edcat+jobcat+union+employ+empcat+retire
          +inccat+default+jobsat+marital+spousedcat+homeown+hometype+address+addresscat+cars
          +carown+cartype+carcatvalue+carbought+carbuy+commute+commutecat+commutecar+commutemotorcycle
          +commutecarpool+commutebus+commuterail+commutepublic+commutebike+commutewalk+commutenonmotor
          +telecommute+reason+polview+polparty+polcontrib+vote+card+cardtype+cardbenefit+cardfee+cardtenure
          +cardtenurecat+card2+card2type+card2benefit+card2fee+card2tenure+card2tenurecat+active+bfast
          +churn+tollfree+equip+callcard+wireless+multline+voice+pager+internet+callid+callwait+forward+confer
          +ebill+owntv+ownvcr+owndvd+owncd+ownpda+ownpc+ownipod+owngame+ownfax+news+response_01+response_02
          +response_03,data = mydata)

summary(anova1)

#Running regression

model_1 <- lm(log_total_spend~region+gender+agecat+edcat+jobcat+employ+empcat+retire+jobsat+inccat+
                reason+polview+commutenonmotor+card+card2+voice+internet+owngame+ownvcr+age+ed+
                income+lninc+debtinc +creddebt+lncreddebt+othdebt +lnothdebt+spoused+reside+pets +
                pets_cats+pets_dogs+pets_birds+pets_reptiles+pets_small+pets_saltfish+pets_freshfish+
                carditems+card2items+tenure+longmon+lnlongmon+longten+lnlongten+tollmon+
                lntollmon+tollten+lntollten+equipmon+lnequipmon+equipten+lnequipten+cardmon+lncardmon+
                cardten+lncardten +wiremon+lnwiremon+wireten +lnwireten+owntv+hourstv,data = mydata)


summary(model_1)

#Stepwise regression on model1S

step_wise <- step(model_1,direction = "both")

summary(step_wise)

#creating dummy variables for significant categorical variables

#Dummy variable for categorical variable gender(n-1 dummies)

dv = caret::dummyVars(~gender , data=mydata)
predict(dv, mydata)

dummy_cat = data.frame(predict(dv, mydata))

final_data <- cbind(mydata, dummy_cat)
final_data$gender.0 <- NULL
final_data$gender <- NULL

#Dummy variable for categorical variable edcat(n-1 dummies)

dv1 = caret::dummyVars(~edcat , data=mydata)
predict(dv1, mydata)

dummy_cat1 = data.frame(predict(dv1, mydata))

final_data <- cbind(final_data, dummy_cat1)
final_data$edcat <- NULL
final_data$edcat.3 <- NULL
final_data$edcat

#Dummy variable for categorical variable reason(n-1 dummies)

dv2 = caret::dummyVars(~reason , data=mydata)
predict(dv2, mydata)

dummy_cat2 = data.frame(predict(dv2, mydata))

final_data <- cbind(final_data, dummy_cat2)
final_data$reason <- NULL
final_data$reason.3 <- NULL

#Dummy variable for categorical variable card(n-1 dummies)

dv3 = caret::dummyVars(~card , data=mydata)
predict(dv3, mydata)

dummy_cat3 = data.frame(predict(dv3, mydata))

final_data <- cbind(final_data, dummy_cat3)
final_data$card <- NULL
final_data$card.1 <- NULL

#Dummy variable for categorical variable card2(n-1 dummies)

dv4 = caret::dummyVars(~card2 , data=mydata)
predict(dv4, mydata)

dummy_cat4 = data.frame(predict(dv4, mydata))

final_data <- cbind(final_data, dummy_cat4)
final_data$card2 <- NULL
final_data$card2.1 <- NULL


#splitingdata for building linear regression model
#data is split in the ratio of 70% and 30%
#training - 70%
#testing - 30%

set.seed(1137)
training_split <- sample(1:nrow(final_data), size = floor(0.70 * nrow(final_data)))

training_sample <- final_data[training_split,]

testing_sample <- final_data[-training_split,]

###########################################Building Final Linear Model#####################


#Building linear model on the training sample based on the significant variables in the above stepAIC

model_2 <- lm(log_total_spend~gender.1 + edcat.1+edcat.2+edcat.4+edcat.5 +reason.1+reason.2+reason.4+
                reason.9 +card.2+card.3+card.4+card.5+card2.2+card2.3+card2.4+card2.5+age+lninc+
                debtinc + creddebt + othdebt + pets_reptiles+carditems + card2items + longmon+
                lnlongmon + longten + equipten + cardmon + cardten,data = training_sample)

summary(model_2)

step_regression <- stepAIC(model_2,direction = "both")

#Final model based on significant variables

model_3 <- lm(log_total_spend~gender.1 +edcat.4 + 
                edcat.5 + reason.2 + reason.9 + card.2 + card.3 + card.4 + card.5 + card2.2 +
                card2.3 + card2.4 + card2.5 + age + lninc + carditems + card2items + 
                equipten + cardmon + cardten, data = training_sample)

step_regression2 <- stepAIC(model_3,direction = "both")

summary(step_regression2)

#Checking the variance inflation factor of the variables.

vif(step_regression2)

#VIF is between 1 and 5 so we can proceed with the above model.

#Predictions

#On training data

pred_total_spent = exp(predict(model_3))

pred_table <- cbind(training_sample,pred_total_spent)

pred_table2 <- pred_table

pred_table<- transform(pred_table, APE = abs(pred_total_spent - total_spend)/total_spend)

mean(pred_table$APE)

#On testing data

pred_total_spent_test = exp(predict(model_3,testing_sample))

pred_table_test <- cbind(testing_sample,pred_total_spent_test)

pred_table_testing <- transform(pred_table_test, APE = abs(pred_total_spent_test - total_spend)/total_spend)

mean(pred_table_testing$APE)

######################################Decile Analysis##################################

#On training Data

decLocations <- quantile(pred_table$pred_total_spent, probs = seq(0.1,0.9,by=0.1))


pred_table$decile <- findInterval(pred_table$pred_total_spent,c(-Inf,decLocations, Inf))

library(sqldf)

training_Decile_analysis <- sqldf("select decile, count(decile) as count, avg(pred_total_spent) as avg_pre_spent,   
               avg(total_spend) as avg_actual_spend
               from pred_table
               group by decile
               order by decile desc")

write.csv(training_Decile_analysis,"Training decile analysis.csv")

#For testing

decLocations2 <- quantile(pred_table_testing$pred_total_spent_test, probs = seq(0.1,0.9,by=0.1))


pred_table_testing$decile <- findInterval(pred_table_testing$pred_total_spent_test,c(-Inf,decLocations, Inf))


testing_Decile_analysis <- sqldf("select decile, count(decile) as count, avg(pred_total_spent_test) as avg_pre_spent_test,   
               avg(total_spend) as avg_actual_spend
               from pred_table_testing
               group by decile
               order by decile desc")

write.csv(testing_Decile_analysis,"Testing decile analysis.csv")
