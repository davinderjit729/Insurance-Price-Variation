
library(dplyr)
library(tidyverse)
library(ISLR2)
library(stargazer)
library(caret)
library(leaps)
library(readxl)
library(ggplot2)


data_complete <- read.csv("C:/Users/user/Downloads/data.csv")
str(data_complete)
head(data_complete)
 #cor.test(data_complete)
#Descriptive Analytics
summary(data_complete)

# finding missing value,no missing value
sum(is.na(data_complete)) 


#to find the correlation...
hist(data_complete$charges)

data_complete %>% ggplot(aes(x = age, y = charges)) + geom_point() + geom_smooth(method="lm", se=FALSE)

data_complete %>% ggplot(aes(x = bmi, y = charges)) + geom_point() + geom_smooth(method="lm", se=FALSE)

data_complete %>% ggplot(aes(x = children, y = charges)) + geom_point() + geom_smooth(method="lm", se=FALSE)

data_complete %>% ggplot(aes(x = smoker, y = charges)) + geom_point() + geom_smooth(method="lm", se=FALSE)

data_complete %>% ggplot(aes(x = gender, y = charges)) + geom_point() + geom_smooth(method="lm", se=FALSE)

data_complete %>% ggplot(aes(x = region, y = charges)) + geom_point() + geom_smooth(method="lm", se=FALSE)

data_complete %>% ggplot(aes(x = marital_status, y = charges)) + geom_point() + geom_smooth(method="lm", se=FALSE)


# corelation
cor(data_complete$charges, data_complete$age)
cor(data_complete$charges, data_complete$children)
cor(data_complete$charges, data_complete$smoker)
cor(data_complete$charges, data_complete$sex)
cor(data_complete$charges, data_complete$bmi)
cor(data_complete$charges, data_complete$southwest)
cor(data_complete$charges, data_complete$smoker)


# linear regression models to find the linearity or non-linearity...

linearmodel1<- lm(charges ~ age, data =data_complete)
coef(linearmodel1)
summary(linearmodel1)

linearmodel2<- lm(charges ~ age+smoker  , data =data_complete)
coef(linearmodel2)
summary(linearmodel2)

linearmodel3<- lm(charges ~ age+smoker+bmi  , data =data_complete)
coef(linearmodel3)
summary(linearmodel3)

linearmodel4<- lm(charges ~ age+smoker+bmi+children  , data =data_complete)
coef(linearmodel4)
summary(linearmodel4)

linearmodel5<- lm(charges ~ age+smoker+bmi+children+sex  , data =data_complete)
coef(linearmodel5)
summary(linearmodel5)

linearmodel6<- lm(charges ~ age+smoker+sex+bmi+children+southwest+northwest+southeast+northeast
                  , data =data_complete)
coef(linearmodel6)
summary(linearmodel6)

# To check the normality of the model choosen

linearmodel6_resides <- linearmodel6$residuals

hist(linearmodel6_resides)

qqnorm(linearmodel6_resides)

linearmodel6_fitted <- linearmodel6$fitted.values

plot(linearmodel6_fitted,linearmodel6_resides)

# residuals..

linearmodel5_resides <-linearmodel5$residuals

hist(linearmodel5_resides)

qqnorm(linearmodel5_resides)

linearmodel5_fitted <- linearmodel5$fitted.values

plot(linearmodel5_fitted,linearmodel5_resides)


#finding interactions in variables

model1<- lm(charges ~ age+smoker+age:smoker  , data =data_complete)
coef(model1)
summary(model1)


model2<- lm(charges ~ bmi+smoker+bmi:smoker , data =data_complete)
coef(model2)
summary(model2)

model3<- lm(charges ~ age+bmi:smoker+smoker+sex+bmi+children+southwest+northwest+southeast+northeast
            , data =data_complete )
coef(model3)
summary(model3)


# used polynomianl regression model....

polynomianlmodel <- lm(charges ~ age+smoker+sex+children+southwest+northwest+southeast+northeast
                          + I(bmi^2),data = data_complete)
summary(polynomianlmodel)


set.seed(12)

# train the model which is looking better...


linearmodel6train <- train(  form = charges ~ age+smoker+sex+bmi+children+southwest+northwest+southeast+northeast
  , data =data_complete,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
linearmodel6train

linearmodeltrytrain <- train(
  form = charges ~ age+bmi:smoker+smoker+sex+bmi+children+southwest+northwest+southeast+northeast
  , data =data_complete,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
linearmodeltrytrain

linearmodel2train <- train(
  form = charges ~ bmi+smoker+bmi:smoker 
  , data =data_complete,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
linearmodel2train


polynomianlmodeltrain <- train(
  form = charges ~ age+smoker+sex+children+southwest+northwest+southeast+northeast
  + I(bmi^2),data = data_complete,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

polynomianlmodeltrain

#variable selection...


subsetmodel <- regsubsets(charges ~ age+smoker+sex+bmi+children+southwest+northwest+southeast+northeast
                           , data =data_complete)
summary(subsetmodel)
plot(subsetmodel, scale = "adjr2")


subsetmodel1 <- regsubsets(charges ~ age+smoker+sex+children+southwest+northwest+southeast+northeast
                          + I(bmi^2),data = data_complete)
summary(subsetmodel1)
plot(subsetmodel, scale = "adjr2")

subsetmodel2 <- regsubsets(charges ~ age+smoker+age:smoker  , data =data_complete)
summary(subsetmodel2)
plot(subsetmodel2, scale = "adjr2")


subsetmodel3 <- regsubsets(charges ~ bmi+smoker+bmi:smoker , data =data_complete)
summary(subsetmodel3)
plot(subsetmodel3, scale = "adjr2")

# regression model for final selection...


forward_model <- step(model3, direction = "forward", criterion = "AIC")



finalmodel<- lm(charges ~ age + smoker + sex + bmi + children + southwest + northwest + 
                    southeast+bmi:smoker , data =data_complete)
coef(finalmodel)
summary(finalmodel)

finalmodeltrain <- train(
  form = charges ~ (age + smoker + sex + bmi + children + southwest + northwest + 
            southeast+bmi:smoker) , data =data_complete,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

finalmodeltrain

finaldata <- read.csv("C:/Users/user/Downloads/finaldata.csv")
View(finaldata)
preds <- predict(model3, newdata=finaldata)
preds



    