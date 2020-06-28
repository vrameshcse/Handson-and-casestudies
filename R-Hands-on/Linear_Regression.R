library(MASS)
library(car)
library(ggplot2)
library(GGally)

##################Checkpoint 1: Business Understanding and Data Understanding##########

# Business understanding #
# Business Objective: Mycar Dream wants to automate the process of predicting the car mileage
# which fits the customer preferences, based on the dataset of car features
# and attributes obtained by market surveys

# Assignment objective: The model should not contain more than 5 variables.
# According to the business needs, set the VIF to 2.
# The model should be highly predictive in nature
# i.e it should show 80% (R squared) of accuracy.

# 1	Mpg	Mileage per gallon (continuous variable)
# 2	Cylinders	Number of cylinders in car (multi-valued discrete)
# 3	Displacement	Volume of fuel inside the engine i.e size of engine (continuous )
# 4	Horsepower	picks up of the car (continuous)
# 5	Weight	Weight of car (continuous)
# 6	Acceleration	Acceleration of car (continuous)
# 7	Model year	Year when the car launched (multi-valued discrete)
# 8	Origin	Origin of car (multi-valued discrete)
# 9	Car name	Name of car company (unique for each instance)

#Response variable: MPG
#Values: 8-46

##Data understanding ##
carmileage <-
  read.csv(
    "carMPG.csv",
    stringsAsFactors = F,
    blank.lines.skip = T,
    na.strings = "NA"
  )

View(carmileage)

str(carmileage)
# we could see there are no missing values, but there are inconsitent values in the
# horsepower attribute. Also we see few attributes have incorrect data types which
# has to be handled. Based on the data understnding we have to check for outliers
# for the below continuous attributes
# Displacement(continuous )
# Horsepower(continuous)
# Weight(continuous)
# Acceleration(continuous)


nrow(unique(carmileage))
# gives 398 observations which indicates there are no duplicate observations

sum(is.na(carmileage))
#Observation: 0. It indicates there are no attributes with na values.

quantile(carmileage$Displacement, seq(0, 1, 0.01))
boxplot(carmileage$Displacement)
# Observation: There are no sudden increase in any of the percentiles also the
# box plot indicates there are no outliers

quantile(as.numeric(carmileage$Horsepower[carmileage$Horsepower != "?"]), seq(0, 1, 0.01))
boxplot(as.numeric(carmileage$Horsepower[carmileage$Horsepower != "?"]))
boxplot.stats(as.numeric(carmileage$Horsepower[carmileage$Horsepower != "?"]))$out
# As the data type is incorrect, it has to be converted to right type before doing
# any analysis. Hence its converted to appropriate type for analysis.
# Observation: We say ther is a sudden increase after 98% and hence it has be
# treated before preparing the model. That will be done in the data preparation

quantile(carmileage$Weight, seq(0, 1, 0.01))
boxplot(carmileage$Weight)
# Observation: There are no sudden increase in any of the percentiles also the
# box plot indicates there are no outliers

quantile(carmileage$Acceleration, seq(0, 1, 0.01))
boxplot(carmileage$Acceleration)
boxplot(carmileage$Acceleration)$out
# Observation: we see there is an increase in the values at 1% and after 99% and hence it has
# to be treated before preparing the model which will be handled in
# data prepartion

########################## Data Preparation ###############################
# Below tasks has to be accomplished based on data understanding
# 1. incosistent data type treatment for variables
# 2. Missing value treatment for horsepower
# 3. Outlier treatment for horsepower and acceleration variables
# 4. synonyms treatment for car name variable
# 5. Transforming variables

# First stage: Variables Formatting
# converting the type of variables
str(carmileage)
# Observation: cylinders, model_year and origin are categorical variables and
# hence has to be converted to factor variables for better understanding
# of data
carmileage$Cylinders <- as.factor(carmileage$Cylinders)
summary(carmileage$Cylinders)

carmileage$Origin <- as.factor(carmileage$Origin)
summary(carmileage$Origin)

summary(carmileage$Car_Name)
# Observation: we see there are synonym values that needs to be treated in the car names
# attribute.

# extract the first word on the car name attribute and assign it to the
# car brand.
carmileage$car_brand <-
  sapply(strsplit(carmileage$Car_Name, split = " ", fixed = T), "[", 1)

summary(carmileage$car_brand)
# We now see synonym variable existing, eg: maxda and mazda are of same type
# but it is spelled differently.
carmileage$car_brand <-
  ifelse(carmileage$car_brand == "chevroelt",
         "chevrolet",
         carmileage$car_brand)
carmileage$car_brand <-
  ifelse(carmileage$car_brand == "chevy",
         "chevrolet",
         carmileage$car_brand)
carmileage$car_brand <-
  ifelse(carmileage$car_brand == "maxda", "mazda", carmileage$car_brand)
carmileage$car_brand <-
  ifelse(carmileage$car_brand == "toyouta",
         "toyota",
         carmileage$car_brand)
carmileage$car_brand <-
  ifelse(carmileage$car_brand == "mercedes-benz",
         "mercedes",
         carmileage$car_brand)
carmileage$car_brand <-
  ifelse(carmileage$car_brand == "vokswagen",
         "volkswagen",
         carmileage$car_brand)
carmileage$car_brand <-
  ifelse(carmileage$car_brand == "vw",
         "volkswagen",
         carmileage$car_brand)

carmileage$car_brand <- as.factor(carmileage$car_brand)
summary(carmileage$car_brand)
#finally check the summary to see if the variables are mapped to correct
#data type
summary(carmileage)
# Horsepower has to be converted to appropriate data type after doing the data
#cleaning

#converting the year variable from categorical to numerical and
# reducing the dimension for better analysis
aggregate(carmileage$MPG,
          by = list(carmileage$Model_year),
          FUN = mean)
# Based on the aggregate function we could conclude that
# Years 2003-2005,2006-2011,2012-2015 has similar MPG and can be grouped together
# for better analysis

carmileage$Yr2003_2005 <- ifelse(carmileage$Model_year < 2006, 1, 0)
carmileage$Yr2006_2011 <-
  ifelse(carmileage$Model_year > 2005 &
           carmileage$Model_year < 2012, 1, 0)
carmileage$Yr2012_2015 <- ifelse(carmileage$Model_year > 2011, 1, 0)

#second stage: Data Cleaning

# Missing value treatement for horsepower
# Since there are no patterns to the missing value, we will have to remove
# the missing value observations to avoid bias
carmileage <- carmileage[carmileage$Horsepower != "?", ]

# inconsitent data type treatment
carmileage$Horsepower <- as.numeric(carmileage$Horsepower)

summary(carmileage)

# Outlier treatment
# Based on the data understanding, we have to treat horsepower and acceleration for
# outlier treatment.

# outlier treatment for horsepower
quantile(carmileage$Horsepower, seq(0, 1, 0.01))
# Observation: we see the outliers are present after 98%, so hence capping the
# the last two percentiles to the 98th percentile value ie., 210.90
carmileage$Horsepower[carmileage$Horsepower > 210.90] <- 210.90
# check the quantile to see if the outliers have been capped
quantile(carmileage$Horsepower, seq(0, 1, 0.01))

# outlier treatment for acceleration
quantile(carmileage$Acceleration, seq(0, 1, 0.01))
# Observation: we see there are outliers at both ends
# so the outliers are replaced with lower and upper whisher values
lowerend <- boxplot.stats(carmileage$Acceleration)$stats[1]
higherend <- boxplot.stats(carmileage$Acceleration)$stats[5]

carmileage$Acceleration[carmileage$Acceleration > higherend] <-
  higherend
carmileage$Acceleration[carmileage$Acceleration < lowerend] <-
  lowerend
# check the quantile to see if the outliers have been capped and floored
quantile(carmileage$Acceleration, seq(0, 1, 0.01))
boxplot(carmileage$Acceleration)

# Third stage:  Variables Transformation
# There are 4 categorical variables that needs to be transformed so that it can be
# used in data modeling for analysis.
#use model.matrix command to convert the factor variables in dummy variables
# dummy <-
#   sapply(factor_carmileage, function(x)
#     data.frame(model.matrix( ~ x - 1, data = factor_carmileage)))
dummy1 <- data.frame(model.matrix( ~ Cylinders, data = carmileage))
dummy1 <- dummy1[, -1]

dummy2 <- data.frame(model.matrix( ~ Origin, data = carmileage))
dummy2 <- dummy2[, -1]

dummy3 <- data.frame(model.matrix( ~ car_brand, data = carmileage))
dummy3 <- dummy3[, -1]

carmileage_1 <-
  cbind(carmileage[, c(-2, -7:-10)], dummy1, dummy2, dummy3)

#splitting the data frame into train and test for model creation and evaluation
#set seed to generate same random numbers everytime
set.seed(100)
#generate indices based on sample
indices <- sample(1:nrow(carmileage_1), 0.7 * nrow(carmileage_1))
# Create train & test data sets
train <- carmileage_1[indices, ]
test <- carmileage_1[-indices, ]

###################### Checkpoint 3: Model Development #########################

model_1 <- lm(MPG ~ ., data = train)
summary(model_1)
# Observation:  The adjusted R2 value is 0.8554 which is 85% fit. It could be
# because of the more number of variables avaialble in the data set which has
# to be eliminated by identifying the significant variables

# Run stepwise selection on the model to exclude the insignificant variables
stepAIC(model_1, direction = "both")

# extract the call output of stepAIC and create a new model based out of that
model_2 <-
  lm(
    formula = MPG ~ Horsepower + Weight + Acceleration + Yr2003_2005 +
      Yr2006_2011 + Cylinders4 + Cylinders5 + Cylinders6 + Cylinders8 +
      Origin3 + car_brandbuick + car_brandcadillac + car_brandchrysler +
      car_branddatsun + car_brandfiat + car_brandmazda + car_brandoldsmobile +
      car_brandplymouth + car_brandpontiac + car_brandrenault +
      car_brandtriumph + car_brandvolkswagen,
    data = train
  )

summary(model_2)
# Observation: The adjusted r2 value of the model is 0.8606, which indicates
# all the insignifcant variables are removed and hence it doesnot impact the
# model output
# We still see so many insignificant variables, which can be eliminated
# by multicollinearity identification and removal

# Multicollinearity identification using VIF
# Given the industry standard as VIF<2, we will try to eliminate the insignficant
# variables till we have variables with VIF <2.

vif(model_2)
# Observation: We see cylinder4(33.225256), cylinder8(29.894429) & Cylinders6(23.701360) has the
# highest VIF value but since they are signifcant, we are moving to next highest VIF values
# i.e horsepower(12.472140) which is less significant when compared to other
# variables so creating a new model excluding horsepower

model_3 <-
 lm(
      formula = MPG ~ Weight + Acceleration + Yr2003_2005 +
        Yr2006_2011 + Cylinders4 + Cylinders5 + Cylinders6 + Cylinders8 +
        Origin3 + car_brandbuick + car_brandcadillac + car_brandchrysler +
        car_branddatsun + car_brandfiat + car_brandmazda + car_brandoldsmobile +
        car_brandplymouth + car_brandpontiac + car_brandrenault +
        car_brandtriumph + car_brandvolkswagen,
      data = train
    )

summary(model_3)
# Observation: The adjusted r2 value of the model is 0.8558 which is more or less
# same to the previous model. hence removing cylinder6 does not significantly impact
# the model

vif(model_3)
# Observation: We see cylinder4(33.219291) and cylinder8(29.597736) has the highest VIF value
# but since they are signifcant, the next highest vif cylinder6 is removed from the model

model_4 <-
  lm(
      formula = MPG ~ Weight + Acceleration + Yr2003_2005 +
        Yr2006_2011 + Cylinders4 + Cylinders5 + Cylinders8 +
        Origin3 + car_brandbuick + car_brandcadillac + car_brandchrysler +
        car_branddatsun + car_brandfiat + car_brandmazda + car_brandoldsmobile +
        car_brandplymouth + car_brandpontiac + car_brandrenault +
        car_brandtriumph + car_brandvolkswagen,
      data = train
    )
 
summary(model_4)
#Observation: The adjusted r2 value of the model is 0.8501 which is more or less
# same to the previous model. hence removing cylinder6 does not significantly impact
# the model

vif(model_4)
# cylinder8 has the highest vif value with less significance. so removing
# cylinder8

model_5 <-
  lm(
      formula = MPG ~ Weight + Acceleration + Yr2003_2005 +
        Yr2006_2011 + Cylinders4 + Cylinders5 +
        Origin3 + car_brandbuick + car_brandcadillac + car_brandchrysler +
        car_branddatsun + car_brandfiat + car_brandmazda + car_brandoldsmobile +
        car_brandplymouth + car_brandpontiac + car_brandrenault +
        car_brandtriumph + car_brandvolkswagen,
      data = train
    )
 
summary(model_5)
#R2 value= 0.8478. There is no significant reduction

vif(model_5)
# removing weight reduces the r2 value, hence removing the next highest vif value
# ie cylinder4

model_6 <-
  lm(
      formula = MPG ~ Weight + Acceleration + Yr2003_2005 +
        Yr2006_2011 + Cylinders5 +
        Origin3 + car_brandbuick + car_brandcadillac + car_brandchrysler +
        car_branddatsun + car_brandfiat + car_brandmazda + car_brandoldsmobile +
        car_brandplymouth + car_brandpontiac + car_brandrenault +
        car_brandtriumph + car_brandvolkswagen,
      data = train
    )
 
summary(model_6)
#R2 value= 0.8341. There is no significant reduction

vif(model_6)
# Observation: VIF values for all the attributes are less than 2 which solves
# the business objective. But still there are attributes with less significance and
# high p-value so we will remove them based on the p-value criteria

#P-value criteria
#car_brandmazda has high p-value comparitively. so removing from the model
model_7 <-
  lm(
      formula = MPG ~ Weight + Acceleration + Yr2003_2005 +
        Yr2006_2011 + Cylinders5 +
        Origin3 + car_brandbuick + car_brandcadillac + car_brandchrysler +
        car_branddatsun + car_brandfiat + car_brandoldsmobile +
        car_brandplymouth + car_brandpontiac + car_brandrenault +
        car_brandtriumph + car_brandvolkswagen,
      data = train
    )

summary(model_7)
#R2 value= 0.8346 There is no significant reduction

#Acceleration has high p-value comparitively. so removing from the model
model_8 <-
  lm(
      formula = MPG ~ Weight + Yr2003_2005 +
        Yr2006_2011 + Cylinders5 +
        Origin3 + car_brandbuick + car_brandcadillac + car_brandchrysler +
        car_branddatsun + car_brandfiat + car_brandoldsmobile +
        car_brandplymouth + car_brandpontiac + car_brandrenault +
        car_brandtriumph + car_brandvolkswagen,
      data = train
    )
summary(model_8)
#R2 value= 0.835. There is no significant reduction

#car_brandbuick has high p-value comparitively. so removing from the model
model_9 <-
  lm(
      formula = MPG ~ Weight + Yr2003_2005 +
        Yr2006_2011 + Cylinders5 +
        Origin3 + car_brandcadillac + car_brandchrysler +
        car_branddatsun + car_brandfiat + car_brandoldsmobile +
        car_brandplymouth + car_brandpontiac + car_brandrenault +
        car_brandtriumph + car_brandvolkswagen,
      data = train
    )
  
summary(model_9)
#R2 value= 0.8354. There is no significant reduction

#car_brandchrysler has high p-value comparitively. so removing from the model
model_10 <-
  lm(
      formula = MPG ~ Weight + Yr2003_2005 +
        Yr2006_2011 + Cylinders5 +
        Origin3 + car_brandcadillac +
        car_branddatsun + car_brandfiat + car_brandoldsmobile +
        car_brandplymouth + car_brandpontiac + car_brandrenault +
        car_brandtriumph + car_brandvolkswagen,
      data = train
    )
  
summary(model_10)
#R2 value= 0.8351. There is no significant reduction

#car_brandtriumph has high p-value comparitively. so removing from the model
model_11 <-
  lm(
      formula = MPG ~ Weight + Yr2003_2005 +
        Yr2006_2011 + Cylinders5 +
        Origin3 + car_brandcadillac +
        car_branddatsun + car_brandfiat + car_brandoldsmobile +
        car_brandplymouth + car_brandpontiac + car_brandrenault +
        car_brandvolkswagen,
      data = train
    )
  
summary(model_11)
#R2 value= 0.8345. There is no significant reduction

#car_branddatsun has high p-value comparitively. so removing from the model
model_12 <-
  lm(
      formula = MPG ~ Weight + Yr2003_2005 +
        Yr2006_2011 + Cylinders5 +
        Origin3 + car_brandcadillac +
        car_brandfiat + car_brandoldsmobile +
        car_brandplymouth + car_brandpontiac + car_brandrenault +
        car_brandvolkswagen,
      data = train
    )
  
summary(model_12)
#R2 value= 0.8332. There is no significant reduction

#car_brandplymouth has high p-value comparitively. so removing from the model
model_13 <-
  lm(
      formula = MPG ~ Weight + Yr2003_2005 +
        Yr2006_2011 + Cylinders5 +
        Origin3 + car_brandcadillac +
        car_brandfiat + car_brandoldsmobile +
        car_brandpontiac + car_brandrenault +
        car_brandvolkswagen,
      data = train
    )

summary(model_13)
#R2 value= 0.8318. There is no significant reduction

#car_brandcadillac has high p-value comparitively. so removing from the model
model_14 <-
  lm(
      formula = MPG ~ Weight + Yr2003_2005 +
        Yr2006_2011 + Cylinders5 +
        Origin3 + car_brandfiat + car_brandoldsmobile +
        car_brandpontiac + car_brandrenault +
        car_brandvolkswagen,
      data = train
    )
summary(model_14)
#R2 value= 0.8305. There is no significant reduction

#Cylinders5 has high p-value comparitively. so removing from the model
model_15 <-
   lm(
      formula = MPG ~ Weight + Yr2003_2005 +
        Yr2006_2011 + Origin3 + car_brandfiat + car_brandoldsmobile +
        car_brandpontiac + car_brandrenault +
        car_brandvolkswagen,
      data = train
    )

summary(model_15)
#R2 value= 0.8285. There is no significant reduction

#car_brandrenault has high p-value comparitively. so removing from the model
model_16 <-
  lm(
      formula = MPG ~ Weight + Yr2003_2005 +
        Yr2006_2011 + Origin3 + car_brandfiat + car_brandoldsmobile +
        car_brandpontiac + car_brandvolkswagen,
      data = train
    )

summary(model_16)
#R2 value= 0.8261. There is no significant reduction

#car_brandfiat has high p-value comparitively. so removing from the model
model_17 <-
  lm(
      formula = MPG ~ Weight + Yr2003_2005 +
        Yr2006_2011 + Origin3 + car_brandoldsmobile +
        car_brandpontiac + car_brandvolkswagen,
      data = train
    )

summary(model_17)
#R2 value= 0.8235. There is no significant reduction

#Origin3 has high p-value comparitively. so removing from the model
model_18 <-
  lm(
      formula = MPG ~ Weight + Yr2003_2005 +
        Yr2006_2011 + car_brandoldsmobile +
        car_brandpontiac + car_brandvolkswagen,
      data = train
    )

summary(model_18)
#R2 value= 0.821. There is no significant reduction

#car_brandpontiac has high p-value comparitively. so removing from the model
model_19 <-
  lm(
      formula = MPG ~ Weight + Yr2003_2005 +
        Yr2006_2011 + car_brandoldsmobile +
        car_brandvolkswagen,
      data = train
    )

summary(model_19)
#R2 value= 0.8186. There is no significant reduction

predict1 <- predict(object = model_19, newdata = test)
test$test_MPG <- predict1
cor(test$MPG, test$test_MPG) # 0.9060
cor(test$MPG, test$test_MPG) ^ 2 # 0.8208 (R square)

##############################Conclusion################################
#model_19 solves the below 3 objectives
# VIF < 2.
# The model should be highly predictive in nature 0.9060
# i.e it should show 80% (R squared) of accuracy R square=82% accuracy
