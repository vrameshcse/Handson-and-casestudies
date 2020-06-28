library(car)
library(caret)
library(MASS)
library(ROCR)
library(klaR)
library(ggplot2)
library(e1071)
library(Hmisc)
library(dummies)
library(caTools)
library(ggplot2)
library(GGally)

# Download the data set as german_credit
german_credit <-
  read.csv(
    "german.csv",
    stringsAsFactors = F,
    strip.white = T,
    na.strings = c("NA", " ", ""),
    blank.lines.skip = T
  )

# Data prpeapartion and feature transformation
str(german_credit)
View(german_credit)
sum(is.na(german_credit))
# observation: No na values
sum(duplicated(german_credit))
# observation: No duplicate observations

aggregate(Credit.amount ~ Duration.in.month + Default_status,
          german_credit,
          FUN = length)
# The number of defaults differ for each year and its more in the second year, so binning
# the variable to years will help for better prediction.

# Binning the duration in months to years and thus converting the numeric variable
# to categorical variable
german_credit$Duration.in.years <- factor(ifelse(
  german_credit$Duration.in.month < 13,
  "Oneyear",
  ifelse(
    german_credit$Duration.in.month > 12 &
      german_credit$Duration.in.month < 25,
    "Twoyear",
    ifelse(
      german_credit$Duration.in.month > 24 &
        german_credit$Duration.in.month < 37,
      "Threeyear",
      ifelse(
        german_credit$Duration.in.month > 36 &
          german_credit$Duration.in.month < 49,
        "Fouryear",
        ifelse(
          german_credit$Duration.in.month > 48 &
            german_credit$Duration.in.month < 61,
          "Fiveyear",
          "Sixyear"
        )
      )
    )
  )
))

aggregate(Credit.amount ~ Age.in.Years + Default_status,
          german_credit,
          FUN = length)
# There are more defaults in junior and mid range ages and less in aged people,
# so binning the age into 3 groups will improve the model predictability

# Binning the age group into 3 bins of <25, 26 to 50 and above 50
# converting the numerical variable into categorical variable
german_credit$Age.group <-
  ifelse(
    german_credit$Age.in.Years < 26,
    "Age <=25",
    ifelse(german_credit$Age.in.Years < 51,
           "Age >25 & <=50",
           "Age >50")
  )


# create a seperate data frame with all the categorical variables of german_credit
german_credit_factors <- german_credit[,-c(2, 5, 13, 21)]
# lapply to convert all the categorical variables to factors data type
german_credit_factors <-
  as.data.frame(lapply(german_credit_factors, factor))
summary(german_credit_factors) # summary to check the factor variables
# using dummy.data.frame from dummies package, do feature transformation
# as the logistic regression model only accepts numerical variables
# all the factor variables are converted to numeric variables
german_credit_dummy <-
  dummy.data.frame(data = german_credit_factors)
# Merge the dummy variables and the other variables to german_credit_1
german_credit_1 <-
  cbind(german_credit[, c(21, 5)], german_credit_dummy)
str(german_credit_1) # view the structure


# Exploratory Data Analysis

#univariate analysis
# summary of response and other variables
summary(german_credit)
summary(as.factor(german_credit$Default_status))#check the distribution of bad and good class
# 0   1
# 700 300

# check the histogram of all the numeric variables
ggplot(german_credit, aes(x = german_credit$Duration.in.month)) + geom_histogram()
# data is skewed and not continous
ggplot(german_credit, aes(x = german_credit$Credit.amount)) + geom_histogram()
# data is skewed
ggplot(german_credit, aes(x = german_credit$Age.in.Years)) + geom_histogram()
# data is skewed

boxplot(german_credit$Duration.in.month) # There are outliers at the upper tail
quantile(german_credit$Duration.in.month, seq(0, 1, 0.01)) # outliers above 98%

boxplot(german_credit$Age.in.Years) # There are outliers at the upper tail
quantile(german_credit$Age.in.Years, seq(0, 1, 0.01)) # outliers above 98%

quantile(german_credit_1$Credit.amount, seq(0, 1, 0.01))# There are outliers at the upper tail
boxplot(german_credit_1$Credit.amount)# outliers above 93%

uppertail_IQR <-
  boxplot.stats(german_credit_1$Credit.amount)$stats[[5]]
sum(boxplot.stats(german_credit$Credit.amount)$out > uppertail_IQR)
german_credit_1$Credit.amount[german_credit_1$Credit.amount > uppertail_IQR] <-
  uppertail_IQR

#Multivariate analysis
plot1 <- ggplot(german_credit,
                aes(
                  x = Credit.amount,
                  y = Age.in.Years,
                  col = factor(Default_status)
                )) + geom_point(alpha = 0.5)
plot1 + facet_wrap(~ Purpose)
# shows that the the most of the loans are for:
# new car
# furniture/equipment
# radio/television
plot1 + facet_wrap(~ Credit.history)
# Plot shows that most of the loans are given for
# A32 : existing credits paid back duly till now
# A34 : critical account/

plot1 + facet_wrap(~ Personal.status.and.sex)
# It reveal that single males tend to borrow more loans,
# junior ones get the higher loan amount
# corresponds to a bad rating.

plot1 + facet_wrap(~ Duration.in.years)
# Plot shows that one year and two year has borrowed more loans and there are more
# defaults in 2nd and 3rd year
plot1 + facet_wrap(~ Status.of.existing.checking.account)
# Plot shows that the ratio of defaults are more for
# A13 :      ... >= 200 DM /salary assignments for at least 1 year
# A14 : no checking account
plot1 + facet_wrap(~ Job_status)
# more loans were given to
# A173 : skilled employee / official

plot2 <- ggplot(german_credit,
                aes(
                  x = Credit.amount,
                  y = Duration.in.month,
                  col = factor(Default_status)
                )) + geom_point(alpha = 0.5)
plot2 + facet_wrap(~ Age.group)
# Plot shows less credit amount and less duration has more defaults
plot2 + facet_wrap(~ Status.of.existing.checking.account)
# Plot shows there are more defaults for
# A14 : no checking account

ggpairs(
  data = german_credit,
  columns = c("Credit.amount",
              "Age.in.Years",
              "Duration.in.month")
)
# positive correlation is high between duration in month and credit amount

# split the data set into train and test with 70:30 ratio of default status
set.seed(100)
split_indices <-
  sample.split(german_credit_1$Default_status, SplitRatio = 0.70)
summary(split_indices)
# Mode   FALSE    TRUE    NA's 
# logical     300     700       0 
train = german_credit_1[split_indices, ]
test = german_credit_1[!split_indices, ]

# Initial Model with all variables
model_1 <-
  glm(Default_status ~ ., family = binomial, data = train)
summary(model_1)
# Observation:
# Null deviance: 855.21  on 699  degrees of freedom
# Residual deviance: 616.61  on 640  degrees of freedom
# AIC: 736.61
# The insignificant variables are to be removed using stepwise selection.

# Stepwise selection, with direction =both
stepAIC(model_1, direction = "both")

model_2 <-
  glm(
    formula = Default_status ~ Status.of.existing.checking.accountA11 +
      Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 +
      Credit.historyA30 + Credit.historyA31 + Credit.historyA32 +
      Credit.historyA33 + PurposeA40 + PurposeA46 + Savings.account.bondsA61 +
      Present.employment.since.A74 + Installment.rate.in.percentage.of.disposable.income1 +
      Installment.rate.in.percentage.of.disposable.income2 + Personal.status.and.sexA93 +
      Other.debtors...guarantorsA101 + Other.debtors...guarantorsA102 +
      Present.residence.since2 + PropertyA121 + Other.installment.plansA141 +
      foreign.workerA201 + Duration.in.yearsFouryear + Duration.in.yearsOneyear +
      `Age.groupAge <=25`,
    family = binomial,
    data = train
  )
summary(model_2)
# Observation:
# Null deviance: 855.21  on 699  degrees of freedom
# Residual deviance: 635.71  on 676  degrees of freedom
# AIC: 683.71
# The AIC has reduced a lot making this a good model than the prior one

# Remove multicollinearity through VIF check (VIF threshold 3)
vif(model_2)
# All the variables has VIF less than 3 so validating the p-value to remove
# insignificant values

# foreign.workerA201 variable has higher p-value and hence forming model_3
# after removing it
model_3 <-
  glm(
    formula = Default_status ~ Status.of.existing.checking.accountA11 +
      Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 +
      Credit.historyA30 + Credit.historyA31 + Credit.historyA32 +
      Credit.historyA33 + PurposeA40 + PurposeA46 + Savings.account.bondsA61 +
      Present.employment.since.A74 + Installment.rate.in.percentage.of.disposable.income1 +
      Installment.rate.in.percentage.of.disposable.income2 + Personal.status.and.sexA93 +
      Other.debtors...guarantorsA101 + Other.debtors...guarantorsA102 +
      Present.residence.since2 + PropertyA121 + Other.installment.plansA141 +
      Duration.in.yearsFouryear + Duration.in.yearsOneyear +
      `Age.groupAge <=25`,
    family = binomial,
    data = train
  )
summary(model_3)
# Observation:
# Null deviance: 855.21  on 699  degrees of freedom
# Residual deviance: 638.13  on 677  degrees of freedom
# AIC: 684.13
# The variables with higher p-value has to be removed to increase the accuracy &
# sensitivity

# PropertyA121 variable has higher p-value and hence forming model_4
# after removing it
model_4 <-
  glm(
    formula = Default_status ~ Status.of.existing.checking.accountA11 +
      Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 +
      Credit.historyA30 + Credit.historyA31 + Credit.historyA32 +
      Credit.historyA33 + PurposeA40 + PurposeA46 + Savings.account.bondsA61 +
      Present.employment.since.A74 + Installment.rate.in.percentage.of.disposable.income1 +
      Installment.rate.in.percentage.of.disposable.income2 + Personal.status.and.sexA93 +
      Other.debtors...guarantorsA101 + Other.debtors...guarantorsA102 +
      Present.residence.since2 + Other.installment.plansA141 +
      Duration.in.yearsFouryear + Duration.in.yearsOneyear +
      `Age.groupAge <=25`,
    family = binomial,
    data = train
  )
summary(model_4)
# Null deviance: 855.21  on 699  degrees of freedom
# Residual deviance: 640.62  on 678  degrees of freedom
# AIC: 684.62
# The variables with higher p-value has to be removed to increase the accuracy &
# sensitivity

# Personal.status.and.sexA93 variable has higher p-value and hence forming model_5
# after removing it
model_5 <-
  glm(
    formula = Default_status ~ Status.of.existing.checking.accountA11 +
      Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 +
      Credit.historyA30 + Credit.historyA31 + Credit.historyA32 +
      Credit.historyA33 + PurposeA40 + PurposeA46 + Savings.account.bondsA61 +
      Present.employment.since.A74 + Installment.rate.in.percentage.of.disposable.income1 +
      Installment.rate.in.percentage.of.disposable.income2 +
      Other.debtors...guarantorsA101 + Other.debtors...guarantorsA102 +
      Present.residence.since2 + Other.installment.plansA141 +
      Duration.in.yearsFouryear + Duration.in.yearsOneyear +
      `Age.groupAge <=25`,
    family = binomial,
    data = train
  )
summary(model_5)
# Null deviance: 855.21  on 699  degrees of freedom
# Residual deviance: 643.51  on 679  degrees of freedom
# AIC: 685.51
# The variables with higher p-value has to be removed to increase the accuracy &
# sensitivity

# Credit.historyA33 variable has higher p-value and hence forming model_6
# after removing it
model_6 <-
  glm(
    formula = Default_status ~ Status.of.existing.checking.accountA11 +
      Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 +
      Credit.historyA30 + Credit.historyA31 + Credit.historyA32 +
      PurposeA40 + PurposeA46 + Savings.account.bondsA61 +
      Present.employment.since.A74 + Installment.rate.in.percentage.of.disposable.income1 +
      Installment.rate.in.percentage.of.disposable.income2 +
      Other.debtors...guarantorsA101 + Other.debtors...guarantorsA102 +
      Present.residence.since2 + Other.installment.plansA141 +
      Duration.in.yearsFouryear + Duration.in.yearsOneyear +
      `Age.groupAge <=25`,
    family = binomial,
    data = train
  )
summary(model_6)
# Null deviance: 855.21  on 699  degrees of freedom
# Residual deviance: 646.11  on 680  degrees of freedom
# AIC: 686.11
# The variables with higher p-value has to be removed to increase the accuracy &
# sensitivity

#Installment.rate.in.percentage.of.disposable.income1variable has higher p-value
# and hence forming model_7 after removing it
model_7 <-
  glm(
    formula = Default_status ~ Status.of.existing.checking.accountA11 +
      Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 +
      Credit.historyA30 + Credit.historyA31 + Credit.historyA32 +
      PurposeA40 + PurposeA46 + Savings.account.bondsA61 +
      Present.employment.since.A74 +
      Installment.rate.in.percentage.of.disposable.income2 +
      Other.debtors...guarantorsA101 + Other.debtors...guarantorsA102 +
      Present.residence.since2 + Other.installment.plansA141 +
      Duration.in.yearsFouryear + Duration.in.yearsOneyear +
      `Age.groupAge <=25`,
    family = binomial,
    data = train
  )
summary(model_7)
# Null deviance: 855.21  on 699  degrees of freedom
# Residual deviance: 649.08  on 681  degrees of freedom
# AIC: 687.08
# The variables with higher p-value has to be removed to increase the accuracy &
# sensitivity

#Status.of.existing.checking.accountA13 variable has higher p-value
# and hence forming model_7 after removing it
model_8 <-
  glm(
    formula = Default_status ~ Status.of.existing.checking.accountA11 +
      Status.of.existing.checking.accountA12 +
      Credit.historyA30 + Credit.historyA31 + Credit.historyA32 +
      PurposeA40 + PurposeA46 + Savings.account.bondsA61 +
      Present.employment.since.A74 +
      Installment.rate.in.percentage.of.disposable.income2 +
      Other.debtors...guarantorsA101 + Other.debtors...guarantorsA102 +
      Present.residence.since2 + Other.installment.plansA141 +
      Duration.in.yearsFouryear + Duration.in.yearsOneyear +
      `Age.groupAge <=25`,
    family = binomial,
    data = train
  )
summary(model_8)
# Null deviance: 855.21  on 699  degrees of freedom
# Residual deviance: 651.99  on 682  degrees of freedom
# AIC: 687.99
# The variables with higher p-value has to be removed to increase the accuracy &
# sensitivity

#Other.installment.plansA141 variable has higher p-value
# and hence forming model_7 after removing it
model_9 <-
  glm(
    formula = Default_status ~ Status.of.existing.checking.accountA11 +
      Status.of.existing.checking.accountA12 +
      Credit.historyA30 + Credit.historyA31 + Credit.historyA32 +
      PurposeA40 + PurposeA46 + Savings.account.bondsA61 +
      Present.employment.since.A74 +
      Installment.rate.in.percentage.of.disposable.income2 +
      Other.debtors...guarantorsA101 + Other.debtors...guarantorsA102 +
      Present.residence.since2 +
      Duration.in.yearsFouryear + Duration.in.yearsOneyear +
      `Age.groupAge <=25`,
    family = binomial,
    data = train
  )
summary(model_9)
model_final <- model_9
# Observation:
# Null deviance= 855.21  on 699  degrees of freedom
# Residual deviance= 655.17  on 683  degrees of freedom
# AIC= 689.17
# Most of the variables are marked as significant and further removing the variables by
# highest p-value is eventually decreasing the accuracy and the sensitivity.
# As the ojective is to predict the potential bad customers, a model with
# high sensitivity is the ideal choice.

# Predict the probabilities of the train and test data set with the model
prediction_train <- predict(model_final, type = "response")
prediction_test <-
  predict(model_final, newdata = test, type = "response")

# create a prediction object for train and test data set
pred_object_train <-
  prediction(prediction_train, train$Default_status)
pred_object_test <- prediction(prediction_test, test$Default_status)

# create a performance object with true postive rate and false positive rate measures
performance_measure_train <-
  performance(pred_object_train, measure = "tpr", x.measure = "fpr")
performance_measure_test <-
  performance(pred_object_test, measure = "tpr", x.measure = "fpr")

# create a performace object with area of curve measure for train data set
auc_train <- performance(pred_object_train, measure = "auc")
# store the auc value in auc_train
auc_train <- auc_train@y.values[[1]]
auc_train
# Observation: AUC = 0.818309

# create a performace object with area of curve measure for test data set
auc_test <- performance(pred_object_test, measure = "auc")
# store the auc value in auc_test
auc_test <- auc_test@y.values[[1]]
auc_test
# Observation: auc = 0.7557937. There is a small variance in the test auc score
# becuase of the unseen data points. But 0.75 score is a good score for the model

# c-statistic and KS -statistic
# store the c-statistic of train and test data set in c_train and c_test variables
c_train <- rcorr.cens(prediction_train, train$Default_status)
c_train
# Observation: C Index= 8.183090e-01 for train data
c_test <- rcorr.cens(prediction_test, test$Default_status)
c_test
# Observation: C Index= 7.557937e-01 for test data which is almost near
# to the train data c-statistic

# find the ks-statistic of train data set and store in ks_table_train
ks_table_train <-
  attr(performance_measure_train, "y.values")[[1]] - (attr(performance_measure_train, "x.values")[[1]])
# find the max value of the ks-statistic
ks_train = max(ks_table_train)
ks_train
# Observation: max value= 0.5244898
which(ks_table_train == ks_train)
# The index of the max ks-statistic is 200
# The KS-statistic decile of train data set is 200/700 which is 3rd decile

# find the ks-statistic of test data set and store in ks_table_test
ks_table_test <-
  attr(performance_measure_test, "y.values")[[1]] - (attr(performance_measure_test, "x.values")[[1]])
# find the max value of the ks-statistic
ks_test = max(ks_table_test)
ks_test
# Observation: max value= 0.4571429
which(ks_table_test == ks_test)
# The KS-statistic decile of test data set is 121/300 which is 4th decile

# Selecting threshold value
# Plot the ROC curve to determince the threshold value
# The model objective is to efficient predict the default customers (default_status=1)
# to avoid funding loans to these customers to avoid loan defaults
# so we have to choose a threshold which has maximum tpr and less fpr to identify
# potential bad customers who would default.
# So 0.2 will be the optimal value to have good sensitivity model.
plot(
  performance_measure_train,
  colorize = TRUE,
  text.adj = c(-0.2, 1.7),
  lab = c(10, 10, 10)
)
plot(
  performance_measure_test,
  colorize = TRUE,
  text.adj = c(-0.2, 1.7),
  lab = c(10, 10, 10)
)

# derive confusion matrix with threshold=0.2
confusionMatrix(as.numeric(prediction_train > 0.2),
                train$Default_status,
                positive = "1")
# Observation:
# Accuracy = 0.6629
# Sensitivity = 0.8571
# Specificity = 0.5796


# derive confusion matrix with threshold=0.2
confusionMatrix(as.numeric(prediction_test > 0.2),
                test$Default_status,
                positive = "1")
# Observation:
# Accuracy = 0.68
# Sensitivity = 0.8333
# Specificity = 0.6143

#conclusion: As bank objective is to identify potential bad customers who would default
# a model with threshold=0.2 and a good sensitivity (tpr) is a ideal choice


#----------------Alternate model after------------------------------------------#

#below is the model after removing all the insignificant variables.
model_alternate <-
  glm(
    formula = Default_status ~ Status.of.existing.checking.accountA11 +
      Status.of.existing.checking.accountA12 +
      Credit.historyA30 + Credit.historyA31 +
      Duration.in.yearsOneyear,
    family = binomial,
    data = train
  )
summary(model_alternate)
# Null deviance: 855.21  on 699  degrees of freedom
# Residual deviance: 713.33  on 694  degrees of freedom
# AIC: 725.33
# Accuracy : 0.6067
# Sensitivity : 0.7556
# Specificity : 0.5429
# we could observe that there is a signifcant increase in the AIC and also the accuracy
# sensitivity have gone down. As the objective is to find the potential bad customers
# a model with high sensitivity is a ideal choice and thus few insignifant variables are
# not removed from the model to have high accuracy of identifying bad customers

factor(as.numeric(factor(train$Churn)))
