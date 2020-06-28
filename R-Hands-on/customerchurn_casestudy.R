
# Install and Load the required packages####
library(MASS)
library(car)
library(ggplot2)
library(stringr)
library(moments)
library(GGally)
library(lubridate)
library(Hmisc)
library(ROCR)
library(caret)
library(e1071)
library(dummies)
library(class)
library(caTools)
library(GGally)

# Load the given files####
# Load the churn_data in to churn variable
churn<-read.csv("churn_data.csv",sep = ",",stringsAsFactors = TRUE)
# Load the customer_data in to customer variable
customer<-read.csv("customer_data.csv",sep = ",",stringsAsFactors = TRUE)
# Load the internet_data in to internet varible
internet<-read.csv("internet_data.csv",sep = ",",stringsAsFactors = TRUE)

# Collate the 3 files in a single file####
# Left merge the customer data in to churn data
customer<-merge(x=customer,y=internet,by="customerID",all.x=TRUE)
# Left merge the internet data in to churn data
churn<-merge(x=customer,y=churn,by="customerID",all.x=TRUE)
# Observation: The final collated data contains 7043 observations with 21 variables
write.table(churn,file ="churn_coll.csv", row.names = FALSE,sep = ",")
# Understand the structure of the collated file
str(churn)
summary(churn)

# Data transformation
aggregate(customerID ~ tenure + Churn,
          churn,
          FUN = length)
# We could see there are more number of customers with less tenure and there are more
# customers who churn belong to tenure of 1&2 so binning the customers into 3 profiles
# namely lowprofile, mediumprofile and highprofile customers will help to improve the
# model accuracy

churn$customerprofile <- factor(
  ifelse(
    churn$tenure < 25,
    "Lowprofile",
    ifelse(churn$tenure < 49,
           "Mediumprofile",
           "Highprofile")
  )
)
# check the distribution of churn customers accross different bins
aggregate(customerID ~ customerprofile + Churn,
          churn,
          FUN = length)

summary(churn)

# Make bar charts to find interesting relationships between variables.
plot(Churn ~., data = churn, type = "c")

#Observations:
# customer who are single tend to churn more than who has partners
# Customer with no dependents tend to churn more
# customer with fiber-optic tend to churn more than other internet services
# customer with no online security , no online back-up, no device protection, no tech support
# tend to churn more
# customer with month-to-month contract tend to churn more

plot = ggplot(churn, aes(x=MonthlyCharges,y=tenure,col=factor(Churn))) + geom_point(alpha=0.5)
print(plot)
# we could observe customers with tenure less than 40 months have more churn rate
plot + facet_wrap(~ gender)
plot + facet_wrap(~ MultipleLines)
plot + facet_wrap(~ Contract)
plot1 = ggplot(churn, aes(x=TotalCharges,y=tenure,col=factor(Churn))) + geom_point(alpha=0.5)

ggpairs(
  data = churn,
  columns = c("tenure",
              "MonthlyCharges",
              "TotalCharges",
              "Churn"))
#Observation: total charges and tenure has high positive correlation of 0.82

ggpairs(
  data = churn,
  columns = c("customerprofile",
              "MonthlyCharges",
              "TotalCharges",
              "Churn"))


ggplot(churn,aes(x=churn$tenure,fill= churn$Churn,prob=TRUE))+
  geom_bar(position=position_dodge())+
  facet_grid(PaymentMethod~InternetService)
#Shows Customer with Fiber Optic, Electronic Check with tenure <=20 turned to more churn 

ggplot(churn,aes(x=churn$tenure,fill= churn$Churn,prob=TRUE))+
  geom_bar(position=position_dodge())+
  facet_grid(PaymentMethod~Contract)
# Customer with Month on MOnth COntract, opted for Electronic Check with tenure <15 turned to more churn

ggplot(churn,aes(x=churn$InternetService,fill= churn$Churn,prob=TRUE))+
  geom_bar(position=position_dodge())+
  facet_grid(Contract~PaymentMethod)
# Customer with Electronic Check, Fiber Optic and on month- month contract turned to be more churned

# Make Box plots for numeric variables to look for outliers. 
boxplot(churn$tenure,boxmex=0.2)
boxplot(churn$MonthlyCharges,boxmex=0.2)
boxplot(churn$TotalCharges,boxmex=0.2)
# The plot shows that there are no outliers and hence no outlier treatment is required

# Bring the variables in the correct format
churn$SeniorCitizen<-as.factor(churn$SeniorCitizen)
summary(churn)
#Observation: All the variables are in the correct format

# Impute the missing values, and perform the outlier treatment (if required).
sum(is.na(churn)) # Observation: TotalCharges variable contains 11 NAs
tot_su<-median(churn$TotalCharges[!is.na(churn$TotalCharges)])# Median of non NA data
churn$TotalCharges[is.na(churn$TotalCharges)]<-tot_su #Imputing the NAs with the median as it's distribution is skewed
sum(!complete.cases(churn)) # Observation: There are no missing values in the data
# Since there are no outliers in the data outlier treatment is not required

churn_backup <- churn
# Removing the first column containing customer IDs
churn <- churn[, -1]
# Store the Churn status in a new variable churn_status
churn_status<-churn$Churn
# Remove the churn status column from the data before creating dummies
data<-churn[,-(ncol(churn)-1)]


# Bring the data in the correct format to implement predictive models
churn1<-dummy.data.frame(data, omit.constants=TRUE, dummy.classes = getOption("dummy.classes"),drop=TRUE)
# remove the additional dummy variables including tenure variable
churn1<-churn1[,-c(2,4,6,8,11,14,17,20,23,26,29,31,33,35,38,40,44,49)]
churn1$Churn<-churn_status # 7043x32 dimension
churn1_backup <- churn1
churn_master <- churn1

# Splitting into training and testing by keeping the same proportion of Churn status in original dataset
set.seed(100)
s <- sample.split(Y = churn_master$Churn,SplitRatio = 0.7)

# training data contains 70% of the data
train=churn_master[s,]

#testing data contains 30% of the data
test=churn_master[!s,]

# K-NN Model####

## ---------------- K-NN modelling---------------------------------------------------------
# Since our objective is to identify customers who may churn (positive case), our objective is to develop a model which- 
# will have a high Sensitivity (true positive rate) so that we can identify maximum no of customers who may churn

# Scaling the numeric variables
churn1$MonthlyCharges<-scale(churn1$MonthlyCharges)

churn1$TotalCharges<-scale(churn1$TotalCharges)

# Splitting into training and testing by keeping the same proportion of Churn status in original dataset
set.seed(100)
s <- sample.split(Y = churn1$Churn,SplitRatio = 0.7)

# training data contains 70% of the data
data_train=churn1[s,]

#testing data contains 30% of the data
data_test=churn1[!s,]

# True class labels of training data
data_train$Churn<-ifelse(data_train$Churn=="Yes",0,1) #Converting the true class labels in to numerics
data_test$Churn<-ifelse(data_test$Churn=="Yes",0,1) #Converting the true class labels in to numerics
cl <- data_train$Churn

#Training and testing data without the true labels
data_train <- data_train[,-ncol(data_train)]
data_test1 <- data_test[, -ncol(data_test)]


# Implement the K-NN model for optimal K. Usually ideal k will be closer to square root of no of variables (30 in this case).

# Create a K-NN model & try different values of k to find the ideal model in terms of confusion matrix
for (i in 1:20){
        if (i %% 2 ==1){
  model.knn = knn(data_train,data_test1,cl,k=3)
  knn_confusion_matrix <- confusionMatrix(model.knn, data_test$Churn, positive = "Yes")
  Accuracy <-round(knn_confusion_matrix$overall[1],2)
  Sensitivity <- round(knn_confusion_matrix$byClass[1],2)
  Specificity <- round(knn_confusion_matrix$byClass[2],2)
  print(paste("for k=",i,"Accuracy is",Accuracy,"Sensitivity is",Sensitivity,"Specificity is",Specificity,sep = " "))
}}

# k= 13 shows better perfomance in terms of confusion matrix (especially sensitivity)
knn_best_model <- knn(data_train,data_test1,cl,k=13, prob = TRUE) # k is finalized to be 13 since it gives an optimal accuracy, sensitivity and specificity
knn_best_model1<-knn(data_train,data_test1,cl,k=13)
#calculating the values for ROC curve
pred <- prediction(attr(knn_best_model,"prob"), data_test[,"Churn"])
perf <- performance(pred,"tpr","fpr")
table(knn_best_model1,data_test[,"Churn"])
confusionMatrix(knn_best_model1, data_test[,"Churn"],positive = "1")
# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.1,cex.lab=1.1)

# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)

# calculating AUC
auc <- performance(pred,"auc")

#-------------------------------------------------------------------------------------------------


# Naive Bayes Model:
# _________________

# Bring the data in the correct format to implement Naive Bayes algorithm.
train_features <- train[,-32]
test_features <- test[,-32]
str(train_featires)
str(test_features)
# Implement the Naive Bayes algorithm.
# Churn is the target variable on the training data set
naive_model <- naiveBayes(Churn~.,train)
#with the above model, we will predict the target variable on the features test data set
#As an output the predicted values are assigned to Class variable of predict_model
predict_model_train1 <- predict(naive_model, train_features)# for class prediction
predict_model_train2 <- predict(naive_model, train_features, type = "raw")# for probability prediction

predict_model_test1 <- predict(naive_model, test_features)# for class prediction
predict_model_test2 <- predict(naive_model, test_features, type = "raw")# for probability prediction

#Extract the probability 
model_prob <- predict_model_train2[,1]
model_prob1 <- predict_model_test2[,1]
#store the class variable as numeric
predict_class1 <- as.numeric(train$Churn=="No")
predict_class2 <- as.numeric(test$Churn=="No")

# create prediction object
naive_pred_object1 <- prediction(model_prob,predict_class1)
naive_pred_object2 <- prediction(model_prob1,predict_class2)

# create performance object to plot roc curve
naive_performance1 <- performance(naive_pred_object1, measure = "tpr", x.measure = "fpr")
naive_performance2 <- performance(naive_pred_object2, measure = "tpr", x.measure = "fpr")

#Calculate AUC for train and test

auc_naive1 <- performance(naive_pred_object1, measure = "auc")
auc_naive1 <- auc_naive1@y.values[[1]]
auc_naive1
# AUC=0.8122899 which indicates a good model

auc_naive2 <- performance(naive_pred_object2, measure = "auc")
auc_naive2 <- auc_naive2@y.values[[1]]
auc_naive2
# AUC=0.8210296 which indicates a good model and almost same as train auc

# plot roc curve
plot(
  naive_performance1,
  colorize = TRUE,
  text.adj = c(-0.2, 1.7),
  lab = c(10, 10, 10)
)

plot(
  naive_performance2,
  colorize = TRUE,
  text.adj = c(-0.2, 1.7),
  lab = c(10, 10, 10)
)

# Evaluating Model by comparing the values predicted by Model with the actual values present in naive_test_data.
# Table can be used to summarise the predicted & Actual values from naive test data
table(predict_model_train1, train$Churn)
confusionMatrix(predict_model_train1, train$Churn,positive = "Yes")
# Accuracy : 0.7132 
# Sensitivity : 0.8173          
# Specificity : 0.6756 

table(predict_model_test1, test$Churn)
confusionMatrix(predict_model_test1, test$Churn,positive = "Yes")
# Accuracy : 0.7198 which is between 64-78% so model is good.
# Positive Class is Yes means TRUE Positive gives churned customer
# As we are trying to predict the churn customer, Sensitivity, TRUE Positive Rate helps us
# Above Model's Sensitivity is 81% means the model can predict 81% of the Positive var accurately.


#----------------------------------------------------------------------------------------

# Logistic Regression:

# Bring the data in the correct format to implement Logistic regression model.

log_model <- glm(Churn ~ ., data = train, family = binomial)
summary(log_model)
# Null deviance: 5704.4  on 4929  degrees of freedom
# Residual deviance: 4138.7  on 4905  degrees of freedom
# AIC: 4188.7

# Implement the Logistic regression algorithm and use stepwise selection to select final variables
stepAIC(log_model,direction = "both")
log_model1 <-  
  glm(formula = Churn ~ SeniorCitizen0 + MultipleLinesNo + `MultipleLinesNo phone service` + 
        InternetServiceDSL + `InternetServiceFiber optic` + OnlineSecurityNo + 
        DeviceProtectionNo + StreamingTVNo + StreamingMoviesNo + 
        `ContractMonth-to-month` + `ContractOne year` + PaperlessBillingNo + 
        `PaymentMethodCredit card (automatic)` + `PaymentMethodElectronic check` + 
        MonthlyCharges + TotalCharges + customerprofileLowprofile, 
      family = binomial, data = train)
summary(log_model1)
# Null deviance: 5704.4  on 4929  degrees of freedom
# Residual deviance: 4143.3  on 4912  degrees of freedom
# AIC: 4179.3

# Select the variables using VIF criterion. 
vif(log_model1)
# MonthlyCharges has high VIF

log_model2 <-  
  glm(formula = Churn ~ SeniorCitizen0 + MultipleLinesNo + `MultipleLinesNo phone service` + 
        InternetServiceDSL + `InternetServiceFiber optic` + OnlineSecurityNo + 
        DeviceProtectionNo + StreamingTVNo + StreamingMoviesNo + 
        `ContractMonth-to-month` + `ContractOne year` + PaperlessBillingNo + 
        `PaymentMethodCredit card (automatic)` + `PaymentMethodElectronic check` + 
        TotalCharges + customerprofileLowprofile, 
      family = binomial, data = train)
summary(log_model2)
# Null deviance: 5704.4  on 4929  degrees of freedom
# Residual deviance: 4152.9  on 4913  degrees of freedom
# AIC: 4186.9

vif(log_model2) # customerprofileLowprofile has high VIF

log_model3 <-  
  glm(formula = Churn ~ SeniorCitizen0 + MultipleLinesNo + `MultipleLinesNo phone service` + 
        InternetServiceDSL + `InternetServiceFiber optic` + OnlineSecurityNo + 
        DeviceProtectionNo + StreamingTVNo + StreamingMoviesNo + 
        `ContractMonth-to-month` + `ContractOne year` + PaperlessBillingNo + 
        `PaymentMethodCredit card (automatic)` + `PaymentMethodElectronic check` + 
        TotalCharges, 
      family = binomial, data = train)
summary(log_model3)
# Null deviance: 5704.4  on 4929  degrees of freedom
# Residual deviance: 4162.3  on 4914  degrees of freedom
# AIC: 4194.3

vif(log_model3)
# InternetServiceDSL,`InternetServiceFiber optic`, `ContractMonth-to-month`,
# `ContractOne year`,TotalCharges seem to occur again with high VIF, check the
# correlation to decide which variable to eliminate

temp <- subset(train, 
               select=c(InternetServiceDSL,`InternetServiceFiber optic`,
                        `ContractMonth-to-month`,`ContractOne year`,TotalCharges))
cor(temp) # generate correlation matrix
# InternetServiceDSL,`InternetServiceFiber optic` are negatively correlated and next to 
# that `ContractMonth-to-month`, `ContractOne year` are correlated.

# Generating a model after eliminating InternetServiceDSL instead of `InternetServiceFiber optic` 
# though `InternetServiceFiber optic` has high VIF as removing the latter variable significantly
# increases the AIC value
log_model4 <-  
  glm(formula = Churn ~ SeniorCitizen0 + MultipleLinesNo + `MultipleLinesNo phone service` + 
        `InternetServiceFiber optic` + OnlineSecurityNo + 
        DeviceProtectionNo + StreamingTVNo + StreamingMoviesNo + 
        `ContractMonth-to-month` + `ContractOne year` + PaperlessBillingNo + 
        `PaymentMethodCredit card (automatic)` + `PaymentMethodElectronic check` + 
        TotalCharges, 
      family = binomial, data = train)
summary(log_model4)
# Null deviance: 5704.4  on 4929  degrees of freedom
# Residual deviance: 4186.8  on 4915  degrees of freedom
# AIC: 4216.8

vif(log_model4)

# Generating a model after eliminating `ContractOne year` instead of `ContractMonth-to-month` 
# though `ContractMonth-to-month`` has high VIF as removing the latter variable significantly
# increases the AIC value
log_model5 <-  
  glm(formula = Churn ~ SeniorCitizen0 + MultipleLinesNo + `MultipleLinesNo phone service` + 
        `InternetServiceFiber optic` + OnlineSecurityNo + 
        DeviceProtectionNo + StreamingTVNo + StreamingMoviesNo + 
        `ContractMonth-to-month` + PaperlessBillingNo + 
        `PaymentMethodCredit card (automatic)` + `PaymentMethodElectronic check` + 
        TotalCharges, 
      family = binomial, data = train)
summary(log_model5)
# Null deviance: 5704.4  on 4929  degrees of freedom
# Residual deviance: 4215.2  on 4916  degrees of freedom
# AIC: 4243.2

vif(log_model5) # all the variables have vif less than 2

# so removing the insignifiant variables which has high p-value
# `MultipleLinesNo phone service` variable has higher p-value
log_model6 <-  
  glm(formula = Churn ~ SeniorCitizen0 + MultipleLinesNo + 
        `InternetServiceFiber optic` + OnlineSecurityNo + 
        DeviceProtectionNo + StreamingTVNo + StreamingMoviesNo + 
        `ContractMonth-to-month` + PaperlessBillingNo + 
        `PaymentMethodCredit card (automatic)` + `PaymentMethodElectronic check` + 
        TotalCharges, 
      family = binomial, data = train)
summary(log_model6)
# Null deviance: 5704.4  on 4929  degrees of freedom
# Residual deviance: 4215.5  on 4917  degrees of freedom
# AIC: 4241.5

# StreamingTVNo variable has higher p-value
log_model7 <-  
  glm(formula = Churn ~ SeniorCitizen0 + MultipleLinesNo + 
        `InternetServiceFiber optic` + OnlineSecurityNo + 
        DeviceProtectionNo + StreamingMoviesNo + 
        `ContractMonth-to-month` + PaperlessBillingNo + 
        `PaymentMethodCredit card (automatic)` + `PaymentMethodElectronic check` + 
        TotalCharges, 
      family = binomial, data = train)
summary(log_model7)
# Null deviance: 5704.4  on 4929  degrees of freedom
# Residual deviance: 4216.1  on 4918  degrees of freedom
# AIC: 4240.1

# DeviceProtectionNo variable has higher p-value
log_model8 <-  
  glm(formula = Churn ~ SeniorCitizen0 + MultipleLinesNo + 
        `InternetServiceFiber optic` + OnlineSecurityNo + 
        StreamingMoviesNo + 
        `ContractMonth-to-month` + PaperlessBillingNo + 
        `PaymentMethodCredit card (automatic)` + `PaymentMethodElectronic check` + 
        TotalCharges, 
      family = binomial, data = train)
summary(log_model8)
# Null deviance: 5704.4  on 4929  degrees of freedom
# Residual deviance: 4216.9  on 4919  degrees of freedom
# AIC: 4238.9

# `PaymentMethodCredit card (automatic)` variable has higher p-value 
log_model9 <-  
  glm(formula = Churn ~ SeniorCitizen0 + MultipleLinesNo + 
        `InternetServiceFiber optic` + OnlineSecurityNo + 
        StreamingMoviesNo + 
        `ContractMonth-to-month` + PaperlessBillingNo + 
        `PaymentMethodElectronic check` + 
        TotalCharges, 
      family = binomial, data = train)
summary(log_model9)
# Null deviance: 5704.4  on 4929  degrees of freedom
# Residual deviance: 4220.2  on 4920  degrees of freedom
# AIC: 4240.2

# SeniorCitizen0 variable has higher p-value
log_model10 <-  
  glm(formula = Churn ~ MultipleLinesNo + 
        `InternetServiceFiber optic` + OnlineSecurityNo + 
        StreamingMoviesNo + 
        `ContractMonth-to-month` + PaperlessBillingNo + 
        `PaymentMethodElectronic check` + 
        TotalCharges, 
      family = binomial, data = train)
summary(log_model10)
# Null deviance: 5704.4  on 4929  degrees of freedom
# Residual deviance: 4229.3  on 4921  degrees of freedom
# AIC: 4247.3

#StreamingMoviesNo variable though is significant, though removing the variable slightly increasaes the
# AIF, the accuracy & sensitivity are better 
log_model11 <-  
  glm(formula = Churn ~ MultipleLinesNo + 
        `InternetServiceFiber optic` + OnlineSecurityNo + 
        `ContractMonth-to-month` + PaperlessBillingNo + 
        `PaymentMethodElectronic check` + 
        TotalCharges, 
      family = binomial, data = train)
summary(log_model11)

# Make the final logistic regression model.
model_final <- log_model11
# Observation:
# Null deviance: 5704.4  on 4929  degrees of freedom
# Residual deviance: 4242.8  on 4922  degrees of freedom
# AIC: 4258.8
# Most of the variables are marked as significant and further removing the variables by
# highest p-value is eventually decreasing the accuracy and the sensitivity.
# As the ojective is to predict the potential customers who would churn, a model with
# high sensitivity is the ideal choice.

# Predict the probabilities of the train and test data set with the model
prediction_train <- predict(model_final, type = "response")
prediction_test <-
  predict(model_final, newdata = test, type = "response")

# create a prediction object for train and test data set
pred_object_train <-
  prediction(prediction_train, train$Churn)
pred_object_test <- prediction(prediction_test, test$Churn)

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
# Observation: AUC = 0.8331124

# create a performace object with area of curve measure for test data set
auc_test <- performance(pred_object_test, measure = "auc")
# store the auc value in auc_test
auc_test <- auc_test@y.values[[1]]
auc_test
# Observation: auc = 0.8304046. There is no variance in the test vs train auc score

# c-statistic and KS -statistic
# store the c-statistic of train and test data set in c_train and c_test variables
c_train <- rcorr.cens(prediction_train, train$Churn)
c_train
# Observation: C Index= 8.331124e-01 for train data
c_test <- rcorr.cens(prediction_test, test$Churn)
c_test
# Observation: C Index= 8.304046e-01 for test data which is almost same
# to the train data c-statistic

# find the ks-statistic of train data set and store in ks_table_train
ks_table_train <-
  attr(performance_measure_train, "y.values")[[1]] - (attr(performance_measure_train, "x.values")[[1]])
# find the max value of the ks-statistic
ks_train = max(ks_table_train)
ks_train
# Observation: max value= 0.5257904
which(ks_table_train == ks_train)
# The index of the max ks-statistic is 2092
# The KS-statistic decile of train data set is 2092/4930 which is 4th decile

# find the ks-statistic of test data set and store in ks_table_test
ks_table_test <-
  attr(performance_measure_test, "y.values")[[1]] - (attr(performance_measure_test, "x.values")[[1]])
# find the max value of the ks-statistic
ks_test = max(ks_table_test)
ks_test
# Observation: max value= 0.5144245
which(ks_table_test == ks_test)
# The KS-statistic decile of test data set is 887/2113 which is 4th decile

# Selecting threshold value
# Plot the ROC curve to determince the threshold value
# The model objective is to efficient predict the customers who would churn(Churn=Yes)
# so we have to choose a threshold which has maximum tpr and less fpr to identify
# potential churning customers.
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
                as.numeric(train$Churn == "Yes"),
                positive = "1")
# Observation:
# Accuracy = 0.7128
# Sensitivity = 0.8448
# Specificity = 0.6651


# derive confusion matrix with threshold=0.2
confusionMatrix(as.numeric(prediction_test > 0.2),
                as.numeric(test$Churn == "Yes"),
                positive = "1")
# Observation:
# Accuracy = 0.7151
# Sensitivity = 0.8360
# Specificity = 0.6714

# Conclusion: As bank objective is to identify potential customers who would churn out
# a model with threshold=0.2 and a good sensitivity (tpr) is a ideal choice






##-------------------------------------- SVM Modelling ----------------------------------

# Bring the data in the correct format to implement the SVM algorithm.

# Implement the SVM algorithm using the optimal cost.

svm_data <- churn1 # 7043x32 dimension
svm_data_train <- svm_data[s,] # train dataset
svm_data_test <- svm_data[!s,] # test dataset

for (i in c(0.001,0.01,0.1, 0.5, 1, 10, 100)){
        
        model.svm = svm(Churn ~., data = svm_data_train,cost = i, scale = F,kernel="linear",probability=TRUE)
        svm_pred_train <- predict(model.svm,svm_data_train,probability = T)
        svm_confusion_matrix <- confusionMatrix(svm_pred_train,svm_data_train$Churn,positive = "Yes")
        Accuracy <- round(svm_confusion_matrix$overall[1],2)
        Sensitivity <- round(svm_confusion_matrix$byClass[1],2)
        Specificity <- round(svm_confusion_matrix$byClass[2],2)
        print(paste("for cost=",i,"Accuracy is",Accuracy,"Sensitivity is",Sensitivity,"Specificity is",Specificity,sep = " "))
}
# We can see that svm model with cost=0.001 gives better results, in terms of Sensitivity
# Accuracy is 0.8 Sensitivity is 0.56 Specificity is 0.88"

# Try with radial kernel to compare the perfomance with linear kernel
for (i in c(0.001,0.01,0.1, 0.5, 1, 10, 100)){
        
        model.svm = svm(Churn ~., data = svm_data_train,cost = i, scale = F,kernel="radial",probability=TRUE)
        svm_pred_train <- predict(model.svm,svm_data_train,probability = T)
        svm_confusion_matrix <- confusionMatrix(svm_pred_train,svm_data_train$Churn,positive = "Yes")
        Accuracy <- round(svm_confusion_matrix$overall[1],2)
        Sensitivity <- round(svm_confusion_matrix$byClass[1],2)
        Specificity <- round(svm_confusion_matrix$byClass[2],2)
        print(paste("for cost=",i,"Accuracy is",Accuracy,"Sensitivity is",Sensitivity,"Specificity is",Specificity,sep = " "))
}
# svm model with cost=0.001 gives better results, in terms of Sensitivity
# for cost= 0.001 Accuracy is 0.8 Sensitivity is 0.54 Specificity is 0.89
# We can see that linear kernel performs better compared to radial kernel

# Generate SVM best model based on cost=0.001 & linear kernel
svm_best_model <- svm(Churn ~., data = svm_data_train,cost = 0.001, scale = F,kernel="linear",probability=TRUE,decision.values=T)

# Predict values for train dataset based on best svm model --------------------------
svm_pred_train <- predict(svm_best_model,svm_data_train,probability = T,decision.values = T)
svm_pred_train_dv <- attr(svm_pred_train, "decision.values")
svm_train_class <- as.numeric(svm_data_train$Churn=="No")
#calculating the values for ROC curve for train data
svm_prediction_train <- prediction(svm_pred_train_dv,svm_train_class)
svm_perf_train <- performance(svm_prediction_train,"tpr","fpr")

# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.1,cex.lab=1.1)

# plotting the ROC curve
plot(svm_perf_train,col="black",lty=3, lwd=3)

# calculating AUC
svm_auc_train <- performance(svm_prediction_train,"auc")@y.values[[1]]
svm_auc_train # 0.84

# We can see a steep slope after True Positive Value of 0.2. Since our intention is to maximise Sensitivity, let us consider 0.2 as the threshold
# Generate Confusion Matrix for Train dataset with threshold=0.2

svm_pred_train_prob <- attr(svm_pred_train, "probabilities")
svm_pred_train_th <- (ifelse(svm_pred_train_prob[,2]>=0.2,"Yes","No"))
confusionMatrix(svm_pred_train_th,svm_data_train$Churn,positive = "Yes")
# Accuracy 0.72 Sensitivity 0.84 Specificity 0.68. We have got a very good Sensitivity value here  

# Let us check the same on test dataset -----------------------------------

# Predict values for test dataset based on best svm model
svm_pred_test <- predict(svm_best_model,svm_data_test,probability = T,decision.values = T)
svm_pred_test_dv <- attr(svm_pred_test, "decision.values")
svm_test_class <- as.numeric(svm_data_test$Churn=="No")
#calculating the values for ROC curve for train data
svm_prediction_test <- prediction(svm_pred_test_dv,svm_test_class)
svm_perf_test <- performance(svm_prediction_test,"tpr","fpr")

# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.1,cex.lab=1.1)

# plotting the ROC curve
plot(svm_perf_test,col="black",lty=3, lwd=3)

# calculating AUC
svm_auc_test <- performance(svm_prediction_test,"auc")@y.values[[1]]
svm_auc_test # 0.83

# Generate Confusion Matrix for Test dataset with threshold=0.2 (selected based on the ROC curve)
svm_pred_test_prob <- attr(svm_pred_test, "probabilities")
svm_pred_test_th <- (ifelse(svm_pred_test_prob[,2]>=0.2,"Yes","No"))
confusionMatrix(svm_pred_test_th,svm_data_test$Churn,positive = "Yes")
# Accuracy 0.71 Sensitivity 0.83 Specificity 0.67. We have got a very good Sensitivity value here also.
