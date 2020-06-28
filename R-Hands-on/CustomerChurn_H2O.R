#Business objective:

#To develop multiple predictive models using NN for a telecom company to identify a customer 
# who will potentially churn and find the best predictive model using the library "h2o".

library(MASS)
library(car)
library(h2o)
library(lattice)
library(ggplot2)
library(caret)
library(caTools)
library("e1071")
library("ROCR")
library("caTools")
library("Hmisc")
library("kernlab")

##Load the data
churn<-read.csv("telecom_nn_train.csv") 

#Make bar charts displaying the relationship between the target variable and various other features
plot(Churn ~., data = churn, type = "c")
#Observations:
# customer who are single tend to churn more than who has partners
# Customer with no dependents tend to churn more
# customer with fiber-optic tend to churn more than other internet services
# customer with no online security , no online back-up, no device protection, no tech support
# tend to churn more
# customer with month-to-month contract tend to churn more

##Check the structure
str(churn)


#####Check for na values
sapply(churn,function(x) sum(is.na(x)))  ## TotalCharges has 6 NAs

#All the NA's(TotalCharges) have zero "tenure" , hence the total charges(tenure*monthlycharge) will be zero
churn$TotalCharges[which(is.na(churn$TotalCharges))] <- 0

#check if all NA's are removed
sum(is.na(churn))

#TotalCharges plotted after NA-treatment
ggplot(churn,aes(x=TotalCharges,fill=Churn))+
  geom_histogram(position = "dodge", binwidth = 100)+
  labs(title="Churn based on total charges")



###Outliar detection
# Make Box plots for numeric variables to look for outliers. 
boxplot(churn$tenure)
boxplot.stats(churn$tenure) #No outliars

boxplot(churn$MonthlyCharges)
boxplot.stats(churn$MonthlyCharges) #No outliars

boxplot(churn$TotalCharges)
boxplot.stats(churn$TotalCharges)  #No outliars



#### Bring the variables in the correct format

str(churn)
#No variable conversion required

###################Checkpoint 1################################## 
##Model building : Neural Networks-Tuning hyperparameters WITHOUT epochs

####Creating training & test data
# Split the data into 80:20 ratio based on churn variable
set.seed(100)
churn_split <- sample.split(churn$Churn,SplitRatio=0.8)

# create train & test datasets with 80:20 ratio
churn_train <- churn[churn_split,]
churn_test <- churn[!(churn_split),]


###write the above datasts into a csv to import to h2o environment
write.csv(churn_train, file='churn-train.csv', row.names=FALSE)
write.csv(churn_test, file='churn-test.csv', row.names=FALSE)


# Initialize the h2o environment
library(h2o)
library(statmod)
h2o.init() 

####Import thr training and test filesto h2o environment
nn_churn_train<-h2o.importFile("churn-train.csv") 
nn_churn_test<-h2o.importFile("churn-test.csv") 

##Verifying that all the records have been imported
nrow(nn_churn_train)
##3944 observations with 31 variables
nrow(nn_churn_test)
##986 observations with 31 variables


# Perform 5-fold cross-validation on the training dataset using "AUTO" distribution.
#We want to predict if a customer will churn or not(2 classes)

churn_nn1 <- h2o.deeplearning(names(nn_churn_train[,-10]),names(nn_churn_train[,10]),
                              training_frame = nn_churn_train,
                              distribution = "AUTO",
                              activation = "RectifierWithDropout",
                              hidden = c(100,100,100),
                              hidden_dropout_ratio = c(0.1, 0.1, 0.1), 
                              reproducible = T,
                              seed = 100,
                              l1 = 1e-5,
                              epochs = 1,nfolds = 5)

churn_nn1 

####Obtain model metrics on train data
performance_trn <- h2o.performance(churn_nn1,nn_churn_train)
h2o.confusionMatrix(performance_trn)
h2o.find_row_by_threshold(performance_trn, 0.317701484260923)
##specificity  0.792879
## accuracy     0.770284
##sensitivity   0.705767

####Obtain model merics on test data
performance_test <- h2o.performance(churn_nn1,nn_churn_test)
h2o.confusionMatrix(performance_test)

#Confusion Matrix for max f1 @ threshold = 0.27435043154568:
#  No Yes    Error      Rate
#No     574 156 0.213699  =156/730
#Yes     57 199 0.222656   =57/256
#Totals 631 355 0.216024  =213/986

h2o.find_row_by_threshold(performance_test, 0.27435043154568)
#specificity  0.786301
##accuracy     0.783976
##sensitivity   0.777344



####Prediction on training data set
pred_nn1_trn <- h2o.predict(churn_nn1,nn_churn_train[,-10])
pred_nn1_trn

#Test the model on the test data
Pred_nn1_tst <- h2o.predict(churn_nn1, nn_churn_test[,-10])
Pred_nn1_tst


########################################################################################################
# Model 2 :Perform 5-fold cross-validation on the training_frame using bernoulli distribution
churn_nn2 <- h2o.deeplearning(names(nn_churn_train[,-10]),names(nn_churn_train[,10]),
                              training_frame = nn_churn_train,
                              distribution = "bernoulli",
                              activation = "RectifierWithDropout",
                              hidden = c(100,100,100),
                              hidden_dropout_ratio = c(0.1, 0.1, 0.1),
                              reproducible = T,
                              seed = 100,
                              l1 = 1e-5,
                              epochs = 1,nfolds = 5)

churn_nn2

performance_train2 <- h2o.performance(churn_nn2,nn_churn_train)
h2o.confusionMatrix(performance_train2)

##Confusion Matrix for max f1 @ threshold = 0.273772443874864:
##  No  Yes    Error       Rate
##No     2308  613 0.209860  =613/2921
##Yes     265  758 0.259042  =265/1023
##Totals 2573 1371 0.222617  =878/3944

h2o.find_row_by_threshold(performance_train2, 0.273772443874864)
#specificity  0.790140
# accuracy     0.777383
#sensitivity   0.740958


performance_test2 <- h2o.performance(churn_nn2,nn_churn_test)
h2o.confusionMatrix(performance_test2)

#Confusion Matrix for max f1 @ threshold = 0.27435043154568:
#  No Yes    Error      Rate
#No     574 156 0.213699  =156/730
#Yes     57 199 0.222656   =57/256
#Totals 631 355 0.216024  =213/986

h2o.find_row_by_threshold(performance_test2, 0.27435043154568)
#specificity  0..786301
## accuracy     0.783976
#sensitivity   0.777344

###From models churn_nn1 and churn_nn2 we see that churn nn2 gives a better sensitivity(74%) compared
##to churn_nn1.Hence we will continue to use "Bernoulli" distribution.

#########################################################################################################

# Model 3 :Perform 5-fold cross-validation on the training data set  using bernoulli distribution and \
##reducing the number of hidden layers to 2


churn_nn3 <- h2o.deeplearning(names(nn_churn_train[,-10]),names(nn_churn_train[,10]),
                              training_frame = nn_churn_train,
                              distribution = "bernoulli",
                              activation = "RectifierWithDropout",
                              hidden = c(100,100),
                              hidden_dropout_ratio = c(0.1, 0.1),
                              reproducible = T,
                              seed = 100,
                              l1 = 1e-5,
                              epochs = 1,nfolds = 5)

churn_nn3

performance_train3 <- h2o.performance(churn_nn3,nn_churn_train)
h2o.confusionMatrix(performance_train3)
##Confusion Matrix for max f1 @ threshold = 0.331649679618544:
##  No  Yes    Error       Rate
##No     2413  508 0.173913  =508/2921
##Yes     295  728 0.288368  =295/1023
##Totals 2708 1236 0.203600  =803/3944

h2o.find_row_by_threshold(performance_train3, 0.331649679618544)
##specificity  0.826087
##accuracy     0.796400
##sensitivity   0.711632

performance_test3 <- h2o.performance(churn_nn3,nn_churn_test)
h2o.confusionMatrix(performance_test3)
#Confusion Matrix for max f1 @ threshold = 0.245648619466403:
#  No Yes    Error      Rate
#No     567 163 0.223288  =163/730
#Yes     54 202 0.210938   =54/256
#Totals 621 365 0.220081  =217/986

h2o.find_row_by_threshold(performance_test3, 0.245648619466403)
# specificity  0.776712
# accuracy      0.779919
# sensitivity   0.789062


###From model churn_nn3 we observe that the accuracy and specificity of the model increased to 79%
## and 82% whereas the sensitivity reduced to 71%. Hence let's see if we can increase the sensitivity
###Let's reduce the number of neuron's in each layer 2*no offeatures i.e 2*30=60 and also increase hidden_dropout_ratio
## to 0.2

churn_nn4 <- h2o.deeplearning(names(nn_churn_train[,-10]),names(nn_churn_train[,10]),
                              training_frame = nn_churn_train,
                              distribution = "bernoulli",
                              activation = "RectifierWithDropout",
                              hidden = c(60,60),
                              hidden_dropout_ratio = c(0.2, 0.2),
                              reproducible = T,
                              seed = 100,
                              l1 = 1e-5,
                              epochs = 1,nfolds = 5)

churn_nn4

performance_train4<- h2o.performance(churn_nn4,nn_churn_train)
h2o.confusionMatrix(performance_train4)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.32604072024912:
#          No  Yes    Error       Rate
# No     2291  630 0.215680  =630/2921
# Yes     254  769 0.248289  =254/1023
# Totals 2545 1399 0.224138  =884/3944

h2o.find_row_by_threshold(performance_train4, 0.32604072024912)
# specificity  0.784320
# accuracy     0.775862
# sensitivity   0.751711


performance_test4 <- h2o.performance(churn_nn4,nn_churn_test)
h2o.confusionMatrix(performance_test4)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.301471679456382:
#         No Yes    Error      Rate
# No     560 170 0.232877  =170/730
# Yes     56 200 0.218750   =56/256
# Totals 616 370 0.229209  =226/986

h2o.find_row_by_threshold(performance_test4, 0.301471679456382)
# specificity  0.767123
# accuracy      0.770791
# sensitivity   0.781250

#### From churn_nn4 we see that the sensitivity increased to 75% compared to 71% from churn_nn3.So let's try
## to reduce the number of hidden layer to 1 as the data set is not really big

##########################################################################################################
churn_nn5 <- h2o.deeplearning(names(nn_churn_train[,-10]),names(nn_churn_train[,10]),
                              training_frame = nn_churn_train,
                              distribution = "AUTO",
                              activation = "RectifierWithDropout",
                              hidden = c(60),
                              hidden_dropout_ratio = c(0.2),
                              l1 = 1e-5,
                              reproducible = T,
                              seed = 100,
                              epochs = 1,nfolds = 5)

churn_nn5

performance_train5 <- h2o.performance(churn_nn5,nn_churn_train)
h2o.confusionMatrix(performance_train5)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.331175697479867:
#          No  Yes    Error       Rate
# No     2346  575 0.196850  =575/2921
# Yes     257  766 0.251222  =257/1023
# Totals 2603 1341 0.210953  =832/3944

h2o.find_row_by_threshold(performance_train5, 0.331175697479867)
# specificity  0.803150
# accuracy     0.789047
# sensitivity   0.748778


performance_test5 <- h2o.performance(churn_nn5,nn_churn_test)
h2o.confusionMatrix(performance_test5)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.380529253004392:
#         No Yes    Error      Rate
# No     597 133 0.182192  =133/730
# Yes     70 186 0.273438   =70/256
# Totals 667 319 0.205882  =203/986


h2o.find_row_by_threshold(performance_test5, 0.380529253004392)
# specificity  0.817808
# accuracy      0.794118
# sensitivity   0.726562

##From churn_nn5 we observe that the sensitivity is almost same as churn_nn4 but specificity and accuracy
##increased. Let's try to reduce the number of neuron's to 30 i.e the number of features and verify the results
########################################################################################################
churn_nn6 <- h2o.deeplearning(names(nn_churn_train[,-10]),names(nn_churn_train[,10]),
                              training_frame = nn_churn_train,
                              distribution = "bernoulli",
                              activation = "RectifierWithDropout",
                              hidden = c(30),
                              hidden_dropout_ratio = c(0.1),
                              reproducible = T,
                              seed = 100,
                              l1 = 1e-5,
                              epochs = 1,nfolds = 5)

churn_nn6

performance_train6 <- h2o.performance(churn_nn6,nn_churn_train)
h2o.confusionMatrix(performance_train6)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.275096550773613:
#          No  Yes    Error       Rate
# No     2293  628 0.214995  =628/2921
# Yes     255  768 0.249267  =255/1023
# Totals 2548 1396 0.223884  =883/3944

h2o.find_row_by_threshold(performance_train6, 0.275096550773613)
# specificity  0.785005
# accuracy     0.776116
# sensitivity   0.750733



performance_test6 <- h2o.performance(churn_nn6,nn_churn_test)
h2o.confusionMatrix(performance_test6)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.484162972545472:
#         No Yes    Error      Rate
# No     620 110 0.150685  =110/730
# Yes     86 170 0.335938   =86/256
# Totals 706 280 0.198783  =196/986

h2o.find_row_by_threshold(performance_test6, 0.484162972545472)
# specificity  0.849315
# accuracy      0.801217
# sensitivity   0.664062


###From model churn_nn6 we see that the train data set is giving a sensitivity of 75% which is better than other
##models
### Let's see if using tanhwithdropout can help improve sensitivity i.e customers who will churn rate
############################################################################################################
###Let's try to build the above modelusing tanhwithdropout which is a sigmoid activation function


churn_nn7 <- h2o.deeplearning(names(nn_churn_train[,-10]),names(nn_churn_train[,10]),
                              training_frame = nn_churn_train,
                              distribution = "bernoulli",
                              activation = "TanhWithDropout",
                              hidden = c(60),
                              hidden_dropout_ratio = c(0.2),
                              reproducible = T,
                              seed = 100,
                              l1 = 1e-5,
                              epochs = 1,nfolds = 5)

churn_nn7

performance_train7 <- h2o.performance(churn_nn7,nn_churn_train)
h2o.confusionMatrix(performance_train7)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.292111167339953:
#          No  Yes    Error       Rate
# No     2237  684 0.234166  =684/2921
# Yes     232  791 0.226784  =232/1023
# Totals 2469 1475 0.232252  =916/3944

h2o.find_row_by_threshold(performance_train7, 0.292111167339953)
# specificity  0.7665834
# accuracy     0.767748
# sensitivity   0.773216


performance_test7 <- h2o.performance(churn_nn7,nn_churn_test)
h2o.confusionMatrix(performance_test7)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.380312902462342:
#         No Yes    Error      Rate
# No     587 143 0.195890  =143/730
# Yes     63 193 0.246094   =63/256
# Totals 650 336 0.208925  =206/986


h2o.find_row_by_threshold(performance_test7, 0.380312902462342)
# specificity  0.804110
# accuracy      0.791075
# sensitivity   0.753906


###Model churn_nn7 is giving comparitively more sensitivity than churn_nn6 but the accuracy of model churn_nn6
##is good. So we can consider churn_nn6 to be good.Also the number of neuron's is 30 in churn_nn6 which is the least.

########################################################################

######################################################################################
#check point 2  : Model - Neural Networks - Tuning hyperparameters WITH epochs
######################################################################################
#

#Taking model churn_nn6 and let's try changing the epochs
######################################################################################
cp2_churn_nn1 <- h2o.deeplearning(names(nn_churn_train[,-10]),names(nn_churn_train[,10]),
                              training_frame = nn_churn_train,
                              distribution = "bernoulli",
                              activation = "RectifierWithDropout",
                              hidden = c(30),
                              hidden_dropout_ratio = c(0.1),
                              reproducible = T,
                              seed = 100,
                              l1 = 1e-5,
                              epochs = 10,nfolds = 5)

cp2_churn_nn1

cp2perf1 <- h2o.performance(cp2_churn_nn1,nn_churn_train)
h2o.confusionMatrix(cp2perf1)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.363509564073306:
#          No  Yes    Error       Rate
# No     2449  472 0.161588  =472/2921
# Yes     279  744 0.272727  =279/1023
# Totals 2728 1216 0.190416  =751/3944


h2o.find_row_by_threshold(cp2perf1, 0.363509564073306)
# specificity  0.838412
# accuracy     0.809584
# sensitivity   0.727273


cp2performance_test1 <- h2o.performance(cp2_churn_nn1,nn_churn_test)
h2o.confusionMatrix(cp2performance_test1)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.385061833983988:
#         No Yes    Error      Rate
# No     601 129 0.176712  =129/730
# Yes     73 183 0.285156   =73/256
# Totals 674 312 0.204868  =202/986

h2o.find_row_by_threshold(cp2performance_test1, 0.385061833983988)
# specificity  0.823288
# accuracy     0.795132
# sensitivity   0.714844


### cp2_churn_nn1 gives a sensitivity of .727. Lets see if we can further increase it by changing epochs to 20
######################################################################################

cp2_churn_nn2 <- h2o.deeplearning(names(nn_churn_train[,-10]),names(nn_churn_train[,10]),
                                  training_frame = nn_churn_train,
                                  distribution = "bernoulli",
                                  activation = "RectifierWithDropout",
                                  hidden = c(30),
                                  hidden_dropout_ratio = c(0.1),
                                  reproducible = T,
                                  seed = 100,
                                  l1 = 1e-5,
                                  epochs = 20,nfolds = 5)

cp2_churn_nn2

cp2perf2 <- h2o.performance(cp2_churn_nn2,nn_churn_train)
h2o.confusionMatrix(cp2perf2)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.333791895505344:
#          No  Yes    Error       Rate
# No     2380  541 0.185211  =541/2921
# Yes     229  794 0.223851  =229/1023
# Totals 2609 1335 0.195233  =770/3944

h2o.find_row_by_threshold(cp2perf2, 0.333791895505344)
# specificity  0.814789
# accuracy     0.804767
# sensitivity   0.776149


cp2performance_test2 <- h2o.performance(cp2_churn_nn2,nn_churn_test)
h2o.confusionMatrix(cp2performance_test2)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.410684902969209:
#         No Yes    Error      Rate
# No     617 113 0.154795  =113/730
# Yes     79 177 0.308594   =79/256
# Totals 696 290 0.194726  =192/986

h2o.find_row_by_threshold(cp2performance_test2, 0.410684902969209)
# specificity  0.845205
# accuracy     0.805274
# Sensitivity   0.691406

##Model cp2_churn_nn2 gave a sensitivity of 77% on train data and sensitivity of 69% on test data.Mse and logloss
## are more on validation data. Hence model might be having bit more bias. So let's try to run with a huge epoch
######################################################################################
cp2_churn_nn3 <- h2o.deeplearning(names(nn_churn_train[,-10]),names(nn_churn_train[,10]),
                                  training_frame = nn_churn_train,
                                  distribution = "bernoulli",
                                  activation = "RectifierWithDropout",
                                  hidden = c(30),
                                  hidden_dropout_ratio = c(0.1),
                                  reproducible = T,
                                  seed = 100,
                                  l1 = 1e-5,
                                  epochs = 100,nfolds = 5)

cp2_churn_nn3

cp2perf3 <- h2o.performance(cp2_churn_nn3,nn_churn_train)
h2o.confusionMatrix(cp2perf3)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.30780945444935:
#          No  Yes    Error       Rate
# No     2370  551 0.188634  =551/2921
# Yes     227  796 0.221896  =227/1023
# Totals 2597 1347 0.197262  =778/3944

h2o.find_row_by_threshold(cp2perf3, 0.30780945444935)
# specificity  0.811366
# accuracy     0.802738
# sensitivity   0.778104


cp2performance_test3 <- h2o.performance(cp2_churn_nn3,nn_churn_test)
h2o.confusionMatrix(cp2performance_test3)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.382672809889058:
#         No Yes    Error      Rate
# No     599 131 0.179452  =131/730
# Yes     73 183 0.285156   =73/256
# Totals 672 314 0.206897  =204/986

h2o.find_row_by_threshold(cp2performance_test3, 0.382672809889058)
# specificity  0.820548
# accuracy     0.793103
# sensitivity   0.714844

##Model cp2_churn_nn3 gave a sensitivity of 77% on train data and sensitivity of 71.5% on test data.Mse and logloss
## are sligtly greater on validation data. So let's try to run by changing epochs to 150

######################################################################################

cp2_churn_nn4 <- h2o.deeplearning(names(nn_churn_train[,-10]),names(nn_churn_train[,10]),
                                  training_frame = nn_churn_train,
                                  distribution = "bernoulli",
                                  activation = "RectifierWithDropout",
                                  hidden = c(30),
                                  hidden_dropout_ratio = c(0.1),
                                  reproducible = T,
                                  seed = 100,
                                  l1 = 1e-5,
                                  epochs = 150,nfolds = 5)

cp2_churn_nn4

cp2perf4 <- h2o.performance(cp2_churn_nn4,nn_churn_train)
h2o.confusionMatrix(cp2perf4)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.30780945444935:
# No  Yes    Error       Rate
# No     2370  551 0.188634  =551/2921
# Yes     227  796 0.221896  =227/1023
# Totals 2597 1347 0.197262  =778/3944

h2o.find_row_by_threshold(cp2perf4, 0.30780945444935)
# specificity  0.811366
# accuracy     0.802738
# sensitivity   0.778104


cp2performance_test4 <- h2o.performance(cp2_churn_nn4,nn_churn_test)
h2o.confusionMatrix(cp2performance_test4)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.382672809889058:
# No Yes    Error      Rate
# No     599 131 0.179452  =131/730
# Yes     73 183 0.285156   =73/256
# Totals 672 314 0.206897  =204/986

h2o.find_row_by_threshold(cp2performance_test4, 0.382672809889058)
# specificity  0.820548
# accuracy     0.793103
# sensitivity   0.714844


##### We see that increasing epochs to 150 in model cp2_churn_nn4 yielded the same results as of cp2_churn_nn3
###The results are converging. So a lower epoch might increase the sensitivity. Let's try with epoch = 50

############################################################################################################
cp2_churn_nn5 <- h2o.deeplearning(names(nn_churn_train[,-10]),names(nn_churn_train[,10]),
                                  training_frame = nn_churn_train,
                                  distribution = "bernoulli",
                                  activation = "RectifierWithDropout",
                                  hidden = c(30),
                                  hidden_dropout_ratio = c(0.1),
                                  reproducible = T,
                                  seed = 100,
                                  l1 = 1e-5,
                                  epochs = 50,nfolds = 5)

cp2_churn_nn5

cp2perf5 <- h2o.performance(cp2_churn_nn5,nn_churn_train)
h2o.confusionMatrix(cp2perf5)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.30780945444935:
# No  Yes    Error       Rate
# No     2370  551 0.188634  =551/2921
# Yes     227  796 0.221896  =227/1023
# Totals 2597 1347 0.197262  =778/3944

h2o.find_row_by_threshold(cp2perf5, 0.30780945444935)
# specificity  0.811366
# accuracy     0.802738
# sensitivity   0.778104


cp2performance_test5 <- h2o.performance(cp2_churn_nn5,nn_churn_test)
h2o.confusionMatrix(cp2performance_test5)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.382672809889058:
# No Yes    Error      Rate
# No     599 131 0.179452  =131/730
# Yes     73 183 0.285156   =73/256
# Totals 672 314 0.206897  =204/986

h2o.find_row_by_threshold(cp2performance_test5, 0.382672809889058)
# specificity  0.820548
# accuracy     0.793103
# sensitivity   0.714844


##### We see that with epochs=50 in model cp2_churn_nn5 yielded the same results as of cp2_churn_nn4 & cp2_churn_nn3
###The results are converging. So a lower epoch might increase the sensitivity. Let's try with epoch = 25


###########################################################################################################
cp2_churn_nn6 <- h2o.deeplearning(names(nn_churn_train[,-10]),names(nn_churn_train[,10]),
                                  training_frame = nn_churn_train,
                                  distribution = "bernoulli",
                                  activation = "RectifierWithDropout",
                                  hidden = c(30),
                                  hidden_dropout_ratio = c(0.1),
                                  reproducible = T,
                                  seed = 100,
                                  l1 = 1e-5,
                                  epochs = 25,nfolds = 5)

cp2_churn_nn6

cp2perf6 <- h2o.performance(cp2_churn_nn6,nn_churn_train)
h2o.confusionMatrix(cp2perf6)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.349095990532077:
#          No  Yes    Error       Rate
# No     2439  482 0.165012  =482/2921
# Yes     259  764 0.253177  =259/1023
# Totals 2698 1246 0.187880  =741/3944

h2o.find_row_by_threshold(cp2perf6, 0.349095990532077)
# specificity  0.834988
# accuracy     0.8012120
# sensitivity   0.746823


cp2performance_test6 <- h2o.performance(cp2_churn_nn6,nn_churn_test)
h2o.confusionMatrix(cp2performance_test6)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.415918908987228:
#         No Yes    Error      Rate
# No     620 110 0.150685  =110/730
# Yes     79 177 0.308594   =79/256
# Totals 699 287 0.191684  =189/986

h2o.find_row_by_threshold(cp2performance_test6, 0.415918908987228)
# specificity  0.849315
# accuracy     0.808316
# sensitivity   0.691406

### Model cp2_churn_nn6 with epoch 25 did not give better performance on test data as compared to model
### cp2_churn_nn2 or cp2_churn_nn1 . We choose cp2_churn_nn1 with epoch = 10 as the best model.
###############################################################################################################
####Checkpoint 3 : Neural Networks - Best Model

final_model<-cp2_churn_nn1

final_model

cp3perf <- h2o.performance(final_model,nn_churn_train)
h2o.confusionMatrix(cp3perf)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.363509564073306:
# No  Yes    Error       Rate
# No     2449  472 0.161588  =472/2921
# Yes     279  744 0.272727  =279/1023
# Totals 2728 1216 0.190416  =751/3944

h2o.find_row_by_threshold(cp3perf, 0.363509564073306)
# specificity  0.838412
# accuracy     0.809584
# sensitivity   0.727273


cp3performance_test <- h2o.performance(final_model,nn_churn_test)
h2o.confusionMatrix(cp3performance_test)
# Observation
# Confusion Matrix for max f1 @ threshold = 0.385061833983988:
# No Yes    Error      Rate
# No     601 129 0.176712  =129/730
# Yes     73 183 0.285156   =73/256
# Totals 674 312 0.204868  =202/986

h2o.find_row_by_threshold(cp3performance_test, 0.385061833983988)
# specificity  0.823288
# accuracy     0.795132
# sensitivity   0.714844


# Test the model on the test data

churnPrediction <- h2o.predict(final_model, nn_churn_test)

####################################final_model performance metrics############################
###    On training dataset
###    specificity  0.838412
###    accuracy     0.809584
###    sensitivity   0.727273

###    on test data set
###    specificity  0.823288
###    accuracy     0.795132
###    sensitivity   0.714844