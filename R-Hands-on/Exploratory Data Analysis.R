############################EDA Casestudy####################################

library(stringr)
library(moments)
library(ggplot2)
library(GGally)
library(scales)
######################Checkpoint-1: Data Cleaning and preparation ############

#Importing loan.csv file in the variable loan
loan <- read.csv("loan.csv", blank.lines.skip = T)

#Structure of the loan data
str(loan)

#Subset the loan data with only the driver columns
loan_d <-
  subset(
    loan,
    select = c(
      "annual_inc",
      "loan_amnt",
      "funded_amnt",
      "int_rate",
      "grade",
      "dti",
      "emp_length",
      "purpose",
      "home_ownership",
      "loan_status"
    )
  )

#Structure of the load data with only the driver variables
str(loan_d)

#Summary of the loan data with driver variables
summary(loan_d)

#summary(loan_d[!is.na(loan_d$annual_inc),]$annual_inc)
boxplot(loan_d[!is.na(loan_d$annual_inc), ]$annual_inc, boxmex = 0.2)

#Removing the data with NA since the number is less than 10% of the total data
loan_d <- loan_d[!is.na(loan_d$annual_inc), ]

#Treating the n/a values of the emp_length variable
aggregate(annual_inc ~ emp_length, loan_d, mean)

#From the aggregate function we understand that the n/a values can be
##replaced by 0 years since the average income is less than the customers
#with <1 year experience
loan_d$emp_length[which(loan_d$emp_length == "n/a")] <- "< 1 year"

#Giving unique ID to each row element in loan data
loan_d$ID <- c(1:42531)

#Standardizing the values in the loan_status column i.e Removing Does not
#meet the credit policy from status as it does not make difference to loan approval
loan_d$loan_status[which(loan_d$loan_status == "Does not meet the credit policy. Status:Fully Paid")] <-
  "Fully Paid"

loan_d$loan_status[which(loan_d$loan_status == "Does not meet the credit policy. Status:Charged Off")] <-
  "Charged Off"

#Remove rows where loan_status is Fully paid
loan_d <- loan_d[which(loan_d$loan_status != "Fully Paid"), ]

#Create a new column named loan_status_1  with three levels current_new,
#default_new and late
loan_d$loan_status_1 <-
  ifelse(
    loan_d$loan_status == "Current" |
      loan_d$loan_status == "In Grace Period",
    "current_new",
    ifelse(
      loan_d$loan_status == "Default" |
        loan_d$loan_status == "Charged Off",
      "default_new",
      "late"
    )
  )

#Convert the loan_status_1 column in to factors
loan_d$loan_status_1 <- as.factor(loan_d$loan_status_1)
str(loan_d$int_rate)

#Create new bin variables for int_rate and emp_length
#Extract decimal numbers from int_rate
loan_d$int_rate_grp <-
  str_extract(loan_d$int_rate, "[[:digit:]]+\\.*[[:digit:]]*")
loan_d$int_rate_grp <- as.numeric(loan_d$int_rate_grp)

#Bin creation for int_rate
loan_d$int_rate_grp <- as.factor(ifelse(
  loan_d$int_rate_grp < 10,
  "Low",
  ifelse(
    loan_d$int_rate_grp >= 10 & loan_d$int_rate_grp <= 18,
    "Medium",
    "High"
  )
))

#Extract employment length in years from emp_length
loan_d$emp_len_grp <-
  str_extract(loan_d$emp_length, "[[:digit:]]+")
loan_d$emp_len_grp <- as.numeric(loan_d$emp_len_grp)

#Bin creation for emp_length
loan_d$emp_len_grp <- as.factor(ifelse(
  loan_d$emp_len_grp >= 1 & loan_d$emp_len_grp <= 4,
  "Junior",
  ifelse(
    loan_d$emp_len_grp >= 5 & loan_d$emp_len_grp <= 8,
    "Mid-level",
    "Senior"
  )
))


########################Checkpoint 2: Exploratory Data Analysis #################


################ Univariate analysis of annual_inc obervation ###################

summary(loan_d$annual_inc)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 2000   39000   55000   65200   78000 1250000
# This shows there are outliers beyond 3rd quartile and the mean is greater than median
# indicated left skewed data

quantile(loan_d$annual_inc, probs = seq(0, 1, .10))
# 0%     10%     20%     30%     40%     50%     60%     70%     80%     90%    100%
# 2000   27600   36000   42000   48000   55000   62000   72000   85000  110000 1250000

#Histogram plot to see how the data are distributed in various bins of the values
ggplot(loan_d, aes(x = annual_inc)) + geom_histogram(col = "Blue", binwidth = 30) + geom_smooth(aes(y = (..count..)), stat = "count")

qqnorm(loan_d$annual_inc)
qqline(loan_d$annual_inc, col = "Red")
#Observation: QQ-plot indicates it is not normally distributed

#Coefficient of variance
Coefficient.annual_inc <-
  sd(loan_d$annual_inc) / mean(loan_d$annual_inc)
Coefficient.annual_inc * 100
sd(loan_d$annual_inc)
#coefficient of variance of 75% indicates that the values are widely distributed
#away from mean

boxplot(loan_d$annual_inc)
# Observation: There are outliers

skewness(loan_d$annual_inc)
#Observation: 6.335396 - Indicates it has a positive skewness or right-tailed distribution

kurtosis(loan_d$annual_inc)
#Observation: 89.81387 - Indicates we have a light tail

length(boxplot.stats(loan_d$annual_inc)$out) / length(loan_d$annual_inc) * 100
#Observation: % of outliers are to be removed to get a meaningful insight

#Treating the outliers in the load dataset
qn <- quantile(loan_d$annual_inc, c(0.05, 0.95), na.rm = TRUE)
loan_final <-
  within(loan_d, {
    annual_inc = ifelse(annual_inc < qn[1], qn[1], annual_inc)
    annual_inc = ifelse(annual_inc > qn[2], qn[2], annual_inc)
  })
boxplot(loan_final$annual_inc)
# Observation: Outliers are floored and capped at 5% and 95% values

skewness(loan_final$annual_inc)
#Observation: 0.9756793 - Indicates it has a positive skewness or right-tailed distribution

kurtosis(loan_final$annual_inc)
#Observation: 3.28839 - Indicates we have close to normal curve

###############univariate analysis of loan amount observation#######################

summary(loan_d$loan_amnt)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 500    6000   10750   12640   18000   35000
# Mean is greater than median which indicates the values are more dense beyond
#50th percentile

quantile(loan_d$loan_amnt, probs = seq(0, 1, .10))
# 0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100%
# 500  3500  5000  7000  9000 10750 13200 16000 20000 25000 35000

#Histogram plot to see how the data are distributed in various bins of the observations
ggplot(loan_d, aes(x = loan_amnt)) + geom_histogram(col = "Blue", binwidth = 30) + geom_smooth(aes(y = (..count..)), stat = "count")

qqnorm(loan_d$loan_amnt)
qqline(loan_d$loan_amnt, col = "Red")
#Observation: QQ-plot indicates it is not normally distributed

#Coefficient of variance
Coefficient.loanamt <- sd(loan_d$loan_amnt) / mean(loan_d$loan_amnt)
Coefficient.loanamt * 100
sd(loan_d$loan_amnt)
#coefficient of variance of 65% indicates that the values are widely distributed
#away from mean but relatively less than annual increment amount

boxplot(loan_d$loan_amnt)
#Observation: There are no outliers

skewness(loan_d$loan_amnt)
#Observation: 0.8388929 - Indicates it has a positive skewness or right-tailed distribution

kurtosis(loan_d$loan_amnt)
#Observation: 3.069213 - Indicates we have a light tail

length(boxplot.stats(loan_d$loan_amnt)$out) / length(loan_d$loan_amnt) * 100
#Observation: 0% - No outliers



###############univariate analysis of Funded amount observation#######################

summary(loan_final$funded_amnt)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 500    5750   10000   11880   16000   35000
# Mean is greater than median which indicates the values are dense beyond
#50th percentile

quantile(loan_final$funded_amnt, probs = seq(0, 1, .10))
# 0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100%
# 500  3200  5000  6475  8000 10000 12000 15000 18000 23275 35000

#Histogram plot to see how the data are distributed in various bins of the observations
ggplot(loan_final, aes(x = funded_amnt)) + geom_histogram(col = "Blue", binwidth = 30) + geom_smooth(aes(y = (..count..)), stat = "count")

qqnorm(loan_final$funded_amnt)
qqline(loan_final$funded_amnt, col = "Red")
#Observation: QQ-plot indicates it is not normally distributed

#Coefficient of variance
Coefficient.fundedamt <-
  sd(loan_d$funded_amnt) / mean(loan_d$funded_amnt)
Coefficient.fundedamt * 100
sd(loan_d$funded_amnt)
#coefficient of variance of 65% indicates that the values are widely distributed
# away from mean but relatively less than annual increment amount and equally
#distributed like loan amount

boxplot(loan_final$funded_amnt)
#Observation: There are outliers

skewness(loan_final$funded_amnt)
#Observation: 0.8388929 - Indicates it has a positive skewness or right-tailed distribution

kurtosis(loan_final$funded_amnt)
#Observation: 3.069213 - Indicates we have a heavy tail

length(boxplot.stats(loan_final$funded_amnt)$out) / length(loan_final$funded_amnt) * 100
#Observation: 0% - No outliers



###############univariate analysis of dti observation#######################

summary(loan_final$dti)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00    9.36   14.63   14.30   19.65   29.95
# Mean is less than median, which incicates it has left tailed distribution and has more
# values below the 50th percentile


quantile(loan_final$dti, probs = seq(0, 1, .10))
# 0%   10%   20%   30%   40%   50%   60%   70%   80%   90%  100%
# 500  3200  5000  6475  8000 10000 12000 15000 18000 23275 35000

#Histogram plot to see how the data are distributed in various bins of the observations
ggplot(loan_final, aes(x = dti)) + geom_histogram(col = "Blue", binwidth = 30) + geom_smooth(aes(y = (..count..)), stat = "count")


qqnorm(loan_final$dti)
qqline(loan_final$dti, col = "Red")
#Observation: QQ-plot indicates it is not normally distributed

#Coefficient of variance
Coefficient.dti <- sd(loan_d$dti) / mean(loan_d$dti)
Coefficient.dti * 100
sd(loan_d$dti)
#coefficient of variance of 47% indicates that the values are widely distributed away from mean
# but relatively less than annual increment amount, loan amount & funded amount

boxplot(loan_final$dti)
#Observation: There are no outliers

skewness(loan_final$dti)
#Observation: -0.177985 - Indicates it has a positive skewness or right-tailed distribution

kurtosis(loan_final$dti)
#Observation: 2.228364 - Indicates we have a light tail

length(boxplot.stats(loan_final$dti)$out) / length(loan_final$dti) * 100
#Observation: 0% - No outliers



###########################Multivariate Analysis#####################################

cor(loan_final$dti, loan_final$annual_inc)
# -0.0688614: Negative Linear Association between dti & annual inc.
#Annual inc has minimal negative impact on dti.

cor(loan_final$dti, loan_final$loan_amnt)
#0.0661684 Positive Linear Association between dti & loan amount.
#Loan amt has a impact on dti.

cor(loan_final$dti, loan_final$funded_amnt)
#0.06542201 Positive Linear Association between dti & funded amount.
#Funded amt has a impact on dti.

cor(loan_final$annual_inc, loan_final$loan_amnt)
#0.4798386 strong positive linear association between annual_inc & loan_amnt
#annual income has strong impact on loan_amnt

cor(loan_final$annual_inc, loan_final$funded_amnt)
#0.4715959 strong positive linear association between annual_inc & funded_amnt
#annual income has strong impact on funded_amnt

cor(loan_final$funded_amnt, loan_final$funded_amnt)
#1 Absolute positive linear association between loan amount & funded_amnt
#loan amount has direct impact on funded_amnt

#using ggally for multivariate analysis
ggpairs(subset(loan_final, select = c(dti, annual_inc, loan_amnt, funded_amnt)))

#Distribtuon of Annual Income against Loan Status
plot.lstatus.annual_inc <-
  ggplot(loan_final, aes(x = annual_inc, fill = loan_status_1)) +
  ggtitle("Distribution Of Annual Inc against Loan Status") +
  labs(x = "Annual income",  fill = "Loan Status") +
  scale_x_continuous(labels = scales::comma, breaks = c(seq(
    from = 0,
    to = max(loan_final$annual_inc),
    by = 20000
  )))

plot.lstatus.annual_inc + geom_histogram() + facet_grid(loan_status_1 ~ .)
#Observation: Above histogram shows occurence of default_new status is more
#when compared to current_new & late status across annual incomes.
#Most number of defaults happened for the annual income 20,000 to 75,000

plot.lstatus.annual_inc + geom_histogram(aes(y = (..density..)), bins = 20) + facet_grid(loan_status_1 ~ .)
#Observation: Among the default_new status, more density observed for the
#annual income 25,000 to 75000.
#Also, Among the late status, more density occured for the annual income 50,000

rm(plot.lstatus.annual_inc)

#---Distribution of lOan amount against loan status --##

plot.lstatus.loan_amnt <-
  ggplot(loan_final, aes(x = loan_amnt, fill = loan_status_1)) +
  ggtitle("Distribution Of Loan Amount against Loan Status") +
  labs(x = "Loan Amount", fill = "Loan Status") +
  scale_x_continuous(breaks = c(seq(
    from = 0, to = max(loan_final$loan_amnt), 2500
  )))

plot.lstatus.loan_amnt + geom_histogram() +
  facet_grid(loan_status_1 ~ .)
#Observation1: recorded highest default for the loan amounts 5000,10000,15000,20000,25000
#Observation2: More defaults occured for the loanamounts <10000


plot.lstatus.loan_amnt + geom_histogram(aes(y = (..density..))) +
  facet_grid(loan_status_1 ~ .)
#Observation: More default densities for the loan amount 3000 to 10000.
#ALso observer more late payments on 11000, 16000 & 20000


#--- Distribtuion of debit to income ratio against loan status --#

plot.lstatus.dti <-
  ggplot(loan_final, aes(x = dti, fill = loan_status_1)) +
  ggtitle("Distribution Of Dti against Loan Status") +
  labs(x = "Dti", fill = "Loan Status") +
  scale_x_continuous(breaks = c(seq(
    from = 0, to = max(loan_final$dti), 2
  )))

plot.lstatus.dti + geom_histogram() + facet_grid(loan_status_1 ~ .)
#Observation: More defaults occured for dti within  8 to 24

plot.lstatus.dti + geom_histogram(aes(y = (..density..))) + facet_grid(loan_status_1 ~ .)
#Observation: More late status recorded occured for dti within  8 to 24


#--- Distribution of Annual income against interest rates

plot.irt.annual_inc <-
  ggplot(loan_final, aes(x = annual_inc, fill = int_rate_grp)) +
  ggtitle("Distribution Of Annual Inc against int_rate_grp ") +
  labs(x = "Annual income", fill = "Interest Rates") +
  scale_x_continuous(labels = scales::comma, breaks = c(seq(
    from = 0,
    to = max(loan_final$annual_inc),
    by = 20000
  )))

plot.irt.annual_inc + geom_histogram() + facet_grid(int_rate_grp ~ .)
#Observation: More occurence of Medium Interest Rates loans acorss Annual Income.
#35000 to 65000 recorded more Medium interest rates.
#50000 recorded most highest interest rates loans.

#plot.irt.annual_inc + geom_histogram(aes(y = (..density..))) + facet_grid(int_rate_grp ~ .)


### Distirbution of loan amount against interest Rate groups
plot.irt.loan_amnt <-
  ggplot(loan_final, aes(x = loan_amnt, fill = int_rate_grp)) +
  ggtitle("Distribution Of Loan Amount against int_rate_grp ") +
  labs(x = "Loan Amount", fill = "Interest Rates") +
  scale_x_continuous(breaks = c(seq(
    from = 0, to = max(loan_final$loan_amnt), 2500
  )))

plot.irt.loan_amnt + geom_histogram() + facet_grid(int_rate_grp ~ .)
#Observation: Loan with loan amount<10000 recorded more medium interest rate loans.
#Also notices significant amount of medium interest loans with loan amount 15000,
#20000 & 25000.Amount of  Loans with  high, low interest rates are less as compared
#with Medium.


### Distribution of Dti against interest Rate groups
plot.irt.dti <-
  ggplot(loan_final, aes(x = dti, fill = int_rate_grp)) +
  ggtitle("Distribution Of Dti against int_rate_grp ") +
  labs(x = "Dti", fill = "Interest Rates") +
  scale_x_continuous(breaks = c(seq(
    from = 0, to = max(loan_final$dti), 2
  )))

plot.irt.dti + geom_histogram() + facet_grid(int_rate_grp ~ .)
#Observation: More medium interest rates observerdfor Debit to income between 8 to 15
#Among high interest rates more occurences recorded for Debit to Income between 11to 23


ggplot(loan_final,
       aes(x = dti,
           y = loan_amnt,
           col = loan_status_1)) +
  scale_x_continuous(breaks = c(seq(
    from = 0, to = max(loan_final$dti), 3
  ))) +
  scale_y_continuous(breaks = c(seq(
    from = 0, to = max(loan_final$loan_amnt), 1000
  ))) +
  geom_point() + facet_grid( ~ int_rate_grp)

#Observation: More Default observered when debit to income is within 12 to 24
#and Loan Amount <80000

######################Hypothesis Testing###########################################

###########Testing: 1#######################
#H0:Mean value of annual income is equal for default_new and current_new loan status
#H1: !H0
#Confidence level: 95%
#Critical value: +- 1.96 (Z-value was referred as sample size is >30)
#Test statistic: two tailed T-test as we dont know the standard deviation of the population

t.test(
  loan_final$annual_inc[loan_final$loan_status_1 == "default_new"],
  loan_final$annual_inc[loan_final$loan_status_1 == "current_new"],
  mean = 0,
  alternative = "two.sided",
  conf.level = 0.95
)

#Observation: As the t-value -11.161 < -1.96 and p value < 2.2e-16 (negligible), we conclude to
# reject the null hypothesis i.e, the mean value of annual income is not equal for
# default-new and current_new loan status

###########Testing: 2#######################
#H0:Mean value of loan amount is equal for default_new and current_new loan status
#H1: !H0
#Confidence level: 95%
#Critical value: +- 1.96 (Z-value was referred as sample size is >30)
#Test statistic: two tailed T-test as we dont know the standard deviation of the population

t.test(
  loan_final$loan_amnt[loan_final$loan_status_1 == "default_new"],
  loan_final$loan_amnt[loan_final$loan_status_1 == "current_new"],
  mean = 0,
  alternative = "two.sided",
  conf.level = 0.95
)

#Observation: As the t-value -18.08 < -1.96 and p value is < 2.2e-16 (negligible), we conclude to
# reject the null hypothesis i.e, the mean value of loan amount is not equal for
# default-new and current_new loan status

###########Testing: 3#######################
#H0:Mean value of funded amount is equal for default_new and current_new loan status
#H1: !H0
#Confidence level: 95%
#Critical value: +- 1.96 (Z-value was referred as sample size is >30)
#Test statistic: two tailed T-test as we dont know the standard deviation of the population

t.test(
  loan_final$funded_amnt[loan_final$loan_status_1 == "default_new"],
  loan_final$funded_amnt[loan_final$loan_status_1 == "current_new"],
  mean = 0,
  alternative = "two.sided",
  conf.level = 0.95
)

#Observation: As the t-value -18.23 < -1.96 and p value is < 2.2e-16 (negligible), we conclude to
# reject the null hypothesis i.e, the mean value of funded amount is not equal for
# default-new and current_new loan status

###########Testing: 4#######################
#H0:Mean value of dti is equal for default_new and current_new loan status
#H1: !H0
#Confidence level: 95%
#Critical value: +- 1.96 (Z-value was referred as sample size is >30)
#Test statistic: two tailed T-test as we dont know the standard deviation of the population

t.test(
  loan_final$dti[loan_final$loan_status_1 == "default_new"],
  loan_final$dti[loan_final$loan_status_1 == "current_new"],
  mean = 0,
  alternative = "two.sided",
  conf.level = 0.95
)

#Observation: As the t-value -4.09 < -1.96 and p value is 4.546e-05 (negligible), we conclude to
# reject the null hypothesis i.e, the mean value of dti is not equal for
# default-new and current_new loan status

###########Testing: 5#######################
#H0:Mean value of annual income is equal for High and Low interest rate group
#H1: !H0
#Confidence level: 95%
#Critical value: +- 1.96 (Z-value was referred as sample size is >30)
#Test statistic: two tailed T-test as we dont know the standard deviation of the population

t.test(
  loan_final$annual_inc[loan_final$int_rate_grp == "High"],
  loan_final$annual_inc[loan_final$int_rate_grp == "Low"],
  mean = 0,
  alternative = "two.sided",
  conf.level = 0.95
)

#Observation: As the t-value 14.664 > 1.96 and p value is < 2.2e-16 (negligible), we conclude to
# reject the null hypothesis i.e, the mean value of annual income is not equal for
# High and Low interest rate group

###########Testing: 6#######################
#H0:Mean value of loan amount is equal for High and Low interest rate group
#H1: !H0
#Confidence level: 95%
#Critical value: +- 1.96 (Z-value was referred as sample size is >30)
#Test statistic: two tailed T-test as we dont know the standard deviation of the population

t.test(
  loan_final$loan_amnt[loan_final$int_rate_grp == "High"],
  loan_final$loan_amnt[loan_final$int_rate_grp == "Low"],
  mean = 0,
  alternative = "two.sided",
  conf.level = 0.95
)

#Observation: As the t-value 27.46 > 1.96 and p value is < 2.2e-16 (negligible), we conclude to
# reject the null hypothesis i.e, the mean value of loan amount is not equal for
# High and Low interest rate group

###########Testing: 7#######################
#H0:Mean value of funded amount is equal for High and Low interest rate group
#H1: !H0
#Confidence level: 95%
#Critical value: +- 1.96 (Z-value was referred as sample size is >30)
#Test statistic: two tailed T-test as we dont know the standard deviation of the population

t.test(
  loan_final$funded_amnt[loan_final$int_rate_grp == "High"],
  loan_final$funded_amnt[loan_final$int_rate_grp == "Low"],
  mean = 0,
  alternative = "two.sided",
  conf.level = 0.95
)

#Observation: As the t-value 27.525 > 1.96 and p value is < 2.2e-16 (negligible), we conclude to
# reject the null hypothesis i.e, the mean value of loan amount is not equal for
# High and Low interest rate group

###########Testing: 8#######################
#H0:Mean value of funded amount is equal for High and Low interest rate group
#H1: !H0
#Confidence level: 95%
#Critical value: +- 1.96 (Z-value was referred as sample size is >30)
#Test statistic: two tailed T-test as we dont know the standard deviation of the population

t.test(
  loan_final$dti[loan_final$int_rate_grp == "High"],
  loan_final$dti[loan_final$int_rate_grp == "Low"],
  mean = 0,
  alternative = "two.sided",
  conf.level = 0.95
)

#Observation: As the t-value 4.33 > 1.96 and p value is 1.555e-05 (negligible), we conclude to
# reject the null hypothesis i.e, the mean value of loan amount is not equal for
# High and Low interest rate group

######################End of Hypothesis Testing###########################################
