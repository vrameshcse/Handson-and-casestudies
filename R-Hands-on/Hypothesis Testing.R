#####################Hypothesis Testing##################################

#####################Data Cleaning########################################
#Load the table into data frame
popularity <-
  read.csv("Article popularity dataset.csv", stringsAsFactors = F)

#View the structure and summary of the table
str(popularity)
summary(popularity)

#Check if we have any NA values in the data frame
sum(is.na(popularity))

############################Handling outliers################################
summary(popularity$shares)

#Validate the quantiles to understand the outlier range
quantile(popularity$shares, probs = seq(0, 1, .10))
quantile(popularity$shares, probs = seq(0, 1, .02))

#Plot qq-plot to view the outliers
qqnorm(popularity$shares)
qqline(popularity$shares, col = "Red")
boxplot(popularity$shares)

#Outlier treatment: Removing the outliers
#Justifcation: Since the outlier is just 11% of the values, and since we are interested in
#in finding the popularitly only for social media vs others, it makes sense to remove the
#outliers rather than finding out median or group mean

#Save the box plot stats in share.out
shares.out <- boxplot.stats(popularity$shares)

#validate outlier effect is applied correctly on the data set
length(popularity$shares) == 
  length(popularity$shares[!popularity$shares %in% shares.out$out]) + length(shares.out$out)

#Remove the outliers and save the data frame
popularity <-
  subset(popularity, !popularity$shares %in% shares.out$out)
############################Completed outlier treatment###############################

#group the fields of different channels and apply factor
popularity$channel.type <- as.factor(
  paste(
    popularity$data_channel_is_socmed,
    popularity$data_channel_is_lifestyle,
    popularity$data_channel_is_entertainment,
    popularity$data_channel_is_bus,
    popularity$data_channel_is_tech,
    popularity$data_channel_is_world,
    sep = ""
  )
)

str(popularity)
summary(popularity$channel.type)

#apply levels to the factor
levels(popularity$channel.type) <-
  list(
    "others" = "000000",
    "others" = "000001",
    "others" = "000010",
    "others" = "000100",
    "others" = "001000",
    "others" = "010000",
    "Social Media" = "100000"
  )

str(popularity$channel.type)
summary(popularity$channel.type)

##############Fetching date from URL############################################

#To ensure a pattern "20" occurs in all the URL
length(grep("20", popularity$URL))

#find the pattern and extract using substr and save it as a seperate field
popularity$Date <-
  as.Date(unname(sapply(popularity$URL, function (x)
    substr(x,
           regexpr("20", x),
           regexpr("20", x) + 9))), format = "%Y/%m/%d")

str(popularity$Date)
summary(popularity$Date)

#create a weekdays vector and identify the day as weekday or weekend and save it as
#a seperate variable
weekdays.vector <-
  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

popularity$day.category <-
  sapply(popularity$Date, function (x)
    ifelse((weekdays(x) %in% weekdays.vector), "weekdays", "weekend"))

summary(as.factor(popularity$day.category))

str(popularity)
summary(popularity)

#########################Data Cleaning completed######################################

#############################Initial insight##########################################
aggregate(
  popularity$shares,
  by = list(popularity$day.category, popularity$channel.type),
  FUN = mean
)

#Confirm at 1% significance level if the average number of shares for each article differ significantly for articles published over weekdays and weekend.
#Null hypothesis: shares mean on weekdays = shares mean on weekend
#Alternate hypothesis: shares mean on weekdays ≠ shares mean on weekend
#Critical values: ±2.58 (we consider z-value as df is greater than 30)
#Test statistic: T-value=-18.18
#p-value: < 2.2e-16
#Final decision: Reject the null hypothesis

t.test(
  popularity$shares[popularity$day.category == "weekdays"],
  y = popularity$shares[popularity$day.category == "weekend"],
  alternative = "two.sided",
  conf.level = 0.99
)


#Confirm at 1% significance level if the average number of shares for each article published over weekend differ significantly for articles on Social media channel and other channels.
#Null hypothesis: shares on social media article over weekend = shares on other channel article over weekend
#Alternate hypothesis: shares on social media article over weekend ≠ shares on other channel article over weekend
#Critical values: ±2.58 (we consider z-value as df is greater than 30)
#Test statistic: t-value: 6.40
#p-value: < 5.664e-10
#Final decision: Reject the null hypothesis

t.test(
  popularity$shares[popularity$channel.type == "Social Media" &
                      popularity$day.category == "weekend"],
  y = popularity$shares[popularity$channel.type == "others" &
                          popularity$day.category == "weekend"],
  alternative = "two.sided",
  conf.level = 0.99
)

#Confirm at 1% significance level if the average number of shares for each article published over weekdays differ significantly for articles on Social media channel and other channels.
#Null hypothesis: shares on social media article over weekdays = shares on other channel article over weekdays
#Alternate hypothesis: shares on social media article over weekdays ≠ shares on other channel article over weekdays
#Critical values: ±2.58 (we consider z-value as df is greater than 30)
#Test statistic: t-value: 18.95
#p-value: < 2.2e-16
#Final decision: Reject the null hypothesis

t.test(
  popularity$shares[popularity$channel.type == "Social Media" &
                      popularity$day.category == "weekdays"],
  y = popularity$shares[popularity$channel.type == "others" &
                          popularity$day.category == "weekdays"],
  alternative = "two.sided",
  conf.level = 0.99
)

#Confirm at 5% significance level if the average number of shares for Social Media articles published over weekdays and weekends differ significantly from each other.
#Null hypothesis: mean of shares of social media articles over weekdays = mean of shares of social media articles over weekend
#Alternate hypothesis: mean of shares of social media articles over weekdays ≠ mean of shares of social media articles over weekend
#Critical values: ±1.96 (we consider z-value as df is greater than 30)
#Test statistic: t-value: -3.44
#p-value: 0.0006402
#Final decision: Reject the null hypothesis

t.test(
  popularity$shares[popularity$channel.type == "Social Media" &
                      popularity$day.category == "weekdays"],
  y = popularity$shares[popularity$channel.type == "Social Media" &
                          popularity$day.category == "weekend"],
  alternative = "two.sided",
  conf.level = 0.95
)