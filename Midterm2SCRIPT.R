##title: "Hypothesis Testing of Drink Preference"

# Reading Survey Data and libraries
data <- read.csv('coffeetea.csv')
library(dplyr)

# Preparing Raw and Unstructered Data to Useful data
finaldata = data[!duplicated(data$Email.Address), ]

# Exploratory Data Analysis
summary(finaldata[,2:7])

# Our main attribute (on which we are going to perform our analysis) is Drink Preference (1st attribute)  
# Drink Preference Analysis
counts = table(finaldata$Do.you.prefer.tea.or.coffee.)
x = barplot(counts, main="Barplot(Histogram for Categorical Data) for Drink Preference",
  xlab="Drink Preference", ylab='counts', col = c("green","red", "yellow"), beside = TRUE)
text(x, counts, labels = counts ,cex=1, pos = 1)
x = barplot(prop.table(table(finaldata$Do.you.prefer.tea.or.coffee.)), main = 'Barplot of Drink Preference (%)', xlab = 'Drink', ylab = 'Percentage of People', ylim = c(0,0.6))
text(x, prop.table(table(finaldata$Do.you.prefer.tea.or.coffee.)), labels = paste(round(prop.table(table(finaldata$Do.you.prefer.tea.or.coffee.)), digits = 4)*100, "%") ,cex=1, col = "blue", pos = 1) 

# Barplots are the histograms of Categorical Variables
# We see that 46.97% of of our sample prefers Coffee, while 48.48% perfer Tea and 4.55% doesn't prefer any of the drink
counts = table(finaldata$Do.you.prefer.tea.or.coffee., finaldata$How.many.times.do.you.drink.it.on.daily.basis.)
x = barplot(counts, main="Distribution of 'How many drinks per Day'",
            xlab="How many Drinks per Day", ylab='counts', col = c("darkblue","red","yellow"),legend = rownames(counts), beside=TRUE)

text(x, counts, labels = counts ,cex=1, pos = 1)

# The distribution of the drinks per day with respect to Drink Preference is shown in the graph. 
counts = table(finaldata$Do.you.prefer.tea.or.coffee., finaldata$Gender)
x = barplot(counts, main="Distribution of 'Drink Prefernce over Gender'",
            xlab="Gender", ylab='Drink preference Counts', col = c('aquamarine', 'burlywood1', 'coral1'), legend = rownames(counts), beside=TRUE, ylim = c(0,20))
text(x, counts, labels = counts ,cex=1, pos = 1)

# The distribution drink preference with respect to the gender is shown in the above graph.
# Altering the Drink Preference column to a bi-valued column. - According to our statistical analysis, the column should have 2 values - 'Coffee', 'Not Coffee'
finaldata$Do.you.prefer.tea.or.coffee. = as.character(finaldata$Do.you.prefer.tea.or.coffee.)
finaldata$Do.you.prefer.tea.or.coffee.[finaldata$Do.you.prefer.tea.or.coffee. != 'Coffee'] = 'Not Coffee'

# Now let's look at the distribution of how the drink preference looks like
x = barplot(prop.table(table(finaldata$Do.you.prefer.tea.or.coffee.)), main = 'Barplot of Drink Preference', xlab = 'Drink', ylab = 'Percentage of People', ylim = c(0,0.6))
text(x, prop.table(table(finaldata$Do.you.prefer.tea.or.coffee.)), labels = paste(round(prop.table(table(finaldata$Do.you.prefer.tea.or.coffee.)), digits = 4)*100, "%") ,cex=1, col = "blue", pos = 1) 


# We can simply say that 46.97% prefer Coffee while 53.03% doesn't prefer Coffee
# Statistical Analysis
# Null Hypothesis: The proportion of people who prefer coffee will be equal to the proportion of people who don't prefer Coffee  
# Alternate Hypothesis: There proportion of people who prefer cofffe will not be equal to the proportion of people who don't prefer coffee

# Sample Size n
n = length(finaldata$Do.you.prefer.tea.or.coffee.)


#Sample size n = 66
#Here Null Hypothesis says that P = P0. So we have P = P0 = 0.5

# Calculating P_hat 
p0 = 0.5
p_hat = length(subset(finaldata$Do.you.prefer.tea.or.coffee., finaldata$Do.you.prefer.tea.or.coffee. == 'Coffee'))/length(finaldata$Do.you.prefer.tea.or.coffee.)
print(p_hat)

# Confidence Intervals
#The Confidence Intervals for the above are p_hat - E and p_hat + E. Where E is the Error.

##CI: (p_hat-E, p_hat+E)

##where E = z * sqrt(p_hat*q_hat / n) and We have q_hat = 1 - p_hat

## Our Confidence level is 95%
##Hence alpha = 0.05 and alpha/2 = 0.025
##Hence we have to calculate z-value for 1-0.025 = 0.975

E = qnorm(0.975)*sqrt(p_hat*(1-p_hat)/n)
print(E)

## Hence the Error for the confodence interval is 0.12
Lower_CI = p_hat - E
Upper_CI = p_hat + E

print(Lower_CI)
print(Upper_CI)

# Lower Confidence Interval = 0.34929
# Upper Confidence Interval = 0.59010
# We can say that, with 95% confidence, the population proportion of people who prefer coffee will definetly be in the range (0.349, 0.590)
# Calculating Test Statistic
z = (p_hat - p0)/sqrt(p0*(1-p0)/n)
print(z)

# Calculating p-value
p_value = 2*pnorm(z)
print(p_value)


## p-value is 0.62 which greater than our confidence level 0.05
# We fail to reject the Null Hypothesis

# Statistical Analysis using Traditional Method
finaltest = prop.test(length(subset(finaldata$Do.you.prefer.tea.or.coffee., finaldata$Do.you.prefer.tea.or.coffee. == 'Coffee')), length(finaldata$Do.you.prefer.tea.or.coffee.), p=0.5, correct=FALSE, alternative = "two.sided")
finaltest


# The p_value obtained in the traditional test is also 0.6225. While it is greater than our confidence level 0.05, We interpret the following
# There is a much higher probability (0.6225) that the null hypothesis (the proportion of people who prefer coffee is equal to the proportion of people who prefer Tea) is TRUE. when compared to our probability (0.05)
# Hence we fail to reject the Null Hypothesis.
print(paste("Test Statistic: ",finaltest$statistic))
print(paste("Parameter: ",finaltest$parameter))
print(paste("P_Value: ",finaltest$p.value))
print(paste("Null Value: ",finaltest$null.value))
print(paste("Confidence Intervals : ",finaltest$conf.int))
print(paste("Alternative: ", finaltest$alternative))
print(paste("Method: ", finaltest$method))