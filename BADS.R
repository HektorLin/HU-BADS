# HW1 Ex 1
{
a = 3
b = 4.5
a
is.character(b)
a^2 + 1/b
sqrt(a*b)
log2(a)
sqrt(3*4.5)
}

# HW1 Ex 2
{
m.A = matrix(c(1:9), nrow=3, byrow=TRUE)
m.A[3,3] = m.A[3,3] + 1 #may try direct input value with 
m.B = matrix(c(1:9), nrow=3, byrow=FALSE)
v.y = c(1:3)
a*m.A
m.A%*%m.B
m.invA = solve(m.A)
m.A%*%m.invA
t(m.B)
m.B[1,] = 1
m.B
m.Beta = solve(t(m.A)%*%m.A)%*%t(m.A)%*%v.y
m.Beta
}

# HW2 Statistical analysis
{
setwd("C:/Users/Hector Lin/Documents/HU-courses-practice/BADS")
Loan_Data = read.csv("Loan_Data.csv",  sep = ";")

# 1. Create two variables, inc.good and inc.bad, 
# which contain the incomes of good and bad credit risks, respectively
# from dicttionary for BAD 1=bad, 0=good
# IMPORTANT: subset function!!!
#temp1 = as.data.frame(subset(Loan_Data, as.numeric(as.character(Loan_Data$BAD)) == 1))
#temp1$dINC_A = as.character(temp1$dINC_A)
#inc.bad = as.numeric(temp1$dINC_A)
#temp2 = as.data.frame(subset(Loan_Data, as.numeric(as.character(Loan_Data$BAD)) != 1))
#temp2$dINC_A = as.character(temp2$dINC_A)
#inc.good = as.numeric(temp2$dINC_A)
inc.bad = Loan_Data$dINC_A[Loan_Data$BAD == 1]
inc.good = Loan_Data$dINC_A[Loan_Data$BAD != 1]


# 2. Depict the distribution of the income of customers with a good and bad risk,
# respectively, by means of a boxplot.
boxplot(inc.good,inc.bad, names = c("Good","Bad"),
                          main = "Income distribution of credit applications",
                          ylab = "Applicants income [$]")

# 3.  Calculate the difference between 
# the average/mean income of good and bad credit applicants.
difference = mean(inc.good) - mean(inc.bad)
print(difference)
hist(inc.good)
hist(inc.bad)

# 4. Identify an appropriate statistical test to verify 
# whether the observed income difference is statistically significant. 
# Perform the test and display its results.
GBtest = t.test(inc.good,inc.bad)
print(GBtest)

# 5. Assign the test result to a variable. 
# Use the print() function to output a message that tells the user
# whether the observed income difference is significant.
if(GBtest$p.value < 0.01) { 
    print("Significant") 
    }
}

# HW3 visualization
{
# some helpful visualizations
# install.packages("ggplot2")
library("ggplot2")
qplot(data = Loan_Data, x=dINC_A, y=dINC_SP, color=BAD)
ggplot(data = Loan_Data, aes(x=dINC_A, y=dINC_SP, color = BAD)) + geom_count(alpha=0.5)

#install.packages("psych")
library("sp")
library("psych")

# the one w/ red wine data
wine <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"),header=TRUE, sep=";")
summary(wine)
head(wine)
describe(wine)
ggplot(data = wine, aes(x = alcohol, y = quality)) + geom_point()
#install.packages('gpairs')
library( gpairs )
gpairs(wine [,-12])

install.packages('corrplot')
library ( corrplot )
corrplot(cor( wine[,1:11]))
corrplot.mixed(cor( wine[,1:11]) , upper ="ellipse")

lr1 <- lm(quality ~volatile.acidity+alcohol, data = wine)
summary(lr1)

# random sampling from data set
wine_sample = wine[sample(nrow(wine),500),] 

lr2 <- lm(quality ~volatile.acidity+alcohol, data = wine_sample)
}

# Missing HW4 here...


# HW5 log regression - copied from Tutor's code, with mini adj. 
{
# Prepare the data
uni <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv",header=TRUE, sep=",")
str(uni)
summary(uni)

class(uni$admit)
table(uni$admit)

xtabs(~admit + rank, data = uni)
#seems that it is ok, no NA. Rank 1 is clearly the "best" with the highest admission ratio

uni$rank <- factor(uni$rank, 
                   #levels = c(4,3,2,1)
                   )
uni$admit <- as.character(uni$admit)

#par(mfrow=c(2,2))
# Boxplot of GPA grouped by variable admit (1=yes/0=no)
boxplot(gpa ~ admit, data = uni)
library(ggplot2)
ggplot(uni, aes(y=gpa, x= admit)) + geom_boxplot()

# Correlation of GPA and GRE scores
cor(uni$gpa, uni$gre)

# Scatterplot with regression line
# Note: The swirly brackets encase a block of code. This is 
#       necessary only for Rmarkdown to make sure that the plot
#       and regression line are drawn 'at the same time'
{
  plot(uni$gpa, uni$gre)
  regressionLine <- lm(gre ~ gpa, data = uni)
  abline(regressionLine, col = 'red')
}
# The glm function with a logit link offers the necessary functionality. 
lr <- glm(admit ~rank, data = uni, family = binomial(link="logit"))

# Given that glm() supports many types of models including logistic 
# regression, you need to specify which model you need. 
# The third parameter in the above function call does just that. 

# Print a summary of the model
summary(lr)
# Note how rank4 is left out and becomes the base level (i.e. all others 0)

# As for interpretation
betas <- lr$coefficients
betas
# The regression part gives a prediction on the odds of our target class 'admission'
# In other words, prob(admission)/prob(rejection)
# If the odds are >1, then the chance to be sucessful is higher than that of rejection
xtabs(~admit + rank, data = uni)
xtabs(~admit + rank, data = uni)[2,] / xtabs(~admit + rank, data = uni)[1,]

#Let's try to interpret it, remember logistic regression assumes a linear relationship between variables
# Baseline (rank 4): Intercept + 0 + 0 + 0
exp(betas[1])
# Rank 1: exp(Intercept + 0 + 0 + 1.68)
exp(betas[[1]]+betas[[4]])
# Which are the odds based on our tabulation

# We can now calculate odds ratio: having attended a rank 1 instead of a rank 4 school decreases admission odds by a factor of 
1.178571/0.2181818
# which turns out is equivalent to our effect for rank 1
exp(betas[4])

# What does that mean in terms of probability?
prob_rank4 = 1/(1+exp(- betas[[1]]))
prob_rank1 = 1/(1+exp(- (betas[[1]]+betas[[4]])))
prob_rank1/prob_rank4
# Not 5 times the probability!

# Now let's include all variables
lr <- glm(admit ~., data = uni, family = binomial(link="logit"))

summary(lr)
#For every one unit change in gre, the log odds of admission (versus non-admission) increases by 0.002.
#For a one unit increase in gpa, the log odds of being admitted to graduate school increases by 0.804.
#The indicator variables for rank have a slightly different interpretation. For example, having attended an undergraduate institution with rank of 2, versus an institution with a rank of 1, changes the log odds of admission by -0.675.

# Compute model predictions. For simplicity, we use the same data
# that we used to build the model (i.e., resubstitution estimate)

uni$prediction <- predict(lr, newdata = uni, type="response" )
# The last parameter says that the predicitons should be on the same
# scale as the response (i.e., dependent variable). Try calling the
# predict function without this parameter and see what happends.
head(predict(lr, newdata = uni))

# Start with an intuitive measure of model performance. For binary outcomes,
# the accuracy of a model describes how often the predicted class matches
# the observed outcome.
# Infer the predicted class from the predicted class probabilities
# We chose the default threshold of 0.5. Is that a good idea? Probably not.

uni$prediction_class <- ifelse(uni$prediction > 0.5, "1", "0")

#Set the vector that will contain your performance values
accuracy <- vector()
accuracy["Model"]<- sum(uni$prediction_class == uni$admit) / length(uni$prediction_class)


# Do you think your accuracy is good?
# Difficult to tell without a benchmark.
# There are two common naive benchmarks depending on if we test on the predicted classes or predicted probabilities.
# First, a simple benchmark for discrete class prediction is to 'predict' the most frequent  outcome for all cases
# Function rep repeats a value for a number of times, here the number of observations
# Always remember what you are predicting (probabilities)

baseline_probability <- sum(uni$admit == "1")/nrow(uni)

class.benchmark <- rep(baseline_probability, nrow(uni))  

#Now we make a little custom function. where we can feed in predicted probabilities 
Accuracy <- function(prediction, class, threshold = 0.5){
  # Predict the second factor level if the predicted prob. is higher than
  # a threshold
  predClass <-  ifelse(prediction > 0.5, levels(class)[2], levels(class)[1])
  # The accuracy is the ratio of predictions that are equal to 
  # the actual observations
  acc <- sum(predClass == class) / length(class)
  return(acc)
}

accuracy["Benchmark"] <- Accuracy(prediction = class.benchmark, uni$admit, threshold = 0.5)

# Second, when prediting probabilities, we can make things more realistic by predicting the event with its sample probability
class.random <- sample(c(0,1), size = nrow(uni), replace = TRUE, prob = c(1-baseline_probability, baseline_probability))
accuracy["Random"] <- Accuracy(prediction = class.random, class = uni$admit,threshold = 0.5)
accuracy
# What if we change the threshold??
}



