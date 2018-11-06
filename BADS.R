# HW1 Ex 1
a = 3
b = 4.5
a
is.character(b)
a^2 + 1/b
sqrt(a*b)
log2(a)
sqrt(3*4.5)



# HW1 Ex 2
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



# HW2 Statistical analysis
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



# HW3
# some helpful visualizations
# install.packages("ggplot2")
library("ggplot2")
qplot(data = Loan_Data, x=dINC_A, y=dINC_SP, color=BAD)
ggplot(data = Loan_Data, aes(x=dINC_A, y=dINC_SP, color = BAD)) + geom_count(alpha=0.5)



