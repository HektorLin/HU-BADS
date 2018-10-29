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
Loan_Data = read.csv2("Loan_Data.csv")
# from dicttionary for BAD 1=bad, 0=good
# subset function!!!
summary(Loan_Data)
temp1 = as.data.frame(subset(Loan_Data, as.numeric(as.character(Loan_Data$BAD)) == 1))
temp1$dINC_A = as.character(temp1$dINC_A)
inc.bad = as.numeric(temp1$dINC_A)

temp2 = as.data.frame(subset(Loan_Data, as.numeric(as.character(Loan_Data$BAD)) != 1))
temp2$dINC_A = as.character(temp2$dINC_A)
inc.good = as.numeric(temp2$dINC_A)

boxplot(inc.good,inc.bad)

