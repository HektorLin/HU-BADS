#Exercise 1
a = 3
b = 4.5
a
is.character(b)
a^2 + 1/b
sqrt(a*b)
log2(a)
sqrt(3*4.5)

#Exercise 2
m.A = matrix(c(1:9), nrow=3, byrow=TRUE)
m.A[3,3] = m.A[3,3] + 1
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