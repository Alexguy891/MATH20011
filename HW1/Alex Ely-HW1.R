a <- seq(5, 200, 5)
length(a)
a[10]
a[19]
a[22]
tenthVec = a * 0.1
oddVec = subset(a, a %% 2 == 1)
sum(subset(a, a %% 2 == 0))
threesVec <- subset(a, a %% 3 == 0)
A <- matrix(5:13, nrow = 3, ncol = 3)
A[,2]
A[3,]
t(A)
B <- diag(diag(A), nrow(A), ncol(A))
inverseB <- solve(B)
newA <- cbind(A,c(2,1,5))
newRowA <- rbind(A,c(0.3,-1.1,3.5))